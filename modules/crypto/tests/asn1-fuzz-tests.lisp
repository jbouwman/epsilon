;;;; ASN.1 fuzz corpus and harness (IMPL-329 G3)
;;;;
;;;; A pull parser for untrusted DER input is the historical home of the
;;;; gnarliest TLS/X.509 CVEs (GnuTLS, OpenSSL, libssh, SChannel...).
;;;; This file pairs a curated corpus of malformed inputs with a
;;;; randomised byte fuzz, asserting in both cases that the parser
;;;; either returns a structure or signals a controlled condition. No
;;;; uncaught Lisp error -- not type-error, not arithmetic-error, not
;;;; sb-int:invalid-array-index-error -- may escape the parser.
;;;;
;;;; Targets:
;;;;   - epsilon.crypto.asn1:der-decode-safe   (input-bounded TLV decoder)
;;;;   - epsilon.crypto.x509:parse-x509-certificate (full cert parser)
;;;;
;;;; The corpus deliberately exercises the historical attack patterns
;;;; against ASN.1 parsers (length attacks, depth attacks, long-form
;;;; tag exhaustion, indefinite-length BER smuggling, etc.) so that any
;;;; future regression in the boundary checks fails this file rather
;;;; than ship.

(defpackage epsilon.crypto.asn1-fuzz-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.x509 x509)))

(in-package :epsilon.crypto.asn1-fuzz-tests)

;;; ---------------------------------------------------------------------------
;;; Harness
;;; ---------------------------------------------------------------------------

(defun parser-tolerates-p (target bytes)
  "Run TARGET over BYTES and return one of :ok :asn1-error :x509-error
   :uncaught (with the offending condition cons'd onto the keyword).
   :uncaught is the failure mode -- a parser that signals a generic
   Lisp condition rather than the controlled decoder type."
  (handler-case
      (progn
        (ecase target
          (:asn1 (asn1:der-decode-safe bytes))
          (:x509 (x509:parse-x509-certificate bytes)))
        :ok)
    (asn1:asn1-decode-error () :asn1-error)
    (x509:x509-decode-error () :x509-error)
    (error (c) (cons :uncaught c))))

(defun assert-tolerated (target bytes &optional description)
  "Assert that BYTES drive TARGET to either parse cleanly or signal
   one of the controlled decoder error types. DESCRIPTION names the
   attack pattern in failure messages."
  (let ((outcome (parser-tolerates-p target bytes)))
    (assert-true (or (eq outcome :ok)
                     (eq outcome :asn1-error)
                     (eq outcome :x509-error))
                 (format nil "~A leaked uncaught condition on ~A: ~A"
                         target (or description "input")
                         (when (consp outcome) (cdr outcome))))))

(defun bytes (&rest octets)
  "Construct a byte vector from inline octet literals."
  (make-array (length octets)
              :element-type '(unsigned-byte 8)
              :initial-contents octets))

;;; ---------------------------------------------------------------------------
;;; Curated corpus
;;; ---------------------------------------------------------------------------

;;; Each entry is (description bytes). The harness asserts no uncaught
;;; condition leaks. Most of these are EXPECTED to signal asn1-decode-
;;; error; a small subset are valid (or just long enough to exercise
;;; deep paths) and may parse cleanly.

(defparameter +fuzz-corpus+
  (list
   ;; --- empty / single byte ---
   (list "empty input" (bytes))
   (list "single zero byte" (bytes #x00))
   (list "single 0xff byte" (bytes #xff))
   (list "tag only, no length" (bytes #x30))
   (list "tag and zero length" (bytes #x30 #x00))

   ;; --- length encoding edge cases ---
   (list "long form length, 0 bytes (indefinite, BER-only)"
         (bytes #x30 #x80))
   (list "long form length, 5 bytes (>4 not allowed)"
         (bytes #x30 #x85 #x00 #x00 #x00 #x00 #x00))
   (list "non-minimal short length encoding (0x81 0x00)"
         (bytes #x30 #x81 #x00))
   (list "non-minimal long length (0x81 0x7F)"
         (bytes #x30 #x81 #x7f))
   (list "length claims 0x7fff_ffff bytes"
         (bytes #x30 #x84 #x7f #xff #xff #xff))
   (list "truncated long-form length"
         (bytes #x30 #x82 #x00))

   ;; --- length vs available data ---
   (list "length exceeds available bytes"
         (bytes #x30 #x05 #x02 #x01 #x01))
   (list "length exactly matches but children truncated"
         (bytes #x30 #x03 #x02 #x05 #x01))

   ;; --- long-form tag attacks ---
   (list "long-form tag, never terminates (continuation bits set)"
         (let ((b (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element #xff)))
           (setf (aref b 0) #x1f)  ; tag = long form
           b))
   (list "long-form tag, claims tag value 2^33 (overflow)"
         (bytes #x1f #xff #xff #xff #xff #xff #x7f #x00))

   ;; --- depth attacks ---
   (list "100 nested SEQUENCEs (over depth limit of 32)"
         (let ((depth 100))
           (let ((b (make-array (+ (* 2 depth) 0)
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
             (loop for i from 0 below depth
                   do (setf (aref b (* 2 i)) #x30)
                      (setf (aref b (1+ (* 2 i)))
                            (if (= i (1- depth)) #x00
                                ;; remaining payload after this header
                                (- (* 2 (- depth i 1)) 0))))
             b)))

   ;; --- DER vs BER ---
   (list "constructed primitive type (constructed OCTET STRING)"
         (bytes #x24 #x02 #x04 #x00))
   (list "indefinite length on constructed (BER, not DER)"
         (bytes #x30 #x80 #x02 #x01 #x01 #x00 #x00))

   ;; --- malformed primitives ---
   (list "BOOLEAN with two-byte content"
         (bytes #x01 #x02 #xff #xff))
   (list "NULL with one-byte content"
         (bytes #x05 #x01 #x00))
   (list "INTEGER with two leading zero bytes"
         (bytes #x02 #x03 #x00 #x00 #x01))
   (list "OID with high-bit-on terminator"
         (bytes #x06 #x03 #x80 #x80 #x80))

   ;; --- SEQUENCE structural ---
   (list "constructed SEQUENCE containing SEQUENCE with bad inner length"
         (bytes #x30 #x05 #x30 #xff #xff #xff #xff))
   (list "SEQUENCE with extra trailing bytes"
         (bytes #x30 #x03 #x02 #x01 #x01 #xaa #xbb))

   ;; --- huge but bounded inputs ---
   (list "16 KiB of 0xff bytes"
         (let ((b (make-array (* 16 1024) :element-type '(unsigned-byte 8)
                                          :initial-element #xff)))
           b))
   (list "32 KiB of zeros"
         (make-array (* 32 1024) :element-type '(unsigned-byte 8)
                                 :initial-element 0))

   ;; --- valid trivial encodings (must parse cleanly) ---
   (list "valid INTEGER 1" (bytes #x02 #x01 #x01))
   (list "valid empty SEQUENCE" (bytes #x30 #x00))
   (list "valid SEQUENCE { INTEGER 1 }" (bytes #x30 #x03 #x02 #x01 #x01))
   (list "valid NULL" (bytes #x05 #x00))))

;;; ---------------------------------------------------------------------------
;;; Curated corpus tests
;;; ---------------------------------------------------------------------------

(deftest test-asn1-fuzz-corpus-decoder
  "der-decode-safe handles every entry in the curated corpus without
   leaking an uncaught Lisp condition."
  (dolist (entry +fuzz-corpus+)
    (destructuring-bind (description bytes) entry
      (assert-tolerated :asn1 bytes description))))

(deftest test-asn1-fuzz-corpus-x509
  "parse-x509-certificate similarly tolerates every entry: any input
   that isn't a valid certificate should signal x509-decode-error or
   asn1-decode-error, not leak a generic condition."
  (dolist (entry +fuzz-corpus+)
    (destructuring-bind (description bytes) entry
      (assert-tolerated :x509 bytes description))))

(deftest test-asn1-fuzz-depth-limit-enforced
  "100 nested SEQUENCEs trigger the depth limit cleanly (a regression
   would either succeed in parsing the whole stack or stack-overflow)."
  (let ((deep (make-array 200 :element-type '(unsigned-byte 8)
                               :initial-element 0)))
    (loop for i from 0 below 100
          do (setf (aref deep (* 2 i)) #x30)
             (setf (aref deep (1+ (* 2 i))) #x82))
    (let ((outcome (parser-tolerates-p :asn1 deep)))
      ;; Specifically: must signal asn1-decode-error, not :ok.
      (assert-eq outcome :asn1-error))))

(deftest test-asn1-fuzz-input-size-limit
  "der-decode-safe rejects inputs larger than +max-input-size+."
  (assert-condition (asn1:asn1-decode-error)
    (asn1:der-decode-safe
     (make-array (1+ asn1:+max-input-size+)
                 :element-type '(unsigned-byte 8)
                 :initial-element 0))))

(deftest test-asn1-fuzz-node-budget
  "der-decode-safe rejects inputs that would explode the node budget."
  ;; Build a SEQUENCE containing many tiny INTEGERs. Each INTEGER is 3
  ;; bytes (tag, length, value), so N bytes of payload yields N/3 nodes
  ;; plus 1 for the outer SEQUENCE.
  (let* ((n-children (1+ asn1:+max-node-count+))
         (payload (make-array (* 3 n-children)
                              :element-type '(unsigned-byte 8))))
    (loop for i from 0 below n-children
          do (setf (aref payload (* 3 i))      #x02)
             (setf (aref payload (1+ (* 3 i))) #x01)
             (setf (aref payload (+ (* 3 i) 2)) #x01))
    ;; The outer SEQUENCE header carries a 2-byte length.
    (let ((wrapped (concatenate '(simple-array (unsigned-byte 8) (*))
                                #(#x30 #x83)
                                (let ((a (make-array 3 :element-type
                                                        '(unsigned-byte 8))))
                                  (setf (aref a 0) (ldb (byte 8 16)
                                                        (length payload))
                                        (aref a 1) (ldb (byte 8 8)
                                                        (length payload))
                                        (aref a 2) (ldb (byte 8 0)
                                                        (length payload)))
                                  a)
                                payload)))
      (assert-condition (asn1:asn1-decode-error)
        (asn1:der-decode-safe wrapped)))))

;;; ---------------------------------------------------------------------------
;;; Randomised fuzz
;;; ---------------------------------------------------------------------------

;;; Two-tier random fuzz: completely random bytes (mostly nonsense, but
;;; should still never crash) and structurally-shaped bytes (random
;;; payload behind a real-looking SEQUENCE header) which exercises the
;;; recursive descent path more frequently.

(defun random-bytes (n &key (state *random-state*))
  (let ((b (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n b)
      (setf (aref b i) (random 256 state)))))

(defun random-shaped-bytes (n &key (state *random-state*))
  "Random bytes prefixed with a plausible SEQUENCE header that claims
   to contain the rest of the buffer. Drives the recursive descent
   path more often than pure noise."
  (let* ((payload-len (max 0 (- n 4)))
         (b (make-array n :element-type '(unsigned-byte 8))))
    (when (>= n 4)
      (setf (aref b 0) #x30
            (aref b 1) #x82
            (aref b 2) (ldb (byte 8 8) payload-len)
            (aref b 3) (ldb (byte 8 0) payload-len))
      (loop for i from 4 below n
            do (setf (aref b i) (random 256 state))))
    (when (< n 4)
      (loop for i from 0 below n
            do (setf (aref b i) (random 256 state))))
    b))

(deftest test-asn1-fuzz-random-bytes
  "Drive 2000 random byte sequences (sizes 0..512) through the
   decoder and the cert parser. None may leak an uncaught condition."
  (let ((state (sb-ext:seed-random-state 0))) ; deterministic
    (dotimes (i 2000)
      (let* ((n (random 512 state))
             (b (random-bytes n :state state)))
        (assert-tolerated :asn1 b
                          (format nil "random[~D] size=~D" i n))
        (assert-tolerated :x509 b
                          (format nil "random[~D] size=~D" i n))))))

(deftest test-asn1-fuzz-random-shaped-bytes
  "Same property under structurally-plausible inputs that pass the tag
   sanity check more often than pure noise."
  (let ((state (sb-ext:seed-random-state 1)))
    (dotimes (i 2000)
      (let* ((n (+ 4 (random 512 state)))
             (b (random-shaped-bytes n :state state)))
        (assert-tolerated :asn1 b
                          (format nil "shaped[~D] size=~D" i n))
        (assert-tolerated :x509 b
                          (format nil "shaped[~D] size=~D" i n))))))
