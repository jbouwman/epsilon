;;;; Tests for SCT parsing (RFC 6962 Certificate Transparency).
;;;;
;;;; We construct synthetic SCTs/SCTLists by hand because real CT log
;;;; vectors rotate over time and would either bind us to a specific
;;;; log's keypair or require pinning a real cert.

(defpackage epsilon.crypto.sct-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.sct sct)))

(in-package :epsilon.crypto.sct-tests)

(defun %u8 (n) (logand #xFF n))
(defun %u16-be (n)
  (let ((b (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref b 0) (logand #xFF (ash n -8)))
    (setf (aref b 1) (logand #xFF n))
    b))
(defun %u64-be (n)
  (let ((b (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for i from 7 downto 0
          for shift from 0 by 8
          do (setf (aref b i) (logand #xFF (ash n (- shift)))))
    b))

(defun %concat (&rest pieces)
  (let* ((total (reduce #'+ pieces :key #'length))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (off 0))
    (dolist (p pieces)
      (replace out p :start1 off)
      (incf off (length p)))
    out))

(defun %make-sct-bytes (&key (version 0)
                            log-id
                            (timestamp 0)
                            (extensions #())
                            (hash-alg 4)        ; sha256
                            (sig-alg 3)         ; ecdsa
                            (signature #()))
  "Encode one SCT body (no length prefix) per RFC 6962 §3.2."
  (let ((ver (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element (%u8 version)))
        (ext-len (%u16-be (length extensions)))
        (hash (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-element (%u8 hash-alg)))
        (sig (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element (%u8 sig-alg)))
        (sig-len (%u16-be (length signature)))
        (ts (%u64-be timestamp)))
    (%concat ver
             (coerce log-id '(simple-array (unsigned-byte 8) (*)))
             ts
             ext-len
             (coerce extensions '(simple-array (unsigned-byte 8) (*)))
             hash sig sig-len
             (coerce signature '(simple-array (unsigned-byte 8) (*))))))

(defun %make-sct-list-bytes (&rest sct-byte-vectors)
  "Encode a SignedCertificateTimestampList per RFC 6962 §3.3."
  (let* ((entries
           (mapcar (lambda (b)
                     (%concat (%u16-be (length b)) b))
                   sct-byte-vectors))
         (total (reduce #'+ entries :key #'length))
         (out (make-array (+ 2 total) :element-type '(unsigned-byte 8))))
    (replace out (%u16-be total) :start1 0)
    (let ((off 2))
      (dolist (e entries)
        (replace out e :start1 off)
        (incf off (length e))))
    out))

;;; ---------------------------------------------------------------------------
;;; Single SCT round-trip
;;; ---------------------------------------------------------------------------

(deftest test-parse-sct-basic
  "Single SCT body parses with all fields intact"
  (let* ((log-id (hex-to-bytes
                  "5614069a2fd7c2ecd3f5e1bd44b23ec74676b9bc99115cc0ef949855d689d0dd"))
         (timestamp 1700000000123)  ; ms since epoch
         (signature (hex-to-bytes "30440220deadbeefcafebabe0102030405060708"))
         (sct-bytes (%make-sct-bytes :version 0
                                     :log-id log-id
                                     :timestamp timestamp
                                     :extensions (hex-to-bytes "ab")
                                     :hash-alg 4 :sig-alg 3
                                     :signature signature))
         (sct (sct:parse-sct sct-bytes)))
    (assert-= 0 (sct:sct-version sct))
    (assert-true (equalp log-id (sct:sct-log-id sct)))
    (assert-= timestamp (sct:sct-timestamp sct))
    (assert-= 1 (length (sct:sct-extensions sct)))
    (assert-= #xab (aref (sct:sct-extensions sct) 0))
    (assert-= 4 (sct:sct-hash-algorithm sct))
    (assert-= 3 (sct:sct-signature-algorithm sct))
    (assert-equal :sha256 (sct:sct-hash-keyword sct))
    (assert-equal :ecdsa (sct:sct-sig-keyword sct))
    (assert-true (equalp signature (sct:sct-signature sct)))))

;;; ---------------------------------------------------------------------------
;;; Algorithm-keyword mapping covers the common TLS codes
;;; ---------------------------------------------------------------------------

(deftest test-sct-algorithm-keyword-mapping
  "Hash and signature byte codes map to expected keywords"
  (let* ((log-id (make-array 32 :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (mk (lambda (h s)
               (sct:parse-sct
                (%make-sct-bytes :log-id log-id :hash-alg h :sig-alg s)))))
    (assert-equal :md5    (sct:sct-hash-keyword (funcall mk 1 1)))
    (assert-equal :sha1   (sct:sct-hash-keyword (funcall mk 2 1)))
    (assert-equal :sha256 (sct:sct-hash-keyword (funcall mk 4 1)))
    (assert-equal :sha384 (sct:sct-hash-keyword (funcall mk 5 1)))
    (assert-equal :sha512 (sct:sct-hash-keyword (funcall mk 6 1)))
    (assert-equal :rsa    (sct:sct-sig-keyword  (funcall mk 4 1)))
    (assert-equal :dsa    (sct:sct-sig-keyword  (funcall mk 4 2)))
    (assert-equal :ecdsa  (sct:sct-sig-keyword  (funcall mk 4 3)))
    (assert-equal :unknown (sct:sct-sig-keyword (funcall mk 4 99)))))

;;; ---------------------------------------------------------------------------
;;; SCT list parsing (multiple entries, varying signature lengths)
;;; ---------------------------------------------------------------------------

(deftest test-parse-sct-list-multiple-entries
  "Parse a list with three SCTs of different signature lengths"
  (let* ((log-id-1 (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element #x11))
         (log-id-2 (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element #x22))
         (log-id-3 (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element #x33))
         (sct-1 (%make-sct-bytes :log-id log-id-1
                                 :timestamp 100
                                 :signature (hex-to-bytes "deadbeef")))
         (sct-2 (%make-sct-bytes :log-id log-id-2
                                 :timestamp 200
                                 :signature (hex-to-bytes
                                             "30440220a1a2a3a4")))
         (sct-3 (%make-sct-bytes :log-id log-id-3
                                 :timestamp 300
                                 :extensions (hex-to-bytes "abcd")
                                 :signature
                                 (make-array 64
                                             :element-type '(unsigned-byte 8)
                                             :initial-element #xa5)))
         (list-bytes (%make-sct-list-bytes sct-1 sct-2 sct-3))
         (parsed (sct:parse-sct-list list-bytes)))
    (assert-= 3 (length parsed))
    (let ((p1 (first parsed))
          (p2 (second parsed))
          (p3 (third parsed)))
      (assert-= 100 (sct:sct-timestamp p1))
      (assert-= 200 (sct:sct-timestamp p2))
      (assert-= 300 (sct:sct-timestamp p3))
      (assert-true (equalp log-id-1 (sct:sct-log-id p1)))
      (assert-true (equalp log-id-2 (sct:sct-log-id p2)))
      (assert-true (equalp log-id-3 (sct:sct-log-id p3)))
      ;; extensions only on the third entry
      (assert-= 0 (length (sct:sct-extensions p1)))
      (assert-= 2 (length (sct:sct-extensions p3)))
      ;; signature byte counts
      (assert-= 4 (length (sct:sct-signature p1)))
      (assert-= 8 (length (sct:sct-signature p2)))
      (assert-= 64 (length (sct:sct-signature p3))))))

(deftest test-parse-sct-list-empty
  "An empty SCT list parses to NIL"
  (let* ((bytes (%make-sct-list-bytes))
         (parsed (sct:parse-sct-list bytes)))
    (assert-true (null parsed))))

;;; ---------------------------------------------------------------------------
;;; Raw-bytes round-trip: an SCT's RAW-BYTES slot should equal the input
;;; bytes for that body.
;;; ---------------------------------------------------------------------------

(deftest test-sct-raw-bytes-round-trip
  "sct-raw-bytes preserves the exact wire bytes of the entry"
  (let* ((log-id (make-array 32 :element-type '(unsigned-byte 8)
                             :initial-element #x77))
         (sct-bytes (%make-sct-bytes :log-id log-id :timestamp 12345
                                     :signature (hex-to-bytes "010203")))
         (sct (sct:parse-sct sct-bytes)))
    (assert-true (equalp sct-bytes (sct:sct-raw-bytes sct)))))
