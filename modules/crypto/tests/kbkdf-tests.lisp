;;;; Tests for KBKDF counter mode (NIST SP 800-108)

(defpackage epsilon.crypto.kbkdf-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.kbkdf kbkdf)
           (epsilon.crypto.hmac hmac)))

(in-package :epsilon.crypto.kbkdf-tests)

(defun %hex (s) (hex-to-bytes s))

;;; ---------------------------------------------------------------------------
;;; NIST CAVS test vector: PRF=HMAC_SHA256, CTRLOCATION=BEFORE_FIXED, RLEN=32
;;;
;;; KI            = dd1d91b7d90b2bd3138533ce92b272fbf8a369316aefe242e659cc0ae238afe0
;;; FixedInputData= 01322b96b30acd197979444e468e1c5c6859bf1b1cf951b7e7
;;;                 25303e237e46b864a145fab25e517b08f8683d0315bb2911d8
;;;                 0a0e8aba17f3b413faac
;;; L  (out bits) = 128
;;; KO (16 bytes) = 10621342bfb0fd40046c0e29f2cfdbf0
;;;
;;; Source: NIST CAVP KBKDF Sample (Counter Mode, HMAC-SHA-256, before-fixed,
;;; 32-bit counter, single-block output).
;;; ---------------------------------------------------------------------------

(deftest test-kbkdf-counter-hmac-sha256-cavs-vector
  "NIST CAVP HMAC-SHA-256 KBKDF counter-mode sample (single block)"
  (let* ((ki (%hex
              "dd1d91b7d90b2bd3138533ce92b272fbf8a369316aefe242e659cc0ae238afe0"))
         (fixed-input (%hex
                       (concatenate 'string
                                    "01322b96b30acd197979444e468e1c5c"
                                    "6859bf1b1cf951b7e725303e237e46b8"
                                    "64a145fab25e517b08f8683d0315bb29"
                                    "11d80a0e8aba17f3b413faac")))
         (out (kbkdf:kbkdf-counter-hmac-sha256 ki fixed-input 16))
         (expected (%hex "10621342bfb0fd40046c0e29f2cfdbf0")))
    (assert-true (equalp out expected))))

;;; ---------------------------------------------------------------------------
;;; Multi-block output (forces n>1) -- check length and that the first
;;; chunk equals the single-block output.
;;; ---------------------------------------------------------------------------

(deftest test-kbkdf-counter-hmac-sha256-multi-block
  "Output spanning multiple PRF blocks is the leftmost L bytes of K(1)..K(n)"
  (let* ((ki (%hex
              "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff"))
         (fixed-input (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                           "label||separator||context"))
         ;; HMAC-SHA-256 emits 32 bytes; ask for 80 bytes -> n=3 iterations.
         (out80 (kbkdf:kbkdf-counter-hmac-sha256 ki fixed-input 80))
         (out32 (kbkdf:kbkdf-counter-hmac-sha256 ki fixed-input 32)))
    (assert-= 80 (length out80))
    (assert-= 32 (length out32))
    ;; The first PRF call's output is identical regardless of total L.
    (assert-true (equalp out32 (subseq out80 0 32)))))

;;; ---------------------------------------------------------------------------
;;; SP 800-108 default form (label || 0x00 || context || [L]_32)
;;; spot-check: derive with the high-level helper, confirm we get the same
;;; bytes by manually constructing the fixed-input blob.
;;; ---------------------------------------------------------------------------

(deftest test-kbkdf-with-label-context-matches-manual-fixed-input
  "kbkdf-counter-hmac-sha256-with-label-context emits the SP 800-108 form"
  (let* ((ki (%hex
              "0102030405060708090a0b0c0d0e0f101112131314151617181920212223242526272829"))
         (label (map '(simple-array (unsigned-byte 8) (*)) #'char-code "MyLabel"))
         (context (map '(simple-array (unsigned-byte 8) (*)) #'char-code "ctx-v1"))
         (l-bits #x000000c0)  ; 192 bits = 24 bytes
         ;; Manual: label || 0x00 || context || [L]_32 BE
         (separator (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (l-bytes (let ((b (make-array 4 :element-type '(unsigned-byte 8))))
                    (setf (aref b 0) (logand #xFF (ash l-bits -24)))
                    (setf (aref b 1) (logand #xFF (ash l-bits -16)))
                    (setf (aref b 2) (logand #xFF (ash l-bits -8)))
                    (setf (aref b 3) (logand #xFF l-bits))
                    b))
         (fixed-input (concatenate '(simple-array (unsigned-byte 8) (*))
                                   label separator context l-bytes))
         (auto (kbkdf:kbkdf-counter-hmac-sha256-with-label-context
                ki label context 24))
         (manual (kbkdf:kbkdf-counter-hmac-sha256 ki fixed-input 24)))
    (assert-= 24 (length auto))
    (assert-true (equalp auto manual))))

;;; ---------------------------------------------------------------------------
;;; Cross-check against direct HMAC: for L <= PRF block size, the KBKDF
;;; counter-mode output is exactly the prefix of HMAC(K, [1] || FixedInput).
;;; ---------------------------------------------------------------------------

(deftest test-kbkdf-counter-equals-hmac-with-counter-prefix
  "KBKDF/HMAC-SHA-256 single-block output = HMAC-SHA-256(K, [1]_32 || FixedInput)"
  (let* ((ki (%hex
              "deadbeefcafef00d000102030405060708090a0b0c0d0e0f1011121314151617"))
         (fixed-input (%hex "112233445566778899aabbccddeeff"))
         (full (kbkdf:kbkdf-counter-hmac-sha256 ki fixed-input 32))
         (counter (let ((b (make-array 4 :element-type '(unsigned-byte 8))))
                    (setf (aref b 0) 0)
                    (setf (aref b 1) 0)
                    (setf (aref b 2) 0)
                    (setf (aref b 3) 1)
                    b))
         (expected (hmac:hmac-sha256
                    ki
                    (concatenate '(simple-array (unsigned-byte 8) (*))
                                 counter fixed-input))))
    (assert-true (equalp full expected))))

;;; ---------------------------------------------------------------------------
;;; SHA-384 / SHA-512 PRF variants behave the same way.
;;; ---------------------------------------------------------------------------

(deftest test-kbkdf-counter-hmac-sha384-output-length
  "HMAC-SHA-384 KBKDF emits requested number of bytes"
  (let* ((ki (%hex (concatenate 'string
                                "0102030405060708090a0b0c0d0e0f10"
                                "11121314151617181920212223242526")))
         (fixed-input (%hex "010203040506070809")))
    (assert-= 24 (length (kbkdf:kbkdf-counter-hmac-sha384 ki fixed-input 24)))
    (assert-= 48 (length (kbkdf:kbkdf-counter-hmac-sha384 ki fixed-input 48)))
    (assert-= 64 (length (kbkdf:kbkdf-counter-hmac-sha384 ki fixed-input 64)))))

(deftest test-kbkdf-counter-hmac-sha512-output-length
  "HMAC-SHA-512 KBKDF emits requested number of bytes"
  (let* ((ki (%hex (concatenate 'string
                                "0102030405060708090a0b0c0d0e0f10"
                                "11121314151617181920212223242526")))
         (fixed-input (%hex "010203040506070809")))
    (assert-= 32 (length (kbkdf:kbkdf-counter-hmac-sha512 ki fixed-input 32)))
    (assert-= 64 (length (kbkdf:kbkdf-counter-hmac-sha512 ki fixed-input 64)))
    (assert-= 100 (length (kbkdf:kbkdf-counter-hmac-sha512 ki fixed-input 100)))))
