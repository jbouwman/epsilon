;;;; Tests for HMAC-SHA256 implementation
;;;;
;;;; Test vectors from RFC 4231

(defpackage epsilon.ssl.hmac-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:hmac #:epsilon.ssl.hmac)
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.hmac-tests)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun make-repeated-byte (byte count)
  "Create a byte array of COUNT copies of BYTE."
  (make-array count :element-type '(unsigned-byte 8) :initial-element byte))

;;; ---------------------------------------------------------------------------
;;; RFC 4231 Test Vectors for HMAC-SHA256
;;; ---------------------------------------------------------------------------

(deftest test-hmac-sha256-rfc4231-case1
  "RFC 4231 Test Case 1: Short key and data"
  ;; Key = 0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b (20 bytes)
  ;; Data = "Hi There"
  ;; HMAC-SHA-256 = b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7
  (let* ((key (make-repeated-byte #x0b 20))
         (data (let ((s "Hi There"))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (expected (hex-to-bytes "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-hmac-sha256-rfc4231-case2
  "RFC 4231 Test Case 2: Key = 'Jefe', Data = 'what do ya want for nothing?'"
  ;; HMAC-SHA-256 = 5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843
  (let* ((key (let ((s "Jefe"))
                (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                  (loop for i from 0 below (length s)
                        do (setf (aref b i) (char-code (char s i))))
                  b)))
         (data (let ((s "what do ya want for nothing?"))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (expected (hex-to-bytes "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-hmac-sha256-rfc4231-case3
  "RFC 4231 Test Case 3: Key and data are 0xaa/0xdd repeated"
  ;; Key = 0xaaaa... (20 bytes)
  ;; Data = 0xdddd... (50 bytes)
  ;; HMAC-SHA-256 = 773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe
  (let* ((key (make-repeated-byte #xaa 20))
         (data (make-repeated-byte #xdd 50))
         (expected (hex-to-bytes "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-hmac-sha256-rfc4231-case4
  "RFC 4231 Test Case 4: Key = 0x0102...19, Data = 0xcdcd... (50 bytes)"
  ;; Key = 0x0102030405060708090a0b0c0d0e0f10111213141516171819 (25 bytes)
  ;; Data = 0xcdcd... (50 bytes)
  ;; HMAC-SHA-256 = 82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b
  (let* ((key (let ((b (make-array 25 :element-type '(unsigned-byte 8))))
                (loop for i from 0 below 25
                      do (setf (aref b i) (1+ i)))
                b))
         (data (make-repeated-byte #xcd 50))
         (expected (hex-to-bytes "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-hmac-sha256-rfc4231-case6
  "RFC 4231 Test Case 6: Long key (131 bytes of 0xaa)"
  ;; Key = 0xaaaa... (131 bytes, longer than block size)
  ;; Data = "Test Using Larger Than Block-Size Key - Hash Key First"
  ;; HMAC-SHA-256 = 60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54
  (let* ((key (make-repeated-byte #xaa 131))
         (data (let ((s "Test Using Larger Than Block-Size Key - Hash Key First"))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (expected (hex-to-bytes "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-hmac-sha256-rfc4231-case7
  "RFC 4231 Test Case 7: Long key and long data"
  ;; Key = 0xaaaa... (131 bytes)
  ;; Data = "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."
  ;; HMAC-SHA-256 = 9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2
  (let* ((key (make-repeated-byte #xaa 131))
         (data (let ((s "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (expected (hex-to-bytes "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"))
         (result (hmac:hmac-sha256 key data)))
    (assert-true (ct:ct-equal result expected))))

;;; ---------------------------------------------------------------------------
;;; Verification tests
;;; ---------------------------------------------------------------------------

(deftest test-hmac-sha256-verify-valid
  "hmac-sha256-verify should return T for valid tag"
  (let* ((key (make-repeated-byte #x0b 20))
         (data (let ((s "Hi There"))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (tag (hmac:hmac-sha256 key data)))
    (assert-true (hmac:hmac-sha256-verify key data tag))))

(deftest test-hmac-sha256-verify-invalid
  "hmac-sha256-verify should return NIL for tampered tag"
  (let* ((key (make-repeated-byte #x0b 20))
         (data (let ((s "Hi There"))
                 (let ((b (make-array (length s) :element-type '(unsigned-byte 8))))
                   (loop for i from 0 below (length s)
                         do (setf (aref b i) (char-code (char s i))))
                   b)))
         (tag (hmac:hmac-sha256 key data)))
    ;; Flip one bit in the tag
    (setf (aref tag 0) (logxor (aref tag 0) 1))
    (assert-not (hmac:hmac-sha256-verify key data tag))))
