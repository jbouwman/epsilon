;;;; Tests for AES Key Wrap (RFC 3394)
;;;;
;;;; The six vectors below are the canonical RFC 3394 §4 test vectors.
;;;; They are the only "pinning" we need for this primitive -- Wrap
;;;; and Unwrap are each other's inverse by construction, so a correct
;;;; ciphertext on any one vector (for each KEK size) is enough to
;;;; prove interop with every other RFC 3394 implementation.

(defpackage epsilon.crypto.aes-kw-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.aes-kw kw)))

(in-package :epsilon.crypto.aes-kw-tests)

(defun h (hex)
  "Decode a whitespace-tolerant hex string to a byte vector."
  (let* ((clean (remove-if (lambda (c) (member c '(#\Space #\Newline #\Tab))) hex))
         (len (floor (length clean) 2))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len out)
      (setf (aref out i)
            (parse-integer clean :start (* 2 i) :end (* 2 (1+ i)) :radix 16)))))

;;; RFC 3394 §4.1: Wrap 128 bits of Key Data with a 128-bit KEK
(defparameter +rfc3394-4.1-kek+  "000102030405060708090A0B0C0D0E0F")
(defparameter +rfc3394-4.1-key+  "00112233445566778899AABBCCDDEEFF")
(defparameter +rfc3394-4.1-wrap+ "1FA68B0A8112B447AEF34BD8FB5A7B829D3E862371D2CFE5")

;;; RFC 3394 §4.2: Wrap 128 bits of Key Data with a 192-bit KEK
(defparameter +rfc3394-4.2-kek+  "000102030405060708090A0B0C0D0E0F1011121314151617")
(defparameter +rfc3394-4.2-key+  "00112233445566778899AABBCCDDEEFF")
(defparameter +rfc3394-4.2-wrap+ "96778B25AE6CA435F92B5B97C050AED2468AB8A17AD84E5D")

;;; RFC 3394 §4.3: Wrap 128 bits of Key Data with a 256-bit KEK
(defparameter +rfc3394-4.3-kek+  "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F")
(defparameter +rfc3394-4.3-key+  "00112233445566778899AABBCCDDEEFF")
(defparameter +rfc3394-4.3-wrap+ "64E8C3F9CE0F5BA263E9777905818A2A93C8191E7D6E8AE7")

;;; RFC 3394 §4.4: Wrap 192 bits of Key Data with a 192-bit KEK
(defparameter +rfc3394-4.4-kek+  "000102030405060708090A0B0C0D0E0F1011121314151617")
(defparameter +rfc3394-4.4-key+  "00112233445566778899AABBCCDDEEFF0001020304050607")
(defparameter +rfc3394-4.4-wrap+
  "031D33264E15D33268F24EC260743EDCE1C6C7DDEE725A936BA814915C6762D2")

;;; RFC 3394 §4.5: Wrap 192 bits of Key Data with a 256-bit KEK
(defparameter +rfc3394-4.5-kek+  "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F")
(defparameter +rfc3394-4.5-key+  "00112233445566778899AABBCCDDEEFF0001020304050607")
(defparameter +rfc3394-4.5-wrap+
  "A8F9BC1612C68B3FF6E6F4FBE30E71E4769C8B80A32CB8958CD5D17D6B254DA1")

;;; RFC 3394 §4.6: Wrap 256 bits of Key Data with a 256-bit KEK
(defparameter +rfc3394-4.6-kek+  "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F")
(defparameter +rfc3394-4.6-key+  "00112233445566778899AABBCCDDEEFF000102030405060708090A0B0C0D0E0F")
(defparameter +rfc3394-4.6-wrap+
  "28C9F404C4B810F4CBCCB35CFB87F8263F5786E2D80ED326CBC7F0E71A99F43BFB988B9B7A02DD21")

(defmacro def-wrap-test (name kek key wrap)
  `(deftest ,name
     ,(format nil "RFC 3394 vector ~A: wrap matches, unwrap is the inverse" name)
     (let ((kek-bytes (h ,kek))
           (key-bytes (h ,key))
           (expected (h ,wrap)))
       (let ((wrapped (kw:aes-key-wrap kek-bytes key-bytes)))
         (assert-equalp wrapped expected)
         (assert-equalp (kw:aes-key-unwrap kek-bytes wrapped) key-bytes)))))

(def-wrap-test test-rfc3394-4.1-128kek-128key +rfc3394-4.1-kek+ +rfc3394-4.1-key+ +rfc3394-4.1-wrap+)
(def-wrap-test test-rfc3394-4.2-192kek-128key +rfc3394-4.2-kek+ +rfc3394-4.2-key+ +rfc3394-4.2-wrap+)
(def-wrap-test test-rfc3394-4.3-256kek-128key +rfc3394-4.3-kek+ +rfc3394-4.3-key+ +rfc3394-4.3-wrap+)
(def-wrap-test test-rfc3394-4.4-192kek-192key +rfc3394-4.4-kek+ +rfc3394-4.4-key+ +rfc3394-4.4-wrap+)
(def-wrap-test test-rfc3394-4.5-256kek-192key +rfc3394-4.5-kek+ +rfc3394-4.5-key+ +rfc3394-4.5-wrap+)
(def-wrap-test test-rfc3394-4.6-256kek-256key +rfc3394-4.6-kek+ +rfc3394-4.6-key+ +rfc3394-4.6-wrap+)

(deftest test-aes-kw-unwrap-rejects-tampered-iv
  "Flipping a byte of a valid wrap makes unwrap return NIL"
  (let* ((kek (h +rfc3394-4.1-kek+))
         (key (h +rfc3394-4.1-key+))
         (wrapped (kw:aes-key-wrap kek key))
         (tampered (copy-seq wrapped)))
    (setf (aref tampered 0) (logxor (aref tampered 0) 1))
    (assert-nil (kw:aes-key-unwrap kek tampered))))

(deftest test-aes-kw-rejects-bad-lengths
  "Invalid KEK and plaintext lengths are signalled"
  (assert-condition (error)
    (kw:aes-key-wrap (make-array 20 :element-type '(unsigned-byte 8))
                     (make-array 16 :element-type '(unsigned-byte 8))))
  (assert-condition (error)
    (kw:aes-key-wrap (make-array 16 :element-type '(unsigned-byte 8))
                     (make-array 8  :element-type '(unsigned-byte 8))))
  (assert-condition (error)
    (kw:aes-key-wrap (make-array 16 :element-type '(unsigned-byte 8))
                     (make-array 17 :element-type '(unsigned-byte 8)))))
