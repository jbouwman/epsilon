;;;; Tests for AES-XTS (NIST SP 800-38E / IEEE Std 1619-2007).
;;;;
;;;; Known-answer vectors are from IEEE 1619-2007 Annex D.4.

(defpackage epsilon.crypto.aes-xts-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.aes-xts xts)))

(in-package :epsilon.crypto.aes-xts-tests)

(defun %hex (s) (hex-to-bytes s))

;;; ---------------------------------------------------------------------------
;;; IEEE 1619 Vector 1 (AES-128-XTS, all-zero inputs)
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-128-vector-1
  "IEEE 1619 D.4.1 Vector 1: all-zero key, DUN=0, all-zero plaintext"
  (let* ((key (%hex (concatenate 'string
                                 ;; K1
                                 "00000000000000000000000000000000"
                                 ;; K2
                                 "00000000000000000000000000000000")))
         (dun 0)
         (plaintext (%hex (concatenate 'string
                                       "00000000000000000000000000000000"
                                       "00000000000000000000000000000000")))
         (expected-ct (%hex (concatenate 'string
                                         "917cf69ebd68b2ec9b9fe9a3eadda692"
                                         "cd43d2f59598ed858c02c2652fbf922e"))))
    (let ((ct (xts:aes-xts-encrypt plaintext key dun)))
      (assert-true (equalp ct expected-ct))
      ;; Decrypt round-trip
      (let ((pt (xts:aes-xts-decrypt ct key dun)))
        (assert-true (equalp pt plaintext))))))

;;; ---------------------------------------------------------------------------
;;; IEEE 1619 Vector 2 (AES-128-XTS, 128-bit DUN value, all-0x44 PT)
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-128-vector-2
  "IEEE 1619 D.4.1 Vector 2: K1=0x11..., K2=0x22..., DUN=0x3333333333"
  (let* ((key (%hex (concatenate 'string
                                 "11111111111111111111111111111111"
                                 "22222222222222222222222222222222")))
         (dun #x3333333333)
         (plaintext (%hex (concatenate 'string
                                       "44444444444444444444444444444444"
                                       "44444444444444444444444444444444")))
         (expected-ct (%hex (concatenate 'string
                                         "c454185e6a16936e39334038acef838b"
                                         "fb186fff7480adc4289382ecd6d394f0"))))
    (let ((ct (xts:aes-xts-encrypt plaintext key dun)))
      (assert-true (equalp ct expected-ct))
      (let ((pt (xts:aes-xts-decrypt ct key dun)))
        (assert-true (equalp pt plaintext))))))

;;; ---------------------------------------------------------------------------
;;; IEEE 1619 Vector 3 (AES-128-XTS, K1=0xfffe..., K2=0x22..., DUN=0x3333333333,
;;; plaintext = 32 bytes 0x44...)
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-128-vector-3
  "IEEE 1619 D.4.1 Vector 3: K1=0xfffe..., K2=0x22..., DUN=0x3333333333"
  (let* ((key (%hex (concatenate 'string
                                 ;; K1: derived from cipher key 1
                                 "fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0"
                                 "22222222222222222222222222222222")))
         (dun #x3333333333)
         (plaintext (%hex (concatenate 'string
                                       "44444444444444444444444444444444"
                                       "44444444444444444444444444444444")))
         (expected-ct (%hex (concatenate 'string
                                         "af85336b597afc1a900b2eb21ec949d2"
                                         "92df4c047e0b21532186a5971a227a89"))))
    (let ((ct (xts:aes-xts-encrypt plaintext key dun)))
      (assert-true (equalp ct expected-ct))
      (let ((pt (xts:aes-xts-decrypt ct key dun)))
        (assert-true (equalp pt plaintext))))))

;;; ---------------------------------------------------------------------------
;;; Sanity: distinct DUNs produce distinct ciphertexts
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-distinct-dun-distinct-ciphertext
  "Different data unit numbers under the same key yield different output"
  (let* ((key (%hex (concatenate 'string
                                 "00112233445566778899aabbccddeeff"
                                 "0102030405060708090a0b0c0d0e0f10")))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "0123456789abcdef0123456789abcdef")))
    (let ((c0 (xts:aes-xts-encrypt plaintext key 0))
          (c1 (xts:aes-xts-encrypt plaintext key 1)))
      (assert-not (equalp c0 c1))
      ;; Decryption recovers plaintext for each DUN
      (assert-true (equalp plaintext (xts:aes-xts-decrypt c0 key 0)))
      (assert-true (equalp plaintext (xts:aes-xts-decrypt c1 key 1))))))

;;; ---------------------------------------------------------------------------
;;; Multi-block (no ciphertext stealing)
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-128-many-full-blocks-roundtrip
  "AES-128-XTS round-trips a 256-byte data unit (16 full blocks)"
  (let* ((key (%hex (concatenate 'string
                                 "27182818284590452353602874713526"
                                 "31415926535897932384626433832795")))
         (dun 1)
         (plaintext (let ((b (make-array 256 :element-type '(unsigned-byte 8))))
                      (loop for i from 0 below 256
                            do (setf (aref b i) (logand #xFF i)))
                      b)))
    (let ((ct (xts:aes-xts-encrypt plaintext key dun)))
      (assert-= 256 (length ct))
      ;; Output differs from plaintext (sanity).
      (assert-not (equalp ct plaintext))
      (let ((pt (xts:aes-xts-decrypt ct key dun)))
        (assert-true (equalp pt plaintext))))))

;;; ---------------------------------------------------------------------------
;;; AES-256-XTS round-trip
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-256-roundtrip
  "AES-256-XTS (64-byte combined key) encrypts and decrypts correctly"
  (let* ((key (%hex (concatenate 'string
                                 "27182818284590452353602874713526"
                                 "62497757247093699959574966967627"
                                 "31415926535897932384626433832795"
                                 "02884197169399375105820974944592")))
         (dun #xff)
         (plaintext (let ((b (make-array 64 :element-type '(unsigned-byte 8))))
                      (loop for i from 0 below 64
                            do (setf (aref b i) (logand #xFF (* i 7))))
                      b)))
    (let ((ct (xts:aes-xts-encrypt plaintext key dun)))
      (assert-= 64 (length ct))
      (assert-not (equalp ct plaintext))
      (assert-true (equalp plaintext (xts:aes-xts-decrypt ct key dun))))))

;;; ---------------------------------------------------------------------------
;;; Ciphertext stealing -- non-multiple-of-16 lengths.
;;; Output length matches input length.
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-ciphertext-stealing-roundtrip
  "AES-XTS round-trips any length >= 16 bytes (ciphertext stealing path)"
  (let ((key (%hex (concatenate 'string
                                "27182818284590452353602874713526"
                                "31415926535897932384626433832795"))))
    (loop for n from 16 to 64
          for plaintext = (let ((b (make-array n :element-type '(unsigned-byte 8))))
                            (loop for i from 0 below n
                                  do (setf (aref b i) (logand #xFF (+ i 1))))
                            b)
          for ct = (xts:aes-xts-encrypt plaintext key 42)
          for pt = (xts:aes-xts-decrypt ct key 42)
          do (assert-= n (length ct))
             (assert-true (equalp pt plaintext)))))

;;; ---------------------------------------------------------------------------
;;; Parameter validation
;;; ---------------------------------------------------------------------------

(deftest test-aes-xts-invalid-key-length
  "AES-XTS rejects key lengths other than 32 or 64 bytes"
  (let ((short-pt (make-array 16 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (assert-condition (error)
      (xts:aes-xts-encrypt short-pt
                           (make-array 16 :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                           0))
    (assert-condition (error)
      (xts:aes-xts-encrypt short-pt
                           (make-array 48 :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                           0))))

(deftest test-aes-xts-rejects-short-input
  "AES-XTS rejects inputs shorter than one block"
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-condition (error)
      (xts:aes-xts-encrypt (make-array 0 :element-type '(unsigned-byte 8))
                           key 0))
    (assert-condition (error)
      (xts:aes-xts-encrypt (make-array 15 :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                           key 0))))
