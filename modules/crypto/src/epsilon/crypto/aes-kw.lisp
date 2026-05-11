;;;; AES Key Wrap (RFC 3394)
;;;;
;;;; A symmetric algorithm for wrapping (encrypting) one AES key under
;;;; another. The JOSE spec identifies three variants by KEK size:
;;;; A128KW (16-byte KEK), A192KW (24-byte KEK), A256KW (32-byte KEK).
;;;; The plaintext to be wrapped must be a multiple of 8 bytes and at
;;;; least 16 bytes (two 64-bit blocks). The wrapped output is always
;;;; 8 bytes longer than the input.
;;;;
;;;; Used standalone for `alg=A*KW` in JWE, and as the second stage of
;;;; `alg=ECDH-ES+A*KW` and `alg=PBES2-HS*+A*KW`.
;;;;
;;;; The algorithm operates on 64-bit half-blocks. Wrap threads the
;;;; plaintext through six passes of the AES block cipher, XORing a
;;;; running counter into the high half each time. Unwrap runs the
;;;; same construction backwards and checks that the recovered IV
;;;; matches the RFC 3394 default `A6A6A6A6A6A6A6A6`. A bad IV signals
;;;; tampering or a wrong KEK; the function returns NIL in that case
;;;; rather than a partial recovery, and callers in JWE treat NIL as
;;;; an authentication failure.

(defpackage epsilon.crypto.aes-kw
  (:use :cl)
  (:import (epsilon.crypto.aes aes))
  (:import-from :epsilon.crypto.primitives #:ensure-byte-vector)
  (:export
   #:+default-iv+
   #:aes-key-wrap
   #:aes-key-unwrap))

(in-package :epsilon.crypto.aes-kw)

(defparameter +default-iv+
  (coerce #(#xA6 #xA6 #xA6 #xA6 #xA6 #xA6 #xA6 #xA6)
          '(simple-array (unsigned-byte 8) (*)))
  "The RFC 3394 §2.2.3.1 default initial value. Used when the
   application does not have a reason to pick something else (which
   is the case for every JOSE A*KW profile).")

(defun %valid-kek-length-p (len)
  (or (= len 16) (= len 24) (= len 32)))

(defun %xor-u64-be (block counter)
  "XOR the 64-bit big-endian integer COUNTER into the 8-byte BLOCK in
   place. BLOCK is always the high half of a 16-byte AES block."
  (declare (type (simple-array (unsigned-byte 8) (*)) block)
           (type (unsigned-byte 64) counter))
  (loop for i from 7 downto 0
        for shift from 0 by 8
        do (setf (aref block i)
                 (logxor (aref block i)
                         (ldb (byte 8 shift) counter)))))

(defun aes-key-wrap (kek plaintext)
  "RFC 3394 Key Wrap. KEK is the 16/24/32-byte key-encryption key,
   PLAINTEXT is the material to wrap (must be a positive multiple of
   8 bytes, >= 16 bytes). Returns a fresh byte vector of length
   (+ 8 (length plaintext))."
  (let ((kek (ensure-byte-vector kek))
        (plaintext (ensure-byte-vector plaintext)))
    (unless (%valid-kek-length-p (length kek))
      (error "aes-key-wrap: KEK must be 16, 24, or 32 bytes (got ~D)"
             (length kek)))
    (let ((n (floor (length plaintext) 8)))
      (unless (and (zerop (mod (length plaintext) 8)) (>= n 2))
        (error "aes-key-wrap: plaintext must be a multiple of 8 bytes ~
                and at least 16 bytes (got ~D)" (length plaintext)))
      (let* ((round-keys (aes:make-aes-round-keys kek))
             ;; A is a 64-bit running value, kept as an 8-byte vector.
             (a (copy-seq +default-iv+))
             ;; R[1..n] mirrors the algorithm's indexing. R[0] is unused.
             (r (make-array (1+ n))))
        (loop for i from 1 to n do
          (setf (aref r i)
                (subseq plaintext (* 8 (1- i)) (* 8 i))))
        ;; Six wrap rounds.
        (let ((block (make-array 16 :element-type '(unsigned-byte 8))))
          (dotimes (j 6)
            (loop for i from 1 to n
                  for t-counter of-type (unsigned-byte 64) = (+ (* n j) i)
                  do
                  (replace block a)
                  (replace block (aref r i) :start1 8)
                  (let ((encrypted (aes:aes-encrypt-block block round-keys)))
                    (replace a encrypted :end2 8)
                    (%xor-u64-be a t-counter)
                    (setf (aref r i) (subseq encrypted 8 16))))))
        ;; Concatenate A || R[1] || ... || R[n].
        (let ((out (make-array (* 8 (1+ n))
                               :element-type '(unsigned-byte 8))))
          (replace out a)
          (loop for i from 1 to n do
            (replace out (aref r i) :start1 (* 8 i)))
          out)))))

(defun aes-key-unwrap (kek ciphertext)
  "RFC 3394 Key Unwrap. Returns the recovered plaintext on success, or
   NIL if the integrity check (the IV comparison at the end) fails.
   CIPHERTEXT must be a multiple of 8 bytes and at least 24 bytes long."
  (let ((kek (ensure-byte-vector kek))
        (ciphertext (ensure-byte-vector ciphertext)))
    (unless (%valid-kek-length-p (length kek))
      (error "aes-key-unwrap: KEK must be 16, 24, or 32 bytes (got ~D)"
             (length kek)))
    (let ((len (length ciphertext)))
      (unless (and (zerop (mod len 8)) (>= len 24))
        (error "aes-key-unwrap: ciphertext must be a multiple of 8 ~
                bytes and at least 24 bytes (got ~D)" len))
      (let* ((n (1- (floor len 8)))
             (dec-keys (aes:make-aes-decrypt-round-keys kek))
             (a (subseq ciphertext 0 8))
             (r (make-array (1+ n))))
        (loop for i from 1 to n do
          (setf (aref r i) (subseq ciphertext (* 8 i) (* 8 (1+ i)))))
        ;; Six unwrap rounds, indices reversed relative to wrap.
        (let ((block (make-array 16 :element-type '(unsigned-byte 8))))
          (loop for j from 5 downto 0 do
            (loop for i from n downto 1
                  for t-counter of-type (unsigned-byte 64) = (+ (* n j) i)
                  do
                  (replace block a)
                  (%xor-u64-be block t-counter)
                  (replace block (aref r i) :start1 8)
                  (let ((decrypted (aes:aes-decrypt-block block dec-keys)))
                    (replace a decrypted :end2 8)
                    (setf (aref r i) (subseq decrypted 8 16))))))
        ;; Verify IV.
        (if (equalp a +default-iv+)
            (let ((out (make-array (* 8 n) :element-type '(unsigned-byte 8))))
              (loop for i from 1 to n do
                (replace out (aref r i) :start1 (* 8 (1- i))))
              out)
            nil)))))
