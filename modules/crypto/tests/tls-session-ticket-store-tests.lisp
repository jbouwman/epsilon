;;;; Tests for the TLS session ticket encryption key (STEK) store.

(defpackage epsilon.crypto.tls-session-ticket-store-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.tls-session-ticket-store stek)))

(in-package :epsilon.crypto.tls-session-ticket-store-tests)

(defun %make-plaintext (&optional (n 64))
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (* 7 i) 256)))
    v))

(deftest test-stek-store-creation
  "A fresh store has exactly one current key and no previous key."
  (let ((store (stek:make-stek-store)))
    (assert-not-null (stek:stek-store-current store))
    (assert-nil (stek:stek-store-previous store))))

(deftest test-stek-seal-then-open-roundtrip
  "Sealing a plaintext and immediately opening it returns the same bytes."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 80))
         (sealed (stek:stek-seal store plain))
         (opened (stek:stek-open store sealed)))
    (assert-true (typep sealed '(simple-array (unsigned-byte 8) (*))))
    ;; Sealed must be longer (key-id + nonce + tag overhead).
    (assert-true (> (length sealed) (length plain)))
    (assert-equalp opened plain)))

(deftest test-stek-seal-different-each-time
  "Sealing the same plaintext twice produces different ciphertext (random nonce)."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 32))
         (a (stek:stek-seal store plain))
         (b (stek:stek-seal store plain)))
    (assert-not (equalp a b))))

(deftest test-stek-rotation-preserves-old-tickets
  "After one rotation, tickets sealed with the old key still open."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 50))
         (old-sealed (stek:stek-seal store plain)))
    (stek:stek-rotate store)
    ;; New current key, previous slot now holds the prior current key.
    (assert-not-null (stek:stek-store-previous store))
    ;; Old ticket still opens.
    (assert-equalp (stek:stek-open store old-sealed) plain)
    ;; New seals work too and use the new current key.
    (let ((new-sealed (stek:stek-seal store plain)))
      (assert-equalp (stek:stek-open store new-sealed) plain))))

(deftest test-stek-double-rotation-expires-oldest
  "After two rotations, tickets sealed with the original key fail to open."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 50))
         (oldest (stek:stek-seal store plain)))
    (stek:stek-rotate store)
    (stek:stek-rotate store)
    (assert-nil (stek:stek-open store oldest))))

(deftest test-stek-tampered-ticket-rejected
  "A ticket with a flipped byte in the ciphertext fails to open (returns NIL)."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 40))
         (sealed (stek:stek-seal store plain))
         (tampered (copy-seq sealed)))
    ;; Flip a byte in the AEAD ciphertext region (past the header).
    (setf (aref tampered (- (length tampered) 1))
          (logxor (aref tampered (- (length tampered) 1)) 1))
    (assert-nil (stek:stek-open store tampered))))

(deftest test-stek-truncated-ticket-rejected
  "A truncated ticket fails to open."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 40))
         (sealed (stek:stek-seal store plain))
         (short (subseq sealed 0 (- (length sealed) 5))))
    (assert-nil (stek:stek-open store short))))

(deftest test-stek-unknown-key-id-rejected
  "A ticket whose key-id doesn't match any current/previous key returns NIL."
  (let* ((store (stek:make-stek-store))
         (plain (%make-plaintext 40))
         (sealed (stek:stek-seal store plain))
         (mangled (copy-seq sealed)))
    ;; Corrupt the key-id (bytes 1..4, after the 1-byte version).
    (setf (aref mangled 1) (logxor (aref mangled 1) #xFF))
    (assert-nil (stek:stek-open store mangled))))
