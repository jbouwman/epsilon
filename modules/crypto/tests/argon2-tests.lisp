;;;; Tests for Argon2id (RFC 9106)

(defpackage epsilon.crypto.argon2-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.argon2 argon2)
   (epsilon.crypto.blake2 blake2)))

(in-package :epsilon.crypto.argon2-tests)

;;;; ---- Building-block tests ----
;;;;
;;;; The Argon2id top-level driver depends on three substantive helpers:
;;;; the variable-length hash H', the parameter pre-hash H0, and the
;;;; compression function G. These tests exercise each in isolation so a
;;;; failing top-level vector test points at a specific subsystem.

(deftest test-argon2-h-prime-short-output
  "H'(X, T <= 64) is just Blake2b-T(LE32(T) || X)."
  ;; The short-output branch is testable against Blake2b directly.
  ;; H'(\"abc\", 32) = Blake2b-32(LE32(32) || \"abc\")
  (let* ((x (string-to-bytes "abc"))
         (out (argon2:%h-prime x 32)))
    (assert-= (length out) 32)))

(deftest test-argon2-h-prime-long-output
  "H'(X, T > 64) returns exactly T bytes via the iterated construction."
  (let* ((x (string-to-bytes "abc")))
    (dolist (n '(65 96 128 200 1024))
      (let ((out (argon2:%h-prime x n)))
        (assert-= (length out) n)))))

(deftest test-argon2-h-prime-deterministic
  "H' is a pure function: same inputs -> identical output."
  (let ((x (string-to-bytes "deterministic")))
    (assert-true (equalp (argon2:%h-prime x 64)
                         (argon2:%h-prime x 64)))
    (assert-true (equalp (argon2:%h-prime x 200)
                         (argon2:%h-prime x 200)))))

(deftest test-argon2-block-bytes-roundtrip
  "bytes-to-block / block-to-bytes are inverses on a 1024-byte buffer."
  (let ((bytes (make-array 1024 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 1024 do (setf (aref bytes i) (mod (* i 13) 256)))
    (let ((block (argon2:%bytes-to-block bytes)))
      (assert-true (equalp (argon2:%block-to-bytes block) bytes)))))

(deftest test-argon2-compress-g-zero-zero
  "G(0, 0) is well-defined and returns a 1024-byte block. Specifically,
   when both inputs are zero blocks the result is also a fresh block of
   the right size, and the function does not signal an error."
  (let* ((z (argon2:%zero-block))
         (out (argon2:%compress-g z z)))
    (assert-= (length out) 128)))

(deftest test-argon2-compress-g-deterministic
  "G is a pure function: same inputs -> identical output."
  (let* ((bytes (make-array 1024 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 1024 do (setf (aref bytes i) (mod (* i 7) 256)))
    (let* ((b (argon2:%bytes-to-block bytes))
           (z (argon2:%zero-block))
           (out1 (argon2:%compress-g b z))
           (out2 (argon2:%compress-g b z)))
      (assert-true (equalp out1 out2)))))

(deftest test-argon2-h0-input-blob-rfc9106-vector
  "Pin the exact byte sequence that gets hashed to produce H0 for the
   RFC 9106 §5.3 vector. If H0 is wrong, this test isolates whether the
   bug is in the byte assembly or in the Blake2b call."
  (let* ((password (make-array 32 :element-type '(unsigned-byte 8)
                                  :initial-element #x01))
         (salt     (make-array 16 :element-type '(unsigned-byte 8)
                                  :initial-element #x02))
         (secret   (make-array  8 :element-type '(unsigned-byte 8)
                                  :initial-element #x03))
         (ad       (make-array 12 :element-type '(unsigned-byte 8)
                                  :initial-element #x04))
         (blob (argon2:%h0-input-blob password salt secret ad
                                      4 32 32 3))
         ;; Construct the expected blob byte-by-byte from the RFC 9106
         ;; §3.2 layout. 6 LE32 parameter words + 4 length-prefixed
         ;; byte vectors.
         (expected (concatenate '(simple-array (unsigned-byte 8) (*))
                                ;; LE32(p=4)
                                #(4 0 0 0)
                                ;; LE32(T=32)
                                #(32 0 0 0)
                                ;; LE32(m=32)
                                #(32 0 0 0)
                                ;; LE32(t=3)
                                #(3 0 0 0)
                                ;; LE32(v=0x13=19)
                                #(19 0 0 0)
                                ;; LE32(y=2 for Argon2id)
                                #(2 0 0 0)
                                ;; LE32(|P|=32) || P
                                #(32 0 0 0) password
                                ;; LE32(|S|=16) || S
                                #(16 0 0 0) salt
                                ;; LE32(|K|=8) || K
                                #(8 0 0 0)  secret
                                ;; LE32(|X|=12) || X
                                #(12 0 0 0) ad)))
    (assert-= (length blob) (length expected))
    (assert-true (equalp blob expected))))

(deftest test-argon2-compute-h0-deterministic
  "H0 is a pure function of its parameters and inputs."
  (let* ((p  (string-to-bytes "p"))
         (s  (string-to-bytes "salt"))
         (k  (make-array 0 :element-type '(unsigned-byte 8)))
         (x  (make-array 0 :element-type '(unsigned-byte 8)))
         (a (argon2:%compute-h0 p s k x 1 32 16 1))
         (b (argon2:%compute-h0 p s k x 1 32 16 1)))
    (assert-= (length a) 64)
    (assert-true (equalp a b))))

;;; RFC 9106 §5.3 -- the canonical Argon2id test vector
;;;
;;; All inputs are short, repetitive byte patterns chosen so that the
;;; reference test vector can be reproduced byte for byte. The "secret"
;;; (K) and "associated data" (X) parameters are exercised here as well.

(deftest test-argon2id-rfc9106-vector
  "RFC 9106 §5.3: Argon2id test vector with secret and associated data."
  (let* ((password (make-array 32 :element-type '(unsigned-byte 8)
                                  :initial-element #x01))
         (salt     (make-array 16 :element-type '(unsigned-byte 8)
                                  :initial-element #x02))
         (secret   (make-array  8 :element-type '(unsigned-byte 8)
                                  :initial-element #x03))
         (ad       (make-array 12 :element-type '(unsigned-byte 8)
                                  :initial-element #x04))
         (expected-hex
           "0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659")
         (tag (argon2:argon2id password salt
                               :iterations 3
                               :memory-kb 32
                               :parallelism 4
                               :tag-length 32
                               :secret secret
                               :associated-data ad))
         (actual-hex (bytes-to-hex tag)))
    (unless (equal actual-hex expected-hex)
      (format t "~%>>> Argon2id RFC 9106 §5.3 vector mismatch:~
                 ~%>>>   expected: ~A~
                 ~%>>>   got:      ~A~%"
              expected-hex actual-hex))
    (assert-equal actual-hex expected-hex)))

;;; Round trip / parameter sweep checks. These don't tie us to a specific
;;; reference vector but catch regressions in any of the parameter handling
;;; paths (different tag length, single-pass, single-lane, etc.).

(deftest test-argon2id-no-secret-no-ad
  "Without :secret or :associated-data the helper still produces a stable
   tag of the requested length."
  (let* ((password (string-to-bytes "correct horse battery staple"))
         (salt     (string-to-bytes "0123456789abcdef")))
    (let ((tag-32 (argon2:argon2id password salt
                                   :iterations 2
                                   :memory-kb 32
                                   :parallelism 1
                                   :tag-length 32)))
      (assert-= (length tag-32) 32)
      ;; Same inputs produce identical output (deterministic).
      (let ((tag-32-again (argon2:argon2id password salt
                                           :iterations 2
                                           :memory-kb 32
                                           :parallelism 1
                                           :tag-length 32)))
        (assert-true (equalp tag-32 tag-32-again))))))

(deftest test-argon2id-tag-length-variants
  "Variable output length: 16, 32, 64, 128 bytes -- the 64-byte threshold
   is the boundary at which the H' helper transitions from a single
   Blake2b-T call to its iterated multi-block construction."
  (let ((password (string-to-bytes "p"))
        (salt     (string-to-bytes "ssssssssssssssss")))
    (dolist (n '(16 32 64 65 128))
      (let ((tag (argon2:argon2id password salt
                                  :iterations 1
                                  :memory-kb 16
                                  :parallelism 1
                                  :tag-length n)))
        (assert-= (length tag) n)))))

(deftest test-argon2id-different-salts-differ
  "A change in the salt produces a different tag."
  (let* ((password (string-to-bytes "same password"))
         (s1 (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (s2 (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (_ (setf (aref s2 0) 1))
         (t1 (argon2:argon2id password s1 :iterations 1 :memory-kb 16
                                          :parallelism 1 :tag-length 32))
         (t2 (argon2:argon2id password s2 :iterations 1 :memory-kb 16
                                          :parallelism 1 :tag-length 32)))
    (declare (ignore _))
    (assert-not (equalp t1 t2))))
