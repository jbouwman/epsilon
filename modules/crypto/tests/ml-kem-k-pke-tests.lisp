;;;; Tests for K-PKE (FIPS 203 Algorithms 12-14)

(defpackage epsilon.crypto.ml-kem-k-pke-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.ml-kem-k-pke k-pke)))

(in-package :epsilon.crypto.ml-kem-k-pke-tests)

;;; ---- Helpers ----

(defun deterministic-bytes (length seed)
  "A reproducible pseudo-random byte vector of the given LENGTH. Used
   instead of kernel entropy so tests are reproducible from run to run."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (state (+ seed 1)))
    (loop for i from 0 below length do
      (setf state (mod (+ (* state 1103515245) 12345) (expt 2 32)))
      (setf (aref out i) (ldb (byte 8 16) state)))
    out))

;;; ---- Byte length contracts ----

(deftest test-k-pke-byte-lengths
  "The ML-KEM-768 byte lengths exported by the K-PKE module match the
   FIPS 203 specification exactly: ek_PKE = 1184, dk_PKE = 1152,
   ciphertext = 1088, message = 32."
  (assert-= k-pke:+ek-pke-length+ 1184)
  (assert-= k-pke:+dk-pke-length+ 1152)
  (assert-= k-pke:+ciphertext-length+ 1088)
  (assert-= k-pke:+message-length+ 32))

(deftest test-k-pke-key-gen-output-shape
  "KeyGen returns byte vectors of the specified lengths."
  (let ((seed (deterministic-bytes 32 1)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (assert-= (length ek) k-pke:+ek-pke-length+)
      (assert-= (length dk) k-pke:+dk-pke-length+))))

(deftest test-k-pke-encrypt-output-shape
  "Encrypt returns a ciphertext of the specified length."
  (let* ((seed (deterministic-bytes 32 2))
         (message (deterministic-bytes 32 100))
         (r (deterministic-bytes 32 101)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (declare (ignore dk))
      (let ((ct (k-pke:encrypt ek message r)))
        (assert-= (length ct) k-pke:+ciphertext-length+)))))

;;; ---- Determinism ----

(deftest test-k-pke-key-gen-deterministic
  "KeyGen is a pure function of the seed."
  (let ((seed (deterministic-bytes 32 3)))
    (multiple-value-bind (ek1 dk1) (k-pke:key-gen seed)
      (multiple-value-bind (ek2 dk2) (k-pke:key-gen seed)
        (assert-true (equalp ek1 ek2))
        (assert-true (equalp dk1 dk2))))))

(deftest test-k-pke-encrypt-deterministic
  "Encrypt with the same (ek, m, r) is deterministic."
  (let* ((seed (deterministic-bytes 32 4))
         (m (deterministic-bytes 32 200))
         (r (deterministic-bytes 32 201)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (declare (ignore dk))
      (assert-true (equalp (k-pke:encrypt ek m r)
                           (k-pke:encrypt ek m r))))))

(deftest test-k-pke-different-seeds-differ
  "Different seeds give different key pairs."
  (multiple-value-bind (ek1 dk1) (k-pke:key-gen (deterministic-bytes 32 10))
    (multiple-value-bind (ek2 dk2) (k-pke:key-gen (deterministic-bytes 32 11))
      (assert-not (equalp ek1 ek2))
      (assert-not (equalp dk1 dk2)))))

;;; ---- The load-bearing correctness test ----

(deftest test-k-pke-roundtrip
  "The load-bearing K-PKE correctness check: KeyGen, Encrypt, and
   Decrypt round-trip a 32-byte message under a freshly generated key
   pair. This single test exercises the entire module-LWE pipeline:

     - G (SHA3-512) for (ρ, σ)
     - Matrix sampling via SampleNTT + SHAKE128
     - CBD sampling via PRF_η2 + SHAKE256
     - Forward NTT of s and y
     - Matrix-vector multiplication in the NTT domain (both straight
       and transposed forms)
     - Vector inner product in the NTT domain
     - Inverse NTT
     - Compress_{du, dv}, byte-encode, byte-decode, Decompress_{du, dv}
     - Message byte-encode at d=1 with the round-to-nearest compress

   Any bug in any of these layers — or any misalignment between the
   matrix sampling order in KeyGen and Encrypt, or between the vector
   order conventions in the NTT-domain operations — causes the
   recovered message to differ from the input."
  (let* ((seed (deterministic-bytes 32 42))
         (message (deterministic-bytes 32 137))
         (r (deterministic-bytes 32 255)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (let* ((ct (k-pke:encrypt ek message r))
             (m-prime (k-pke:decrypt dk ct)))
        (assert-true (equalp m-prime message))))))

(deftest test-k-pke-roundtrip-several-messages
  "Round trips across a handful of different messages under different
   key pairs and different encryption coins. Each trial is a fresh
   end-to-end invocation."
  (dotimes (trial 5)
    (let* ((seed (deterministic-bytes 32 (+ 1000 trial)))
           (message (deterministic-bytes 32 (+ 2000 trial)))
           (r (deterministic-bytes 32 (+ 3000 trial))))
      (multiple-value-bind (ek dk) (k-pke:key-gen seed)
        (let ((m-prime (k-pke:decrypt dk (k-pke:encrypt ek message r))))
          (assert-true (equalp m-prime message)))))))

(deftest test-k-pke-roundtrip-all-zero-message
  "An all-zero message must round-trip. This is a useful edge case
   because the Decompress_1 step maps 0 bits to 0 (and 1 bits to
   round(q/2)) -- if the compress/decompress boundary logic is
   slightly off it might get the zero case right by accident and fail
   on the random message test, or vice versa."
  (let* ((seed (deterministic-bytes 32 77))
         (message (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
         (r (deterministic-bytes 32 78)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (let ((m-prime (k-pke:decrypt dk (k-pke:encrypt ek message r))))
        (assert-true (equalp m-prime message))))))

(deftest test-k-pke-roundtrip-all-ones-message
  "An all-0xFF message must round-trip. Complementary edge case to
   the all-zero message: every bit is 1, so every μ coefficient is
   the Decompress_1(1) = round(q/2) = 1665 value, and the noise must
   not push any of them across the decision boundary."
  (let* ((seed (deterministic-bytes 32 88))
         (message (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element #xff))
         (r (deterministic-bytes 32 89)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (let ((m-prime (k-pke:decrypt dk (k-pke:encrypt ek message r))))
        (assert-true (equalp m-prime message))))))

(deftest test-k-pke-decrypt-different-ciphertexts-differ
  "Different encryption coins produce different ciphertexts (the same
   message under the same key gets a fresh random value for each
   encryption -- the whole point of the `r` parameter)."
  (let* ((seed (deterministic-bytes 32 55))
         (message (deterministic-bytes 32 66))
         (r1 (deterministic-bytes 32 67))
         (r2 (deterministic-bytes 32 68)))
    (multiple-value-bind (ek dk) (k-pke:key-gen seed)
      (declare (ignore dk))
      (assert-not (equalp (k-pke:encrypt ek message r1)
                          (k-pke:encrypt ek message r2))))))
