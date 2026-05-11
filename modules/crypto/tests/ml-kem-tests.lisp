;;;; Tests for the ML-KEM-768 wrapper (FIPS 203 §6)

(defpackage epsilon.crypto.ml-kem-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.ml-kem ml-kem)))

(in-package :epsilon.crypto.ml-kem-tests)

(defun deterministic-bytes (length seed)
  "Reproducible pseudo-random byte vector -- same generator used by
   the K-PKE tests."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (state (+ seed 1)))
    (loop for i from 0 below length do
      (setf state (mod (+ (* state 1103515245) 12345) (expt 2 32)))
      (setf (aref out i) (ldb (byte 8 16) state)))
    out))

;;; ---- Constants ----

(deftest test-ml-kem-byte-lengths
  "Published ML-KEM-768 parameter sizes (FIPS 203 §8.1):
     ek = 1184 bytes
     dk = 2400 bytes
     ciphertext = 1088 bytes
     shared secret = 32 bytes"
  (assert-= ml-kem:+ek-length+ 1184)
  (assert-= ml-kem:+dk-length+ 2400)
  (assert-= ml-kem:+ciphertext-length+ 1088)
  (assert-= ml-kem:+shared-secret-length+ 32))

;;; ---- Output shape ----

(deftest test-ml-kem-keygen-internal-output-shape
  "keygen-internal returns (ek, dk) of the specified byte lengths."
  (let ((d (deterministic-bytes 32 1))
        (z (deterministic-bytes 32 2)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (assert-= (length ek) ml-kem:+ek-length+)
      (assert-= (length dk) ml-kem:+dk-length+))))

(deftest test-ml-kem-encaps-internal-output-shape
  "encaps-internal returns (shared-secret, ciphertext) of the
   specified byte lengths."
  (let ((d (deterministic-bytes 32 3))
        (z (deterministic-bytes 32 4))
        (m (deterministic-bytes 32 5)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (declare (ignore dk))
      (multiple-value-bind (k c) (ml-kem:encaps-internal ek m)
        (assert-= (length k) ml-kem:+shared-secret-length+)
        (assert-= (length c) ml-kem:+ciphertext-length+)))))

(deftest test-ml-kem-keygen-random-output-shape
  "The random `keygen` also returns correctly sized outputs (cheap
   smoke test that the random variant is wired up to the internal one)."
  (multiple-value-bind (ek dk) (ml-kem:keygen)
    (assert-= (length ek) ml-kem:+ek-length+)
    (assert-= (length dk) ml-kem:+dk-length+)))

;;; ---- Determinism ----

(deftest test-ml-kem-keygen-internal-deterministic
  "keygen-internal is a pure function of (d, z)."
  (let ((d (deterministic-bytes 32 10))
        (z (deterministic-bytes 32 11)))
    (multiple-value-bind (ek1 dk1) (ml-kem:keygen-internal d z)
      (multiple-value-bind (ek2 dk2) (ml-kem:keygen-internal d z)
        (assert-true (equalp ek1 ek2))
        (assert-true (equalp dk1 dk2))))))

(deftest test-ml-kem-encaps-internal-deterministic
  "encaps-internal is a pure function of (ek, m)."
  (let ((d (deterministic-bytes 32 20))
        (z (deterministic-bytes 32 21))
        (m (deterministic-bytes 32 22)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (declare (ignore dk))
      (multiple-value-bind (k1 c1) (ml-kem:encaps-internal ek m)
        (multiple-value-bind (k2 c2) (ml-kem:encaps-internal ek m)
          (assert-true (equalp k1 k2))
          (assert-true (equalp c1 c2)))))))

;;; ---- dk layout ----

(deftest test-ml-kem-dk-embeds-ek
  "The bytes of the private key at offset 1152..2336 are exactly the
   public key ek. FIPS 203 defines dk = dk_PKE || ek || H(ek) || z
   and Decaps reads ek back out from this fixed offset to re-encrypt
   during the FO check."
  (let ((d (deterministic-bytes 32 30))
        (z (deterministic-bytes 32 31)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (assert-true (equalp (subseq dk 1152 (+ 1152 1184)) ek)))))

(deftest test-ml-kem-dk-embeds-h-ek
  "Offset 2336..2368 of dk is H(ek) = SHA3-256(ek)."
  (let ((d (deterministic-bytes 32 40))
        (z (deterministic-bytes 32 41)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (assert-true (equalp (subseq dk 2336 2368)
                           (epsilon.crypto.sha3:sha3-256 ek))))))

(deftest test-ml-kem-dk-embeds-z
  "Offset 2368..2400 of dk is the implicit-rejection secret z
   supplied to keygen-internal."
  (let ((d (deterministic-bytes 32 50))
        (z (deterministic-bytes 32 51)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (declare (ignore ek))
      (assert-true (equalp (subseq dk 2368 2400) z)))))

;;; ---- Input validation ----

(deftest test-ml-kem-validate-ek-passes-for-real-ek
  "A freshly generated ek passes validation."
  (multiple-value-bind (ek dk) (ml-kem:keygen-internal
                                (deterministic-bytes 32 60)
                                (deterministic-bytes 32 61))
    (declare (ignore dk))
    (assert-true (ml-kem:validate-ek ek))))

(deftest test-ml-kem-validate-dk-passes-for-real-dk
  "A freshly generated dk passes validation (hash check succeeds)."
  (multiple-value-bind (ek dk) (ml-kem:keygen-internal
                                (deterministic-bytes 32 70)
                                (deterministic-bytes 32 71))
    (declare (ignore ek))
    (assert-true (ml-kem:validate-dk dk))))

(deftest test-ml-kem-validate-ek-rejects-wrong-length
  "An ek of wrong length is rejected up front."
  (assert-condition (ml-kem:ml-kem-error)
                    (ml-kem:validate-ek
                     (make-array 100 :element-type '(unsigned-byte 8)))))

(deftest test-ml-kem-validate-dk-rejects-wrong-length
  "A dk of wrong length is rejected up front."
  (assert-condition (ml-kem:ml-kem-error)
                    (ml-kem:validate-dk
                     (make-array 100 :element-type '(unsigned-byte 8)))))

(deftest test-ml-kem-validate-dk-rejects-bad-hash
  "A dk whose stored H(ek) does not match the embedded ek is rejected.
   This catches private-key corruption or deliberately tampered dk blobs."
  (multiple-value-bind (ek dk) (ml-kem:keygen-internal
                                (deterministic-bytes 32 80)
                                (deterministic-bytes 32 81))
    (declare (ignore ek))
    (let ((tampered (copy-seq dk)))
      ;; Flip a bit in the H(ek) field (offset 2336..2368).
      (setf (aref tampered 2340)
            (logxor (aref tampered 2340) 1))
      (assert-condition (ml-kem:ml-kem-error)
                        (ml-kem:validate-dk tampered)))))

;;; ---- The load-bearing round trip ----

(deftest test-ml-kem-encaps-decaps-roundtrip
  "The core ML-KEM round trip: KeyGen, Encaps, Decaps, and the two
   shared secrets agree. This exercises the full FO transform stack:
   K-PKE.KeyGen, G for the randomness derivation, K-PKE.Encrypt for
   encapsulation, K-PKE.Decrypt for the first half of Decaps, G again
   for the shared-secret re-derivation, K-PKE.Encrypt again for the
   re-encryption check, and the constant-time selection between K'
   and K̄."
  (let ((d (deterministic-bytes 32 100))
        (z (deterministic-bytes 32 101))
        (m (deterministic-bytes 32 102)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (multiple-value-bind (k-shared ciphertext) (ml-kem:encaps-internal ek m)
        (let ((k-recovered (ml-kem:decaps dk ciphertext)))
          (assert-true (equalp k-recovered k-shared)))))))

(deftest test-ml-kem-roundtrip-several-trials
  "Several independent round trips with different seed tuples."
  (dotimes (trial 5)
    (let ((d (deterministic-bytes 32 (+ 1000 trial)))
          (z (deterministic-bytes 32 (+ 2000 trial)))
          (m (deterministic-bytes 32 (+ 3000 trial))))
      (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
        (multiple-value-bind (k-shared ciphertext) (ml-kem:encaps-internal ek m)
          (assert-true (equalp (ml-kem:decaps dk ciphertext) k-shared)))))))

(deftest test-ml-kem-random-keygen-roundtrip
  "The fully-random `keygen` + `encaps` + `decaps` path also round
   trips. This tests the kernel-entropy wiring at both keygen and
   encaps."
  (multiple-value-bind (ek dk) (ml-kem:keygen)
    (multiple-value-bind (k-shared ciphertext) (ml-kem:encaps ek)
      (assert-true (equalp (ml-kem:decaps dk ciphertext) k-shared)))))

;;; ---- Implicit rejection (IND-CCA2 property) ----

(deftest test-ml-kem-decaps-rejects-tampered-ciphertext
  "A tampered ciphertext produces a shared secret that differs from
   the original encaps output. Note: Decaps does NOT signal an error;
   it silently returns K̄ = J(z || c), which is pseudo-random and
   unrelated to the original K. The caller cannot distinguish this
   from a valid secret locally, which is the IND-CCA2 protection."
  (let ((d (deterministic-bytes 32 200))
        (z (deterministic-bytes 32 201))
        (m (deterministic-bytes 32 202)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (multiple-value-bind (k-shared ciphertext) (ml-kem:encaps-internal ek m)
        (let ((tampered (copy-seq ciphertext)))
          ;; Flip a byte in the middle of the ciphertext.
          (setf (aref tampered 500) (logxor (aref tampered 500) 1))
          (let ((k-rejected (ml-kem:decaps dk tampered)))
            ;; The rejected key is the right length (32 bytes)...
            (assert-= (length k-rejected) 32)
            ;; ...but differs from the original shared secret.
            (assert-not (equalp k-rejected k-shared))))))))

(deftest test-ml-kem-decaps-rejection-is-deterministic-per-dk
  "Implicit rejection is derived from J(z || c), so the same (dk,
   tampered ciphertext) pair always yields the same `rejected' shared
   secret. This is a property worth pinning because it is NOT
   obviously deterministic -- a naive Decaps could conceivably draw
   fresh randomness for the rejected branch, which would leak
   information."
  (let ((d (deterministic-bytes 32 300))
        (z (deterministic-bytes 32 301)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal d z)
      (declare (ignore ek))
      (let ((bad-ciphertext (make-array ml-kem:+ciphertext-length+
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
        (assert-true (equalp (ml-kem:decaps dk bad-ciphertext)
                             (ml-kem:decaps dk bad-ciphertext)))))))

(deftest test-ml-kem-decaps-rejection-varies-with-z
  "Two dk values that share the same seed d but differ in z must
   produce different rejection secrets for the same bad ciphertext.
   This confirms z is actually mixed into the rejection path."
  (let ((d (deterministic-bytes 32 400))
        (z1 (deterministic-bytes 32 401))
        (z2 (deterministic-bytes 32 402)))
    (multiple-value-bind (ek1 dk1) (ml-kem:keygen-internal d z1)
      (multiple-value-bind (ek2 dk2) (ml-kem:keygen-internal d z2)
        ;; Same d means same ek (ek is deterministic in d alone).
        (assert-true (equalp ek1 ek2))
        (let ((bad (make-array ml-kem:+ciphertext-length+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
          (assert-not (equalp (ml-kem:decaps dk1 bad)
                              (ml-kem:decaps dk2 bad))))))))

;;; ---- Regression pinning ----
;;;
;;; Until a session with internet access can pull the canonical NIST
;;; ACVP vectors for ML-KEM-768, the regression pins below stand in as
;;; a "did this byte for byte change" guard: the SHA3-256 hashes of
;;; ek, dk, ciphertext, and shared secret for the deterministic
;;; keygen-internal(0, 0) + encaps-internal(ek, 0) run.
;;;
;;; These pins do NOT prove interop with other ML-KEM implementations
;;; -- they just prove our implementation is stable under refactoring.
;;; A bug that happens to be present today would silently persist
;;; through any number of refactors that leave the byte output
;;; unchanged. When ACVP vectors become available, the hashes below
;;; should be replaced with direct equality checks against canonical
;;; test-vector outputs.

(deftest test-ml-kem-regression-pins-zero-seeds
  "Regression pins: SHA3-256 hashes of the outputs of
   keygen-internal + encaps-internal + decaps when every seed
   (d, z, m) is the 32-byte all-zero value. Any accidental change
   to the implementation will cause one of these four assertions
   to fail and a human must investigate whether the change was
   intentional (and update the pins) or a bug (and revert)."
  (let* ((zero (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal zero zero)
      ;; Pinned: SHA3-256(ek) for keygen-internal(d=0, z=0).
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ek))
       "07f81a8b0e266a3ee92d3a63cdae5cff921905544c9dd797a849e1d054180eca")
      ;; Pinned: SHA3-256(dk) for the same inputs. Changes here also
      ;; flag drift in the dk layout (dk = dk_PKE || ek || H(ek) || z),
      ;; which is why even a change that leaves ek and ciphertext
      ;; intact but reorders dk would still trip this pin.
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 dk))
       "b476cca5af51be72dd16e096491931b4c7c2236772d3a091d6cff0287e83c70b")
      (multiple-value-bind (k ciphertext) (ml-kem:encaps-internal ek zero)
        ;; Pinned: SHA3-256(ciphertext) and SHA3-256(shared secret) for
        ;; encaps-internal with m=0 against the zero-seed ek.
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ciphertext))
         "458a9896b26a4cba613b45288e09d89f688d69f181d4f11e3c486057fb3066ac")
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 k))
         "57da0c2dc4a659456ad650b0dcbaf368a37e09ece3e3046999a371c4b6623250")
        ;; Finally, decaps must recover the same shared secret.
        (assert-true (equalp (ml-kem:decaps dk ciphertext) k))))))

(deftest test-ml-kem-wrong-dk-decaps-to-rejection
  "Using a different dk (unrelated to the encapsulator's ek) falls
   into the rejection branch. The caller cannot tell without an
   external channel whether the key was wrong or the ciphertext was
   just mangled -- again by design."
  (let* ((d1 (deterministic-bytes 32 500))
         (z1 (deterministic-bytes 32 501))
         (d2 (deterministic-bytes 32 502))
         (z2 (deterministic-bytes 32 503))
         (m  (deterministic-bytes 32 504)))
    (multiple-value-bind (ek1 dk1) (ml-kem:keygen-internal d1 z1)
      (declare (ignore dk1))
      (multiple-value-bind (ek2 dk2) (ml-kem:keygen-internal d2 z2)
        (declare (ignore ek2))
        (multiple-value-bind (k-shared ciphertext) (ml-kem:encaps-internal ek1 m)
          ;; Decapsulate under the WRONG private key.
          (let ((k-wrong (ml-kem:decaps dk2 ciphertext)))
            (assert-= (length k-wrong) 32)
            (assert-not (equalp k-wrong k-shared))))))))

;;; ---------------------------------------------------------------------------
;;; ML-KEM-512 and ML-KEM-1024 parameter sets (FIPS 203 Table 2)
;;; ---------------------------------------------------------------------------

(deftest test-ml-kem-512-byte-lengths
  "ML-KEM-512: ek=800, dk=1632, ct=768 per FIPS 203 Table 2."
  (assert-= (ml-kem:parameter-set-ek-length        :ml-kem-512)  800)
  (assert-= (ml-kem:parameter-set-dk-length        :ml-kem-512) 1632)
  (assert-= (ml-kem:parameter-set-ciphertext-length :ml-kem-512)  768))

(deftest test-ml-kem-1024-byte-lengths
  "ML-KEM-1024: ek=1568, dk=3168, ct=1568 per FIPS 203 Table 2."
  (assert-= (ml-kem:parameter-set-ek-length        :ml-kem-1024) 1568)
  (assert-= (ml-kem:parameter-set-dk-length        :ml-kem-1024) 3168)
  (assert-= (ml-kem:parameter-set-ciphertext-length :ml-kem-1024) 1568))

(deftest test-ml-kem-512-keygen-encaps-decaps-roundtrip
  "Full KEM round trip under ML-KEM-512: the shared secret from
   `encaps` must match the one recovered by `decaps`."
  (multiple-value-bind (ek dk) (ml-kem:keygen :ml-kem-512)
    (assert-= (length ek)  800)
    (assert-= (length dk) 1632)
    (multiple-value-bind (k-enc ct) (ml-kem:encaps ek :ml-kem-512)
      (assert-= (length ct) 768)
      (assert-= (length k-enc) 32)
      (let ((k-dec (ml-kem:decaps dk ct :ml-kem-512)))
        (assert-equalp k-dec k-enc)))))

(deftest test-ml-kem-1024-keygen-encaps-decaps-roundtrip
  "Full KEM round trip under ML-KEM-1024."
  (multiple-value-bind (ek dk) (ml-kem:keygen :ml-kem-1024)
    (assert-= (length ek) 1568)
    (assert-= (length dk) 3168)
    (multiple-value-bind (k-enc ct) (ml-kem:encaps ek :ml-kem-1024)
      (assert-= (length ct) 1568)
      (let ((k-dec (ml-kem:decaps dk ct :ml-kem-1024)))
        (assert-equalp k-dec k-enc)))))

(deftest test-ml-kem-512-keygen-internal-deterministic
  "keygen-internal with a fixed (d, z) is deterministic under ML-KEM-512."
  (let ((d (make-array 32 :element-type '(unsigned-byte 8) :initial-element 7))
        (z (make-array 32 :element-type '(unsigned-byte 8) :initial-element 3)))
    (multiple-value-bind (ek1 dk1) (ml-kem:keygen-internal d z :ml-kem-512)
      (multiple-value-bind (ek2 dk2) (ml-kem:keygen-internal d z :ml-kem-512)
        (assert-equalp ek1 ek2)
        (assert-equalp dk1 dk2)))))

(deftest test-ml-kem-1024-implicit-rejection
  "ML-KEM-1024 decaps against a tampered ciphertext returns a
   non-matching pseudo-random value rather than signalling an error."
  (multiple-value-bind (ek dk) (ml-kem:keygen :ml-kem-1024)
    (multiple-value-bind (k-enc ct) (ml-kem:encaps ek :ml-kem-1024)
      (let ((tampered (copy-seq ct)))
        (setf (aref tampered 0) (logxor (aref tampered 0) 1))
        (let ((k-dec (ml-kem:decaps dk tampered :ml-kem-1024)))
          (assert-= (length k-dec) 32)
          (assert-not (equalp k-dec k-enc)))))))

(deftest test-ml-kem-parameter-sets-distinct
  "ML-KEM-512 / 768 / 1024 produce different-size outputs for the
   same random input -- sanity check that the parameter-set argument
   is actually reaching the low-level routines and not silently
   collapsing to the 768 defaults."
  (multiple-value-bind (ek512 dk512) (ml-kem:keygen :ml-kem-512)
    (declare (ignore dk512))
    (multiple-value-bind (ek768 dk768) (ml-kem:keygen :ml-kem-768)
      (declare (ignore dk768))
      (multiple-value-bind (ek1024 dk1024) (ml-kem:keygen :ml-kem-1024)
        (declare (ignore dk1024))
        (assert-not (= (length ek512) (length ek768)))
        (assert-not (= (length ek768) (length ek1024)))
        (assert-not (= (length ek512) (length ek1024)))))))

;;; ---------------------------------------------------------------------------
;;; Regression pins for ML-KEM-512, ML-KEM-768, ML-KEM-1024 (A1 scaffold)
;;; ---------------------------------------------------------------------------
;;;
;;; FIPS 203 Phase 6e deliverable. These pins lock down the byte output
;;; of keygen-internal + encaps-internal + decaps for all three
;;; parameter sets with the all-zero seed tuple (d=0, z=0, m=0), plus
;;; the SHA3-256 of the implicit-rejection branch (decaps against a
;;; one-bit-flipped ciphertext). Together they:
;;;
;;;   1. Exercise each distinct parameter set end-to-end.
;;;   2. Catch accidental byte-level drift in any of the k-pke helpers
;;;      (matrix sampling, CBD, NTT, compression, encoding).
;;;   3. Cover the IND-CCA2 rejection path in addition to the valid
;;;      decapsulation path.
;;;
;;; These are REGRESSION pins, not NIST ACVP canonical vectors. They
;;; prove our implementation is internally self-consistent and has not
;;; silently drifted, but they do NOT prove interop with OpenSSL,
;;; BoringSSL, PQClean, or any other reference. When canonical ACVP or
;;; PQClean KAT vectors are imported into this tree, the pins below
;;; should be replaced with direct byte-equality checks against the
;;; reference values.

(deftest test-ml-kem-512-regression-pins-zero-seeds
  "ML-KEM-512 byte-level regression pins under (d=0, z=0, m=0)."
  (let ((zero (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal zero zero :ml-kem-512)
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ek))
       "e5bd1b37a75e0f092974e846e8c37c45487d60739f99351719a5394723262b3b")
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 dk))
       "c4d6b5ebc673f958555366a0e7f6f4849fac965157fd4e334d107460b3f3ef5d")
      (multiple-value-bind (k ct) (ml-kem:encaps-internal ek zero :ml-kem-512)
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ct))
         "d1bfd1b75d50a1f77351142f19cfce270d0a5fca1a131e00a9299a3abafbdd54")
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 k))
         "cd8611c2a9facbfbbb8c8f4110f2de9aae421fc1140d3c52be2a2364ec6e74bc")
        ;; decaps recovers the same shared secret
        (assert-true (equalp (ml-kem:decaps dk ct :ml-kem-512) k))
        ;; Implicit rejection pin: flipping one ciphertext byte yields a
        ;; pseudo-random shared secret whose SHA3-256 is also deterministic
        ;; in this fully-fixed input configuration.
        (let ((tampered (copy-seq ct)))
          (setf (aref tampered 0) (logxor (aref tampered 0) 1))
          (assert-equal
           (bytes-to-hex (epsilon.crypto.sha3:sha3-256
                          (ml-kem:decaps dk tampered :ml-kem-512)))
           "446e11d8480968d0a13a77843dfaacb64daa5922ca2e29fc289ea2e35437de85"))))))

(deftest test-ml-kem-768-implicit-rejection-regression-pin
  "Pin the SHA3-256 of the implicit-rejection branch under (0, 0, 0)
   for the ML-KEM-768 default parameter set. Complements the existing
   valid-decapsulation pin in `test-ml-kem-regression-pins-zero-seeds`."
  (let ((zero (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal zero zero)
      (multiple-value-bind (k ct) (ml-kem:encaps-internal ek zero)
        (declare (ignore k))
        (let ((tampered (copy-seq ct)))
          (setf (aref tampered 0) (logxor (aref tampered 0) 1))
          (assert-equal
           (bytes-to-hex (epsilon.crypto.sha3:sha3-256 (ml-kem:decaps dk tampered)))
           "649b5b861816541f229d996a922a61c6ece7eff33bd97dd7a7c2c04371cf9690"))))))

(deftest test-ml-kem-1024-regression-pins-zero-seeds
  "ML-KEM-1024 byte-level regression pins under (d=0, z=0, m=0)."
  (let ((zero (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (multiple-value-bind (ek dk) (ml-kem:keygen-internal zero zero :ml-kem-1024)
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ek))
       "9f62e8c88195d7ad50b14514fbe94a887554204da7a40dafbe72c5e15d39e969")
      (assert-equal
       (bytes-to-hex (epsilon.crypto.sha3:sha3-256 dk))
       "ed78c8ae08ce4952cb6626fc14dfb008362ff13a92c9ea5afb09f47841908bb4")
      (multiple-value-bind (k ct) (ml-kem:encaps-internal ek zero :ml-kem-1024)
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 ct))
         "ecccadf704142d9c85715cb7a2390216a2e0a01728c28c825f2396be88766a03")
        (assert-equal
         (bytes-to-hex (epsilon.crypto.sha3:sha3-256 k))
         "4485aaf3d8eff15aa8ccca5b4ed8e2f04d0722d912e960fc165d5dfa653cdd9b")
        (assert-true (equalp (ml-kem:decaps dk ct :ml-kem-1024) k))
        (let ((tampered (copy-seq ct)))
          (setf (aref tampered 0) (logxor (aref tampered 0) 1))
          (assert-equal
           (bytes-to-hex (epsilon.crypto.sha3:sha3-256
                          (ml-kem:decaps dk tampered :ml-kem-1024)))
           "3f120d4cf1f4e56b6cb7b5ab7ce48fbe882884809c34d3a1de3b52b32e892be8"))))))
