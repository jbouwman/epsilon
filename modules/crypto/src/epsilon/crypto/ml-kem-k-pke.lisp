;;;; K-PKE: the IND-CPA public-key scheme underlying ML-KEM (FIPS 203 §5)
;;;;
;;;; This file implements FIPS 203 Algorithms 12-14 (KeyGen, Encrypt,
;;;; Decrypt) for the ML-KEM-768 parameter set. K-PKE is the "bare"
;;;; Module-LWE encryption scheme; the IND-CCA2-secure ML-KEM wrapper
;;;; that the rest of the world actually uses is a Fujisaki-Okamoto
;;;; transform on top of K-PKE, scoped to the next session (Phase 6d).
;;;;
;;;; ML-KEM-768 parameters (FIPS 203 §8):
;;;;   k  = 3         (module rank)
;;;;   η1 = 2         (secret/key-noise CBD parameter)
;;;;   η2 = 2         (encryption-noise CBD parameter)
;;;;   du = 10        (compression width for ciphertext component u)
;;;;   dv = 4         (compression width for ciphertext component v)
;;;;
;;;; The intermediate byte lengths follow directly:
;;;;   ek_PKE = 384·k + 32 = 1184 bytes
;;;;   dk_PKE = 384·k      = 1152 bytes
;;;;   ct     = 32·du·k + 32·dv = 1088 bytes
;;;;   message = 32 bytes
;;;;
;;;; The core correctness identity, which the round-trip test validates:
;;;;
;;;;   w = v' - NTT^-1(ŝ^T ∘ NTT(u'))
;;;;     = μ + (small LWE noise)
;;;;
;;;; where μ is the 1-bit-compressed message. As long as the noise does
;;;; not push any coefficient past the q/4 decision boundary, the
;;;; Compress_1 in decrypt recovers the original message exactly. For
;;;; ML-KEM-768 the per-coefficient failure probability is negligibly
;;;; small (< 2^-164 per the FIPS 203 analysis), so random round-trip
;;;; tests always succeed in practice.

(defpackage epsilon.crypto.ml-kem-k-pke
  (:use :cl)
  (:import (epsilon.crypto.ml-kem-ntt ml)
            (epsilon.crypto.ml-kem-sample sample)
            (epsilon.crypto.ml-kem-codec codec)
            (epsilon.crypto.sha3 sha3))
  (:export
   ;; ML-KEM-768 parameters (exported for use by higher layers and tests)
   #:+k+
   #:+eta1+
   #:+eta2+
   #:+du+
   #:+dv+
   #:+ek-pke-length+
   #:+dk-pke-length+
   #:+ciphertext-length+
   #:+message-length+
   ;; Parameter-set descriptor (FIPS 203 Table 2)
   #:params
   #:params-k
   #:params-eta1
   #:params-eta2
   #:params-du
   #:params-dv
   #:params-ek-pke-length
   #:params-dk-pke-length
   #:params-ciphertext-length
   #:*ml-kem-512-params*
   #:*ml-kem-768-params*
   #:*ml-kem-1024-params*
   ;; K-PKE API
   #:key-gen
   #:encrypt
   #:decrypt))

(in-package :epsilon.crypto.ml-kem-k-pke)

;;; ---------------------------------------------------------------------------
;;; Parameter constants (ML-KEM-768)
;;; ---------------------------------------------------------------------------

(defconstant +k+ 3)
(defconstant +eta1+ 2)
(defconstant +eta2+ 2)
(defconstant +du+ 10)
(defconstant +dv+ 4)

(defconstant +message-length+ 32)
(defconstant +ek-pke-length+  (+ (* 384 +k+) 32))    ; 1184
(defconstant +dk-pke-length+  (* 384 +k+))            ; 1152
(defconstant +ciphertext-length+
  (+ (* 32 +du+ +k+) (* 32 +dv+)))                    ; 1088

;;; FIPS 203 Table 2: the three standardised parameter sets.
;;;
;;;   | set          | k | η1 | η2 | du | dv |   ek   |   dk   |   ct   |
;;;   |--------------|---|----|----|----|----|--------|--------|--------|
;;;   | ML-KEM-512   | 2 |  3 |  2 | 10 |  4 |  800   |  768   |  768   |
;;;   | ML-KEM-768   | 3 |  2 |  2 | 10 |  4 | 1184   | 1152   | 1088   |
;;;   | ML-KEM-1024  | 4 |  2 |  2 | 11 |  5 | 1568   | 1536   | 1568   |
;;;
;;; where ek = 384·k + 32, dk = 384·k, ct = 32·du·k + 32·dv. The
;;; message length is always 32 bytes; the only thing that varies
;;; between sets is the module rank and the noise/compression params.

(defstruct (params (:constructor %make-params))
  (k   3 :type (integer 1 8))
  (eta1 2 :type (integer 1 3))
  (eta2 2 :type (integer 1 3))
  (du  10 :type (integer 1 16))
  (dv   4 :type (integer 1 16))
  (ek-pke-length    0 :type fixnum)
  (dk-pke-length    0 :type fixnum)
  (ciphertext-length 0 :type fixnum))

(defun %make-params-derived (&key k eta1 eta2 du dv)
  (%make-params :k k :eta1 eta1 :eta2 eta2 :du du :dv dv
                :ek-pke-length (+ (* 384 k) 32)
                :dk-pke-length (* 384 k)
                :ciphertext-length (+ (* 32 du k) (* 32 dv))))

(defparameter *ml-kem-512-params*
  (%make-params-derived :k 2 :eta1 3 :eta2 2 :du 10 :dv 4))

(defparameter *ml-kem-768-params*
  (%make-params-derived :k 3 :eta1 2 :eta2 2 :du 10 :dv 4))

(defparameter *ml-kem-1024-params*
  (%make-params-derived :k 4 :eta1 2 :eta2 2 :du 11 :dv 5))

;;; ---------------------------------------------------------------------------
;;; Hash helpers (FIPS 203 §4.1)
;;; ---------------------------------------------------------------------------

(defun %g (input)
  "G(x) = SHA3-512(x). Returns (values rho sigma), each a 32-byte vector."
  (let ((hash (sha3:sha3-512 input)))
    (values (subseq hash 0 32) (subseq hash 32 64))))

(defun %prf (eta sigma n)
  "PRF_η(σ, N) = SHAKE256(σ || N, 64·η bytes). σ is 32 bytes, N is one
   byte. Returns a fresh byte vector of length 64·η."
  (declare (type (integer 1 3) eta)
           (type (simple-array (unsigned-byte 8) (*)) sigma))
  (let ((seed (make-array 33 :element-type '(unsigned-byte 8))))
    (replace seed sigma)
    (setf (aref seed 32) n)
    (sha3:shake256 seed (* 64 eta))))

;;; ---------------------------------------------------------------------------
;;; Vector and matrix operations on polynomials
;;; ---------------------------------------------------------------------------

(defun %make-poly-vector (k)
  "Allocate a length-K vector of zero polynomials (stored as a simple
   general vector; each element is a 256-u16 polynomial)."
  (let ((v (make-array k)))
    (loop for i from 0 below k do (setf (aref v i) (ml:make-poly)))
    v))

(defun %vector-ntt (v)
  "Apply NTT to each polynomial in V, returning a new vector."
  (let ((out (make-array (length v))))
    (loop for i from 0 below (length v)
          do (setf (aref out i) (ml:ntt (aref v i))))
    out))

(defun %vector-ntt-inverse (v)
  (let ((out (make-array (length v))))
    (loop for i from 0 below (length v)
          do (setf (aref out i) (ml:ntt-inverse (aref v i))))
    out))

(defun %vector-add (a b)
  "Pointwise polynomial addition across corresponding vector entries."
  (let ((out (make-array (length a))))
    (loop for i from 0 below (length a)
          do (setf (aref out i) (ml:poly-add (aref a i) (aref b i))))
    out))

(defun %matrix-vector-mul-ntt (matrix vec)
  "Compute MATRIX · VEC in the NTT domain. MATRIX is a k-by-k 2D array
   of polynomials, VEC is a length-k vector. Returns a length-k vector.
   Each entry r[i] = sum_j ntt-multiply(matrix[i,j], vec[j])."
  (let* ((k (length vec))
         (result (%make-poly-vector k)))
    (loop for i from 0 below k do
      (loop for j from 0 below k do
        (setf (aref result i)
              (ml:poly-add (aref result i)
                           (ml:ntt-multiply (aref matrix i j)
                                            (aref vec j))))))
    result))

(defun %transpose-matrix-vector-mul-ntt (matrix vec)
  "Compute MATRIX^T · VEC in the NTT domain. Same as the untransposed
   version except the row and column indices are swapped. FIPS 203
   Algorithm 13 uses this form for the encryption matrix-vector
   product -- swapping (i, j) here is what lets encryption and
   decryption use the *same* sampled matrix A without one side having
   to transpose the bytes."
  (let* ((k (length vec))
         (result (%make-poly-vector k)))
    (loop for i from 0 below k do
      (loop for j from 0 below k do
        (setf (aref result i)
              (ml:poly-add (aref result i)
                           (ml:ntt-multiply (aref matrix j i)
                                            (aref vec j))))))
    result))

(defun %vector-inner-product-ntt (a b)
  "Compute a^T · b as a single polynomial: sum_i ntt-multiply(a_i, b_i).
   Used for the t̂^T ∘ ŷ step of encryption and the ŝ^T ∘ NTT(u') step
   of decryption."
  (let ((result (ml:make-poly)))
    (loop for i from 0 below (length a)
          do (setf result
                   (ml:poly-add result
                                (ml:ntt-multiply (aref a i) (aref b i)))))
    result))

;;; ---------------------------------------------------------------------------
;;; Vector-level compression / encoding
;;; ---------------------------------------------------------------------------

(defun %compress-poly-vector (v d)
  (let ((out (make-array (length v))))
    (loop for i from 0 below (length v)
          do (setf (aref out i) (codec:compress-poly (aref v i) d)))
    out))

(defun %decompress-poly-vector (v d)
  (let ((out (make-array (length v))))
    (loop for i from 0 below (length v)
          do (setf (aref out i) (codec:decompress-poly (aref v i) d)))
    out))

(defun %encode-poly-vector (v d)
  "Byte-encode every polynomial in V (with width d) and concatenate.
   Output length is (length v) * 32 * d bytes."
  (let* ((count (length v))
         (out-len (* count 32 d))
         (out (make-array out-len :element-type '(unsigned-byte 8)))
         (off 0))
    (loop for i from 0 below count do
      (let ((encoded (codec:byte-encode (aref v i) d)))
        (replace out encoded :start1 off)
        (incf off (length encoded))))
    out))

(defun %decode-poly-vector (bytes k d)
  "Split BYTES into K chunks of 32·d bytes and byte-decode each chunk."
  (let* ((chunk-size (* 32 d))
         (out (%make-poly-vector k)))
    (loop for i from 0 below k do
      (setf (aref out i)
            (codec:byte-decode
             (subseq bytes (* i chunk-size) (* (1+ i) chunk-size))
             d)))
    out))

;;; ---------------------------------------------------------------------------
;;; Matrix sampling from ρ
;;; ---------------------------------------------------------------------------

(defun %sample-matrix (rho params)
  "Build the k-by-k matrix Â whose entry (i, j) is SampleNTT(ρ || j || i)
   per FIPS 203 Algorithm 12 step 4 / Algorithm 13 step 4. Note the j, i
   byte ordering -- this is a common implementation pitfall."
  (let* ((k (params-k params))
         (a (make-array (list k k)))
         (seed (make-array 34 :element-type '(unsigned-byte 8))))
    (replace seed rho)
    (loop for i from 0 below k do
      (loop for j from 0 below k do
        (setf (aref seed 32) j)
        (setf (aref seed 33) i)
        (setf (aref a i j)
              (sample:sample-ntt (copy-seq seed)))))
    a))

;;; ---------------------------------------------------------------------------
;;; K-PKE.KeyGen (FIPS 203 Algorithm 12)
;;; ---------------------------------------------------------------------------

(defun key-gen (seed &optional (params *ml-kem-768-params*))
  "K-PKE.KeyGen(d): generate an (ek_PKE, dk_PKE) pair from the 32-byte
   seed d under PARAMS (default: ML-KEM-768). Returns (values ek-pke dk-pke)."
  (declare (type (simple-array (unsigned-byte 8) (*)) seed))
  (assert (= (length seed) 32) ()
          "K-PKE.KeyGen: seed must be 32 bytes (got ~D)" (length seed))
  (let ((k (params-k params))
        (eta1 (params-eta1 params)))
    ;; Step 1: (ρ, σ) ← G(d || k)
    (let ((g-input (make-array 33 :element-type '(unsigned-byte 8))))
      (replace g-input seed)
      (setf (aref g-input 32) k)
      (multiple-value-bind (rho sigma) (%g g-input)
        ;; Step 3-4: sample Â from ρ
        (let* ((a-hat (%sample-matrix rho params))
               ;; Steps 5-7: sample s and e from σ via PRF_η1
               (s (%make-poly-vector k))
               (e (%make-poly-vector k)))
          (loop for i from 0 below k do
            (setf (aref s i)
                  (sample:sample-poly-cbd (%prf eta1 sigma i) eta1)))
          (loop for i from 0 below k do
            (setf (aref e i)
                  (sample:sample-poly-cbd (%prf eta1 sigma (+ k i)) eta1)))
          ;; Steps 8-9: NTT(s), NTT(e)
          (let* ((s-hat (%vector-ntt s))
                 (e-hat (%vector-ntt e))
                 ;; Step 10: t̂ ← Â ∘ ŝ + ê
                 (t-hat (%vector-add (%matrix-vector-mul-ntt a-hat s-hat) e-hat))
                 ;; Step 11: ek_PKE ← ByteEncode12(t̂) || ρ
                 (ek (concatenate '(simple-array (unsigned-byte 8) (*))
                                  (%encode-poly-vector t-hat 12)
                                  rho))
                 ;; Step 12: dk_PKE ← ByteEncode12(ŝ)
                 (dk (%encode-poly-vector s-hat 12)))
            (values ek dk)))))))

;;; ---------------------------------------------------------------------------
;;; K-PKE.Encrypt (FIPS 203 Algorithm 13)
;;; ---------------------------------------------------------------------------

(defun encrypt (ek-pke message randomness &optional (params *ml-kem-768-params*))
  "K-PKE.Encrypt(ek_PKE, m, r): produce a ciphertext for the 32-byte
   MESSAGE under EK-PKE, using the 32-byte RANDOMNESS as the encryption
   coin. PARAMS selects the parameter set (default: ML-KEM-768)."
  (declare (type (simple-array (unsigned-byte 8) (*)) ek-pke message randomness))
  (let ((k (params-k params))
        (eta1 (params-eta1 params))
        (eta2 (params-eta2 params))
        (du (params-du params))
        (dv (params-dv params)))
    (assert (= (length ek-pke) (params-ek-pke-length params)))
    (assert (= (length message) +message-length+))
    (assert (= (length randomness) 32))
    ;; Step 1-3: parse t̂ and ρ out of ek_PKE
    (let* ((t-hat (%decode-poly-vector
                   (subseq ek-pke 0 (* 384 k)) k 12))
           (rho (subseq ek-pke (* 384 k) (+ (* 384 k) 32)))
           ;; Step 4: recover Â from ρ (same values as key-gen used)
           (a-hat (%sample-matrix rho params))
           ;; Step 5-7: sample y, e1, e2 from r via PRF_η1 and PRF_η2
           (y (%make-poly-vector k))
           (e1 (%make-poly-vector k)))
      (loop for i from 0 below k do
        (setf (aref y i)
              (sample:sample-poly-cbd (%prf eta1 randomness i) eta1)))
      (loop for i from 0 below k do
        (setf (aref e1 i)
              (sample:sample-poly-cbd (%prf eta2 randomness (+ k i)) eta2)))
      (let* ((e2 (sample:sample-poly-cbd
                  (%prf eta2 randomness (* 2 k)) eta2))
             ;; Step 8: ŷ ← NTT(y)
             (y-hat (%vector-ntt y))
             ;; Step 9: u ← NTT^-1(Â^T ∘ ŷ) + e1
             (u (%vector-add
                 (%vector-ntt-inverse (%transpose-matrix-vector-mul-ntt a-hat y-hat))
                 e1))
             ;; Step 10: μ ← Decompress_1(ByteDecode_1(m))
             (mu (codec:decompress-poly (codec:byte-decode message 1) 1))
             ;; Step 11: v ← NTT^-1(t̂^T ∘ ŷ) + e2 + μ
             (v (ml:poly-add
                 (ml:poly-add
                  (ml:ntt-inverse (%vector-inner-product-ntt t-hat y-hat))
                  e2)
                 mu))
             ;; Step 12-13: compress and byte-encode u, v
             (c1 (%encode-poly-vector (%compress-poly-vector u du) du))
             (c2 (codec:byte-encode (codec:compress-poly v dv) dv)))
        (concatenate '(simple-array (unsigned-byte 8) (*)) c1 c2)))))

;;; ---------------------------------------------------------------------------
;;; K-PKE.Decrypt (FIPS 203 Algorithm 14)
;;; ---------------------------------------------------------------------------

(defun decrypt (dk-pke ciphertext &optional (params *ml-kem-768-params*))
  "K-PKE.Decrypt(dk_PKE, c): recover the 32-byte message from a K-PKE
   ciphertext. PARAMS selects the parameter set (default: ML-KEM-768)."
  (declare (type (simple-array (unsigned-byte 8) (*)) dk-pke ciphertext))
  (let ((k (params-k params))
        (du (params-du params))
        (dv (params-dv params)))
    (assert (= (length dk-pke) (params-dk-pke-length params)))
    (assert (= (length ciphertext) (params-ciphertext-length params)))
    (let* ((c1-len (* 32 du k))
           ;; Step 1-2: split c into (c1, c2)
           (c1 (subseq ciphertext 0 c1-len))
           (c2 (subseq ciphertext c1-len (+ c1-len (* 32 dv))))
           ;; Step 3-4: u' ← Decompress_du(ByteDecode_du(c1))
           (u-prime (%decompress-poly-vector (%decode-poly-vector c1 k du) du))
           ;; v' ← Decompress_dv(ByteDecode_dv(c2))
           (v-prime (codec:decompress-poly (codec:byte-decode c2 dv) dv))
           ;; Step 5: ŝ ← ByteDecode12(dk_PKE)
           (s-hat (%decode-poly-vector dk-pke k 12))
           ;; Step 6: w ← v' - NTT^-1(ŝ^T ∘ NTT(u'))
           (u-hat (%vector-ntt u-prime))
           (w (ml:poly-sub v-prime
                           (ml:ntt-inverse (%vector-inner-product-ntt s-hat u-hat)))))
      ;; Step 7: m ← ByteEncode_1(Compress_1(w))
      (codec:byte-encode (codec:compress-poly w 1) 1))))
