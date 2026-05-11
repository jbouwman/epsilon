;;;; ML-KEM-768 (FIPS 203)
;;;;
;;;; The IND-CCA2-secure KEM obtained by wrapping K-PKE with the
;;;; Fujisaki-Okamoto-like transform specified in FIPS 203 §6.
;;;;
;;;; API
;;;; ---
;;;;
;;;;   (keygen)              -> (values ek dk)
;;;;     Fresh random key pair. ek is 1184 bytes, dk is 2400 bytes.
;;;;
;;;;   (encaps ek)           -> (values shared-secret ciphertext)
;;;;     Returns a 32-byte shared secret and the corresponding 1088-byte
;;;;     ciphertext to send to the holder of dk.
;;;;
;;;;   (decaps dk ciphertext) -> shared-secret
;;;;     Recovers the 32-byte shared secret. On any kind of tampering
;;;;     or wrong-key input, returns a pseudo-random 32-byte value
;;;;     derived from the private rejection secret z (see FIPS 203
;;;;     §6.3, "implicit rejection"). Does NOT signal an error -- the
;;;;     caller cannot distinguish a valid ciphertext from an invalid
;;;;     one via the decaps output alone, which is the whole point of
;;;;     the transform.
;;;;
;;;;   (keygen-internal d z) -> (values ek dk)
;;;;   (encaps-internal ek m) -> (values K c)
;;;;     The deterministic versions used by test vectors. d, z, and m
;;;;     are 32 bytes each. The non-internal variants just generate
;;;;     fresh kernel-random inputs and delegate.
;;;;
;;;; Key and ciphertext layouts
;;;; --------------------------
;;;;
;;;;   ek (1184 bytes) = ek_PKE  (directly the K-PKE public key)
;;;;
;;;;   dk (2400 bytes) = dk_PKE      (1152) ||
;;;;                     ek          (1184) ||
;;;;                     H(ek)         (32) ||
;;;;                     z             (32)
;;;;     where H = SHA3-256, and z is the "implicit rejection" secret
;;;;     drawn at key-generation time.
;;;;
;;;;   ciphertext (1088 bytes) = K-PKE ciphertext (no additional framing)
;;;;
;;;; Decaps is constant-time in the ciphertext-equality check via
;;;; `ct-equal` and in the final (K' vs K̄) selection via
;;;; `ct-select-byte`. Decaps does NOT branch on whether re-encryption
;;;; matched; both branches are always fully computed and the result
;;;; is XOR-selected.

(defpackage epsilon.crypto.ml-kem
  (:use :cl)
  (:import (epsilon.crypto.ml-kem-k-pke k-pke)
            (epsilon.crypto.ml-kem-codec codec)
            (epsilon.crypto.sha3 sha3)
            (epsilon.crypto.drbg drbg)
            (epsilon.crypto.ct ct))
  (:import-from :epsilon.crypto.primitives #:ensure-byte-vector)
  (:export
   #:+ek-length+
   #:+dk-length+
   #:+ciphertext-length+
   #:+shared-secret-length+
   ;; Parameter set selection (FIPS 203 Table 2)
   #:resolve-parameter-set
   #:parameter-set-ek-length
   #:parameter-set-dk-length
   #:parameter-set-ciphertext-length
   #:keygen
   #:encaps
   #:decaps
   #:keygen-internal
   #:encaps-internal
   #:validate-ek
   #:validate-dk
   #:ml-kem-error))

(in-package :epsilon.crypto.ml-kem)

;;; ---------------------------------------------------------------------------
;;; Layout constants
;;; ---------------------------------------------------------------------------

(defconstant +ek-length+ k-pke:+ek-pke-length+)    ; 1184
(defconstant +dk-length+ (+ (* 2 k-pke:+ek-pke-length+)
                            (- k-pke:+dk-pke-length+ k-pke:+ek-pke-length+)
                            64))                    ; 2400
(defconstant +ciphertext-length+ k-pke:+ciphertext-length+)  ; 1088
(defconstant +shared-secret-length+ 32)

;;; Parameter-set resolution. Callers select a set via :parameter-set
;;; on keygen/encaps/decaps; everything downstream reads the
;;; corresponding `k-pke:params` struct. The wrapper dk adds 2*32
;;; bytes of ancillary state (h=SHA3-256(ek), z) on top of the K-PKE
;;; dk and includes a copy of ek, so:
;;;
;;;   wrapper dk = dk_PKE || ek || H(ek) || z
;;;              = 384·k + (384·k + 32) + 32 + 32
;;;              = 768·k + 96
;;;
;;; giving dk = 1632, 2400, 3168 for ML-KEM-512/768/1024.

(defun resolve-parameter-set (selector)
  "Map a SELECTOR into a `k-pke:params`. SELECTOR can be one of
   :ml-kem-512, :ml-kem-768, :ml-kem-1024, or an already-built
   `k-pke:params` (returned unchanged)."
  (etypecase selector
    (k-pke:params selector)
    (keyword
     (ecase selector
       (:ml-kem-512  k-pke:*ml-kem-512-params*)
       (:ml-kem-768  k-pke:*ml-kem-768-params*)
       (:ml-kem-1024 k-pke:*ml-kem-1024-params*)))))

(defun parameter-set-ek-length (params)
  (k-pke:params-ek-pke-length (resolve-parameter-set params)))

(defun parameter-set-dk-length (params)
  (let ((p (resolve-parameter-set params)))
    (+ (k-pke:params-dk-pke-length p)
       (k-pke:params-ek-pke-length p)
       64)))

(defun parameter-set-ciphertext-length (params)
  (k-pke:params-ciphertext-length (resolve-parameter-set params)))

(define-condition ml-kem-error (error)
  ((message :initarg :message :reader ml-kem-error-message))
  (:report (lambda (c s) (format s "ML-KEM: ~A" (ml-kem-error-message c)))))

(defun ml-kem-error (fmt &rest args)
  (error 'ml-kem-error :message (apply #'format nil fmt args)))

;;; ---------------------------------------------------------------------------
;;; Hash helpers
;;; ---------------------------------------------------------------------------

(defun %h (bytes)
  "H (FIPS 203 §4.1): SHA3-256, used to summarise ek into a 32-byte hash."
  (sha3:sha3-256 bytes))

(defun %g (bytes)
  "G (FIPS 203 §4.1): SHA3-512 split into two 32-byte halves (K, r)."
  (let ((hash (sha3:sha3-512 bytes)))
    (values (subseq hash 0 32) (subseq hash 32 64))))

(defun %j (bytes)
  "J (FIPS 203 §4.1): SHAKE256 with 32-byte output. Used to derive the
   implicit-rejection value K̄ from z || c when decapsulation fails."
  (sha3:shake256 bytes 32))

;;; ---------------------------------------------------------------------------
;;; Byte-vector helpers
;;; ---------------------------------------------------------------------------

(defun %concat (&rest arrays)
  "Concatenate byte vectors into a fresh simple-array."
  (let* ((total (reduce #'+ arrays :key #'length))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (off 0))
    (dolist (arr arrays)
      (replace out arr :start1 off)
      (incf off (length arr)))
    out))


;;; ---------------------------------------------------------------------------
;;; Input validation (FIPS 203 §7.2 / §7.3)
;;; ---------------------------------------------------------------------------

(defun validate-ek (ek &optional (params :ml-kem-768))
  "Encapsulation Key Check (FIPS 203 §7.2). Checks:
     1. |ek| matches the parameter set.
     2. ByteEncode_12(ByteDecode_12(ek[0:384·k])) = ek[0:384·k]. The
        `modulus check': ensures every 12-bit value packed into ek
        is a canonical element of Z_q (< 3329), rejecting inputs
        where the upper 767 codepoints have been abused.

   Signals ML-KEM-ERROR on failure."
  (let* ((params (resolve-parameter-set params))
         (k (k-pke:params-k params))
         (ek-len (k-pke:params-ek-pke-length params))
         (ek (ensure-byte-vector ek)))
    (unless (= (length ek) ek-len)
      (ml-kem-error "invalid ek length: ~D (expected ~D)"
                    (length ek) ek-len))
    (let* ((t-bytes-len (* 384 k))
           (t-bytes (subseq ek 0 t-bytes-len)))
      (let* ((t-hat (loop for i from 0 below k
                          collect (codec:byte-decode
                                    (subseq t-bytes (* i 384) (* (1+ i) 384))
                                    12)))
             (re-encoded
               (let ((chunks
                       (loop for p in t-hat
                             collect (codec:byte-encode p 12))))
                 (apply #'%concat chunks))))
        (unless (equalp re-encoded t-bytes)
          (ml-kem-error "ek modulus check failed: stored t̂ coefficients ~
                         are not all in [0, q)"))))
    t))

(defun validate-dk (dk &optional (params :ml-kem-768))
  "Decapsulation Key Check (FIPS 203 §7.3). Checks:
     1. |dk| matches the parameter set.
     2. H(ek) matches the stored hash at offset dk_PKE + ek.

   Signals ML-KEM-ERROR on failure."
  (let* ((params (resolve-parameter-set params))
         (dk-pke-len (k-pke:params-dk-pke-length params))
         (ek-len     (k-pke:params-ek-pke-length params))
         (dk-len     (parameter-set-dk-length params))
         (dk (ensure-byte-vector dk)))
    (unless (= (length dk) dk-len)
      (ml-kem-error "invalid dk length: ~D (expected ~D)"
                    (length dk) dk-len))
    (let* ((ek (subseq dk dk-pke-len (+ dk-pke-len ek-len)))
           (stored-hash
             (subseq dk (+ dk-pke-len ek-len)
                     (+ dk-pke-len ek-len 32)))
           (computed-hash (%h ek)))
      (unless (equalp stored-hash computed-hash)
        (ml-kem-error "dk hash check failed: stored H(ek) does not match ~
                       H of the embedded ek")))
    t))

;;; ---------------------------------------------------------------------------
;;; KeyGen (FIPS 203 Algorithms 16 and 19)
;;; ---------------------------------------------------------------------------

(defun keygen-internal (d z &optional (parameter-set :ml-kem-768))
  "ML-KEM.KeyGen_internal(d, z) -- FIPS 203 Algorithm 16.
   Deterministic variant that takes the two 32-byte seeds explicitly.
   PARAMETER-SET selects ML-KEM-512/768/1024 (default: 768).
   Returns (values ek dk)."
  (let ((params (resolve-parameter-set parameter-set))
        (d (ensure-byte-vector d))
        (z (ensure-byte-vector z)))
    (unless (= (length d) 32) (ml-kem-error "keygen-internal: d must be 32 bytes"))
    (unless (= (length z) 32) (ml-kem-error "keygen-internal: z must be 32 bytes"))
    (multiple-value-bind (ek-pke dk-pke) (k-pke:key-gen d params)
      (let* ((ek ek-pke)
             (dk (%concat dk-pke ek (%h ek) z)))
        (values ek dk)))))

(defun keygen (&optional (parameter-set :ml-kem-768))
  "ML-KEM.KeyGen() -- FIPS 203 Algorithm 19.
   Generate a fresh key pair using kernel entropy under PARAMETER-SET
   (default :ml-kem-768). Returns (values ek dk)."
  (keygen-internal (drbg:random-bytes 32) (drbg:random-bytes 32) parameter-set))

;;; ---------------------------------------------------------------------------
;;; Encaps (FIPS 203 Algorithms 17 and 20)
;;; ---------------------------------------------------------------------------

(defun encaps-internal (ek m &optional (parameter-set :ml-kem-768))
  "ML-KEM.Encaps_internal(ek, m) -- FIPS 203 Algorithm 17.
   Deterministic variant that takes the 32-byte seed m explicitly.
   PARAMETER-SET selects ML-KEM-512/768/1024 (default: 768).
   Returns (values shared-secret ciphertext)."
  (let ((params (resolve-parameter-set parameter-set))
        (ek (ensure-byte-vector ek))
        (m (ensure-byte-vector m)))
    (validate-ek ek params)
    (unless (= (length m) 32) (ml-kem-error "encaps-internal: m must be 32 bytes"))
    (multiple-value-bind (k-shared r) (%g (%concat m (%h ek)))
      (let ((ciphertext (k-pke:encrypt ek m r params)))
        (values k-shared ciphertext)))))

(defun encaps (ek &optional (parameter-set :ml-kem-768))
  "ML-KEM.Encaps(ek) -- FIPS 203 Algorithm 20.
   Generate a fresh shared secret and the ciphertext that encapsulates
   it to the owner of the corresponding dk under PARAMETER-SET
   (default :ml-kem-768). Returns (values shared-secret ciphertext)."
  (encaps-internal ek (drbg:random-bytes 32) parameter-set))

;;; ---------------------------------------------------------------------------
;;; Decaps (FIPS 203 Algorithm 18)
;;; ---------------------------------------------------------------------------

(defun %ct-select-bytes (condition a b)
  "Return a fresh byte vector that is A if CONDITION is true, B otherwise.
   Runs in constant time by selecting each byte via ct-select-byte. A
   and B must be the same length."
  (declare (type (simple-array (unsigned-byte 8) (*)) a b))
  (let* ((len (length a))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref out i)
                   (ct:ct-select-byte condition (aref a i) (aref b i))))
    out))

(defun decaps (dk c &optional (parameter-set :ml-kem-768))
  "ML-KEM.Decaps(dk, c) -- FIPS 203 Algorithm 18 (+ the outer wrapper of
   Algorithm 21, which is a pure pass-through). PARAMETER-SET selects
   ML-KEM-512/768/1024 (default: 768).

   Returns a 32-byte shared secret. On valid ciphertext, the secret
   matches the one returned by the corresponding `encaps` call. On
   invalid (or tampered) ciphertext, returns a pseudo-random 32-byte
   value derived from the implicit rejection secret z; the caller
   cannot distinguish this from a valid secret without a separate
   validation channel, and that is the IND-CCA2 property at work."
  (let* ((params (resolve-parameter-set parameter-set))
         (dk-pke-len (k-pke:params-dk-pke-length params))
         (ek-len     (k-pke:params-ek-pke-length params))
         (ct-len     (k-pke:params-ciphertext-length params))
         (dk (ensure-byte-vector dk))
         (c (ensure-byte-vector c)))
    (validate-dk dk params)
    (unless (= (length c) ct-len)
      (ml-kem-error "decaps: ciphertext must be ~D bytes (got ~D)"
                    ct-len (length c)))
    ;; Parse dk into (dk_PKE, ek, h, z).
    (let* ((dk-pke (subseq dk 0 dk-pke-len))
           (ek-pke (subseq dk dk-pke-len (+ dk-pke-len ek-len)))
           (h (subseq dk (+ dk-pke-len ek-len)
                      (+ dk-pke-len ek-len 32)))
           (z (subseq dk (+ dk-pke-len ek-len 32)
                      (+ dk-pke-len ek-len 64)))
           ;; Step 1: m' = K-PKE.Decrypt(dk_PKE, c)
           (m-prime (k-pke:decrypt dk-pke c params)))
      (multiple-value-bind (k-prime r-prime) (%g (%concat m-prime h))
        ;; Step 3: K̄ = J(z || c)  -- the implicit rejection secret.
        (let* ((k-bar (%j (%concat z c)))
               ;; Step 4: c' = K-PKE.Encrypt(ek, m', r') -- re-encrypt.
               (c-prime (k-pke:encrypt ek-pke m-prime r-prime params))
               ;; Constant-time c == c' and constant-time select
               ;; between K' (the "good" secret) and K̄ (implicit reject).
               (valid (ct:ct-equal c c-prime)))
          (%ct-select-bytes valid k-prime k-bar))))))
