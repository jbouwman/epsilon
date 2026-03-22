;;;; HMAC-DRBG (NIST SP 800-90A)
;;;;
;;;; Deterministic Random Bit Generator using HMAC-SHA256.
;;;; Used as the primary CSPRNG for all key generation and nonce creation,
;;;; seeded from platform entropy.
;;;;
;;;; Reference: NIST SP 800-90A Rev. 1, Section 10.1.2

(defpackage epsilon.ssl.drbg
  (:use :cl)
  (:local-nicknames
   (#:hmac #:epsilon.ssl.hmac)
   (#:ct #:epsilon.ssl.ct)
   (#:entropy #:epsilon.ssl.entropy))
  (:export
   ;; HMAC-DRBG state management
   #:make-hmac-drbg
   #:drbg-generate
   #:drbg-reseed
   ;; Global DRBG
   #:*drbg*
   #:ensure-drbg
   #:random-bytes
   #:random-integer))

(in-package :epsilon.ssl.drbg)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +outlen+ 32
  "Output length of HMAC-SHA256 in bytes.")

(defconstant +seed-len+ 48
  "Seed length: 32 bytes entropy + 16 bytes nonce.")

(defconstant +reseed-interval+ (expt 2 48)
  "Maximum number of generate calls before mandatory reseed.")

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG state
;;; ---------------------------------------------------------------------------

(defstruct (hmac-drbg (:constructor %make-hmac-drbg))
  "HMAC-DRBG internal state per SP 800-90A Section 10.1.2."
  ;; Key (K): 32-byte HMAC key
  (key (make-array +outlen+ :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (32)))
  ;; Value (V): 32-byte chaining value
  (v (make-array +outlen+ :element-type '(unsigned-byte 8) :initial-element #x01)
   :type (simple-array (unsigned-byte 8) (32)))
  ;; Reseed counter
  (reseed-counter 1 :type (unsigned-byte 64)))

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG Update (SP 800-90A Section 10.1.2.2)
;;; ---------------------------------------------------------------------------

(defun drbg-update (state provided-data)
  "Update the DRBG state with optional provided-data.
   PROVIDED-DATA may be nil (no additional input) or a byte array."
  (declare (type hmac-drbg state))
  (let ((k (hmac-drbg-key state))
        (v (hmac-drbg-v state)))
    ;; Step 1: K = HMAC(K, V || 0x00 || provided_data)
    (let* ((separator (make-array 1 :element-type '(unsigned-byte 8)
                                    :initial-element #x00))
           (concat (if provided-data
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    v separator provided-data)
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    v separator))))
      (let ((new-k (hmac:hmac-sha256 k concat)))
        (replace k new-k)))
    ;; Step 2: V = HMAC(K, V)
    (let ((new-v (hmac:hmac-sha256 k v)))
      (replace v new-v))
    ;; Step 3: If provided_data is non-nil, do another round
    (when (and provided-data (> (length provided-data) 0))
      ;; K = HMAC(K, V || 0x01 || provided_data)
      (let* ((separator (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element #x01))
             (concat (concatenate '(simple-array (unsigned-byte 8) (*))
                                  v separator provided-data)))
        (let ((new-k (hmac:hmac-sha256 k concat)))
          (replace k new-k)))
      ;; V = HMAC(K, V)
      (let ((new-v (hmac:hmac-sha256 k v)))
        (replace v new-v))))
  state)

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG Instantiate (SP 800-90A Section 10.1.2.3)
;;; ---------------------------------------------------------------------------

(defun make-hmac-drbg (&key entropy-input nonce personalization)
  "Create and seed an HMAC-DRBG instance.
   ENTROPY-INPUT: byte array of entropy (at least 32 bytes).
   NONCE: byte array (at least 16 bytes).
   PERSONALIZATION: optional byte array.
   If ENTROPY-INPUT is nil, platform entropy is used."
  (let* ((entropy (or entropy-input (entropy:random-bytes 32)))
         (nonce (or nonce (entropy:random-bytes 16)))
         (seed-material (if personalization
                            (concatenate '(simple-array (unsigned-byte 8) (*))
                                         entropy nonce personalization)
                            (concatenate '(simple-array (unsigned-byte 8) (*))
                                         entropy nonce)))
         (state (%make-hmac-drbg)))
    ;; K is initialized to all-zeros, V to all-ones (by constructor)
    ;; Then update with seed material
    (drbg-update state seed-material)
    (setf (hmac-drbg-reseed-counter state) 1)
    state))

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG Reseed (SP 800-90A Section 10.1.2.4)
;;; ---------------------------------------------------------------------------

(defun drbg-reseed (state &key entropy-input additional-input)
  "Reseed the DRBG with fresh entropy.
   ENTROPY-INPUT: byte array (at least 32 bytes). If nil, uses platform entropy.
   ADDITIONAL-INPUT: optional byte array."
  (declare (type hmac-drbg state))
  (let* ((entropy (or entropy-input (entropy:random-bytes 32)))
         (seed-material (if additional-input
                            (concatenate '(simple-array (unsigned-byte 8) (*))
                                         entropy additional-input)
                            entropy)))
    (drbg-update state seed-material)
    (setf (hmac-drbg-reseed-counter state) 1))
  state)

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG Generate (SP 800-90A Section 10.1.2.5)
;;; ---------------------------------------------------------------------------

(defun drbg-generate (state requested-bytes &key additional-input)
  "Generate REQUESTED-BYTES pseudorandom bytes from the DRBG.
   Returns a byte array of length REQUESTED-BYTES."
  (declare (type hmac-drbg state)
           (type (integer 1) requested-bytes))
  ;; Step 1: Check reseed counter
  (when (> (hmac-drbg-reseed-counter state) +reseed-interval+)
    (drbg-reseed state))
  ;; Step 2: If additional input, update state
  (when (and additional-input (> (length additional-input) 0))
    (drbg-update state additional-input))
  ;; Step 3: Generate output blocks
  (let ((output (make-array requested-bytes :element-type '(unsigned-byte 8)
                                            :initial-element 0))
        (k (hmac-drbg-key state))
        (v (hmac-drbg-v state))
        (generated 0))
    (declare (type fixnum generated))
    (loop while (< generated requested-bytes)
          do ;; V = HMAC(K, V)
             (let ((new-v (hmac:hmac-sha256 k v)))
               (replace v new-v))
             ;; Copy output bytes
             (let ((copy-len (min +outlen+ (- requested-bytes generated))))
               (replace output v :start1 generated :end1 (+ generated copy-len)
                                 :start2 0 :end2 copy-len)
               (incf generated copy-len)))
    ;; Step 4: Update state (backtracking resistance)
    (drbg-update state additional-input)
    ;; Step 5: Increment reseed counter
    (incf (hmac-drbg-reseed-counter state))
    output))

;;; ---------------------------------------------------------------------------
;;; Global DRBG instance
;;; ---------------------------------------------------------------------------

(defvar *drbg* nil
  "Global HMAC-DRBG instance. Lazily initialized on first use.")

(defun ensure-drbg ()
  "Ensure the global DRBG is initialized. Returns the DRBG instance."
  (or *drbg*
      (setf *drbg* (make-hmac-drbg))))

(defun random-bytes (n)
  "Generate N cryptographically secure random bytes using the global DRBG."
  (declare (type (integer 0) n))
  (when (zerop n)
    (return-from random-bytes
      (make-array 0 :element-type '(unsigned-byte 8))))
  (drbg-generate (ensure-drbg) n))

(defun random-integer (limit)
  "Generate a uniform random integer in [0, LIMIT).
   Uses rejection sampling to avoid modular bias."
  (declare (type (integer 1) limit))
  (let* ((byte-count (ceiling (integer-length limit) 8))
         (mask (1- (ash 1 (* byte-count 8)))))
    (loop
      (let* ((bytes (random-bytes byte-count))
             (value (let ((v 0))
                      (loop for i from 0 below byte-count
                            do (setf v (logior (ash v 8) (aref bytes i))))
                      v)))
        (let ((masked (logand value mask)))
          (when (< masked limit)
            (return masked)))))))
