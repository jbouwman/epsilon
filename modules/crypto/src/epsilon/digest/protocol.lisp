;;;; epsilon.digest.protocol - Hasher protocol definitions
;;;;
;;;; Defines the core abstractions for hash computation:
;;;; - Hasher: stateful hash computation
;;;; - One-shot hashing interface
;;;; - IO integration protocols

(defpackage epsilon.digest.protocol
  (:use :cl)
  (:local-nicknames (tc epsilon.typeclass))
  (:enter t))

;;; ============================================================================
;;; Hasher Protocol
;;; ============================================================================

(tc:deftypeclass hasher ()
  "Interface for stateful hash computation."
  (hasher-update (hasher data &key start end)
    "Add DATA[start:end] to the hash computation.
     DATA must be a (simple-array (unsigned-byte 8) (*)).
     START defaults to 0, END defaults to (length data).
     Returns the hasher for method chaining.")
  (hasher-finalize (hasher &key output output-length)
    "Complete the hash computation and return the digest.
     After finalize, the hasher state is consumed. Call reset to reuse.")
  (hasher-reset (hasher)
    "Reset the hasher to its initial state.
     Preserves the hasher's mode (keyed, derive-key, etc.) and key.
     Returns the hasher for method chaining.")
  (hasher-copy (hasher)
    "Create an independent copy of the hasher state.")
  (hasher-algorithm (hasher)
    "Return a keyword identifying the hash algorithm.")
  (hasher-output-length (hasher)
    "Return the default output length in bytes.")
  (hasher-block-length (hasher)
    "Return the internal block size in bytes."))

;;; ============================================================================
;;; Hasher Predicates
;;; ============================================================================

(defun hasher-p (obj)
  "Return T if OBJ implements the hasher protocol."
  (and (compute-applicable-methods #'hasher-update
                                   (list obj #() :start 0 :end 0))
       t))

(defun cryptographic-hasher-p (hasher)
  "Return T if HASHER is a cryptographic hash function.
   Cryptographic hashes are suitable for security applications.
   Note: MD5 and SHA-1 are included but are considered weak/broken."
  (and (member (hasher-algorithm hasher)
               '(:blake3 :blake3-keyed :blake3-derive
                 :md5 :sha1
                 :sha256 :sha384 :sha512
                 :sha3-256 :sha3-384 :sha3-512))
       t))

(defun keyed-hasher-p (hasher)
  "Return T if HASHER is in keyed (MAC) mode."
  (and (member (hasher-algorithm hasher) '(:blake3-keyed :highway64 :highway128 :highway256))
       t))

;;; ============================================================================
;;; One-Shot Hashing Interface
;;; ============================================================================

(defgeneric hash-bytes (algorithm data &key start end key output-length)
  (:documentation
   "Compute hash of DATA[start:end] using ALGORITHM.

    ALGORITHM is a keyword like :blake3, :xxhash64, :xxhash128.
    KEY is for keyed algorithms (BLAKE3-keyed, Highway).
    OUTPUT-LENGTH is for variable-length algorithms (BLAKE3).

    Returns the hash as a byte array (or integer for xxHash64)."))

;;; ============================================================================
;;; Stream Hashing Protocol
;;; ============================================================================

(defgeneric hash-reader (reader algorithm &key key output-length buffer-size)
  (:documentation
   "Hash all data from READER using ALGORITHM.

    Reads until EOF, then returns the hash.
    BUFFER-SIZE controls internal buffer size (default 8192)."))

(defgeneric make-hashing-reader (source hasher)
  (:documentation
   "Create a reader that hashes all data as it passes through.

    SOURCE: the underlying reader
    HASHER: the hasher to update with each read

    When reading is complete, call (hasher-finalize hasher) to get the hash."))

(defgeneric make-hashing-writer (sink hasher)
  (:documentation
   "Create a writer that hashes all data as it passes through.

    SINK: the underlying writer
    HASHER: the hasher to update with each write

    When writing is complete, call (hasher-finalize hasher) to get the hash."))

;;; ============================================================================
;;; Algorithm Constants
;;; ============================================================================

(defconstant +blake3-output-length+ 32
  "Default output length for BLAKE3 in bytes.")

(defconstant +blake3-key-length+ 32
  "Key length for BLAKE3 keyed mode in bytes.")

(defconstant +blake3-block-length+ 64
  "Block size for BLAKE3 in bytes.")

(defconstant +xxhash64-output-length+ 8
  "Output length for xxHash64 in bytes.")

(defconstant +xxhash128-output-length+ 16
  "Output length for xxHash128 in bytes.")

;;; Traditional algorithm constants

(defconstant +md5-output-length+ 16
  "Output length for MD5 in bytes.")

(defconstant +sha1-output-length+ 20
  "Output length for SHA-1 in bytes.")

(defconstant +sha256-output-length+ 32
  "Output length for SHA-256 in bytes.")

(defconstant +sha384-output-length+ 48
  "Output length for SHA-384 in bytes.")

(defconstant +sha512-output-length+ 64
  "Output length for SHA-512 in bytes.")

(defconstant +sha3-256-output-length+ 32
  "Output length for SHA3-256 in bytes.")

(defconstant +sha3-384-output-length+ 48
  "Output length for SHA3-384 in bytes.")

(defconstant +sha3-512-output-length+ 64
  "Output length for SHA3-512 in bytes.")

(defconstant +crc32-output-length+ 4
  "Output length for CRC-32 in bytes.")

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition digest-error (error)
  ((message :initarg :message :reader digest-error-message))
  (:report (lambda (c s)
             (format s "Digest error: ~A" (digest-error-message c)))))

(define-condition algorithm-unavailable-error (digest-error)
  ((algorithm :initarg :algorithm :reader algorithm-unavailable-algorithm))
  (:report (lambda (c s)
             (format s "Hash algorithm ~A is not available. ~A"
                     (algorithm-unavailable-algorithm c)
                     (digest-error-message c)))))

(define-condition invalid-key-error (digest-error)
  ((expected-length :initarg :expected :reader invalid-key-expected-length)
   (actual-length :initarg :actual :reader invalid-key-actual-length))
  (:report (lambda (c s)
             (format s "Invalid key length: expected ~D bytes, got ~D bytes"
                     (invalid-key-expected-length c)
                     (invalid-key-actual-length c)))))

(define-condition hasher-finalized-error (digest-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Hasher has already been finalized. Call reset to reuse."))))
