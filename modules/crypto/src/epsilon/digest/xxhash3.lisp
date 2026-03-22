;;;; epsilon.digest.xxhash3 - xxHash implementation (pure-Lisp)
;;;;
;;;; Provides xxHash64 and xxHash32 non-cryptographic hash functions
;;;; using the pure-Lisp implementation from epsilon.ssl.xxhash.
;;;;
;;;; Use cases:
;;;; - Hash tables and data structures
;;;; - File checksums and deduplication
;;;; - Fast content addressing
;;;;
;;;; WARNING: xxHash is NOT cryptographically secure.
;;;; Do not use for passwords, signatures, or security.

(defpackage epsilon.digest.xxhash3
  (:use :cl)
  (:require (epsilon.digest.protocol proto)
            (epsilon.typeclass tc)
            (epsilon.ssl.xxhash xxh))
  (:enter t))

;;; ============================================================================
;;; xxHash64 Hasher (pure-Lisp, streaming)
;;; ============================================================================

(defstruct (xxhash64-hasher (:constructor %make-xxhash64-hasher))
  "xxHash64 stateful hasher."
  (state nil :type (or null xxh::xxh64-state))
  (seed 0 :type (unsigned-byte 64))
  (finalized-p nil :type boolean))

(defun make-xxhash64-hasher (&key (seed 0))
  "Create a new xxHash64 hasher with optional SEED."
  (declare (type (unsigned-byte 64) seed))
  (%make-xxhash64-hasher :state (xxh:make-xxh64-state :seed seed) :seed seed))

;;; ============================================================================
;;; xxHash128 Hasher (pure-Lisp, two XXH64 with different seeds)
;;; ============================================================================

(defstruct (xxhash128-hasher (:constructor %make-xxhash128-hasher))
  "xxHash128 stateful hasher (dual XXH64)."
  (state-lo nil :type (or null xxh::xxh64-state))
  (state-hi nil :type (or null xxh::xxh64-state))
  (seed 0 :type (unsigned-byte 64))
  (finalized-p nil :type boolean))

(defun make-xxhash128-hasher (&key (seed 0))
  "Create a new xxHash128 hasher with optional SEED."
  (declare (type (unsigned-byte 64) seed))
  (let ((seed-hi (logand #xFFFFFFFFFFFFFFFF (+ seed 1))))
    (%make-xxhash128-hasher
     :state-lo (xxh:make-xxh64-state :seed seed)
     :state-hi (xxh:make-xxh64-state :seed seed-hi)
     :seed seed)))

;;; ============================================================================
;;; Hasher Protocol - 64-bit
;;; ============================================================================

(tc:definstance proto:hasher xxhash64-hasher
  (proto:hasher-update (hasher data &key (start 0) (end (length data)))
    (when (xxhash64-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (xxh:xxh64-update (xxhash64-hasher-state hasher) data :start start :end end)
    hasher)

  (proto:hasher-finalize (hasher &key output output-length)
    (declare (ignore output-length))
    (when (xxhash64-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (setf (xxhash64-hasher-finalized-p hasher) t)
    (let ((hash (xxh:xxh64-finalize (xxhash64-hasher-state hasher))))
      (if output
          (progn
            (setf (aref output 0) (ldb (byte 8 0) hash)
                  (aref output 1) (ldb (byte 8 8) hash)
                  (aref output 2) (ldb (byte 8 16) hash)
                  (aref output 3) (ldb (byte 8 24) hash)
                  (aref output 4) (ldb (byte 8 32) hash)
                  (aref output 5) (ldb (byte 8 40) hash)
                  (aref output 6) (ldb (byte 8 48) hash)
                  (aref output 7) (ldb (byte 8 56) hash))
            output)
          hash)))

  (proto:hasher-reset (hasher)
    (setf (xxhash64-hasher-state hasher)
          (xxh:make-xxh64-state :seed (xxhash64-hasher-seed hasher)))
    (setf (xxhash64-hasher-finalized-p hasher) nil)
    hasher)

  (proto:hasher-copy (hasher)
    (when (xxhash64-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (%make-xxhash64-hasher
     :state (xxh:xxh64-copy (xxhash64-hasher-state hasher))
     :seed (xxhash64-hasher-seed hasher)))

  (proto:hasher-algorithm (hasher)
    (declare (ignore hasher))
    :xxhash64)

  (proto:hasher-output-length (hasher)
    (declare (ignore hasher))
    8)

  (proto:hasher-block-length (hasher)
    (declare (ignore hasher))
    32))

;;; ============================================================================
;;; Hasher Protocol - 128-bit
;;; ============================================================================

(tc:definstance proto:hasher xxhash128-hasher
  (proto:hasher-update (hasher data &key (start 0) (end (length data)))
    (when (xxhash128-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (xxh:xxh64-update (xxhash128-hasher-state-lo hasher) data :start start :end end)
    (xxh:xxh64-update (xxhash128-hasher-state-hi hasher) data :start start :end end)
    hasher)

  (proto:hasher-finalize (hasher &key output output-length)
    (declare (ignore output-length))
    (when (xxhash128-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (setf (xxhash128-hasher-finalized-p hasher) t)
    (let ((low (xxh:xxh64-finalize (xxhash128-hasher-state-lo hasher)))
          (high (xxh:xxh64-finalize (xxhash128-hasher-state-hi hasher))))
      (if output
          (progn
            ;; Write low64 (little-endian)
            (setf (aref output 0) (ldb (byte 8 0) low)
                  (aref output 1) (ldb (byte 8 8) low)
                  (aref output 2) (ldb (byte 8 16) low)
                  (aref output 3) (ldb (byte 8 24) low)
                  (aref output 4) (ldb (byte 8 32) low)
                  (aref output 5) (ldb (byte 8 40) low)
                  (aref output 6) (ldb (byte 8 48) low)
                  (aref output 7) (ldb (byte 8 56) low))
            ;; Write high64 (little-endian)
            (setf (aref output 8) (ldb (byte 8 0) high)
                  (aref output 9) (ldb (byte 8 8) high)
                  (aref output 10) (ldb (byte 8 16) high)
                  (aref output 11) (ldb (byte 8 24) high)
                  (aref output 12) (ldb (byte 8 32) high)
                  (aref output 13) (ldb (byte 8 40) high)
                  (aref output 14) (ldb (byte 8 48) high)
                  (aref output 15) (ldb (byte 8 56) high))
            output)
          (values low high))))

  (proto:hasher-reset (hasher)
    (let ((seed (xxhash128-hasher-seed hasher)))
      (setf (xxhash128-hasher-state-lo hasher)
            (xxh:make-xxh64-state :seed seed))
      (setf (xxhash128-hasher-state-hi hasher)
            (xxh:make-xxh64-state :seed (logand #xFFFFFFFFFFFFFFFF (+ seed 1)))))
    (setf (xxhash128-hasher-finalized-p hasher) nil)
    hasher)

  (proto:hasher-copy (hasher)
    (when (xxhash128-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (%make-xxhash128-hasher
     :state-lo (xxh:xxh64-copy (xxhash128-hasher-state-lo hasher))
     :state-hi (xxh:xxh64-copy (xxhash128-hasher-state-hi hasher))
     :seed (xxhash128-hasher-seed hasher)))

  (proto:hasher-algorithm (hasher)
    (declare (ignore hasher))
    :xxhash128)

  (proto:hasher-output-length (hasher)
    (declare (ignore hasher))
    16)

  (proto:hasher-block-length (hasher)
    (declare (ignore hasher))
    32))

;;; ============================================================================
;;; One-Shot Functions
;;; ============================================================================

(defun xxhash64 (data &key (start 0) (end (length data)) (seed 0))
  "Compute xxHash64 hash of DATA[start:end].
   Returns an unsigned 64-bit integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (type (unsigned-byte 64) seed))
  (xxh:xxh64 data :start start :end end :seed seed))

(defun xxhash128 (data &key (start 0) (end (length data)) (seed 0))
  "Compute xxHash128 hash of DATA[start:end].
   Returns (values low64 high64)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (type (unsigned-byte 64) seed))
  (values (xxh:xxh64 data :start start :end end :seed seed)
          (xxh:xxh64 data :start start :end end
                     :seed (logand #xFFFFFFFFFFFFFFFF (+ seed 1)))))

(defun xxhash64-bytes (data &key (start 0) (end (length data)) (seed 0))
  "Compute xxHash64 hash and return as 8-byte array (little-endian)."
  (let ((hash (xxhash64 data :start start :end end :seed seed))
        (result (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (aref result 0) (ldb (byte 8 0) hash)
          (aref result 1) (ldb (byte 8 8) hash)
          (aref result 2) (ldb (byte 8 16) hash)
          (aref result 3) (ldb (byte 8 24) hash)
          (aref result 4) (ldb (byte 8 32) hash)
          (aref result 5) (ldb (byte 8 40) hash)
          (aref result 6) (ldb (byte 8 48) hash)
          (aref result 7) (ldb (byte 8 56) hash))
    result))

(defun xxhash128-bytes (data &key (start 0) (end (length data)) (seed 0))
  "Compute xxHash128 hash and return as 16-byte array (little-endian)."
  (multiple-value-bind (low high)
      (xxhash128 data :start start :end end :seed seed)
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (setf (aref result 0) (ldb (byte 8 0) low)
            (aref result 1) (ldb (byte 8 8) low)
            (aref result 2) (ldb (byte 8 16) low)
            (aref result 3) (ldb (byte 8 24) low)
            (aref result 4) (ldb (byte 8 32) low)
            (aref result 5) (ldb (byte 8 40) low)
            (aref result 6) (ldb (byte 8 48) low)
            (aref result 7) (ldb (byte 8 56) low))
      (setf (aref result 8) (ldb (byte 8 0) high)
            (aref result 9) (ldb (byte 8 8) high)
            (aref result 10) (ldb (byte 8 16) high)
            (aref result 11) (ldb (byte 8 24) high)
            (aref result 12) (ldb (byte 8 32) high)
            (aref result 13) (ldb (byte 8 40) high)
            (aref result 14) (ldb (byte 8 48) high)
            (aref result 15) (ldb (byte 8 56) high))
      result)))

;;; ============================================================================
;;; Classic xxHash64 (direct alias)
;;; ============================================================================

(defun xxh64 (data &key (start 0) (end (length data)) (seed 0))
  "Compute classic XXH64 hash of DATA[start:end]."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (type (unsigned-byte 64) seed))
  (xxh:xxh64 data :start start :end end :seed seed))

;;; ============================================================================
;;; Hash-Bytes Methods
;;; ============================================================================

(defmethod proto:hash-bytes ((algorithm (eql :xxhash64)) data &key (start 0) (end (length data))
                                                                  key output-length)
  "Compute xxHash64 via generic interface.
   KEY is used as seed if provided. Returns an integer."
  (declare (ignore output-length))
  (xxhash64 data :start start :end end :seed (or key 0)))

(defmethod proto:hash-bytes ((algorithm (eql :xxhash128)) data &key (start 0) (end (length data))
                                                                   key output-length)
  "Compute xxHash128 via generic interface.
   KEY is used as seed if provided. Returns (values low64 high64)."
  (declare (ignore output-length))
  (xxhash128 data :start start :end end :seed (or key 0)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun xxhash-available-p ()
  "Return T if xxHash is available. Always T (pure-Lisp implementation)."
  t)

(defun xxhash64-to-hex (hash)
  "Convert xxHash64 integer to hex string."
  (format nil "~16,'0X" hash))

(defun xxhash128-to-hex (low high)
  "Convert xxHash128 to hex string."
  (format nil "~16,'0X~16,'0X" high low))
