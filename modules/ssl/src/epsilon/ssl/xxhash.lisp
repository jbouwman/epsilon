;;;; xxHash (XXH64 and XXH32)
;;;;
;;;; Pure-Lisp implementation of the xxHash non-cryptographic hash functions.
;;;; Eliminates the C libxxhash dependency, consistent with the SSL module's
;;;; zero-C-dependencies philosophy.
;;;;
;;;; Reference: https://github.com/Cyan4973/xxHash/blob/dev/doc/xxhash_spec.md

(defpackage epsilon.ssl.xxhash
  (:use :cl :epsilon.ssl.primitives)
  (:export
   ;; XXH64
   #:xxh64  #:xxh64-hex
   #:make-xxh64-state  #:xxh64-update  #:xxh64-finalize  #:xxh64-copy
   ;; XXH32
   #:xxh32  #:xxh32-hex
   #:make-xxh32-state  #:xxh32-update  #:xxh32-finalize  #:xxh32-copy
   ;; Constants
   #:+xxh64-block-size+  #:+xxh64-digest-size+
   #:+xxh32-block-size+  #:+xxh32-digest-size+))

(in-package :epsilon.ssl.xxhash)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

;; XXH64 primes (from xxhash.h)
(defconstant +xxh64-prime1+ #x9E3779B185EBCA87)
(defconstant +xxh64-prime2+ #xC2B2AE3D27D4EB4F)
(defconstant +xxh64-prime3+ #x165667B19E3779F9)
(defconstant +xxh64-prime4+ #x85EBCA77C2B2AE63)
(defconstant +xxh64-prime5+ #x27D4EB2F165667C5)

;; XXH32 primes (from xxhash.h)
(defconstant +xxh32-prime1+ #x9E3779B1)
(defconstant +xxh32-prime2+ #x85EBCA77)
(defconstant +xxh32-prime3+ #xC2B2AE3D)
(defconstant +xxh32-prime4+ #x27D4EB2F)
(defconstant +xxh32-prime5+ #x165667B1)

;; Block and digest sizes
(defconstant +xxh64-block-size+ 32
  "XXH64 processes data in 32-byte stripes (4 lanes x 8 bytes).")
(defconstant +xxh64-digest-size+ 8
  "XXH64 produces an 8-byte (64-bit) digest.")
(defconstant +xxh32-block-size+ 16
  "XXH32 processes data in 16-byte stripes (4 lanes x 4 bytes).")
(defconstant +xxh32-digest-size+ 4
  "XXH32 produces a 4-byte (32-bit) digest.")

;;; ---------------------------------------------------------------------------
;;; XXH64 core functions
;;; ---------------------------------------------------------------------------

(declaim (inline xxh64-round xxh64-merge-acc))

(defun xxh64-round (acc input)
  "XXH64 accumulator round: rotl(acc + input * PRIME2, 31) * PRIME1."
  (declare (type (unsigned-byte 64) acc input)
           (optimize (speed 3) (safety 1)))
  (u64* (u64-rotl (u64+ acc (u64* input +xxh64-prime2+)) 31)
        +xxh64-prime1+))

(defun xxh64-merge-acc (acc val)
  "Merge an accumulator value into the hash: (acc XOR round(0, val)) * PRIME1 + PRIME4."
  (declare (type (unsigned-byte 64) acc val)
           (optimize (speed 3) (safety 1)))
  (u64+ (u64* (logxor acc (xxh64-round 0 val)) +xxh64-prime1+)
        +xxh64-prime4+))

(defun xxh64-avalanche (h)
  "XXH64 final avalanche: three XOR-shift-multiply steps."
  (declare (type (unsigned-byte 64) h)
           (optimize (speed 3) (safety 1)))
  (setf h (u64* (logxor h (ash h -33)) +xxh64-prime2+))
  (setf h (u64* (logxor h (ash h -29)) +xxh64-prime3+))
  (logxor h (ash h -32)))

;;; ---------------------------------------------------------------------------
;;; XXH64 state
;;; ---------------------------------------------------------------------------

(defstruct (xxh64-state (:constructor %make-xxh64-state))
  "Internal state for incremental XXH64 hashing."
  (acc1 0 :type (unsigned-byte 64))
  (acc2 0 :type (unsigned-byte 64))
  (acc3 0 :type (unsigned-byte 64))
  (acc4 0 :type (unsigned-byte 64))
  (buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (32)))
  (buffer-count 0 :type (integer 0 32))
  (total-length 0 :type (unsigned-byte 64))
  (seed 0 :type (unsigned-byte 64)))

(defun make-xxh64-state (&key (seed 0))
  "Create a fresh XXH64 hasher state with the given SEED."
  (let ((state (%make-xxh64-state)))
    (setf (xxh64-state-seed state) seed)
    (setf (xxh64-state-acc1 state) (u64+ seed +xxh64-prime1+ +xxh64-prime2+))
    (setf (xxh64-state-acc2 state) (u64+ seed +xxh64-prime2+))
    (setf (xxh64-state-acc3 state) seed)
    (setf (xxh64-state-acc4 state) (logand +u64-mask+ (- seed +xxh64-prime1+)))
    state))

(defun xxh64-copy (state)
  "Return an independent copy of an XXH64 state."
  (let ((new (%make-xxh64-state)))
    (setf (xxh64-state-acc1 new) (xxh64-state-acc1 state))
    (setf (xxh64-state-acc2 new) (xxh64-state-acc2 state))
    (setf (xxh64-state-acc3 new) (xxh64-state-acc3 state))
    (setf (xxh64-state-acc4 new) (xxh64-state-acc4 state))
    (replace (xxh64-state-buffer new) (xxh64-state-buffer state))
    (setf (xxh64-state-buffer-count new) (xxh64-state-buffer-count state))
    (setf (xxh64-state-total-length new) (xxh64-state-total-length state))
    (setf (xxh64-state-seed new) (xxh64-state-seed state))
    new))

;;; ---------------------------------------------------------------------------
;;; XXH64 stripe processing
;;; ---------------------------------------------------------------------------

(defun xxh64-process-stripe (state data offset)
  "Process a single 32-byte stripe from DATA starting at OFFSET."
  (declare (type xxh64-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (setf (xxh64-state-acc1 state)
        (xxh64-round (xxh64-state-acc1 state) (read-le64 data offset)))
  (setf (xxh64-state-acc2 state)
        (xxh64-round (xxh64-state-acc2 state) (read-le64 data (+ offset 8))))
  (setf (xxh64-state-acc3 state)
        (xxh64-round (xxh64-state-acc3 state) (read-le64 data (+ offset 16))))
  (setf (xxh64-state-acc4 state)
        (xxh64-round (xxh64-state-acc4 state) (read-le64 data (+ offset 24)))))

;;; ---------------------------------------------------------------------------
;;; XXH64 incremental API
;;; ---------------------------------------------------------------------------

(defun xxh64-update (state data &key (start 0) (end nil))
  "Feed DATA bytes into the XXH64 state."
  (declare (type xxh64-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start))
  (let* ((end (or end (length data)))
         (data-len (- end start))
         (buf (xxh64-state-buffer state))
         (buf-count (xxh64-state-buffer-count state)))
    (declare (type fixnum end data-len buf-count))
    (incf (xxh64-state-total-length state) data-len)
    (let ((pos start))
      (declare (type fixnum pos))
      ;; If there's data in the buffer, try to fill it to a complete stripe
      (when (> buf-count 0)
        (let ((copy-len (min (- 32 buf-count) data-len)))
          (declare (type fixnum copy-len))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 32)
            (xxh64-process-stripe state buf 0)
            (setf buf-count 0))))
      ;; Process complete stripes directly from input
      (loop while (<= (+ pos 32) end)
            do (xxh64-process-stripe state data pos)
               (incf pos 32))
      ;; Buffer remaining bytes
      (when (< pos end)
        (let ((remaining (- end pos)))
          (replace buf data :start1 0 :end1 remaining
                            :start2 pos :end2 end)
          (setf buf-count remaining)))
      (setf (xxh64-state-buffer-count state) buf-count)))
  state)

(defun xxh64-finalize (state)
  "Finalize the XXH64 computation and return the hash as a (unsigned-byte 64) integer.
   The state should not be used after this call."
  (declare (type xxh64-state state))
  (let ((total (xxh64-state-total-length state))
        (buf (xxh64-state-buffer state))
        (buf-count (xxh64-state-buffer-count state))
        (h 0))
    (declare (type (unsigned-byte 64) total h)
             (type (integer 0 32) buf-count))
    ;; Step 1: Converge accumulators or use seed+PRIME5
    (if (>= total 32)
        (let ((acc1 (xxh64-state-acc1 state))
              (acc2 (xxh64-state-acc2 state))
              (acc3 (xxh64-state-acc3 state))
              (acc4 (xxh64-state-acc4 state)))
          (setf h (u64+ (u64-rotl acc1 1) (u64-rotl acc2 7)
                        (u64-rotl acc3 12) (u64-rotl acc4 18)))
          (setf h (xxh64-merge-acc h acc1))
          (setf h (xxh64-merge-acc h acc2))
          (setf h (xxh64-merge-acc h acc3))
          (setf h (xxh64-merge-acc h acc4)))
        (setf h (u64+ (xxh64-state-seed state) +xxh64-prime5+)))
    ;; Step 2: Add total length
    (setf h (u64+ h total))
    ;; Step 3: Process remaining buffer
    (let ((pos 0))
      (declare (type fixnum pos))
      ;; 8-byte chunks
      (loop while (<= (+ pos 8) buf-count)
            do (let ((k (read-le64 buf pos)))
                 (setf h (logxor h (xxh64-round 0 k)))
                 (setf h (u64+ (u64* (u64-rotl h 27) +xxh64-prime1+)
                                +xxh64-prime4+)))
               (incf pos 8))
      ;; 4-byte chunks
      (loop while (<= (+ pos 4) buf-count)
            do (let ((k (read-le32 buf pos)))
                 (setf h (logxor h (u64* k +xxh64-prime1+)))
                 (setf h (u64+ (u64* (u64-rotl h 23) +xxh64-prime2+)
                                +xxh64-prime3+)))
               (incf pos 4))
      ;; Individual bytes
      (loop while (< pos buf-count)
            do (setf h (logxor h (u64* (aref buf pos) +xxh64-prime5+)))
               (setf h (u64* (u64-rotl h 11) +xxh64-prime1+))
               (incf pos 1)))
    ;; Step 4: Avalanche
    (xxh64-avalanche h)))

;;; ---------------------------------------------------------------------------
;;; XXH64 one-shot API
;;; ---------------------------------------------------------------------------

(defun xxh64 (data &key (start 0) (end nil) (seed 0))
  "Compute the XXH64 hash of DATA (a byte array).
   Returns the hash as a (unsigned-byte 64) integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-xxh64-state :seed seed)))
    (xxh64-update state data :start start :end end)
    (xxh64-finalize state)))

(defun xxh64-hex (data &key (start 0) (end nil) (seed 0))
  "Compute the XXH64 hash of DATA and return it as a 16-character uppercase hex string."
  (format nil "~16,'0X" (xxh64 data :start start :end end :seed seed)))

;;; ---------------------------------------------------------------------------
;;; XXH32 core functions
;;; ---------------------------------------------------------------------------

(declaim (inline xxh32-round))

(defun xxh32-round (acc input)
  "XXH32 accumulator round: rotl(acc + input * PRIME2, 13) * PRIME1."
  (declare (type (unsigned-byte 32) acc input)
           (optimize (speed 3) (safety 1)))
  (u32* (u32-rotl (u32+ acc (u32* input +xxh32-prime2+)) 13)
        +xxh32-prime1+))

(defun xxh32-avalanche (h)
  "XXH32 final avalanche: three XOR-shift-multiply steps."
  (declare (type (unsigned-byte 32) h)
           (optimize (speed 3) (safety 1)))
  (setf h (u32* (logxor h (ash h -15)) +xxh32-prime2+))
  (setf h (u32* (logxor h (ash h -13)) +xxh32-prime3+))
  (logxor h (ash h -16)))

;;; ---------------------------------------------------------------------------
;;; XXH32 state
;;; ---------------------------------------------------------------------------

(defstruct (xxh32-state (:constructor %make-xxh32-state))
  "Internal state for incremental XXH32 hashing."
  (acc1 0 :type (unsigned-byte 32))
  (acc2 0 :type (unsigned-byte 32))
  (acc3 0 :type (unsigned-byte 32))
  (acc4 0 :type (unsigned-byte 32))
  (buffer (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (16)))
  (buffer-count 0 :type (integer 0 16))
  (total-length 0 :type (unsigned-byte 64))
  (seed 0 :type (unsigned-byte 32)))

(defun make-xxh32-state (&key (seed 0))
  "Create a fresh XXH32 hasher state with the given SEED."
  (let ((state (%make-xxh32-state)))
    (setf (xxh32-state-seed state) seed)
    (setf (xxh32-state-acc1 state) (u32+ seed +xxh32-prime1+ +xxh32-prime2+))
    (setf (xxh32-state-acc2 state) (u32+ seed +xxh32-prime2+))
    (setf (xxh32-state-acc3 state) seed)
    (setf (xxh32-state-acc4 state) (logand +u32-mask+ (- seed +xxh32-prime1+)))
    state))

(defun xxh32-copy (state)
  "Return an independent copy of an XXH32 state."
  (let ((new (%make-xxh32-state)))
    (setf (xxh32-state-acc1 new) (xxh32-state-acc1 state))
    (setf (xxh32-state-acc2 new) (xxh32-state-acc2 state))
    (setf (xxh32-state-acc3 new) (xxh32-state-acc3 state))
    (setf (xxh32-state-acc4 new) (xxh32-state-acc4 state))
    (replace (xxh32-state-buffer new) (xxh32-state-buffer state))
    (setf (xxh32-state-buffer-count new) (xxh32-state-buffer-count state))
    (setf (xxh32-state-total-length new) (xxh32-state-total-length state))
    (setf (xxh32-state-seed new) (xxh32-state-seed state))
    new))

;;; ---------------------------------------------------------------------------
;;; XXH32 stripe processing
;;; ---------------------------------------------------------------------------

(defun xxh32-process-stripe (state data offset)
  "Process a single 16-byte stripe from DATA starting at OFFSET."
  (declare (type xxh32-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (setf (xxh32-state-acc1 state)
        (xxh32-round (xxh32-state-acc1 state) (read-le32 data offset)))
  (setf (xxh32-state-acc2 state)
        (xxh32-round (xxh32-state-acc2 state) (read-le32 data (+ offset 4))))
  (setf (xxh32-state-acc3 state)
        (xxh32-round (xxh32-state-acc3 state) (read-le32 data (+ offset 8))))
  (setf (xxh32-state-acc4 state)
        (xxh32-round (xxh32-state-acc4 state) (read-le32 data (+ offset 12)))))

;;; ---------------------------------------------------------------------------
;;; XXH32 incremental API
;;; ---------------------------------------------------------------------------

(defun xxh32-update (state data &key (start 0) (end nil))
  "Feed DATA bytes into the XXH32 state."
  (declare (type xxh32-state state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start))
  (let* ((end (or end (length data)))
         (data-len (- end start))
         (buf (xxh32-state-buffer state))
         (buf-count (xxh32-state-buffer-count state)))
    (declare (type fixnum end data-len buf-count))
    (incf (xxh32-state-total-length state) data-len)
    (let ((pos start))
      (declare (type fixnum pos))
      ;; If there's data in the buffer, try to fill it to a complete stripe
      (when (> buf-count 0)
        (let ((copy-len (min (- 16 buf-count) data-len)))
          (declare (type fixnum copy-len))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 16)
            (xxh32-process-stripe state buf 0)
            (setf buf-count 0))))
      ;; Process complete stripes directly from input
      (loop while (<= (+ pos 16) end)
            do (xxh32-process-stripe state data pos)
               (incf pos 16))
      ;; Buffer remaining bytes
      (when (< pos end)
        (let ((remaining (- end pos)))
          (replace buf data :start1 0 :end1 remaining
                            :start2 pos :end2 end)
          (setf buf-count remaining)))
      (setf (xxh32-state-buffer-count state) buf-count)))
  state)

(defun xxh32-finalize (state)
  "Finalize the XXH32 computation and return the hash as a (unsigned-byte 32) integer.
   The state should not be used after this call."
  (declare (type xxh32-state state))
  (let ((total (xxh32-state-total-length state))
        (buf (xxh32-state-buffer state))
        (buf-count (xxh32-state-buffer-count state))
        (h 0))
    (declare (type (unsigned-byte 64) total)
             (type (unsigned-byte 32) h)
             (type (integer 0 16) buf-count))
    ;; Step 1: Converge accumulators or use seed+PRIME5
    (if (>= total 16)
        (let ((acc1 (xxh32-state-acc1 state))
              (acc2 (xxh32-state-acc2 state))
              (acc3 (xxh32-state-acc3 state))
              (acc4 (xxh32-state-acc4 state)))
          (setf h (u32+ (u32-rotl acc1 1) (u32-rotl acc2 7)
                        (u32-rotl acc3 12) (u32-rotl acc4 18))))
        (setf h (u32+ (xxh32-state-seed state) +xxh32-prime5+)))
    ;; Step 2: Add total length (truncated to 32 bits)
    (setf h (u32+ h (logand +u32-mask+ total)))
    ;; Step 3: Process remaining buffer
    (let ((pos 0))
      (declare (type fixnum pos))
      ;; 4-byte chunks
      (loop while (<= (+ pos 4) buf-count)
            do (let ((k (read-le32 buf pos)))
                 (setf h (u32+ h (u32* k +xxh32-prime3+)))
                 (setf h (u32* (u32-rotl h 17) +xxh32-prime4+)))
               (incf pos 4))
      ;; Individual bytes
      (loop while (< pos buf-count)
            do (setf h (u32+ h (u32* (aref buf pos) +xxh32-prime5+)))
               (setf h (u32* (u32-rotl h 11) +xxh32-prime1+))
               (incf pos 1)))
    ;; Step 4: Avalanche
    (xxh32-avalanche h)))

;;; ---------------------------------------------------------------------------
;;; XXH32 one-shot API
;;; ---------------------------------------------------------------------------

(defun xxh32 (data &key (start 0) (end nil) (seed 0))
  "Compute the XXH32 hash of DATA (a byte array).
   Returns the hash as a (unsigned-byte 32) integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-xxh32-state :seed seed)))
    (xxh32-update state data :start start :end end)
    (xxh32-finalize state)))

(defun xxh32-hex (data &key (start 0) (end nil) (seed 0))
  "Compute the XXH32 hash of DATA and return it as an 8-character uppercase hex string."
  (format nil "~8,'0X" (xxh32 data :start start :end end :seed seed)))
