;;;; epsilon.io.buffer - Efficient byte buffer implementation
;;;;
;;;; A mutable buffer with read/write cursors, inspired by Java NIO's ByteBuffer
;;;; and Rust's bytes::BytesMut. Designed for zero-copy operations where possible.
;;;;
;;;; Key concepts:
;;;; - POSITION: read cursor (where next read comes from)
;;;; - LIMIT: write cursor / read bound
;;;; - CAPACITY: total size of backing storage
;;;;
;;;; Invariant: 0 <= position <= limit <= capacity

(package epsilon.io.buffer
  (import (epsilon.sys.lock lock)
          (epsilon.io.protocol proto)
          (epsilon.io.conditions cond)))

;;; ============================================================================
;;; Buffer Structure
;;; ============================================================================

(defstruct (buf (:constructor %make-buf))
  "Mutable byte buffer with position tracking.

   The buffer has three regions:
   - [0, position):     already read (or: write position for output buffers)
   - [position, limit): readable data
   - [limit, capacity): writable space"
  (data nil :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)
  (limit 0 :type fixnum)
  (capacity 0 :type fixnum :read-only t)
  (mark -1 :type fixnum))

(defun make-buf (capacity &key (initial-contents nil))
  "Create a new buffer with CAPACITY bytes.

   If INITIAL-CONTENTS is provided, copies data into buffer and sets
   limit to the length of contents. Otherwise, position and limit start at 0."
  (declare (type fixnum capacity))
  (let* ((data (make-array capacity
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
         (buf (%make-buf :data data
                         :capacity capacity
                         :position 0
                         :limit (if initial-contents
                                    (length initial-contents)
                                    0))))
    (when initial-contents
      (replace data initial-contents))
    buf))

(defun buf-from-bytes (bytes)
  "Create a buffer containing a copy of BYTES.
   Position is 0, limit is (length bytes)."
  (make-buf (length bytes) :initial-contents bytes))

(defun buf-wrap (bytes)
  "Create a buffer that wraps BYTES directly (no copy).
   Mutations to the buffer affect the original array.
   Position is 0, limit is (length bytes)."
  (%make-buf :data bytes
             :capacity (length bytes)
             :position 0
             :limit (length bytes)))

;;; ============================================================================
;;; Buffer Queries
;;; ============================================================================

(declaim (inline buf-remaining buf-space buf-empty-p buf-full-p))

(defun buf-remaining (buf)
  "Return number of bytes available for reading: [position, limit)."
  (declare (type buf buf))
  (- (buf-limit buf) (buf-position buf)))

(defun buf-space (buf)
  "Return number of bytes available for writing: [limit, capacity)."
  (declare (type buf buf))
  (- (buf-capacity buf) (buf-limit buf)))

(defun buf-empty-p (buf)
  "Return T if no bytes available for reading."
  (declare (type buf buf))
  (= (buf-position buf) (buf-limit buf)))

(defun buf-full-p (buf)
  "Return T if no space available for writing."
  (declare (type buf buf))
  (= (buf-limit buf) (buf-capacity buf)))

;;; ============================================================================
;;; Reading from Buffer
;;; ============================================================================

(defun buf-get-byte (buf)
  "Read one byte from buffer, advancing position.
   Signals buffer-underflow-error if empty."
  (declare (type buf buf))
  (when (buf-empty-p buf)
    (error 'cond:buffer-underflow-error :available 0 :requested 1))
  (prog1 (aref (buf-data buf) (buf-position buf))
    (incf (buf-position buf))))

(defun buf-peek-byte (buf &optional (offset 0))
  "Read byte at position+offset without advancing.
   Returns NIL if offset is beyond available data."
  (declare (type buf buf) (type fixnum offset))
  (let ((idx (+ (buf-position buf) offset)))
    (when (< idx (buf-limit buf))
      (aref (buf-data buf) idx))))

(defun buf-get-bytes (buf count)
  "Read COUNT bytes into a new byte vector, advancing position.
   Signals buffer-underflow-error if insufficient data."
  (declare (type buf buf) (type fixnum count))
  (let ((remaining (buf-remaining buf)))
    (when (< remaining count)
      (error 'cond:buffer-underflow-error :available remaining :requested count))
    (let ((result (make-array count :element-type '(unsigned-byte 8))))
      (replace result (buf-data buf) :start2 (buf-position buf))
      (incf (buf-position buf) count)
      result)))

(defun buf-get-into (buf dest &key (start 0) (end (length dest)))
  "Copy bytes from buffer into DEST[start:end], advancing position.
   Returns number of bytes copied (may be less than requested)."
  (declare (type buf buf)
           (type (simple-array (unsigned-byte 8) (*)) dest)
           (type fixnum start end))
  (let* ((requested (- end start))
         (available (buf-remaining buf))
         (count (min requested available)))
    (when (plusp count)
      (replace dest (buf-data buf)
               :start1 start
               :end1 (+ start count)
               :start2 (buf-position buf))
      (incf (buf-position buf) count))
    count))

;;; ============================================================================
;;; Writing to Buffer
;;; ============================================================================

(defun buf-put-byte (buf byte)
  "Write one byte to buffer at limit, advancing limit.
   Signals buffer-overflow-error if full."
  (declare (type buf buf) (type (unsigned-byte 8) byte))
  (when (buf-full-p buf)
    (error 'cond:buffer-overflow-error :capacity (buf-capacity buf) :required 1))
  (setf (aref (buf-data buf) (buf-limit buf)) byte)
  (incf (buf-limit buf))
  buf)

(defun buf-put-bytes (buf bytes &key (start 0) (end (length bytes)))
  "Write BYTES[start:end] to buffer at limit, advancing limit.
   Signals buffer-overflow-error if insufficient space."
  (declare (type buf buf)
           (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start end))
  (let* ((count (- end start))
         (space (buf-space buf)))
    (when (< space count)
      (error 'cond:buffer-overflow-error
             :capacity (buf-capacity buf)
             :required (+ (buf-limit buf) count)))
    (replace (buf-data buf) bytes
             :start1 (buf-limit buf)
             :start2 start
             :end2 end)
    (incf (buf-limit buf) count)
    buf))

(defun buf-put-from (buf src &key (start 0) (end (length src)))
  "Copy bytes from SRC[start:end] into buffer at limit.
   Returns number of bytes copied (may be less than requested if buffer fills)."
  (declare (type buf buf)
           (type (simple-array (unsigned-byte 8) (*)) src)
           (type fixnum start end))
  (let* ((requested (- end start))
         (space (buf-space buf))
         (count (min requested space)))
    (when (plusp count)
      (replace (buf-data buf) src
               :start1 (buf-limit buf)
               :end1 (+ (buf-limit buf) count)
               :start2 start)
      (incf (buf-limit buf) count))
    count))

;;; ============================================================================
;;; Buffer Lifecycle Operations
;;; ============================================================================

(defun buf-clear (buf)
  "Reset buffer to empty state. Position and limit become 0.
   Does NOT zero the data."
  (declare (type buf buf))
  (setf (buf-position buf) 0
        (buf-limit buf) 0
        (buf-mark buf) -1)
  buf)

(defun buf-flip (buf)
  "Prepare buffer for reading after writing.
   Sets limit to current position, then position to 0.
   Use this when transitioning from filling to draining."
  (declare (type buf buf))
  (setf (buf-limit buf) (buf-position buf)
        (buf-position buf) 0
        (buf-mark buf) -1)
  buf)

(defun buf-compact (buf)
  "Move unread data to the start of buffer.
   Use this to make room for more writes while preserving unread data.
   After compact: position=0, limit=remaining, data shifted left."
  (declare (type buf buf))
  (let ((remaining (buf-remaining buf)))
    (when (and (plusp remaining) (plusp (buf-position buf)))
      (replace (buf-data buf) (buf-data buf)
               :start1 0
               :start2 (buf-position buf)
               :end2 (buf-limit buf)))
    (setf (buf-position buf) 0
          (buf-limit buf) remaining
          (buf-mark buf) -1))
  buf)

(defun buf-rewind (buf)
  "Reset position to 0 for re-reading. Limit unchanged."
  (declare (type buf buf))
  (setf (buf-position buf) 0
        (buf-mark buf) -1)
  buf)

;;; ============================================================================
;;; Mark and Reset
;;; ============================================================================

(defun buf-set-mark (buf)
  "Record current position for later reset."
  (declare (type buf buf))
  (setf (buf-mark buf) (buf-position buf))
  buf)

(defun buf-reset-to-mark (buf)
  "Restore position to previously marked position.
   Signals error if mark was not set."
  (declare (type buf buf))
  (let ((m (buf-mark buf)))
    (when (minusp m)
      (error "No mark set on buffer"))
    (setf (buf-position buf) m))
  buf)

;;; ============================================================================
;;; Slicing (Zero-Copy Views)
;;; ============================================================================

(defun buf-slice (buf start end)
  "Create a new buffer sharing the same data array.
   The slice views [start, end) of the current readable region.
   Mutations in either buffer affect the shared data."
  (declare (type buf buf) (type fixnum start end))
  (let ((base (buf-position buf)))
    (%make-buf :data (buf-data buf)
               :position (+ base start)
               :limit (+ base end)
               :capacity (buf-capacity buf))))

(defun buf-slice-remaining (buf)
  "Create a slice of the entire readable region."
  (declare (type buf buf))
  (buf-slice buf 0 (buf-remaining buf)))

;;; ============================================================================
;;; Conversion
;;; ============================================================================

(defun buf-to-bytes (buf)
  "Return a fresh byte vector containing readable data [position, limit)."
  (declare (type buf buf))
  (let ((remaining (buf-remaining buf)))
    (if (zerop remaining)
        (make-array 0 :element-type '(unsigned-byte 8))
        (subseq (buf-data buf) (buf-position buf) (buf-limit buf)))))

(defun buf-to-string (buf &key (encoding :utf-8))
  "Decode readable bytes as a string using ENCODING."
  (declare (type buf buf))
  (let ((bytes (buf-to-bytes buf)))
    (ecase encoding
      (:utf-8 (sb-ext:octets-to-string bytes :external-format :utf-8))
      (:ascii (map 'string #'code-char bytes))
      (:latin-1 (sb-ext:octets-to-string bytes :external-format :latin-1)))))

(defun string-to-buf (string &key (encoding :utf-8))
  "Encode STRING as bytes and wrap in a buffer."
  (let ((bytes (ecase encoding
                 (:utf-8 (sb-ext:string-to-octets string :external-format :utf-8))
                 (:ascii (map '(simple-array (unsigned-byte 8) (*))
                              #'char-code string))
                 (:latin-1 (sb-ext:string-to-octets string :external-format :latin-1)))))
    (buf-from-bytes bytes)))

;;; ============================================================================
;;; Buffer Pool
;;; ============================================================================

(defstruct (buf-pool (:constructor %make-buf-pool))
  "Pool of reusable buffers to reduce allocation pressure.

   Thread-safety: Uses a lock for concurrent access."
  (default-size 8192 :type fixnum :read-only t)
  (max-pooled-size 65536 :type fixnum :read-only t)
  (max-pooled-count 64 :type fixnum :read-only t)
  (free-list nil :type list)
  (allocated 0 :type fixnum)
  (lock (lock:make-lock "buf-pool")))

(defvar *default-buf-pool* nil
  "Default buffer pool for with-buf macro.")

(defun make-buf-pool (&key (default-size 8192) (max-pooled-size 65536)
                        (max-pooled-count 64))
  "Create a new buffer pool."
  (%make-buf-pool :default-size default-size
                  :max-pooled-size max-pooled-size
                  :max-pooled-count max-pooled-count))

(defun buf-pool-acquire (pool &optional size)
  "Acquire a buffer from POOL.
   If SIZE is provided and larger than default, allocates a new buffer.
   Returns a cleared buffer ready for use."
  (declare (type buf-pool pool))
  (let ((size (or size (buf-pool-default-size pool))))
    (if (> size (buf-pool-default-size pool))
        ;; Oversized buffers are not pooled
        (progn
          (lock:with-lock ((buf-pool-lock pool))
            (incf (buf-pool-allocated pool)))
          (make-buf size))
        ;; Try to get from free list
        (lock:with-lock ((buf-pool-lock pool))
          (let ((buf (pop (buf-pool-free-list pool))))
            (if buf
                (buf-clear buf)
                (progn
                  (incf (buf-pool-allocated pool))
                  (make-buf (buf-pool-default-size pool)))))))))

(defun buf-pool-release (pool buf)
  "Return a buffer to the pool.
   Buffers larger than max-pooled-size are discarded (GC'd)."
  (declare (type buf-pool pool) (type buf buf))
  (when (<= (buf-capacity buf) (buf-pool-max-pooled-size pool))
    (lock:with-lock ((buf-pool-lock pool))
      (when (< (length (buf-pool-free-list pool)) (buf-pool-max-pooled-count pool))
        (push buf (buf-pool-free-list pool))))))

(defun buf-pool-stats (pool)
  "Return plist of pool statistics."
  (lock:with-lock ((buf-pool-lock pool))
    (list :allocated (buf-pool-allocated pool)
          :pooled (length (buf-pool-free-list pool))
          :default-size (buf-pool-default-size pool))))

(defmacro with-buf ((var &optional size pool) &body body)
  "Execute BODY with VAR bound to a pooled buffer.
   Buffer is returned to pool on exit (normal or error)."
  (let ((pool-var (gensym "POOL")))
    `(let* ((,pool-var (or ,pool *default-buf-pool*
                           (setf *default-buf-pool* (make-buf-pool))))
            (,var (buf-pool-acquire ,pool-var ,size)))
       (unwind-protect
            (progn ,@body)
         (buf-pool-release ,pool-var ,var)))))

;;; ============================================================================
;;; Buffer as Reader/Writer (Protocol Implementation)
;;; ============================================================================

(defmethod proto:read-into ((buf buf) dest &key (start 0) (end (length dest)))
  "Read from buffer into destination array."
  (buf-get-into buf dest :start start :end end))

(defmethod proto:write-from ((buf buf) src &key (start 0) (end (length src)))
  "Write from source array into buffer."
  (buf-put-from buf src :start start :end end))

(defmethod proto:position* ((buf buf))
  (buf-position buf))

(defmethod proto:size* ((buf buf))
  (buf-limit buf))
