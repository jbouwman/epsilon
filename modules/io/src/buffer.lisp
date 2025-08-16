;;;; Simple buffer management for epsilon.io

(in-package :epsilon.io)

;;;; Basic Buffer Implementation

(defun allocate-buffer (size)
  "Allocate a buffer of given size"
  (let ((data (make-array size :element-type '(unsigned-byte 8))))
    (make-buffer :data data :capacity size :position 0 :limit size :mark -1 
                 :owner nil :pinned-p nil :address 0)))

(defun free-buffer (buffer)
  "Free buffer resources"
  (when (buffer-data buffer)
    (setf (buffer-data buffer) nil))
  nil)

(defun buffer-remaining (buffer)
  "Get remaining bytes in buffer"
  (- (buffer-limit buffer) (buffer-position buffer)))

(defun buffer-to-string (buffer &key (encoding :utf-8))
  "Convert buffer to string"
  (declare (ignore encoding))
  (when (buffer-data buffer)
    (map 'string #'code-char (buffer-data buffer))))

(defun string-to-buffer (string)
  "Convert string to buffer"
  (let* ((bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))
         (buffer (allocate-buffer (length bytes))))
    (setf (buffer-data buffer) bytes)
    (setf (buffer-position buffer) 0)
    (setf (buffer-limit buffer) (length bytes))
    buffer))

;;;; Buffer Pool for Efficiency

(defvar *default-buffer-pool* nil
  "Default buffer pool for memory efficiency")

(defstruct buffer-pool
  "Memory pool for buffer reuse"
  (size 8192 :type fixnum)
  (count 100 :type fixnum)
  (available '() :type list)
  (allocated 0 :type fixnum))

(defun create-buffer-pool (&key (size 8192) (count 100))
  "Create buffer pool"
  (make-buffer-pool :size size :count count))

(defun acquire-buffer (pool)
  "Acquire buffer from pool"
  (if (buffer-pool-available pool)
      (pop (buffer-pool-available pool))
      (progn
        (incf (buffer-pool-allocated pool))
        (allocate-buffer (buffer-pool-size pool)))))

(defun release-buffer (pool buffer)
  "Release buffer back to pool"
  (when (< (length (buffer-pool-available pool)) (buffer-pool-count pool))
    ;; Reset buffer state
    (setf (buffer-position buffer) 0)
    (setf (buffer-limit buffer) (buffer-capacity buffer))
    ;; Reset buffer data if needed
    (push buffer (buffer-pool-available pool))))

(defmacro with-pooled-buffer ((buffer pool) &body body)
  "Execute body with pooled buffer"
  `(let ((,buffer (acquire-buffer ,pool)))
     (unwind-protect
          (progn ,@body)
       (release-buffer ,pool ,buffer))))