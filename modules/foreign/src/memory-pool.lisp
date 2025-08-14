;;;; memory-pool.lisp - Memory pooling for efficient FFI allocations
;;;;
;;;; This module provides memory pools to reduce allocation overhead
;;;; for frequently allocated foreign memory blocks.

(defpackage epsilon.foreign.memory-pool
  (:use cl)
  (:export
   #:create-pool
   #:pool-allocate
   #:pool-free
   #:with-pooled-memory
   #:with-smart-pooled-memory
   #:pool-statistics
   #:pool-reset
   #:pool-destroy
   #:*default-pool*
   #:*pool-statistics*))

(in-package epsilon.foreign.memory-pool)

;;; Pool structure

(defstruct memory-pool
  "A pool of reusable memory blocks"
  (name "unnamed" :type string)
  (block-size 1024 :type fixnum)
  (max-blocks 100 :type fixnum)
  (free-blocks nil :type list)
  (used-blocks nil :type list)
  (total-allocated 0 :type fixnum)
  (peak-usage 0 :type fixnum)
  (allocation-count 0 :type fixnum)
  (reuse-count 0 :type fixnum)
  (lock (sb-thread:make-mutex :name "pool-lock")))

;;; Global pools

(defvar *default-pool* nil
  "Default memory pool for general use")

(defvar *pool-statistics* t
  "Whether to collect pool statistics")

(defvar *all-pools* '()
  "List of all active memory pools")

;;; Pool management

(defun create-pool (&key (name "pool") (block-size 1024) (max-blocks 100))
  "Create a new memory pool"
  (let ((pool (make-memory-pool :name name
                                :block-size block-size
                                :max-blocks max-blocks)))
    (push pool *all-pools*)
    pool))

(defun ensure-default-pool ()
  "Ensure default pool exists"
  (unless *default-pool*
    (setf *default-pool* (create-pool :name "default"
                                      :block-size 4096
                                      :max-blocks 256))))

(defun allocate-new-block (pool)
  "Allocate a new memory block for the pool"
  (let ((ptr (sb-alien:make-alien sb-alien:char (memory-pool-block-size pool))))
    (when *pool-statistics*
      (incf (memory-pool-total-allocated pool)))
    ptr))

(defun pool-allocate (size &optional (pool *default-pool*))
  "Allocate memory from pool"
  (ensure-default-pool)
  (unless pool (setf pool *default-pool*))
  
  (when (> size (memory-pool-block-size pool))
    (error "Requested size ~D exceeds pool block size ~D" 
           size (memory-pool-block-size pool)))
  
  (sb-thread:with-mutex ((memory-pool-lock pool))
    (let ((ptr (or (pop (memory-pool-free-blocks pool))
                   (when (< (memory-pool-total-allocated pool)
                           (memory-pool-max-blocks pool))
                     (allocate-new-block pool)))))
      (when ptr
        (push ptr (memory-pool-used-blocks pool))
        (when *pool-statistics*
          (if (memory-pool-free-blocks pool)
              (incf (memory-pool-reuse-count pool))
              (incf (memory-pool-allocation-count pool)))
          (setf (memory-pool-peak-usage pool)
                (max (memory-pool-peak-usage pool)
                     (length (memory-pool-used-blocks pool)))))
        ;; Clear the memory
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "memset"
                               (sb-alien:function sb-alien:void
                                                 (* sb-alien:char)
                                                 sb-alien:int
                                                 sb-alien:unsigned-long))
         ptr 0 size))
      ptr)))

(defun pool-free (ptr &optional (pool *default-pool*))
  "Return memory to pool"
  (ensure-default-pool)
  (unless pool (setf pool *default-pool*))
  
  (sb-thread:with-mutex ((memory-pool-lock pool))
    (setf (memory-pool-used-blocks pool)
          (delete ptr (memory-pool-used-blocks pool) :test #'sb-sys:sap=))
    (push ptr (memory-pool-free-blocks pool))))

(defmacro with-pooled-memory ((var size &key (pool '*default-pool*)) &body body)
  "Allocate memory from pool and ensure it's returned"
  `(let ((,var (pool-allocate ,size ,pool)))
     (unwind-protect
          (progn ,@body)
       (pool-free ,var ,pool))))

;;; Pool statistics and management

(defun pool-statistics (pool)
  "Get statistics for a memory pool"
  (sb-thread:with-mutex ((memory-pool-lock pool))
    (list :name (memory-pool-name pool)
          :block-size (memory-pool-block-size pool)
          :total-blocks (memory-pool-total-allocated pool)
          :free-blocks (length (memory-pool-free-blocks pool))
          :used-blocks (length (memory-pool-used-blocks pool))
          :peak-usage (memory-pool-peak-usage pool)
          :allocations (memory-pool-allocation-count pool)
          :reuses (memory-pool-reuse-count pool)
          :reuse-ratio (if (zerop (memory-pool-allocation-count pool))
                          0.0
                          (/ (memory-pool-reuse-count pool)
                             (float (+ (memory-pool-allocation-count pool)
                                      (memory-pool-reuse-count pool))))))))

(defun pool-reset (pool)
  "Reset pool, moving all used blocks to free list"
  (sb-thread:with-mutex ((memory-pool-lock pool))
    (setf (memory-pool-free-blocks pool)
          (append (memory-pool-used-blocks pool)
                  (memory-pool-free-blocks pool)))
    (setf (memory-pool-used-blocks pool) nil)))

(defun pool-destroy (pool)
  "Destroy a pool and free all its memory"
  (sb-thread:with-mutex ((memory-pool-lock pool))
    ;; Free all blocks
    (dolist (ptr (memory-pool-free-blocks pool))
      (sb-alien:free-alien ptr))
    (dolist (ptr (memory-pool-used-blocks pool))
      (sb-alien:free-alien ptr))
    ;; Clear lists
    (setf (memory-pool-free-blocks pool) nil)
    (setf (memory-pool-used-blocks pool) nil)
    (setf (memory-pool-total-allocated pool) 0))
  ;; Remove from global list
  (setf *all-pools* (delete pool *all-pools*))
  (when (eq pool *default-pool*)
    (setf *default-pool* nil)))

(defun all-pools-statistics ()
  "Get statistics for all active pools"
  (mapcar #'pool-statistics *all-pools*))

;;; Specialized pools for common sizes

(defvar *small-pool* nil "Pool for small allocations (256 bytes)")
(defvar *medium-pool* nil "Pool for medium allocations (4KB)")
(defvar *large-pool* nil "Pool for large allocations (64KB)")

(defun ensure-specialized-pools ()
  "Ensure specialized pools exist"
  (unless *small-pool*
    (setf *small-pool* (create-pool :name "small"
                                    :block-size 256
                                    :max-blocks 1000)))
  (unless *medium-pool*
    (setf *medium-pool* (create-pool :name "medium"
                                     :block-size 4096
                                     :max-blocks 256)))
  (unless *large-pool*
    (setf *large-pool* (create-pool :name "large"
                                    :block-size 65536
                                    :max-blocks 16))))

(defun smart-pool-allocate (size)
  "Allocate from the most appropriate pool based on size"
  (ensure-specialized-pools)
  (cond ((<= size 256) (pool-allocate size *small-pool*))
        ((<= size 4096) (pool-allocate size *medium-pool*))
        ((<= size 65536) (pool-allocate size *large-pool*))
        (t (error "Size ~D too large for pooled allocation" size))))

(defmacro with-smart-pooled-memory ((var size) &body body)
  "Allocate memory from the most appropriate pool"
  (let ((pool-var (gensym "POOL")))
    `(let* ((,pool-var (cond ((<= ,size 256) (progn (ensure-specialized-pools) *small-pool*))
                             ((<= ,size 4096) (progn (ensure-specialized-pools) *medium-pool*))
                             ((<= ,size 65536) (progn (ensure-specialized-pools) *large-pool*))
                             (t (error "Size too large for pooled allocation"))))
            (,var (pool-allocate ,size ,pool-var)))
       (unwind-protect
            (progn ,@body)
         (pool-free ,var ,pool-var)))))

;;; Performance monitoring

(defun pool-efficiency-report ()
  "Generate efficiency report for all pools"
  (format t "~%Memory Pool Efficiency Report~%")
  (format t "==============================~%")
  (dolist (pool *all-pools*)
    (let ((stats (pool-statistics pool)))
      (format t "~%Pool: ~A~%" (getf stats :name))
      (format t "  Block size:    ~D bytes~%" (getf stats :block-size))
      (format t "  Total blocks:  ~D~%" (getf stats :total-blocks))
      (format t "  In use:        ~D~%" (getf stats :used-blocks))
      (format t "  Available:     ~D~%" (getf stats :free-blocks))
      (format t "  Peak usage:    ~D~%" (getf stats :peak-usage))
      (format t "  Reuse ratio:   ~,1f%~%" (* 100 (getf stats :reuse-ratio))))))

;;; Initialize default pool on load
(eval-when (:load-toplevel :execute)
  (ensure-default-pool))