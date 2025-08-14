;;;; batch.lisp - Batch operations optimization for FFI calls
;;;;
;;;; This module provides optimizations for batching multiple FFI calls
;;;; to reduce overhead and improve cache locality.

(defpackage epsilon.foreign.batch
  (:use cl)
  (:local-nicknames
   (pool epsilon.foreign.memory-pool)
   (ffi epsilon.foreign))
  (:export
   #:with-foreign-batch
   #:batch-call
   #:batch-allocate
   #:batch-free
   #:define-batch-operation
   #:*batch-threshold*
   #:*current-batch*
   #:execute-batch
   #:optimize-call-sequence))

(in-package epsilon.foreign.batch)

;;; Batch context

(defstruct batch-context
  "Context for batched FFI operations"
  (calls '() :type list)
  (allocations '() :type list)
  (deallocations '() :type list)
  (pool nil)
  (deferred-results (make-hash-table))
  (optimization-level 1 :type fixnum))

(defvar *current-batch* nil
  "Current batch context")

(defvar *batch-threshold* 5
  "Minimum number of operations to trigger batch optimization")

;;; Batch operations

(defmacro with-foreign-batch ((&key (optimize t) (pool nil)) &body body)
  "Execute body with batched FFI operations"
  `(let ((*current-batch* (make-batch-context :pool ,pool
                                              :optimization-level (if ,optimize 2 1))))
     (unwind-protect
          (progn
            ,@body
            (execute-batch *current-batch*))
       (cleanup-batch *current-batch*))))

(defun batch-call (name-lib return-type arg-types &rest args)
  "Add a call to the current batch"
  (if *current-batch*
      (let ((call-id (gensym "CALL")))
        (push (list call-id name-lib return-type arg-types args)
              (batch-context-calls *current-batch*))
        ;; Return a promise for the result
        (lambda ()
          (gethash call-id (batch-context-deferred-results *current-batch*))))
      ;; No batch context, execute immediately
      (apply #'ffi:shared-call name-lib return-type arg-types args)))

(defun batch-allocate (type &optional (count 1))
  "Batch memory allocation"
  (if *current-batch*
      (let ((ptr (if (batch-context-pool *current-batch*)
                     (pool:pool-allocate (* count (foreign-type-size type))
                                        (batch-context-pool *current-batch*))
                     (ffi:foreign-alloc type :count count))))
        (push (list ptr type count) (batch-context-allocations *current-batch*))
        ptr)
      (ffi:foreign-alloc type :count count)))

(defun batch-free (ptr)
  "Batch memory deallocation"
  (if *current-batch*
      (push ptr (batch-context-deallocations *current-batch*))
      (ffi:foreign-free ptr)))

;;; Batch execution

(defun execute-batch (batch)
  "Execute all operations in a batch"
  (when batch
    (let ((calls (nreverse (batch-context-calls batch))))
      ;; Optimize call sequence if threshold met
      (when (>= (length calls) *batch-threshold*)
        (setf calls (optimize-call-sequence calls)))
      
      ;; Execute all calls
      (dolist (call calls)
        (destructuring-bind (id name-lib return-type arg-types args) call
          (let ((result (apply #'ffi:shared-call 
                              name-lib return-type arg-types args)))
            (setf (gethash id (batch-context-deferred-results batch)) result)))))))

(defun cleanup-batch (batch)
  "Clean up batch resources"
  (when batch
    ;; Free all deferred deallocations
    (dolist (ptr (batch-context-deallocations batch))
      (ffi:foreign-free ptr))
    ;; Clear batch data
    (setf (batch-context-calls batch) nil)
    (setf (batch-context-allocations batch) nil)
    (setf (batch-context-deallocations batch) nil)
    (clrhash (batch-context-deferred-results batch))))

;;; Call sequence optimization

(defun optimize-call-sequence (calls)
  "Optimize sequence of FFI calls for better performance"
  ;; Group calls by library to improve locality
  (let ((grouped (make-hash-table :test 'equal)))
    (dolist (call calls)
      (let ((lib (second (second call))))  ; Extract library from name-lib
        (push call (gethash lib grouped '()))))
    
    ;; Flatten grouped calls, keeping same-library calls together
    (let ((optimized '()))
      (maphash (lambda (lib calls)
                 (declare (ignore lib))
                 (setf optimized (append optimized (nreverse calls))))
               grouped)
      optimized)))

(defun foreign-type-size (type)
  "Get size of foreign type in bytes"
  (case type
    ((:char :unsigned-char) 1)
    ((:short :unsigned-short) 2)
    ((:int :unsigned-int :float) 4)
    ((:long :unsigned-long :pointer :double) 8)
    (t 8)))  ; Default to pointer size

;;; Batch operation patterns

(defmacro define-batch-operation (name params &body body)
  "Define a reusable batch operation pattern"
  `(defun ,name ,params
     (with-foreign-batch ()
       ,@body)))

;;; Common batch patterns

(define-batch-operation batch-string-operations (strings operation)
  "Batch multiple string operations"
  (mapcar (lambda (str)
           (ecase operation
             (:length (batch-call '("strlen" "libc") :unsigned-long '(:string) str))
             (:uppercase (batch-call '("toupper" "libc") :int '(:int) 
                                    (char-code (char str 0))))
             (:lowercase (batch-call '("tolower" "libc") :int '(:int)
                                    (char-code (char str 0))))))
         strings))

(define-batch-operation batch-memory-copy (operations)
  "Batch multiple memory copy operations"
  (dolist (op operations)
    (destructuring-bind (dest src size) op
      (batch-call '("memcpy" "libc") :pointer '(:pointer :pointer :unsigned-long)
                  dest src size))))

(define-batch-operation batch-file-operations (files operation)
  "Batch file operations"
  (mapcar (lambda (file)
           (ecase operation
             (:exists (batch-call '("access" "libc") :int '(:string :int) file 0))
             (:size (let ((stat-buf (batch-allocate :char 144))) ; sizeof(struct stat)
                     (batch-call '("stat" "libc") :int '(:string :pointer) 
                                file stat-buf)))
             (:delete (batch-call '("unlink" "libc") :int '(:string) file))))
         files))

;;; Performance monitoring

(defvar *batch-statistics* (make-hash-table :test 'equal)
  "Statistics for batch operations")

(defun record-batch-performance (batch-size execution-time)
  "Record performance statistics for batches"
  (push (list :size batch-size :time execution-time)
        (gethash batch-size *batch-statistics* '())))

(defun batch-performance-report ()
  "Generate performance report for batch operations"
  (format t "~%Batch Operations Performance Report~%")
  (format t "===================================~%")
  (maphash (lambda (size stats)
            (let ((times (mapcar (lambda (s) (getf s :time)) stats)))
              (format t "~%Batch size ~D:~%" size)
              (format t "  Executions:  ~D~%" (length times))
              (format t "  Avg time:    ~,3f ms~%" 
                     (/ (reduce #'+ times) (length times)))
              (format t "  Min time:    ~,3f ms~%" (apply #'min times))
              (format t "  Max time:    ~,3f ms~%" (apply #'max times))))
          *batch-statistics*))

;;; Compiler optimizations

(defmacro with-batched-calls ((&key (threshold 3)) &body body)
  "Automatically batch FFI calls in body if count exceeds threshold"
  `(let ((call-count (count-ffi-calls ',body)))
     (if (>= call-count ,threshold)
         (with-foreign-batch ()
           ,@(transform-to-batch-calls body))
         (progn ,@body))))

(defun count-ffi-calls (forms)
  "Count FFI calls in a list of forms"
  (let ((count 0))
    (labels ((walk (form)
              (when (listp form)
                (when (member (car form) '(shared-call foreign-call defshared))
                  (incf count))
                (mapc #'walk (cdr form)))))
      (mapc #'walk forms))
    count))

(defun transform-to-batch-calls (forms)
  "Transform regular FFI calls to batch calls"
  (mapcar (lambda (form)
           (if (and (listp form)
                    (or (eq (car form) 'shared-call)
                        (and (symbolp (car form))
                             (string= (symbol-name (car form)) "SHARED-CALL"))))
               `(batch-call ,@(cdr form))
               form))
         forms))