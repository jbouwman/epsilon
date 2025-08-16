;;;; Clean async I/O operations using platform-provided epsilon.async
;;;;
;;;; This module provides high-level async I/O operations that use the 
;;;; platform-specific epsilon.async implementations.

(in-package :epsilon.io)

;;; ============================================================================
;;; I/O Context for Managing Async Operations
;;; ============================================================================

(defstruct io-context
  "High-level I/O context for managing async operations"
  (running-p t :type boolean)
  (operations '() :type list)
  (completion-handler nil :type (or null function)))

(defun create-io-context (&key completion-handler)
  "Create a new I/O context"
  (epsilon.async:ensure-async-system)
  (make-io-context :completion-handler completion-handler))

(defun close-io-context (context)
  "Close an I/O context and clean up resources"
  (setf (io-context-running-p context) nil)
  (setf (io-context-operations context) '()))

;;; ============================================================================
;;; High-Level Async Operations
;;; ============================================================================

(defun async-read (fd buffer &key context on-complete on-error)
  "Perform async read operation using platform epsilon.async"
  (let ((operation (epsilon.async:async-read 
                    fd buffer
                    :on-complete on-complete
                    :on-error on-error)))
    (when context
      (push operation (io-context-operations context)))
    operation))

(defun async-write (fd buffer &key context on-complete on-error)
  "Perform async write operation using platform epsilon.async"
  (let ((operation (epsilon.async:async-write 
                    fd buffer
                    :on-complete on-complete
                    :on-error on-error)))
    (when context
      (push operation (io-context-operations context)))
    operation))

(defun async-accept (listener-fd &key context on-complete on-error)
  "Perform async accept operation using platform epsilon.async"
  (let ((operation (epsilon.async:async-accept 
                    listener-fd
                    :on-complete on-complete
                    :on-error on-error)))
    (when context
      (push operation (io-context-operations context)))
    operation))

(defun async-connect (fd address &key context on-complete on-error)
  "Perform async connect operation using platform epsilon.async"
  (let ((operation (epsilon.async:async-connect 
                    fd address
                    :on-complete on-complete
                    :on-error on-error)))
    (when context
      (push operation (io-context-operations context)))
    operation))

;;; ============================================================================
;;; Completion Processing
;;; ============================================================================

(defun process-completions (&optional (timeout-ms 100))
  "Process completed async operations"
  (let ((completions (epsilon.async:poll-completions timeout-ms)))
    (dolist (completion completions)
      (when (epsilon.async:async-operation-callback completion)
        (handler-case
            (funcall (epsilon.async:async-operation-callback completion) completion)
          (error (e)
            (when (epsilon.async:async-operation-error-callback completion)
              (funcall (epsilon.async:async-operation-error-callback completion) e))))))
    (length completions)))

(defun run-io-loop (context &key (timeout-ms 100) (max-iterations nil))
  "Run the I/O event loop for a context"
  (let ((iterations 0))
    (loop while (and (io-context-running-p context)
                     (or (null max-iterations) (< iterations max-iterations)))
          do (progn
               (process-completions timeout-ms)
               (when max-iterations
                 (incf iterations))
               ;; Small sleep to prevent busy waiting
               (sleep 0.001)))))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-io-context ((context-var &key completion-handler) &body body)
  "Execute body with an I/O context"
  `(let ((,context-var (create-io-context :completion-handler ,completion-handler)))
     (unwind-protect
          (progn ,@body)
       (close-io-context ,context-var))))

(defmacro with-async-operation ((operation-var async-form &key timeout) &body body)
  "Execute async operation and wait for completion"
  (let ((completed-var (gensym "COMPLETED"))
        (result-var (gensym "RESULT"))
        (error-var (gensym "ERROR")))
    `(let ((,completed-var nil)
           (,result-var nil)
           (,error-var nil))
       (let ((,operation-var ,async-form))
         ;; Set up completion callbacks if not already provided
         (unless (epsilon.async:async-operation-callback ,operation-var)
           (setf (epsilon.async:async-operation-callback ,operation-var)
                 (lambda (result)
                   (setf ,result-var result)
                   (setf ,completed-var t))))
         (unless (epsilon.async:async-operation-error-callback ,operation-var)
           (setf (epsilon.async:async-operation-error-callback ,operation-var)
                 (lambda (error)
                   (setf ,error-var error)
                   (setf ,completed-var t))))
         
         ;; Wait for completion with adaptive polling
         (let ((start-time (get-internal-real-time))
               (timeout-internal ,(if timeout
                                    `(* ,timeout internal-time-units-per-second)
                                    `(* 30 internal-time-units-per-second))) ; 30 second default
               (poll-interval 0.001))
           (loop until (or ,completed-var
                          (> (- (get-internal-real-time) start-time) timeout-internal))
                 do (progn
                      (process-completions 10)
                      (sleep poll-interval)
                      ;; Gradually increase poll interval to reduce CPU usage
                      (when (< poll-interval 0.1)
                        (setf poll-interval (min 0.1 (* poll-interval 1.5))))))
           
           (cond
             (,error-var (error ,error-var))
             (,completed-var ,@body)
             (t (error "Async operation timed out"))))))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set file descriptor to non-blocking mode"
  (epsilon.async:set-nonblocking fd))