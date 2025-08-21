;;;; Error Handling Utilities
;;;;
;;;; This module provides standardized error handling patterns and utilities
;;;; for consistent error management across all modules.

(defpackage epsilon.error-utils
  (:use cl)
  (:local-nicknames
   (condition epsilon.condition)
   (log epsilon.log))
  (:export
   ;; Safe operation wrappers
   #:safe-operation
   #:safe-file-operation
   #:safe-network-operation
   #:safe-parse-operation
   
   ;; Error classification
   #:compilation-error
   #:file-operation-error
   #:network-error
   #:parse-error-with-context
   
   ;; Error handling macros
   #:with-error-context
   #:with-cleanup-on-error
   #:ignore-file-errors
   #:log-and-continue
   
   ;; Error reporting utilities
   #:format-error-context
   #:collect-error-info
   #:make-error-report))

(in-package epsilon.error-utils)

;;; Error condition definitions

(define-condition compilation-error (error)
  ((message :initarg :message :reader compilation-error-message)
   (file :initarg :file :reader compilation-error-file)
   (line :initarg :line :reader compilation-error-line)
   (context :initarg :context :reader compilation-error-context))
  (:report (lambda (condition stream)
             (format stream "Compilation error~@[ in ~A~]~@[:~D~]: ~A"
                     (compilation-error-file condition)
                     (compilation-error-line condition)
                     (compilation-error-message condition)))))

(define-condition file-operation-error (error)
  ((pathname :initarg :pathname :reader file-operation-error-pathname)
   (operation :initarg :operation :reader file-operation-error-operation)
   (underlying-error :initarg :underlying-error :reader file-operation-error-underlying-error))
  (:report (lambda (condition stream)
             (format stream "File operation ~A failed for ~A: ~A"
                     (file-operation-error-operation condition)
                     (file-operation-error-pathname condition)
                     (file-operation-error-underlying-error condition)))))

(define-condition network-error (error)
  ((host :initarg :host :reader network-error-host)
   (port :initarg :port :reader network-error-port)
   (operation :initarg :operation :reader network-error-operation)
   (underlying-error :initarg :underlying-error :reader network-error-underlying-error))
  (:report (lambda (condition stream)
             (format stream "Network ~A failed for ~@[~A~]~@[:~D~]: ~A"
                     (network-error-operation condition)
                     (network-error-host condition)
                     (network-error-port condition)
                     (network-error-underlying-error condition)))))

(define-condition parse-error-with-context (parse-error)
  ((input :initarg :input :reader parse-error-input)
   (position :initarg :position :reader parse-error-position)
   (context :initarg :context :reader parse-error-context))
  (:report (lambda (condition stream)
             (format stream "Parse error~@[ at position ~D~]~@[ in ~A~]: ~A"
                     (parse-error-position condition)
                     (parse-error-context condition)
                     (parse-error-input condition)))))

;;; Safe operation wrappers

(defmacro safe-operation ((success-var &optional (error-var (gensym "ERROR"))) 
                          &body body)
  "Execute body safely, binding SUCCESS-VAR to result and ERROR-VAR to any error."
  `(handler-case
       (values (progn ,@body) t nil)
     (error (,error-var)
       (values nil nil ,error-var))))

(defmacro safe-file-operation ((pathname operation &optional (default nil)) 
                               &body body)
  "Execute file operation safely with standardized error handling."
  `(handler-case
       (progn ,@body)
     (error (e)
       (signal 'file-operation-error
               :pathname ,pathname
               :operation ,operation
               :underlying-error e)
       ,default)))

(defmacro safe-network-operation ((host port operation &optional (default nil))
                                  &body body)
  "Execute network operation safely with standardized error handling."
  `(handler-case
       (progn ,@body)
     (error (e)
       (signal 'network-error
               :host ,host
               :port ,port
               :operation ,operation
               :underlying-error e)
       ,default)))

(defmacro safe-parse-operation ((input context &optional (default nil))
                                &body body)
  "Execute parsing operation safely with context information."
  `(handler-case
       (progn ,@body)
     (parse-error (e)
       (error 'parse-error-with-context
              :input ,input
              :context ,context
              :position nil))
     (error (e)
       (error 'parse-error-with-context
              :input ,input
              :context ,context
              :position nil))
     (:no-error (result)
       result)))

;;; Error handling macros

(defmacro with-error-context ((context-info) &body body)
  "Execute body with error context information for better debugging."
  (let ((context-var (gensym "CONTEXT")))
    `(let ((,context-var ,context-info))
       (handler-bind ((error (lambda (e)
                               (log:debug "Error in context ~A: ~A" ,context-var e))))
         ,@body))))

(defmacro with-cleanup-on-error (cleanup-forms &body body)
  "Execute body, running cleanup forms if an error occurs."
  `(let ((success nil))
     (unwind-protect
          (multiple-value-prog1
              (progn ,@body)
            (setf success t))
       (unless success
         ,@cleanup-forms))))

(defmacro ignore-file-errors (&body body)
  "Execute body, ignoring common file operation errors."
  `(condition:ignore-some-conditions (file-error sb-posix:syscall-error)
     ,@body))

(defmacro log-and-continue ((log-level &optional (message "Error occurred")) 
                            &body body)
  "Execute body, logging errors and continuing execution."
  `(handler-case
       (progn ,@body)
     (error (e)
       (log:log ,log-level "~A: ~A" ,message e)
       nil)))

;;; Error reporting utilities

(defun format-error-context (error &optional (stream *standard-output*))
  "Format error context information for display."
  (typecase error
    (compilation-error
     (format stream "~&Compilation Error:~%")
     (format stream "  Message: ~A~%" (compilation-error-message error))
     (when (compilation-error-file error)
       (format stream "  File: ~A~%" (compilation-error-file error)))
     (when (compilation-error-line error)
       (format stream "  Line: ~A~%" (compilation-error-line error)))
     (when (compilation-error-context error)
       (format stream "  Context: ~A~%" (compilation-error-context error))))
    
    (file-operation-error
     (format stream "~&File Operation Error:~%")
     (format stream "  Operation: ~A~%" (file-operation-error-operation error))
     (format stream "  Pathname: ~A~%" (file-operation-error-pathname error))
     (format stream "  Cause: ~A~%" (file-operation-error-underlying-error error)))
    
    (network-error
     (format stream "~&Network Error:~%")
     (format stream "  Operation: ~A~%" (network-error-operation error))
     (when (network-error-host error)
       (format stream "  Host: ~A~%" (network-error-host error)))
     (when (network-error-port error)
       (format stream "  Port: ~A~%" (network-error-port error)))
     (format stream "  Cause: ~A~%" (network-error-underlying-error error)))
    
    (parse-error-with-context
     (format stream "~&Parse Error:~%")
     (format stream "  Input: ~A~%" (parse-error-input error))
     (when (parse-error-position error)
       (format stream "  Position: ~A~%" (parse-error-position error)))
     (when (parse-error-context error)
       (format stream "  Context: ~A~%" (parse-error-context error))))
    
    (t
     (format stream "~&Error: ~A~%" error))))

(defun collect-error-info (error)
  "Collect error information into a structured format."
  (let ((info (list :type (type-of error)
                    :message (princ-to-string error))))
    (typecase error
      (compilation-error
       (append info
               (list :file (compilation-error-file error)
                     :line (compilation-error-line error)
                     :context (compilation-error-context error))))
      
      (file-operation-error
       (append info
               (list :pathname (file-operation-error-pathname error)
                     :operation (file-operation-error-operation error)
                     :underlying-error (file-operation-error-underlying-error error))))
      
      (network-error
       (append info
               (list :host (network-error-host error)
                     :port (network-error-port error)
                     :operation (network-error-operation error)
                     :underlying-error (network-error-underlying-error error))))
      
      (parse-error-with-context
       (append info
               (list :input (parse-error-input error)
                     :position (parse-error-position error)
                     :context (parse-error-context error))))
      
      (t info))))

(defun make-error-report (errors &optional (title "Error Report"))
  "Create a formatted error report from a list of errors."
  (with-output-to-string (stream)
    (format stream "~&~A~%" title)
    (format stream "~A~%" (make-string (length title) :initial-element #\=))
    (format stream "~&~D error~:P found:~%~%" (length errors))
    (loop for error in errors
          for i from 1
          do (progn
               (format stream "~&~D. " i)
               (format-error-context error stream)
               (format stream "~%")))))

;;; Integration with existing modules

(defun enhance-file-operations ()
  "Enhance file operations to use standardized error handling."
  ;; This could be used to wrap existing file utilities
  nil)

(defun enhance-network-operations ()
  "Enhance network operations to use standardized error handling."
  ;; This could be used to wrap existing network utilities
  nil)