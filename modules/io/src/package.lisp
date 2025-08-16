;;;; Core package definition for epsilon.io

(defpackage :epsilon.io
  (:use :cl)
  (:export
   ;; Core buffer management
   #:buffer
   #:allocate-buffer
   #:free-buffer
   #:buffer-remaining
   #:buffer-to-string
   #:string-to-buffer
   
   ;; Buffer pool
   #:buffer-pool
   #:create-buffer-pool
   #:acquire-buffer
   #:release-buffer
   #:with-pooled-buffer
   
   ;; Enhanced async I/O using platform epsilon.async
   #:io-context
   #:create-io-context
   #:close-io-context
   #:async-read
   #:async-write
   #:async-accept
   #:async-connect
   
   ;; Completion processing
   #:process-completions
   #:run-io-loop
   
   ;; Convenience macros
   #:with-io-context
   #:with-async-operation
   
   ;; Utilities
   #:set-nonblocking
   
   ;; Pipelines
   #:pipeline
   #:make-pipeline
   #:string-source
   #:file-source
   
   ;; Conditions
   #:io-error))

(in-package :epsilon.io)

;;;; Core Data Types

(defstruct buffer
  "Memory buffer with ownership tracking"
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (capacity 0 :type fixnum :read-only t)
  (position 0 :type fixnum)
  (limit 0 :type fixnum)
  (mark -1 :type fixnum)
  (owner nil)
  (pinned-p nil :type boolean)
  (address 0 :type fixnum :read-only t))


(defstruct pipeline
  "Stream processing pipeline"
  source
  (transforms '() :type list)
  sink)

(defstruct string-source
  "String data source"
  (string "" :type string))

(defstruct file-source
  "File data source"
  (path nil))

;;;; Error Conditions

(define-condition io-error (error)
  ((operation :initarg :operation :reader io-error-operation)
   (message :initarg :message :reader io-error-message))
  (:report (lambda (condition stream)
             (format stream "I/O error during ~A: ~A"
                     (io-error-operation condition)
                     (io-error-message condition)))))