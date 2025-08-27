;;;; Backend Protocol for epsilon.foreign
;;;;
;;;; Defines the generic interface that all FFI backends must implement

(defpackage epsilon.foreign.backend.protocol
  (:use cl)
  (:export
   ;; Backend class and protocol
   #:backend
   #:backend-name
   #:backend-capabilities
   #:backend-priority
   #:backend-call
   #:backend-alloc
   #:backend-free
   
   ;; Capability checking
   #:has-capability-p
   #:supports-signature-p
   #:estimated-call-overhead
   #:supports-caching-p
   #:supports-jit-p
   
   ;; Registry and management
   #:register-backend
   #:unregister-backend
   #:find-backend
   #:list-backends
   #:select-backend-for-signature
   
   ;; Dynamic backend switching
   #:*default-backend*
   #:*current-backend*
   #:with-backend
   #:with-fallback-backend
   
   ;; Error conditions
   #:backend-error
   #:backend-already-registered
   #:no-suitable-backend
   #:backend-call-error
   #:allocation-error
   #:double-free-error
   
   ;; Test support
   #:test-backend
   #:mock-backend
   #:failing-backend
   #:thread-safe-backend
   #:trampoline-backend
   #:libffi-backend))

(in-package epsilon.foreign.backend.protocol)

;;;; Error Conditions

(define-condition backend-error (error)
  ((backend :initarg :backend :reader error-backend)
   (operation :initarg :operation :reader error-operation))
  (:documentation "Base condition for backend errors"))

(define-condition backend-already-registered (backend-error)
  ((name :initarg :name :reader error-backend-name))
  (:report (lambda (c s)
             (format s "Backend ~A is already registered" 
                     (error-backend-name c)))))

(define-condition no-suitable-backend (backend-error)
  ((signature :initarg :signature :reader error-signature))
  (:report (lambda (c s)
             (format s "No suitable backend found for signature ~S"
                     (error-signature c)))))

(define-condition backend-call-error (backend-error)
  ((details :initarg :details :reader error-details))
  (:report (lambda (c s)
             (format s "Backend call failed: ~A" (error-details c)))))

(define-condition allocation-error (backend-error)
  ((size :initarg :size :reader error-size))
  (:report (lambda (c s)
             (format s "Failed to allocate ~D bytes" (error-size c)))))

(define-condition double-free-error (backend-error)
  ((pointer :initarg :pointer :reader error-pointer))
  (:report (lambda (c s)
             (format s "Double free detected for pointer ~X" 
                     (sb-sys:sap-int (error-pointer c))))))

;;;; Backend Protocol

(defclass backend ()
  ((name :initarg :name 
         :initform (error "Backend name required")
         :reader backend-name)
   (capabilities :initarg :capabilities 
                 :initform '(:basic)
                 :reader backend-capabilities)
   (priority :initarg :priority 
             :initform 0
             :reader backend-priority))
  (:documentation "Base class for all FFI backends"))

(defgeneric backend-call (backend address return-type arg-types &rest args)
  (:documentation "Execute a foreign function call through this backend"))

(defgeneric backend-alloc (backend size)
  (:documentation "Allocate foreign memory through this backend"))

(defgeneric backend-free (backend pointer)
  (:documentation "Free foreign memory through this backend"))

(defgeneric has-capability-p (backend capability)
  (:documentation "Check if backend has a specific capability")
  (:method ((backend backend) capability)
    (member capability (backend-capabilities backend))))

(defgeneric supports-signature-p (backend return-type arg-types)
  (:documentation "Check if backend can handle a specific signature")
  (:method ((backend backend) return-type arg-types)
    ;; Default: basic backend supports simple types
    (and (member return-type '(:void :int :long :pointer :float :double))
         (every (lambda (type) 
                  (member type '(:int :long :pointer :float :double :string)))
                arg-types))))

(defgeneric estimated-call-overhead (backend)
  (:documentation "Return estimated call overhead in nanoseconds")
  (:method ((backend backend))
    1000)) ; Default 1 microsecond

(defgeneric supports-caching-p (backend)
  (:documentation "Check if backend benefits from caching")
  (:method ((backend backend))
    nil))

(defgeneric supports-jit-p (backend)
  (:documentation "Check if backend supports JIT compilation")
  (:method ((backend backend))
    nil))

;;;; Backend Registry

(defvar *backend-registry* (make-hash-table :test #'eq)
  "Registry of available backends")

(defvar *default-backend* nil
  "Default backend for operations")

(defvar *current-backend* nil
  "Currently active backend")

(defun register-backend (name backend)
  "Register a backend with the given name"
  (when (gethash name *backend-registry*)
    (error 'backend-already-registered :name name))
  (setf (gethash name *backend-registry*) backend)
  
  ;; Set as default if first backend or higher priority
  (when (or (null *default-backend*)
            (> (backend-priority backend)
               (backend-priority *default-backend*)))
    (setf *default-backend* backend
          *current-backend* backend))
  backend)

(defun unregister-backend (name)
  "Remove a backend from the registry"
  (let ((backend (gethash name *backend-registry*)))
    (remhash name *backend-registry*)
    (when (eq backend *default-backend*)
      (setf *default-backend* (find-highest-priority-backend)
            *current-backend* *default-backend*))
    backend))

(defun find-backend (name)
  "Find a registered backend by name"
  (gethash name *backend-registry*))

(defun list-backends ()
  "List all registered backends"
  (loop for name being the hash-keys of *backend-registry*
        using (hash-value backend)
        collect (cons name backend)))

(defun find-highest-priority-backend ()
  "Find the backend with highest priority"
  (let ((best nil))
    (maphash (lambda (name backend)
               (declare (ignore name))
               (when (or (null best)
                         (> (backend-priority backend)
                            (backend-priority best)))
                 (setf best backend)))
             *backend-registry*)
    best))

(defun select-backend-for-signature (return-type arg-types)
  "Select the best backend for a given signature"
  (let ((suitable-backends
         (loop for (name . backend) in (list-backends)
               when (supports-signature-p backend return-type arg-types)
               collect backend)))
    
    (unless suitable-backends
      (error 'no-suitable-backend 
             :signature (list return-type arg-types)))
    
    ;; Return highest priority suitable backend
    (first (sort suitable-backends #'> :key #'backend-priority))))

;;;; Dynamic Backend Switching

(defmacro with-backend (backend &body body)
  "Execute body with specified backend active"
  `(let ((*current-backend* ,backend))
     ,@body))

(defmacro with-fallback-backend (fallback-name &body body)
  "Execute body with fallback backend if current fails"
  `(handler-case
       (progn ,@body)
     (backend-error ()
       (with-backend (find-backend ,fallback-name)
         ,@body))))

;;;; Test Backend Implementations

(defclass test-backend (backend)
  ()
  (:documentation "Test backend for unit testing"))

(defclass mock-backend (backend)
  ((expectations :initform '() :accessor backend-expectations)
   (calls :initform '() :accessor backend-calls)
   (allocated :initform '() :accessor backend-allocated))
  (:documentation "Mock backend for testing"))

(defmethod backend-call ((backend mock-backend) address return-type arg-types &rest args)
  (push (list :address address 
              :return-type return-type 
              :arg-types arg-types 
              :args args)
        (backend-calls backend))
  ;; Return a default value based on return type
  (case return-type
    (:int 84)
    (:pointer (sb-sys:int-sap #x1000))
    (:void nil)
    (t 0)))

(defmethod backend-alloc ((backend mock-backend) size)
  (let ((ptr (sb-sys:int-sap (+ #x10000 (* (length (backend-allocated backend)) #x1000)))))
    (push (cons ptr size) (backend-allocated backend))
    ptr))

(defmethod backend-free ((backend mock-backend) pointer)
  (unless (assoc pointer (backend-allocated backend) :test #'sb-sys:sap=)
    (error 'double-free-error :pointer pointer))
  (setf (backend-allocated backend)
        (remove pointer (backend-allocated backend) 
                :key #'car :test #'sb-sys:sap=)))

(defclass failing-backend (backend)
  ()
  (:documentation "Backend that always fails for testing error handling"))

(defmethod backend-call ((backend failing-backend) address return-type arg-types &rest args)
  (declare (ignore address return-type arg-types args))
  (error 'backend-call-error :details "Intentional failure"))

(defmethod backend-alloc ((backend failing-backend) size)
  (error 'allocation-error :size size))

(defclass thread-safe-backend (mock-backend)
  ((lock :initform (sb-thread:make-mutex :name "backend-lock")))
  (:documentation "Thread-safe mock backend"))

(defmethod backend-call ((backend thread-safe-backend) address return-type arg-types &rest args)
  (sb-thread:with-mutex ((slot-value backend 'lock))
    (call-next-method)))

(defclass trampoline-backend (backend)
  ()
  (:default-initargs 
   :capabilities '(:basic :fast)
   :priority 100)
  (:documentation "Fast trampoline-based backend"))

(defmethod estimated-call-overhead ((backend trampoline-backend))
  10) ; 10 nanoseconds

(defmethod supports-caching-p ((backend trampoline-backend))
  t)

(defmethod supports-jit-p ((backend trampoline-backend))
  t)

(defclass libffi-backend (backend)
  ()
  (:default-initargs
   :capabilities '(:basic :callbacks :structs-by-value :varargs)
   :priority 50)
  (:documentation "libffi-based backend for complex signatures"))

(defmethod estimated-call-overhead ((backend libffi-backend))
  100) ; 100 nanoseconds

(defmethod supports-signature-p ((backend libffi-backend) return-type arg-types)
  ;; libffi can handle almost anything
  (declare (ignore return-type arg-types))
  t)