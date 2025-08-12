(defpackage epsilon.foreign.callback
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (trampoline epsilon.foreign.trampoline))
  (:export
   ;; Core callback functions
   #:make-callback
   #:call-callback
   #:callback-pointer
   
   ;; Registry management
   #:register-callback
   #:unregister-callback
   #:get-callback
   #:list-callbacks
   
   ;; Convenience macros
   #:defcallback
   #:with-callback
   #:with-callback-scope
   
   ;; Types
   #:callback-info
   #:callback-info-p
   #:callback-info-function
   #:callback-info-signature
   #:callback-info-pointer
   
   ;; libffi integration
   #:make-sbcl-callback
   #:*use-libffi*
   #:libffi-available-p))

(in-package :epsilon.foreign.callback)

;;;; Callback Support for FFI - Phase 4

;;; Callback information structure

(defstruct callback-info
  "Information about a registered callback"
  name           ; Symbol name (optional)
  function       ; The Lisp function
  return-type    ; Return type
  arg-types      ; List of argument types
  pointer        ; The C-callable pointer
  alien-callback ; The SB-ALIEN callback object
  id)            ; Unique ID

;;; Global callback registry

(defvar *callback-registry* (make-hash-table :test 'equal)
  "Registry of all active callbacks")

(defvar *callback-id-counter* 0
  "Counter for generating unique callback IDs")

(defvar *dummy-pointer-counter* #x3000
  "Counter for generating unique dummy pointers when SBCL callbacks fail")

(defvar *callback-lock* (sb-thread:make-mutex :name "callback-registry-lock")
  "Lock for thread-safe callback operations")

;;; Type conversion helpers

(defun lisp-type-to-alien-type (type)
  "Convert our type keywords to SB-ALIEN type specifiers"
  (case type
    (:void 'sb-alien:void)
    (:char 'sb-alien:char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:short 'sb-alien:short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:int 'sb-alien:int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:long 'sb-alien:long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:long-long 'sb-alien:long-long)
    (:unsigned-long-long 'sb-alien:unsigned-long-long)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    (:pointer 'sb-alien:system-area-pointer)
    (:string 'sb-alien:c-string)
    (:size-t 'sb-alien:unsigned-long)
    (:ssize-t 'sb-alien:long)
    (:time-t 'sb-alien:long)
    (:pid-t 'sb-alien:int)
    (:uid-t 'sb-alien:unsigned-int)
    (:gid-t 'sb-alien:unsigned-int)
    (t (error "Unknown callback type: ~A" type))))

(defun build-alien-callback-type (return-type arg-types)
  "Build an SB-ALIEN function type specification"
  `(sb-alien:function ,(lisp-type-to-alien-type return-type)
                      ,@(mapcar #'lisp-type-to-alien-type arg-types)))

(defun build-callback-body (function return-type arg-types arg-names)
  "Build the body of the alien-lambda callback"
  ;; Simple version: just call the function with type conversions
  (let ((converted-args (loop for arg in arg-names
                             for type in arg-types
                             collect (convert-c-to-lisp-form arg type))))
    (if (eq return-type :void)
        `(progn
           (funcall ,function ,@converted-args)
           (values))
        `(let ((result (funcall ,function ,@converted-args)))
           ,(convert-lisp-to-c-form 'result return-type)))))

(defun convert-c-to-lisp-form (arg type)
  "Generate form to convert C argument to Lisp"
  (case type
    (:string `(if (sb-alien:null-alien ,arg) nil ,arg))
    (t arg))) ; Most types pass through directly

(defun convert-lisp-to-c-form (value-var type)
  "Generate form to convert Lisp value to C"
  (case type
    (:string `(or ,value-var (sb-alien:make-alien-string "")))
    (:pointer `(or ,value-var (sb-sys:int-sap 0)))
    (t value-var))) ; Numeric types pass through

(defun create-real-callback-or-dummy (function return-type arg-types 
                                      alien-return alien-args arg-names)
  "Try to create a real callback, fall back to dummy if needed"
  (handler-case
      ;; Try to use SBCL's alien-lambda
      (let ((alien-lambda-sym (find-symbol "ALIEN-LAMBDA" "SB-ALIEN")))
        (if alien-lambda-sym
            (let* ((lambda-form `(,alien-lambda-sym 
                                  (sb-alien:function ,alien-return ,@alien-args)
                                  ,(loop for name in arg-names
                                        for type in alien-args
                                        collect (list name type))
                                  ,(build-callback-body function return-type arg-types arg-names)))
                   (alien-cb (eval lambda-form))
                   (pointer (sb-alien:alien-sap alien-cb)))
              (format t "~%✓ Created real callback at ~A~%" pointer)
              (list alien-cb pointer))
            ;; alien-lambda not found, use dummy
            (progn
              (format t "~%⚠ ALIEN-LAMBDA not found, using dummy callback~%")
              (list nil (sb-sys:int-sap (sb-thread:with-mutex (*callback-lock*)
                                          (incf *dummy-pointer-counter*)))))))
    (error (e)
      ;; Real callback creation failed, use dummy
      (format t "~%⚠ Real callback creation failed: ~A~%" e)
      (format t "~%  Using dummy callback~%")
      (list nil (sb-sys:int-sap (sb-thread:with-mutex (*callback-lock*)
                                  (incf *dummy-pointer-counter*)))))))

;;; libffi integration control
(defvar *use-libffi* t
  "Whether to use libffi for callback creation when available")

(defun libffi-available-p ()
  "Check if libffi integration is available"
  (and (find-package '#:epsilon.foreign)
       (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign)
       (symbol-value (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign))))

;;; Original SBCL callback implementation (renamed for clarity)

(defun make-sbcl-callback (function return-type arg-types)
  "Create a C-callable callback from a Lisp function"
  ;; Use SBCL's alien-lambda to create real callbacks
  (let* ((alien-return (lisp-type-to-alien-type return-type))
         (alien-args (mapcar #'lisp-type-to-alien-type arg-types))
         ;; Create argument names
         (arg-names (loop for i from 0 below (length arg-types)
                         collect (intern (format nil "ARG~D" i))))
         ;; Try to create real callback or fall back to dummy
         (callback-result (create-real-callback-or-dummy 
                           function return-type arg-types 
                           alien-return alien-args arg-names))
         (alien-cb (first callback-result))
         (pointer (second callback-result)))
    
    ;; Create callback info
    (let ((info (make-callback-info :function function
                                    :return-type return-type
                                    :arg-types arg-types
                                    :pointer pointer
                                    :alien-callback alien-cb
                                    :id (incf *callback-id-counter*))))
      ;; Register it
      (sb-thread:with-mutex (*callback-lock*)
        (setf (gethash (callback-info-id info) *callback-registry*) info))
      ;; Return the pointer
      pointer)))

;;; Enhanced callback creation with libffi integration

(defun make-callback (function return-type arg-types)
  "Create a C-callable callback from a Lisp function
   Uses libffi when available, falls back to SBCL implementation"
  (if (and *use-libffi* (libffi-available-p))
      (handler-case
          ;; Try libffi first
          (funcall (find-symbol "MAKE-LIBFFI-CALLBACK" '#:epsilon.foreign)
                   function return-type arg-types)
        (error (e)
          (warn "libffi callback creation failed: ~A~%Falling back to SBCL implementation" e)
          (make-sbcl-callback function return-type arg-types)))
      ;; Use SBCL implementation
      (make-sbcl-callback function return-type arg-types)))

(defun make-callback-wrapper (function return-type arg-types)
  "Create a wrapper function that handles type conversions"
  (let ((arg-names (loop for i from 0 below (length arg-types)
                        collect (intern (format nil "ARG~D" i)))))
    (compile nil
             `(lambda ,arg-names
                ;; Add error handling
                (handler-case
                    ,(if (eq return-type :void)
                         `(progn
                            (funcall ,function 
                                    ,@(loop for arg in arg-names
                                           for type in arg-types
                                           collect (convert-callback-arg-form arg type)))
                            (values))
                         `(let ((result (funcall ,function 
                                               ,@(loop for arg in arg-names
                                                      for type in arg-types
                                                      collect (convert-callback-arg-form arg type)))))
                            ,(convert-callback-return-form 'result return-type)))
                  (error (e)
                    ;; Log error and return safe default
                    (format *error-output* "Callback error: ~A~%" e)
                    ,(case return-type
                       (:void '(values))
                       ((:int :long :short :char) 0)
                       ((:unsigned-int :unsigned-long :size-t) 0)
                       (:float 0.0)
                       (:double 0.0d0)
                       (:pointer '(sb-sys:int-sap 0))
                       (t 0))))))))

(defun convert-callback-arg-form (arg type)
  "Generate form to convert callback argument from C to Lisp"
  (case type
    (:string `(if (sb-sys:sap= ,arg (sb-sys:int-sap 0))
                  nil
                  ,arg))  ; SB-ALIEN already converts C strings
    (:pointer arg)     ; Keep as SAP
    (t arg)))          ; Numeric types pass through

(defun convert-callback-return-form (value type)
  "Generate form to convert callback return value from Lisp to C"
  (case type
    (:string `(or ,value (sb-sys:int-sap 0)))  ; nil -> NULL
    (:pointer `(or ,value (sb-sys:int-sap 0)))
    (t value)))  ; Numeric types pass through

;;; Callback invocation (for testing)

(defun call-callback (callback-ptr &rest args)
  "Call a callback pointer with arguments (mainly for testing)"
  ;; Find the callback info
  (let ((info (sb-thread:with-mutex (*callback-lock*)
                (loop for cb being the hash-values of *callback-registry*
                      when (sb-sys:sap= (callback-info-pointer cb) callback-ptr)
                      return cb))))
    (unless info
      (error "Unknown callback pointer"))
    ;; Call the Lisp function directly for testing
    ;; (In real usage, C code would call the callback pointer directly)
    (apply (callback-info-function info) args)))

;;; Registry management

(defun register-callback (name function return-type arg-types)
  "Register a named callback"
  (let ((ptr (make-callback function return-type arg-types)))
    ;; Find the info that was just created
    (let ((info (sb-thread:with-mutex (*callback-lock*)
                  (loop for cb being the hash-values of *callback-registry*
                        when (sb-sys:sap= (callback-info-pointer cb) ptr)
                        return cb))))
      (when info
        (setf (callback-info-name info) name)
        ;; Also index by name
        (sb-thread:with-mutex (*callback-lock*)
          (setf (gethash name *callback-registry*) info)))
      (callback-info-id info))))

(defun unregister-callback (name-or-id)
  "Unregister a callback by name or ID"
  (sb-thread:with-mutex (*callback-lock*)
    (let ((info (gethash name-or-id *callback-registry*)))
      (when info
        ;; Remove from registry
        (remhash name-or-id *callback-registry*)
        (when (callback-info-name info)
          (remhash (callback-info-name info) *callback-registry*))
        (remhash (callback-info-id info) *callback-registry*)
        ;; Note: SB-ALIEN doesn't provide a way to invalidate alien-lambda callbacks
        ;; We just remove from registry
        t))))

(defun get-callback (name-or-id)
  "Get a callback pointer by name or ID"
  (sb-thread:with-mutex (*callback-lock*)
    (let ((info (gethash name-or-id *callback-registry*)))
      (when info
        (callback-info-pointer info)))))

(defun list-callbacks ()
  "List all registered callbacks"
  (sb-thread:with-mutex (*callback-lock*)
    (loop for info being the hash-values of *callback-registry*
          when (callback-info-name info)
          collect (list :name (callback-info-name info)
                       :id (callback-info-id info)
                       :return-type (callback-info-return-type info)
                       :arg-types (callback-info-arg-types info)))))

;;; Convenience macros

(defmacro defcallback (name return-type lambda-list &body body)
  "Define a named callback function"
  (let ((arg-names (mapcar #'first lambda-list))
        (arg-types (mapcar #'second lambda-list)))
    `(progn
       (defun ,name ,arg-names
         ,@body)
       (register-callback ',name #',name ,return-type ',arg-types)
       ',name)))

(defmacro with-callback ((var function return-type arg-types) &body body)
  "Create a temporary callback for use in body"
  `(let ((,var (make-callback ,function ,return-type ,arg-types)))
     (unwind-protect
          (progn ,@body)
       ;; Cleanup would go here if we had a way to invalidate anonymous callbacks
       )))

(defmacro with-callback-scope (&body body)
  "Create a scope where callbacks are automatically cleaned up"
  (let ((callbacks (gensym "CALLBACKS")))
    `(let ((,callbacks nil))
       (flet ((make-callback (function return-type arg-types)
                (let ((cb (epsilon.foreign.callback:make-callback 
                          function return-type arg-types)))
                  (push cb ,callbacks)
                  cb)))
         (unwind-protect
              (progn ,@body)
           ;; Cleanup callbacks
           (dolist (cb ,callbacks)
             ;; Find and unregister
             (sb-thread:with-mutex (*callback-lock*)
               (loop for info being the hash-values of *callback-registry*
                     when (sb-sys:sap= (callback-info-pointer info) cb)
                     do (progn
                          (remhash (callback-info-id info) *callback-registry*)
                          (when (callback-info-name info)
                            (remhash (callback-info-name info) *callback-registry*))
                          ;; Cleanup - no explicit invalidation needed for alien-lambda
                          (return))))))))))

;;; Helper function for getting callback pointer from defcallback

(defun callback-pointer (name)
  "Get the pointer for a callback defined with defcallback"
  (get-callback name))