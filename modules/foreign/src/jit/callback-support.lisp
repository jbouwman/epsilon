;;;; callback-support.lisp - JIT callback support for FFI
;;;;
;;;; Provides callbacks (Lisp functions callable from C) integrated with
;;;; the JIT FFI infrastructure. Uses SBCL's define-alien-callable
;;;; (GC-safe static trampolines) for actual callback mechanism.
;;;;
;;;; Strategy:
;;;; - Use SBCL's alien-callable infrastructure for GC-safe C pointers
;;;; - Wrap with JIT infrastructure for consistent API
;;;; - Provide lifecycle management and registry
;;;; - Support type-safe argument/return conversion

(defpackage epsilon.foreign.jit.callback
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit))
  (:export
   ;; Callback creation
   #:make-jit-callback
   #:free-jit-callback
   #:with-jit-callback

   ;; Callback info
   #:jit-callback
   #:jit-callback-p
   #:jit-callback-id
   #:jit-callback-name
   #:jit-callback-pointer
   #:jit-callback-function
   #:jit-callback-return-type
   #:jit-callback-arg-types

   ;; Testing
   #:call-callback
   #:callback-pointer
   #:callback-pointer-int

   ;; Registry
   #:register-callback
   #:unregister-callback
   #:get-callback
   #:list-callbacks
   #:clear-callbacks

   ;; Convenience macro
   #:defcallback)
  (:enter t))

;;; ============================================================================
;;; Callback Structure
;;; ============================================================================

(defstruct (jit-callback (:constructor %make-jit-callback))
  "A JIT callback - Lisp function callable from C"
  (id nil :type (or null integer))
  (name nil :type (or null symbol))
  (function nil :type (or null function))
  (return-type nil :type keyword)
  (arg-types nil :type list)
  (pointer nil :type (or null sb-sys:system-area-pointer))
  (alien-callback nil))  ; The underlying SBCL alien-lambda

;;; ============================================================================
;;; Registry
;;; ============================================================================

(defvar *callback-registry* (make-hash-table :test 'eq)
  "Registry of active JIT callbacks by ID")

(defvar *callback-name-index* (make-hash-table :test 'eq)
  "Index of callbacks by name")

(defvar *callback-pointer-index* (make-hash-table :test 'eql)
  "Index of callbacks by pointer address")

(defvar *callback-id-counter* 0
  "Counter for generating unique callback IDs")

(defvar *callback-lock* (sb-thread:make-mutex :name "jit-callback-lock")
  "Lock for thread-safe callback operations")

(defun next-callback-id ()
  "Generate the next callback ID"
  (sb-thread:with-mutex (*callback-lock*)
    (incf *callback-id-counter*)))

;;; ============================================================================
;;; Type Conversion
;;; ============================================================================

(defun lisp-type-to-alien (type)
  "Convert our type keywords to SB-ALIEN type specifiers"
  (case type
    (:void 'sb-alien:void)
    (:char 'sb-alien:char)
    (:uchar 'sb-alien:unsigned-char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:short 'sb-alien:short)
    (:ushort 'sb-alien:unsigned-short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:int 'sb-alien:int)
    (:uint 'sb-alien:unsigned-int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:long 'sb-alien:long)
    (:ulong 'sb-alien:unsigned-long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    (:pointer 'sb-alien:system-area-pointer)
    (:size-t 'sb-alien:unsigned-long)
    (:ssize-t 'sb-alien:long)
    (otherwise (error "Unknown callback type: ~A" type))))

;;; ============================================================================
;;; Callback Body Generation
;;; ============================================================================

(defun generate-callback-body (function return-type arg-types arg-names)
  "Generate the body for an alien-lambda callback.
   Handles type conversions and error handling."
  (declare (ignore arg-types))  ; Reserved for future type coercion
  (let ((call-form `(funcall ,function ,@arg-names)))
    (if (eq return-type :void)
        `(progn
           (handler-case
               ,call-form
             (error (e)
               (format *error-output* "~&Callback error: ~A~%" e)))
           (values))
        `(handler-case
             ,call-form
           (error (e)
             (format *error-output* "~&Callback error: ~A~%" e)
             ,(default-return-value return-type))))))

(defun default-return-value (type)
  "Return a safe default value for the given type on error."
  (case type
    (:void '(values))
    ((:char :short :int :long :ssize-t) 0)
    ((:uchar :ushort :uint :ulong :size-t :unsigned-char :unsigned-short
      :unsigned-int :unsigned-long) 0)
    (:float 0.0)
    (:double 0.0d0)
    (:pointer '(sb-sys:int-sap 0))
    (otherwise 0)))

;;; ============================================================================
;;; Callback Creation
;;; ============================================================================

(defun make-jit-callback (function return-type arg-types &key name)
  "Create a JIT callback from a Lisp function.

   FUNCTION - The Lisp function to wrap
   RETURN-TYPE - Return type keyword (:int, :double, :void, :pointer, etc.)
   ARG-TYPES - List of argument type keywords
   :NAME - Optional name for the callback (for registry lookup)

   Returns a JIT-CALLBACK structure. The callback pointer can be passed to
   C functions that expect function pointers.

   Example:
   (make-jit-callback (lambda (x) (* x 2)) :int '(:int))

   The callback remains valid until FREE-JIT-CALLBACK is called or
   CLEAR-CALLBACKS is used."
  (let* ((id (next-callback-id))
         (alien-return (lisp-type-to-alien return-type))
         (alien-args (mapcar #'lisp-type-to-alien arg-types))
         (arg-names (loop for i from 0 below (length arg-types)
                          collect (intern (format nil "ARG~D" i))))
         ;; Build typed argument list for alien-lambda2
         (typed-args (loop for name in arg-names
                           for type in alien-args
                           collect (list name type)))
         ;; Generate the body
         (body (generate-callback-body function return-type arg-types arg-names))
         ;; Unique symbol for define-alien-callable registry
         (cb-name (or name (intern (format nil "JIT-CB-~D" id)
                                   (find-package :epsilon.foreign.jit.callback)))))

    ;; Create callback using alien-lambda2 + %define-alien-callable.
    ;; This uses GC-safe static trampolines (immobile space) instead of
    ;; alien-lambda which can corrupt memory (see IMPL-224).
    (multiple-value-bind (alien-cb pointer)
        (handler-case
            (let* ((lambda2-form `(sb-alien::alien-lambda2
                                   ,alien-return
                                   ,typed-args
                                   ,body))
                   (fn (eval lambda2-form))
                   (type-spec `(sb-alien:function ,alien-return ,@alien-args))
                   (parsed-type (sb-alien::parse-alien-type type-spec nil)))
              (sb-alien::%define-alien-callable cb-name fn parsed-type type-spec)
              (let ((acf (sb-alien:alien-callable-function cb-name)))
                (values acf (sb-alien:alien-sap acf))))
          (error ()
            (values nil nil)))

      ;; Create callback structure
      (let ((callback (%make-jit-callback
                       :id id
                       :name name
                       :function function
                       :return-type return-type
                       :arg-types arg-types
                       :pointer (or pointer (sb-sys:int-sap 0))
                       :alien-callback alien-cb)))

        ;; Register it
        (sb-thread:with-mutex (*callback-lock*)
          (setf (gethash id *callback-registry*) callback)
          (when pointer
            (setf (gethash (sb-sys:sap-int pointer) *callback-pointer-index*) callback))
          (when name
            (setf (gethash name *callback-name-index*) callback)))

        callback))))

(defun free-jit-callback (callback)
  "Free a JIT callback and remove it from the registry.

   Note: The callback pointer becomes invalid after this call.
   Any C code still holding the pointer will crash if it calls it."
  (when callback
    (sb-thread:with-mutex (*callback-lock*)
      (let ((id (jit-callback-id callback))
            (name (jit-callback-name callback))
            (pointer (jit-callback-pointer callback)))
        ;; Remove from registries
        (remhash id *callback-registry*)
        (when pointer
          (remhash (sb-sys:sap-int pointer) *callback-pointer-index*))
        (when name
          (remhash name *callback-name-index*))
        ;; Clear the callback structure
        (setf (jit-callback-pointer callback) nil)
        (setf (jit-callback-alien-callback callback) nil)))
    t))

(defmacro with-jit-callback ((var function return-type arg-types) &body body)
  "Create a temporary JIT callback for use within BODY.
   The callback is automatically freed when BODY exits."
  (let ((cb (gensym "CALLBACK")))
    `(let* ((,cb (make-jit-callback ,function ,return-type ,arg-types))
            (,var (jit-callback-pointer ,cb)))
       (unwind-protect
            (progn ,@body)
         (free-jit-callback ,cb)))))

;;; ============================================================================
;;; Registry Operations
;;; ============================================================================

(defun register-callback (name function return-type arg-types)
  "Register a named callback. Returns the callback ID."
  (let ((callback (make-jit-callback function return-type arg-types :name name)))
    (jit-callback-id callback)))

(defun unregister-callback (name-or-id)
  "Unregister a callback by name or ID."
  (let ((callback (get-callback name-or-id)))
    (when callback
      (free-jit-callback callback))))

(defun get-callback (name-or-id)
  "Get a callback by name or ID. Returns the JIT-CALLBACK structure."
  (sb-thread:with-mutex (*callback-lock*)
    (etypecase name-or-id
      (symbol (gethash name-or-id *callback-name-index*))
      (integer (gethash name-or-id *callback-registry*)))))

(defun get-callback-by-pointer (pointer)
  "Get a callback by its pointer address."
  (sb-thread:with-mutex (*callback-lock*)
    (gethash (if (integerp pointer) pointer (sb-sys:sap-int pointer))
             *callback-pointer-index*)))

(defun list-callbacks ()
  "List all registered callbacks."
  (sb-thread:with-mutex (*callback-lock*)
    (let ((result '()))
      (maphash (lambda (id callback)
                 (push (list :id id
                             :name (jit-callback-name callback)
                             :return-type (jit-callback-return-type callback)
                             :arg-types (jit-callback-arg-types callback)
                             :pointer (when (jit-callback-pointer callback)
                                        (sb-sys:sap-int (jit-callback-pointer callback))))
                       result))
               *callback-registry*)
      result)))

(defun clear-callbacks ()
  "Clear all registered callbacks."
  (sb-thread:with-mutex (*callback-lock*)
    (maphash (lambda (id callback)
               (declare (ignore id))
               (setf (jit-callback-pointer callback) nil)
               (setf (jit-callback-alien-callback callback) nil))
             *callback-registry*)
    (clrhash *callback-registry*)
    (clrhash *callback-name-index*)
    (clrhash *callback-pointer-index*))
  t)

;;; ============================================================================
;;; Convenience Macro
;;; ============================================================================

(defmacro defcallback (name return-type lambda-list &body body)
  "Define a named JIT callback.

   NAME - Symbol naming the callback
   RETURN-TYPE - Return type keyword
   LAMBDA-LIST - List of (name type) pairs
   BODY - Callback body

   Example:
   (defcallback my-comparator :int ((a :int) (b :int))
     (- a b))

   After definition, (get-callback 'my-comparator) returns the callback,
   and (jit-callback-pointer (get-callback 'my-comparator)) returns the
   C-callable function pointer."
  (let ((arg-names (mapcar #'first lambda-list))
        (arg-types (mapcar #'second lambda-list)))
    `(progn
       ;; Unregister any existing callback with this name
       (when (get-callback ',name)
         (unregister-callback ',name))
       ;; Define the Lisp function
       (defun ,name ,arg-names
         ,@body)
       ;; Register the callback
       (register-callback ',name #',name ,return-type ',arg-types)
       ',name)))

;;; ============================================================================
;;; Callback Invocation (for testing)
;;; ============================================================================

(defun call-callback (callback &rest args)
  "Call a callback directly from Lisp (mainly for testing).
   In real usage, C code calls the callback via the pointer.
   This function mirrors the error handling of the alien-lambda wrapper."
  (let ((return-type (jit-callback-return-type callback)))
    (handler-case
        (apply (jit-callback-function callback) args)
      (error (e)
        (format *error-output* "~&Callback error: ~A~%" e)
        (default-return-value return-type)))))

(defun callback-pointer (name)
  "Get the C-callable pointer for a named callback."
  (let ((callback (get-callback name)))
    (when callback
      (jit-callback-pointer callback))))

(defun callback-pointer-int (name)
  "Get the C-callable pointer as an integer for a named callback."
  (let ((pointer (callback-pointer name)))
    (when pointer
      (sb-sys:sap-int pointer))))
