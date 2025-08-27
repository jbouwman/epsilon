;;;; Core API for epsilon.foreign
;;;;
;;;; Minimal 22-function public API

(defpackage epsilon.foreign.core
  (:use cl)
  (:local-nicknames
   (#:backend epsilon.foreign.backend.protocol)
   (#:types epsilon.foreign.types)
   (#:lib epsilon.library))
  (:export
   ;; Function Definition & Calling (5)
   #:defshared
   #:shared-call
   #:lib-open
   #:lib-close
   #:lib-function
   
   ;; Memory Management (3)
   #:foreign-alloc
   #:foreign-free
   #:with-foreign-memory
   
   ;; Type Conversion (4)
   #:convert-to-foreign
   #:convert-from-foreign
   #:define-c-type
   #:with-c-string
   
   ;; Structures (5)
   #:define-c-struct
   #:with-c-struct
   #:struct-ref
   #:struct-size
   
   ;; Callbacks (3)
   #:defcallback
   #:with-callback
   #:callback-pointer
   
   ;; Error Handling (2)
   #:foreign-error
   #:with-foreign-error-handler))

(in-package epsilon.foreign.core)

;;;; Error Conditions

(define-condition foreign-error (error)
  ((function :initarg :function :reader foreign-error-function)
   (code :initarg :code :reader foreign-error-code :initform nil)
   (details :initarg :details :reader foreign-error-details :initform nil))
  (:report (lambda (c s)
             (format s "Foreign function error in ~A~@[: ~A~]"
                     (foreign-error-function c)
                     (foreign-error-details c)))))

;;;; Global State

(defvar *function-cache* (make-hash-table :test 'equal))
(defvar *struct-definitions* (make-hash-table :test 'eq))
(defvar *callbacks* (make-hash-table :test 'eq))
(defvar *current-backend* nil)

;;;; Function Definition & Calling

(defmacro defshared (name c-name library return-type args &rest options)
  "Define a Lisp function that calls a foreign function.
   
   Arguments:
     name - Lisp function name
     c-name - C function name (string)
     library - Library name (string) or nil for libc
     return-type - Return type keyword
     args - List of (arg-name arg-type) pairs
     options - Optional keyword arguments
     
   Options:
     :documentation - Function documentation
     :convention - Calling convention (:cdecl or :stdcall)
     
   Example:
     (defshared strlen \"strlen\" \"libc\" :size-t ((str :string))
       :documentation \"Get string length\")"
  (let* ((doc (getf options :documentation))
         (arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args)))
    `(defun ,name ,arg-names
       ,@(when doc (list doc))
       (shared-call (list ,c-name ,(or library "libc")) 
                    ,return-type ',arg-types 
                    ,@arg-names))))

(defun shared-call (function-designator return-type arg-types &rest args)
  "Call a foreign function.
   
   Arguments:
     function-designator - String, symbol, or (name library) list
     return-type - Return type keyword
     arg-types - List of argument type keywords  
     args - Arguments to pass
     
   Returns:
     The foreign function's return value, converted to Lisp
     
   Example:
     (shared-call \"strlen\" :size-t '(:string) \"hello\")"
  (let* ((key (list function-designator return-type arg-types))
         (cached (gethash key *function-cache*)))
    (unless cached
      (let* ((address (resolve-function-address function-designator))
             (backend (or *current-backend*
                          (backend:select-backend-for-signature 
                           return-type arg-types))))
        (unless address
          (error 'foreign-error 
                 :function function-designator
                 :details "Function not found"))
        (setf cached (list backend address)
              (gethash key *function-cache*) cached)))
    
    (destructuring-bind (backend address) cached
      (let ((converted-args 
             (loop for arg in args
                   for type in arg-types
                   collect (types:convert-to-foreign arg type))))
        (unwind-protect
             (types:convert-from-foreign
              (apply #'backend:backend-call 
                     backend address return-type arg-types converted-args)
              return-type)
          ;; Clean up string conversions
          (loop for arg in converted-args
                for type in arg-types
                when (eq type :string)
                  do (types:free-converted-object arg type)))))))

(defun resolve-function-address (designator)
  "Resolve a function designator to an address"
  (etypecase designator
    (string (lib-function (lib-open "libc") designator))
    (symbol (lib-function (lib-open "libc") (string designator)))
    (list (destructuring-bind (name library) designator
            (lib-function (lib-open library) name)))))

;;;; Library Management

(defun lib-open (name &key local paths)
  "Open a shared library"
  (lib:lib-open name :local local :paths paths))

(defun lib-close (handle)
  "Close a shared library"
  (lib:lib-close handle))

(defun lib-function (handle name)
  "Get a function pointer from a library"
  (lib:lib-function handle name))

;;;; Memory Management

(defun foreign-alloc (size)
  "Allocate foreign memory"
  (if (zerop size)
      (sb-sys:int-sap 1)  ; Return non-null for zero allocation
      (let ((backend (or *current-backend* backend:*default-backend*)))
        (or (backend:backend-alloc backend size)
            (sb-alien:alien-sap (sb-alien:make-alien sb-alien:char size))))))

(defun foreign-free (pointer)
  "Free foreign memory"
  (unless (sb-sys:sap= pointer (sb-sys:int-sap 1))  ; Skip our zero-size marker
    (let ((backend (or *current-backend* backend:*default-backend*)))
      (handler-case
          (backend:backend-free backend pointer)
        (error ()
          ;; Fallback to sb-alien
          (sb-alien:free-alien 
           (sb-alien:sap-alien pointer (* sb-alien:char))))))))

(defmacro with-foreign-memory ((var size) &body body)
  "Allocate foreign memory for the duration of body"
  `(let ((,var (foreign-alloc ,size)))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;;; Type Conversion

(defun convert-to-foreign (value type)
  "Convert a Lisp value to foreign representation"
  (types:convert-to-foreign value type))

(defun convert-from-foreign (value type)
  "Convert a foreign value to Lisp representation"
  (types:convert-from-foreign value type))

(defun define-c-type (name size &rest options)
  "Define a new C type"
  (apply #'types:define-foreign-type name :size size options))

(defmacro with-c-string ((var string) &body body)
  "Bind VAR to a foreign string for the duration of BODY"
  `(types:with-c-string (,var ,string)
     ,@body))

;;;; Structure Support

(defun define-c-struct (name fields)
  "Define a C structure type"
  (let* ((layout (compute-struct-layout fields))
         (size (reduce #'max layout :key #'second :initial-value 0)))
    (setf (gethash name *struct-definitions*)
          (list :fields fields :layout layout :size size))
    name))

(defun compute-struct-layout (fields)
  "Compute field offsets and struct size"
  (let ((offset 0)
        (layout '()))
    (dolist (field fields)
      (destructuring-bind (name type) field
        (let* ((size (types:sizeof type))
               (align (types:alignof type))
               (aligned-offset (align-to offset align)))
          (push (list name aligned-offset size type) layout)
          (setf offset (+ aligned-offset size)))))
    (nreverse layout)))

(defun align-to (value alignment)
  "Align value to alignment boundary"
  (let ((remainder (mod value alignment)))
    (if (zerop remainder)
        value
        (+ value (- alignment remainder)))))

(defmacro with-c-struct ((var type) &body body)
  "Allocate a C struct for the duration of body"
  (let ((def (gensym)))
    `(let ((,def (gethash ',type *struct-definitions*)))
       (unless ,def
         (error "Unknown struct type: ~A" ',type))
       (let ((,var (foreign-alloc (getf ,def :size))))
         (unwind-protect
              (progn ,@body)
           (foreign-free ,var))))))

(defun struct-ref (struct field)
  "Get a field value from a struct"
  (let* ((struct-type (find-struct-type struct))
         (def (gethash struct-type *struct-definitions*))
         (field-info (find field (getf def :layout) :key #'first)))
    (unless field-info
      (error "Unknown field ~A in struct ~A" field struct-type))
    (destructuring-bind (name offset size type) field-info
      (declare (ignore name))
      (types:convert-from-foreign
       (ecase size
         (1 (sb-sys:sap-ref-8 struct offset))
         (2 (sb-sys:sap-ref-16 struct offset))
         (4 (sb-sys:sap-ref-32 struct offset))
         (8 (sb-sys:sap-ref-64 struct offset)))
       type))))

(defun (setf struct-ref) (value struct field)
  "Set a field value in a struct"
  (let* ((struct-type (find-struct-type struct))
         (def (gethash struct-type *struct-definitions*))
         (field-info (find field (getf def :layout) :key #'first)))
    (unless field-info
      (error "Unknown field ~A in struct ~A" field struct-type))
    (destructuring-bind (name offset size type) field-info
      (declare (ignore name))
      (let ((foreign-value (types:convert-to-foreign value type)))
        (ecase size
          (1 (setf (sb-sys:sap-ref-8 struct offset) foreign-value))
          (2 (setf (sb-sys:sap-ref-16 struct offset) foreign-value))
          (4 (setf (sb-sys:sap-ref-32 struct offset) foreign-value))
          (8 (setf (sb-sys:sap-ref-64 struct offset) foreign-value)))
        value))))

(defun find-struct-type (struct)
  "Find the type of a struct pointer"
  ;; For now, return first matching struct
  ;; In real implementation, would track this properly
  (first (loop for name being the hash-keys of *struct-definitions*
               collect name)))

(defun struct-size (type)
  "Get the size of a struct type"
  (let ((def (gethash type *struct-definitions*)))
    (unless def
      (error "Unknown struct type: ~A" type))
    (getf def :size)))

;;;; Callback Support

(defvar *callback-id* 0)

(defmacro defcallback (name return-type args &body body)
  "Define a callback function"
  `(progn
     (setf (gethash ',name *callbacks*)
           (list :return-type ,return-type
                 :arg-types ',(mapcar #'second args)
                 :function (lambda ,(mapcar #'first args)
                             ,@body)))
     ',name))

(defmacro with-callback ((var return-type args &body callback-body) &body body)
  "Create a temporary callback"
  (let ((callback-name (gensym "CALLBACK")))
    `(progn
       (defcallback ,callback-name ,return-type ,args
         ,@callback-body)
       (let ((,var (callback-pointer ',callback-name)))
         (unwind-protect
              (progn ,@body)
           (remhash ',callback-name *callbacks*))))))

(defun callback-pointer (name)
  "Get pointer to a callback function"
  (let ((callback (gethash name *callbacks*)))
    (unless callback
      (error "Unknown callback: ~A" name))
    ;; In real implementation, would create actual callback trampoline
    ;; For now, return a dummy pointer
    (sb-sys:int-sap (+ #x100000 (incf *callback-id*)))))

;;;; Error Handling

(defmacro with-foreign-error-handler (form &body clauses)
  "Handle foreign function errors"
  `(handler-case
       ,form
     (foreign-error (e)
       ,(or (second (assoc :on-error clauses))
            `(error e)))
     (error (e)
       ,(or (second (assoc :on-any-error clauses))
            `(error 'foreign-error :details (format nil "~A" e))))))