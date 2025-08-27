;;;; Type System for epsilon.foreign
;;;;
;;;; Table-driven type conversion with caching and optimization

(defpackage epsilon.foreign.types
  (:use cl)
  (:export
   ;; Type info structure
   #:type-info
   #:type-info-p
   #:type-size
   #:type-alignment
   #:type-sbcl-type
   #:type-base-type
   
   ;; Type registration
   #:define-foreign-type
   #:define-primitive-type
   #:define-type-alias
   #:define-array-type
   #:define-function-type
   #:get-type-info
   #:resolve-type-alias
   
   ;; Type conversion
   #:convert-to-foreign
   #:convert-from-foreign
   #:free-converted-object
   #:with-c-string
   #:with-foreign-values
   
   ;; Type queries
   #:sizeof
   #:alignof
   #:compatible-types-p
   #:void-pointer-p
   #:function-pointer-p
   #:pointer-target-type
   #:function-return-type
   #:function-arg-types
   
   ;; Pointer operations
   #:pointer-to
   #:address-of
   #:dereference
   
   ;; Memory management
   #:handle-to-pointer
   #:pointer-to-handle
   
   ;; Caching
   #:clear-conversion-cache
   
   ;; Error conditions
   #:overflow-error))

(in-package epsilon.foreign.types)

;;;; Error Conditions

(define-condition overflow-error (error)
  ((value :initarg :value :reader overflow-value)
   (type :initarg :type :reader overflow-type))
  (:report (lambda (c s)
             (format s "Value ~A overflows type ~A"
                     (overflow-value c) (overflow-type c)))))

;;;; Type Information Structure

(defstruct type-info
  name
  size
  alignment
  to-foreign
  from-foreign
  sbcl-type
  base-type
  ;; For compound types
  element-type    ; For arrays/pointers
  return-type      ; For function pointers
  arg-types        ; For function pointers
  )

;;;; Type Registry

(defparameter *type-registry* (make-hash-table :test 'eq))
(defparameter *type-aliases* (make-hash-table :test 'eq))
(defparameter *conversion-cache* (make-hash-table :test 'equal))

(defun get-type-info (type)
  "Get type information for a type keyword"
  (or (gethash type *type-registry*)
      (gethash (resolve-type-alias type) *type-registry*)))

(defun resolve-type-alias (type)
  "Resolve type alias to base type"
  (or (gethash type *type-aliases*) type))

(defun clear-conversion-cache ()
  "Clear the conversion cache"
  (clrhash *conversion-cache*))

;;;; Primitive Type Definitions

(defmacro define-primitive-type (name &key size align sbcl-type
                                       to-foreign from-foreign)
  `(setf (gethash ',name *type-registry*)
         (make-type-info
          :name ',name
          :size ,size
          :alignment ,align
          :sbcl-type ',sbcl-type
          :to-foreign ,(or to-foreign '#'identity)
          :from-foreign ,(or from-foreign '#'identity)
          :base-type ',name)))

;; Define all primitive types
(define-primitive-type :void
  :size 0 :align 1 :sbcl-type sb-alien:void)

(define-primitive-type :char
  :size 1 :align 1 :sbcl-type sb-alien:char)

(define-primitive-type :unsigned-char
  :size 1 :align 1 :sbcl-type sb-alien:unsigned-char)

(define-primitive-type :short
  :size 2 :align 2 :sbcl-type sb-alien:short)

(define-primitive-type :unsigned-short
  :size 2 :align 2 :sbcl-type sb-alien:unsigned-short)

(define-primitive-type :int
  :size 4 :align 4 :sbcl-type sb-alien:int
  :to-foreign (lambda (x)
                (unless (typep x '(signed-byte 32))
                  (error 'overflow-error :value x :type :int))
                x))

(define-primitive-type :unsigned-int
  :size 4 :align 4 :sbcl-type sb-alien:unsigned-int)

(define-primitive-type :long
  :size 8 :align 8 :sbcl-type sb-alien:long)

(define-primitive-type :unsigned-long
  :size 8 :align 8 :sbcl-type sb-alien:unsigned-long)

(define-primitive-type :float
  :size 4 :align 4 :sbcl-type single-float
  :to-foreign (lambda (x) (float x 1.0)))

(define-primitive-type :double
  :size 8 :align 8 :sbcl-type double-float
  :to-foreign (lambda (x) (float x 1.0d0)))

(define-primitive-type :pointer
  :size 8 :align 8 :sbcl-type sb-sys:system-area-pointer
  :to-foreign (lambda (x)
                (etypecase x
                  (sb-sys:system-area-pointer x)
                  (null (sb-sys:int-sap 0))
                  (integer (sb-sys:int-sap x)))))

(define-primitive-type :bool
  :size 1 :align 1 :sbcl-type sb-alien:int
  :to-foreign (lambda (x) (if x 1 0))
  :from-foreign (lambda (x) (not (zerop x))))

(define-primitive-type :string
  :size 8 :align 8 :sbcl-type sb-alien:c-string
  :to-foreign (lambda (x)
                (cond
                  ((null x) (sb-sys:int-sap 0))
                  ((stringp x)
                   (let* ((utf8 (sb-ext:string-to-octets x :external-format :utf-8))
                          (len (length utf8))
                          (ptr (sb-alien:make-alien sb-alien:unsigned-char (1+ len))))
                     (dotimes (i len)
                       (setf (sb-alien:deref ptr i) (aref utf8 i)))
                     (setf (sb-alien:deref ptr len) 0)
                     (sb-alien:alien-sap ptr)))
                  (t (error "Cannot convert ~S to string" x))))
  :from-foreign (lambda (x)
                  (if (sb-sys:sap= x (sb-sys:int-sap 0))
                      nil
                      (sb-alien:cast (sb-alien:sap-alien x (* sb-alien:char))
                                     sb-alien:c-string))))

;;;; Custom Type Definition

(defun define-foreign-type (name &key base-type size alignment
                                  to-foreign from-foreign)
  "Define a custom foreign type"
  (let ((base-info (get-type-info (or base-type :pointer))))
    (setf (gethash name *type-registry*)
          (make-type-info
           :name name
           :size (or size (type-info-size base-info))
           :alignment (or alignment (type-info-alignment base-info))
           :to-foreign (or to-foreign (type-info-to-foreign base-info))
           :from-foreign (or from-foreign (type-info-from-foreign base-info))
           :sbcl-type (type-info-sbcl-type base-info)
           :base-type base-type))))

(defun define-type-alias (alias base-type)
  "Define a type alias"
  (setf (gethash alias *type-aliases*) base-type))

;; Common aliases
(define-type-alias :size-t :unsigned-long)
(define-type-alias :ssize-t :long)
(define-type-alias :ptrdiff-t :long)

;;;; Array Types

(defun define-array-type (name element-type)
  "Define an array type"
  (let ((element-info (get-type-info element-type)))
    (setf (gethash name *type-registry*)
          (make-type-info
           :name name
           :size 8  ; Pointer size
           :alignment 8
           :element-type element-type
           :base-type :pointer
           :sbcl-type 'sb-sys:system-area-pointer))))

(define-array-type :int-array :int)

;;;; Function Pointer Types

(defun define-function-type (name &key return-type arg-types)
  "Define a function pointer type"
  (setf (gethash name *type-registry*)
        (make-type-info
         :name name
         :size 8
         :alignment 8
         :return-type return-type
         :arg-types arg-types
         :base-type :pointer
         :sbcl-type 'sb-sys:system-area-pointer)))

;;;; Type Conversion Functions

(defun convert-to-foreign (value type)
  "Convert a Lisp value to foreign representation"
  (let* ((type-info (get-type-info type))
         (converter (type-info-to-foreign type-info)))
    (if converter
        (funcall converter value)
        value)))

(defun convert-from-foreign (value type)
  "Convert a foreign value to Lisp representation"
  (let* ((type-info (get-type-info type))
         (converter (type-info-from-foreign type-info)))
    (if converter
        (funcall converter value)
        value)))

(defun free-converted-object (object type)
  "Free memory allocated during conversion"
  (when (eq type :string)
    (unless (sb-sys:sap= object (sb-sys:int-sap 0))
      (sb-alien:free-alien (sb-alien:sap-alien object (* sb-alien:char))))))

;;;; Conversion Macros

(defmacro with-c-string ((var string) &body body)
  "Bind VAR to a foreign string for the duration of BODY"
  `(let ((,var (convert-to-foreign ,string :string)))
     (unwind-protect
          (progn ,@body)
       (free-converted-object ,var :string))))

(defmacro with-foreign-values (bindings &body body)
  "Bind multiple foreign values with automatic cleanup"
  (let ((vars (mapcar #'first bindings))
        (vals (mapcar #'second bindings))
        (types (mapcar #'third bindings)))
    `(let ,(mapcar (lambda (var val type)
                     `(,var (convert-to-foreign ,val ,type)))
                   vars vals types)
       (unwind-protect
            (progn ,@body)
         ,@(mapcar (lambda (var type)
                     `(when (member ,type '(:string))
                        (free-converted-object ,var ,type)))
                   vars types)))))

;;;; Type Size and Alignment

(defun sizeof (type)
  "Return the size of a type in bytes"
  (if (and (listp type) (eq (first type) :array))
      (* (sizeof (second type)) (third type))
      (type-info-size (get-type-info type))))

(defun alignof (type)
  "Return the alignment of a type"
  (type-info-alignment (get-type-info type)))

;;;; Type Compatibility

(defun compatible-types-p (type1 type2)
  "Check if two types are compatible"
  (or (eq type1 type2)
      (and (member type1 '(:int :long :short :char))
           (member type2 '(:int :long :short :char)))
      (and (eq type1 :pointer) (eq type2 :pointer))))

;;;; Pointer Operations

(defun pointer-to (type)
  "Create a pointer type to the given type"
  (make-type-info
   :name (list :pointer type)
   :size 8
   :alignment 8
   :element-type type
   :base-type :pointer
   :sbcl-type 'sb-sys:system-area-pointer))

(defun void-pointer-p (type-info)
  "Check if type is a void pointer"
  (and (eq (type-info-base-type type-info) :pointer)
       (eq (type-info-element-type type-info) :void)))

(defun function-pointer-p (type-info)
  "Check if type is a function pointer"
  (and (eq (type-info-base-type type-info) :pointer)
       (type-info-return-type type-info)))

(defun pointer-target-type (type-info)
  "Get the target type of a pointer"
  (type-info-element-type type-info))

(defun function-return-type (type-info)
  "Get the return type of a function pointer"
  (type-info-return-type type-info))

(defun function-arg-types (type-info)
  "Get the argument types of a function pointer"
  (type-info-arg-types type-info))

(defun address-of (object)
  "Get the address of an object"
  (sb-kernel:get-lisp-obj-address object))

(defun dereference (pointer type)
  "Dereference a pointer to get value of given type"
  (let ((type-info (get-type-info type)))
    (case type
      (:int (sb-sys:sap-ref-32 pointer 0))
      (:long (sb-sys:sap-ref-64 pointer 0))
      (:pointer (sb-sys:sap-ref-sap pointer 0))
      (t (error "Cannot dereference type ~A" type)))))

;;;; Handle conversions (placeholder)

(defun handle-to-pointer (handle)
  "Convert handle to pointer"
  handle)

(defun pointer-to-handle (pointer)
  "Convert pointer to handle"
  pointer)