;;;; data.lisp - Algebraic data types
;;;;
;;;; Provides sum types (tagged unions) with constructors, predicates,
;;;; and accessors. Integrates with pattern matching.

(defpackage :epsilon.data
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map)
   (:mmap :epsilon.mutable-map))
  (:export
   #:defdata
   #:variant-p
   #:variant-type
   #:variant-tag
   #:variant-values
   #:list-variants
   #:adt-instance
   #:adt-instance-p
   #:adt-instance-tag
   #:adt-instance-values
   #:adt-instance-type-name
   ;; Deriving support dispatch tables
   #:*adt-show-table*
   #:*adt-equal-table*
   #:register-adt-show
   #:register-adt-equal))

(in-package :epsilon.data)

;;; Type registry

(defvar *data-types* map:+empty+
  "Registry of defined data types")

(defclass data-type ()
  ((name :initarg :name :reader data-type-name)
   (type-params :initarg :type-params :reader data-type-params)
   (variants :initarg :variants :reader data-type-variants)
   (documentation :initarg :documentation :reader data-type-documentation))
  (:documentation "Metadata for an algebraic data type"))

(defclass variant ()
  ((name :initarg :name :reader variant-name)
   (fields :initarg :fields :reader variant-fields)
   (parent-type :initarg :parent-type :reader variant-parent-type))
  (:documentation "A variant (constructor) of a data type"))

;;; Runtime representation

(defstruct (adt-instance (:constructor make-adt-instance (tag values type-name)))
  "Runtime representation of an ADT value"
  tag
  values
  type-name)

(defmethod print-object ((obj adt-instance) stream)
  (if *print-readably*
      (call-next-method)
      (let ((tag (adt-instance-tag obj))
            (values (adt-instance-values obj)))
        (if (null values)
            (format stream "#<~A>" tag)
            (format stream "#<~A ~{~S~^ ~}>" tag values)))))

;;; Public API

(defun variant-p (obj)
  "Check if OBJ is an ADT variant instance"
  (adt-instance-p obj))

(defun variant-type (obj)
  "Get the type name of a variant"
  (when (adt-instance-p obj)
    (adt-instance-type-name obj)))

(defun variant-tag (obj)
  "Get the tag (constructor name) of a variant"
  (when (adt-instance-p obj)
    (adt-instance-tag obj)))

(defun variant-values (obj)
  "Get the values of a variant as a list"
  (when (adt-instance-p obj)
    (adt-instance-values obj)))

(defun list-variants (type-name)
  "List all variant constructors for a type"
  (let ((data-type (map:get *data-types* type-name)))
    (when data-type
      (mapcar #'variant-name (data-type-variants data-type)))))

;;; Code generation helpers

(defun generate-constructor (type-name variant-name fields)
  "Generate a constructor function for a variant"
  (let ((field-params (loop for i from 0 below (length fields)
                            collect (intern (format nil "ARG~D" i)))))
    (if (null fields)
        ;; Nullary constructor - create a singleton or factory
        `(defun ,variant-name ()
           ,(format nil "Construct a ~A variant of type ~A" variant-name type-name)
           (make-adt-instance ',variant-name nil ',type-name))
        ;; Constructor with parameters
        `(defun ,variant-name ,field-params
           ,(format nil "Construct a ~A variant of type ~A" variant-name type-name)
           (make-adt-instance ',variant-name (list ,@field-params) ',type-name)))))

(defun generate-predicate (type-name variant-name)
  "Generate a predicate for a variant"
  (declare (ignore type-name))
  (let ((pred-name (intern (format nil "~A-P" variant-name))))
    `(defun ,pred-name (obj)
       ,(format nil "Check if OBJ is a ~A variant" variant-name)
       (and (adt-instance-p obj)
            (eq (adt-instance-tag obj) ',variant-name)))))

(defun generate-type-predicate (type-name variants)
  "Generate a predicate for the entire type"
  (declare (ignore variants))
  (let ((pred-name (intern (format nil "~A-P" type-name))))
    `(defun ,pred-name (obj)
       ,(format nil "Check if OBJ is a ~A" type-name)
       (and (adt-instance-p obj)
            (eq (adt-instance-type-name obj) ',type-name)))))

(defun generate-accessors (variant-name fields)
  "Generate accessor functions for variant fields"
  (let ((seen-fields (mmap:make-map))
        (accessors '()))
    (loop for field in fields
          for i from 0
          for field-str = (if (and (symbolp field) (not (listp field)))
                              (symbol-name field)
                              nil)
          for count = (if field-str
                         (progn
                           (mmap:update! seen-fields field-str #'1+ 0)
                           (mmap:get seen-fields field-str))
                         1)
          for accessor-name = (cond
                                ;; First occurrence of named field: name-field
                                ((and field-str (= count 1))
                                 (intern (format nil "~A-~A" variant-name field)))
                                ;; Subsequent occurrences: name-field0, name-field1, etc.
                                (field-str
                                 (intern (format nil "~A-~A~D" variant-name field (- count 2))))
                                ;; No field name, use index
                                (t
                                 (intern (format nil "~A-~D" variant-name i))))
          do (push `(defun ,accessor-name (obj)
                      ,(format nil "Access field ~D of ~A variant" i variant-name)
                      (unless (and (adt-instance-p obj)
                                   (eq (adt-instance-tag obj) ',variant-name))
                        (error "Expected ~A, got ~S" ',variant-name obj))
                      (nth ,i (adt-instance-values obj)))
                   accessors))
    (nreverse accessors)))

;;; Deriving support for algebraic data types
;;;
;;; ADTs use a struct-based runtime representation (adt-instance), so CLOS
;;; dispatch cannot distinguish different data types. Instead, we use a
;;; dispatch table keyed by type-name, and install a single method on
;;; adt-instance that consults the table.

(defvar *adt-show-table* (make-hash-table :test 'eq)
  "Dispatch table for show-value on ADT instances, keyed by type-name symbol.")

(defvar *adt-equal-table* (make-hash-table :test 'eq)
  "Dispatch table for equal-p on ADT instances, keyed by type-name symbol.")

(defun register-adt-show (type-name)
  "Register that TYPE-NAME ADTs support show-value via structural display."
  (setf (gethash type-name *adt-show-table*) t))

(defun register-adt-equal (type-name)
  "Register that TYPE-NAME ADTs support equal-p via structural equality."
  (setf (gethash type-name *adt-equal-table*) t))

(defun generate-data-deriving-forms (type-name deriving-list)
  "Generate registration forms for derived type class behavior on data types.
   TYPE-NAME is the ADT name. Returns a list of forms that register
   the type in the appropriate dispatch tables."
  (let ((forms nil))
    (dolist (tc-name deriving-list)
      (let ((tc-str (symbol-name tc-name)))
        (cond
          ((string= tc-str "SHOW")
           (push `(register-adt-show ',type-name) forms))
          ((string= tc-str "EQ-CLASS")
           (push `(register-adt-equal ',type-name) forms))
          (t
           (error "Cannot derive ~A for data type ~A: unknown type class"
                  tc-name type-name)))))
    (nreverse forms)))

;;; defdata macro

(defmacro defdata (name params &body variants-and-options)
  "Define an algebraic data type.

   Syntax:
   (defdata name (type-params...)
     \"Optional documentation\"
     (variant-name field-type...)
     ...)

   Supports (:deriving show eq-class) for automatic instance generation.

   Examples:
   (defdata maybe (a)
     \"Optional value\"
     (just a)
     (nothing))

   (defdata tree (a)
     \"Binary tree type\"
     (leaf a)
     (branch (tree a) (tree a)))

   (defdata json ()
     (json-null)
     (json-bool boolean)
     (json-number number)
     (json-string string)
     (json-array list)
     (json-object map))"
  (let* ((doc (when (stringp (first variants-and-options))
                (first variants-and-options)))
         (rest-body (if doc (rest variants-and-options) variants-and-options))
         ;; Separate deriving clause from variant definitions
         (deriving-list nil)
         (variants (loop for item in rest-body
                         if (and (listp item) (eq (first item) :deriving))
                           do (setf deriving-list (rest item))
                         else
                           collect item))
         (type-params (if (listp params) params nil)))

    `(progn
       ;; Register type metadata
       (setf *data-types*
             (map:assoc *data-types* ',name
                        (make-instance 'data-type
                                       :name ',name
                                       :type-params ',type-params
                                       :documentation ,doc
                                       :variants (list ,@(loop for variant in variants
                                                               collect `(make-instance 'variant
                                                                                       :name ',(first variant)
                                                                                       :fields ',(rest variant)
                                                                                       :parent-type ',name))))))

       ;; Generate constructors for each variant
       ,@(loop for variant in variants
               for variant-name = (first variant)
               for fields = (rest variant)
               collect (generate-constructor name variant-name fields))

       ;; Generate predicates for each variant
       ,@(loop for variant in variants
               for variant-name = (first variant)
               collect (generate-predicate name variant-name))

       ;; Generate type predicate
       ,(generate-type-predicate name variants)

       ;; Generate accessors for each variant's fields
       ,@(loop for variant in variants
               for variant-name = (first variant)
               for fields = (rest variant)
               append (generate-accessors variant-name fields))

       ;; Derived type class support (dispatch table registration)
       ,@(when deriving-list
           (generate-data-deriving-forms name deriving-list))

       ;; Return type name
       ',name)))
