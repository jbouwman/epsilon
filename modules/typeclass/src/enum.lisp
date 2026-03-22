;;;; epsilon.enum - Enumeration Type Definitions
;;;;
;;;; Provides a defenum macro for defining enumeration types with:
;;;; - Type predicate for validation
;;;; - Keyword-based values
;;;; - String serialization/deserialization
;;;;
;;;; Usage:
;;;;   (defenum pitch-class (:c :cs :d :ds :e :f :fs :g :gs :a :as :b))
;;;;
;;;;   (pitch-class-p :c)        ; => T
;;;;   (pitch-class-p :invalid)  ; => NIL
;;;;   (pitch-class-values)      ; => (:C :CS :D ...)
;;;;   (pitch-class-from-string "c") ; => :C

(defpackage epsilon.enum
  (:use :cl)
  (:export
   #:defenum
   #:enum-values
   #:enum-p
   #:enum-from-string
   #:enum-to-string))

(in-package epsilon.enum)

;;; Registry for enum metadata

(defvar *enum-registry* (make-hash-table :test 'eq)
  "Maps enum type names to their valid values")

(defun register-enum (name values)
  "Register an enum type with its valid values"
  (setf (gethash name *enum-registry*) values))

(defun enum-values (name)
  "Get the valid values for an enum type"
  (gethash name *enum-registry*))

;;; Core defenum macro

(defmacro defenum (name values &key (documentation nil))
  "Define an enumeration type.

   NAME is a symbol naming the enum type.
   VALUES is a list of keyword symbols representing valid values.

   Generates:
   - (NAME-p value) - predicate to check if value is valid
   - (NAME-values) - returns list of valid values
   - (NAME-from-string str) - parse string to enum value
   - (NAME-to-string val) - convert enum value to string

   Example:
     (defenum pitch-class (:c :d :e :f :g :a :b))
     (pitch-class-p :c)  ; => T
     (pitch-class-p :x)  ; => NIL"
  (let ((predicate-name (intern (format nil "~A-P" name)))
        (values-name (intern (format nil "~A-VALUES" name)))
        (from-string-name (intern (format nil "~A-FROM-STRING" name)))
        (to-string-name (intern (format nil "~A-TO-STRING" name)))
        (values-list (if (and (listp values) (every #'keywordp values))
                         values
                         (error "DEFENUM values must be a list of keywords: ~S" values))))
    `(progn
       ;; Register in enum registry
       (register-enum ',name ',values-list)

       ;; Define the type
       (deftype ,name ()
         ,@(when documentation `(,documentation))
         '(member ,@values-list))

       ;; Predicate function
       (defun ,predicate-name (value)
         ,(format nil "Check if VALUE is a valid ~A" name)
         (and (keywordp value)
              (member value ',values-list :test #'eq)
              t))

       ;; Values accessor
       (defun ,values-name ()
         ,(format nil "Return the list of valid ~A values" name)
         ',values-list)

       ;; Parse from string
       (defun ,from-string-name (string)
         ,(format nil "Parse STRING to a ~A value, or NIL if invalid" name)
         (let ((key (intern (string-upcase string) :keyword)))
           (when (member key ',values-list :test #'eq)
             key)))

       ;; Convert to string
       (defun ,to-string-name (value)
         ,(format nil "Convert ~A VALUE to lowercase string" name)
         (when (,predicate-name value)
           (string-downcase (symbol-name value))))

       ;; Return the type name
       ',name)))

;;; Generic enum functions

(defun enum-p (enum-name value)
  "Check if VALUE is valid for enum ENUM-NAME"
  (let ((values (enum-values enum-name)))
    (and values
         (keywordp value)
         (member value values :test #'eq)
         t)))

(defun enum-from-string (enum-name string)
  "Parse STRING to a value for enum ENUM-NAME"
  (let ((values (enum-values enum-name))
        (key (intern (string-upcase string) :keyword)))
    (when (and values (member key values :test #'eq))
      key)))

(defun enum-to-string (value)
  "Convert an enum VALUE (keyword) to lowercase string"
  (when (keywordp value)
    (string-downcase (symbol-name value))))
