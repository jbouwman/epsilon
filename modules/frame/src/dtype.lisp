(defpackage #:epsilon.frame.dtype
  (:use #:cl)
  (:export
   #:dtype
   #:dtype-p
   #:make-dtype
   #:dtype-name
   #:dtype-size
   #:dtype-type
   #:dtype-array-type
   #:dtype-default
   #:*dtypes*
   #:find-dtype
   #:infer-dtype
   #:coerce-to-dtype))

(in-package #:epsilon.frame.dtype)

(defstruct dtype
  "Data type descriptor for columns"
  (name nil :type symbol)
  (size 0 :type fixnum)
  (type nil :type (or symbol cons))
  (array-type nil :type (or symbol cons))
  (default nil))

(defparameter *dtypes*
  (make-hash-table :test 'eq)
  "Registry of available data types")

(defmacro define-dtype (name size type array-type default)
  "Define a new data type"
  `(setf (gethash ,name *dtypes*)
         (make-dtype :name ,name
                     :size ,size
                     :type ',type
                     :array-type ',array-type
                     :default ,default)))

;; Define standard data types
(define-dtype :bool 1 boolean (simple-array bit (*)) nil)
(define-dtype :int8 1 (signed-byte 8) (simple-array (signed-byte 8) (*)) 0)
(define-dtype :int16 2 (signed-byte 16) (simple-array (signed-byte 16) (*)) 0)
(define-dtype :int32 4 (signed-byte 32) (simple-array (signed-byte 32) (*)) 0)
(define-dtype :int64 8 (signed-byte 64) (simple-array (signed-byte 64) (*)) 0)
(define-dtype :uint8 1 (unsigned-byte 8) (simple-array (unsigned-byte 8) (*)) 0)
(define-dtype :uint16 2 (unsigned-byte 16) (simple-array (unsigned-byte 16) (*)) 0)
(define-dtype :uint32 4 (unsigned-byte 32) (simple-array (unsigned-byte 32) (*)) 0)
(define-dtype :uint64 8 (unsigned-byte 64) (simple-array (unsigned-byte 64) (*)) 0)
(define-dtype :float32 4 single-float (simple-array single-float (*)) 0.0f0)
(define-dtype :float64 8 double-float (simple-array double-float (*)) 0.0d0)
(define-dtype :string 0 string (simple-array t (*)) "")
(define-dtype :any 0 t (simple-array t (*)) nil)

(defun find-dtype (name)
  "Find a dtype by name"
  (or (gethash name *dtypes*)
      (error "Unknown dtype: ~A" name)))

(defun infer-dtype (value)
  "Infer the dtype from a value"
  (typecase value
    (boolean :bool)
    ((integer -2147483648 2147483647) :int32)
    ((integer 0 4294967295) :uint32)
    ((integer * *) :int64)
    (single-float :float32)
    (double-float :float64)
    (string :string)
    (t :any)))

(defun coerce-to-dtype (value dtype-name)
  "Coerce a value to a specific dtype"
  (let ((dtype (find-dtype dtype-name)))
    (case dtype-name
      (:bool (not (null value)))
      ((:int8 :int16 :int32 :int64) 
       (if (numberp value) 
           (truncate value)
           (dtype-default dtype)))
      ((:uint8 :uint16 :uint32 :uint64)
       (if (numberp value)
           (max 0 (truncate value))
           (dtype-default dtype)))
      (:float32 
       (if (numberp value)
           (float value 0.0f0)
           (dtype-default dtype)))
      (:float64
       (if (numberp value)
           (float value 0.0d0)
           (dtype-default dtype)))
      (:string
       (if value
           (princ-to-string value)
           ""))
      (t value))))