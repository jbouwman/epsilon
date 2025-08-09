(defpackage #:epsilon.tool.catalog
  (:use
   #:cl
   #:epsilon.type)
  (:local-nicknames
   (#:hex #:epsilon.hex)
   (#:re #:epsilon.regex)
   (#:seq #:epsilon.sequence)
   (#:digest #:epsilon.digest)
   (#:str #:epsilon.string))
  (:export
   ;; Classes
   #:type-catalog
   #:type-definition
   #:type-field
   
   ;; Functions
   #:make-type-definition
   #:make-type-field
   #:parse-reference
   
   ;; Utilities
   #:sha-256))

(in-package #:epsilon.tool.catalog)

(defun sha-256 (data)
  (let ((digest (digest:make-digest :sha-256)))
    (etypecase data
      (string 
       (let ((bytes (->u8 (map 'vector #'char-code data))))
         (epsilon.digest.generic:update-digest digest bytes :start 0 :end (length bytes))))
      ((simple-array (unsigned-byte 8) (*))
       (epsilon.digest.generic:update-digest digest data :start 0 :end (length data)))
      (vector 
       (let ((bytes (->u8 data)))
         (epsilon.digest.generic:update-digest digest bytes :start 0 :end (length bytes))))
      (integer 
       (let ((bytes (->u8 (make-array 1 :initial-element data))))
         (epsilon.digest.generic:update-digest digest bytes :start 0 :end (length bytes)))))
    (digest:get-digest digest)))

(defclass type-catalog ()
  ((serial :initform (sha-256 0))
   (code :initform (make-array 0 :adjustable t))
   (name :initform (make-hash-table :test #'equal))))

(defmethod print-object ((object type-catalog) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (serial code) object
      (format stream "serial ~A types ~A"
              (subseq (hex:u8-to-hex serial) 0 12)
              (length code)))))

(defclass type-field ()
  ((name :initarg :name)
   (description :initarg :description
                :initform "")
   (type :initarg :type)))

(defun make-type-field (field-definition)
  (destructuring-bind (&key name description type) field-definition
    (unless (stringp name)
      (error "invalid field definition: name is not a string: ~A" name))
    (unless (stringp type)
      (error "invalid field definition: type is not a string: ~A" type))
    (make-instance 'type-field
                   :name name
                   :description description
                   :type type)))

(defclass type-definition ()
  ((name :initarg :name)
   (description :initarg :description
                :initform "")
   (fields :initarg :fields)))

(defun make-type-definition (typedef)
  (destructuring-bind (&key name description fields) typedef
    (unless (stringp name)
      (error "invalid type definition: name is not a string: ~A" name))
    (make-instance 'type-definition
                   :name name
                   :description description
                   :fields (mapcar #'make-type-field fields))))

(defun excise-range (seq a b)
  (concatenate 'string
               (subseq seq 0 a)
               (subseq seq b (length seq))))

(defun parse-excise (string pattern)
  (multiple-value-bind (start-excise end-excise start-group end-group)
      (re:scan pattern string)
    (if start-excise
        (values (excise-range string start-excise end-excise)
                (subseq string
                        (aref start-group 0)
                        (aref end-group 0)))
        (values string nil))))

(defun parse-reference (ref)
  (let ((segments (seq:realize
                   (seq:map (lambda (s)
                              (string-trim '(#\Space) s))
                            (str:split #\, ref)))))
    (if (< 1 (length segments))
        (list :type (mapcar #'parse-reference segments)) ; todo structured references later
        (multiple-value-bind (name code)
            (parse-excise ref "\\(([0-9]+)\\)")
          (multiple-value-bind (name seq)
              (parse-excise name "(\\*)")
            (list name (and code (parse-integer code))
                  (if seq t nil)))))))
