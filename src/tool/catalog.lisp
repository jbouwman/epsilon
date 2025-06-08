(defpackage #:epsilon.tool.catalog
  (:use
   #:cl)
  (:local-nicknames
   (#:hex #:epsilon.lib.hex)
   (#:re #:epsilon.lib.regex)
   (#:digest #:epsilon.lib.digest)
   (#:type #:epsilon.lib.type)))

(in-package #:epsilon.tool.catalog)

(defun sha-256 (->u8)
  (let ((digest (lib.digest.sha-2::%make-sha256-digest)))
    (update-digest digest ->u8)
    (produce-digest digest)))

(defclass type-catalog ()
  ((serial :initform (sha-256 (->u8 0)))
   (code :initform (make-array 0 :adjustable t))
   (name :initform (make-hash-table :test #'equal))))

(defmethod print-object ((object catalog) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (serial code) object
      (format stream "serial ~A types ~A"
              (subseq (hex:u8-to-hex serial) 0 12)
              (length code)))))

(defclass type-definition ()
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
    (make-instance 'type-definition
                   :name name
                   :description description
                   :type type)))

(defclass type-definition ()
  ((name :initarg :name)
   (description :initform "")
   (fields :initarg :fields)))

(defun make-type-definition (typedef)
  (destructuring-bind (&key name description type) typedef
    (unless (stringp name)
      (error "invalid type definition: name is not a string: %s" name))
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
      (lib.regex:scan pattern string)
    (if start-excise
        (values (excise-range string start-excise end-excise)
                (subseq string
                        (aref start-group 0)
                        (aref end-group 0)))
        (values string nil))))

(defun parse-reference (ref)
  (let ((segments (re:split ", *" ref)))
    (if (< 1 (length segments))
        (list :type (mapcar #'parse-reference segments)) ; todo structured references later
        (multiple-value-bind (name code)
            (parse-excise ref "\\(([0-9]+)\\)")
          (multiple-value-bind (name seq)
              (parse-excise name "(\\*)")
            (list name (and code (parse-integer code))
                  (if seq t nil)))))))
