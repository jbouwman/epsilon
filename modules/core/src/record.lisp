;;;; Functional record types inspired by Clojure's defrecord
;;;;
;;;; This module provides immutable record types with efficient field access,
;;;; protocol implementation support, and functional update semantics.

(defpackage :epsilon.record
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map)
   (:protocol :epsilon.protocol))
  (:export
   #:defrecord
   #:make-record
   #:record-p
   #:record-type
   #:record-fields
   #:record-get
   #:record-assoc
   #:record-dissoc
   #:record-update
   #:record-merge
   #:record->map
   #:map->record))

(in-package :epsilon.record)

(defclass record-type ()
  ((name :initarg :name
         :reader record-type-name
         :type symbol)
   (fields :initarg :fields
           :reader record-type-fields
           :type list)
   (constructor :initarg :constructor
                :reader record-type-constructor
                :type symbol)
   (predicate :initarg :predicate
              :reader record-type-predicate
              :type symbol)
   (accessors :initarg :accessors
              :reader record-type-accessors
              :type list))
  (:documentation "Metadata for a record type"))

(defvar *record-types* map:+empty+
  "Registry of defined record types")

(defun register-record-type (type)
  "Register a record type"
  (setf *record-types* (map:assoc *record-types*
                                   (record-type-name type)
                                   type)))

(defun find-record-type (name)
  "Find a record type by name"
  (map:get *record-types* name))

(defmacro defrecord (name fields &body options)
  "Define a functional record type with named fields.
   
   Creates an immutable record type with:
   - A constructor function (make-NAME)
   - A type predicate (NAME-p)
   - Field accessors (NAME-FIELD)
   - Functional update operations
   
   Options can include protocol implementations:
   (:implements protocol-name
     (method-name (self ...) body)
     ...)
   
   Example:
   (defrecord person (name age)
     (:implements printable
       (print-object (self stream)
         (format stream \"#<person ~A ~A>\" 
                 (person-name self) 
                 (person-age self)))))"
  (let* ((constructor (intern (format nil "MAKE-~A" name)))
         (predicate (intern (format nil "~A-P" name)))
         (class-name (intern (format nil "~A-CLASS" name)))
         (accessors (mapcar (lambda (field)
                              (intern (format nil "~A-~A" name field)))
                            fields))
         (field-keywords (mapcar (lambda (f) (intern (string f) :keyword))
                                 fields))
         (implementations '()))
    
    ;; Parse options for protocol implementations
    (dolist (option options)
      (when (and (listp option) (eq (first option) :implements))
        (let ((protocol-name (second option))
              (methods (cddr option)))
          (push (cons protocol-name methods) implementations))))
    
    `(progn
       ;; Define the record class
       (defclass ,class-name ()
         ,(mapcar (lambda (field keyword)
                    `(,field :initarg ,keyword
                             :reader ,(intern (format nil "~A-~A" name field))
                             :type t))
                  fields field-keywords)
         (:documentation ,(format nil "Record type ~A" name)))
       
       ;; Register the record type
       (register-record-type
        (make-instance 'record-type
                       :name ',name
                       :fields ',fields
                       :constructor ',constructor
                       :predicate ',predicate
                       :accessors ',accessors))
       
       ;; Constructor function
       (defun ,constructor (&key ,@fields)
         ,(format nil "Create a new ~A record" name)
         (make-instance ',class-name
                        ,@(loop for field in fields
                                for keyword in field-keywords
                                append `(,keyword ,field))))
       
       ;; Type predicate
       (defun ,predicate (obj)
         ,(format nil "Check if OBJ is a ~A record" name)
         (typep obj ',class-name))
       
       ;; Generic record operations
       (defmethod record-p ((obj ,class-name))
         t)
       
       (defmethod record-type ((obj ,class-name))
         ',name)
       
       (defmethod record-fields ((obj ,class-name))
         ',fields)
       
       (defmethod record-get ((obj ,class-name) field)
         (case field
           ,@(mapcar (lambda (field accessor)
                       `(,field (,accessor obj)))
                     fields accessors)
           (otherwise nil)))
       
       
       (defmethod record->map ((obj ,class-name))
         (reduce (lambda (m field)
                   (map:assoc m
                              (intern (string field) :keyword)
                              (record-get obj field)))
                 ',fields
                 :initial-value map:+empty+))
       
       ;; Protocol implementations
       ,@(loop for (protocol-name . methods) in implementations
               append (loop for (method-name lambda-list . body) in methods
                            collect (let* ((typed-lambda-list 
                                            (mapcar (lambda (param)
                                                      (cond 
                                                        ((and (symbolp param) (string= (symbol-name param) "SELF")) 
                                                         `(,param ,class-name))
                                                        ((and (listp param) (string= (symbol-name (first param)) "SELF"))
                                                         `(,(first param) ,class-name ,@(cddr param)))
                                                        (t param)))
                                                    lambda-list)))
                                      `(defmethod ,method-name ,typed-lambda-list
                                         ,@body))))
       
       ;; Return the record type name
       ',name)))

;; Generic functions for record operations

(defgeneric record-p (obj)
  (:documentation "Check if OBJ is a record")
  (:method (obj) nil))

(defgeneric record-type (obj)
  (:documentation "Get the type of a record")
  (:method (obj) nil))

(defgeneric record-fields (obj)
  (:documentation "Get the fields of a record")
  (:method (obj) nil))

(defgeneric record-get (obj field)
  (:documentation "Get the value of FIELD from record OBJ")
  (:method (obj field) nil))

(defgeneric record-assoc (obj &rest kvs)
  (:documentation "Return a new record with updated field values")
  (:method (obj &rest kvs) 
    (declare (ignore kvs))
    obj)
  (:method ((obj standard-object) &rest kvs)
    ;; Generic implementation for all records
    (let* ((type-name (record-type obj)))
      (when type-name
        (let* ((record-type (find-record-type type-name))
               (constructor (record-type-constructor record-type))
               (fields (record-type-fields record-type))
               (args '()))
          ;; Build arguments list with original values
          (dolist (field fields)
            (let ((keyword (intern (string field) :keyword)))
              (push keyword args)
              (push (record-get obj field) args)))
          (setf args (nreverse args))
          ;; Apply updates by replacing values in the args list
          (loop for (k v) on kvs by #'cddr
                do (setf (getf args k) v))
          ;; Create new record
          (apply constructor args))))))

(defgeneric record-dissoc (obj &rest keys)
  (:documentation "Return a new record with fields removed")
  (:method (obj &rest keys) 
    (declare (ignore keys))
    obj)
  (:method ((obj standard-object) &rest keys)
    ;; For records, dissoc just sets fields to nil
    (when (record-type obj)
      (let ((kvs '()))
        (dolist (key keys)
          (push (intern (string key) :keyword) kvs)
          (push nil kvs))
        (if kvs
            (apply #'record-assoc obj (nreverse kvs))
            obj)))))

(defgeneric record-update (obj field fn &rest args)
  (:documentation "Return a new record with FIELD updated by applying FN")
  (:method (obj field fn &rest args) 
    (declare (ignore field fn args))
    obj)
  (:method ((obj standard-object) field fn &rest args)
    (when (record-type obj)
      (let ((new-value (apply fn (record-get obj field) args)))
        (record-assoc obj (intern (string field) :keyword) new-value)))))

(defgeneric record-merge (obj updates)
  (:documentation "Return a new record with fields merged from UPDATES map")
  (:method (obj updates) obj)
  (:method ((obj standard-object) updates)
    (when (record-type obj)
      (if (zerop (map:count updates))
          obj
          (let ((kvs '()))
            (map:each (lambda (k v)
                        (push k kvs)
                        (push v kvs))
                      updates)
            (if kvs
                (apply #'record-assoc obj (nreverse kvs))
                obj))))))

(defgeneric record->map (obj)
  (:documentation "Convert a record to a map")
  (:method (obj) map:+empty+))

(defun map->record (type m)
  "Create a record of TYPE from map M"
  (let ((record-type (find-record-type type)))
    (unless record-type
      (error "Unknown record type: ~S" type))
    (let ((constructor (record-type-constructor record-type))
          (args '()))
      (dolist (field (record-type-fields record-type))
        (let ((keyword (intern (string field) :keyword)))
          (push keyword args)
          (push (map:get m keyword) args)))
      (apply constructor (nreverse args)))))

;; Helper function for creating records
(defun make-record (type &rest field-values)
  "Create a record of TYPE with the given field values"
  (let ((record-type (find-record-type type)))
    (unless record-type
      (error "Unknown record type: ~S" type))
    (apply (record-type-constructor record-type) field-values)))
