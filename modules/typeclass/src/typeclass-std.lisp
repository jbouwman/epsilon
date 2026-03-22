;;;; Standard type classes for Epsilon
;;;;
;;;; Defines the common vocabulary of type classes:
;;;;   show       - String representation
;;;;   eq-class   - Equality comparison
;;;;   ord        - Total ordering (requires eq-class)
;;;;   hash-class - Hash values for use in hash maps
;;;;   serialize  - Encoding/decoding for wire formats
;;;;
;;;; Also installs ADT dispatch methods that consult the data module's
;;;; dispatch tables for derived instances.

(defpackage :epsilon.typeclass.std
  (:use :cl)
  (:local-nicknames
   (:tc :epsilon.typeclass)
   (:data :epsilon.data))
  (:export
   ;; Type class names (for deriving references)
   #:show
   #:eq-class
   #:ord
   #:hash-class
   #:serialize
   ;; Method names
   #:show-value
   #:equal-p
   #:compare
   #:hash-value
   #:encode
   #:decode
   ;; Deriving support (shared by defrecord and defstruct+)
   #:generate-deriving-instances))

(in-package :epsilon.typeclass.std)

;;; ============================================================
;;; show - String representation
;;; ============================================================

(tc:deftypeclass show ()
  "Types with a string representation."
  (show-value (object) "Return a human-readable string representation."))

;;; Built-in instances

(tc:definstance show number
  (show-value (n) (princ-to-string n)))

(tc:definstance show string
  (show-value (s) s))

(tc:definstance show symbol
  (show-value (s) (symbol-name s)))

(tc:definstance show character
  (show-value (c) (string c)))

(tc:definstance show list
  (show-value (xs)
    (format nil "(~{~A~^ ~})" (mapcar #'show-value xs))))

(tc:definstance show vector
  (show-value (v)
    (format nil "#(~{~A~^ ~})"
            (loop for x across v collect (show-value x)))))

;;; ADT dispatch: consult *adt-show-table* for registered data types
(defmethod show-value ((obj data:adt-instance))
  (if (gethash (data:adt-instance-type-name obj) data:*adt-show-table*)
      (let ((tag (data:adt-instance-tag obj))
            (vals (data:adt-instance-values obj)))
        (if (null vals)
            (format nil "#<~A>" tag)
            (format nil "#<~A ~{~A~^ ~}>" tag (mapcar #'show-value vals))))
      (format nil "#<~A ~A>" (data:adt-instance-type-name obj)
              (data:adt-instance-tag obj))))

;;; ============================================================
;;; eq-class - Equality
;;; ============================================================

(tc:deftypeclass eq-class ()
  "Types with an equality relation."
  (equal-p (a b) "Return T if A and B are considered equal."))

;;; Built-in instances

(tc:definstance eq-class number
  (equal-p (a b) (= a b)))

(tc:definstance eq-class string
  (equal-p (a b) (string= a b)))

(tc:definstance eq-class symbol
  (equal-p (a b) (eq a b)))

(tc:definstance eq-class character
  (equal-p (a b) (char= a b)))

(tc:definstance eq-class list
  (equal-p (a b)
    (and (= (length a) (length b))
         (every #'equal-p a b))))

;;; ADT dispatch: structural equality for registered data types
(defmethod equal-p ((a data:adt-instance) (b data:adt-instance))
  (let ((type-a (data:adt-instance-type-name a)))
    (and (eq type-a (data:adt-instance-type-name b))
         (gethash type-a data:*adt-equal-table*)
         (eq (data:adt-instance-tag a) (data:adt-instance-tag b))
         (let ((vals-a (data:adt-instance-values a))
               (vals-b (data:adt-instance-values b)))
           (and (= (length vals-a) (length vals-b))
                (every #'equal-p vals-a vals-b))))))

;;; ============================================================
;;; ord - Total ordering (superclass: eq-class)
;;; ============================================================

(tc:deftypeclass ord (eq-class)
  "Totally ordered types. Returns :lt, :eq, or :gt."
  (compare (a b) "Compare A and B. Return :lt, :eq, or :gt."))

;;; Built-in instances

(tc:definstance ord number
  (compare (a b)
    (cond ((< a b) :lt)
          ((= a b) :eq)
          (t :gt))))

(tc:definstance ord string
  (compare (a b)
    (cond ((string< a b) :lt)
          ((string= a b) :eq)
          (t :gt))))

(tc:definstance ord symbol
  (compare (a b)
    (cond ((string< (symbol-name a) (symbol-name b)) :lt)
          ((eq a b) :eq)
          (t :gt))))

(tc:definstance ord character
  (compare (a b)
    (cond ((char< a b) :lt)
          ((char= a b) :eq)
          (t :gt))))

;;; ============================================================
;;; hash-class - Hash values
;;; ============================================================

(tc:deftypeclass hash-class ()
  "Types with a hash function for use in hash-based data structures."
  (hash-value (object) "Return a fixnum hash for OBJECT."))

;;; Built-in instances

(tc:definstance hash-class number
  (hash-value (n) (sxhash n)))

(tc:definstance hash-class string
  (hash-value (s) (sxhash s)))

(tc:definstance hash-class symbol
  (hash-value (s) (sxhash s)))

(tc:definstance hash-class character
  (hash-value (c) (sxhash c)))

(tc:definstance hash-class list
  (hash-value (xs)
    (reduce #'logxor xs :key #'hash-value :initial-value 0)))

;;; ============================================================
;;; serialize - Encoding/decoding for wire formats
;;; ============================================================

(tc:deftypeclass serialize ()
  "Format-specific serialization. First argument is a format keyword
   (e.g., :json, :msgpack). Use with EQL specializers:
   (definstance serialize (eql :json) ...)"
  (encode (format object) "Encode OBJECT into the given FORMAT.")
  (decode (format data) "Decode DATA from the given FORMAT."))

;;; ============================================================
;;; Deriving support (shared by defrecord and defstruct+)
;;; ============================================================

(defun generate-deriving-instances (name class-name fields accessors deriving-list)
  "Generate definstance forms for derived type class instances.
   NAME is the display name symbol, CLASS-NAME is the type for dispatch,
   FIELDS is the list of field symbols, ACCESSORS is the list of accessor symbols.
   Returns a list of forms to be spliced into a macro expansion."
  (let ((forms nil))
    (dolist (tc-name deriving-list)
      (let ((tc-str (symbol-name tc-name)))
        (cond
          ((string= tc-str "SHOW")
           (push `(tc:definstance ,tc-name ,class-name
                    (show-value (obj)
                      (format nil "#<~A~{ ~A=~S~}>" ',name
                              (list ,@(loop for field in fields
                                            for accessor in accessors
                                            append `(,(symbol-name field)
                                                     (,accessor obj)))))))
                 forms))
          ((string= tc-str "EQ-CLASS")
           (push `(tc:definstance ,tc-name ,class-name
                    (equal-p (a b)
                      (and ,@(mapcar (lambda (acc)
                                       `(equal-p (,acc a) (,acc b)))
                                     accessors))))
                 forms))
          ((string= tc-str "HASH-CLASS")
           (push `(tc:definstance ,tc-name ,class-name
                    (hash-value (obj)
                      (logxor ,@(mapcar (lambda (acc)
                                          `(hash-value (,acc obj)))
                                        accessors))))
                 forms))
          ((string= tc-str "ORD")
           ;; Lexicographic ordering over fields
           (push `(tc:definstance ,tc-name ,class-name
                    (compare (a b)
                      ,@(if (null accessors)
                            '(:eq)
                            (list (reduce
                                   (lambda (acc-form rest-form)
                                     `(let ((c ,acc-form))
                                        (if (eq c :eq) ,rest-form c)))
                                   (mapcar (lambda (acc)
                                             `(compare (,acc a) (,acc b)))
                                           accessors)
                                   :from-end t)))))
                 forms))
          (t
           (error "Cannot derive ~A for ~A: unknown type class" tc-name name)))))
    (nreverse forms)))
