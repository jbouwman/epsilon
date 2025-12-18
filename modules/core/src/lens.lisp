;;;; lens.lisp - Functional lenses for immutable updates
;;;;
;;;; Lenses provide composable accessors for nested immutable data structures.
;;;; They separate the "where" from the "what" of data manipulation.

(defpackage :epsilon.lens
  (:use :cl)
  (:shadow #:set)
  (:local-nicknames
   (:map :epsilon.map)
   (:th :epsilon.threading)
   (:eset :epsilon.set))
  (:export
   ;; Lens type
   #:lens
   #:make-lens
   #:lens-p
   #:lens-getter
   #:lens-setter

   ;; Core operations
   #:view
   #:set
   #:over

   ;; Composition
   #:compose
   #:>>>
   #:<<<

   ;; Macro for defining lenses
   #:deflens

   ;; Common lenses
   #:first-lens
   #:second-lens
   #:nth-lens
   #:key-lens
   #:at

   ;; Record lenses
   #:field-lens

   ;; Traversals
   #:each-lens
   #:filtered-lens

   ;; Additional lenses
   #:car-lens
   #:cdr-lens
   #:identity-lens
   #:aref-lens
   #:slot-lens

   ;; Utilities
   #:zoom))

(in-package :epsilon.lens)

;;; Core lens type

(defstruct (lens (:constructor %make-lens))
  "A lens focuses on a part of a data structure.
   GETTER: S -> A (extract the focused value)
   SETTER: A -> S -> S (set the focused value, returning new structure)
   OVER-FN: optional (F S) -> S function for traversals"
  getter
  setter
  over-fn)

(defun make-lens (getter setter &optional over-fn)
  "Create a lens with getter and setter, optional over function for traversals"
  (%make-lens :getter getter :setter setter :over-fn over-fn))

;;; Core operations

(defun view (lens structure)
  "View the focused value through a lens"
  (funcall (lens-getter lens) structure))

(defun set (lens value structure)
  "Set the focused value through a lens, returning new structure"
  (funcall (lens-setter lens) value structure))

(defun over (lens f structure)
  "Apply F to the focused value through a lens, returning new structure"
  (if (lens-over-fn lens)
      ;; Traversal lens - use custom over function
      (funcall (lens-over-fn lens) f structure)
      ;; Normal lens - apply f to focused value and set
      (set lens (funcall f (view lens structure)) structure)))

;;; Composition

(defun compose (&rest lenses)
  "Compose lenses left-to-right (outer to inner).
   (compose a b) focuses on b within a.
   (view (compose address-lens city-lens) person) gets the city from the address."
  (if (null lenses)
      (make-lens #'identity (lambda (v s) (declare (ignore s)) v))
      (reduce (lambda (outer inner)
                (make-lens
                 ;; Getter: compose getters
                 (lambda (s)
                   (view inner (view outer s)))
                 ;; Setter: update through both
                 (lambda (v s)
                   (over outer
                         (lambda (inner-s)
                           (set inner v inner-s))
                         s))))
              lenses)))

(defun >>> (&rest lenses)
  "Alias for compose - left-to-right lens composition"
  (apply #'compose lenses))

(defun <<< (&rest lenses)
  "Right-to-left lens composition"
  (apply #'compose (reverse lenses)))

;;; Macro for defining lenses

(defmacro deflens (name args &key get set)
  "Define a named lens function.

   Example:
   (deflens person-name-lens (person)
     :get (person-name person)
     :set (record-assoc person :name value))"
  (let ((structure (first args))
        (value-var (gensym "VALUE")))
    `(defun ,name ()
       (make-lens
        (lambda (,structure) ,get)
        (lambda (,value-var ,structure)
          (declare (ignorable ,value-var))
          (let ((,(intern "VALUE" (symbol-package name)) ,value-var))
            ,set))))))

;;; Common lenses

(defun first-lens ()
  "Lens focusing on the first element of a list"
  (make-lens
   #'first
   (lambda (v s) (cons v (rest s)))))

(defun second-lens ()
  "Lens focusing on the second element of a list"
  (make-lens
   #'second
   (lambda (v s) (list* (first s) v (cddr s)))))

(defun nth-lens (n)
  "Lens focusing on the nth element of a list"
  (make-lens
   (lambda (s) (nth n s))
   (lambda (v s)
     (let ((result (copy-list s)))
       (setf (nth n result) v)
       result))))

(defun key-lens (key &optional default)
  "Lens focusing on a map key"
  (make-lens
   (lambda (m) (map:get m key default))
   (lambda (v m) (map:assoc m key v))))

(defun at (key &optional default)
  "Alias for key-lens - focus on a map key"
  (key-lens key default))

;;; Record lenses

(defun field-lens (accessor-fn updater-fn)
  "Create a lens for a record field.
   ACCESSOR-FN is the record's field accessor (getter).
   UPDATER-FN is a function (value record) -> new-record.

   Example:
   (field-lens #'person-name
               (lambda (v p) (record-assoc p :name v)))"
  (make-lens
   accessor-fn
   updater-fn))

;;; Traversals (lenses that focus on multiple values)

(defun each-lens ()
  "Traversal lens that focuses on each element of a list or set.
   view returns the whole collection.
   over applies function to each element."
  (make-lens
   #'identity  ; view returns whole collection
   (lambda (v s)
     (declare (ignore v s))
     (error "each-lens does not support set, use over instead"))
   ;; over-fn: apply function to each element
   (lambda (f s)
     (etypecase s
       (list (mapcar f s))
       (eset:hamt-set (eset:map s f))))))

(defun filtered-lens (pred)
  "Traversal that focuses on elements matching PRED.
   view returns matching elements.
   over applies function to matching elements only."
  (make-lens
   (lambda (s)
     (etypecase s
       (list (remove-if-not pred s))
       (eset:hamt-set (eset:filter pred s))))
   (lambda (v s)
     (declare (ignore v s))
     (error "filtered-lens does not support set, use over instead"))
   ;; over-fn: apply function only to matching elements
   (lambda (f s)
     (etypecase s
       (list (mapcar (lambda (x)
                       (if (funcall pred x)
                           (funcall f x)
                           x))
                     s))
       (eset:hamt-set (eset:map s (lambda (x)
                                    (if (funcall pred x)
                                        (funcall f x)
                                        x))))))))

;;; Utilities

(defmacro zoom (lens-expr structure &body operations)
  "Focus on a part of STRUCTURE and apply OPERATIONS.
   The first form in OPERATIONS is applied to the focused value.

   Example:
   (zoom (>>> (at :address) (at :city)) person
     string-upcase)"
  (let ((lens-var (gensym "LENS"))
        (struct-var (gensym "STRUCT")))
    `(let ((,lens-var ,lens-expr)
           (,struct-var ,structure))
       (over ,lens-var
             (lambda (focused)
               (declare (ignorable focused))
               ,(if (= 1 (length operations))
                    `(funcall #',(first operations) focused)
                    `(th:-> focused ,@operations)))
             ,struct-var))))

;;; Additional convenience lenses

(defun car-lens ()
  "Lens focusing on the car of a cons cell"
  (make-lens
   #'car
   (lambda (v s) (cons v (cdr s)))))

(defun cdr-lens ()
  "Lens focusing on the cdr of a cons cell"
  (make-lens
   #'cdr
   (lambda (v s) (cons (car s) v))))

(defun identity-lens ()
  "Identity lens - focuses on the whole structure"
  (make-lens
   #'identity
   (lambda (v s) (declare (ignore s)) v)))

(defun aref-lens (&rest indices)
  "Lens focusing on an array element at INDICES"
  (make-lens
   (lambda (arr) (apply #'aref arr indices))
   (lambda (v arr)
     (let ((result (copy-seq arr)))
       (setf (apply #'aref result indices) v)
       result))))

(defun slot-lens (slot-name)
  "Lens focusing on a CLOS slot"
  (make-lens
   (lambda (obj) (slot-value obj slot-name))
   (lambda (v obj)
     ;; Create a shallow copy and update the slot
     ;; Note: This only works for simple structs/classes
     (let ((copy (copy-structure obj)))
       (setf (slot-value copy slot-name) v)
       copy))))
