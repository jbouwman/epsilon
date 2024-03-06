(in-package #:lib.type)

;;;; types.lisp -- various useful types

;; TODO merge this into lib/type.lisp

(deftype octet ()
  '(unsigned-byte 8))

(deftype index ()
  '(mod #.array-dimension-limit))


;;; Type `octet-vector' and constructors
;;

(deftype octet-vector (&optional (length '*))
  `(array octet (,length)))

(declaim (ftype (function (index
                           &key
                           (:initial-element octet))
                          octet-vector)
                make-octet-vector)
         (inline make-octet-vector))

(defun make-octet-vector (count
                          &key
                          (initial-element 0))
  "Make and return an `octet-vector' with COUNT elements.

If supplied, INITIAL-ELEMENT is used to populate the vector. The value
of INITIAL-ELEMENT has to of type `octet'. "
  (make-array count
              :element-type    'octet
              :initial-element initial-element))

(declaim (ftype (function (&rest octet) octet-vector) octet-vector)
         (inline octet-vector))

(defun octet-vector (&rest args)
  "Make and return an `octet-vector' containing the elements ARGS.
ARGS have to be of type `octet'."
  (make-array (length args)
              :element-type     'octet
              :initial-contents args
              :adjustable       nil
              :fill-pointer     nil))


;;; Type `simple-octet-vector'
;;

(deftype simple-octet-vector (&optional (length '*))
  `(simple-array octet (,length)))
