(defpackage epsilon.compute.types
  (:use :cl)
  (:export
   ;; Type system
   :compute-type
   :compute-type-p
   :make-compute-type
   :scalar-type
   :vector-type
   :matrix-type
   :tensor-type
   :symbolic-type
   
   ;; Type predicates
   :scalar-p
   :vector-p
   :matrix-p
   :tensor-p
   :symbolic-p
   
   ;; Type constructors
   :make-scalar
   :make-vector
   :make-matrix
   :make-tensor
   
   ;; Shape operations
   :shape
   :rank
   :size
   :reshape
   :broadcast-shapes
   
   ;; Type inference
   :infer-type
   :unify-types
   :promote-type))

(in-package epsilon.compute.types)

;;; Type definitions

(defstruct compute-type
  "Base type for all compute types"
  (kind :scalar :type keyword)
  (dtype :float64 :type keyword)
  (shape nil :type list))

(defun scalar-type (&optional (dtype :float64))
  "Create a scalar type"
  (make-compute-type :kind :scalar :dtype dtype :shape '()))

(defun vector-type (size &optional (dtype :float64))
  "Create a vector type"
  (make-compute-type :kind :vector :dtype dtype :shape (list size)))

(defun matrix-type (rows cols &optional (dtype :float64))
  "Create a matrix type"
  (make-compute-type :kind :matrix :dtype dtype :shape (list rows cols)))

(defun tensor-type (shape &optional (dtype :float64))
  "Create a tensor type"
  (make-compute-type :kind :tensor :dtype dtype :shape shape))

(defun symbolic-type ()
  "Create a symbolic type"
  (make-compute-type :kind :symbolic :dtype :symbolic :shape nil))

;;; Type predicates

(defun scalar-p (type)
  "Check if type is scalar"
  (and (compute-type-p type)
       (eq (compute-type-kind type) :scalar)))

(defun vector-p (type)
  "Check if type is vector"
  (and (compute-type-p type)
       (eq (compute-type-kind type) :vector)))

(defun matrix-p (type)
  "Check if type is matrix"
  (and (compute-type-p type)
       (eq (compute-type-kind type) :matrix)))

(defun tensor-p (type)
  "Check if type is tensor"
  (and (compute-type-p type)
       (eq (compute-type-kind type) :tensor)))

(defun symbolic-p (type)
  "Check if type is symbolic"
  (and (compute-type-p type)
       (eq (compute-type-kind type) :symbolic)))

;;; Shape operations

(defun shape (type)
  "Get shape of a compute type"
  (compute-type-shape type))

(defun rank (type)
  "Get rank (number of dimensions) of a compute type"
  (length (compute-type-shape type)))

(defun size (type)
  "Get total number of elements in a compute type"
  (reduce #'* (compute-type-shape type) :initial-value 1))

(defun reshape (type new-shape)
  "Create a new type with different shape but same total size"
  (let ((old-size (size type))
        (new-size (reduce #'* new-shape :initial-value 1)))
    (unless (= old-size new-size)
      (error "Cannot reshape from size ~A to size ~A" old-size new-size))
    (make-compute-type :kind (cond ((null new-shape) :scalar)
                                   ((= (length new-shape) 1) :vector)
                                   ((= (length new-shape) 2) :matrix)
                                   (t :tensor))
                      :dtype (compute-type-dtype type)
                      :shape new-shape)))

(defun broadcast-shapes (shape1 shape2)
  "Compute the broadcast shape of two shapes"
  (let ((rank1 (length shape1))
        (rank2 (length shape2)))
    (cond
      ((null shape1) shape2)
      ((null shape2) shape1)
      (t
       (let* ((max-rank (max rank1 rank2))
              (padded1 (append (make-list (- max-rank rank1) :initial-element 1) shape1))
              (padded2 (append (make-list (- max-rank rank2) :initial-element 1) shape2))
              (result nil))
         (loop for d1 in padded1
               for d2 in padded2
               do (cond
                    ((= d1 d2) (push d1 result))
                    ((= d1 1) (push d2 result))
                    ((= d2 1) (push d1 result))
                    (t (error "Incompatible shapes for broadcasting: ~A and ~A" shape1 shape2))))
         (nreverse result))))))

;;; Type inference

(defun infer-type (op &rest arg-types)
  "Infer the result type of an operation"
  (case op
    ((+ - * /)
     (let ((shapes (mapcar #'shape arg-types))
           (dtypes (mapcar #'compute-type-dtype arg-types)))
       (let ((result-shape (reduce #'broadcast-shapes shapes))
             (result-dtype (reduce #'promote-dtype dtypes)))
         (cond
           ((null result-shape) (scalar-type result-dtype))
           ((= (length result-shape) 1) (vector-type (first result-shape) result-dtype))
           ((= (length result-shape) 2) (apply #'matrix-type (append result-shape (list result-dtype))))
           (t (tensor-type result-shape result-dtype))))))
    ((sin cos tan exp log sqrt abs)
     (first arg-types))
    (transpose
     (let ((type (first arg-types)))
       (if (matrix-p type)
           (let ((shape (compute-type-shape type)))
             (matrix-type (second shape) (first shape) (compute-type-dtype type)))
           type)))
    (otherwise
     (symbolic-type))))

(defun promote-dtype (dtype1 dtype2)
  "Promote two dtypes to their common type"
  (cond
    ((eq dtype1 :symbolic) :symbolic)
    ((eq dtype2 :symbolic) :symbolic)
    ((eq dtype1 :complex128) :complex128)
    ((eq dtype2 :complex128) :complex128)
    ((eq dtype1 :complex64) :complex64)
    ((eq dtype2 :complex64) :complex64)
    ((eq dtype1 :float64) :float64)
    ((eq dtype2 :float64) :float64)
    ((eq dtype1 :float32) :float32)
    ((eq dtype2 :float32) :float32)
    (t :float64)))

(defun unify-types (&rest types)
  "Unify multiple types into a common type"
  (reduce (lambda (t1 t2)
            (infer-type '+ t1 t2))
          types))