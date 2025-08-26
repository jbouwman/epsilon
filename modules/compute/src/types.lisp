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
   :create-tensor
   
   ;; Shape operations
   :shape
   :rank
   :size
   :reshape
   :broadcast-shapes
   
   ;; Type inference
   :infer-type
   :unify-types
   :promote-type
   
   ;; Enhanced shape inference
   :infer-shape
   :compatible-shapes-p
   :broadcast-compatible-p
   :matmul-shape
   :einstein-shape
   :reduce-shape
   
   ;; Broadcasting metadata
   :broadcast-info
   :broadcast-info-p
   :make-broadcast-info
   :broadcast-info-operand-shapes
   :broadcast-info-result-shape
   :broadcast-info-stride-patterns
   :broadcast-info-dimension-mapping
   :broadcast-info-memory-layout
   :compute-broadcast-info
   :compute-stride-pattern
   :compute-dimension-mapping))

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
  "Compute the broadcast shape of two shapes using NumPy broadcasting rules.
   
   Broadcasting Rules:
   1. Shapes are aligned from the rightmost dimension
   2. Dimensions must be equal or one must be 1
   3. Missing dimensions are treated as 1
   
   Examples:
     (3,4) + (4,)   -> (3,4)    ; trailing dimension broadcast
     (3,1) + (1,4)  -> (3,4)    ; both dimensions broadcast  
     (3,4) + (2,4)  -> ERROR    ; incompatible dimensions"
  ;; Input validation
  (unless (and (listp shape1) (listp shape2))
    (error "Shapes must be lists, got ~A (~A) and ~A (~A)" 
           shape1 (type-of shape1) shape2 (type-of shape2)))
  
  (unless (and (every #'(lambda (d) (and (integerp d) (> d 0))) shape1)
               (every #'(lambda (d) (and (integerp d) (> d 0))) shape2))
    (error "Shape dimensions must be positive integers, got ~A and ~A" shape1 shape2))
  
  ;; Handle trivial cases  
  (cond
    ((null shape1) shape2)
    ((null shape2) shape1)
    (t
     ;; Main broadcasting algorithm
     (let* ((rank1 (length shape1))
            (rank2 (length shape2))
            (max-rank (max rank1 rank2))
            (padded1 (append (make-list (- max-rank rank1) :initial-element 1) shape1))
            (padded2 (append (make-list (- max-rank rank2) :initial-element 1) shape2))
            (result nil))
       ;; Check compatibility and build result shape
       (loop for d1 in padded1
             for d2 in padded2  
             for dim-index from 0
             do (cond
                  ((= d1 d2) 
                   (push d1 result))
                  ((= d1 1) 
                   (push d2 result))
                  ((= d2 1) 
                   (push d1 result))
                  (t 
                   ;; Enhanced error message with dimension details
                   (error "Cannot broadcast shapes ~A and ~A: dimension ~D is incompatible (~D vs ~D). Broadcasting requires dimensions to be equal or one to be 1."
                          shape1 shape2 dim-index d1 d2))))
       (nreverse result)))))

;;; Broadcasting metadata for memory-efficient operations

(defstruct broadcast-info
  "Metadata describing how to broadcast arrays without copying data.
   Used by the virtual broadcasting engine to create memory-efficient views."
  operand-shapes        ; List of original shapes before broadcasting
  result-shape          ; Final broadcast result shape
  stride-patterns       ; List of stride patterns for each operand
  dimension-mapping     ; How dimensions map from operands to result
  memory-layout)        ; Memory layout optimization hints

(defun compute-broadcast-info (shapes)
  "Compute broadcasting metadata for multiple shapes.
   Returns broadcast-info structure with all necessary information for
   memory-efficient virtual broadcasting."
  (let* ((result-shape (reduce #'broadcast-shapes shapes))
         (stride-patterns (mapcar (lambda (shape) 
                                   (compute-stride-pattern shape result-shape))
                                 shapes))
         (dimension-mapping (mapcar (lambda (shape)
                                     (compute-dimension-mapping shape result-shape))
                                   shapes)))
    (make-broadcast-info
     :operand-shapes shapes
     :result-shape result-shape
     :stride-patterns stride-patterns
     :dimension-mapping dimension-mapping
     :memory-layout :row-major))) ; Default to row-major layout

(defun compute-stride-pattern (operand-shape result-shape)
  "Compute stride pattern for broadcasting an operand shape to result shape.
   Returns list of stride multipliers for efficient array access."
  (let* ((operand-rank (length operand-shape))
         (result-rank (length result-shape))
         (rank-diff (- result-rank operand-rank))
         ;; Pad operand shape with 1s on the left
         (padded-operand (append (make-list rank-diff :initial-element 1) 
                                operand-shape))
         (strides nil))
    ;; Compute stride for each dimension
    (loop for op-dim in (reverse padded-operand)
          for res-dim in (reverse result-shape)
          for stride = (if (= op-dim 1) 0 1) ; 0 stride means broadcast this dimension
          collect stride into stride-list
          finally (setf strides (reverse stride-list)))
    strides))

(defun compute-dimension-mapping (operand-shape result-shape)
  "Compute how dimensions of operand map to dimensions of result.
   Returns list of (operand-dim . result-dim) pairs."
  (let* ((operand-rank (length operand-shape))
         (result-rank (length result-shape))
         (rank-diff (- result-rank operand-rank)))
    ;; Map operand dimensions to result dimensions
    (loop for i from 0 below operand-rank
          for result-dim = (+ i rank-diff)
          collect (cons i result-dim))))

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

;;; Enhanced Shape Inference

(defun infer-shape (op &rest shapes)
  "Infer the result shape for an operation on given shapes"
  (case op
    ((+ - * /)
     (reduce #'broadcast-shapes shapes))
    (matmul
     (matmul-shape (first shapes) (second shapes)))
    ((sum mean)
     (let ((shape (first shapes))
           (axis (second shapes)))
       (reduce-shape shape axis)))
    (transpose
     (let ((shape (first shapes)))
       (if (>= (length shape) 2)
           (append (last shape 1) (butlast shape 1))
           shape)))
    (reshape
     (second shapes))
    (otherwise
     (first shapes))))

(defun compatible-shapes-p (shape1 shape2)
  "Check if two shapes are compatible for element-wise operations"
  (handler-case
      (progn (broadcast-shapes shape1 shape2) t)
    (error () nil)))

(defun broadcast-compatible-p (shape1 shape2)
  "Check if shapes are broadcast compatible"
  (compatible-shapes-p shape1 shape2))

(defun matmul-shape (shape1 shape2)
  "Compute shape for matrix multiplication"
  (cond
    ((and (null shape1) (null shape2))
     '())
    ((null shape1)
     (cdr shape2))
    ((null shape2)
     (butlast shape1))
    ((and (= (length shape1) 1) (= (length shape2) 1))
     (if (= (first shape1) (first shape2)) '() 
         (error "Incompatible shapes for dot product: ~A and ~A" shape1 shape2)))
    ((= (length shape1) 1)
     (if (= (first shape1) (first shape2))
         (cdr shape2)
         (error "Incompatible shapes for matrix multiplication: ~A and ~A" shape1 shape2)))
    ((= (length shape2) 1)
     (if (= (car (last shape1)) (first shape2))
         (butlast shape1)
         (error "Incompatible shapes for matrix multiplication: ~A and ~A" shape1 shape2)))
    (t
     (if (= (car (last shape1)) (first shape2))
         (append (butlast shape1) (cdr shape2))
         (error "Incompatible shapes for matrix multiplication: ~A and ~A" shape1 shape2)))))

(defun einstein-shape (subscripts input-shapes)
  "Compute shape for Einstein summation"
  (let* ((all-indices (remove-duplicates 
                       (apply #'append 
                              (mapcar (lambda (s) (coerce s 'list)) subscripts))))
         (output-indices (if (find #\- subscripts)
                             (coerce (subseq subscripts (1+ (position #\- subscripts))) 'list)
                             all-indices)))
    (mapcar (lambda (idx)
              (let ((pos (position idx (coerce (first subscripts) 'list))))
                (if pos 
                    (nth pos (first input-shapes))
                    1)))
            output-indices)))

(defun reduce-shape (shape axis)
  "Compute shape after reduction along axis"
  (cond
    ((null axis) '())
    ((numberp axis)
     (if (and (>= axis 0) (< axis (length shape)))
         (append (subseq shape 0 axis) (subseq shape (1+ axis)))
         shape))
    ((listp axis)
     (let ((sorted-axes (sort (copy-list axis) #'>)))
       (reduce (lambda (s ax) (reduce-shape s ax)) sorted-axes :initial-value shape)))
    (t shape)))