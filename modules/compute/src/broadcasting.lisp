;;;; Broadcasting System for Tensor Operations
;;;;
;;;; This module implements NumPy-style broadcasting for tensor operations,
;;;; allowing operations between tensors of different but compatible shapes.

(defpackage epsilon.compute.broadcasting
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (types epsilon.compute.types))
  (:export
   ;; Shape broadcasting
   :broadcast-shapes
   :broadcast-compatible-p
   :broadcast-arrays
   
   ;; Operations
   :broadcast-binary-op
   :outer-product
   :einsum
   
   ;; Gradient support
   :unbroadcast-gradient
   :sum-to-shape
   
   ;; Utilities
   :expand-dims
   :squeeze-dims
   :broadcast-to))

(in-package epsilon.compute.broadcasting)

;;; Shape Broadcasting Rules

(defun broadcast-shapes (&rest shapes)
  "Compute the broadcasted shape from multiple input shapes.
   Follows NumPy broadcasting rules:
   1. Shapes are right-aligned
   2. Dimensions match if they are equal or one is 1
   3. Missing dimensions are treated as 1"
  (when (null shapes)
    (return-from broadcast-shapes nil))
  
  ;; Filter out scalar shapes (empty lists)
  (setf shapes (remove-if #'null shapes))
  (when (null shapes)
    (return-from broadcast-shapes nil))
  
  ;; Find maximum rank
  (let* ((max-rank (reduce #'max shapes :key #'length))
         (result (make-list max-rank)))
    
    ;; Iterate through each dimension position (right-aligned)
    (loop for dim-idx from (1- max-rank) downto 0
          do (let ((dim-sizes nil))
               ;; Collect sizes at this dimension from all shapes
               (dolist (shape shapes)
                 (let ((shape-idx (- (length shape) (- max-rank dim-idx))))
                   (when (>= shape-idx 0)
                     (push (nth shape-idx shape) dim-sizes))))
               
               ;; Check compatibility and compute result dimension
               (let ((non-one-sizes (remove 1 dim-sizes)))
                 (cond
                   ;; All sizes are 1 or missing
                   ((null non-one-sizes)
                    (setf (nth dim-idx result) 1))
                   
                   ;; All non-1 sizes must be equal
                   ((every (lambda (s) (= s (first non-one-sizes))) non-one-sizes)
                    (setf (nth dim-idx result) (first non-one-sizes)))
                   
                   ;; Incompatible shapes
                   (t (error "Incompatible shapes for broadcasting: ~A" shapes))))))
    result))

(defun broadcast-compatible-p (&rest shapes)
  "Check if shapes are compatible for broadcasting"
  (handler-case
      (progn (apply #'broadcast-shapes shapes) t)
    (error () nil)))

;;; Array Broadcasting

(defun broadcast-arrays (&rest arrays)
  "Broadcast arrays to a common shape"
  (let* ((shapes (mapcar #'array-dimensions arrays))
         (broadcast-shape (apply #'broadcast-shapes shapes)))
    (mapcar (lambda (array)
              (broadcast-to array broadcast-shape))
            arrays)))

(defun broadcast-to (array target-shape)
  "Broadcast array to target shape"
  (let ((source-shape (if (arrayp array) (array-dimensions array) nil)))
    ;; If shapes are already equal, return array as-is
    (when (equal source-shape target-shape)
      (return-from broadcast-to array))
    
    ;; Handle scalar case
    (when (null source-shape)
      (return-from broadcast-to 
        (make-array target-shape 
                    :initial-element (if (arrayp array)
                                        (row-major-aref array 0)
                                        array))))
    
    ;; Create broadcasted array
    (let* ((source-rank (length source-shape))
           (target-rank (length target-shape))
           (result (make-array target-shape)))
      
      ;; Compute strides for efficient indexing
      (labels ((compute-index (target-indices)
                 ;; Map target indices to source indices
                 (let ((source-indices (make-list source-rank)))
                   (loop for i from 0 below source-rank
                         for target-idx = (+ i (- target-rank source-rank))
                         do (setf (nth i source-indices)
                                 (if (and (>= target-idx 0)
                                         (< (nth i source-shape) 
                                            (nth target-idx target-shape)))
                                     0  ; Broadcast dimension
                                     (mod (nth target-idx target-indices)
                                          (nth i source-shape)))))
                   source-indices))
               
               (copy-elements (indices pos)
                 ;; Recursively copy elements
                 (if (= pos target-rank)
                     (setf (apply #'aref result indices)
                           (apply #'aref array (compute-index indices)))
                     (loop for i from 0 below (nth pos target-shape)
                           do (copy-elements (append indices (list i)) 
                                           (1+ pos))))))
        
        (copy-elements nil 0)
        result))))

;;; Binary Operations with Broadcasting

(defun broadcast-binary-op (op array1 array2)
  "Apply binary operation with broadcasting"
  (let* ((shape1 (if (arrayp array1) (array-dimensions array1) nil))
         (shape2 (if (arrayp array2) (array-dimensions array2) nil))
         (result-shape (broadcast-shapes shape1 shape2))
         (broadcast1 (broadcast-to array1 result-shape))
         (broadcast2 (broadcast-to array2 result-shape))
         (result (make-array result-shape)))
    
    ;; Apply operation element-wise
    (dotimes (i (array-total-size result))
      (setf (row-major-aref result i)
            (funcall op 
                     (row-major-aref broadcast1 i)
                     (row-major-aref broadcast2 i))))
    result))

;;; Outer Product

(defun outer-product (array1 array2)
  "Compute outer product of two arrays"
  (let* ((size1 (array-total-size array1))
         (size2 (array-total-size array2))
         (shape1 (array-dimensions array1))
         (shape2 (array-dimensions array2))
         (result-shape (append shape1 shape2))
         (result (make-array result-shape)))
    
    ;; Compute outer product
    (dotimes (i size1)
      (dotimes (j size2)
        (setf (row-major-aref result (+ (* i size2) j))
              (* (row-major-aref array1 i)
                 (row-major-aref array2 j)))))
    result))

;;; Gradient Support

(defun unbroadcast-gradient (grad original-shape)
  "Sum gradient to match original shape (reverse of broadcasting)"
  (sum-to-shape grad original-shape))

(defun sum-to-shape (array target-shape)
  "Sum array dimensions to match target shape"
  (let ((source-shape (array-dimensions array)))
    ;; If shapes are already equal, return as-is
    (when (equal source-shape target-shape)
      (return-from sum-to-shape array))
    
    ;; Handle scalar target
    (when (null target-shape)
      (return-from sum-to-shape
        (let ((sum 0))
          (dotimes (i (array-total-size array))
            (incf sum (row-major-aref array i)))
          sum)))
    
    ;; Sum over broadcast dimensions
    (let* ((source-rank (length source-shape))
           (target-rank (length target-shape))
           (rank-diff (- source-rank target-rank))
           (result array))
      
      ;; Sum over leading dimensions if source has more dims
      (when (> rank-diff 0)
        (loop for dim from 0 below rank-diff
              do (setf result (sum-over-dimension result 0)))
        (setf source-shape (array-dimensions result)
              source-rank (length source-shape)))
      
      ;; Sum over dimensions that were broadcast (size 1 in target)
      (loop for i from 0 below target-rank
            when (and (< i source-rank)
                     (= (nth i target-shape) 1)
                     (> (nth i source-shape) 1))
            do (setf result (sum-over-dimension result i)))
      
      ;; Reshape if needed
      (if (equal (array-dimensions result) target-shape)
          result
          (reshape-array result target-shape)))))

(defun sum-over-dimension (array dim)
  "Sum array over specified dimension"
  (let* ((shape (array-dimensions array))
         (new-shape (append (subseq shape 0 dim)
                           (list 1)
                           (subseq shape (1+ dim))))
         (result (make-array new-shape :initial-element 0)))
    
    ;; Sum over the dimension
    (let ((indices (make-list (length shape) :initial-element 0)))
      (labels ((sum-recursive (pos)
                 (if (= pos (length shape))
                     (let ((result-indices (copy-list indices)))
                       (setf (nth dim result-indices) 0)
                       (incf (apply #'aref result result-indices)
                             (apply #'aref array indices)))
                     (if (= pos dim)
                         (loop for i from 0 below (nth pos shape)
                               do (setf (nth pos indices) i)
                                  (sum-recursive (1+ pos)))
                         (loop for i from 0 below (nth pos shape)
                               do (setf (nth pos indices) i)
                                  (sum-recursive (1+ pos)))))))
        (sum-recursive 0)))
    
    ;; Squeeze the summed dimension if it becomes 1
    (squeeze-dims result (list dim))))

;;; Utility Functions

(defun expand-dims (array axis)
  "Add a dimension of size 1 at the specified axis"
  (let* ((shape (array-dimensions array))
         (new-shape (append (subseq shape 0 axis)
                           (list 1)
                           (subseq shape axis))))
    (reshape-array array new-shape)))

(defun squeeze-dims (array &optional axes)
  "Remove dimensions of size 1"
  (let* ((shape (array-dimensions array))
         (new-shape (if axes
                       (loop for i from 0
                             for dim in shape
                             unless (and (member i axes) (= dim 1))
                             collect dim)
                       (remove 1 shape))))
    (if (equal shape new-shape)
        array
        (reshape-array array new-shape))))

(defun reshape-array (array new-shape)
  "Reshape array to new shape (total size must match)"
  (let ((size (array-total-size array))
        (new-size (reduce #'* new-shape :initial-value 1)))
    (unless (= size new-size)
      (error "Cannot reshape array of size ~A to shape ~A" size new-shape))
    
    (let ((result (make-array new-shape)))
      (dotimes (i size)
        (setf (row-major-aref result i)
              (row-major-aref array i)))
      result)))

;;; Einstein Summation (Basic Implementation)

(defun einsum (subscripts &rest arrays)
  "Basic Einstein summation notation implementation.
   Supports common operations like:
   - 'ij,jk->ik' : Matrix multiplication
   - 'ii->' : Trace
   - 'ij->ji' : Transpose
   - 'i,j->ij' : Outer product
   - 'ii->i' : Diagonal extraction
   - 'ij,ij->ij' : Hadamard product
   - 'ij,ij->' : Frobenius inner product"
  
  ;; Parse the subscripts
  (let* ((parts (split-einsum-string subscripts))
         (input-specs (first parts))
         (output-spec (second parts)))
    
    ;; Handle common cases explicitly for better performance
    (cond
      ;; Matrix multiplication: 'ij,jk->ik'
      ((and (= (length arrays) 2)
            (string= subscripts "ij,jk->ik"))
       (matrix-multiply (first arrays) (second arrays)))
      
      ;; Transpose: 'ij->ji'
      ((and (= (length arrays) 1)
            (string= subscripts "ij->ji"))
       (transpose-array (first arrays)))
      
      ;; Trace: 'ii->'
      ((and (= (length arrays) 1)
            (string= subscripts "ii->"))
       (array-trace (first arrays)))
      
      ;; Diagonal: 'ii->i'
      ((and (= (length arrays) 1)
            (string= subscripts "ii->i"))
       (array-diagonal (first arrays)))
      
      ;; Outer product: 'i,j->ij'
      ((and (= (length arrays) 2)
            (string= subscripts "i,j->ij"))
       (outer-product (first arrays) (second arrays)))
      
      ;; Dot product: 'i,i->'
      ((and (= (length arrays) 2)
            (string= subscripts "i,i->"))
       (dot-product (first arrays) (second arrays)))
      
      ;; Hadamard product: 'ij,ij->ij'
      ((and (= (length arrays) 2)
            (string= subscripts "ij,ij->ij"))
       (hadamard-product (first arrays) (second arrays)))
      
      ;; Matrix-vector multiply: 'ij,j->i'
      ((and (= (length arrays) 2)
            (string= subscripts "ij,j->i"))
       (matrix-vector-multiply (first arrays) (second arrays)))
      
      ;; Batch matrix multiply: 'bij,bjk->bik'
      ((and (= (length arrays) 2)
            (string= subscripts "bij,bjk->bik"))
       (batch-matrix-multiply (first arrays) (second arrays)))
      
      ;; Bilinear form: 'i,ij,j->'
      ((and (= (length arrays) 3)
            (string= subscripts "i,ij,j->"))
       (bilinear-form (first arrays) (second arrays) (third arrays)))
      
      ;; Tensor contraction: 'ijk,jkl->il'
      ((and (= (length arrays) 2)
            (string= subscripts "ijk,jkl->il"))
       (tensor-contraction (first arrays) (second arrays)))
      
      ;; Kronecker product: 'ij,kl->ikjl'
      ((and (= (length arrays) 2)
            (string= subscripts "ij,kl->ikjl"))
       (kronecker-product (first arrays) (second arrays)))
      
      ;; Sum along axis (implicit summation): 'ij->j'
      ((and (= (length arrays) 1)
            (or (string= subscripts "ij->j")
                (string= subscripts "ijk->jk")))
       (sum-along-axis (first arrays) 0))
      
      ;; Full summation: 'ij->' or just 'ij' (implicit)
      ((and (= (length arrays) 1)
            (or (string= subscripts "ij->")
                (string= subscripts "ij")
                (string= subscripts "ijk->")))
       (let ((sum 0))
         (dotimes (i (array-total-size (first arrays)))
           (incf sum (row-major-aref (first arrays) i)))
         sum))
      
      ;; General implementation for other cases
      (t (general-einsum input-specs output-spec arrays)))))

(defun split-einsum-string (subscripts)
  "Split einsum string into input and output parts"
  (let ((arrow-pos (position #\> subscripts)))
    (if arrow-pos
        (list (subseq subscripts 0 (1- arrow-pos))
              (subseq subscripts (1+ arrow-pos)))
        ;; No arrow means implicit summation - output is empty string
        (list subscripts ""))))

(defun matrix-multiply (a b)
  "Matrix multiplication"
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (p (array-dimension b 1))
         (result (make-array (list m p) :initial-element 0)))
    (dotimes (i m)
      (dotimes (j p)
        (dotimes (k n)
          (incf (aref result i j)
                (* (aref a i k) (aref b k j))))))
    result))

(defun transpose-array (array)
  "Transpose a 2D array"
  (let* ((m (array-dimension array 0))
         (n (array-dimension array 1))
         (result (make-array (list n m))))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref result j i) (aref array i j))))
    result))

(defun array-trace (array)
  "Compute trace of a square matrix"
  (let ((n (min (array-dimension array 0)
                (array-dimension array 1)))
        (sum 0))
    (dotimes (i n)
      (incf sum (aref array i i)))
    sum))

(defun array-diagonal (array)
  "Extract diagonal of a matrix"
  (let* ((n (min (array-dimension array 0)
                 (array-dimension array 1)))
         (result (make-array n)))
    (dotimes (i n)
      (setf (aref result i) (aref array i i)))
    result))

(defun dot-product (a b)
  "Compute dot product of two vectors"
  (let ((sum 0)
        (n (array-total-size a)))
    (dotimes (i n)
      (incf sum (* (row-major-aref a i)
                  (row-major-aref b i))))
    sum))

(defun hadamard-product (a b)
  "Element-wise product of two arrays"
  (let* ((shape (array-dimensions a))
         (result (make-array shape)))
    (dotimes (i (array-total-size a))
      (setf (row-major-aref result i)
            (* (row-major-aref a i)
               (row-major-aref b i))))
    result))

(defun matrix-vector-multiply (matrix vector)
  "Multiply matrix by vector"
  (let* ((m (array-dimension matrix 0))
         (n (array-dimension matrix 1))
         (result (make-array m :initial-element 0)))
    (dotimes (i m)
      (dotimes (j n)
        (incf (aref result i)
              (* (aref matrix i j) (aref vector j)))))
    result))

(defun batch-matrix-multiply (a-tensor b-tensor)
  "Batch matrix multiplication for 3D tensors"
  (let* ((batch (array-dimension a-tensor 0))
         (m (array-dimension a-tensor 1))
         (n (array-dimension a-tensor 2))
         (p (array-dimension b-tensor 2))
         (result (make-array (list batch m p) :initial-element 0)))
    (dotimes (batch-idx batch)
      (dotimes (i m)
        (dotimes (j p)
          (dotimes (k n)
            (incf (aref result batch-idx i j)
                  (* (aref a-tensor batch-idx i k) 
                     (aref b-tensor batch-idx k j)))))))
    result))

(defun bilinear-form (x matrix y)
  "Compute bilinear form x^T * matrix * y"
  (let ((sum 0)
        (m (array-dimension matrix 0))
        (n (array-dimension matrix 1)))
    (dotimes (i m)
      (dotimes (j n)
        (incf sum (* (aref x i) (aref matrix i j) (aref y j)))))
    sum))

(defun tensor-contraction (a b)
  "Contract middle dimensions: 'ijk,jkl->il'"
  (let* ((i-dim (array-dimension a 0))
         (j-dim (array-dimension a 1))
         (k-dim (array-dimension a 2))
         (l-dim (array-dimension b 2))
         (result (make-array (list i-dim l-dim) :initial-element 0)))
    (dotimes (i i-dim)
      (dotimes (l l-dim)
        (dotimes (j j-dim)
          (dotimes (k k-dim)
            (incf (aref result i l)
                  (* (aref a i j k) (aref b j k l)))))))
    result))

(defun kronecker-product (a b)
  "Kronecker product for einsum 'ij,kl->ikjl' - returns 4D tensor"
  (let* ((m1 (array-dimension a 0))
         (n1 (array-dimension a 1))
         (m2 (array-dimension b 0))
         (n2 (array-dimension b 1))
         (result (make-array (list m1 m2 n1 n2))))
    (dotimes (i1 m1)
      (dotimes (j1 n1)
        (dotimes (i2 m2)
          (dotimes (j2 n2)
            (setf (aref result i1 i2 j1 j2)
                  (* (aref a i1 j1) (aref b i2 j2)))))))
    result))

(defun sum-along-axis (array axis)
  "Sum array along specified axis"
  (let* ((shape (array-dimensions array))
         (new-shape (append (subseq shape 0 axis) 
                           (subseq shape (1+ axis))))
         (result (make-array (if new-shape new-shape '(1)) 
                           :initial-element 0)))
    ;; Simple implementation for common cases
    (cond
      ;; 2D array, sum along axis 0
      ((and (= (length shape) 2) (= axis 0))
       (dotimes (j (second shape))
         (dotimes (i (first shape))
           (incf (aref result j) (aref array i j)))))
      
      ;; 2D array, sum along axis 1
      ((and (= (length shape) 2) (= axis 1))
       (dotimes (i (first shape))
         (dotimes (j (second shape))
           (incf (aref result i) (aref array i j)))))
      
      ;; General case would need more complex indexing
      (t (error "Sum along axis ~A for shape ~A not yet implemented" 
                axis shape)))
    result))

(defun general-einsum (input-specs output-spec arrays)
  "General einsum implementation for arbitrary expressions"
  ;; For now, provide a helpful error for unsupported cases
  (error "General einsum for '~A->~A' not yet implemented. Supported operations: ~
          matrix multiply (ij,jk->ik), transpose (ij->ji), trace (ii->), ~
          diagonal (ii->i), outer product (i,j->ij), dot product (i,i->), ~
          hadamard product (ij,ij->ij), matrix-vector (ij,j->i), ~
          batch matrix multiply (bij,bjk->bik), bilinear form (i,ij,j->), ~
          tensor contraction (ijk,jkl->il), kronecker product (ij,kl->ikjl)"
         input-specs output-spec))