(defpackage epsilon.compute.einsum-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.einsum-tests)

(deftest test-einsum-matrix-multiplication
  "Test einsum for matrix multiplication"
  ;; Standard matrix multiplication: C = A @ B
  (let* ((A #2A((1 2) (3 4)))
         (B #2A((5 6) (7 8)))
         (result (c:evaluate (c:einsum "ij,jk->ik" (c:const A) (c:const B))))
         (expected #2A((19 22) (43 50))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2 2)))
    (is (equalp result expected))))

(deftest test-einsum-dot-product
  "Test einsum for dot product"
  ;; Dot product of two vectors
  (let* ((v1 #(1 2 3))
         (v2 #(4 5 6))
         (result (c:evaluate (c:einsum "i,i->" (c:const v1) (c:const v2))))
         (expected 32)) ; 1*4 + 2*5 + 3*6 = 32
    (is (numberp result))
    (is (= result expected))))

(deftest test-einsum-outer-product
  "Test einsum for outer product"
  ;; Outer product of two vectors
  (let* ((v1 #(1 2 3))
         (v2 #(4 5))
         (result (c:evaluate (c:einsum "i,j->ij" (c:const v1) (c:const v2))))
         (expected #2A((4 5) (8 10) (12 15))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(3 2)))
    (is (equalp result expected))))

(deftest test-einsum-transpose
  "Test einsum for matrix transpose"
  (let* ((A #2A((1 2 3) (4 5 6)))
         (result (c:evaluate (c:einsum "ij->ji" (c:const A))))
         (expected #2A((1 4) (2 5) (3 6))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(3 2)))
    (is (equalp result expected))))

(deftest test-einsum-trace
  "Test einsum for matrix trace"
  (let* ((A #2A((1 2) (3 4)))
         (result (c:evaluate (c:einsum "ii->" (c:const A))))
         (expected 5)) ; 1 + 4 = 5
    (is (numberp result))
    (is (= result expected))))

(deftest test-einsum-diagonal
  "Test einsum for extracting diagonal"
  (let* ((A #2A((1 2 3) (4 5 6) (7 8 9)))
         (result (c:evaluate (c:einsum "ii->i" (c:const A))))
         (expected #(1 5 9)))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(3)))
    (is (equalp result expected))))

(deftest test-einsum-batch-matrix-multiply
  "Test einsum for batch matrix multiplication"
  ;; Batch of 2 matrix multiplications
  (let* ((A #3A(((1 2) (3 4))
                ((5 6) (7 8))))
         (B #3A(((9 10) (11 12))
                ((13 14) (15 16))))
         (result (c:evaluate (c:einsum "bij,bjk->bik" (c:const A) (c:const B))))
         (expected #3A(((31 34) (71 78))
                      ((155 166) (211 226)))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2 2 2)))
    (is (equalp result expected))))

(deftest test-einsum-implicit-summation
  "Test einsum with implicit summation (no output specified)"
  ;; Sum all elements when no output is specified
  (let* ((A #2A((1 2) (3 4)))
         (result (c:evaluate (c:einsum "ij" (c:const A))))
         (expected 10)) ; 1 + 2 + 3 + 4 = 10
    (is (numberp result))
    (is (= result expected))))

(deftest test-einsum-hadamard-product
  "Test einsum for element-wise product"
  (let* ((A #2A((1 2) (3 4)))
         (B #2A((5 6) (7 8)))
         (result (c:evaluate (c:einsum "ij,ij->ij" (c:const A) (c:const B))))
         (expected #2A((5 12) (21 32))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2 2)))
    (is (equalp result expected))))

(deftest test-einsum-matrix-vector-multiply
  "Test einsum for matrix-vector multiplication"
  (let* ((A #2A((1 2 3) (4 5 6)))
         (v #(7 8 9))
         (result (c:evaluate (c:einsum "ij,j->i" (c:const A) (c:const v))))
         (expected #(50 122))) ; [1*7+2*8+3*9, 4*7+5*8+6*9]
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2)))
    (is (equalp result expected))))

(deftest test-einsum-tensor-contraction
  "Test einsum for general tensor contraction"
  ;; Contract middle indices: (2,3,4) x (4,5) -> (2,3,5)
  (let* ((A (make-array '(2 3 4) :initial-contents
                       '(((1 2 3 4) (5 6 7 8) (9 10 11 12))
                         ((13 14 15 16) (17 18 19 20) (21 22 23 24)))))
         (B (make-array '(4 5) :initial-contents
                       '((25 26 27 28 29)
                         (30 31 32 33 34)
                         (35 36 37 38 39)
                         (40 41 42 43 44))))
         (result (c:evaluate (c:einsum "ijk,kl->ijl" (c:const A) (c:const B)))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2 3 5)))
    ;; Check first element: 1*25 + 2*30 + 3*35 + 4*40 = 350
    (is (= (aref result 0 0 0) 350))))

(deftest test-einsum-bilinear-form
  "Test einsum for bilinear form computation"
  ;; Compute x^T A y
  (let* ((x #(1 2))
         (A #2A((3 4) (5 6)))
         (y #(7 8))
         (result (c:evaluate (c:einsum "i,ij,j->" 
                                      (c:const x) (c:const A) (c:const y))))
         (expected 219)) ; x^T A y = [1 2] [[3 4] [5 6]] [7 8] = 1*3*7 + 1*4*8 + 2*5*7 + 2*6*8 = 21+32+70+96 = 219
    (is (numberp result))
    (is (= result expected))))

(deftest test-einsum-kronecker-product
  "Test einsum for Kronecker product"
  (let* ((A #2A((1 2) (3 4)))
         (B #2A((5 6) (7 8)))
         (result (c:evaluate (c:einsum "ij,kl->ikjl" (c:const A) (c:const B))))
         ;; Reshape to standard Kronecker form
         (result-2d (make-array '(4 4))))
    (is (arrayp result))
    (is (equal (array-dimensions result) '(2 2 2 2)))
    ;; Verify by reshaping and checking a few elements
    (dotimes (i 2)
      (dotimes (j 2)
        (dotimes (k 2)
          (dotimes (l 2)
            (setf (aref result-2d (+ (* i 2) k) (+ (* j 2) l))
                  (aref result i k j l))))))
    ;; Check Kronecker product structure
    (is (= (aref result-2d 0 0) 5))  ; 1*5
    (is (= (aref result-2d 0 1) 6))  ; 1*6
    (is (= (aref result-2d 1 1) 8))  ; 1*8
    (is (= (aref result-2d 2 2) 20)) ; 4*5 (from A[1,1]*B[0,0])
    (is (= (aref result-2d 3 3) 32)))) ; 4*8

(deftest test-einsum-errors
  "Test einsum error handling"
  ;; Dimension mismatch
  (is-thrown (error)
    (c:evaluate (c:einsum "ij,jk->ik" 
                         (c:const #2A((1 2) (3 4)))
                         (c:const #2A((5 6 7))))))
  
  ;; Invalid notation (can be enhanced based on parser)
  (is-thrown (error)
    (c:evaluate (c:einsum "invalid notation" (c:const #(1 2 3))))))