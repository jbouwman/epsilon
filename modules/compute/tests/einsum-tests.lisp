(defpackage epsilon.compute.einsum-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.einsum-tests)

(deftest test-einsum-basic
  "Test basic Einstein summation notation"
  ;; Matrix multiplication: C_ik = A_ij * B_jk
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    (let ((result (c:einsum "ij,jk->ik" A B)))
      (is (sym:expr-p result))
      (is (eq (sym:expr-op result) 'epsilon.compute:einsum)))))

(deftest test-einsum-trace
  "Test trace operation via einsum"
  ;; Trace: tr(A) = A_ii (repeated index implies summation)
  (let ((A (c:var 'A)))
    (let ((trace (c:einsum "ii->" A)))
      (is (sym:expr-p trace))
      (is (eq (sym:expr-op trace) 'epsilon.compute:einsum)))))

(deftest test-einsum-outer-product
  "Test outer product via einsum"
  ;; Outer product: C_ij = a_i * b_j
  (let ((a (c:var 'a))
        (b (c:var 'b)))
    (let ((outer (c:einsum "i,j->ij" a b)))
      (is (sym:expr-p outer)))))

(deftest test-einsum-hadamard-product
  "Test element-wise (Hadamard) product"
  ;; Element-wise: C_ij = A_ij * B_ij
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    (let ((hadamard (c:einsum "ij,ij->ij" A B)))
      (is (sym:expr-p hadamard)))))

(deftest test-einsum-batch-matrix-multiply
  "Test batched matrix multiplication"
  ;; Batch MM: C_bij = A_bik * B_bkj
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    (let ((batch-mm (c:einsum "bik,bkj->bij" A B)))
      (is (sym:expr-p batch-mm)))))

(deftest test-einsum-chain-rule
  "Test chain rule for tensor contractions"
  ;; Test that (AB)C can be computed efficiently
  (let ((A (c:var 'A))
        (B (c:var 'B))
        (Cv (c:var 'C)))
    ;; This should eventually optimize the contraction path
    (let ((result (c:einsum "ij,jk,kl->il" A B Cv)))
      (is (sym:expr-p result)))))

(deftest test-einsum-transpose-patterns
  "Test various transpose patterns"
  (let ((A (c:var 'A)))
    ;; Matrix transpose: B_ji = A_ij
    (let ((transpose (c:einsum "ij->ji" A)))
      (is (sym:expr-p transpose)))
    
    ;; Tensor transpose: B_ikj = A_ijk  
    (let ((tensor-transpose (c:einsum "ijk->ikj" A)))
      (is (sym:expr-p tensor-transpose)))))

(deftest test-einsum-broadcasting
  "Test broadcasting in einsum operations"
  (let ((A (c:var 'A))   ; Matrix
        (b (c:var 'b)))  ; Vector
    ;; Broadcasting: C_ij = A_ij * b_j
    (let ((broadcast (c:einsum "ij,j->ij" A b)))
      (is (sym:expr-p broadcast)))))

(deftest test-einsum-reduction-patterns
  "Test various reduction patterns"
  (let ((A (c:var 'A)))
    ;; Sum over all elements: scalar = A_ij (implicit sum)
    (let ((total-sum (c:einsum "ij->" A)))
      (is (sym:expr-p total-sum)))
    
    ;; Sum over rows: b_j = A_ij
    (let ((row-sum (c:einsum "ij->j" A)))
      (is (sym:expr-p row-sum)))
    
    ;; Sum over columns: a_i = A_ij  
    (let ((col-sum (c:einsum "ij->i" A)))
      (is (sym:expr-p col-sum)))))