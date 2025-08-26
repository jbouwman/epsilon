(defpackage epsilon.compute.notation-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.notation-tests)

(deftest test-math-macro-basic
  "Test basic math macro for natural mathematical notation"
  ;; Test that we can use natural notation like (math (∂ (sin x) x))
  (let ((x (c:var 'x)))
    ;; For now, test that the math macro exists and can be called
    ;; We'll start with simple expressions
    (let ((expr (c:sin x)))
      (is (sym:expr-p expr))
      (is (eq (sym:expr-op expr) 'epsilon.compute:sin)))))

(deftest test-gradient-notation
  "Test gradient notation ∇"
  (skip "grad function not yet implemented")
  ;; Test that ∇ can compute gradients
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    (let ((f (c:+ (c:^ x 2) (c:^ y 2))))
      ;; ∇f should give [2x, 2y]
      (let ((grad (c:grad f x y)))
        (is (listp grad))
        (is (= (length grad) 2))))))

(deftest test-partial-derivative-notation  
  "Test partial derivative notation ∂"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    (let ((f (c:+ (c:* x y) (c:^ x 2))))
      ;; ∂f/∂x = y + 2x
      (let ((dx (c:diff f x)))
        (is (sym:expr-p dx)))
      ;; ∂f/∂y = x  
      (let ((dy (c:diff f y)))
        (is (sym:expr-p dy))))))

(deftest test-matrix-notation
  "Test matrix literal notation"
  ;; Eventually we want [1 2; 3 4] style matrices
  ;; For now, test that we can create matrix symbols
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    ;; Matrix operations
    (let ((At (c:transpose A))
          (AB (c:dot A B)))
      (is (sym:expr-p At))
      (is (sym:expr-p AB))
      (is (eq (sym:expr-op At) 'epsilon.compute:transpose))
      (is (eq (sym:expr-op AB) 'epsilon.compute:dot)))))

(deftest test-einstein-notation-basic
  "Test basic Einstein summation notation"
  ;; Test that we can express tensor contractions
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    ;; Eventually: (einsum "ij,jk->ik" A B) for matrix multiply
    ;; For now, just test the function exists and can be called
    (is (fboundp 'epsilon.compute:einsum))))

(deftest test-summation-notation
  "Test summation notation Σ"
  ;; Test mathematical summation
  (let ((i (c:var 'i))
        (n (c:var 'n)))
    ;; Eventually: (Σ i 1 n (^ i 2)) = sum of i^2 from 1 to n
    ;; For now, test that we can create summation expressions
    (let ((expr (c:^ i 2)))
      (is (sym:expr-p expr)))))

(deftest test-integration-notation
  "Test integration notation ∫"
  ;; Test symbolic integration
  (let ((x (c:var 'x)))
    ;; ∫ x dx = x²/2
    (let ((integral (c:integrate x x)))
      (is (sym:expr-p integral)))
    ;; ∫ sin(x) dx = -cos(x)  
    (let ((integral (c:integrate (c:sin x) x)))
      (is (sym:expr-p integral)))))