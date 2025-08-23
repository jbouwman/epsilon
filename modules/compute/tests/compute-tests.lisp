(defpackage epsilon.compute.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)
   (simp epsilon.compute.simplify)))

(in-package epsilon.compute.tests)

(deftest test-basic-symbolic-operations
  "Test basic symbolic arithmetic"
  ;; Variables
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    
    ;; Addition
    (let ((sum (c:+ x y)))
      (is (sym:expr-p sum))
      (is (eq (sym:expr-op sum) '+)))
    
    ;; Multiplication
    (let ((prod (c:* x 2)))
      (is (sym:expr-p prod))
      (is (eq (sym:expr-op prod) '*)))
    
    ;; Mixed numeric and symbolic
    (let ((expr (c:+ 1 x 2)))
      (is (sym:expr-p expr)))))

(deftest test-simplification
  "Test expression simplification"
  (let ((x (c:var 'x)))
    ;; x + 0 = x
    (let ((simplified (c:simplify (c:+ x 0))))
      (is (sym:var-p simplified))
      (is (eq (sym:var-name simplified) 'x)))
    
    ;; x * 1 = x
    (let ((simplified (c:simplify (c:* x 1))))
      (is (sym:var-p simplified))
      (is (eq (sym:var-name simplified) 'x)))
    
    ;; x * 0 = 0
    (let ((simplified (c:simplify (c:* x 0))))
      (is (sym:const-p simplified))
      (is (= (sym:const-value simplified) 0)))
    
    ;; x - x = 0
    (let ((simplified (c:simplify (c:- x x))))
      (is (sym:const-p simplified))
      (is (= (sym:const-value simplified) 0)))))

(deftest test-differentiation
  "Test symbolic differentiation"
  (let ((x (c:var 'x)))
    ;; d/dx(x) = 1
    (let ((deriv (c:diff x x)))
      (is (sym:const-p deriv))
      (is (= (sym:const-value deriv) 1)))
    
    ;; d/dx(x^2) = 2x
    (let* ((expr (c:^ x 2))
           (deriv (c:simplify (c:diff expr x))))
      (is (sym:expr-p deriv))
      (is (eq (sym:expr-op deriv) '*)))
    
    ;; d/dx(sin(x)) = cos(x)
    (let* ((expr (c:sin x))
           (deriv (c:diff expr x)))
      (is (sym:expr-p deriv))
      (is (eq (sym:expr-op deriv) '*))
      (is (eq (sym:expr-op (first (sym:expr-args deriv))) 'cos)))
    
    ;; d/dx(x^3 + 2x^2 + x + 1)
    (let* ((expr (c:+ (c:^ x 3) (c:* 2 (c:^ x 2)) x 1))
           (deriv (c:diff expr x))
           (simplified (c:simplify deriv)))
      (is (sym:expr-p simplified)))))

(deftest test-substitution
  "Test variable substitution"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    ;; Substitute x = 2 in x + y
    (let* ((expr (c:+ x y))
           (subst (c:substitute expr '((x . 2)))))
      (is (sym:expr-p subst))
      (is (= (length (sym:expr-args subst)) 2)))
    
    ;; Substitute x = 3, y = 4 in x^2 + y
    (let* ((expr (c:+ (c:^ x 2) y))
           (result (c:evaluate expr '((x . 3) (y . 4)))))
      (is (numberp result))
      (is (= result 13)))))

(deftest test-trigonometric-simplification
  "Test trigonometric identities"
  (let ((x (c:var 'x)))
    ;; sin^2(x) + cos^2(x) = 1
    (let* ((sin2 (c:^ (c:sin x) 2))
           (cos2 (c:^ (c:cos x) 2))
           (expr (c:+ sin2 cos2))
           (simplified (c:simplify expr)))
      (is (sym:const-p simplified))
      (is (= (sym:const-value simplified) 1)))
    
    ;; tan(x) = sin(x)/cos(x)
    (let* ((tan-expr (c:tan x))
           (simplified (c:simplify tan-expr)))
      (is (sym:expr-p simplified))
      (is (eq (sym:expr-op simplified) '/)))))

(deftest test-matrix-operations
  "Test matrix operation symbols"
  (let ((A (c:var 'A))
        (B (c:var 'B)))
    ;; Transpose
    (let ((At (c:transpose A)))
      (is (sym:expr-p At))
      (is (eq (sym:expr-op At) 'transpose)))
    
    ;; Matrix multiplication (dot product)
    (let ((AB (c:dot A B)))
      (is (sym:expr-p AB))
      (is (eq (sym:expr-op AB) 'dot)))
    
    ;; Determinant
    (let ((detA (c:det A)))
      (is (sym:expr-p detA))
      (is (eq (sym:expr-op detA) 'det)))))

(deftest test-power-rule
  "Test power rule differentiation"
  (let ((x (c:var 'x)))
    ;; d/dx(x^n) = n*x^(n-1)
    (loop for n from 2 to 5
          do (let* ((expr (c:^ x n))
                    (deriv (c:diff expr x))
                    (simplified (c:simplify deriv)))
               (is (sym:expr-p simplified))
               ;; Check it has the form n * x^(n-1)
               (when (and (sym:expr-p simplified)
                         (eq (sym:expr-op simplified) '*))
                 (let ((coeff (first (sym:expr-args simplified))))
                   (when (sym:const-p coeff)
                     (is (= (sym:const-value coeff) n)))))))))

(deftest test-chain-rule
  "Test chain rule differentiation"
  (let ((x (c:var 'x)))
    ;; d/dx(sin(x^2)) = 2x*cos(x^2)
    (let* ((expr (c:sin (c:^ x 2)))
           (deriv (c:diff expr x))
           (simplified (c:simplify deriv)))
      (is (sym:expr-p simplified)))
    
    ;; d/dx(e^(2x)) = 2*e^(2x)
    (let* ((expr (c:exp (c:* 2 x)))
           (deriv (c:diff expr x))
           (simplified (c:simplify deriv)))
      (is (sym:expr-p simplified)))))

(deftest test-partial-derivatives
  "Test partial derivatives"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    ;; ∂/∂x(x^2 + xy + y^2)
    (let* ((expr (c:+ (c:^ x 2) (c:* x y) (c:^ y 2)))
           (dx (c:diff expr x))
           (dy (c:diff expr y)))
      ;; ∂/∂x = 2x + y
      (is (sym:expr-p dx))
      ;; ∂/∂y = x + 2y
      (is (sym:expr-p dy)))))

(deftest test-expression-evaluation
  "Test numerical evaluation"
  ;; Pure numeric expressions
  (is (= (c:evaluate (c:+ 2 3)) 5))
  (is (= (c:evaluate (c:* 4 5)) 20))
  (is (= (c:evaluate (c:^ 2 3)) 8))
  
  ;; Trigonometric
  (is (< (abs (- (c:evaluate (c:sin (/ pi 2))) 1.0)) 0.0001))
  (is (< (abs (c:evaluate (c:cos 0))) 0.0001))
  
  ;; With substitution
  (let ((x (c:var 'x)))
    (is (= (c:evaluate (c:+ x 5) '((x . 3))) 8))
    (is (= (c:evaluate (c:^ x 2) '((x . 4))) 16))))