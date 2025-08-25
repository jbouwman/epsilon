(defpackage epsilon.compute.autodiff-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)
   (ad epsilon.compute.autodiff)))

(in-package epsilon.compute.autodiff-tests)

;; Utility function for floating point comparison
(defun approximately-equal (a b tolerance)
  (< (abs (- a b)) tolerance))

(deftest test-forward-mode-basic
  "Test basic forward-mode automatic differentiation"
  (let ((x (c:var 'x)))
    ;; f(x) = x^2, f'(x) = 2x
    (let* ((f (c:^ x 2))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      ;; The result should be a multiplication
      (is (eq (sym:expr-op df) 'epsilon.compute:*)))))

(deftest test-forward-mode-chain-rule
  "Test forward-mode with chain rule"
  (let ((x (c:var 'x)))
    ;; f(x) = sin(x^2), f'(x) = 2x * cos(x^2)
    (let* ((f (c:sin (c:^ x 2)))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      (is (eq (sym:expr-op df) 'epsilon.compute:*)))))

(deftest test-forward-mode-multivariate
  "Test forward-mode with multiple variables"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    ;; f(x,y) = x*y + x^2
    (let ((f (c:+ (c:* x y) (c:^ x 2))))
      ;; ∂f/∂x = y + 2x
      (let ((dx (c:diff f x)))
        (is (sym:expr-p dx)))
      ;; ∂f/∂y = x
      (let ((dy (c:diff f y)))
        (is (sym:expr-p dy))))))

(deftest test-jacobian-computation
  "Test Jacobian matrix computation"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    ;; Vector function f(x,y) = [x^2 + y, x*y]
    (let ((f1 (c:+ (c:^ x 2) y))
          (f2 (c:* x y)))
      ;; Jacobian should be 2x2 matrix of partial derivatives
      (let ((jac-f1 (c:grad f1 x y))
            (jac-f2 (c:grad f2 x y)))
        (is (listp jac-f1))
        (is (listp jac-f2))
        (is (= (length jac-f1) 2))
        (is (= (length jac-f2) 2))))))

(deftest test-hessian-computation
  "Test Hessian matrix computation"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    ;; f(x,y) = x^2*y + x*y^2
    (let ((f (c:+ (c:* (c:^ x 2) y) (c:* x (c:^ y 2)))))
      ;; Second derivatives
      (let ((fxx (c:diff (c:diff f x) x))
            (fxy (c:diff (c:diff f x) y))
            (fyy (c:diff (c:diff f y) y)))
        (is (sym:expr-p fxx))
        (is (sym:expr-p fxy))  
        (is (sym:expr-p fyy))))))

(deftest test-higher-order-derivatives
  "Test higher-order derivatives"
  (let ((x (c:var 'x)))
    ;; f(x) = x^4
    (let* ((f (c:^ x 4))
           (f1 (c:diff f x))         ; 4x^3
           (f2 (c:diff f1 x))        ; 12x^2  
           (f3 (c:diff f2 x))        ; 24x
           (f4 (c:diff f3 x)))       ; 24
      (is (sym:expr-p f1))
      (is (sym:expr-p f2))
      (is (sym:expr-p f3)) 
      (is (sym:expr-p f4)))))

(deftest test-product-rule
  "Test product rule in differentiation"
  (let ((x (c:var 'x)))
    ;; f(x) = x * sin(x), f'(x) = sin(x) + x*cos(x)
    (let* ((f (c:* x (c:sin x)))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      ;; Result should be a sum (from product rule)
      (is (eq (sym:expr-op df) 'epsilon.compute:+)))))

(deftest test-quotient-rule
  "Test quotient rule in differentiation"
  (let ((x (c:var 'x)))
    ;; f(x) = x / sin(x), f'(x) = (sin(x) - x*cos(x)) / sin^2(x)
    (let* ((f (c:/ x (c:sin x)))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      ;; Result should be a division (from quotient rule)
      (is (eq (sym:expr-op df) 'epsilon.compute:/)))))

(deftest test-exponential-derivatives
  "Test derivatives of exponential functions"
  (let ((x (c:var 'x)))
    ;; d/dx(e^x) = e^x
    (let* ((f (c:exp x))
           (df (c:diff f x)))
      (is (sym:expr-p df)))
    
    ;; d/dx(e^(x^2)) = 2x * e^(x^2)
    (let* ((f (c:exp (c:^ x 2)))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      (is (eq (sym:expr-op df) 'epsilon.compute:*)))))

(deftest test-logarithmic-derivatives
  "Test derivatives of logarithmic functions"
  (let ((x (c:var 'x)))
    ;; d/dx(log(x)) = 1/x
    (let* ((f (c:log x))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      (is (eq (sym:expr-op df) 'epsilon.compute:/)))
    
    ;; d/dx(log(x^2)) = 2/x (via chain rule)
    (let* ((f (c:log (c:^ x 2)))
           (df (c:diff f x)))
      (is (sym:expr-p df)))))

(deftest test-trigonometric-derivatives
  "Test derivatives of trigonometric functions"
  (let ((x (c:var 'x)))
    ;; d/dx(sin(x)) = cos(x)
    (let* ((f (c:sin x))
           (df (c:diff f x)))
      (is (sym:expr-p df))
      (is (eq (sym:expr-op df) 'epsilon.compute:*))
      ;; Should contain cos as a factor
      (let ((factors (sym:expr-args df)))
        (is (some (lambda (factor)
                    (and (sym:expr-p factor)
                         (eq (sym:expr-op factor) 'epsilon.compute:cos)))
                  factors))))
    
    ;; d/dx(cos(x)) = -sin(x)  
    (let* ((f (c:cos x))
           (df (c:diff f x)))
      (is (sym:expr-p df)))))

;;; Reverse-Mode Automatic Differentiation Tests

(deftest test-reverse-mode-basic
  "Test basic reverse-mode differentiation"
  (let* ((x (c:var 'x))
         (expr (c:* x x)))  ; x^2
    (let ((grad (c:gradient expr '(x) '((x . 3)))))
      (is (listp grad))
      (is (= (length grad) 1))
      (is (= (first grad) 6)))))  ; 2*3 = 6

(deftest test-gradient-multivariate
  "Test gradient of multivariate function"
  ;; f(x,y) = x^2 + y^2, gradient should be (2x, 2y)
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (expr (c:+ (c:* x x) (c:* y y))))
    (let ((grad (c:gradient expr '(x y) '((x . 2) (y . 3)))))
      (is (= (length grad) 2))
      (is (= (first grad) 4))   ; 2*2 = 4
      (is (= (second grad) 6))))) ; 2*3 = 6

(deftest test-chain-rule-reverse
  "Test chain rule in reverse mode"
  ;; f(x) = sin(x^2), f'(x) = 2x*cos(x^2)
  (let* ((x (c:var 'x))
         (expr (c:sin (c:* x x))))
    (let ((grad (c:gradient expr '(x) '((x . 1)))))
      (is (= (length grad) 1))
      ;; At x=1: f'(1) = 2*1*cos(1) = 2*cos(1)
      (is (approximately-equal (first grad) (* 2 (cos 1)) 1e-10)))))

(deftest test-jacobian-comparison
  "Test that forward and reverse Jacobians match"
  ;; f1(x,y) = x^2 + y, f2(x,y) = x + y^2
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f1 (c:+ (c:* x x) y))
         (f2 (c:+ x (c:* y y))))
    (let ((jac-forward (c:jacobian (list f1 f2) '(x y) '((x . 2) (y . 3)) :mode :forward))
          (jac-reverse (c:jacobian (list f1 f2) '(x y) '((x . 2) (y . 3)) :mode :reverse)))
      (is (arrayp jac-forward))
      (is (arrayp jac-reverse))
      (is (equal (array-dimensions jac-forward) (array-dimensions jac-reverse)))
      ;; Elements should match
      (loop for i from 0 below 2
            do (loop for j from 0 below 2
                    do (is (= (aref jac-forward i j) (aref jac-reverse i j))))))))

(deftest test-hessian-mixed-mode
  "Test Hessian computation using mixed-mode AD"
  ;; f(x,y) = x^2*y + y^3
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (expr (c:+ (c:* (c:* x x) y) (c:* y (c:* y y)))))
    (let ((hess (c:hessian expr '(x y) '((x . 2) (y . 1)))))
      (is (arrayp hess))
      (is (equal (array-dimensions hess) '(2 2)))
      ;; Hessian should be symmetric
      (is (= (aref hess 0 1) (aref hess 1 0))))))

(deftest test-autodiff-with-macro
  "Test with-autodiff macro functionality"
  (let* ((x (c:var 'x))
         (expr (c:* x x)))
    
    ;; Test forward mode
    (c:with-autodiff (:mode :forward)
      (let ((result (c:forward-diff expr 'x)))
        (is (numberp result))))
    
    ;; Test reverse mode
    (c:with-autodiff (:mode :reverse)
      (let ((grad (c:gradient expr '(x) '((x . 3)))))
        (is (listp grad))
        (is (= (first grad) 6))))))