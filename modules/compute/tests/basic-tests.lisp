(defpackage epsilon.compute.basic-tests
  (:use cl epsilon.test))

(in-package epsilon.compute.basic-tests)

(deftest test-module-loads
  "Test that the module loads without errors"
  (is (find-package 'epsilon.compute))
  (is (find-package 'epsilon.compute.types))
  (is (find-package 'epsilon.compute.symbolic))
  (is (find-package 'epsilon.compute.simplify)))

(deftest test-basic-functionality  
  "Test basic symbolic functionality works"
  ;; Test we can create variables and expressions
  (let ((x (epsilon.compute:var 'x))
        (y (epsilon.compute:var 'y)))
    (is (epsilon.compute.symbolic:var-p x))
    (is (epsilon.compute.symbolic:var-p y))
    ;; Test we can create expressions  
    (let ((expr (epsilon.compute:+ x y)))
      (is (epsilon.compute.symbolic:expr-p expr))
      (is (eq (epsilon.compute.symbolic:expr-op expr) 'epsilon.compute:+)))
    ;; Test differentiation works
    (let ((expr (epsilon.compute:^ x 2)))
      (let ((deriv (epsilon.compute:diff expr x)))
        (is (epsilon.compute.symbolic:expr-p deriv))))))
