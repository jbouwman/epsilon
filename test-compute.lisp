;;; Standalone test for epsilon.compute module

;; Load the required files directly
(load "/home/jbouwman/git/epsilon-3/modules/compute/src/types.lisp")
(load "/home/jbouwman/git/epsilon-3/modules/compute/src/symbolic.lisp")
(load "/home/jbouwman/git/epsilon-3/modules/compute/src/simplify.lisp")
(load "/home/jbouwman/git/epsilon-3/modules/compute/src/compute.lisp")

;; Test the functionality
(in-package :epsilon.compute)

(format t "~%=== Testing Epsilon.Compute Module ===~%~%")

;; Test 1: Basic symbolic variables
(format t "Test 1: Creating symbolic variables~%")
(let ((x (var 'x))
      (y (var 'y)))
  (format t "  x = ~A~%" x)
  (format t "  y = ~A~%" y)
  (format t "  x + y = ~A~%" (+ x y)))

;; Test 2: Simplification
(format t "~%Test 2: Simplification~%")
(let ((x (var 'x)))
  (format t "  x + 0 = ~A~%" (+ x 0))
  (format t "  simplify(x + 0) = ~A~%" (simplify (+ x 0)))
  (format t "  x * 1 = ~A~%" (* x 1))
  (format t "  simplify(x * 1) = ~A~%" (simplify (* x 1)))
  (format t "  x - x = ~A~%" (- x x))
  (format t "  simplify(x - x) = ~A~%" (simplify (- x x))))

;; Test 3: Differentiation
(format t "~%Test 3: Differentiation~%")
(let ((x (var 'x)))
  (format t "  d/dx(x) = ~A~%" (diff x x))
  (format t "  d/dx(x^2) = ~A~%" (diff (^ x 2) x))
  (format t "  simplify(d/dx(x^2)) = ~A~%" (simplify (diff (^ x 2) x)))
  (format t "  d/dx(sin(x)) = ~A~%" (diff (sin x) x))
  (format t "  d/dx(x^3 + 2x) = ~A~%" (diff (+ (^ x 3) (* 2 x)) x)))

;; Test 4: Substitution and evaluation
(format t "~%Test 4: Substitution and Evaluation~%")
(let ((x (var 'x))
      (y (var 'y)))
  (let ((expr (+ (^ x 2) (* 2 x y) (^ y 2))))
    (format t "  Expression: ~A~%" expr)
    (format t "  Substitute x=3: ~A~%" (substitute expr '((x . 3))))
    (format t "  Evaluate at x=3, y=4: ~A~%" (evaluate expr '((x . 3) (y . 4))))))

;; Test 5: Trigonometric identities
(format t "~%Test 5: Trigonometric Identities~%")
(let ((x (var 'x)))
  (let ((expr (+ (^ (sin x) 2) (^ (cos x) 2))))
    (format t "  sin²(x) + cos²(x) = ~A~%" expr)
    (format t "  simplify(sin²(x) + cos²(x)) = ~A~%" (simplify expr))))

;; Test 6: Chain rule
(format t "~%Test 6: Chain Rule~%")
(let ((x (var 'x)))
  (let ((expr (sin (^ x 2))))
    (format t "  f(x) = sin(x²)~%")
    (format t "  f'(x) = ~A~%" (diff expr x))
    (format t "  simplified = ~A~%" (simplify (diff expr x)))))

;; Test 7: Product rule
(format t "~%Test 7: Product Rule~%")
(let ((x (var 'x)))
  (let ((expr (* x (sin x))))
    (format t "  f(x) = x·sin(x)~%")
    (format t "  f'(x) = ~A~%" (diff expr x))
    (format t "  simplified = ~A~%" (simplify (diff expr x)))))

;; Test 8: Matrix operations
(format t "~%Test 8: Matrix Operations~%")
(let ((A (var 'A))
      (B (var 'B)))
  (format t "  transpose(A) = ~A~%" (transpose A))
  (format t "  det(A) = ~A~%" (det A))
  (format t "  A·B = ~A~%" (dot A B))
  (format t "  trace(A) = ~A~%" (trace A)))

(format t "~%=== All Tests Complete ===~%")