;;;; Test the epsilon.compute module with native acceleration

;; The module is already loaded by epsilon
(in-package :epsilon.compute)

(format t "~%Testing epsilon.compute module with native acceleration~%")
(format t "========================================================~%~%")

;; Test basic symbolic operations
(let ((x (var 'x))
      (y (var 'y)))
  
  ;; Create expression: x^2 + 2*x*y + y^2
  (let ((expr (+ (^ x 2) (* 2 x y) (^ y 2))))
    (format t "Expression: x^2 + 2xy + y^2~%")
    (format t "Symbolic: ~A~%~%" expr)
    
    ;; Evaluate with specific values
    (let ((result (evaluate expr '((x . 3) (y . 4)))))
      (format t "Evaluated at x=3, y=4: ~A~%" result)
      (format t "Expected: ~A~%~%" (+ (* 3 3) (* 2 3 4) (* 4 4))))))

;; Test matrix operations with native backend
(format t "~%Testing matrix operations:~%")
(let ((a #2A((1.0 2.0) (3.0 4.0)))
      (b #2A((5.0 6.0) (7.0 8.0))))
  (format t "Matrix A: ~A~%" a)
  (format t "Matrix B: ~A~%" b)
  (let ((c (native-matrix-multiply a b)))
    (format t "A * B = ~A~%" c)
    (format t "Expected: [[19 22] [43 50]]~%")))

;; Test vector operations
(format t "~%Testing vector operations:~%")
(let ((v1 #(1.0 2.0 3.0))
      (v2 #(4.0 5.0 6.0)))
  (format t "Vector v1: ~A~%" v1)
  (format t "Vector v2: ~A~%" v2)
  (let ((dot (native-dot-product v1 v2)))
    (format t "v1 · v2 = ~A~%" dot)
    (format t "Expected: ~A~%" (+ (* 1 4) (* 2 5) (* 3 6)))))

;; Test differentiation
(format t "~%Testing symbolic differentiation:~%")
(let ((x (var 'x)))
  (let ((f (+ (^ x 3) (* 2 (^ x 2)) x 1)))
    (format t "f(x) = x^3 + 2x^2 + x + 1~%")
    (let ((df (diff f x)))
      (format t "f'(x) = ~A~%" df)
      (let ((simplified (simplify df)))
        (format t "Simplified: ~A~%" simplified)))))

;; Test Einstein summation
(format t "~%Testing Einstein summation:~%")
(let ((result (einsum "ij,jk->ik" 
                     #2A((1 2) (3 4))
                     #2A((5 6) (7 8)))))
  (format t "einsum('ij,jk->ik', A, B) = ~A~%" result))

(format t "~%Native backend status: ~A~%" *native-backend-available*)
(format t "~%All tests completed!~%")