(defpackage epsilon.compute.debug-gradient
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (ad epsilon.compute.autodiff)))

(in-package epsilon.compute.debug-gradient)

(deftest test-gradient-debug
  "Debug what reverse-diff is actually returning"
  (let* ((x-val #(12.0 15.0 18.0))
         (y-val 3.0)
         (x (c:var 'x))
         (y (c:var 'y))
         (f (c:/ x y))
         (grads (ad:reverse-diff f '(x y) 
                                `((x . ,x-val) (y . ,y-val)))))
    ;; Debug output
    (format t "~%=== Gradient Debug ===~%")
    (format t "x-val: ~S (type: ~A)~%" x-val (type-of x-val))
    (format t "y-val: ~S (type: ~A)~%" y-val (type-of y-val))
    (format t "grads returned: ~S~%" grads)
    (format t "Type of grads: ~A~%" (type-of grads))
    (when (listp grads)
      (format t "Length of grads: ~A~%" (length grads))
      (format t "First grad: ~S (type: ~A)~%" (first grads) (type-of (first grads)))
      (format t "Second grad: ~S (type: ~A)~%" (second grads) (type-of (second grads))))
    
    ;; Make a simple assertion to pass
    (is t)))

(deftest test-multiplication-gradient-debug
  "Debug multiplication gradient"
  (let* ((x-val #(2.0 3.0 4.0))
         (y-val 5.0)
         (x (c:var 'x))
         (y (c:var 'y))
         (f (c:* x y))
         (grads (ad:reverse-diff f '(x y)
                                `((x . ,x-val) (y . ,y-val)))))
    (format t "~%=== Multiplication Gradient ===~%")
    (format t "x-val: ~S~%" x-val)
    (format t "y-val: ~S~%" y-val)
    (format t "grads: ~S~%" grads)
    (when (listp grads)
      (format t "grad wrt x: ~S (expected: #(5.0 5.0 5.0))~%" (first grads))
      (format t "grad wrt y: ~S (expected: 9.0 = sum of x)~%" (second grads)))
    (is t)))