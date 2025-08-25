(defpackage epsilon.compute.debug-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.debug-tests)

(deftest test-symbol-identity
  "Debug test to check symbol identity"
  (let ((x (c:var 'x))
        (y (c:var 'y)))
    (let ((sum (c:+ x y)))
      (format t "~%Expression: ~A~%" sum)
      (format t "Is expr-p: ~A~%" (sym:expr-p sum))
      (format t "Op: ~A~%" (sym:expr-op sum))
      (format t "Op package: ~A~%" (symbol-package (sym:expr-op sum)))
      (format t "CL:+ : ~A~%" 'cl:+)
      (format t "epsilon.compute:+ : ~A~%" 'epsilon.compute:+)
      (format t "Op eq cl:+? ~A~%" (eq (sym:expr-op sum) 'cl:+))
      (format t "Op eq epsilon.compute:+? ~A~%" (eq (sym:expr-op sum) 'epsilon.compute:+))
      (is t))))