(defpackage epsilon.compute.simplify-debug
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.simplify-debug)

(deftest test-simplification-debug
  "Debug simplification"
  (let ((x (c:var 'x)))
    ;; x + 0
    (let* ((expr (c:+ x 0))
           (simplified (c:simplify expr)))
      (format t "~%x + 0 = ~A~%" expr)
      (format t "simplified = ~A~%" simplified)
      (format t "is var? ~A~%" (sym:var-p simplified))
      (is t))))