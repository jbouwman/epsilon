(in-package #:epsilon.sys.ffi.sys)

(defun quoted-form-p (form)
  (and (proper-list-p form)
       (= 2 (length form))
       (eql 'quote (car form))))

(defun constant-form-p (form &optional env)
  (let ((form (if (symbolp form)
                  (macroexpand form env)
                  form)))
    (or (quoted-form-p form)
        (constantp form env))))

(defun constant-form-value (form &optional env)
  (declare (ignorable env))
  (cond
    ((quoted-form-p form)
     (second form))
    (t
     #+clozure
     (ccl::eval-constant form)
     #+sbcl
     (sb-int:constant-form-value form env)
     #-(or clozure sbcl)
     (eval form))))
