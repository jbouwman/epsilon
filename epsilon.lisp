(in-package :cl-user)

(require :sb-cltl2)
(require :sb-rotate-byte)

(defun load-order (entry)
  (etypecase entry
    (string (list (list entry)))
    (cons   (mapcar (lambda (child)
                      (cons (car entry) child))
                    (mapcan #'load-order (cdr entry))))))

(defun load-file (file)
  (let ((in (format nil "src/~{~A~^/~}.lisp" file))
        (out (format nil "src/~{~A~^/~}.fasl" file)))
    (compile-file in)
    (load out)))

(defun load-epsilon ()
  (with-open-file (stream "epsilon.sexp")
    (mapc #'load-file (mapcan #'load-order (read stream)))))
