(in-package :cl-user)

(require :sb-cltl2)
(require :sb-posix)
(require :sb-bsd-sockets)
(require :sb-rotate-byte)

(defun load-order (entry)
  (etypecase entry
    (string (list (list entry)))
    (cons   (mapcar (lambda (child)
                      (cons (car entry) child))
                    (mapcan #'load-order (cdr entry))))))

(defun read-order (dir file)
  (with-open-file (stream (format nil "~A/~A" dir file))
    (mapcar (lambda (x)
              (cons dir x))
            (mapcan #'load-order (read stream)))))

(defun load-file (file)
  (let ((in (format nil "~{~A~^/~}.lisp" file))
        (out (format nil "~{~A~^/~}.fasl" file)))
    (compile-file in :verbose nil :print nil)
    (load out)))

(defun load-files (files)
  (dolist (file files)
    (load-file file)))

(defun load-epsilon ()
  (load-files (read-order "src" "epsilon.sexp")))

(defun load-epsilon-tests ()
  (load-files (read-order "tests" "tests.sexp")))
