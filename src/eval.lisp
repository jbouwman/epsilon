(in-package #:encode)

(defun partial (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))
