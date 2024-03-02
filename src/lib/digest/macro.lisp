(defpackage #:lib.digest.macro
  (:use
   #:cl)
  (:export
   #:circular-list-subseq
   #:make-circular-list
   #:massage-symbol
   #:quotationp
   #:unquote))

;;;; macro-utils.lisp -- things to make compiler macros easier

(in-package #:lib.digest.macro)

(defun quotationp (thing)
  (and (consp thing) (consp (rest thing))
       (null (cddr thing)) (eq (first thing) 'quote)))

(defun unquote (thing)
  (if (quotationp thing) (second thing) thing))

(defun massage-symbol (symbol)
  (let ((package (symbol-package symbol))
        (ironclad (load-time-value (find-package :ironclad))))
    (cond
      ((eq package ironclad) symbol)
      ((eq package (load-time-value (find-package :keyword)))
       (find-symbol (symbol-name symbol) ironclad))
      (t nil))))

;;; a few functions that are useful during compilation

(defun make-circular-list (&rest elements)
  (let ((list (copy-seq elements)))
    (setf (cdr (last list)) list)))

;;; SUBSEQ is defined to error on circular lists, so we define our own

(defun circular-list-subseq (list start end)
  (let* ((length (- end start))
         (subseq (make-list length)))
    (do ((i 0 (1+ i))
         (list (nthcdr start list) (cdr list))
         (xsubseq subseq (cdr xsubseq)))
        ((>= i length) subseq)
      (setf (first xsubseq) (first list)))))
