;;;; annotation.lisp - General-purpose annotation system for Epsilon
;;;;
;;;; Provides a syntactically orthogonal annotation mechanism via the
;;;; #@ reader macro.  Annotations sit outside the form they annotate
;;;; and compose naturally:
;;;;
;;;;   #@(:category :integration)
;;;;   #@(:timeout 30)
;;;;   (deftest test-foo ...)
;;;;
;;;; The reader macro expands to (with-annotations ...) which pushes
;;;; annotations onto *pending-annotations* for consumption by macros
;;;; like deftest.

(defpackage epsilon.annotation
  (:use :cl)
  (:export
   #:*pending-annotations*
   #:with-annotations
   #:consume-annotations
   #:get-annotation
   #:annotations
   #:set-annotations))

(in-package :epsilon.annotation)

(defvar *pending-annotations* nil
  "Alist of annotations currently in scope, set by #@ reader macro.")

(defmacro with-annotations ((key value) &body body)
  "Execute BODY with an annotation binding in scope."
  `(let ((*pending-annotations* (cons (cons ,key ,value) *pending-annotations*)))
     ,@body))

(defun consume-annotations ()
  "Return current pending annotations and reset to NIL."
  (prog1 *pending-annotations*
    (setf *pending-annotations* nil)))

(defun get-annotation (key &optional default)
  "Get annotation value from pending annotations."
  (let ((pair (assoc key *pending-annotations*)))
    (if pair (cdr pair) default)))

(defun annotations (symbol)
  "Return the stored annotations alist for SYMBOL."
  (get symbol 'annotations))

(defun set-annotations (symbol annotations)
  "Store ANNOTATIONS alist on SYMBOL."
  (setf (get symbol 'annotations) annotations))

(defsetf annotations set-annotations)
