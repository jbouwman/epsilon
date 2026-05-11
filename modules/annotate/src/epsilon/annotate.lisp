;;;; annotation mechanism via the #@ reader macro.  Annotations sit
;;;; outside the form they annotate and compose naturally:
;;;;
;;;;   #@(:timeout 30)
;;;;   (deftest test-foo ...)
(defpackage epsilon.annotate
  (:use :cl)
  (:export *pending-annotations*
           with-annotations
           consume-annotations
           annotations
           set-annotations))

(defvar *pending-annotations*
  nil
  "Alist of annotations currently in scope, set by #@ reader macro.")

(defmacro with-annotations ((key value) &body body)
  "Execute BODY with an annotation binding in scope."
  `(let ((*pending-annotations* (cons (cons ,key ,value) *pending-annotations*))) ,@body))

(defun consume-annotations ()
  "Return current pending annotations and reset to NIL."
  (prog1 *pending-annotations* (setf *pending-annotations* nil)))

(defun annotations (symbol)
  "Return the stored annotations alist for SYMBOL."
  (get symbol 'annotations))

(defun set-annotations (symbol annotations)
  "Store ANNOTATIONS alist on SYMBOL."
  (setf (get symbol 'annotations) annotations))

(defsetf annotations
  set-annotations)
