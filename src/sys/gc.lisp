(defpackage #:epsilon.sys.gc
  (:use #:cl)
  (:export
   #:finalize
   #:cancel-finalization))

(in-package #:epsilon.sys.gc)

;;;; Finalizers

(defun finalize (object function)
  "Pushes a new @code{function} to the @code{object}'s list of
   finalizers. @code{function} should take no arguments. Returns
   @code{object}.

   @b{Note:} @code{function} should not attempt to look at
   @code{object} by closing over it because that will prevent it from
   being garbage collected."
  (sb-ext:finalize object function))

(defun cancel-finalization (object)
  "Cancels all of @code{object}'s finalizers, if any."
  (sb-ext:cancel-finalization object))
