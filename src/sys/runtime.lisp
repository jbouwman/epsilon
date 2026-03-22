;;;; Runtime utilities -- merged from sys/timeout.lisp and sys/gc.lisp

(defpackage #:epsilon.sys.runtime
  (:use #:cl)
  (:export
   ;; Timeout (from sys/timeout.lisp)
   #:timeout
   #:with-timeout
   ;; Finalizers (from sys/gc.lisp)
   #:finalize
   #:cancel-finalization)
  (:enter t))

;;;; Timeout

(deftype timeout ()
  'sb-ext:timeout)

(defmacro with-timeout ((timeout) &body body)
  `(sb-ext:with-timeout ,timeout
     ,@body))

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
