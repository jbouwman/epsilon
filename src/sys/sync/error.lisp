(defpackage #:epsilon.sys.sync.error
  (:use
   #:cl)
  (:export
   #:thread-exit
   #:thread-exit-condition
   #:thread-error))

(in-package #:epsilon.sys.sync.error)

(defun bool (thing) (if thing t nil))

(define-condition thread-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream (message condition)))))

(define-condition thread-exit (error)
  ((exit-condition :initarg :condition
                   :reader thread-exit-condition))
  (:report (lambda (condition stream)
             (format stream "Thread exited with condition: ~A"
                     (thread-exit-condition condition)))))
