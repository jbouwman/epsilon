(defpackage #:sys.sync.semaphore
  (:use
   #:cl)
  (:export
   #:make-semaphore
   #:semaphore
   #:semaphore-p
   #:signal-semaphore
   #:wait-on-semaphore))

;;; Semaphores

(in-package #:sys.sync.semaphore)

(deftype semaphore ()
  'sb-thread:semaphore)

(defun semaphore-p (object)
  "Returns T if OBJECT is a semaphore; returns NIL otherwise."
  (typep object 'semaphore))

(defun make-semaphore (&key name (count 0))
  "Create a semaphore with the supplied NAME and initial counter value COUNT."
  (sb-thread:make-semaphore :name name :count count))

(defun signal-semaphore (semaphore &key (count 1))
  "Increment SEMAPHORE by COUNT. If there are threads waiting on this
semaphore, then COUNT of them are woken up."
  (sb-thread:signal-semaphore semaphore count))

(defun wait-on-semaphore (semaphore &key timeout)
  "Decrement the count of SEMAPHORE by 1 if the count is larger than zero.

If count is zero, blocks until the semaphore can be decremented.
Returns generalized boolean T on success.

If TIMEOUT is given, it is the maximum number of seconds to wait. If the count
cannot be decremented in that time, returns NIL without decrementing the count."
  (cond
    ((and timeout (zerop timeout))
     (sb-thread:try-semaphore semaphore))
    (t
     (if (sb-thread:wait-on-semaphore semaphore :timeout timeout)
         t nil))))
