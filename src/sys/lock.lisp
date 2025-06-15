(defpackage #:epsilon.sys.lock
  (:use
   #:cl)
  (:export
   #:acquire-lock
   #:lock
   #:lock-name
   #:lock-p
   #:make-lock
   #:make-recursive-lock
   #:release-lock
   #:with-lock
   #:with-recursive-lock-held))

(in-package #:epsilon.sys.lock)

(defun acquire-lock (lock &key (wait t) timeout)
  "Acquire the lock LOCK for the calling thread.

  WAIT governs what happens if the lock is not available: if WAIT
  is true, the calling thread will wait until the lock is available
  and then acquire it; if WAIT is NIL, ACQUIRE-LOCK will return
  immediately.

  If WAIT is true, TIMEOUT may specify a maximum amount of seconds to
  wait for the lock to become available.

  ACQUIRE-LOCK returns T if the lock was acquired and NIL
  otherwise.

  This specification does not define what happens if a thread
  attempts to acquire a lock that it already holds. For applications
  that require locks to be safe when acquired recursively, see instead
  MAKE-RECURSIVE-LOCK and friends."
  (check-type timeout (or null (real 0)))
  (sb-thread:grab-mutex lock :waitp wait :timeout timeout))

(deftype lock ()
  'sb-thread:mutex)

(defun lock-name (lock)
  (sb-thread:mutex-name lock))

(defun lock-p (object)
  (typep object 'sb-thread:mutex))

(defun make-lock (&optional name)
  (sb-thread:make-mutex :name (or name "Anonymous lock")))

(defun release-lock (lock)
  "Release LOCK. It is an error to call this unless
  the lock has previously been acquired (and not released) by the same
  thread. If other threads are waiting for the lock, the
  ACQUIRE-LOCK call in one of them will now be able to continue.

  Returns the lock."
  (sb-thread:release-mutex lock)
  lock)

(defmacro with-lock ((place) &body body)
  "Evaluates BODY with the lock named by PLACE, the value of which
  is a lock created by MAKE-LOCK. Before the forms in BODY are
  evaluated, the lock is acquired as if by using ACQUIRE-LOCK. After the
  forms in BODY have been evaluated, or if a non-local control transfer
  is caused (e.g. by THROW or SIGNAL), the lock is released as if by
  RELEASE-LOCK.

  Note that if the debugger is entered, it is unspecified whether the
  lock is released at debugger entry or at debugger exit when execution
  is restarted."
  `(sb-thread:with-mutex (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (sb-thread:make-mutex :name (or name "Anonymous lock")))

(defmacro with-recursive-lock-held ((place) &body body)
  `(sb-thread:with-recursive-lock (,place)
     ,@body))
