;;;; io-wait -- Park a coroutine on file-descriptor readiness
;;;;
;;;; Bridges the scheduler's coroutine-park mechanism to a platform I/O
;;;; reactor.  Both platform managers (epoll on Linux, kqueue on Darwin)
;;;; expose the package EPSILON.NET.REACTOR with the same API; one of
;;;; them is loaded depending on platform.  The scheduler stays
;;;; reactor-agnostic: the application installs a callback registrar
;;;; via *FD-WAIT-REGISTER* at startup, and PARK-ON-FD uses it.
;;;;
;;;; Flow:
;;;;   coroutine calls (park-on-fd fd events)
;;;;     -> *fd-wait-register* registers a one-shot callback on fd
;;;;     -> coroutine parks with a predicate that checks a `fired` flag
;;;;   reactor thread observes fd readiness
;;;;     -> invokes the callback
;;;;     -> callback sets fired=t AND calls wake-coroutine-from-foreign,
;;;;        which routes the coroutine to its owner carrier's incoming
;;;;        queue and signals that carrier's idle-sem
;;;;   owner carrier drains incoming, fiber-switches in, park returns.

(defpackage :epsilon.scheduler.io-wait
  (:use :cl)
  (:import (epsilon.scheduler.coroutine coro)
            (epsilon.scheduler sched))
  (:export
   #:*fd-wait-register*
   #:park-on-fd))

(defvar *fd-wait-register* nil
  "A function (FD EVENTS CALLBACK) that registers a one-shot callback
for when FD is ready for one of EVENTS (a list of :IN, :OUT, etc.).
The callback is invoked by the reactor thread with one argument (the
event) when readiness is reached.  Must be set once at application
startup by the platform layer; left NIL for environments without a
reactor (in which case PARK-ON-FD signals an error).")

(defstruct fd-wait-slot
  "Heap-allocated flag shared between the reactor callback and the
PARK-ON-FD wake predicate."
  (fired nil :type boolean))

(defun %make-wait-callback (slot coro)
  "Build the reactor callback as a top-level closure so the SBCL
compiler doesn't try to stack-allocate it alongside the caller's
other closures (which triggers a STACK-ALLOCATED-P AVER in the
vendored build)."
  (lambda (event)
    (declare (ignore event))
    (setf (fd-wait-slot-fired slot) t)
    (sched:wake-coroutine-from-foreign coro)))

(defun %make-wake-predicate (slot)
  (lambda () (fd-wait-slot-fired slot)))

(defun park-on-fd (fd events &key timeout cancellation)
  "Park the current coroutine until FD is ready for one of EVENTS
(:IN, :OUT, :ERR, :HUP, ...) or TIMEOUT seconds elapse.

Returns T if the fd signalled readiness, NIL on timeout.  Signals
FIBER-CANCELLED if CANCELLATION fires before either.

Safe to call from deep in a call stack -- the fiber preserves all
intermediate frames across the park.  Must be called from within a
coroutine running under EPSILON.SCHEDULER."
  (let ((register *fd-wait-register*))
    (unless register
      (error "park-on-fd: *fd-wait-register* not set -- no reactor registered"))
    (let ((coro coro:*current-coroutine*))
      (unless coro
        (error "park-on-fd called outside a coroutine"))
      (let ((slot (make-fd-wait-slot)))
        (funcall register fd events (%make-wait-callback slot coro))
        (coro:coroutine-park :wake-when (%make-wake-predicate slot)
                              :timeout timeout
                              :cancellation cancellation)
        (fd-wait-slot-fired slot)))))
