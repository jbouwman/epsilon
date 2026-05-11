;;;; epsilon.scheduler.fiber-discipline-seed -- ground-truth annotations
;;;; for the symbols the scheduler / lint / watchdog reason about.
;;;;
;;;; Every annotation here is the canonical answer to "is calling this
;;;; from a coroutine safe?". Authors of new code in epsilon-contrib
;;;; should add to this list when they introduce a new wait primitive
;;;; (or use DEFINE-FIBER-FN at the definition site).
;;;;
;;;; The split between this seed file and fiber-discipline.lisp is
;;;; deliberate: the macros / accessors (fiber-discipline.lisp) have
;;;; minimal deps and can be required by other modules; this seed file
;;;; drags in the union of every package whose symbols it annotates,
;;;; so its load order is gated on those packages being ready.

(defpackage :epsilon.scheduler.fiber-discipline-seed
  (:use :cl)
  (:import (epsilon.scheduler.fiber-discipline fd)
           (epsilon.sys.semaphore sem)
           (epsilon.sys.lock lock)
           (epsilon.sys.thread thread)
           (epsilon.scheduler.io-wait io-wait)
           (epsilon.scheduler.fiber-io fio)
           (epsilon.scheduler sched)))

(in-package :epsilon.scheduler.fiber-discipline-seed)

;;; ---------------------------------------------------------------------------
;;; SBCL-side primitives -- always present; no package require needed.
;;; ---------------------------------------------------------------------------

(fd:declaim-fiber-kind sb-ext:with-timeout :blocks-thread
  :reason "SIGALRM cannot preempt non-interruptible foreign calls")

(fd:declaim-fiber-kind sb-thread:condition-wait :blocks-thread
  :reason "futex wait via SBCL condition variable")

(fd:declaim-fiber-kind sb-thread:grab-mutex :blocks-thread
  :reason "futex wait when contended")

(fd:declaim-fiber-kind sb-thread:join-thread :blocks-thread
  :reason "thread join is a syscall wait")

(fd:declaim-fiber-kind sleep :blocks-thread
  :reason "blocks the OS thread; use epsilon.scheduler:coroutine-sleep"
  :alternative epsilon.scheduler:coroutine-sleep)

(fd:declaim-fiber-kind read-byte :blocks-thread
  :reason "synchronous read; use fiber-io:fiber-read"
  :alternative epsilon.scheduler.fiber-io:fiber-read)

(fd:declaim-fiber-kind read-sequence :blocks-thread
  :reason "synchronous read; use fiber-io:fiber-read"
  :alternative epsilon.scheduler.fiber-io:fiber-read)

(fd:declaim-fiber-kind sb-unix:unix-read :blocks-thread
  :reason "raw read(2); only safe inside fiber-aware loops that pre-park"
  :alternative epsilon.scheduler.fiber-io:fiber-read)

;;; ---------------------------------------------------------------------------
;;; epsilon.sys.* primitives -- the wrappers most epsilon-contrib code
;;; reaches for. The semaphore wait was the smoking gun in the
;;; 2026-05-04 manual-site carrier-stall investigation: net-adapters'
;;; read-into used it to wait on a poll-read waker, pinning the carrier
;;; 5 s at a time.
;;; ---------------------------------------------------------------------------

(fd:declaim-fiber-kind epsilon.sys.semaphore:wait-on-semaphore :blocks-thread
  :reason "futex wait; carrier blocks for the timeout window")

(fd:declaim-fiber-kind epsilon.sys.thread:make-thread :cpu-bound
  :reason "spawning a thread is non-blocking; the new thread may itself block")

;;; ---------------------------------------------------------------------------
;;; epsilon.scheduler canonical fiber-aware primitives
;;;
;;; These are the symbols fiber-aware code MUST use when it has to
;;; wait. The lint sees these as the recommended alternatives.
;;; ---------------------------------------------------------------------------

(fd:declaim-fiber-kind epsilon.scheduler.io-wait:park-on-fd :fiber-aware
  :reason "parks the coroutine via the platform reactor")

(fd:declaim-fiber-kind epsilon.scheduler:coroutine-sleep :fiber-aware
  :reason "parks the coroutine on the carrier's timer heap")

(fd:declaim-fiber-kind epsilon.scheduler:coroutine-wait-until :fiber-aware
  :reason "parks the coroutine until predicate fires")

(fd:declaim-fiber-kind epsilon.scheduler:run-blocking :fiber-aware
  :reason "offloads to a non-carrier thread; carrier resumes")

(fd:declaim-fiber-kind epsilon.scheduler.fiber-io:fiber-read :fiber-aware
  :reason "EAGAIN loop with park-on-fd")

(fd:declaim-fiber-kind epsilon.scheduler.fiber-io:fiber-wait-readable :fiber-aware
  :reason "park-on-fd shorthand for :IN")

(fd:declaim-fiber-kind epsilon.scheduler.fiber-io:fiber-wait-writable :fiber-aware
  :reason "park-on-fd shorthand for :OUT")

(fd:declaim-fiber-kind epsilon.scheduler.fiber-io:fiber-read-exact :fiber-aware
  :reason "looped fiber-read")

(fd:declaim-fiber-kind epsilon.scheduler.fiber-io:fiber-sleep-ms :fiber-aware
  :reason "millisecond fiber-aware sleep")
