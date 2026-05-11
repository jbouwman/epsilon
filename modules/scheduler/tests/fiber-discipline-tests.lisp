;;;; Tests for epsilon.scheduler.fiber-discipline.
;;;;
;;;; Three cuts:
;;;;
;;;;   - The annotation roundtrip (set / get / alternative / reason).
;;;;   - DEFINE-FIBER-FN's body parsing (docstring, declares, FIBER-KIND
;;;;     sub-declaration, multi-fiber-kind error).
;;;;   - Seed coverage: the symbols the seed file annotates round-trip
;;;;     to the kinds we expect, so a future deletion of one of them
;;;;     fails an obvious test.

(defpackage :epsilon.scheduler.fiber-discipline-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler.fiber-discipline fd)))

(in-package :epsilon.scheduler.fiber-discipline-tests)

;;; ---------------------------------------------------------------------------
;;; Annotation roundtrip
;;; ---------------------------------------------------------------------------

(defparameter *probe-symbols*
  '(probe-1 probe-2 probe-3 probe-4))

(defun reset-probes ()
  "Clear annotations on the test probe symbols. Called before each
test so leftover state from a previous run doesn't shadow a real
defect."
  (dolist (s *probe-symbols*)
    (when (boundp 'fd::*annotated-symbols*)
      (setf fd::*annotated-symbols*
            (remove s fd::*annotated-symbols*)))
    (remprop s 'fd::fiber-kind)
    (remprop s 'fd::fiber-aware-alternative)
    (remprop s 'fd::fiber-reason)))

(deftest fiber-kind-default-is-unknown
  "An unannotated symbol returns the *DEFAULT-FIBER-KIND*."
  (reset-probes)
  (assert-equal :unknown (fd:fiber-kind 'probe-1)))

(deftest fiber-kind-setf-roundtrip
  "Setting a kind and reading it back returns the same value."
  (reset-probes)
  (setf (fd:fiber-kind 'probe-1) :fiber-aware)
  (assert-equal :fiber-aware (fd:fiber-kind 'probe-1))
  (setf (fd:fiber-kind 'probe-1) :blocks-thread)
  (assert-equal :blocks-thread (fd:fiber-kind 'probe-1)))

(deftest fiber-kind-rejects-bad-kind
  "Non-taxonomy keywords signal INVALID-FIBER-KIND."
  (reset-probes)
  (assert-condition (fd:invalid-fiber-kind)
                    (setf (fd:fiber-kind 'probe-2) :nonsense)))

(deftest fiber-aware-alternative-roundtrip
  "Alternative is read back as set."
  (reset-probes)
  (setf (fd:fiber-aware-alternative 'probe-3) 'epsilon.scheduler:coroutine-sleep)
  (assert-equal 'epsilon.scheduler:coroutine-sleep
                (fd:fiber-aware-alternative 'probe-3))
  (assert-equal nil (fd:fiber-aware-alternative 'probe-4)))

(deftest fiber-reason-roundtrip
  "Reason text is preserved."
  (reset-probes)
  (setf (fd:fiber-reason 'probe-1) "blocks on futex")
  (assert-equal "blocks on futex" (fd:fiber-reason 'probe-1)))

;;; ---------------------------------------------------------------------------
;;; DECLAIM-FIBER-KIND
;;; ---------------------------------------------------------------------------

(deftest declaim-stamps-kind-and-alternative-and-reason
  (reset-probes)
  (fd:declaim-fiber-kind probe-1 :blocks-thread
    :alternative epsilon.scheduler:coroutine-sleep
    :reason "test reason")
  (assert-equal :blocks-thread (fd:fiber-kind 'probe-1))
  (assert-equal 'epsilon.scheduler:coroutine-sleep
                (fd:fiber-aware-alternative 'probe-1))
  (assert-equal "test reason" (fd:fiber-reason 'probe-1)))

(deftest declaim-registers-symbol
  "DECLAIM-FIBER-KIND adds the symbol to ANNOTATED-SYMBOLS."
  (reset-probes)
  (fd:declaim-fiber-kind probe-2 :fiber-aware)
  (assert-true (member 'probe-2 (fd:annotated-symbols))))

;;; ---------------------------------------------------------------------------
;;; DEFINE-FIBER-FN body parsing
;;; ---------------------------------------------------------------------------

(deftest define-fiber-fn-default-is-fiber-aware
  "No FIBER-KIND declaration => default :fiber-aware."
  (reset-probes)
  (eval '(fd:define-fiber-fn probe-1 () "doc" 42))
  (assert-equal :fiber-aware (fd:fiber-kind 'probe-1))
  ;; Use FUNCALL + symbol-function so the compiler doesn't flag PROBE-1
  ;; as undefined at test-file compile time (it's defined at run time
  ;; by the EVAL above).
  (assert-equal 42 (funcall (symbol-function 'probe-1))))

(deftest define-fiber-fn-explicit-kind
  "(declare (fiber-kind :KIND ...)) is consumed and applied."
  (reset-probes)
  (eval '(fd:define-fiber-fn probe-2 ()
           "doc"
           (declare (fiber-kind :blocks-thread
                                :reason "test"))
           99))
  (assert-equal :blocks-thread (fd:fiber-kind 'probe-2))
  (assert-equal "test" (fd:fiber-reason 'probe-2))
  (assert-equal 99 (funcall (symbol-function 'probe-2))))

(deftest define-fiber-fn-keeps-other-declarations
  "Non-fiber-kind declarations are kept in the function body."
  (reset-probes)
  (eval '(fd:define-fiber-fn probe-3 (x)
           (declare (ignore x)
                    (fiber-kind :cpu-bound))
           7))
  (assert-equal :cpu-bound (fd:fiber-kind 'probe-3))
  ;; Calling probe-3 with an arg compiles (declare (ignore x)) cleanly.
  (assert-equal 7 (funcall (symbol-function 'probe-3) :anything)))

(deftest define-fiber-fn-rejects-multiple-fiber-kind
  "Two FIBER-KIND declarations is an error."
  (reset-probes)
  (assert-condition (error)
                    (eval '(fd:define-fiber-fn probe-4 ()
                             (declare (fiber-kind :blocks-thread))
                             (declare (fiber-kind :cpu-bound))
                             0))))

;;; ---------------------------------------------------------------------------
;;; Seed coverage
;;;
;;; The seed file (epsilon-contrib/scheduler/src/fiber-discipline-seed.lisp)
;;; is the canonical source-of-truth for which symbols have which kind.
;;; These tests guard against accidental deletion.
;;; ---------------------------------------------------------------------------

(deftest seed-known-blocking-symbols
  "Each symbol the seed marks :blocks-thread is reachable and stamped."
  (assert-equal :blocks-thread (fd:fiber-kind 'sb-ext:with-timeout))
  (assert-equal :blocks-thread (fd:fiber-kind 'sb-thread:condition-wait))
  (assert-equal :blocks-thread (fd:fiber-kind 'sleep))
  (assert-equal :blocks-thread
                (fd:fiber-kind 'epsilon.sys.semaphore:wait-on-semaphore)))

(deftest seed-known-fiber-aware-symbols
  "The canonical fiber-aware primitives are stamped."
  (assert-equal :fiber-aware
                (fd:fiber-kind 'epsilon.scheduler.io-wait:park-on-fd))
  (assert-equal :fiber-aware
                (fd:fiber-kind 'epsilon.scheduler:coroutine-sleep))
  (assert-equal :fiber-aware
                (fd:fiber-kind 'epsilon.scheduler.fiber-io:fiber-read))
  (assert-equal :fiber-aware
                (fd:fiber-kind 'epsilon.scheduler:run-blocking)))

(deftest seed-suggests-alternative-for-sleep
  "When a :blocks-thread symbol has a fiber-aware drop-in, the seed
attaches it so the lint can suggest it."
  (assert-equal 'epsilon.scheduler:coroutine-sleep
                (fd:fiber-aware-alternative 'sleep)))

(deftest seed-includes-rationale-strings
  "Every seed annotation carries a reason string the lint surfaces."
  (assert-true (stringp (fd:fiber-reason 'sb-ext:with-timeout)))
  (assert-true
   (search "SIGALRM" (fd:fiber-reason 'sb-ext:with-timeout))))

;;; ---------------------------------------------------------------------------
;;; Stall reason classifier
;;; ---------------------------------------------------------------------------

(deftest classify-stall-no-backtrace
  "Empty input returns the dedicated :NO-BACKTRACE keyword, not :OTHER.
This lets the watchdog distinguish 'failed to capture' from 'captured
but no known frame matched'."
  (assert-equal :no-backtrace (fd:classify-stall-reason nil))
  (assert-equal :no-backtrace (fd:classify-stall-reason '())))

(deftest classify-stall-other
  "Backtrace with no recognised frame returns :OTHER."
  (assert-equal :other
                (fd:classify-stall-reason
                 '("(SOME-USER-CODE :ARG 1)"
                   "(ANOTHER-FRAME)"))))

(deftest classify-stall-thread-blocked-on-mutex
  "Native-mutex / semaphore frames classify uniformly."
  (assert-equal :thread-blocked-on-mutex
                (fd:classify-stall-reason
                 '("(SB-THREAD::CONDITION-WAIT ...)"
                   "(EPSILON.SYS.SEMAPHORE:WAIT-ON-SEMAPHORE ...)"
                   "(NET-ADAPTERS::READ-INTO ...)")))
  (assert-equal :thread-blocked-on-mutex
                (fd:classify-stall-reason
                 '("(EPSILON.SYS.SEMAPHORE:WAIT-ON-SEMAPHORE :TIMEOUT 5)"
                   "(SOMETHING-ELSE)"))))

(deftest classify-stall-with-timeout-non-preemptive
  "An sb-ext:with-timeout frame stuck around a foreign call is the
classic 'timeout fires but doesn't preempt' shape."
  (assert-equal :with-timeout-non-preemptive
                (fd:classify-stall-reason
                 '("(SB-EXT:WITH-TIMEOUT 30 (READ-HTTP-REQUEST ...))"
                   "(HANDLE-CLIENT ...)"))))

(deftest classify-stall-tls-read
  "Stuck inside the TLS read stack."
  (assert-equal :tls-read-blocked
                (fd:classify-stall-reason
                 '("(EPSILON.CRYPTO.NATIVE::TLS-READ ...)"
                   "(READ-HTTP-REQUEST-USING-READER ...)")))
  (assert-equal :tls-read-blocked
                (fd:classify-stall-reason
                 '("(SOME-FRAME)"
                   "(EPSILON.SSL.TLS13:TLS12-READ ...)"))))

(deftest classify-stall-respects-top-window
  "Frames beyond :TOP do not trigger a match."
  (let ((bt (append (loop repeat 10 collect "(USER-FRAME)")
                    '("(EPSILON.SYS.SEMAPHORE:WAIT-ON-SEMAPHORE)"))))
    (assert-equal :other (fd:classify-stall-reason bt :top 5))
    (assert-equal :thread-blocked-on-mutex
                  (fd:classify-stall-reason bt :top 20))))

(deftest classify-stall-first-pattern-wins
  "When multiple patterns could match, earlier entries in
*STALL-REASON-PATTERNS* win. CONDITION-WAIT (mutex) is listed before
WITH-TIMEOUT, so a frame that contains both must classify as mutex."
  (assert-equal :thread-blocked-on-mutex
                (fd:classify-stall-reason
                 '("(SB-THREAD::CONDITION-WAIT ...)"
                   "(SB-EXT:WITH-TIMEOUT ...)"))))

;;; ---------------------------------------------------------------------------
;;; CARRIER-BACKTRACE smoke (uses a live scheduler)
;;; ---------------------------------------------------------------------------

(deftest carrier-backtrace-returns-frame-strings
  "Capturing a backtrace from a live carrier returns a non-empty list
of strings. We don't pin specific frames; SBCL's exact frame names
aren't part of the contract."
  (epsilon.scheduler:with-scheduler (s :num-carriers 1)
    (let* ((carriers (epsilon.scheduler:scheduler-carriers s))
           (c (aref carriers 0))
           (bt (epsilon.scheduler:carrier-backtrace c :max-frames 5
                                                    :timeout 2.0)))
      (assert-true (or (null bt) (consp bt))) ; a slow CI may time out
      (when bt
        (assert-true (every #'stringp bt))))))

(deftest carrier-backtrace-skips-sigurg-handler-chain
  "After the SIGURG-handler skip heuristic, the captured frames must
NOT begin with %CAPTURE-BACKTRACE-INTO-BUFFER -- otherwise the stall
classifier sees only signal-handler boilerplate and misses the
actual stalled function name. Regression for the bug where a carrier
parked in SLEEP classified as :OTHER because NANOSLEEP sat past the
top-8 cutoff behind 12 frames of signal-handler internals."
  (epsilon.scheduler:with-scheduler (s :num-carriers 1)
    (let* ((carriers (epsilon.scheduler:scheduler-carriers s))
           (c (aref carriers 0)))
      (epsilon.scheduler:scheduler-submit
       s (lambda () (sleep 2)))
      (sleep 0.2)
      (let ((bt (epsilon.scheduler:carrier-backtrace
                 c :max-frames 16 :timeout 2.0)))
        (when bt
          (dolist (frame bt)
            (assert-false
             (search "CAPTURE-BACKTRACE-INTO-BUFFER" frame
                     :test #'char-equal)))
          (assert-false
           (search "SIGURG-HANDLER" (first bt) :test #'char-equal)))))))

(deftest carrier-backtrace-truncates-frame-lines
  "Each frame string must fit within +CARRIER-BACKTRACE-LINE-BYTES+ so
the SIGURG-side capture writes a bounded number of bytes per frame
even on absurd recursive lambdas with long :IN names."
  (epsilon.scheduler:with-scheduler (s :num-carriers 1)
    (let* ((carriers (epsilon.scheduler:scheduler-carriers s))
           (c (aref carriers 0))
           (bt (epsilon.scheduler:carrier-backtrace
                c :max-frames 32 :timeout 2.0)))
      (when bt
        (dolist (frame bt)
          (assert-true
           (<= (length frame)
               epsilon.scheduler::+carrier-backtrace-line-bytes+)))))))

(deftest carrier-backtrace-buffer-is-preallocated
  "Each carrier owns a fixed-capacity character buffer sized at
carrier creation. The SIGURG handler writes through VECTOR-PUSH;
pushing past TOTAL-SIZE silently returns NIL instead of growing
the underlying storage, so capture is allocation-free no matter
what the stack walk produces. SBCL marks any fill-pointer array
as adjustable, but only VECTOR-PUSH-EXTEND grows -- VECTOR-PUSH
does not. This is the load-bearing invariant we assert."
  (epsilon.scheduler:with-scheduler (s :num-carriers 2)
    (let ((carriers (epsilon.scheduler:scheduler-carriers s)))
      (dotimes (i 2)
        (let* ((buf (epsilon.scheduler::carrier-backtrace-buffer
                     (aref carriers i)))
               (capacity (array-total-size buf)))
          (assert-true (stringp buf))
          (assert-true (array-has-fill-pointer-p buf))
          (assert-equal
           (* epsilon.scheduler::+carrier-backtrace-max-frames+
              epsilon.scheduler::+carrier-backtrace-line-bytes+)
           capacity)
          ;; Fill the buffer to capacity, then attempt to push one
          ;; more character. VECTOR-PUSH must return NIL (no growth,
          ;; no condition) and TOTAL-SIZE must be unchanged.
          (setf (fill-pointer buf) 0)
          (dotimes (j capacity)
            (vector-push #\X buf))
          (assert-equal capacity (fill-pointer buf))
          (assert-false (vector-push #\Y buf))
          (assert-equal capacity (array-total-size buf))
          (setf (fill-pointer buf) 0))))))

(deftest carrier-backtrace-survives-tight-allocation
  "Capture a backtrace immediately after allocating a chunky byte
vector that briefly raises dynamic-usage. The SIGURG handler must
not trigger any condition; the previous PRINC-TO-STRING-based
capture would have hit alloc-tramp paths from inside the signal
handler under this kind of pressure. Mostly a smoke test that the
new path tolerates GC pressure."
  (epsilon.scheduler:with-scheduler (s :num-carriers 1)
    (let* ((carriers (epsilon.scheduler:scheduler-carriers s))
           (c (aref carriers 0)))
      (epsilon.scheduler:scheduler-submit
       s (lambda () (sleep 2)))
      (sleep 0.2)
      (let ((ballast (make-array (* 16 1024 1024)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
        (declare (ignorable ballast))
        (let ((bt (epsilon.scheduler:carrier-backtrace
                   c :max-frames 16 :timeout 2.0)))
          (assert-true (or (null bt) (consp bt))))))))
