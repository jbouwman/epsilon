;;;; Tests for the timer heap and scheduler timer integration.

(defpackage :epsilon.scheduler.timer-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler.timer timer)
            (epsilon.scheduler sched)
            (epsilon.scheduler.coroutine coro)
   (epsilon.sys.lock lock)
   (epsilon.sys.thread thread)))

;;; ============================================================================
;;; Timer Heap Unit Tests
;;; ============================================================================

(deftest test-timer-heap-creation
  "A new timer heap starts empty."
  (let ((h (timer:make-timer-heap)))
    (assert-true (timer:timer-heap-p h))
    (assert-true (timer:timer-heap-empty-p h))
    (assert-= 0 (timer:timer-heap-size h))))

(deftest test-timer-heap-insert-and-peek
  "Insert a single entry and peek at it."
  (let ((h (timer:make-timer-heap)))
    (timer:timer-heap-insert h 100 :coro-a)
    (assert-= 1 (timer:timer-heap-size h))
    (assert-true (not (timer:timer-heap-empty-p h)))
    (multiple-value-bind (deadline coro) (timer:timer-heap-peek h)
      (assert-= 100 deadline)
      (assert-eq :coro-a coro))))

(deftest test-timer-heap-min-ordering
  "The heap always returns the minimum deadline first."
  (let ((h (timer:make-timer-heap)))
    (timer:timer-heap-insert h 300 :c)
    (timer:timer-heap-insert h 100 :a)
    (timer:timer-heap-insert h 200 :b)
    (assert-= 3 (timer:timer-heap-size h))
    ;; Peek should be the minimum
    (multiple-value-bind (deadline coro) (timer:timer-heap-peek h)
      (assert-= 100 deadline)
      (assert-eq :a coro))
    ;; Pop in order
    (multiple-value-bind (d c) (timer:timer-heap-pop h)
      (assert-= 100 d) (assert-eq :a c))
    (multiple-value-bind (d c) (timer:timer-heap-pop h)
      (assert-= 200 d) (assert-eq :b c))
    (multiple-value-bind (d c) (timer:timer-heap-pop h)
      (assert-= 300 d) (assert-eq :c c))
    (assert-true (timer:timer-heap-empty-p h))))

(deftest test-timer-heap-pop-empty
  "Popping from an empty heap returns NIL, NIL."
  (let ((h (timer:make-timer-heap)))
    (multiple-value-bind (deadline coro) (timer:timer-heap-pop h)
      (assert-nil deadline)
      (assert-nil coro))))

(deftest test-timer-heap-many-entries
  "Insert many entries and verify they come out in sorted order."
  (let ((h (timer:make-timer-heap))
        (n 1000))
    ;; Insert in reverse order
    (dotimes (i n)
      (timer:timer-heap-insert h (- n i) (- n i)))
    (assert-= n (timer:timer-heap-size h))
    ;; Pop should yield 1, 2, 3, ...
    (dotimes (i n)
      (multiple-value-bind (deadline coro) (timer:timer-heap-pop h)
        (assert-= (1+ i) deadline)
        (assert-= (1+ i) coro)))
    (assert-true (timer:timer-heap-empty-p h))))

(deftest test-timer-heap-duplicate-deadlines
  "Multiple entries with the same deadline all get returned."
  (let ((h (timer:make-timer-heap)))
    (timer:timer-heap-insert h 100 :a)
    (timer:timer-heap-insert h 100 :b)
    (timer:timer-heap-insert h 100 :c)
    (assert-= 3 (timer:timer-heap-size h))
    ;; All three should be poppable with deadline 100
    (let ((results nil))
      (dotimes (i 3)
        (multiple-value-bind (d c) (timer:timer-heap-pop h)
          (assert-= 100 d)
          (push c results)))
      ;; All three coroutines should be present
      (assert-= 3 (length results))
      (assert-not-null (member :a results))
      (assert-not-null (member :b results))
      (assert-not-null (member :c results)))))

(deftest test-timer-heap-drain
  "Drain returns all entries at or before the given deadline."
  (let ((h (timer:make-timer-heap)))
    (timer:timer-heap-insert h 100 :a)
    (timer:timer-heap-insert h 200 :b)
    (timer:timer-heap-insert h 300 :c)
    (timer:timer-heap-insert h 400 :d)
    ;; Drain at 250 should return :a and :b
    (let ((expired (timer:timer-heap-drain h 250)))
      (assert-= 2 (length expired))
      (assert-eq :a (first expired))
      (assert-eq :b (second expired)))
    ;; Remaining: :c (300) and :d (400)
    (assert-= 2 (timer:timer-heap-size h))
    (multiple-value-bind (d c) (timer:timer-heap-pop h)
      (assert-= 300 d) (assert-eq :c c))))

(deftest test-timer-heap-drain-empty
  "Draining an empty heap returns an empty list."
  (let ((h (timer:make-timer-heap)))
    (assert-nil (timer:timer-heap-drain h 999))))

(deftest test-timer-heap-drain-none-expired
  "Draining when no entries have expired returns an empty list."
  (let ((h (timer:make-timer-heap)))
    (timer:timer-heap-insert h 1000 :a)
    (assert-nil (timer:timer-heap-drain h 500))
    (assert-= 1 (timer:timer-heap-size h))))

(deftest test-timer-heap-growth
  "Heap grows its backing array when needed."
  (let ((h (timer:make-timer-heap :initial-capacity 4)))
    ;; Insert more than initial capacity
    (dotimes (i 100)
      (timer:timer-heap-insert h i i))
    (assert-= 100 (timer:timer-heap-size h))
    ;; Should still return in order
    (dotimes (i 100)
      (multiple-value-bind (d c) (timer:timer-heap-pop h)
        (assert-= i d)
        (assert-= i c)))))

;;; ============================================================================
;;; Scheduler Timer Integration Tests
;;; ============================================================================

(deftest test-scheduler-timeout-wakeup
  "A parked coroutine with timeout is woken after the deadline."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :timeout 0.05)
          (setf result :woke-up))))
    (assert-eq :woke-up result)))

(deftest test-scheduler-timeout-before-predicate
  "Timeout fires even if predicate never becomes true."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park
           :wake-when (lambda () nil)  ; never true
           :timeout 0.05)
          (setf result :timeout-fired))))
    (assert-eq :timeout-fired result)))

(deftest test-scheduler-predicate-before-timeout
  "Predicate wakes coroutine before timeout expires (lazy deletion)."
  (let* ((state (vector nil nil)))  ; (0)=flag (1)=result
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park
           :wake-when (lambda () (svref state 0))
           :timeout 10)
          (setf (svref state 1) :predicate-won)))
      (sleep 0.05)
      (setf (svref state 0) t))
    (assert-eq :predicate-won (svref state 1))))

(deftest test-scheduler-coroutine-sleep
  "coroutine-sleep suspends for approximately the right duration."
  (let ((elapsed nil)
        (delay 0.1))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (let ((start (get-internal-real-time)))
            (sched:coroutine-sleep delay)
            (setf elapsed
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second))))))
    ;; Should sleep at least delay minus 20ms of scheduler slack.
    (assert-true (>= elapsed (- delay 0.02)))
    ;; Should not overshoot by more than 100ms.
    (assert-true (< elapsed (+ delay 0.1)))))

(deftest test-scheduler-many-concurrent-sleeps
  "1000 coroutines each sleeping different durations all complete."
  (let ((n 1000)
        (completed 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i n)
        (sched:scheduler-submit s
          (lambda ()
            (coro:coroutine-park :timeout 0.001)
            (lock:with-lock (lock)
              (incf completed))))))
    (assert-= n completed)))

(deftest test-scheduler-sleep-ordering
  "Coroutines sleeping for shorter durations wake before longer ones."
  (let ((log nil)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 1)
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :timeout 0.08)
          (lock:with-lock (lock)
            (push :long log))))
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :timeout 0.02)
          (lock:with-lock (lock)
            (push :short log)))))
    (assert-equal '(:long :short) log)))

(deftest test-scheduler-mixed-yield-and-sleep
  "Coroutines that yield and coroutines that sleep coexist."
  (let ((results (make-array 0 :fill-pointer 0 :adjustable t))
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 2)
      ;; Yielding coroutine: yield twice, then record on third invocation.
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-yield)
          (coro:coroutine-yield)
          (lock:with-lock (lock)
            (vector-push-extend :yielder results))))
      ;; Sleeping coroutine
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :timeout 0.02)
          (lock:with-lock (lock)
            (vector-push-extend :sleeper results)))))
    (assert-= 2 (length results))
    (assert-not-null (find :yielder results))
    (assert-not-null (find :sleeper results))))

;;; ============================================================================
;;; Stage 4: I/O Integration Tests
;;; ============================================================================

(deftest test-coroutine-wait-until-basic
  "coroutine-wait-until suspends until predicate becomes true."
  (let* ((state (vector nil nil)))  ; (0)=flag (1)=result
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (sched:coroutine-wait-until (lambda () (svref state 0)))
          (setf (svref state 1) :done)))
      (sleep 0.05)
      (setf (svref state 0) t))
    (assert-eq :done (svref state 1))))

(deftest test-coroutine-wait-until-with-timeout
  "coroutine-wait-until wakes on timeout if predicate never fires."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (sched:coroutine-wait-until (lambda () nil) :timeout 0.05)
          (setf result :timeout))))
    (assert-eq :timeout result)))

(deftest test-external-async-completion
  "Simulates an async I/O operation completed by an external thread.
The coroutine parks until an external thread signals completion."
  (let* ((state (vector nil nil))  ; (0)=completed-p (1)=result
         (work-thread nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (sched:coroutine-wait-until (lambda () (svref state 0)))
          (setf (svref state 1) :got-it)))
      (setf work-thread
            (thread:make-thread
             (lambda ()
               (sleep 0.05)
               (setf (svref state 0) t))
             :name "simulated-io")))
    (when (and work-thread (thread:thread-alive-p work-thread))
      (thread:join-thread work-thread))
    (assert-eq :got-it (svref state 1))))

(deftest test-carrier-idle-hook
  "The carrier idle hook is called during idle time."
  (let ((hook-called 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 2
                              :idle-hook (lambda ()
                                           (lock:with-lock (lock)
                                             (incf hook-called))))
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :timeout 0.05))))
    (assert-true (> hook-called 0))))

(deftest test-many-async-waiters
  "Multiple coroutines waiting on different external conditions."
  (let ((n 100)
        (flags (make-array 100 :initial-element nil))
        (completed 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i n)
        (let ((idx i))
          (sched:scheduler-submit s
            (lambda ()
              (sched:coroutine-wait-until (lambda () (aref flags idx)))
              (lock:with-lock (lock)
                (incf completed))))))
      (sleep 0.02)
      (dotimes (i n)
        (setf (aref flags i) t)))
    (assert-= n completed)))
