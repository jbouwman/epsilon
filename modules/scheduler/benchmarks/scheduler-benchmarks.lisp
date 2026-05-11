;;;; Benchmarks for the M:N coroutine scheduler.
;;;;
;;;; Measures spawn-and-join latency, concurrent coroutine throughput,
;;;; memory footprint, and work-stealing efficiency.

(defpackage :epsilon.scheduler.scheduler-benchmarks
  (:use :cl)
  (:import
   (epsilon.scheduler sched)
   (epsilon.scheduler.coroutine coro)
   (epsilon.channel channel)
   (epsilon.benchmark bench)
   (epsilon.sys.lock lock)
   (epsilon.sys.thread thread))
  (:export #:register-scheduler-benchmarks
           #:run-scheduler-benchmarks))

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *default-carriers* 4
  "Number of carrier threads for benchmarks.")

;;; ---------------------------------------------------------------------------
;;; Spawn-and-Join Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-spawn-benchmarks ()

  ;; Coroutine: spawn N coroutines that complete immediately
  (bench:defbenchmark scheduler/spawn/coroutine-noop ()
    (let ((n 10000))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s (lambda () nil))))))

  ;; Thread: spawn N threads that complete immediately (baseline)
  (bench:defbenchmark scheduler/spawn/thread-noop ()
    (let ((n 1000)  ;; fewer because threads are expensive
          (threads nil))
      (dotimes (i n)
        (push (thread:make-thread (lambda () nil)
                                     :name "bench-thread")
              threads))
      (dolist (th threads)
        (thread:join-thread th))))

  ;; Coroutine: spawn N using channel:spawn (via hook)
  (bench:defbenchmark scheduler/spawn/channel-spawn ()
    (let ((n 10000))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (channel:spawn nil)))))

  ;; Coroutine: spawn N that each yield once
  (bench:defbenchmark scheduler/spawn/yield-once ()
    (let ((n 10000))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s
            (let ((first t))
              (lambda ()
                (when first
                  (setf first nil)
                  (coro:coroutine-yield)))))))))

  ;; Coroutine: spawn N that each yield K times
  (bench:defbenchmark scheduler/spawn/yield-10 ()
    (let ((n 1000)
          (k 10))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s
            (let ((count 0))
              (lambda ()
                (if (< count k)
                    (progn (incf count) (coro:coroutine-yield))
                    nil)))))))))

;;; ---------------------------------------------------------------------------
;;; Throughput and Scaling Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-throughput-benchmarks ()

  ;; Async/await throughput: N async promises resolved
  (bench:defbenchmark scheduler/throughput/async-await ()
    (let ((n 1000))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (let ((promises nil))
          (dotimes (i n)
            (let ((idx i))
              (push (channel:async (* idx idx)) promises)))
          (dolist (p promises)
            (channel:await p))))))

  ;; Timer-based: N coroutines sleeping briefly
  (bench:defbenchmark scheduler/throughput/timer-sleep ()
    (let ((n 1000))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s
            (lambda ()
              (coro:coroutine-park :timeout 0.001
                                   :then (lambda () nil)))))))))

;;; ---------------------------------------------------------------------------
;;; Memory Footprint Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-memory-benchmarks ()

  ;; Measure per-coroutine memory overhead
  (bench:defbenchmark scheduler/memory/100k-coroutines ()
    (let ((n 100000))
      ;; Force GC to get clean baseline
      (sb-ext:gc :full t)
      (let ((before (sb-kernel:dynamic-usage)))
        (sched:with-scheduler (s :num-carriers *default-carriers*)
          (dotimes (i n)
            (sched:scheduler-submit s (lambda () nil))))
        (sb-ext:gc :full t)
        (let* ((after (sb-kernel:dynamic-usage))
               (delta (- after before))
               (per-coro (if (> n 0) (/ delta n) 0)))
          ;; Report (consume prevents dead code elimination)
          (bench:consume per-coro))))))

;;; ---------------------------------------------------------------------------
;;; Work-Stealing Balance Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-stealing-benchmarks ()

  ;; Imbalanced workload: all work submitted to carrier 0
  ;; Measures how well work-stealing distributes the load
  (bench:defbenchmark scheduler/stealing/imbalanced ()
    (let ((n 10000)
          (completed 0)
          (lock (lock:make-lock)))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s
            (lambda ()
              ;; Small amount of work to keep carriers busy
              (let ((sum 0))
                (dotimes (j 100)
                  (incf sum j))
                (lock:with-lock (lock)
                  (incf completed))
                sum))))
        ;; Wait handled by with-scheduler
        )
      (bench:consume completed))))

;;; ---------------------------------------------------------------------------
;;; Run-Blocking Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-blocking-benchmarks ()

  ;; run-blocking: offload CPU work to thread pool
  (bench:defbenchmark scheduler/blocking/offload ()
    (let ((n 100)
          (completed 0)
          (lock (lock:make-lock)))
      (sched:with-scheduler (s :num-carriers *default-carriers*)
        (dotimes (i n)
          (sched:scheduler-submit s
            (lambda ()
              (sched:run-blocking
               (lambda ()
                 ;; Simulate CPU work
                 (let ((sum 0))
                   (dotimes (j 1000)
                     (incf sum j))
                   sum))
               :then (lambda (val)
                       (declare (ignore val))
                       (lock:with-lock (lock)
                         (incf completed))))))))
      (bench:consume completed))))

;;; ---------------------------------------------------------------------------
;;; Registration and Running
;;; ---------------------------------------------------------------------------

(defun register-scheduler-benchmarks ()
  "Register all scheduler benchmarks."
  (register-spawn-benchmarks)
  (register-throughput-benchmarks)
  (register-memory-benchmarks)
  (register-stealing-benchmarks)
  (register-blocking-benchmarks))

(defun run-scheduler-benchmarks ()
  "Run all scheduler benchmarks and print results."
  (register-scheduler-benchmarks)
  (bench:run-benchmark "scheduler/"))
