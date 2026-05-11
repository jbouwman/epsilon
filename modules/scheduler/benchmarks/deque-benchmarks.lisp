;;;; Benchmarks for the Chase-Lev work-stealing deque.
;;;;
;;;; Compares ws-deque performance against sb-concurrency:queue (lock-free FIFO)
;;;; for single-thread throughput, multi-thread steal throughput, and mixed
;;;; producer/consumer workloads.

(defpackage :epsilon.scheduler.deque-benchmarks
  (:use :cl)
  (:import
   (epsilon.scheduler.deque deque)
   (epsilon.benchmark bench)
   (epsilon.sys.thread thread))
  (:export #:register-deque-benchmarks
           #:run-deque-benchmarks))

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *bench-items* 100000
  "Number of items per benchmark iteration.")

(defvar *bench-thieves* 4
  "Number of thief threads for concurrent benchmarks.")

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun make-sbcq ()
  "Create an sb-concurrency:queue."
  (sb-concurrency:make-queue))

(defun sbcq-push (q item)
  (sb-concurrency:enqueue item q))

(defun sbcq-pop (q)
  (sb-concurrency:dequeue q))

;;; ---------------------------------------------------------------------------
;;; Single-Thread Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-single-thread-benchmarks ()

  ;; ws-deque: push N items then pop all
  (bench:defbenchmark deque/single/push-pop ()
    (let ((d (deque:make-ws-deque :initial-size 1024)))
      (dotimes (i *bench-items*)
        (deque:ws-deque-push d i))
      (dotimes (i *bench-items*)
        (bench:consume (deque:ws-deque-pop d)))))

  ;; sb-concurrency:queue: enqueue N items then dequeue all
  (bench:defbenchmark deque/single/sbcq-push-pop ()
    (let ((q (make-sbcq)))
      (dotimes (i *bench-items*)
        (sbcq-push q i))
      (dotimes (i *bench-items*)
        (bench:consume (sbcq-pop q)))))

  ;; ws-deque: interleaved push/pop (simulates owner work loop)
  (bench:defbenchmark deque/single/interleaved ()
    (let ((d (deque:make-ws-deque :initial-size 256)))
      (dotimes (i *bench-items*)
        (deque:ws-deque-push d i)
        (when (zerop (mod i 2))
          (bench:consume (deque:ws-deque-pop d))))
      ;; Drain
      (loop (multiple-value-bind (item found) (deque:ws-deque-pop d)
              (declare (ignore item))
              (unless found (return)))))))

;;; ---------------------------------------------------------------------------
;;; Multi-Thread Steal Benchmarks
;;; ---------------------------------------------------------------------------

(defun register-steal-benchmarks ()

  ;; ws-deque: owner pushes, multiple thieves steal
  (bench:defbenchmark deque/steal/ws-deque ()
    (let* ((d (deque:make-ws-deque :initial-size 1024))
           (done (make-array 1 :element-type 'fixnum :initial-element 0))
           (stolen-count (make-array *bench-thieves*
                                     :element-type 'fixnum
                                     :initial-element 0))
           (threads nil))
      ;; Start thieves
      (dotimes (i *bench-thieves*)
        (let ((idx i))
          (push (thread:make-thread
                 (lambda ()
                   (let ((count 0))
                     (declare (type fixnum count))
                     (loop
                       (multiple-value-bind (item status)
                           (deque:ws-deque-steal d)
                         (declare (ignore item))
                         (cond
                           ((eq status t) (incf count))
                           ((eq status :abort) nil)
                           (t
                            (when (plusp (aref done 0))
                              ;; Drain
                              (loop
                                (multiple-value-bind (item2 status2)
                                    (deque:ws-deque-steal d)
                                  (declare (ignore item2))
                                  (cond
                                    ((eq status2 t) (incf count))
                                    ((eq status2 :abort) nil)
                                    (t (return)))))
                              (return))
                            (thread:thread-yield)))))
                     (setf (aref stolen-count idx) count)))
                 :name (format nil "thief-~d" i))
                threads)))
      ;; Owner pushes
      (dotimes (i *bench-items*)
        (deque:ws-deque-push d i))
      (setf (aref done 0) 1)
      ;; Drain remaining
      (loop (multiple-value-bind (item found) (deque:ws-deque-pop d)
              (declare (ignore item))
              (unless found (return))))
      ;; Wait
      (dolist (th threads)
        (thread:join-thread th))
      (bench:consume stolen-count)))

  ;; sb-concurrency:queue: producer enqueues, consumers dequeue
  (bench:defbenchmark deque/steal/sbcq ()
    (let* ((q (make-sbcq))
           (done (make-array 1 :element-type 'fixnum :initial-element 0))
           (consumed-count (make-array *bench-thieves*
                                       :element-type 'fixnum
                                       :initial-element 0))
           (threads nil))
      ;; Start consumers
      (dotimes (i *bench-thieves*)
        (let ((idx i))
          (push (thread:make-thread
                 (lambda ()
                   (let ((count 0))
                     (declare (type fixnum count))
                     (loop
                       (let ((item (sbcq-pop q)))
                         (cond
                           (item (incf count))
                           (t
                            (when (plusp (aref done 0))
                              ;; Drain
                              (loop
                                (let ((item2 (sbcq-pop q)))
                                  (if item2
                                      (incf count)
                                      (return))))
                              (return))
                            (thread:thread-yield)))))
                     (setf (aref consumed-count idx) count)))
                 :name (format nil "consumer-~d" i))
                threads)))
      ;; Producer
      (dotimes (i *bench-items*)
        (sbcq-push q i))
      (setf (aref done 0) 1)
      ;; Wait
      (dolist (th threads)
        (thread:join-thread th))
      (bench:consume consumed-count))))

;;; ---------------------------------------------------------------------------
;;; Registration and Running
;;; ---------------------------------------------------------------------------

(defun register-deque-benchmarks ()
  "Register all deque benchmarks."
  (register-single-thread-benchmarks)
  (register-steal-benchmarks))

(defun run-deque-benchmarks ()
  "Run all deque benchmarks and print results."
  (register-deque-benchmarks)
  (bench:run-benchmark "deque/"))
