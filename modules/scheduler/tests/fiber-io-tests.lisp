;;;; Fiber-aware I/O tests
;;;;
;;;; Uses pipe(2) for read/write paths and a stub reactor (mirroring
;;;; io-wait-tests) to drive fd-readiness wakeups deterministically
;;;; without depending on the platform reactor packages.

(defpackage :epsilon.scheduler.fiber-io.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.fiber-io fio)
           (epsilon.scheduler.io-wait io-wait)
           (epsilon.foreign lib)
           (epsilon.sys.lock lock)))

(in-package :epsilon.scheduler.fiber-io.tests)

;;; ---- Pipe helpers ----------------------------------------------------------

(lib:defshared %pipe "pipe" "libc" :int (pipefd :pointer))
(lib:defshared %close "close" "libc" :int (fd :int))

(defun make-test-pipe ()
  (lib:with-foreign-memory ((pipefd :int :count 2))
    (when (< (%pipe pipefd) 0)
      (error "pipe(2) failed"))
    (cons (sb-sys:sap-ref-32 pipefd 0)
          (sb-sys:sap-ref-32 pipefd 4))))

(defun close-pipe (pipe)
  (ignore-errors (%close (car pipe)))
  (ignore-errors (%close (cdr pipe))))

(defun raw-write-bytes (fd bytes)
  "Synchronously write BYTES (a list of small integers) to FD."
  (let ((n (length bytes)))
    (lib:with-foreign-memory ((sap :char :count n))
      (loop for i from 0 for b in bytes
            do (setf (sb-sys:sap-ref-8 sap i) b))
      (sb-unix:unix-write fd sap 0 n))))

;;; ---- Stub reactor (matches io-wait-tests) ---------------------------------

(defstruct stub-reactor
  (pending nil :type list)
  (lock (lock:make-lock "stub-reactor") :type lock:lock))

(defun stub-registrar (reactor)
  (lambda (fd events cb)
    (declare (ignore fd events))
    (lock:with-lock ((stub-reactor-lock reactor))
      (push cb (stub-reactor-pending reactor)))))

(defun fire-pending (reactor)
  (let ((cbs (lock:with-lock ((stub-reactor-lock reactor))
               (prog1 (stub-reactor-pending reactor)
                 (setf (stub-reactor-pending reactor) nil)))))
    (dolist (cb cbs) (funcall cb :ready))))

(defmacro with-stub-register ((reactor-var) &body body)
  `(let* ((,reactor-var (make-stub-reactor))
          (old-register io-wait:*fd-wait-register*))
     (unwind-protect
          (progn
            (setf io-wait:*fd-wait-register* (stub-registrar ,reactor-var))
            ,@body)
       (setf io-wait:*fd-wait-register* old-register))))

;;; ---- fiber-set-nonblocking -------------------------------------------------

(deftest test-fiber-set-nonblocking
  "FIBER-SET-NONBLOCKING toggles O_NONBLOCK without erroring."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((fd (car pipe)))
           (assert-true (fio:fiber-set-nonblocking fd))
           (assert-true (fio:fiber-set-nonblocking fd)))
      (close-pipe pipe))))

;;; ---- fiber-read with data already present ---------------------------------

(deftest test-fiber-read-immediate
  "FIBER-READ reads available bytes from a pipe (no parking needed)."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((rd (car pipe))
               (wr (cdr pipe)))
           (fio:fiber-set-nonblocking rd)
           (raw-write-bytes wr '(65 66 67 68 69))
           (with-stub-register (_reactor)
             (sched:with-scheduler (s :num-carriers 1)
               (let ((result nil))
                 (sched:scheduler-submit
                  s
                  (lambda ()
                    (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
                      (let ((n (fio:fiber-read rd buf)))
                        (setf result (cons n (subseq buf 0 n)))))))
                 (sched:scheduler-wait s)
                 (assert-= 5 (car result))
                 (assert-equalp #(65 66 67 68 69) (cdr result))))))
      (close-pipe pipe))))

;;; ---- fiber-read parks on EAGAIN; stub fires callback after producer writes -

(deftest test-fiber-read-parks-and-resumes
  "On an empty pipe, fiber-read parks via the stub reactor, then wakes
when the test fires the recorded callback after writing."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((rd (car pipe))
               (wr (cdr pipe)))
           (fio:fiber-set-nonblocking rd)
           (with-stub-register (reactor)
             (sched:with-scheduler (s :num-carriers 1)
               (let ((received nil))
                 (sched:scheduler-submit
                  s
                  (lambda ()
                    (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
                      (let ((n (fio:fiber-read rd buf :timeout 5)))
                        (setf received (cons n (subseq buf 0 n)))))))
                 ;; Producer thread: wait for the fiber to register a
                 ;; callback (i.e. enter park), then write data and fire
                 ;; the stub.
                 (sb-thread:make-thread
                  (lambda ()
                    (loop until (lock:with-lock ((stub-reactor-lock reactor))
                                  (stub-reactor-pending reactor))
                          do (sleep 0.005))
                    (raw-write-bytes wr '(1 2 3 4))
                    (fire-pending reactor)))
                 (sched:scheduler-wait s)
                 (assert-= 4 (car received))
                 (assert-equalp #(1 2 3 4) (cdr received))))))
      (close-pipe pipe))))

;;; ---- fiber-write to a pipe -------------------------------------------------

(deftest test-fiber-write-immediate
  "FIBER-WRITE pushes bytes to a pipe; the read end sees them."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((rd (car pipe))
               (wr (cdr pipe)))
           (fio:fiber-set-nonblocking wr)
           (with-stub-register (_reactor)
             (sched:with-scheduler (s :num-carriers 1)
               (sched:scheduler-submit
                s
                (lambda ()
                  (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                                           :initial-contents '(7 8 9))))
                    (fio:fiber-write-all wr buf))))
               (sched:scheduler-wait s)
               (lib:with-foreign-memory ((sap :char :count 3))
                 (let ((n (sb-unix:unix-read rd sap 3)))
                   (assert-= 3 n)
                   (assert-= 7 (sb-sys:sap-ref-8 sap 0))
                   (assert-= 8 (sb-sys:sap-ref-8 sap 1))
                   (assert-= 9 (sb-sys:sap-ref-8 sap 2)))))))
      (close-pipe pipe))))

;;; ---- fiber-read-exact loops over partial reads ----------------------------

(deftest test-fiber-read-exact-loops
  "FIBER-READ-EXACT keeps reading until the buffer is full, even when
the producer dribbles data in multiple bursts."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((rd (car pipe))
               (wr (cdr pipe)))
           (fio:fiber-set-nonblocking rd)
           (with-stub-register (reactor)
             (sched:with-scheduler (s :num-carriers 1)
               (let ((buf (make-array 6 :element-type '(unsigned-byte 8))))
                 (sched:scheduler-submit
                  s
                  (lambda ()
                    (fio:fiber-read-exact rd buf :timeout 5)))
                 (sb-thread:make-thread
                  (lambda ()
                    (dotimes (round 3)
                      ;; Wait until the fiber is parked, then deliver 2 bytes.
                      (loop until (lock:with-lock ((stub-reactor-lock reactor))
                                    (stub-reactor-pending reactor))
                            do (sleep 0.005))
                      (raw-write-bytes wr (list (* 10 (1+ round))
                                                (1+ (* 10 (1+ round)))))
                      (fire-pending reactor))))
                 (sched:scheduler-wait s)
                 (assert-equalp #(10 11 20 21 30 31) buf)))))
      (close-pipe pipe))))

;;; ---- fiber-sleep-ms --------------------------------------------------------

(deftest test-fiber-sleep-ms
  "FIBER-SLEEP-MS yields for at least the requested duration."
  (sched:with-scheduler (s :num-carriers 1)
    (let ((elapsed nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((t0 (get-internal-real-time)))
           (fio:fiber-sleep-ms 50)
           (setf elapsed (/ (- (get-internal-real-time) t0)
                            (float internal-time-units-per-second))))))
      (sched:scheduler-wait s)
      (assert-true (>= elapsed 0.04)))))

;;; ---- fiber-read signals fiber-io-timeout, not 0, on timeout ---------------

(deftest test-fiber-read-timeout-signals
  "When the wait times out, fiber-read signals FIBER-IO-TIMEOUT (so
callers can distinguish timeout from EOF)."
  (let ((pipe (make-test-pipe)))
    (unwind-protect
         (let ((rd (car pipe)))
           (fio:fiber-set-nonblocking rd)
           ;; Stub reactor never fires its callback, so the fiber's
           ;; wait must time out.
           (with-stub-register (_reactor)
             (sched:with-scheduler (s :num-carriers 1)
               (let ((timed-out nil))
                 (sched:scheduler-submit
                  s
                  (lambda ()
                    (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
                      (handler-case
                          (fio:fiber-read rd buf :timeout 0.1)
                        (fio:fiber-io-timeout ()
                          (setf timed-out t))))))
                 (sched:scheduler-wait s)
                 (assert-true timed-out)))))
      (close-pipe pipe))))
