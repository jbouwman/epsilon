;;;; Parallel Test Execution for epsilon.test
;;;;
;;;; Provides parallel test execution using channels and the Result type,
;;;; inspired by Go's t.Parallel() and Rust's default parallel test execution.
;;;;
;;;; Tests marked with (:parallel t) run concurrently.
;;;; Tests without :parallel or with (:parallel nil) run sequentially.

(defpackage epsilon.test.parallel
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.result result)
            (epsilon.channel channel)
            (epsilon.syntax th)
            (epsilon.test.suite suite)
            (epsilon.log log)
            (epsilon.sys.thread thread))
  (:enter t))

;;; Configuration

(defvar *parallel-workers* 1
  "Number of parallel worker threads for test execution.")

(defvar *parallel-enabled* t
  "Global flag to enable/disable parallel execution.")

(defvar *parallel-tests* map:+empty+
  "Map from test symbol to parallel flag (T or NIL).")

;;; Test Classification

(defun register-parallel-test (test-symbol parallel-p)
  "Register whether a test can run in parallel."
  (setf *parallel-tests* (map:assoc *parallel-tests* test-symbol parallel-p)))

(defun parallel-test-p (test-symbol)
  "Check if a test is marked for parallel execution."
  (map:get *parallel-tests* test-symbol nil))

(defun classify-tests (tests)
  "Partition tests into parallel and sequential groups.
   Returns (values parallel-tests sequential-tests)."
  (let ((parallel '())
        (sequential '()))
    (dolist (test tests)
      (if (parallel-test-p test)
          (push test parallel)
          (push test sequential)))
    (values (nreverse parallel) (nreverse sequential))))

;;; Test Result Type
;;;
;;; Uses epsilon.result for explicit success/failure handling

(defstruct test-execution
  "Result of executing a single test."
  test-symbol
  result        ; Result type: ok(test-result) or err(condition)
  duration)

(defun execute-test (test-symbol run-fn)
  "Execute a test, capturing the result as a Result type.
   Returns a test-execution struct."
  (let ((start-time (get-internal-real-time)))
    (make-test-execution
     :test-symbol test-symbol
     :result (result:from-condition (funcall run-fn))
     :duration (/ (- (get-internal-real-time) start-time)
                  internal-time-units-per-second))))

(defun execution-passed-p (execution)
  "Check if a test execution passed."
  (result:ok-p (test-execution-result execution)))

(defun execution-value (execution)
  "Get the value from a successful execution, or the error from a failed one."
  (let ((result (test-execution-result execution)))
    (if (result:ok-p result)
        (result:unwrap result)
        (result:unwrap-err result))))

;;; Channel-based Worker Pool
;;;
;;; Uses epsilon.channel for thread-safe communication

(defstruct parallel-runner
  "Manages parallel test execution using channels."
  (task-channel nil)      ; Channel for sending tasks to workers
  (result-channel nil)    ; Channel for collecting results
  (workers '())           ; List of worker threads
  (num-workers 4))

(defun create-parallel-runner (&key (num-workers *parallel-workers*))
  "Create a new parallel runner with worker threads."
  (let* ((task-ch (channel:make-channel :capacity num-workers))
         (result-ch (channel:make-channel))
         (runner (make-parallel-runner
                  :task-channel task-ch
                  :result-channel result-ch
                  :num-workers num-workers)))
    ;; Start worker threads
    (dotimes (i num-workers)
      (push (thread:make-thread
             (lambda () (worker-loop task-ch result-ch))
             :name (format nil "test-worker-~D" i))
            (parallel-runner-workers runner)))
    runner))

(defun worker-loop (task-channel result-channel)
  "Worker thread loop - receives tasks and sends results via channels."
  (loop
    (multiple-value-bind (task received-p)
        (channel:receive task-channel)
      (unless received-p
        ;; Channel closed, exit worker
        (return))
      ;; Execute the task and send result
      (let* ((test-symbol (car task))
             (run-fn (cdr task))
             (execution (execute-test test-symbol run-fn)))
        (channel:send result-channel execution)))))

(defun submit-tests (runner tests run-single-test-fn)
  "Submit tests to the runner's task channel."
  (dolist (test tests)
    (channel:send (parallel-runner-task-channel runner)
                  (cons test (lambda () (funcall run-single-test-fn test))))))

(defun collect-results (runner count)
  "Collect COUNT results from the result channel."
  (let ((results '()))
    (dotimes (i count)
      (multiple-value-bind (result received-p)
          (channel:receive (parallel-runner-result-channel runner))
        (when received-p
          (push result results))))
    (nreverse results)))

(defun shutdown-runner (runner)
  "Shutdown the runner and its workers."
  ;; Close task channel to signal workers to exit
  (channel:close-channel (parallel-runner-task-channel runner))
  ;; Wait for workers to finish
  (dolist (worker (parallel-runner-workers runner))
    (thread:join-thread worker))
  ;; Close result channel
  (channel:close-channel (parallel-runner-result-channel runner)))

;;; Parallel Test Execution

(defun run-tests-parallel (tests run-single-test-fn &key (workers *parallel-workers*))
  "Run TESTS in parallel using WORKERS threads.
   RUN-SINGLE-TEST-FN is called with each test symbol and should return a result.
   Returns list of test results."
  (unless *parallel-enabled*
    ;; Fall back to sequential if parallel disabled
    (return-from run-tests-parallel
      (mapcar run-single-test-fn tests)))

  (multiple-value-bind (parallel-tests sequential-tests)
      (classify-tests tests)

    (let ((results '()))
      ;; First run parallel tests using channel-based runner
      (when parallel-tests
        (let ((runner (create-parallel-runner :num-workers (min workers (length parallel-tests)))))
          (unwind-protect
               (progn
                 ;; Submit all parallel tests
                 (submit-tests runner parallel-tests run-single-test-fn)
                 ;; Collect results
                 (let ((executions (collect-results runner (length parallel-tests))))
                   (dolist (exec executions)
                     (push (execution-value exec) results))))
            (shutdown-runner runner))))

      ;; Then run sequential tests in order
      (dolist (test sequential-tests)
        (push (funcall run-single-test-fn test) results))

      (nreverse results))))

;;; Test Result Aggregation

(defstruct parallel-run-result
  "Result of running tests in parallel."
  (test-results '() :type list)
  (total-time 0)
  (parallel-tests-count 0)
  (sequential-tests-count 0))

(defun aggregate-results (executions)
  "Aggregate results from parallel test execution."
  (let ((result (make-parallel-run-result)))
    (dolist (exec executions)
      (push (execution-value exec) (parallel-run-result-test-results result)))
    (setf (parallel-run-result-test-results result)
          (nreverse (parallel-run-result-test-results result)))
    result))

;;; Integration with Test Suite
;;; Register callback with suite module for parallel test detection

(setf suite:*parallel-test-p-callback* #'parallel-test-p)
