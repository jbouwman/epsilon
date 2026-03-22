;;;; Test Isolation for epsilon.test
;;;;
;;;; Provides subprocess isolation for tests that might crash or have
;;;; side effects that affect other tests.
;;;;
;;;; Tests marked with (:isolate :process) run in a separate subprocess.
;;;; Uses epsilon.process for subprocess management and epsilon.result
;;;; for explicit error handling.

(defpackage epsilon.test.isolation
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.result result)
            (epsilon.process process)
            (epsilon.syntax th)
            (epsilon.log log)
            (epsilon.path path)
            (epsilon.file fs))
  (:enter t))

;;; Configuration

(defvar *isolation-timeout* 60
  "Timeout in seconds for isolated test execution.")

(defvar *isolated-tests* map:+empty+
  "Map from test symbol to isolation mode (:process, :thread, or nil).")

(defvar *epsilon-executable* nil
  "Path to epsilon executable for subprocess isolation.
   Auto-detected if nil.")

;;; Test Classification

(defun register-isolated-test (test-symbol isolation-mode)
  "Register a test for isolated execution.
   ISOLATION-MODE can be :process, :thread, or nil."
  (setf *isolated-tests* (map:assoc *isolated-tests* test-symbol isolation-mode)))

(defun isolation-mode (test-symbol)
  "Get the isolation mode for a test."
  (map:get *isolated-tests* test-symbol nil))

(defun isolated-test-p (test-symbol)
  "Check if a test requires isolation."
  (not (null (isolation-mode test-symbol))))

(defun process-isolated-p (test-symbol)
  "Check if a test requires process isolation."
  (eq (isolation-mode test-symbol) :process))

;;; Executable Detection

(defun find-epsilon-executable ()
  "Find the epsilon executable path."
  (or *epsilon-executable*
      ;; Try to find it relative to current location
      (let ((candidates (list "./epsilon"
                              "../epsilon"
                              (fs:join-paths (namestring (user-homedir-pathname)) "epsilon"))))
        (dolist (path candidates)
          (when (probe-file path)
            (return path))))
      ;; Try finding in PATH
      (process:find-executable "epsilon")
      ;; Fall back to assuming it's in PATH
      "epsilon"))

(defun format-test-spec (test-symbol)
  "Format a test symbol as a test specification string."
  (let ((pkg-name (package-name (symbol-package test-symbol)))
        (test-name (symbol-name test-symbol)))
    (format nil "~A:~A" pkg-name test-name)))

;;; Isolation Result Type
;;;
;;; Uses epsilon.result for explicit success/failure handling

(defstruct isolation-result
  "Result of running a test in isolation."
  test-symbol
  status        ; :passed :failed :error :timeout
  exit-code
  output
  error-output
  duration)

(defun make-passed-result (test-symbol output error-output duration)
  "Create a passed isolation result."
  (make-isolation-result
   :test-symbol test-symbol
   :status :passed
   :exit-code 0
   :output output
   :error-output error-output
   :duration duration))

(defun make-failed-result (test-symbol exit-code output error-output duration)
  "Create a failed isolation result."
  (make-isolation-result
   :test-symbol test-symbol
   :status :failed
   :exit-code exit-code
   :output output
   :error-output error-output
   :duration duration))

(defun make-timeout-result (test-symbol duration)
  "Create a timeout isolation result."
  (make-isolation-result
   :test-symbol test-symbol
   :status :timeout
   :exit-code -1
   :output ""
   :error-output ""
   :duration duration))

(defun make-error-result (test-symbol error-msg duration)
  "Create an error isolation result."
  (make-isolation-result
   :test-symbol test-symbol
   :status :error
   :exit-code -1
   :output ""
   :error-output error-msg
   :duration duration))

;;; Subprocess Execution
;;;
;;; Uses epsilon.process for clean subprocess management

(defun run-test-in-subprocess (test-symbol module-name &key (timeout *isolation-timeout*))
  "Run a single test in a subprocess.
   Returns an isolation-result."
  (let* ((epsilon (find-epsilon-executable))
         (test-spec (format-test-spec test-symbol))
         (start-time (get-internal-real-time)))

    ;; Use epsilon.process:run-sync for clean subprocess execution
    (let ((run-result
            (result:from-condition
              (process:run-sync epsilon
                                :args (list "--module" (string module-name)
                                            "--test" test-spec)
                                :timeout timeout
                                :error-on-failure nil))))

      (let ((duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))

        (if (result:ok-p run-result)
            ;; Process completed - check exit code
            (multiple-value-bind (output error-output exit-code)
                (result:unwrap run-result)
              (if (zerop exit-code)
                  (make-passed-result test-symbol output error-output duration)
                  (make-failed-result test-symbol exit-code output error-output duration)))

            ;; Process failed - check error type
            (let ((err (result:unwrap-err run-result)))
              (typecase err
                (process:process-timeout-error
                 (make-timeout-result test-symbol duration))
                (t
                 (make-error-result test-symbol
                                    (format nil "Error running subprocess: ~A" err)
                                    duration)))))))))

;;; Batch Isolated Execution

(defun run-isolated-tests (tests module-name)
  "Run all isolated tests, each in its own subprocess.
   Returns list of isolation-results."
  (mapcar (lambda (test)
            (log:debug "Running isolated test: ~A" test)
            (run-test-in-subprocess test module-name))
          tests))

;;; Integration with Test Runner

(defun partition-by-isolation (tests)
  "Partition tests into isolated and non-isolated groups.
   Returns (values isolated-tests normal-tests)."
  (let ((isolated '())
        (normal '()))
    (dolist (test tests)
      (if (isolated-test-p test)
          (push test isolated)
          (push test normal)))
    (values (nreverse isolated) (nreverse normal))))

(defun isolation-result-to-test-result (iso-result)
  "Convert an isolation-result to a format compatible with the test runner."
  ;; This returns a minimal result that can be aggregated
  (list :test (isolation-result-test-symbol iso-result)
        :status (isolation-result-status iso-result)
        :duration (isolation-result-duration iso-result)
        :output (isolation-result-output iso-result)
        :error-output (isolation-result-error-output iso-result)))

;;; Result Predicates

(defun isolation-passed-p (result)
  "Check if an isolation result indicates success."
  (eq (isolation-result-status result) :passed))

(defun isolation-failed-p (result)
  "Check if an isolation result indicates failure."
  (member (isolation-result-status result) '(:failed :error :timeout)))
