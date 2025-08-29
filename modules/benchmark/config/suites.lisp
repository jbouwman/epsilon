;;;; Benchmark Suite Configuration
;;;;
;;;; This file defines which benchmark suites to run and their configurations

(in-package :epsilon.tool.benchmark.suites)

;;; Suite definitions

(defparameter *default-suites* '(core-operations)
  "Default benchmark suites to run")

(defparameter *ci-suites* '(quick)
  "Suites to run in CI/CD pipelines")

(defparameter *all-suites* '(core-operations ffi-operations http2-operations crypto-operations)
  "All available benchmark suites")

;;; Configuration for baseline comparison

(defparameter *baseline-suites* '(core-operations crypto-operations)
  "Suites to track for baseline comparison")

(defparameter *regression-threshold* 1.1
  "Performance ratio threshold for regression detection (1.1 = 10% slower)")

(defparameter *improvement-threshold* 0.9
  "Performance ratio threshold for improvement detection (0.9 = 10% faster)")

;;; Run configurations

(defun run-default-benchmarks ()
  "Run the default benchmark suites"
  (register-all-suites)
  (dolist (suite *default-suites*)
    (when (get-suite suite)
      (format t "~%Running suite: ~A~%" suite)
      (run-suite suite))))

(defun run-ci-benchmarks ()
  "Run quick benchmarks for CI"
  (register-quick-suite)
  (run-suite 'quick))

(defun run-all-benchmarks ()
  "Run all benchmark suites"
  (register-all-suites)
  (dolist (suite (list-suites))
    (format t "~%Running suite: ~A~%" suite)
    (run-suite suite)))

(defun save-baselines ()
  "Save baseline results for configured suites"
  (register-all-suites)
  (dolist (suite *baseline-suites*)
    (when (get-suite suite)
      (format t "~%Saving baselines for suite: ~A~%" suite)
      (let ((results (run-suite suite)))
        (dolist (result results)
          (benchmark:save-baseline result))))))

(defun compare-baselines ()
  "Compare current performance against baselines"
  (register-all-suites)
  (let ((any-regression nil))
    (dolist (suite *baseline-suites*)
      (when (get-suite suite)
        (format t "~%Comparing suite: ~A~%" suite)
        (let ((results (run-suite suite)))
          (dolist (result results)
            (let ((comparison (benchmark:compare-with-baseline result)))
              (when comparison
                (let ((name (benchmark:benchmark-result-name result))
                      (regression (getf comparison :regression))
                      (improvement (getf comparison :improvement))
                      (percent (getf comparison :percent-change)))
                  (cond
                    (regression
                     (setf any-regression t)
                     (format t "  ~A: REGRESSION DETECTED (~,1F% slower)~%" name percent))
                    (improvement
                     (format t "  ~A: Performance improved (~,1F% faster)~%" name (- percent)))
                    (t
                     (format t "  ~A: No significant change (~,1F%)~%" name percent))))))))))
    (when any-regression
      (format t "~%WARNING: Performance regressions detected!~%")
      (sb-ext:exit :code 1))))