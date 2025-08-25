;;;; HTTP Test Runner
;;;;
;;;; Comprehensive test harness for all HTTP functionality

(defpackage :epsilon.http.test-runner
  (:use :cl)
  (:export
   #:run-all-tests
   #:run-basic-tests
   #:run-integration-tests
   #:run-external-tests
   #:run-performance-tests
   #:test-report))

(in-package :epsilon.http.test-runner)

(defparameter *test-results* nil
  "Collection of test results")

(defparameter *test-start-time* nil
  "Test suite start time")

(defstruct test-result
  suite
  name
  status  ; :pass :fail :error :skip
  duration
  message)

;;;; Test Suite Runners

(defun run-test-suite (suite-name package-name &key skip-external)
  "Run a specific test suite and collect results"
  (format t "~&~%═══════════════════════════════════════════════~%")
  (format t "Running ~A tests...~%" suite-name)
  (format t "═══════════════════════════════════════════════~%")
  
  (let ((start-time (get-internal-real-time))
        (suite-results nil))
    
    (handler-case
        (progn
          ;; Set environment for external tests if needed
          (when skip-external
            (setf (sb-ext:posix-getenv "EPSILON_SKIP_EXTERNAL_TESTS") "1"))
          
          ;; Load and run the test package
          (let ((test-package (find-package package-name)))
            (if test-package
                (progn
                  ;; Run all tests in the package
                  (do-symbols (sym test-package)
                    (when (and (fboundp sym)
                              (str:starts-with-p "TEST-" (string sym)))
                      (let ((test-start (get-internal-real-time)))
                        (handler-case
                            (progn
                              (funcall sym)
                              (push (make-test-result
                                    :suite suite-name
                                    :name (string sym)
                                    :status :pass
                                    :duration (/ (- (get-internal-real-time) test-start)
                                               internal-time-units-per-second))
                                   suite-results))
                          (epsilon.test:test-skipped (e)
                            (push (make-test-result
                                  :suite suite-name
                                  :name (string sym)
                                  :status :skip
                                  :duration 0
                                  :message (format nil "~A" e))
                                 suite-results))
                          (error (e)
                            (push (make-test-result
                                  :suite suite-name
                                  :name (string sym)
                                  :status :fail
                                  :duration (/ (- (get-internal-real-time) test-start)
                                             internal-time-units-per-second)
                                  :message (format nil "~A" e))
                                 suite-results)))))))
                (format t "Warning: Package ~A not found~%" package-name))))
      (error (e)
        (format t "Error running suite ~A: ~A~%" suite-name e)))
    
    ;; Print suite summary
    (let* ((total (length suite-results))
           (passed (count :pass suite-results :key #'test-result-status))
           (failed (count :fail suite-results :key #'test-result-status))
           (skipped (count :skip suite-results :key #'test-result-status))
           (duration (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
      
      (format t "~%Suite: ~A - " suite-name)
      (format t "~D tests, ~D passed, ~D failed, ~D skipped (~,2F seconds)~%"
              total passed failed skipped duration)
      
      ;; Show failures
      (when (> failed 0)
        (format t "~%Failures:~%")
        (dolist (result suite-results)
          (when (eq (test-result-status result) :fail)
            (format t "  ✗ ~A: ~A~%" 
                   (test-result-name result)
                   (test-result-message result)))))
      
      ;; Add to global results
      (setf *test-results* (append suite-results *test-results*))
      
      suite-results)))

(defun run-basic-tests ()
  "Run basic HTTP functionality tests"
  (run-test-suite "Basic HTTP" :epsilon.http.tests))

(defun run-integration-tests ()
  "Run local client-server integration tests"
  (run-test-suite "Local Integration" :epsilon.http.test-local-integration))

(defun run-external-tests (&key skip)
  "Run external server compliance tests"
  (run-test-suite "External Servers" :epsilon.http.test-external-servers
                 :skip-external skip))

(defun run-mtls-tests ()
  "Run mTLS tests"
  (run-test-suite "mTLS" :epsilon.http.test-mtls-http1))

(defun run-streaming-tests ()
  "Run streaming tests"
  (run-test-suite "Streaming" :epsilon.http.streaming.tests))

(defun run-pool-tests ()
  "Run connection pool tests"
  (run-test-suite "Connection Pool" :epsilon.http.pool.tests))

(defun run-performance-tests ()
  "Run performance benchmarks"
  (format t "~&~%═══════════════════════════════════════════════~%")
  (format t "Running Performance Benchmarks...~%")
  (format t "═══════════════════════════════════════════════~%")
  
  ;; Simple performance tests
  (let ((start (get-internal-real-time)))
    
    ;; Test 1: Measure request latency
    (format t "~%1. Request Latency Test~%")
    (handler-case
        (let ((times nil))
          (dotimes (i 10)
            (let ((req-start (get-internal-real-time)))
              (epsilon.http:get "http://httpbin.org/get")
              (push (/ (- (get-internal-real-time) req-start)
                      internal-time-units-per-second)
                   times)))
          (format t "   Average latency: ~,3F seconds~%"
                 (/ (reduce #'+ times) (length times)))
          (format t "   Min latency: ~,3F seconds~%"
                 (reduce #'min times))
          (format t "   Max latency: ~,3F seconds~%"
                 (reduce #'max times)))
      (error (e)
        (format t "   Error: ~A~%" e)))
    
    ;; Test 2: Concurrent request throughput
    (format t "~%2. Concurrent Request Test~%")
    (handler-case
        (let ((threads nil)
              (success-count 0))
          (dotimes (i 20)
            (push (sb-thread:make-thread
                   (lambda ()
                     (handler-case
                         (progn
                           (epsilon.http:get "http://httpbin.org/get")
                           (sb-ext:atomic-incf success-count))
                       (error () nil))))
                  threads))
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          (format t "   Successful requests: ~D/20~%" success-count))
      (error (e)
        (format t "   Error: ~A~%" e)))
    
    (format t "~%Total benchmark time: ~,2F seconds~%"
           (/ (- (get-internal-real-time) start)
              internal-time-units-per-second))))

(defun test-report ()
  "Generate comprehensive test report"
  (format t "~&~%")
  (format t "╔═══════════════════════════════════════════════════════╗~%")
  (format t "║           HTTP MODULE TEST REPORT                      ║~%")
  (format t "╚═══════════════════════════════════════════════════════╝~%")
  
  (when *test-results*
    (let* ((total (length *test-results*))
           (passed (count :pass *test-results* :key #'test-result-status))
           (failed (count :fail *test-results* :key #'test-result-status))
           (skipped (count :skip *test-results* :key #'test-result-status))
           (error-count (count :error *test-results* :key #'test-result-status))
           (total-duration (reduce #'+ *test-results* :key #'test-result-duration)))
      
      (format t "~%Summary:~%")
      (format t "  Total Tests:    ~D~%" total)
      (format t "  Passed:         ~D (~,1F%)~%" 
              passed (if (> total 0) (* 100 (/ passed total)) 0))
      (format t "  Failed:         ~D~%" failed)
      (format t "  Skipped:        ~D~%" skipped)
      (format t "  Errors:         ~D~%" error-count)
      (format t "  Total Duration: ~,2F seconds~%" total-duration)
      
      ;; Coverage by suite
      (format t "~%Coverage by Suite:~%")
      (let ((suites (remove-duplicates *test-results* 
                                      :key #'test-result-suite
                                      :test #'string=)))
        (dolist (suite-result suites)
          (let* ((suite (test-result-suite suite-result))
                 (suite-tests (remove-if-not (lambda (r)
                                              (string= (test-result-suite r) suite))
                                            *test-results*))
                 (suite-passed (count :pass suite-tests :key #'test-result-status))
                 (suite-total (length suite-tests)))
            (format t "  ~A: ~D/~D passed~%" suite suite-passed suite-total))))
      
      ;; List all failures
      (when (> failed 0)
        (format t "~%Failed Tests:~%")
        (dolist (result *test-results*)
          (when (eq (test-result-status result) :fail)
            (format t "  [~A] ~A~%" 
                   (test-result-suite result)
                   (test-result-name result))
            (when (test-result-message result)
              (format t "    → ~A~%" (test-result-message result))))))
      
      ;; List skipped tests
      (when (> skipped 0)
        (format t "~%Skipped Tests:~%")
        (dolist (result *test-results*)
          (when (eq (test-result-status result) :skip)
            (format t "  [~A] ~A~%" 
                   (test-result-suite result)
                   (test-result-name result))
            (when (test-result-message result)
              (format t "    → ~A~%" (test-result-message result))))))
      
      ;; Final verdict
      (format t "~%")
      (cond
        ((= failed 0)
         (format t "✅ All tests passed!~%"))
        ((< failed 3)
         (format t "⚠️  Minor issues detected (~D failures)~%" failed))
        (t
         (format t "❌ Multiple failures detected (~D failures)~%" failed))))))

(defun run-all-tests (&key skip-external skip-performance)
  "Run all HTTP tests"
  (setf *test-results* nil)
  (setf *test-start-time* (get-internal-real-time))
  
  (format t "~%╔═══════════════════════════════════════════════════════╗~%")
  (format t "║         EPSILON HTTP TEST SUITE                        ║~%")
  (format t "╚═══════════════════════════════════════════════════════╝~%")
  (format t "~%Starting comprehensive HTTP module testing...~%")
  
  ;; Run test suites
  (run-basic-tests)
  (run-integration-tests)
  
  (unless skip-external
    (run-external-tests))
  
  ;; Run specialized tests
  (handler-case (run-mtls-tests)
    (error (e) (format t "mTLS tests error: ~A~%" e)))
  
  (handler-case (run-streaming-tests)
    (error (e) (format t "Streaming tests error: ~A~%" e)))
  
  (handler-case (run-pool-tests)
    (error (e) (format t "Pool tests error: ~A~%" e)))
  
  ;; Run performance tests
  (unless skip-performance
    (run-performance-tests))
  
  ;; Generate final report
  (test-report)
  
  ;; Return success/failure
  (= 0 (count :fail *test-results* :key #'test-result-status)))

;;;; Command Line Interface

(defun main ()
  "Main entry point for test runner"
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond
      ((member "--help" args :test #'string=)
       (format t "HTTP Test Runner~%")
       (format t "Usage: epsilon-http-test [options]~%")
       (format t "Options:~%")
       (format t "  --all              Run all tests~%")
       (format t "  --basic            Run basic tests only~%")
       (format t "  --integration      Run integration tests only~%")
       (format t "  --external         Run external server tests only~%")
       (format t "  --skip-external    Skip external server tests~%")
       (format t "  --skip-performance Skip performance tests~%")
       (format t "  --help             Show this help~%"))
      
      ((member "--basic" args :test #'string=)
       (run-basic-tests)
       (test-report))
      
      ((member "--integration" args :test #'string=)
       (run-integration-tests)
       (test-report))
      
      ((member "--external" args :test #'string=)
       (run-external-tests)
       (test-report))
      
      (t
       ;; Default: run all tests
       (run-all-tests 
        :skip-external (member "--skip-external" args :test #'string=)
        :skip-performance (member "--skip-performance" args :test #'string=))))))