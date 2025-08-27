;;;; Test Framework for epsilon.foreign Refactoring
;;;; 
;;;; Provides infrastructure for parallel testing of old and new implementations

(defpackage epsilon.foreign.test-framework
  (:use cl epsilon.test)
  (:export
   ;; Test execution
   #:with-parallel-test
   #:compare-implementations
   #:assert-same-result
   #:assert-better-performance
   
   ;; Benchmarking
   #:benchmark-operation
   #:measure-memory
   #:detect-memory-leak
   
   ;; Compatibility testing
   #:test-api-compatibility
   #:verify-deprecation-warning
   #:check-migration-path))

(in-package epsilon.foreign.test-framework)

;;;; Parallel Testing Infrastructure

(defmacro with-parallel-test ((old-result new-result) &body body)
  "Execute body with both old and new implementations, binding results"
  `(let ((,old-result (with-implementation :old ,@body))
         (,new-result (with-implementation :new ,@body)))
     (values ,old-result ,new-result)))

(defun compare-implementations (operation &rest args)
  "Compare results from old and new implementations"
  (with-parallel-test (old-result new-result)
    (apply operation args))
  (assert-same-result old-result new-result))

(defun assert-same-result (old new &key (test #'equal))
  "Assert that old and new implementations produce same result"
  (unless (funcall test old new)
    (error "Implementation mismatch:~%Old: ~S~%New: ~S" old new)))

;;;; Performance Benchmarking

(defstruct benchmark-result
  name
  iterations
  total-time
  average-time
  min-time
  max-time
  memory-allocated
  gc-count)

(defun benchmark-operation (name operation &key (iterations 10000) warmup-iterations)
  "Benchmark an operation, returning detailed metrics"
  (when warmup-iterations
    (dotimes (i warmup-iterations)
      (funcall operation)))
  
  (let ((times '())
        (start-memory (sb-ext:get-bytes-consed))
        (start-gc-count 0))
    
    (dotimes (i iterations)
      (let ((start (get-internal-real-time)))
        (funcall operation)
        (push (- (get-internal-real-time) start) times)))
    
    (let ((sorted-times (sort times #'<)))
      (make-benchmark-result
       :name name
       :iterations iterations
       :total-time (reduce #'+ sorted-times)
       :average-time (/ (reduce #'+ sorted-times) iterations)
       :min-time (first sorted-times)
       :max-time (first (last sorted-times))
       :memory-allocated (- (sb-ext:get-bytes-consed) start-memory)
       :gc-count 0))))

(defun assert-better-performance (new-time old-time &key (threshold 0.5))
  "Assert new implementation is at least (1-threshold) times faster"
  (unless (< new-time (* old-time (- 1 threshold)))
    (error "Performance regression: new ~F vs old ~F (threshold ~F)"
           new-time old-time threshold)))

;;;; Memory Management Testing

(defun measure-memory (operation)
  "Measure memory allocated by an operation"
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall operation)
    (- (sb-ext:get-bytes-consed) before)))

(defmacro detect-memory-leak (operation &key (iterations 100) (threshold 0.1))
  "Detect if operation leaks memory over iterations"
  `(let* ((single-alloc (measure-memory ,operation))
          (total-alloc (measure-memory 
                        (lambda () 
                          (dotimes (i ,iterations)
                            (funcall ,operation)))))
          (expected (* single-alloc ,iterations))
          (leak-ratio (/ (- total-alloc expected) expected)))
     (when (> leak-ratio ,threshold)
       (error "Memory leak detected: ~F% over expected" 
              (* leak-ratio 100)))))

;;;; Compatibility Testing

(defun test-api-compatibility (old-function new-function test-cases)
  "Test that new function maintains compatibility with old"
  (dolist (test-case test-cases)
    (destructuring-bind (args &key expected-result expected-error) test-case
      (if expected-error
          (assert-same-error 
           (apply old-function args)
           (apply new-function args))
          (assert-same-result
           (apply old-function args)
           (apply new-function args))))))

(defun verify-deprecation-warning (deprecated-function)
  "Verify that calling deprecated function produces warning"
  (let ((warnings '()))
    (handler-bind ((warning (lambda (w) 
                              (push w warnings)
                              (muffle-warning w))))
      (funcall deprecated-function))
    (assert (some (lambda (w) 
                    (search "deprecated" (format nil "~A" w)))
                  warnings)
            ()
            "No deprecation warning for ~S" deprecated-function)))

(defun check-migration-path (old-code expected-new-code)
  "Verify that migration documentation is correct"
  (let ((migrated (migrate-code old-code)))
    (unless (equal migrated expected-new-code)
      (error "Migration failed:~%Old: ~S~%Expected: ~S~%Got: ~S"
             old-code expected-new-code migrated))))

;;;; Test Result Reporting

(defun report-benchmark-comparison (old-result new-result)
  "Generate comparison report for benchmarks"
  (format t "~%Performance Comparison: ~A~%" (benchmark-result-name old-result))
  (format t "~20A  Old        New        Improvement~%" "Metric")
  (format t "~60,,,'-A~%" "")
  (flet ((report-metric (name old new)
           (format t "~20A  ~10F ~10F ~10,2F%~%"
                   name old new 
                   (* 100 (- 1 (/ new old))))))
    (report-metric "Average Time" 
                   (benchmark-result-average-time old-result)
                   (benchmark-result-average-time new-result))
    (report-metric "Min Time"
                   (benchmark-result-min-time old-result)
                   (benchmark-result-min-time new-result))
    (report-metric "Memory/Op"
                   (/ (benchmark-result-memory-allocated old-result)
                      (benchmark-result-iterations old-result))
                   (/ (benchmark-result-memory-allocated new-result)
                      (benchmark-result-iterations new-result)))
    (report-metric "GC Count"
                   (benchmark-result-gc-count old-result)
                   (benchmark-result-gc-count new-result))))

;;;; Test Suite Management

(defvar *test-results* '())
(defvar *implementation-mode* :old)

(defmacro with-implementation (mode &body body)
  "Execute body with specific implementation active"
  (let ((old-mode (gensym)))
    `(let ((,old-mode *implementation-mode*))
       (unwind-protect
            (progn
              (setf *implementation-mode* ,mode)
              ,@body)
         (setf *implementation-mode* ,old-mode)))))

(defun record-test-result (test-name status &optional details)
  "Record result of a test for reporting"
  (push (list test-name status *implementation-mode* details)
        *test-results*))

(defun generate-test-report ()
  "Generate comprehensive test report"
  (format t "~%Test Results Summary~%")
  (format t "====================~%")
  (let ((passed 0) (failed 0))
    (dolist (result *test-results*)
      (destructuring-bind (name status mode details) result
        (if (eq status :pass)
            (incf passed)
            (incf failed))
        (format t "~A [~A] ~A~@[ - ~A~]~%"
                (if (eq status :pass) "✓" "✗")
                mode name details)))
    (format t "~%Total: ~D passed, ~D failed~%" passed failed)))