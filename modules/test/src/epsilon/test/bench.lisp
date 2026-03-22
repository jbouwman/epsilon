;;;; Benchmarking Support for epsilon.test
;;;;
;;;; Provides performance benchmarking with statistical analysis.
;;;;
;;;; Inspired by Go's testing.B and Rust's criterion crate.

(defpackage epsilon.test.bench
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.log log))
  (:enter t))

;;; Configuration

(defvar *warmup-iterations* 3
  "Number of warmup iterations before measuring.")

(defvar *min-iterations* 10
  "Minimum number of benchmark iterations.")

(defvar *max-iterations* 10000
  "Maximum number of benchmark iterations.")

(defvar *target-time* 1.0
  "Target time in seconds for benchmark run.")

(defvar *benchmark-results* nil
  "List of benchmark results from current run.")

;;; Timing Utilities

(defun get-time-ns ()
  "Get current time in nanoseconds."
  (* (get-internal-real-time)
     (/ 1000000000 internal-time-units-per-second)))

(defun time-call (fn)
  "Time a single call to FN, returning elapsed nanoseconds."
  (let ((start (get-time-ns)))
    (funcall fn)
    (- (get-time-ns) start)))

;;; Statistics

(defun mean (values)
  "Calculate arithmetic mean of VALUES."
  (if (null values)
      0
      (/ (reduce #'+ values) (length values))))

(defun variance (values &optional (m (mean values)))
  "Calculate variance of VALUES."
  (if (or (null values) (= 1 (length values)))
      0
      (/ (reduce #'+ (mapcar (lambda (x) (expt (- x m) 2)) values))
         (1- (length values)))))

(defun std-dev (values)
  "Calculate standard deviation of VALUES."
  (sqrt (variance values)))

(defun percentile (values p)
  "Calculate P-th percentile of VALUES (0-100)."
  (let* ((sorted (sort (copy-list values) #'<))
         (n (length sorted))
         (k (* (/ p 100) (1- n)))
         (f (floor k))
         (c (ceiling k)))
    (if (= f c)
        (nth f sorted)
        (/ (+ (nth f sorted) (nth c sorted)) 2))))

(defun median (values)
  "Calculate median of VALUES."
  (percentile values 50))

(defun min-value (values)
  "Return minimum value."
  (reduce #'min values))

(defun max-value (values)
  "Return maximum value."
  (reduce #'max values))

;;; Benchmark Result

(defclass benchmark-result ()
  ((name :initarg :name :reader bench-name)
   (iterations :initarg :iterations :reader bench-iterations)
   (total-time-ns :initarg :total-time :reader bench-total-time)
   (times :initarg :times :reader bench-times
          :documentation "List of individual iteration times in nanoseconds")
   (setup-fn :initarg :setup :initform nil :reader bench-setup)
   (teardown-fn :initarg :teardown :initform nil :reader bench-teardown))
  (:documentation "Result of running a benchmark."))

(defun bench-mean-ns (result)
  "Mean time per iteration in nanoseconds."
  (mean (bench-times result)))

(defun bench-mean-ms (result)
  "Mean time per iteration in milliseconds."
  (/ (bench-mean-ns result) 1000000.0))

(defun bench-mean-us (result)
  "Mean time per iteration in microseconds."
  (/ (bench-mean-ns result) 1000.0))

(defun bench-std-dev-ns (result)
  "Standard deviation in nanoseconds."
  (std-dev (bench-times result)))

(defun bench-min-ns (result)
  "Minimum time in nanoseconds."
  (min-value (bench-times result)))

(defun bench-max-ns (result)
  "Maximum time in nanoseconds."
  (max-value (bench-times result)))

(defun bench-ops-per-sec (result)
  "Operations per second."
  (let ((mean-ns (bench-mean-ns result)))
    (if (zerop mean-ns)
        0
        (/ 1000000000.0 mean-ns))))

(defun format-time (ns)
  "Format nanoseconds in appropriate unit."
  (cond
    ((>= ns 1000000000) (format nil "~,3Fs" (/ ns 1000000000.0)))
    ((>= ns 1000000) (format nil "~,3Fms" (/ ns 1000000.0)))
    ((>= ns 1000) (format nil "~,3Fus" (/ ns 1000.0)))
    (t (format nil "~,0Fns" ns))))

(defun format-benchmark-result (result &optional baseline)
  "Format a benchmark result for display."
  (with-output-to-string (s)
    (format s "~A~%" (bench-name result))
    (format s "  iterations: ~D~%" (bench-iterations result))
    (format s "  mean:       ~A~%" (format-time (bench-mean-ns result)))
    (format s "  std-dev:    ~A~%" (format-time (bench-std-dev-ns result)))
    (format s "  min:        ~A~%" (format-time (bench-min-ns result)))
    (format s "  max:        ~A~%" (format-time (bench-max-ns result)))
    (format s "  throughput: ~,0F ops/sec~%" (bench-ops-per-sec result))
    (when baseline
      (let* ((baseline-mean (bench-mean-ns baseline))
             (current-mean (bench-mean-ns result))
             (diff-pct (if (zerop baseline-mean)
                           0
                           (* 100 (/ (- current-mean baseline-mean) baseline-mean)))))
        (format s "  vs baseline: ~@D%~%" (round diff-pct))))))

;;; Benchmark Execution

(defun calibrate-iterations (fn)
  "Determine how many iterations to run to get meaningful timing."
  ;; Start with a few iterations and scale up
  (let ((iters 1)
        (target-ns (* *target-time* 1000000000)))
    (loop while (< iters *max-iterations*)
          do (let* ((start (get-time-ns))
                    (end (progn
                           (dotimes (i iters) (funcall fn))
                           (get-time-ns)))
                    (elapsed (- end start)))
               (when (>= elapsed target-ns)
                 (return (max *min-iterations* iters)))
               ;; Scale up iterations
               (setf iters (max (* iters 2)
                                (if (> elapsed 0)
                                    (ceiling (* iters (/ target-ns elapsed)))
                                    (* iters 10))))))
    *max-iterations*))

(defun run-benchmark (name fn &key setup teardown)
  "Run a benchmark for function FN.
   SETUP is called before each iteration.
   TEARDOWN is called after each iteration."
  ;; Warmup
  (dotimes (i *warmup-iterations*)
    (when setup (funcall setup))
    (funcall fn)
    (when teardown (funcall teardown)))

  ;; Calibrate
  (let* ((iterations (calibrate-iterations fn))
         (times nil)
         (total-start (get-time-ns)))

    ;; Run benchmark
    (dotimes (i iterations)
      (when setup (funcall setup))
      (let ((start (get-time-ns)))
        (funcall fn)
        (push (- (get-time-ns) start) times))
      (when teardown (funcall teardown)))

    (let ((total-time (- (get-time-ns) total-start)))
      (make-instance 'benchmark-result
                     :name name
                     :iterations iterations
                     :total-time total-time
                     :times (nreverse times)
                     :setup setup
                     :teardown teardown))))

(defun compare-benchmarks (results)
  "Compare multiple benchmark results, using first as baseline."
  (when results
    (let ((baseline (first results)))
      (with-output-to-string (s)
        (format s "Benchmark Comparison~%")
        (format s "====================~%~%")
        (dolist (result results)
          (format s "~A" (format-benchmark-result result
                                                   (unless (eq result baseline)
                                                     baseline)))
          (format s "~%"))))))

;;; Benchmark Collection

(defvar *benchmarks* (make-hash-table :test 'equal)
  "Registry of defined benchmarks.")

(defun register-benchmark (name fn &key setup teardown description)
  "Register a benchmark function."
  (setf (gethash name *benchmarks*)
        (list :fn fn :setup setup :teardown teardown :description description)))

(defun get-benchmark (name)
  "Get a registered benchmark by name."
  (gethash name *benchmarks*))

(defun list-benchmarks ()
  "List all registered benchmarks."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *benchmarks*)
    (sort names #'string<)))

(defun run-benchmarks (&optional pattern)
  "Run all benchmarks matching PATTERN (or all if nil)."
  (let ((results nil))
    (maphash
     (lambda (name spec)
       (when (or (null pattern)
                 (search pattern name :test #'char-equal))
         (format t "Running benchmark: ~A~%" name)
         (let ((result (run-benchmark name
                                      (getf spec :fn)
                                      :setup (getf spec :setup)
                                      :teardown (getf spec :teardown))))
           (push result results)
           (format t "~A~%" (format-benchmark-result result)))))
     *benchmarks*)
    (nreverse results)))

(defun clear-benchmarks ()
  "Clear all registered benchmarks."
  (clrhash *benchmarks*))
