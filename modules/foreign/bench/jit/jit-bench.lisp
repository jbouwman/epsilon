;;;; jit-bench.lisp - Benchmarks for JIT-compiled FFI stubs
;;;;
;;;; Compare performance of:
;;;; 1. Direct SBCL alien-funcall (baseline)
;;;; 2. JIT stub calls

(defpackage :epsilon.foreign.jit.bench
  (:use :cl)
  (:local-nicknames
   (:jit :epsilon.foreign.jit))
  (:export #:run-benchmarks)
   (:enter t))

;;; ============================================================================
;;; Benchmark Infrastructure
;;; ============================================================================

(defmacro time-iterations (n &body body)
  "Execute BODY N times and return elapsed time in seconds."
  (let ((start (gensym "START"))
        (end (gensym "END"))
        (i (gensym "I")))
    `(let ((,start (get-internal-real-time)))
       (dotimes (,i ,n)
         (declare (ignorable ,i))
         ,@body)
       (let ((,end (get-internal-real-time)))
         (/ (- ,end ,start) internal-time-units-per-second)))))

(defun format-rate (count seconds)
  "Format calls per second with appropriate suffix."
  (let ((rate (/ count seconds)))
    (cond
      ((>= rate 1e9) (format nil "~,2F B/s" (/ rate 1e9)))
      ((>= rate 1e6) (format nil "~,2F M/s" (/ rate 1e6)))
      ((>= rate 1e3) (format nil "~,2F K/s" (/ rate 1e3)))
      (t (format nil "~,2F /s" rate)))))

;;; ============================================================================
;;; Direct FFI (Baseline)
;;; ============================================================================

;; Direct alien function definition
(sb-alien:define-alien-routine ("abs" libc-abs) sb-alien:int
  (n sb-alien:int))

(sb-alien:define-alien-routine ("getpid" libc-getpid) sb-alien:int)

;;; ============================================================================
;;; Benchmark Tests
;;; ============================================================================

(defun get-foreign-symbol-address (name)
  "Get the address of a foreign symbol as an integer."
  (let ((result (sb-sys:find-foreign-symbol-address name)))
    (etypecase result
      (integer result)
      (sb-sys:system-area-pointer (sb-sys:sap-int result))
      (null nil))))

(defun bench-abs-direct (iterations)
  "Benchmark direct alien-funcall to abs()."
  (let ((elapsed (time-iterations iterations
                   (libc-abs -42))))
    (values elapsed (format-rate iterations elapsed))))

(defun bench-abs-jit (iterations)
  "Benchmark JIT stub call to abs()."
  (let* ((abs-addr (get-foreign-symbol-address "abs"))
         (stub (jit::compile-stub abs-addr :int '(:int))))
    (let ((elapsed (time-iterations iterations
                     (jit:call-stub stub -42))))
      (values elapsed (format-rate iterations elapsed)))))

(defun bench-getpid-direct (iterations)
  "Benchmark direct alien-funcall to getpid()."
  (let ((elapsed (time-iterations iterations
                   (libc-getpid))))
    (values elapsed (format-rate iterations elapsed))))

(defun bench-getpid-jit (iterations)
  "Benchmark JIT stub call to getpid()."
  (let* ((getpid-addr (get-foreign-symbol-address "getpid"))
         (stub (jit::compile-stub getpid-addr :int '())))
    (let ((elapsed (time-iterations iterations
                     (jit:call-stub stub))))
      (values elapsed (format-rate iterations elapsed)))))

;;; ============================================================================
;;; Main Benchmark Runner
;;; ============================================================================

(defun run-benchmarks (&key (iterations 1000000) (warmup 10000))
  "Run all benchmarks and print results."
  (let ((out *standard-output*))
    (fresh-line out)
    (format out "~%JIT FFI Benchmark~%")
    (format out "=================~%")
    (format out "Platform: ~A~%" jit:*current-platform*)
    (format out "Iterations: ~:D~%" iterations)
    (format out "~%")
    (force-output out)

    ;; Clear stub cache before starting
    (jit:clear-stub-cache)

    ;; Warmup
    (format out "Warming up...~%")
    (force-output out)
    (dotimes (i warmup) (libc-abs -42))
    (let* ((abs-addr (get-foreign-symbol-address "abs"))
           (stub (jit::compile-stub abs-addr :int '(:int))))
      (dotimes (i warmup) (jit:call-stub stub -42)))
    (jit:clear-stub-cache)

    (format out "~%Benchmark Results~%")
    (format out "-----------------~%~%")
    (force-output out)

    ;; abs() benchmark
    (format out "abs(-42) - single integer argument:~%")
    (force-output out)
    (multiple-value-bind (direct-time direct-rate)
        (bench-abs-direct iterations)
      (format out "  Direct alien-funcall: ~,4Fs (~A)~%" direct-time direct-rate)
      (force-output out)
      (multiple-value-bind (jit-time jit-rate)
          (bench-abs-jit iterations)
        (format out "  JIT stub:             ~,4Fs (~A)~%" jit-time jit-rate)
        (format out "  Ratio (JIT/Direct):   ~,2Fx~%" (/ jit-time direct-time))
        (force-output out)))

    (jit:clear-stub-cache)
    (format out "~%")

    ;; getpid() benchmark
    (format out "getpid() - no arguments:~%")
    (force-output out)
    (multiple-value-bind (direct-time direct-rate)
        (bench-getpid-direct iterations)
      (format out "  Direct alien-funcall: ~,4Fs (~A)~%" direct-time direct-rate)
      (force-output out)
      (multiple-value-bind (jit-time jit-rate)
          (bench-getpid-jit iterations)
        (format out "  JIT stub:             ~,4Fs (~A)~%" jit-time jit-rate)
        (format out "  Ratio (JIT/Direct):   ~,2Fx~%" (/ jit-time direct-time))
        (force-output out)))

    (jit:clear-stub-cache)
    (format out "~%")

    ;; Notes
    (format out "Note: JIT overhead includes Lisp argument marshalling via call-stub.~%")
    (format out "      Direct alien-funcall is SBCL's optimized static FFI path.~%")
    (format out "~%")
    (force-output out)

    (values)))
