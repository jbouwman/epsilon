;;;; JIT FFI Performance Benchmarks
;;;;
;;;; Comprehensive benchmarks for the epsilon.foreign.jit module.
;;;; Uses the epsilon.tool.benchmark framework for consistent measurement.
;;;;
;;;; Benchmark Categories:
;;;; 1. Call Overhead - JIT vs static alien-funcall vs libffi
;;;; 2. Callback Overhead - Lisp functions callable from C
;;;; 3. Signature Discovery - libclang parsing times
;;;; 4. Memory Usage - Stub region consumption

(defpackage epsilon.tool.benchmark.jit-ffi
  (:use cl)
  (:local-nicknames
   (bench epsilon.tool.benchmark)
   (suites epsilon.tool.benchmark.suites)
   (jit epsilon.foreign.jit))
  (:export
   register-jit-ffi-benchmarks
   run-call-overhead-benchmarks
   run-callback-benchmarks
   quick-performance-test)
  (:enter t))

;;; Static alien definitions for baseline comparison

(sb-alien:define-alien-routine ("abs" libc-abs) sb-alien:int
  (n sb-alien:int))

(sb-alien:define-alien-routine ("getpid" libc-getpid) sb-alien:int)

(sb-alien:define-alien-routine ("sin" libc-sin) sb-alien:double
  (x sb-alien:double))

(sb-alien:define-alien-routine ("cos" libc-cos) sb-alien:double
  (x sb-alien:double))

(sb-alien:define-alien-routine ("sqrt" libc-sqrt) sb-alien:double
  (x sb-alien:double))

(sb-alien:define-alien-routine ("floor" libc-floor) sb-alien:double
  (x sb-alien:double))

(sb-alien:define-alien-routine ("strlen" libc-strlen) sb-alien:unsigned-long
  (s sb-alien:c-string))

;;; Utility functions

(defun get-foreign-symbol-address (name)
  "Get the address of a foreign symbol as an integer."
  (let ((result (sb-sys:find-foreign-symbol-address name)))
    (etypecase result
      (integer result)
      (sb-sys:system-area-pointer (sb-sys:sap-int result))
      (null nil))))

;;; ============================================================================
;;; Call Overhead Benchmarks
;;; ============================================================================

(defun register-call-overhead-benchmarks ()
  "Register benchmarks comparing call methods"

  ;; Zero-argument calls
  (bench:defbenchmark jit-ffi/call/static-getpid ()
    "Static alien-funcall to getpid() - baseline"
    (bench:consume (libc-getpid)))

  (let ((caller (jit:make-jit-caller
                 (get-foreign-symbol-address "getpid")
                 :int '())))
    (bench:defbenchmark jit-ffi/call/jit-getpid ()
      "JIT caller to getpid()"
      (bench:consume (funcall caller))))

  ;; Single integer argument
  (bench:defbenchmark jit-ffi/call/static-abs ()
    "Static alien-funcall to abs() - baseline"
    (bench:consume (libc-abs -42)))

  (let ((caller (jit:make-jit-caller
                 (get-foreign-symbol-address "abs")
                 :int '(:int))))
    (bench:defbenchmark jit-ffi/call/jit-abs ()
      "JIT caller to abs()"
      (bench:consume (funcall caller -42))))

  ;; Single double argument
  (bench:defbenchmark jit-ffi/call/static-sin ()
    "Static alien-funcall to sin() - baseline"
    (bench:consume (libc-sin 1.0d0)))

  (let ((caller (jit:make-jit-caller
                 (get-foreign-symbol-address "sin")
                 :double '(:double))))
    (bench:defbenchmark jit-ffi/call/jit-sin ()
      "JIT caller to sin()"
      (bench:consume (funcall caller 1.0d0))))

  ;; Multiple double operations for throughput
  (bench:defbenchmark jit-ffi/call/static-sqrt ()
    "Static alien-funcall to sqrt()"
    (bench:consume (libc-sqrt 2.0d0)))

  (let ((caller (jit:make-jit-caller
                 (get-foreign-symbol-address "sqrt")
                 :double '(:double))))
    (bench:defbenchmark jit-ffi/call/jit-sqrt ()
      "JIT caller to sqrt()"
      (bench:consume (funcall caller 2.0d0))))

  ;; String argument (pointer passing)
  (bench:defbenchmark jit-ffi/call/static-strlen ()
    "Static alien-funcall to strlen()"
    (bench:consume (libc-strlen "Hello, World!"))))

;;; ============================================================================
;;; Stub Compilation Benchmarks
;;; ============================================================================

(defun register-stub-compilation-benchmarks ()
  "Register benchmarks for stub compilation times"

  ;; Cold compilation (no cache)
  (bench:defbenchmark jit-ffi/compile/cold-int ()
    "Compile stub for int->int function (cold)"
    (jit:clear-stub-cache)
    (bench:consume
     (jit:make-jit-caller
      (get-foreign-symbol-address "abs")
      :int '(:int))))

  (bench:defbenchmark jit-ffi/compile/cold-double ()
    "Compile stub for double->double function (cold)"
    (jit:clear-stub-cache)
    (bench:consume
     (jit:make-jit-caller
      (get-foreign-symbol-address "sin")
      :double '(:double))))

  ;; Cached retrieval
  (let ((addr (get-foreign-symbol-address "abs")))
    ;; Pre-warm cache
    (jit:make-jit-caller addr :int '(:int))
    (bench:defbenchmark jit-ffi/compile/cached-lookup ()
      "Retrieve cached stub (warm)"
      (bench:consume
       (jit:get-or-create-stub addr :int '(:int))))))

;;; ============================================================================
;;; Memory Usage Benchmarks
;;; ============================================================================

(defun register-memory-benchmarks ()
  "Register memory usage benchmarks"

  (bench:defbenchmark jit-ffi/memory/stub-region-alloc ()
    "Allocate new stub region"
    (let ((region (jit:make-executable-memory 4096)))
      (bench:consume region)
      (jit:free-executable-memory region)))

  ;; Measure stub sizes
  (bench:defbenchmark jit-ffi/memory/stub-size-int ()
    "Size of int->int stub"
    (jit:clear-stub-cache)
    (let* ((caller (jit:make-jit-caller
                    (get-foreign-symbol-address "abs")
                    :int '(:int)))
           (stats (jit:stub-cache-stats)))
      (declare (ignore caller))
      (bench:consume (getf stats :region-used)))))

;;; ============================================================================
;;; Parameterized Call Overhead Comparison
;;; ============================================================================

(defun run-call-overhead-benchmarks ()
  "Run parameterized benchmarks comparing call methods"

  ;; Integer functions comparison
  (bench:with-benchmark-group "jit-ffi/call-overhead/int"
    (let ((abs-jit (jit:make-jit-caller
                    (get-foreign-symbol-address "abs")
                    :int '(:int))))

      (bench:benchmark-with-input "static-abs"
        :input -42
        :throughput (bench:throughput-elements 1)
        (bench:consume (libc-abs input)))

      (bench:benchmark-with-input "jit-abs"
        :input -42
        :throughput (bench:throughput-elements 1)
        (bench:consume (funcall abs-jit input)))))

  ;; Floating point functions comparison
  (bench:with-benchmark-group "jit-ffi/call-overhead/float"
    (let ((sin-jit (jit:make-jit-caller
                    (get-foreign-symbol-address "sin")
                    :double '(:double)))
          (cos-jit (jit:make-jit-caller
                    (get-foreign-symbol-address "cos")
                    :double '(:double)))
          (sqrt-jit (jit:make-jit-caller
                     (get-foreign-symbol-address "sqrt")
                     :double '(:double))))

      ;; sin comparison
      (bench:benchmark-with-input "static-sin"
        :input 1.0d0
        :throughput (bench:throughput-elements 1)
        (bench:consume (libc-sin input)))

      (bench:benchmark-with-input "jit-sin"
        :input 1.0d0
        :throughput (bench:throughput-elements 1)
        (bench:consume (funcall sin-jit input)))

      ;; sqrt comparison
      (bench:benchmark-with-input "static-sqrt"
        :input 2.0d0
        :throughput (bench:throughput-elements 1)
        (bench:consume (libc-sqrt input)))

      (bench:benchmark-with-input "jit-sqrt"
        :input 2.0d0
        :throughput (bench:throughput-elements 1)
        (bench:consume (funcall sqrt-jit input))))))

;;; ============================================================================
;;; Callback Benchmarks (when available)
;;; ============================================================================

(defun register-callback-benchmarks ()
  "Register callback benchmarks if callback support is available"
  (when (find-package "EPSILON.FOREIGN.JIT.CALLBACK")
    (let ((cb-pkg (find-package "EPSILON.FOREIGN.JIT.CALLBACK")))
      (when (and cb-pkg (find-symbol "MAKE-JIT-CALLBACK" cb-pkg))
        ;; Callback creation
        (bench:defbenchmark jit-ffi/callback/create ()
          "Create a JIT callback"
          (let ((make-cb (symbol-function
                          (find-symbol "MAKE-JIT-CALLBACK" cb-pkg))))
            (bench:consume
             (funcall make-cb (lambda (x) (* x 2)) :int '(:int)))))))))

;;; ============================================================================
;;; Performance Budgets
;;; ============================================================================

(defun define-jit-ffi-budgets ()
  "Define performance budgets for CI"
  ;; Call overhead targets (from IMPL-151 spec)
  (bench:defbudget "jit-ffi/call/jit-getpid" :max-time 0.00000002)  ; 20ns
  (bench:defbudget "jit-ffi/call/jit-abs" :max-time 0.00000002)     ; 20ns
  (bench:defbudget "jit-ffi/call/jit-sin" :max-time 0.00000002)     ; 20ns

  ;; Static baseline should be faster
  (bench:defbudget "jit-ffi/call/static-getpid" :max-time 0.00000001)  ; 10ns
  (bench:defbudget "jit-ffi/call/static-abs" :max-time 0.00000001)     ; 10ns
  (bench:defbudget "jit-ffi/call/static-sin" :max-time 0.00000001)     ; 10ns

  ;; Stub compilation should be reasonably fast
  (bench:defbudget "jit-ffi/compile/cold-int" :max-time 0.001)      ; 1ms
  (bench:defbudget "jit-ffi/compile/cached-lookup" :max-time 0.000001))  ; 1us

;;; ============================================================================
;;; Main Registration
;;; ============================================================================

(defun register-jit-ffi-benchmarks ()
  "Register all JIT FFI benchmarks"
  (format t "~%Registering JIT FFI benchmarks...~%")
  (format t "  Platform: ~A~%" jit:*current-platform*)

  ;; Clear cache before registering
  (jit:clear-stub-cache)

  (register-call-overhead-benchmarks)
  (register-stub-compilation-benchmarks)
  (register-memory-benchmarks)
  (register-callback-benchmarks)
  (define-jit-ffi-budgets)

  ;; Register the suite
  (suites:register-suite 'jit-ffi-performance
    :description "JIT-compiled FFI performance benchmarks"
    :benchmarks '(;; Call overhead - static baselines
                  jit-ffi/call/static-getpid
                  jit-ffi/call/static-abs
                  jit-ffi/call/static-sin
                  jit-ffi/call/static-sqrt
                  jit-ffi/call/static-strlen
                  ;; Call overhead - JIT
                  jit-ffi/call/jit-getpid
                  jit-ffi/call/jit-abs
                  jit-ffi/call/jit-sin
                  jit-ffi/call/jit-sqrt
                  ;; Compilation
                  jit-ffi/compile/cold-int
                  jit-ffi/compile/cold-double
                  jit-ffi/compile/cached-lookup
                  ;; Memory
                  jit-ffi/memory/stub-region-alloc)))

;;; ============================================================================
;;; Entry Points
;;; ============================================================================

(defun quick-performance-test ()
  "Quick performance test for CI/CD with budget checking"
  (format t "~%Running quick JIT FFI performance test...~%")
  (format t "Platform: ~A~%" jit:*current-platform*)

  (jit:clear-stub-cache)
  (register-jit-ffi-benchmarks)

  (let ((results nil))
    (dolist (name '(jit-ffi/call/static-abs
                    jit-ffi/call/jit-abs
                    jit-ffi/call/static-sin
                    jit-ffi/call/jit-sin
                    jit-ffi/compile/cold-int
                    jit-ffi/compile/cached-lookup))
      (let ((fn (bench:get-benchmark name)))
        (when fn
          (format t "  Running ~A...~%" name)
          (push (bench:run-benchmark fn :name (string name)) results))))

    ;; Check against budgets
    (format t "~%Checking performance budgets...~%")
    (bench:check-budgets (nreverse results))))

(defun run-all-benchmarks ()
  "Run all JIT FFI benchmarks with detailed output"
  (format t "~%========================================~%")
  (format t "    JIT FFI Performance Benchmarks~%")
  (format t "========================================~%")
  (format t "Platform: ~A~%" jit:*current-platform*)
  (format t "~%")

  (jit:clear-stub-cache)
  (register-jit-ffi-benchmarks)

  ;; Run registered benchmarks
  (let ((benchmarks (bench:list-benchmarks)))
    (dolist (name benchmarks)
      (when (search "jit-ffi/" (string name))
        (let ((fn (bench:get-benchmark name)))
          (when fn
            (format t "~%Running: ~A~%" name)
            (let ((result (bench:run-benchmark fn :name (string name))))
              (bench:format-benchmark-result result)))))))

  ;; Run parameterized comparisons
  (format t "~%--- Call Overhead Comparison ---~%")
  (run-call-overhead-benchmarks)

  (format t "~%========================================~%")
  (format t "     Benchmarks Complete~%")
  (format t "========================================~%")

  ;; Print summary
  (let ((stats (jit:stub-cache-stats)))
    (format t "~%Stub Cache Stats:~%")
    (format t "  Stubs compiled: ~D~%" (getf stats :stub-count))
    (format t "  Memory used: ~D bytes~%" (getf stats :region-used))))
