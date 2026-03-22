;;;; FFI Performance Benchmarks
;;;;
;;;; Benchmarks for Foreign Function Interface operations.
;;;; Uses the epsilon.tool.benchmark framework for consistent measurement.

(defpackage epsilon.tool.benchmark.ffi
  (:use cl)
  (:local-nicknames
   (bench epsilon.tool.benchmark)
   (lib epsilon.foreign))
  (:export
   register-ffi-benchmarks
   run-memory-throughput-benchmarks
   quick-performance-test)
  (:enter t))

;;; Define shared library functions for benchmarking

(lib:defshared bench-strlen "strlen" "libc" :unsigned-long
  (str :string)
  :documentation "strlen for benchmarking")

(lib:defshared bench-getpid "getpid" "libc" :int
  :documentation "getpid for benchmarking")

(lib:defshared bench-memcmp "memcmp" "libc" :int
  (s1 :pointer) (s2 :pointer) (n :unsigned-long)
  :documentation "memcmp for benchmarking")

(lib:defshared bench-memcpy "memcpy" "libc" :pointer
  (dst :pointer) (src :pointer) (n :unsigned-long)
  :documentation "memcpy for benchmarking")

(lib:defshared bench-abs "abs" "libc" :int
  (n :int)
  :documentation "abs for benchmarking")

;;; Zero-argument FFI calls

(defun register-zero-arg-benchmarks ()
  "Register benchmarks for zero-argument FFI calls"

  (bench:defbenchmark ffi/call/getpid ()
    (bench:consume (bench-getpid))))

;;; Simple argument FFI calls

(defun register-simple-arg-benchmarks ()
  "Register benchmarks for simple argument FFI calls"

  ;; Integer argument
  (bench:defbenchmark ffi/call/abs-int ()
    (bench:consume (bench-abs -42)))

  ;; String argument - short
  (bench:defbenchmark ffi/call/strlen-short ()
    (bench:consume (bench-strlen "Hello, World!")))

  ;; String argument - long
  (bench:defbenchmark ffi/call/strlen-long ()
    (let ((s (make-string 1000 :initial-element #\A)))
      (bench:consume (bench-strlen s)))))

;;; Pointer argument FFI calls

(defun register-pointer-arg-benchmarks ()
  "Register benchmarks for pointer argument FFI calls"

  ;; Small buffer comparison (128 bytes)
  (bench:defbenchmark ffi/memcmp/128b ()
    (let* ((size 128)
           (ptr1 (lib:foreign-alloc size))
           (ptr2 (lib:foreign-alloc size)))
      (unwind-protect
          (progn
            ;; Initialize with data
            (dotimes (i size)
              (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
              (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
            (bench:consume (bench-memcmp ptr1 ptr2 size)))
        (lib:foreign-free ptr1)
        (lib:foreign-free ptr2))))

  ;; Large buffer comparison (4KB)
  (bench:defbenchmark ffi/memcmp/4kb ()
    (let* ((size 4096)
           (ptr1 (lib:foreign-alloc size))
           (ptr2 (lib:foreign-alloc size)))
      (unwind-protect
          (progn
            (dotimes (i size)
              (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
              (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
            (bench:consume (bench-memcmp ptr1 ptr2 size)))
        (lib:foreign-free ptr1)
        (lib:foreign-free ptr2))))

  ;; Memory copy (1KB)
  (bench:defbenchmark ffi/memcpy/1kb ()
    (let* ((size 1024)
           (src (lib:foreign-alloc size))
           (dst (lib:foreign-alloc size)))
      (unwind-protect
          (progn
            (dotimes (i size)
              (setf (sb-sys:sap-ref-8 src i) (mod i 256)))
            (bench:consume (bench-memcpy dst src size)))
        (lib:foreign-free src)
        (lib:foreign-free dst))))

  ;; Memory copy (64KB)
  (bench:defbenchmark ffi/memcpy/64kb ()
    (let* ((size 65536)
           (src (lib:foreign-alloc size))
           (dst (lib:foreign-alloc size)))
      (unwind-protect
          (progn
            (dotimes (i size)
              (setf (sb-sys:sap-ref-8 src i) (mod i 256)))
            (bench:consume (bench-memcpy dst src size)))
        (lib:foreign-free src)
        (lib:foreign-free dst)))))

;;; Memory allocation benchmarks

(defun register-allocation-benchmarks ()
  "Register memory allocation benchmarks"

  (bench:defbenchmark ffi/alloc/malloc-free-64b ()
    (let ((ptr (lib:foreign-alloc 64)))
      (bench:consume ptr)
      (lib:foreign-free ptr)))

  (bench:defbenchmark ffi/alloc/malloc-free-1kb ()
    (let ((ptr (lib:foreign-alloc 1024)))
      (bench:consume ptr)
      (lib:foreign-free ptr)))

  (bench:defbenchmark ffi/alloc/malloc-free-64kb ()
    (let ((ptr (lib:foreign-alloc 65536)))
      (bench:consume ptr)
      (lib:foreign-free ptr)))

  (bench:defbenchmark ffi/alloc/malloc-free-1mb ()
    (let ((ptr (lib:foreign-alloc 1048576)))
      (bench:consume ptr)
      (lib:foreign-free ptr))))

;;; Struct operations (if available)

(defun register-struct-benchmarks ()
  "Register struct manipulation benchmarks"

  ;; Only register if struct operations are available
  (when (fboundp 'lib:define-foreign-struct)
    ;; Define a simple struct for benchmarking
    (lib:define-foreign-struct benchmark-point
      (x :int)
      (y :int)
      (z :int))

    (bench:defbenchmark ffi/struct/create ()
      (bench:consume
       (lib:make-foreign-struct 'benchmark-point
                               :x 10 :y 20 :z 30)))

    (let ((point (lib:make-foreign-struct 'benchmark-point
                                          :x 10 :y 20 :z 30)))
      (bench:defbenchmark ffi/struct/access ()
        (bench:consume
         (+ (lib:foreign-struct-slot point 'benchmark-point 'x)
            (lib:foreign-struct-slot point 'benchmark-point 'y)
            (lib:foreign-struct-slot point 'benchmark-point 'z))))

      (bench:defbenchmark ffi/struct/modify ()
        (setf (lib:foreign-struct-slot point 'benchmark-point 'x) 100)
        (setf (lib:foreign-struct-slot point 'benchmark-point 'y) 200)
        (bench:consume
         (setf (lib:foreign-struct-slot point 'benchmark-point 'z) 300))))))

;;; Callback benchmarks (if available)

(defun register-callback-benchmarks ()
  "Register callback benchmarks"

  (when (fboundp 'lib:define-foreign-callback)
    ;; Define a simple callback
    (lib:define-foreign-callback benchmark-callback :int ((x :int))
      (* x 2))

    ;; Benchmark callback creation
    (bench:defbenchmark ffi/callback/create ()
      (bench:consume (lib:create-callback 'benchmark-callback)))

    ;; Benchmark callback invocation (if possible)
    (when (fboundp 'lib:invoke-callback)
      (let ((callback (lib:create-callback 'benchmark-callback)))
        (bench:defbenchmark ffi/callback/invoke ()
          (bench:consume (lib:invoke-callback callback 21)))))))

;;; Comparison with SBCL Alien

(defun register-comparison-benchmarks ()
  "Register benchmarks comparing with SBCL's alien interface"

  ;; Define SBCL alien version for comparison
  (sb-alien:define-alien-routine "strlen" sb-alien:unsigned-long
    (str sb-alien:c-string))

  (let ((test-string "Hello, World!"))
    ;; Epsilon FFI version
    (bench:defbenchmark ffi/compare/epsilon-strlen ()
      (bench:consume (bench-strlen test-string)))

    ;; SBCL alien version
    (bench:defbenchmark ffi/compare/sbcl-strlen ()
      (bench:consume (strlen test-string)))))

;;; Parameterized Memory Throughput Benchmarks

(defun run-memory-throughput-benchmarks ()
  "Run memory operation benchmarks with throughput metrics"

  ;; memcpy throughput at various sizes
  (bench:with-benchmark-group "ffi/memcpy-throughput"
    (dolist (size '(64 256 1024 4096 16384 65536 262144))
      (let ((src (lib:foreign-alloc size))
            (dst (lib:foreign-alloc size)))
        (unwind-protect
            (progn
              ;; Initialize source
              (dotimes (i (min size 1024))
                (setf (sb-sys:sap-ref-8 src i) (mod i 256)))
              (bench:benchmark-with-input (format nil "memcpy-~A" (format-size size))
                :input (list dst src size)
                :throughput (bench:throughput-bytes size)
                (bench:consume (bench-memcpy (first input) (second input) (third input)))))
          (lib:foreign-free src)
          (lib:foreign-free dst)))))

  ;; memcmp throughput at various sizes
  (bench:with-benchmark-group "ffi/memcmp-throughput"
    (dolist (size '(64 256 1024 4096 16384 65536))
      (let ((ptr1 (lib:foreign-alloc size))
            (ptr2 (lib:foreign-alloc size)))
        (unwind-protect
            (progn
              ;; Initialize with identical data
              (dotimes (i (min size 1024))
                (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
                (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
              (bench:benchmark-with-input (format nil "memcmp-~A" (format-size size))
                :input (list ptr1 ptr2 size)
                :throughput (bench:throughput-bytes size)
                (bench:consume (bench-memcmp (first input) (second input) (third input)))))
          (lib:foreign-free ptr1)
          (lib:foreign-free ptr2)))))

  ;; malloc/free throughput at various sizes
  (bench:with-benchmark-group "ffi/alloc-throughput"
    (dolist (size '(64 256 1024 4096 16384 65536 262144 1048576))
      (bench:benchmark-with-input (format nil "malloc-free-~A" (format-size size))
        :input size
        :throughput (bench:throughput-bytes size)
        (let ((ptr (lib:foreign-alloc input)))
          (bench:consume ptr)
          (lib:foreign-free ptr))))))

(defun format-size (bytes)
  "Format byte size for display"
  (cond
    ((>= bytes 1048576) (format nil "~DMB" (/ bytes 1048576)))
    ((>= bytes 1024) (format nil "~DKB" (/ bytes 1024)))
    (t (format nil "~DB" bytes))))

;;; Performance Budgets

(defun define-ffi-budgets ()
  "Define performance budgets for CI"
  ;; FFI call overhead should be minimal
  (bench:defbudget "ffi/call/getpid" :max-time 0.0000005)       ; 500ns
  (bench:defbudget "ffi/call/abs-int" :max-time 0.0000005)      ; 500ns
  (bench:defbudget "ffi/call/strlen-short" :max-time 0.000001)  ; 1us
  ;; Memory operations
  (bench:defbudget "ffi/memcpy/1kb" :max-time 0.000002)         ; 2us
  (bench:defbudget "ffi/alloc/malloc-free-1kb" :max-time 0.000005)) ; 5us

;;; Main Registration

(defun register-ffi-benchmarks ()
  "Register all FFI benchmarks"
  (register-zero-arg-benchmarks)
  (register-simple-arg-benchmarks)
  (register-pointer-arg-benchmarks)
  (register-allocation-benchmarks)
  (register-struct-benchmarks)
  (register-callback-benchmarks)
  (register-comparison-benchmarks)
  (define-ffi-budgets))

;;; Entry Points

(defun quick-performance-test ()
  "Quick performance test for CI/CD with budget checking"
  (format t "~%Running quick FFI performance test...~%")

  (register-ffi-benchmarks)

  (let ((results nil))
    (dolist (name '(ffi/call/getpid
                    ffi/call/abs-int
                    ffi/call/strlen-short
                    ffi/memcpy/1kb
                    ffi/alloc/malloc-free-1kb))
      (let ((fn (bench:get-benchmark name)))
        (when fn
          (push (bench:run-benchmark fn :name (string name)) results))))

    (bench:check-budgets (nreverse results))))
