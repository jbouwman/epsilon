;;;; FFI Performance Benchmarks
;;;;
;;;; This module provides benchmarks for Foreign Function Interface operations

(defpackage epsilon.tool.benchmark.ffi
  (:use cl)
  (:local-nicknames
   (benchmark epsilon.tool.benchmark)
   (suites epsilon.tool.benchmark.suites)
   (lib epsilon.foreign))
  (:export
   register-ffi-benchmarks))

(in-package epsilon.tool.benchmark.ffi)

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
  
  (benchmark:defbenchmark ffi-getpid ()
    (bench-getpid)))

;;; Simple argument FFI calls

(defun register-simple-arg-benchmarks ()
  "Register benchmarks for simple argument FFI calls"
  
  ;; Integer argument
  (benchmark:defbenchmark ffi-abs-int ()
    (bench-abs -42))
  
  ;; String argument
  (let ((test-string "Hello, World!"))
    (benchmark:defbenchmark ffi-strlen-short ()
      (bench-strlen test-string)))
  
  (let ((long-string (make-string 1000 :initial-element #\A)))
    (benchmark:defbenchmark ffi-strlen-long ()
      (bench-strlen long-string))))

;;; Pointer argument FFI calls

(defun register-pointer-arg-benchmarks ()
  "Register benchmarks for pointer argument FFI calls"
  
  ;; Small buffer comparison
  (let* ((size 128)
         (ptr1 (lib:foreign-alloc size))
         (ptr2 (lib:foreign-alloc size)))
    ;; Initialize with data
    (dotimes (i size)
      (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
      (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
    
    (benchmark:defbenchmark ffi-memcmp-128b ()
      (bench-memcmp ptr1 ptr2 size)))
  
  ;; Large buffer comparison
  (let* ((size 4096)
         (ptr1 (lib:foreign-alloc size))
         (ptr2 (lib:foreign-alloc size)))
    ;; Initialize with data
    (dotimes (i size)
      (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
      (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
    
    (benchmark:defbenchmark ffi-memcmp-4kb ()
      (bench-memcmp ptr1 ptr2 size)))
  
  ;; Memory copy operations
  (let* ((size 1024)
         (src (lib:foreign-alloc size))
         (dst (lib:foreign-alloc size)))
    ;; Initialize source
    (dotimes (i size)
      (setf (sb-sys:sap-ref-8 src i) (mod i 256)))
    
    (benchmark:defbenchmark ffi-memcpy-1kb ()
      (bench-memcpy dst src size))))

;;; Memory allocation benchmarks

(defun register-allocation-benchmarks ()
  "Register memory allocation benchmarks"
  
  (benchmark:defbenchmark ffi-malloc-free-64b ()
    (let ((ptr (lib:foreign-alloc 64)))
      (lib:foreign-free ptr)))
  
  (benchmark:defbenchmark ffi-malloc-free-1kb ()
    (let ((ptr (lib:foreign-alloc 1024)))
      (lib:foreign-free ptr)))
  
  (benchmark:defbenchmark ffi-malloc-free-64kb ()
    (let ((ptr (lib:foreign-alloc 65536)))
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
    
    (benchmark:defbenchmark ffi-struct-create ()
      (lib:make-foreign-struct 'benchmark-point
                              :x 10 :y 20 :z 30))
    
    (let ((point (lib:make-foreign-struct 'benchmark-point
                                         :x 10 :y 20 :z 30)))
      (benchmark:defbenchmark ffi-struct-access ()
        (+ (lib:foreign-struct-slot point 'benchmark-point 'x)
           (lib:foreign-struct-slot point 'benchmark-point 'y)
           (lib:foreign-struct-slot point 'benchmark-point 'z)))
      
      (benchmark:defbenchmark ffi-struct-modify ()
        (progn
          (setf (lib:foreign-struct-slot point 'benchmark-point 'x) 100)
          (setf (lib:foreign-struct-slot point 'benchmark-point 'y) 200)
          (setf (lib:foreign-struct-slot point 'benchmark-point 'z) 300))))))

;;; Callback benchmarks (if available)

(defun register-callback-benchmarks ()
  "Register callback benchmarks"
  
  (when (fboundp 'lib:define-foreign-callback)
    ;; Define a simple callback
    (lib:define-foreign-callback benchmark-callback :int ((x :int))
      (* x 2))
    
    ;; Benchmark callback creation
    (benchmark:defbenchmark ffi-callback-create ()
      (lib:create-callback 'benchmark-callback))
    
    ;; Benchmark callback invocation (if possible)
    (when (fboundp 'lib:invoke-callback)
      (let ((callback (lib:create-callback 'benchmark-callback)))
        (benchmark:defbenchmark ffi-callback-invoke ()
          (lib:invoke-callback callback 21))))))

;;; Comparison with SBCL Alien

(defun register-comparison-benchmarks ()
  "Register benchmarks comparing with SBCL's alien interface"
  
  ;; Define SBCL alien version for comparison
  (sb-alien:define-alien-routine "strlen" sb-alien:unsigned-long
    (str sb-alien:c-string))
  
  (let ((test-string "Hello, World!"))
    ;; Epsilon FFI version
    (benchmark:defbenchmark ffi-epsilon-strlen ()
      (bench-strlen test-string))
    
    ;; SBCL alien version
    (benchmark:defbenchmark ffi-sbcl-strlen ()
      (strlen test-string))))

;;; Main registration function

(defun register-ffi-benchmarks ()
  "Register all FFI benchmarks"
  (register-zero-arg-benchmarks)
  (register-simple-arg-benchmarks)
  (register-pointer-arg-benchmarks)
  (register-allocation-benchmarks)
  (register-struct-benchmarks)
  (register-callback-benchmarks)
  (register-comparison-benchmarks)
  
  ;; Register the enhanced FFI suite
  (suites:register-suite 'ffi-operations
                        :description "Foreign Function Interface benchmarks"
                        :benchmarks '(;; Basic calls
                                     ffi-getpid
                                     ffi-abs-int
                                     ffi-strlen-short
                                     ffi-strlen-long
                                     ;; Pointer operations
                                     ffi-memcmp-128b
                                     ffi-memcmp-4kb
                                     ffi-memcpy-1kb
                                     ;; Memory allocation
                                     ffi-malloc-free-64b
                                     ffi-malloc-free-1kb
                                     ffi-malloc-free-64kb
                                     ;; Comparison
                                     ffi-epsilon-strlen
                                     ffi-sbcl-strlen)))