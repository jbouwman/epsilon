(defpackage epsilon.foreign.benchmark
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.foreign.benchmark)

;;;; FFI Performance Benchmarks

;; Test functions
(lib:defshared bench-strlen "strlen" "libc" :unsigned-long
  ((str :string))
  :documentation "strlen for benchmarking")

(lib:defshared bench-getpid "getpid" "libc" :int ()
  :documentation "getpid for benchmarking")

(lib:defshared bench-memcmp "memcmp" "libc" :int
  ((s1 :pointer) (s2 :pointer) (n :unsigned-long))
  :documentation "memcmp for benchmarking")

(defun run-benchmark (name iterations fn)
  "Run a benchmark and report timing"
  (format t "~%Benchmark: ~A (~:D iterations)~%" name iterations)
  (force-output)
  
  ;; Warm up
  (dotimes (i 1000)
    (funcall fn))
  
  ;; Actual benchmark
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall fn))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (per-call (* (/ elapsed iterations) 1000000))) ; microseconds
      (format t "  Total time: ~,3F seconds~%" elapsed)
      (format t "  Per call: ~,3F microseconds~%" per-call)
      (format t "  Calls/sec: ~:D~%" (round (/ iterations elapsed)))
      per-call)))

(defun benchmark-zero-arg-calls ()
  "Benchmark zero-argument FFI calls"
  (run-benchmark "getpid() [zero args]" 1000000
                 (lambda () (bench-getpid))))

(defun benchmark-string-calls ()
  "Benchmark string argument FFI calls"
  (let ((test-string "Hello, World!"))
    (run-benchmark "strlen() [string arg]" 100000
                   (lambda () (bench-strlen test-string)))))

(defun benchmark-pointer-calls ()
  "Benchmark pointer argument FFI calls"
  (let* ((size 128)
         (ptr1 (lib:foreign-alloc size))
         (ptr2 (lib:foreign-alloc size)))
    (unwind-protect
         (progn
           ;; Initialize with some data
           (dotimes (i size)
             (setf (sb-sys:sap-ref-8 ptr1 i) (mod i 256))
             (setf (sb-sys:sap-ref-8 ptr2 i) (mod i 256)))
           
           (run-benchmark "memcmp() [pointer args]" 100000
                          (lambda () (bench-memcmp ptr1 ptr2 size))))
      (lib:foreign-free ptr1)
      (lib:foreign-free ptr2))))

(defun benchmark-allocation ()
  "Benchmark memory allocation/deallocation"
  (run-benchmark "malloc/free [64 bytes]" 100000
                 (lambda ()
                   (let ((ptr (lib:foreign-alloc 64)))
                     (lib:foreign-free ptr)))))

(defun compare-with-sbcl-alien ()
  "Compare performance with SBCL's alien interface"
  (format t "~%~%=== Comparison with SBCL Alien Interface ===~%")
  
  ;; SBCL alien version
  (sb-alien:define-alien-routine "strlen" sb-alien:unsigned-long
    (str sb-alien:c-string))
  
  (let ((test-string "Hello, World!"))
    ;; Epsilon FFI
    (format t "~%Epsilon FFI:~%")
    (let ((epsilon-time
           (run-benchmark "strlen() via epsilon.foreign" 100000
                          (lambda () (bench-strlen test-string)))))
      
      ;; SBCL alien
      (format t "~%SBCL Alien:~%")
      (let ((sbcl-time
             (run-benchmark "strlen() via sb-alien" 100000
                            (lambda () (strlen test-string)))))
        
        (format t "~%~%Performance ratio: ~,2Fx (~A is faster)~%"
                (if (< epsilon-time sbcl-time)
                    (/ sbcl-time epsilon-time)
                    (/ epsilon-time sbcl-time))
                (if (< epsilon-time sbcl-time)
                    "epsilon.foreign"
                    "sb-alien"))))))

(defun run-all-benchmarks ()
  "Run all FFI benchmarks"
  (format t "~%========================================~%")
  (format t "     Epsilon FFI Performance Benchmarks~%")
  (format t "========================================~%")
  
  (benchmark-zero-arg-calls)
  (benchmark-string-calls)
  (benchmark-pointer-calls)
  (benchmark-allocation)
  (compare-with-sbcl-alien)
  
  (format t "~%Benchmark complete.~%"))

;; Benchmark entry point
(defun benchmark ()
  "Run FFI performance benchmarks"
  (run-all-benchmarks))