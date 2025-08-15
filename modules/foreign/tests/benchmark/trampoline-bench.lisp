(defpackage epsilon.foreign.trampoline-bench
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.foreign.trampoline-bench)

;;;; Benchmark comparison between eval-based and trampoline-based FFI

;; Define functions using both approaches
(lib:defshared old-getpid "getpid" "libc" :int
	       :documentation "getpid using old eval-based approach")

(lib:defshared-fast new-getpid "getpid" "libc" :int
		    :documentation "getpid using new trampoline approach")

(lib:defshared old-strlen "strlen" "libc" :unsigned-long
	       (str :string)
	       :documentation "strlen using old eval-based approach")

(lib:defshared-fast new-strlen "strlen" "libc" :unsigned-long
		    (str :string)
		    :documentation "strlen using new trampoline approach")

(defun benchmark-comparison (name iterations old-fn new-fn &rest args)
  "Compare performance of old vs new FFI approaches"
  (format t "~%Benchmark: ~A (~:D iterations)~%" name iterations)
  
  ;; Warm up both
  (dotimes (i 1000)
    (apply old-fn args)
    (apply new-fn args))
  
  ;; Benchmark old approach
  (format t "  Old (eval-based):~%")
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (apply old-fn args))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (old-per-call (* (/ elapsed iterations) 1000000))) ; microseconds
      (format t "    Time: ~,3F seconds (~,3F μs/call)~%" elapsed old-per-call)
      
      ;; Benchmark new approach
      (format t "  New (trampoline):~%")
      (let ((start2 (get-internal-real-time)))
        (dotimes (i iterations)
          (apply new-fn args))
        (let* ((end2 (get-internal-real-time))
               (elapsed2 (/ (- end2 start2) internal-time-units-per-second))
               (new-per-call (* (/ elapsed2 iterations) 1000000)))
          (format t "    Time: ~,3F seconds (~,3F μs/call)~%" elapsed2 new-per-call)
          
          ;; Calculate speedup
          (let ((speedup (/ old-per-call new-per-call)))
            (format t "  Speedup: ~,2Fx (~A% ~A)~%"
                    speedup
                    (round (* (abs (1- speedup)) 100))
                    (if (> speedup 1) "faster" "slower"))))))))

(defun run-comparison-benchmarks ()
  "Run all comparison benchmarks"
  (format t "~%========================================~%")
  (format t "   FFI Trampoline Performance Comparison~%")
  (format t "========================================~%")
  
  ;; Zero-argument function
  (benchmark-comparison "getpid() [zero args]" 1000000
			#'old-getpid #'new-getpid)
  
  ;; String argument function
  (let ((test-string "Hello, World!"))
    (benchmark-comparison "strlen() [string arg]" 100000
                          #'old-strlen #'new-strlen test-string))
  
  ;; Test with longer string
  (let ((long-string (make-string 1000 :initial-element #\A)))
    (benchmark-comparison "strlen() [1000 char string]" 100000
                          #'old-strlen #'new-strlen long-string))
  
  (format t "~%Benchmark complete.~%"))

;; Also test the raw trampoline performance
(defun benchmark-raw-trampoline ()
  "Benchmark raw trampoline without macro overhead"
  (format t "~%Raw Trampoline Performance:~%")
  
  (let* ((lib-handle (lib:lib-open "libc"))
         (getpid-addr (lib:lib-function lib-handle "getpid"))
         (strlen-addr (lib:lib-function lib-handle "strlen"))
         (getpid-trampoline (lib:get-or-create-trampoline :int '()))
         (strlen-trampoline (lib:get-or-create-trampoline :unsigned-long '(:string)))
         (test-string "Test string")
         (iterations 1000000))
    
    ;; Warm up
    (dotimes (i 1000)
      (funcall getpid-trampoline getpid-addr)
      (funcall strlen-trampoline strlen-addr test-string))
    
    ;; Benchmark getpid
    (format t "  Raw getpid trampoline:~%")
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (funcall getpid-trampoline getpid-addr))
      (let* ((end (get-internal-real-time))
             (elapsed (/ (- end start) internal-time-units-per-second))
             (per-call (* (/ elapsed iterations) 1000000000))) ; nanoseconds
        (format t "    ~,3F seconds (~,1F ns/call)~%" elapsed per-call)))
    
    ;; Benchmark strlen
    (format t "  Raw strlen trampoline:~%")
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (funcall strlen-trampoline strlen-addr test-string))
      (let* ((end (get-internal-real-time))
             (elapsed (/ (- end start) internal-time-units-per-second))
             (per-call (* (/ elapsed iterations) 1000000000))) ; nanoseconds
        (format t "    ~,3F seconds (~,1F ns/call)~%" elapsed per-call)))))

(defun benchmark ()
  "Entry point for benchmarks"
  (run-comparison-benchmarks)
  (benchmark-raw-trampoline))
