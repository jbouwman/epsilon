;;;; defjit-test.lisp - Test the defjit macro

(in-package :epsilon.foreign.jit.integration)

;; Test defjit macro
(defjit jit-sin "sin" :double ((x :double))
  :documentation "JIT-compiled sine function")

(defjit jit-hypot "hypot" :double ((x :double) (y :double))
  :documentation "JIT-compiled hypotenuse")

(defjit jit-abs "abs" :int ((n :int))
  :documentation "JIT-compiled absolute value")

(defun run-defjit-tests ()
  ;; Test the defined functions
  (format t "~%Testing defjit functions:~%")
  (format t "jit-sin(pi/2) = ~,6F (expected: 1.0)~%" (jit-sin (/ pi 2)))
  (format t "jit-hypot(3, 4) = ~,6F (expected: 5.0)~%" (jit-hypot 3.0d0 4.0d0))
  (format t "jit-abs(-42) = ~D (expected: 42)~%" (jit-abs -42))
  (force-output)

  ;; Benchmark
  (format t "~%Performance (1M iterations):~%")
  (let ((iterations 1000000))
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations) (jit-sin 1.0d0))
      (let* ((end (get-internal-real-time))
             (elapsed (float (/ (- end start) internal-time-units-per-second))))
        (format t "jit-sin: ~,4Fs (~,1F M/s)~%" elapsed (/ 1.0 elapsed))))
    (force-output)))

(run-defjit-tests)
