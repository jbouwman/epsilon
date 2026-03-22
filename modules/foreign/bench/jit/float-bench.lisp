;;;; float-bench.lisp - Benchmark floating point JIT FFI

(in-package :epsilon.foreign.jit)

(sb-alien:define-alien-routine ("sin" libc-sin) sb-alien:double
  (x sb-alien:double))

(defun get-addr (name)
  (sb-sys:find-foreign-symbol-address name))

(defmacro time-it (n &body body)
  (let ((start (gensym)) (end (gensym)) (i (gensym)))
    `(let ((,start (get-internal-real-time)))
       (dotimes (,i ,n) ,@body)
       (let ((,end (get-internal-real-time)))
         (float (/ (- ,end ,start) internal-time-units-per-second))))))

(defun run-float-bench ()
  (let ((iterations 1000000))
    (format t "~%JIT FFI Float Benchmark~%")
    (format t "=======================~%")
    (format t "Platform: ~A~%" *current-platform*)
    (format t "Iterations: ~:D~%~%" iterations)
    (force-output)

    ;; Warmup
    (dotimes (i 10000) (libc-sin 1.0d0))

    (format t "sin(1.0) benchmark:~%")
    (force-output)

    (let ((direct-time (time-it iterations (libc-sin 1.0d0))))
      (format t "  1. Static alien-funcall:  ~,4Fs (~,1F M/s)~%"
              direct-time (/ 1.0 direct-time))
      (force-output)

      (clear-stub-cache)
      (let ((caller (make-jit-caller (get-addr "sin") :double '(:double))))
        (dotimes (i 10000) (funcall caller 1.0d0))
        (let ((jit-time (time-it iterations (funcall caller 1.0d0))))
          (format t "  2. JIT make-jit-caller:   ~,4Fs (~,1F M/s)~%"
                  jit-time (/ 1.0 jit-time))
          (format t "  Ratio (JIT/Static):       ~,2Fx~%" (/ jit-time direct-time))
          (force-output))))))

(run-float-bench)
