(handler-case
    (progn
      (require :asdf)
      (load "epsilon.asd")
      (asdf:load-system "epsilon/tests"))
  (error (condition)
    (format *error-output "~A~%" condition)
    (sb-debug:print-backtrace :stream *error-output*)))

(sb-posix:exit (if (epsilon.tool.test:run-success-p (epsilon.tool.test:run-tests))
                   0
                   1))
     
