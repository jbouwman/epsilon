(in-package :epsilon.tool.release)

;; Test minimal release functionality
(format t "Testing discover-modules...~%")
(let ((modules (discover-modules)))
  (format t "Modules: ~A~%" modules)
  (format t "Type: ~A~%" (type-of modules))
  (format t "First module: ~A~%" (first modules)))

(format t "~%Testing build-release with minimal params...~%")
(handler-case
    (build-release :version "test" :modules '(epsilon.core) :output-dir "/tmp/epsilon-test")
  (error (e)
    (format t "Error in build-release: ~A~%" e)
    (format t "Error type: ~A~%" (type-of e))))