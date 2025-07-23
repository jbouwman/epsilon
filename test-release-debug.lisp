(require :asdf)
(load "bootstrap.lisp")

(in-package :cl-user)

;; Test individual components
(format t "Testing map:keys build:*modules*...~%")
(let ((keys (epsilon.lib.map:keys epsilon.tool.build:*modules*)))
  (format t "Keys: ~A~%" keys)
  (format t "Type: ~A~%" (type-of keys))
  (format t "Is list?: ~A~%" (listp keys)))

(format t "~%Testing build:get-module...~%")
(handler-case
    (let ((module-info (epsilon.tool.build:get-module 'epsilon.core)))
      (format t "Module info type: ~A~%" (type-of module-info))
      (format t "Module info: ~A~%" module-info))
  (error (e)
    (format t "Error in get-module: ~A~%" e)))

(format t "~%Testing path operations...~%")
(handler-case
    (let ((test-path (epsilon.lib.path:path-join "/tmp" "test")))
      (format t "Path join result: ~A~%" test-path)
      (format t "Path type: ~A~%" (type-of test-path)))
  (error (e)
    (format t "Error in path operations: ~A~%" e)))