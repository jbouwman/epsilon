(epsilon.tool.test:define-test-package #:net.http/tests)

(in-package #:net.http/tests)

(deftest http ()
  (epsilon.net.http:get "https://lisp.org/"))
