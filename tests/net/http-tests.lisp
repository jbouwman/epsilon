(tool.test:define-test-package #:net.http/tests)

(in-package #:net.http/tests)

(deftest http ()
  (net.http:get "https://lisp.org/"))
