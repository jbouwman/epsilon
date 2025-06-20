(defpackage :epsilon.tool.build.tests
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames
   (#:build #:epsilon.tool.build)
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:uri #:epsilon.lib.uri)
   (#:test #:epsilon.tool.test)))

(in-package :epsilon.tool.build.tests)

(defun load-project ()
  (build::load-project (uri:uri "file:///Users/jbouwman/git/epsilon")))

(deftest build-order
  (is (seq:each (lambda (x) (format t "~S~%" x)) (build::build-order (load-project))))


