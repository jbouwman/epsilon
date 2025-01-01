(defpackage #:epsilon.lib.uri.tests
  (:use
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.uri.tests)

(deftest file-uri ()
  (is (string-equal (uri:render-uri (uri:extend (uri:uri "file:///home") "two"))
                    "file:///home/two")))
