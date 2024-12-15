(defpackage #:net.http.tests)

(in-package #:net.http.tests)

(deftest http ()
  (epsilon.net.http:get "https://lisp.org/"))
