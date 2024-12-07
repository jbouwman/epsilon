;;;; package.lisp

(defpackage :epsilon.io
  (:use :cl)
  (:local-nicknames
   (:uri :epsilon.lib.uri))
  (:export #:read-string
           #:read-bytes
           #:open-stream
           #:open-text-stream))
