(defpackage #:epsilon
  (:use #:cl)
  (:nicknames #:e)
  (:local-nicknames (#:uri #:epsilon.lib.uri)
                    (#:build #:epsilon.tool.build))
  (:shadow #:load)
  (:export #:build
           #:load))

(in-package #:epsilon)

(defun build (uri)
  (build:build (uri:uri uri)))

(defun load (uri)
  (build:build (uri:uri uri)))
