(defpackage #:epsilon.sys.pkg
  (:use #:cl)
  (:local-nicknames
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string))
  (:export #:parse
           #:name))

(in-package #:epsilon.sys.pkg)

(defun parse (package-name)
  "Split package name into hierarchical components."
  (seq:realize (str:split #\. (string package-name))))
