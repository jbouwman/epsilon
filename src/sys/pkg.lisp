(defpackage #:epsilon.sys.pkg
  (:use #:cl)
  (:local-nicknames
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string))
  (:export
   #:parse
   #:normalize))

(in-package #:epsilon.sys.pkg)

(defun parse (package-name)
  "Split package name into hierarchical components."
  (seq:map #'string-downcase
           (str:split #\.
                      (string package-name))))

(defun normalize (sym)
  (when sym
    (str:join #\. (parse (symbol-name sym)))))
