(cl:defpackage #:epsilon.sys.pkg
  (:use #:cl)
  (:local-nicknames
   (#:seq #:epsilon.sequence)
   (#:str #:epsilon.string))
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
  "Lowercase the dotted-component form of SYM's symbol-name, returning
   it as a string.  Inverse-of-sorts of `parse'.  NIL passes through."
  (when sym
    (str:join #\. (parse (symbol-name sym)))))
