(defpackage #:epsilon.lib.digest.reader
  (:use #:cl)
  (:export
   #:in-ironclad-readtable))

(in-package #:epsilon.lib.digest.reader)

;;; easy-to-type readmacro for creating s-boxes and the like

(defun array-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((array-data (read stream nil stream nil))
        (array-element-type `(unsigned-byte ,arg)))
    ;; FIXME: need to make this work for multi-dimensional arrays
    `(make-array ,(length array-data) :element-type ',array-element-type
                :initial-contents ',array-data)))

(defparameter *ironclad-readtable*
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\@ #'array-reader readtable)
    readtable))

(defmacro in-ironclad-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* *ironclad-readtable*)))
