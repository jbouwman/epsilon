;;;; Shared test utilities for epsilon.compression
;;;;
;;;; Canonical implementation of make-test-data used across all compression
;;;; test files. Eliminates 3 identical copies (IMPL-244).

(defpackage epsilon.compression.test-support
  (:use :cl)
  (:export
   #:make-test-data))

(in-package :epsilon.compression.test-support)

(defun make-test-data (size &key (pattern :random))
  "Create test data of specified size with the given pattern."
  (let ((data (make-array size :element-type '(unsigned-byte 8))))
    (ecase pattern
      (:random
       (dotimes (i size)
         (setf (aref data i) (random 256))))
      (:zeros
       (fill data 0))
      (:ones
       (fill data 255))
      (:sequential
       (dotimes (i size)
         (setf (aref data i) (mod i 256))))
      (:text
       ;; Repeating text pattern (compresses well)
       (let ((text "The quick brown fox jumps over the lazy dog. "))
         (dotimes (i size)
           (setf (aref data i)
                 (char-code (char text (mod i (length text)))))))))
    data))
