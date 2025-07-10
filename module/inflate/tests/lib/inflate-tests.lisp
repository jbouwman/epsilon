;;;; Tests for DEFLATE/Inflate Decompression Module

(defpackage :epsilon.lib.inflate.tests
  (:use :cl :epsilon.tool.test)
  (:local-nicknames
   (:inflate :epsilon.lib.inflate))
  (:export :run-inflate-tests))

(in-package :epsilon.lib.inflate.tests)

(def-test-package :inflate-basic-functionality
  "Basic inflate module functionality tests")

(def-test huffman-decode-table-construction :inflate-basic-functionality
  "Test huffman decode table construction"
  (let* ((code-lengths #(3 3 3 3 3 2 4 4))
         (table (inflate:construct-huffman-decode-table code-lengths)))
    (is (inflate:huffman-decode-table-p table))
    (is (not (null (inflate:hdt-counts table))))
    (is (not (null (inflate:hdt-offsets table))))
    (is (not (null (inflate:hdt-symbols table))))))

(def-test inflate-state-creation :inflate-basic-functionality
  "Test inflate state creation"
  (let ((state (inflate:make-inflate-state :deflate)))
    (is (inflate:inflate-state-p state))
    (is (not (null (inflate:inflate-state-window state))))
    (is (= (length (inflate:inflate-state-window state)) 32768))))

(def-test inflate-state-formats :inflate-basic-functionality
  "Test inflate state creation with different formats"
  (dolist (format '(:deflate :gzip :zlib))
    (let ((state (inflate:make-inflate-state format)))
      (is (inflate:inflate-state-p state))
      (is (eq (inflate:inflate-state-data-format state) 
              (case format
                (:deflate 'deflate)
                (:gzip 'gzip)
                (:zlib 'zlib)))))))

(def-test invalid-format-error :inflate-basic-functionality
  "Test error handling for invalid formats"
  (is-thrown-p (inflate:make-inflate-state :invalid-format)))

(def-test fixed-tables-availability :inflate-basic-functionality
  "Test that fixed huffman tables are available"
  (is (inflate:huffman-decode-table-p inflate:*fixed-literal/length-table*))
  (is (inflate:huffman-decode-table-p inflate:*fixed-distance-table*)))

(def-test constants-defined :inflate-basic-functionality
  "Test that required constants are defined"
  (is (= inflate:+block-no-compress+ 0))
  (is (= inflate:+block-fixed-codes+ 1))
  (is (= inflate:+block-dynamic-codes+ 2))
  (is (= inflate:+block-invalid+ 3))
  (is (= inflate:+max-code-length+ 16))
  (is (= inflate:+max-codes+ 288))
  (is (= inflate:+max-n-code-lengths+ 19))
  (is (= inflate:+deflate-max-bits+ 15)))

(def-test stream-interface :inflate-basic-functionality
  "Test stream interface creation"
  (let ((input-stream (make-string-input-stream "test")))
    (let ((stream (inflate:make-inflating-stream :deflate input-stream)))
      (is (typep stream 'inflate:inflating-stream))
      (is (eq (inflate:inflating-stream-source stream) input-stream))
      (is (inflate:inflate-state-p (inflate:inflating-stream-state stream))))))

(def-test backward-compatibility :inflate-basic-functionality
  "Test backward compatibility functions"
  (let ((state (inflate:make-inflate-state :deflate)))
    (is (inflate:inflate-state-p state))
    ;; Test that the %inflate alias works
    (is (fboundp 'inflate:%inflate))))

(def-test helper-functions :inflate-basic-functionality
  "Test helper functions"
  (is (= (inflate:n-length-extra-bits 8) 1))
  (is (= (inflate:n-distance-extra-bits 4) 1))
  (is (= (inflate:length-base 0) 3))
  (is (= (inflate:distance-base 0) 1)))

(def-test error-conditions :inflate-basic-functionality
  "Test that error conditions are properly defined"
  (is (find-class 'inflate:deflate-error nil))
  (is (find-class 'inflate:invalid-deflate-block nil))
  (is (find-class 'inflate:invalid-huffman-code nil))
  (is (find-class 'inflate:reserved-block-type-error nil))
  (is (find-class 'inflate:unassigned-huffman-code-error nil)))

(defun run-inflate-tests ()
  "Run all inflate module tests"
  (run-package-tests :inflate-basic-functionality))