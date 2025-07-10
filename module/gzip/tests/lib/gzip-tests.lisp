;;;; Tests for GZIP Module

(defpackage :epsilon.lib.gzip.tests
  (:use :cl :epsilon.tool.test)
  (:local-nicknames
   (:gzip :epsilon.lib.gzip))
  (:export :run-gzip-tests))

(in-package :epsilon.lib.gzip.tests)

(def-test-package :gzip-basic-functionality
  "Basic gzip module functionality tests")

(def-test gzip-header-creation :gzip-basic-functionality
  "Test gzip header creation"
  (let ((header (make-instance 'gzip:gzip-header 
                               :flags 0
                               :compression-method gzip:+gzip-deflate-method+)))
    (is (typep header 'gzip:gzip-header))
    (is (= (gzip:flags header) 0))
    (is (= (gzip:compression-method header) gzip:+gzip-deflate-method+))))

(def-test gzip-constants-defined :gzip-basic-functionality
  "Test that required constants are defined"
  (is (= gzip:+gzip-flag-text+ 0))
  (is (= gzip:+gzip-flag-crc+ 1))
  (is (= gzip:+gzip-flag-extra+ 2))
  (is (= gzip:+gzip-flag-name+ 3))
  (is (= gzip:+gzip-flag-comment+ 4))
  (is (= gzip:+gzip-deflate-method+ 8))
  (is (= gzip:+gzip-xfl-max-compression+ 2))
  (is (= gzip:+gzip-xfl-fast-compression+ 4))
  (is (= gzip:+gzip-fast-compression+ 4))
  (is (= gzip:+gzip-deflate-compression+ 8))
  (is (= gzip:+gzip-flags+ 0))
  (is (= gzip:+gzip-unix-os+ 3))
  (is (= gzip:+gzip-mtime+ 0)))

(def-test gzip-signature :gzip-basic-functionality
  "Test GZIP signature"
  (is (not (null gzip:*gzip-signature*)))
  (is (= (length gzip:*gzip-signature*) 2))
  (is (= (aref gzip:*gzip-signature* 0) #x1F))
  (is (= (aref gzip:*gzip-signature* 1) #x8B)))

(def-test gzip-codec-creation :gzip-basic-functionality
  "Test GZIP codec creation"
  (let ((codec (make-instance 'gzip:gzip-codec)))
    (is (typep codec 'gzip:gzip-codec))))

(def-test gzip-compressor-creation :gzip-basic-functionality
  "Test GZIP compressor creation"
  (let ((compressor (make-instance 'gzip:gzip-compressor)))
    (is (typep compressor 'gzip:gzip-compressor))))

(def-test gzip-decompression-stub :gzip-basic-functionality
  "Test that decompression function exists"
  (is (fboundp 'gzip:gzip-decode)))

(def-test gzip-compression-stub :gzip-basic-functionality
  "Test that compression function exists but not implemented"
  (is (fboundp 'gzip:gzip-encode))
  (is-thrown-p (gzip:gzip-encode nil nil)))

(def-test gzip-stream-stubs :gzip-basic-functionality
  "Test that stream functions exist"
  (is (fboundp 'gzip:make-gzip-decompressing-stream))
  (is (fboundp 'gzip:make-gzip-compressing-stream))
  (is-thrown-p (gzip:make-gzip-compressing-stream nil)))

(def-test error-conditions :gzip-basic-functionality
  "Test that error conditions are properly defined"
  (is (find-class 'gzip:gzip-error nil))
  (is (find-class 'gzip:invalid-gzip-header-error nil)))

(def-test header-accessors :gzip-basic-functionality
  "Test header accessor functions"
  (let ((header (make-instance 'gzip:gzip-header 
                               :flags 15
                               :mtime 12345)))
    (is (= (gzip:flags header) 15))
    (is (= (gzip:mtime header) 12345))
    (setf (gzip:filename header) "test.txt")
    (is (string= (gzip:filename header) "test.txt"))))

(defun run-gzip-tests ()
  "Run all gzip module tests"
  (run-package-tests :gzip-basic-functionality))