;;;; Tests for ZLIB Module

(defpackage :epsilon.lib.zlib.tests
  (:use :cl :epsilon.tool.test)
  (:local-nicknames
   (:zlib :epsilon.lib.zlib))
  (:export :run-zlib-tests))

(in-package :epsilon.lib.zlib.tests)

(def-test-package :zlib-basic-functionality
  "Basic zlib module functionality tests")

(def-test zlib-header-creation :zlib-basic-functionality
  "Test zlib header creation"
  (let ((header (make-instance 'zlib:zlib-header 
                               :flags 0
                               :cmf zlib:+zlib-compression-method+)))
    (is (typep header 'zlib:zlib-header))
    (is (= (zlib:flags header) 0))
    (is (= (zlib:cmf header) zlib:+zlib-compression-method+))))

(def-test zlib-constants-defined :zlib-basic-functionality
  "Test that required constants are defined"
  (is (= zlib:+zlib-compression-method+ 8))
  (is (= zlib:+zlib-flag-fdict+ 5))
  (is (= zlib:+zlib-magic+ #x789c))
  (is (= zlib:+zlib-flevel-fastest+ 0))
  (is (= zlib:+zlib-flevel-fast+ 1))
  (is (= zlib:+zlib-flevel-default+ 2))
  (is (= zlib:+zlib-flevel-maximum+ 3)))

(def-test zlib-helper-functions :zlib-basic-functionality
  "Test ZLIB helper functions"
  (let ((cmf-byte #x78)  ; compression method 8, compression info 7
        (flag-byte #x9C)) ; FCHECK = 12, FLEVEL = 2
    (is (= (zlib:zlib-compression-method cmf-byte) 8))
    (is (= (zlib:zlib-compression-info cmf-byte) 7))
    (is (= (zlib:zlib-flag-fcheck flag-byte) 12))
    (is (= (zlib:zlib-flag-flevel flag-byte) 2))))

(def-test zlib-codec-creation :zlib-basic-functionality
  "Test ZLIB codec creation"
  (let ((codec (make-instance 'zlib:zlib-codec)))
    (is (typep codec 'zlib:zlib-codec))))

(def-test zlib-compressor-creation :zlib-basic-functionality
  "Test ZLIB compressor creation"
  (let ((compressor (make-instance 'zlib:zlib-compressor)))
    (is (typep compressor 'zlib:zlib-compressor))))

(def-test zlib-decompression-stub :zlib-basic-functionality
  "Test that decompression function exists"
  (is (fboundp 'zlib:zlib-decode)))

(def-test zlib-compression-stub :zlib-basic-functionality
  "Test that compression function exists but not implemented"
  (is (fboundp 'zlib:zlib-encode))
  (is-thrown-p (zlib:zlib-encode nil nil)))

(def-test zlib-stream-stubs :zlib-basic-functionality
  "Test that stream functions exist"
  (is (fboundp 'zlib:make-zlib-decompressing-stream))
  (is (fboundp 'zlib:make-zlib-compressing-stream))
  (is-thrown-p (zlib:make-zlib-compressing-stream nil)))

(def-test error-conditions :zlib-basic-functionality
  "Test that error conditions are properly defined"
  (is (find-class 'zlib:zlib-error nil))
  (is (find-class 'zlib:invalid-zlib-header-error nil)))

(def-test header-accessors :zlib-basic-functionality
  "Test header accessor functions"
  (let ((header (make-instance 'zlib:zlib-header 
                               :flags 156  ; Standard zlib flag byte
                               :cmf 120))) ; Standard zlib CMF byte
    (is (= (zlib:flags header) 156))
    (is (= (zlib:cmf header) 120))
    (setf (zlib:fdict header) 12345)
    (is (= (zlib:fdict header) 12345))))

(def-test header-parsing :zlib-basic-functionality
  "Test header byte parsing"
  ;; Standard zlib header: 0x789C
  (let ((cmf #x78)    ; compression method 8, window size 32K
        (flg #x9C))   ; no dictionary, compression level 2
    (is (= (zlib:zlib-compression-method cmf) 8))
    (is (= (zlib:zlib-compression-info cmf) 7))  ; window size = 2^(7+8) = 32K
    (is (= (zlib:zlib-flag-fcheck flg) 12))
    (is (= (zlib:zlib-flag-flevel flg) 2))))

(defun run-zlib-tests ()
  "Run all zlib module tests"
  (run-package-tests :zlib-basic-functionality))