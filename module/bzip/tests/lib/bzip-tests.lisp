;;;; BZip2 Tests
;;;;
;;;; Test suite for BZip2 decompression functionality

(defpackage :epsilon.lib.bzip-tests
  (:use :cl :epsilon.tool.test)
  (:local-nicknames
   (:bzip :epsilon.lib.bzip)))

(in-package :epsilon.lib.bzip-tests)

(deftest make-state
  "Test creation of bzip2 state"
  (let ((state (bzip:make-bzip2-state)))
    (is (bzip:bzip2-state-p state))
    (is-equal 0 (bzip:bzip2-state-input-index state))
    (is-equal 0 (bzip:bzip2-state-output-index state))))

(deftest codec-creation
  "Test bzip2 codec instantiation"
  (let ((codec (make-instance 'bzip:bzip2-codec)))
    (is codec)))

(deftest stream-creation
  "Test decompressing stream creation"
  (with-open-file (stream "/dev/null" :element-type '(unsigned-byte 8))
    (let ((dstream (bzip:make-decompressing-stream :bzip2 stream)))
      (is dstream))))

;;; Placeholder for actual decompression tests
;;; These would require test data files

(deftest decompress-invalid-data
  "Test error handling for invalid data"
  (let ((state (bzip:make-bzip2-state))
        (invalid-data (make-array 10 :element-type '(unsigned-byte 8)
                                    :initial-element 0))
        (output (make-array 100 :element-type '(unsigned-byte 8))))
    (is-thrown (error)
      (bzip:decompress output state invalid-data))))

(deftest conditions
  "Test that conditions are properly defined"
  (is (find-class 'bzip:invalid-bzip2-data))
  (is (find-class 'bzip:bzip2-randomized-blocks-unimplemented)))