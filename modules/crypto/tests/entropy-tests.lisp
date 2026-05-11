;;;; Tests for platform entropy

(defpackage epsilon.crypto.entropy-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.entropy entropy)
   (epsilon.crypto.ct ct)))

(in-package :epsilon.crypto.entropy-tests)

(deftest test-entropy-available
  "Platform entropy should be available on Linux and macOS"
  (assert-true (entropy:entropy-available-p)))

(deftest test-random-bytes-length
  "random-bytes should return the requested number of bytes"
  (assert-= (length (entropy:random-bytes 0)) 0)
  (assert-= (length (entropy:random-bytes 1)) 1)
  (assert-= (length (entropy:random-bytes 32)) 32)
  (assert-= (length (entropy:random-bytes 256)) 256)
  (assert-= (length (entropy:random-bytes 1024)) 1024))

(deftest test-random-bytes-not-all-zeros
  "Random bytes should not be all zeros (with overwhelming probability)"
  (let ((bytes (entropy:random-bytes 32)))
    (assert-not (ct:ct-zerop bytes))))

(deftest test-random-bytes-unique
  "Two calls should produce different output (with overwhelming probability)"
  (let ((a (entropy:random-bytes 32))
        (b (entropy:random-bytes 32)))
    (assert-not (ct:ct-equal a b))))

(deftest test-fill-random-bytes
  "fill-random-bytes should fill an existing buffer"
  (let ((buf (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (entropy:fill-random-bytes buf)
    (assert-not (ct:ct-zerop buf))))

(deftest test-fill-random-bytes-returns-buffer
  "fill-random-bytes should return the same buffer"
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-eq (entropy:fill-random-bytes buf) buf)))
