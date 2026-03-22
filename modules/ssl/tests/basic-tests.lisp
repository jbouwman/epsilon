;;;; Basic tests for epsilon.ssl module loading

(defpackage epsilon.ssl.basic-tests
  (:use :cl :epsilon.test)
  (:enter t))

(in-package :epsilon.ssl.basic-tests)

(deftest test-module-loads
  "Test that all sub-packages are available"
  (assert-true (find-package 'epsilon.ssl))
  (assert-true (find-package 'epsilon.ssl.ct))
  (assert-true (find-package 'epsilon.ssl.entropy))
  (assert-true (find-package 'epsilon.ssl.sha256))
  (assert-true (find-package 'epsilon.ssl.hmac))
  (assert-true (find-package 'epsilon.ssl.drbg)))

(deftest test-exports-exist
  "Test that key exported symbols are accessible"
  ;; Constant-time
  (assert-true (fboundp 'epsilon.ssl:ct-equal))
  (assert-true (fboundp 'epsilon.ssl:ct-select))
  ;; SHA-256
  (assert-true (fboundp 'epsilon.ssl:sha256))
  (assert-true (fboundp 'epsilon.ssl:sha256-hex))
  ;; HMAC
  (assert-true (fboundp 'epsilon.ssl:hmac-sha256))
  ;; DRBG
  (assert-true (fboundp 'epsilon.ssl:random-bytes))
  (assert-true (fboundp 'epsilon.ssl:random-integer)))
