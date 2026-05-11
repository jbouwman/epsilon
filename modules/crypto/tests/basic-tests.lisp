;;;; Basic tests for epsilon.crypto module loading

(defpackage epsilon.crypto.basic-tests
  (:use :cl :epsilon.test))

(in-package :epsilon.crypto.basic-tests)

(deftest test-module-loads
  "Test that all sub-packages are available"
  (assert-true (find-package 'epsilon.crypto))
  (assert-true (find-package 'epsilon.crypto.ct))
  (assert-true (find-package 'epsilon.crypto.entropy))
  (assert-true (find-package 'epsilon.crypto.sha256))
  (assert-true (find-package 'epsilon.crypto.hmac))
  (assert-true (find-package 'epsilon.crypto.drbg)))

(deftest test-exports-exist
  "Test that key exported symbols are accessible"
  ;; Constant-time
  (assert-true (fboundp 'epsilon.crypto:ct-equal))
  (assert-true (fboundp 'epsilon.crypto:ct-select))
  ;; SHA-256
  (assert-true (fboundp 'epsilon.crypto:sha256))
  (assert-true (fboundp 'epsilon.crypto:sha256-hex))
  ;; HMAC
  (assert-true (fboundp 'epsilon.crypto:hmac-sha256))
  ;; DRBG
  (assert-true (fboundp 'epsilon.crypto:random-bytes))
  (assert-true (fboundp 'epsilon.crypto:random-integer)))
