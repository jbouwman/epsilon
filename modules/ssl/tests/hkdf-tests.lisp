;;;; Tests for HKDF (RFC 5869)
;;;;
;;;; Test vectors from RFC 5869 Appendix A

(defpackage epsilon.ssl.hkdf-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:hkdf #:epsilon.ssl.hkdf)
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.hkdf-tests)

;;; ---------------------------------------------------------------------------
;;; RFC 5869 Appendix A - Test Case 1 (SHA-256)
;;; ---------------------------------------------------------------------------

(deftest test-hkdf-rfc5869-case1-extract
  "RFC 5869 Test Case 1: HKDF-Extract with SHA-256"
  (let* ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
         (salt (hex-to-bytes "000102030405060708090a0b0c"))
         (expected-prk (hex-to-bytes "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"))
         (prk (hkdf:hkdf-extract :sha256 salt ikm)))
    (assert-true (ct:ct-equal prk expected-prk))))

(deftest test-hkdf-rfc5869-case1-expand
  "RFC 5869 Test Case 1: HKDF-Expand with SHA-256"
  (let* ((prk (hex-to-bytes "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"))
         (info (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9"))
         (expected-okm (hex-to-bytes "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"))
         (okm (hkdf:hkdf-expand :sha256 prk info 42)))
    (assert-true (ct:ct-equal okm expected-okm))))

(deftest test-hkdf-rfc5869-case1-combined
  "RFC 5869 Test Case 1: Combined HKDF"
  (let* ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
         (salt (hex-to-bytes "000102030405060708090a0b0c"))
         (info (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9"))
         (expected-okm (hex-to-bytes "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"))
         (okm (hkdf:hkdf :sha256 salt ikm info 42)))
    (assert-true (ct:ct-equal okm expected-okm))))

;;; ---------------------------------------------------------------------------
;;; RFC 5869 Appendix A - Test Case 2 (SHA-256, longer inputs)
;;; ---------------------------------------------------------------------------

(deftest test-hkdf-rfc5869-case2
  "RFC 5869 Test Case 2: SHA-256 with longer inputs"
  (let* ((ikm (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f"))
         (salt (hex-to-bytes "606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf"))
         (info (hex-to-bytes "b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         (expected-okm (hex-to-bytes "b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87"))
         (okm (hkdf:hkdf :sha256 salt ikm info 82)))
    (assert-true (ct:ct-equal okm expected-okm))))

;;; ---------------------------------------------------------------------------
;;; RFC 5869 Appendix A - Test Case 3 (SHA-256, zero-length salt/info)
;;; ---------------------------------------------------------------------------

(deftest test-hkdf-rfc5869-case3
  "RFC 5869 Test Case 3: SHA-256 with zero-length salt and info"
  (let* ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
         (salt nil)
         (info (make-array 0 :element-type '(unsigned-byte 8)))
         (expected-okm (hex-to-bytes "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8"))
         (okm (hkdf:hkdf :sha256 salt ikm info 42)))
    (assert-true (ct:ct-equal okm expected-okm))))

;;; ---------------------------------------------------------------------------
;;; Edge cases
;;; ---------------------------------------------------------------------------

(deftest test-hkdf-extract-empty-salt
  "HKDF-Extract with nil salt uses zero-filled salt"
  (let* ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b"))
         (explicit-zero-salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (prk-nil (hkdf:hkdf-extract :sha256 nil ikm))
         (prk-zeros (hkdf:hkdf-extract :sha256 explicit-zero-salt ikm)))
    (assert-true (ct:ct-equal prk-nil prk-zeros))))

(deftest test-hkdf-expand-single-block
  "HKDF-Expand requesting exactly hash output length"
  (let* ((prk (hex-to-bytes "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"))
         (info (make-array 0 :element-type '(unsigned-byte 8)))
         (okm (hkdf:hkdf-expand :sha256 prk info 32)))
    (assert-= (length okm) 32)))

(deftest test-hkdf-expand-small
  "HKDF-Expand requesting small output"
  (let* ((prk (hex-to-bytes "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"))
         (info (make-array 0 :element-type '(unsigned-byte 8)))
         (okm (hkdf:hkdf-expand :sha256 prk info 1)))
    (assert-= (length okm) 1)))
