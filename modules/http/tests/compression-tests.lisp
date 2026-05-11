;;;; HTTP Content Compression Tests
;;;;
;;;; Tests for transparent content compression/decompression support.

(defpackage :epsilon.http.compression.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.response response)
   (epsilon.compression compress)
   (epsilon.map map)))

(defun gzip-compress (data)
  "Compress DATA with gzip format using streaming deflater."
  (compress:with-deflater (ctx :window-bits compress:+gzip-wbits+)
    (let ((output (compress:deflate-update ctx data :flush compress:+finish+)))
      output)))

(deftest test-decompress-gzip-body ()
  "Test decompression of a gzip-encoded response body"
  (let* ((original "Hello, compressed world!")
         (original-bytes (sb-ext:string-to-octets original :external-format :utf-8))
         ;; Compress with gzip format
         (compressed (gzip-compress original-bytes)))
    ;; Verify decompression via client utility
    (let ((decompressed (client::decompress-body compressed "gzip")))
      (assert-equal original
                (sb-ext:octets-to-string decompressed :external-format :utf-8)))))

(deftest test-decompress-deflate-body ()
  "Test decompression of a deflate-encoded response body"
  (let* ((original "Hello, deflated world!")
         (original-bytes (sb-ext:string-to-octets original :external-format :utf-8))
         (compressed (compress:compress original-bytes)))
    (let ((decompressed (client::decompress-body compressed "deflate")))
      (assert-equal original
                (sb-ext:octets-to-string decompressed :external-format :utf-8)))))

(deftest test-decompress-identity ()
  "Test that identity encoding returns body unchanged"
  (let ((body (sb-ext:string-to-octets "unchanged" :external-format :utf-8)))
    (assert-true (equalp body (client::decompress-body body "identity")))))

(deftest test-decompress-nil-encoding ()
  "Test that nil encoding returns body unchanged"
  (let ((body (sb-ext:string-to-octets "unchanged" :external-format :utf-8)))
    (assert-true (equalp body (client::decompress-body body nil)))))

(deftest test-decompress-nil-body ()
  "Test that nil body returns nil"
  (assert-true (null (client::decompress-body nil "gzip"))))

(deftest test-decompress-unknown-encoding ()
  "Test that unknown encoding returns body unchanged"
  (let ((body (sb-ext:string-to-octets "unchanged" :external-format :utf-8)))
    (assert-true (equalp body (client::decompress-body body "fancy-encoding")))))

(deftest test-gzip-roundtrip ()
  "Test full compress/decompress roundtrip with varying data sizes"
  (dolist (size '(1 100 1000 10000))
    (let* ((original (make-string size :initial-element #\A))
           (original-bytes (sb-ext:string-to-octets original :external-format :utf-8))
           (compressed (gzip-compress original-bytes))
           (decompressed (client::decompress-body compressed "gzip")))
      (assert-true (equalp original-bytes decompressed)))))

(deftest test-brotli-decompression ()
  "Test Brotli decompression if available"
  (when (compress:brotli-available-p)
    (let* ((original "Hello, Brotli world!")
           (original-bytes (sb-ext:string-to-octets original :external-format :utf-8))
           (compressed (compress:brotli-compress original-bytes))
           (decompressed (client::decompress-body compressed "br")))
      (assert-equal original
                (sb-ext:octets-to-string decompressed :external-format :utf-8)))))

(deftest test-zstd-decompression ()
  "Test Zstd decompression if available"
  (when (compress:zstd-available-p)
    (let* ((original "Hello, Zstd world!")
           (original-bytes (sb-ext:string-to-octets original :external-format :utf-8))
           (compressed (compress:zstd-compress original-bytes))
           (decompressed (client::decompress-body compressed "zstd")))
      (assert-equal original
                (sb-ext:octets-to-string decompressed :external-format :utf-8)))))
