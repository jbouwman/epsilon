;;;; Zstd Compression Tests
;;;;
;;;; Tests for one-shot and streaming zstd compression/decompression.

(defpackage epsilon.compression.zstd-tests
  (:use :cl :epsilon.syntax :epsilon.compression.test-support)
  (:import (epsilon.test test)
            (epsilon.compression compress)))

;;; Availability Test

(test:deftest zstd-availability ()
  "Test that zstd is available"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (test:assert-true (compress:zstd-available-p) "Zstd should be available"))

;;; One-shot Compression Tests

(test:deftest zstd-compress-decompress-roundtrip ()
  "Test basic zstd compress/decompress roundtrip"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 1000 :pattern :sequential))
         (compressed (compress:zstd-compress original))
         (decompressed (compress:zstd-decompress compressed)))
    (test:assert-equalp decompressed original
                    "Decompressed data should match original")))

(test:deftest zstd-compress-empty-data ()
  "Test zstd compressing empty data"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-array 0 :element-type '(unsigned-byte 8)))
         (compressed (compress:zstd-compress original))
         (decompressed (compress:zstd-decompress compressed)))
    (test:assert-= (length decompressed) 0
               "Empty input should produce empty output")))

(test:deftest zstd-compress-levels ()
  "Test different zstd compression levels"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let ((original (make-test-data 10000 :pattern :text)))
    ;; Test min, default, and max levels
    (dolist (level (list compress:+zstd-min-level+
                         compress:+zstd-default-level+
                         compress:+zstd-max-level+))
      (let* ((compressed (compress:zstd-compress original :level level))
             (decompressed (compress:zstd-decompress compressed)))
        (test:assert-equalp decompressed original
                        (format nil "Level ~D roundtrip failed" level))))))

(test:deftest zstd-compress-ratio-improves-with-level ()
  "Higher compression levels should produce smaller output"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 50000 :pattern :text))
         (fast (compress:zstd-compress original :level compress:+zstd-min-level+))
         (best (compress:zstd-compress original :level compress:+zstd-max-level+)))
    (test:assert-true (<= (length best) (length fast))
                  "Best compression should be <= fast compression")))

(test:deftest zstd-compress-large-data ()
  "Test zstd compression of larger data"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 100000 :pattern :sequential))
         (compressed (compress:zstd-compress original))
         (decompressed (compress:zstd-decompress compressed)))
    (test:assert-equalp decompressed original
                    "Large data roundtrip failed")))

(test:deftest zstd-decompress-invalid-data ()
  "Test that invalid data signals an error"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let ((garbage (make-test-data 100 :pattern :random)))
    (test:assert-condition (error)
      (compress:zstd-decompress garbage))))

;;; Streaming Compression Tests

(test:deftest zstd-streaming-compress-single-chunk ()
  "Test zstd streaming compression with single chunk"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let ((original (make-test-data 5000 :pattern :text)))
    (compress:with-zstd-compressor (ctx :level compress:+zstd-default-level+)
      (let* ((output1 (compress:zstd-compress-update ctx original))
             (output2 (compress:zstd-compress-finish ctx))
             (compressed (concatenate '(vector (unsigned-byte 8)) output1 output2))
             (decompressed (compress:zstd-decompress compressed)))
        (test:assert-equalp decompressed original
                        "Streaming single chunk roundtrip failed")))))

(test:deftest zstd-streaming-compress-multiple-chunks ()
  "Test zstd streaming compression with multiple chunks"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 10000 :pattern :text))
         (chunk-size 1000))
    (compress:with-zstd-compressor (ctx :level compress:+zstd-default-level+)
      (let ((compressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length original) by chunk-size
              for end = (min (+ start chunk-size) (length original))
              for chunk = (subseq original start end)
              for output = (compress:zstd-compress-update ctx chunk)
              when (> (length output) 0)
                do (push output compressed-chunks))
        ;; Finish
        (let ((final (compress:zstd-compress-finish ctx)))
          (when (> (length final) 0)
            (push final compressed-chunks)))
        ;; Decompress and verify
        (let* ((compressed (apply #'concatenate '(vector (unsigned-byte 8))
                                  (nreverse compressed-chunks)))
               (decompressed (compress:zstd-decompress compressed)))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk roundtrip failed"))))))

(test:deftest zstd-streaming-decompress-single-chunk ()
  "Test zstd streaming decompression with single chunk"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 5000 :pattern :text))
         (compressed (compress:zstd-compress original)))
    (compress:with-zstd-decompressor (ctx)
      (let ((decompressed (compress:zstd-decompress-update ctx compressed)))
        (test:assert-equalp decompressed original
                        "Streaming decompress single chunk failed")
        (test:assert-true (compress:zstd-decompress-finished-p ctx)
                      "Should be finished after complete input")))))

(test:deftest zstd-streaming-decompress-multiple-chunks ()
  "Test zstd streaming decompression with multiple chunks"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let* ((original (make-test-data 10000 :pattern :text))
         (compressed (compress:zstd-compress original))
         (chunk-size 500))
    (compress:with-zstd-decompressor (ctx)
      (let ((decompressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length compressed) by chunk-size
              for end = (min (+ start chunk-size) (length compressed))
              for chunk = (subseq compressed start end)
              for output = (compress:zstd-decompress-update ctx chunk)
              when (> (length output) 0)
                do (push output decompressed-chunks)
              until (compress:zstd-decompress-finished-p ctx))
        ;; Verify
        (let ((decompressed (apply #'concatenate '(vector (unsigned-byte 8))
                                   (nreverse decompressed-chunks))))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk decompress failed"))))))

;;; Utility Tests

(test:deftest zstd-compress-bound-reasonable ()
  "Test that zstd-compress-bound returns reasonable values"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let ((bound-100 (compress:zstd-compress-bound 100))
        (bound-1000 (compress:zstd-compress-bound 1000))
        (bound-10000 (compress:zstd-compress-bound 10000)))
    (test:assert-true (> bound-100 100)
                  "Bound should be greater than input")
    (test:assert-true (> bound-1000 1000)
                  "Bound should be greater than input")
    (test:assert-true (> bound-10000 10000)
                  "Bound should be greater than input")))

(test:deftest zstd-version-present ()
  "Test that zstd version is available"
  (unless (compress:zstd-available-p)
    (test:skip "zstd not available"))
  (let ((version (compress:zstd-version)))
    (test:assert-true (stringp version) "Version should be a string")
    (test:assert-true (> (length version) 0) "Version should not be empty")))
