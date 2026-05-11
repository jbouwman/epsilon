;;;; Brotli Compression Tests
;;;;
;;;; Tests for one-shot and streaming brotli compression/decompression.

(defpackage epsilon.compression.brotli-tests
  (:use :cl :epsilon.syntax :epsilon.compression.test-support)
  (:import (epsilon.test test)
            (epsilon.compression compress)))

;;; Availability Test

(test:deftest brotli-availability ()
  "Test that brotli is available"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (test:assert-true (compress:brotli-available-p) "Brotli should be available"))

;;; One-shot Compression Tests

(test:deftest brotli-compress-decompress-roundtrip ()
  "Test basic brotli compress/decompress roundtrip"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 1000 :pattern :sequential))
         (compressed (compress:brotli-compress original))
         (decompressed (compress:brotli-decompress compressed)))
    (test:assert-equalp decompressed original
                    "Decompressed data should match original")))

(test:deftest brotli-compress-empty-data ()
  "Test brotli compressing empty data"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-array 0 :element-type '(unsigned-byte 8)))
         (compressed (compress:brotli-compress original))
         (decompressed (compress:brotli-decompress compressed)))
    (test:assert-= (length decompressed) 0
               "Empty input should produce empty output")))

(test:deftest brotli-compress-quality-levels ()
  "Test different brotli quality levels"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((original (make-test-data 10000 :pattern :text)))
    ;; Test min, default, and max quality levels
    (dolist (quality (list compress:+brotli-quality-min+
                           6  ; mid-level
                           compress:+brotli-quality-max+))
      (let* ((compressed (compress:brotli-compress original :quality quality))
             (decompressed (compress:brotli-decompress compressed)))
        (test:assert-equalp decompressed original
                        (format nil "Quality ~D roundtrip failed" quality))))))

(test:deftest brotli-compress-ratio-improves-with-quality ()
  "Higher quality levels should produce smaller output"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 50000 :pattern :text))
         (fast (compress:brotli-compress original :quality compress:+brotli-quality-min+))
         (best (compress:brotli-compress original :quality compress:+brotli-quality-max+)))
    (test:assert-true (<= (length best) (length fast))
                  "Best compression should be <= fast compression")))

(test:deftest brotli-compress-modes ()
  "Test brotli compression modes"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((original (make-test-data 10000 :pattern :text)))
    ;; Test different modes
    (dolist (mode (list compress:+brotli-mode-generic+
                        compress:+brotli-mode-text+))
      (let* ((compressed (compress:brotli-compress original :mode mode))
             (decompressed (compress:brotli-decompress compressed)))
        (test:assert-equalp decompressed original
                        (format nil "Mode ~D roundtrip failed" mode))))))

(test:deftest brotli-compress-large-data ()
  "Test brotli compression of larger data"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 100000 :pattern :sequential))
         (compressed (compress:brotli-compress original))
         (decompressed (compress:brotli-decompress compressed)))
    (test:assert-equalp decompressed original
                    "Large data roundtrip failed")))

(test:deftest brotli-decompress-invalid-data ()
  "Test that invalid data signals an error"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((garbage (make-test-data 100 :pattern :random)))
    (test:assert-condition (error)
      (compress:brotli-decompress garbage))))

;;; Streaming Compression Tests

(test:deftest brotli-streaming-compress-single-chunk ()
  "Test brotli streaming compression with single chunk"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((original (make-test-data 5000 :pattern :text)))
    (compress:with-brotli-compressor (ctx :quality 6)
      (let* ((output1 (compress:brotli-compress-update ctx original))
             (output2 (compress:brotli-compress-finish ctx))
             (compressed (concatenate '(vector (unsigned-byte 8)) output1 output2))
             (decompressed (compress:brotli-decompress compressed)))
        (test:assert-equalp decompressed original
                        "Streaming single chunk roundtrip failed")))))

(test:deftest brotli-streaming-compress-multiple-chunks ()
  "Test brotli streaming compression with multiple chunks"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 10000 :pattern :text))
         (chunk-size 1000))
    (compress:with-brotli-compressor (ctx :quality 6)
      (let ((compressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length original) by chunk-size
              for end = (min (+ start chunk-size) (length original))
              for chunk = (subseq original start end)
              for output = (compress:brotli-compress-update ctx chunk)
              when (> (length output) 0)
                do (push output compressed-chunks))
        ;; Finish
        (let ((final (compress:brotli-compress-finish ctx)))
          (when (> (length final) 0)
            (push final compressed-chunks)))
        ;; Decompress and verify
        (let* ((compressed (apply #'concatenate '(vector (unsigned-byte 8))
                                  (nreverse compressed-chunks)))
               (decompressed (compress:brotli-decompress compressed)))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk roundtrip failed"))))))

(test:deftest brotli-streaming-decompress-single-chunk ()
  "Test brotli streaming decompression with single chunk"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 5000 :pattern :text))
         (compressed (compress:brotli-compress original)))
    (compress:with-brotli-decompressor (ctx)
      (let ((decompressed (compress:brotli-decompress-update ctx compressed)))
        (test:assert-equalp decompressed original
                        "Streaming decompress single chunk failed")
        (test:assert-true (compress:brotli-decompress-finished-p ctx)
                      "Should be finished after complete input")))))

(test:deftest brotli-streaming-decompress-multiple-chunks ()
  "Test brotli streaming decompression with multiple chunks"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 10000 :pattern :text))
         (compressed (compress:brotli-compress original))
         (chunk-size 500))
    (compress:with-brotli-decompressor (ctx)
      (let ((decompressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length compressed) by chunk-size
              for end = (min (+ start chunk-size) (length compressed))
              for chunk = (subseq compressed start end)
              for output = (compress:brotli-decompress-update ctx chunk)
              when (> (length output) 0)
                do (push output decompressed-chunks)
              until (compress:brotli-decompress-finished-p ctx))
        ;; Verify
        (let ((decompressed (apply #'concatenate '(vector (unsigned-byte 8))
                                   (nreverse decompressed-chunks))))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk decompress failed"))))))

;;; Utility Tests

(test:deftest brotli-compress-bound-reasonable ()
  "Test that brotli-compress-bound returns reasonable values"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((bound-100 (compress:brotli-compress-bound 100))
        (bound-1000 (compress:brotli-compress-bound 1000))
        (bound-10000 (compress:brotli-compress-bound 10000)))
    (test:assert-true (> bound-100 100)
                  "Bound should be greater than input")
    (test:assert-true (> bound-1000 1000)
                  "Bound should be greater than input")
    (test:assert-true (> bound-10000 10000)
                  "Bound should be greater than input")))

(test:deftest brotli-version-present ()
  "Test that brotli version is available"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let ((version (compress:brotli-version)))
    (test:assert-true (stringp version) "Version should be a string")
    (test:assert-true (> (length version) 0) "Version should not be empty")))

;;; Text mode optimization test

(test:deftest brotli-text-mode-better-for-text ()
  "Text mode should produce better compression for text data"
  (unless (compress:brotli-available-p)
    (test:skip "brotli not available"))
  (let* ((original (make-test-data 50000 :pattern :text))
         (generic (compress:brotli-compress original :mode compress:+brotli-mode-generic+))
         (text (compress:brotli-compress original :mode compress:+brotli-mode-text+)))
    ;; Text mode should be at least as good as generic for text
    (test:assert-true (<= (length text) (+ (length generic) 100))
                  "Text mode should be competitive for text data")))
