;;;; Zlib Compression Tests
;;;;
;;;; Tests for one-shot and streaming compression/decompression.

(defpackage epsilon.compression.zlib-tests
  (:use :cl :epsilon.syntax :epsilon.compression.test-support)
  (:import (epsilon.test test)
            (epsilon.compression compress)))

;;; One-shot Compression Tests

(test:deftest compress-decompress-roundtrip ()
  "Test basic compress/decompress roundtrip"
  (let* ((original (make-test-data 1000 :pattern :sequential))
         (compressed (compress:compress original))
         (decompressed (compress:decompress compressed)))
    (test:assert-equalp decompressed original
                    "Decompressed data should match original")))

(test:deftest compress-empty-data ()
  "Test compressing empty data"
  (let* ((original (make-array 0 :element-type '(unsigned-byte 8)))
         (compressed (compress:compress original))
         (decompressed (compress:decompress compressed)))
    (test:assert-= (length decompressed) 0
               "Empty input should produce empty output")))

(test:deftest compress-levels ()
  "Test different compression levels"
  (let ((original (make-test-data 10000 :pattern :text)))
    ;; Test each compression level
    (dolist (level (list compress:+no-compression+
                         compress:+best-speed+
                         compress:+default-compression+
                         compress:+best-compression+))
      (let* ((compressed (compress:compress original :level level))
             (decompressed (compress:decompress compressed)))
        (test:assert-equalp decompressed original
                        (format nil "Level ~D roundtrip failed" level))))))

(test:deftest compress-ratio-improves-with-level ()
  "Higher compression levels should produce smaller output for compressible data"
  (let* ((original (make-test-data 50000 :pattern :text))
         (fast (compress:compress original :level compress:+best-speed+))
         (best (compress:compress original :level compress:+best-compression+)))
    (test:assert-true (<= (length best) (length fast))
                  "Best compression should be <= fast compression")))

(test:deftest compress-large-data ()
  "Test compression of larger data"
  (let* ((original (make-test-data 100000 :pattern :sequential))
         (compressed (compress:compress original))
         (decompressed (compress:decompress compressed)))
    (test:assert-equalp decompressed original
                    "Large data roundtrip failed")))

(test:deftest decompress-invalid-data ()
  "Test that invalid data signals an error"
  (let ((garbage (make-test-data 100 :pattern :random)))
    (test:assert-condition (error)
      (compress:decompress garbage))))

;;; Streaming Compression Tests

(test:deftest streaming-compress-single-chunk ()
  "Test streaming compression with single chunk"
  (let ((original (make-test-data 5000 :pattern :text)))
    (compress:with-deflater (ctx :level compress:+default-compression+)
      (let* ((output1 (compress:deflate-update ctx original))
             (output2 (compress:deflate-finish ctx))
             (compressed (concatenate '(vector (unsigned-byte 8)) output1 output2))
             (decompressed (compress:decompress compressed)))
        (test:assert-equalp decompressed original
                        "Streaming single chunk roundtrip failed")))))

(test:deftest streaming-compress-multiple-chunks ()
  "Test streaming compression with multiple chunks"
  (let* ((original (make-test-data 10000 :pattern :text))
         (chunk-size 1000))
    (compress:with-deflater (ctx :level compress:+default-compression+)
      (let ((compressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length original) by chunk-size
              for end = (min (+ start chunk-size) (length original))
              for chunk = (subseq original start end)
              for output = (compress:deflate-update ctx chunk)
              when (> (length output) 0)
                do (push output compressed-chunks))
        ;; Finish
        (let ((final (compress:deflate-finish ctx)))
          (when (> (length final) 0)
            (push final compressed-chunks)))
        ;; Decompress and verify
        (let* ((compressed (apply #'concatenate '(vector (unsigned-byte 8))
                                  (nreverse compressed-chunks)))
               (decompressed (compress:decompress compressed)))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk roundtrip failed"))))))

(test:deftest streaming-decompress-single-chunk ()
  "Test streaming decompression with single chunk"
  (let* ((original (make-test-data 5000 :pattern :text))
         (compressed (compress:compress original)))
    (compress:with-inflater (ctx)
      (let ((decompressed (compress:inflate-update ctx compressed)))
        (test:assert-equalp decompressed original
                        "Streaming decompress single chunk failed")
        (test:assert-true (compress:inflate-finished-p ctx)
                      "Should be finished after complete input")))))

(test:deftest streaming-decompress-multiple-chunks ()
  "Test streaming decompression with multiple chunks"
  (let* ((original (make-test-data 10000 :pattern :text))
         (compressed (compress:compress original))
         (chunk-size 500))
    (compress:with-inflater (ctx)
      (let ((decompressed-chunks nil))
        ;; Feed chunks
        (loop for start from 0 below (length compressed) by chunk-size
              for end = (min (+ start chunk-size) (length compressed))
              for chunk = (subseq compressed start end)
              for output = (compress:inflate-update ctx chunk)
              when (> (length output) 0)
                do (push output decompressed-chunks)
              until (compress:inflate-finished-p ctx))
        ;; Verify
        (let ((decompressed (apply #'concatenate '(vector (unsigned-byte 8))
                                   (nreverse decompressed-chunks))))
          (test:assert-equalp decompressed original
                          "Streaming multi-chunk decompress failed"))))))

;;; Checksum Tests

(test:deftest crc32-basic ()
  "Test CRC-32 calculation"
  (let ((data (map '(vector (unsigned-byte 8)) #'char-code "hello")))
    (let ((crc (compress:crc32 data)))
      (test:assert-true (integerp crc) "CRC-32 should return integer")
      ;; Known CRC-32 for "hello"
      (test:assert-= crc #x3610A686 "CRC-32 of 'hello' should be correct"))))

(test:deftest crc32-incremental ()
  "Test incremental CRC-32 calculation"
  (let* ((part1 (map '(vector (unsigned-byte 8)) #'char-code "hel"))
         (part2 (map '(vector (unsigned-byte 8)) #'char-code "lo"))
         (full (map '(vector (unsigned-byte 8)) #'char-code "hello"))
         (crc1 (compress:crc32 part1))
         (crc2 (compress:crc32 part2 crc1))
         (crc-full (compress:crc32 full)))
    (test:assert-= crc2 crc-full
               "Incremental CRC should match full CRC")))

(test:deftest adler32-basic ()
  "Test Adler-32 calculation"
  (let ((data (map '(vector (unsigned-byte 8)) #'char-code "hello")))
    (let ((adler (compress:adler32 data)))
      (test:assert-true (integerp adler) "Adler-32 should return integer")
      ;; Known Adler-32 for "hello"
      (test:assert-= adler #x062C0215 "Adler-32 of 'hello' should be correct"))))

;;; Gzip Format Tests (using window bits)

(test:deftest gzip-format-roundtrip ()
  "Test gzip format compression/decompression"
  (let ((original (make-test-data 5000 :pattern :text)))
    ;; Compress with gzip format
    (compress:with-deflater (ctx :window-bits compress:+gzip-wbits+)
      (let* ((output1 (compress:deflate-update ctx original))
             (output2 (compress:deflate-finish ctx))
             (compressed (concatenate '(vector (unsigned-byte 8)) output1 output2)))
        ;; Decompress with auto-detect
        (compress:with-inflater (ictx :window-bits compress:+auto-wbits+)
          (let ((decompressed (compress:inflate-update ictx compressed)))
            (test:assert-equalp decompressed original
                            "Gzip format roundtrip failed")))))))

(test:deftest raw-deflate-roundtrip ()
  "Test raw deflate format (no header)"
  (let ((original (make-test-data 5000 :pattern :text)))
    ;; Compress with raw format
    (compress:with-deflater (ctx :window-bits compress:+raw-wbits+)
      (let* ((output1 (compress:deflate-update ctx original))
             (output2 (compress:deflate-finish ctx))
             (compressed (concatenate '(vector (unsigned-byte 8)) output1 output2)))
        ;; Decompress with raw format
        (compress:with-inflater (ictx :window-bits compress:+raw-wbits+)
          (let ((decompressed (compress:inflate-update ictx compressed)))
            (test:assert-equalp decompressed original
                            "Raw deflate roundtrip failed")))))))

;;; Utility Tests

(test:deftest compress-bound-reasonable ()
  "Test that compress-bound returns reasonable values"
  (let ((bound-100 (compress:compress-bound 100))
        (bound-1000 (compress:compress-bound 1000))
        (bound-10000 (compress:compress-bound 10000)))
    (test:assert-true (> bound-100 100)
                  "Bound should be greater than input")
    (test:assert-true (> bound-1000 1000)
                  "Bound should be greater than input")
    (test:assert-true (> bound-10000 10000)
                  "Bound should be greater than input")
    (test:assert-true (< bound-100 (* 100 2))
                  "Bound should not be excessive for small data")))

(test:deftest zlib-version-present ()
  "Test that zlib version is available"
  (let ((version (compress:zlib-version)))
    (test:assert-true (stringp version) "Version should be a string")
    (test:assert-true (> (length version) 0) "Version should not be empty")
    (test:assert-true (char= (char version 0) #\1)
                  "Version should start with 1.x")))
