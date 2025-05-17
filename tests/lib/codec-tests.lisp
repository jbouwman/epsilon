(defpackage #:epsilon.lib.codec-tests
  (:use
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:codec #:epsilon.lib.codec)
   (#:fs #:epsilon.sys.fs)
   (#:stream #:epsilon.lib.stream)
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.codec-tests)

(defun read-file-into-byte-vector (file-path)
  "Read a file into a byte vector."
  (with-open-file (stream file-path :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun compare-streams (stream1 stream2 &key (context-size 20))
  "Compare contents of two streams and report differences with context."
  (let* ((buf1 (stream:buffer stream1))
         (buf2 (stream:buffer stream2))
         (len1 (length buf1))
         (len2 (length buf2)))
    (if (= len1 len2)
        (loop for i from 0 below len1
              when (/= (aref buf1 i) (aref buf2 i))
                do (let* ((start (max 0 (- i context-size)))
                          (end (min len1 (+ i context-size)))
                          (buf1-context (subseq buf1 start end))
                          (buf2-context (subseq buf2 start end)))
                     (format *error-output* "~&Difference detected at position ~D: ~%~%Actual:   ~A~%Expected: ~A~%"
                             i buf1-context buf2-context)
                     (return nil))
              finally (return t))
        (progn
          (format *error-output* "~&Streams have different lengths: ~D vs ~D~%" len1 len2)
          nil))))

(defun decompress (codec compressed original)
  (let* ((original-data (read-file-into-byte-vector original))
         (original-stream (stream:make-input-stream original-data))
         (compressed-data (read-file-into-byte-vector compressed))
         (compressed-stream (stream:make-input-stream compressed-data))
         (decompressed-stream (stream:make-output-stream)))
    (codec:decode codec compressed-stream decompressed-stream)
    (let ((decompressed-input (stream:make-input-stream (stream:buffer decompressed-stream))))
      (is (compare-streams decompressed-input original-stream)))))

(defun get-test-relative-path (name)
  (project-file :epsilon/tests
                (format nil "tests/lib/~a" name)))

(defun test-decompress (codec compressed original)
  (decompress codec                     ; FIXME native URL
              (get-test-relative-path compressed)
              (get-test-relative-path original)))

(defun roundtrip (codec original)
  (let* ((original-data (read-file-into-byte-vector original))
         (original-stream (stream:make-input-stream original-data))
         (compressed-stream (stream:make-output-stream))
         (decompressed-stream (stream:make-output-stream)))
    (codec:2encode codec original-stream compressed-stream)
    (let ((compressed-input (stream:make-input-stream (stream:buffer compressed-stream))))
      (decode codec compressed-input decompressed-stream)
      (let ((decompressed-input (stream:make-input-stream (stream:buffer decompressed-stream))))
        (is (compare-streams decompressed-input original-stream))))))

(defun test-roundtrip (codec original)
  (let ((path (project-file :epsilon/tests
                            (format nil "tests/lib/~a" original))))
    (roundtrip codec path)))

(deftest deflate ()
  (test-roundtrip :deflate "shilling.txt"))

(deftest zlib ()
  (test-roundtrip :zlib "shilling.txt"))

(deftest gzip ()
  (test-roundtrip :gzip "shilling.txt"))

(deftest gzip-1 ()
  (test-decompress :gzip "shilling.txt.gz1" "shilling.txt"))

(deftest gzip-9 ()
  (test-decompress :gzip "shilling.txt.gz9" "shilling.txt"))

(deftest bzip2 ()
  (test-decompress :bzip2 "shilling.txt.bz2" "shilling.txt"))
