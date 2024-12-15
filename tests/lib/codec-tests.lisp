(defpackage #:epsilon.lib.codec.tests
  (:use
   #:cl
   #:epsilon.lib.codec
   #:epsilon.lib.stream
   #:epsilon.sys.fs
   #:epsilon.tool.test)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.codec.tests)

(defun decompress (codec compressed original)
  (with-temp-file (decompressed)        ; FIME to URL
    (decode-file codec compressed decompressed)
    (is (file= original decompressed))))

(defun get-test-relative-path (name)
  (project-file :epsilon/tests
                (format nil "tests/lib/~a" name)))

(defun test-decompress (codec compressed original)
  (decompress codec                     ; FIXME native URL
              (get-test-relative-path compressed)
              (get-test-relative-path original)))

(defun roundtrip (codec original)
  (with-temp-file (compressed)
    (encode-file codec original compressed)
    (decompress codec compressed original)))

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
