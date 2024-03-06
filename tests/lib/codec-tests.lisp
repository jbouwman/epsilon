(tool.test:define-test-package #:lib.codec/tests
  (:use
   #:lib.codec
   #:lib.stream
   #:sys.directory))

(in-package #:lib.codec/tests)

(defun decompress (codec compressed original)
  (with-tempfile (decompressed)
    (decode-file codec compressed decompressed)
    (is (file= original decompressed))))

(defun test-decompress (codec compressed original)
  (decompress codec (test-file compressed) (test-file original)))

(defun roundtrip (codec original)
  (with-tempfile (compressed)
    (encode-file codec original compressed)
    (decompress codec compressed original)))

(defun test-roundtrip (codec original)
  (roundtrip codec (test-file original)))

(deftest deflate ()
  (skip)
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
