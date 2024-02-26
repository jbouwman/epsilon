(in-package :epsilon-tests)

(defun decompress (codec compressed original)
  (uiop:with-temporary-file (:pathname decompressed)
    (decode-file codec compressed decompressed)
    (is (encode::file= original decompressed))))

(defun test-decompress (codec compressed original)
  (decompress codec (test-file compressed) (test-file original)))

(defun roundtrip (codec original)
  (uiop:with-temporary-file (:pathname compressed)
    (encode-file codec original compressed)
    (decompress codec compressed original)))

(defun test-roundtrip (codec original)
  (roundtrip codec (test-file original)))

(deftest deflate-roundtrip ()
  (skip)
  (test-roundtrip :deflate "shilling.txt"))

(deftest zlib-roundtrip ()
  (test-roundtrip :zlib "shilling.txt"))

(deftest gzip-roundtrip ()
  (test-roundtrip :gzip "shilling.txt"))

(deftest gzip-decompress-1 ()
  (test-decompress :gzip "shilling.txt.gz1" "shilling.txt"))

(deftest gzip-decompress-9 ()
  (test-decompress :gzip "shilling.txt.gz9" "shilling.txt"))

(deftest bzip2-decompress ()
  (test-decompress :bzip2 "shilling.txt.bz2" "shilling.txt"))
