(tool.test:define-test-package #:lib.codec/tests
  (:use
   #:lib.codec
   #:lib.stream
   #:sys.fs)
  (:local-nicknames
   (#:uri #:lib.uri)))

(in-package #:lib.codec/tests)

(defun decompress (codec compressed original)
  (with-temp-file (decompressed)        ; FIME to URL
    (decode-file codec compressed decompressed)
    (is (file= original decompressed))))

(defun test-decompress (codec compressed original)
  (decompress codec                     ; FIXME native URL
              (uri:path (test-data compressed))
              (uri:path (test-data original))))

(defun roundtrip (codec original)
  (with-temp-file (compressed)
    (encode-file codec original compressed)
    (decompress codec compressed original)))

(defun test-roundtrip (codec original)
  (roundtrip codec (uri:path (test-data original))))

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
