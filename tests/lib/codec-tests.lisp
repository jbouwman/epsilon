(epsilon.tool.test:define-test-package #:epsilon.lib.codec/tests
  (:use
   #:epsilon.lib.codec
   #:epsilon.lib.stream
   #:epsilon.sys.fs)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.codec/tests)



;; (get-test-relative-path 'deflate "../shilling.txt")


(defun decompress (codec compressed original)
  (with-temp-file (decompressed)        ; FIME to URL
    (decode-file codec compressed decompressed)
    (is (file= original decompressed))))

(defun test-decompress (codec compressed original)
  (decompress codec                     ; FIXME native URL
              (get-test-relative-path 'zlib compressed)
              (get-test-relative-path 'zlib original)))

(defun roundtrip (codec original)
  (with-temp-file (compressed)
    (encode-file codec original compressed)
    (decompress codec compressed original)))

(defun test-roundtrip (codec original)
  (let ((path (get-test-relative-path 'zlib original)))
    (roundtrip codec path)))

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
