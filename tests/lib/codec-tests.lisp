(defpackage :epsilon.lib.codec-tests
  (:use
   :cl
   :epsilon.lib.codec
   :epsilon.lib.function
   :epsilon.tool.test)
  (:local-nicknames
   (:fs :epsilon.sys.fs)
   (:codec :epsilon.lib.codec)
   (:uri :epsilon.lib.uri)))

(in-package :epsilon.lib.codec-tests)

(defun encode-file (codec-name in out)
  (fs:stream-files (curry #'encode (codec::codec codec-name)) in out))

(defun decode-file (codec-name in out)
  (fs:stream-files (curry #'decode (codec::codec codec-name)) in out))

(defun decompress (codec compressed original)
  (fs:with-temp-file (decompressed)        ; FIME to URL
    (decode-file codec compressed decompressed)
    (is (fs:file= original decompressed))))

(defun get-test-relative-path (name)
  (project-file :epsilon/tests
                (format nil "tests/lib/~a" name)))

(defun test-decompress (codec compressed original)
  (decompress codec                     ; FIXME native URL
              (get-test-relative-path compressed)
              (get-test-relative-path original)))

(defun roundtrip (codec original)
  (fs:with-temp-file (compressed)
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
