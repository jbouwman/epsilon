(defpackage epsilon.digest.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (digest epsilon.digest)
   (hex epsilon.hex)))

(in-package epsilon.digest.tests)

;; shasum -a 256 tests/lib/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2
  (let ((shilling (module-file "epsilon.core" "tests/lib/shilling.txt")))
    (with-open-file (stream shilling :element-type 'unsigned-byte)
      (let ((hasher (digest:make-sha256)))
        (loop for byte = (read-byte stream nil)
              while byte
              do (digest:update hasher (vector byte)))
        (is (string=
             "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
             (string-downcase (digest:bytes-to-hex (digest:finalize hasher)))))))))

(deftest sha-1-basic
  "Test SHA-1 with known test vectors"
  ;; Test empty string: sha1sum /dev/null -> da39a3ee5e6b4b0d3255bfef95601890afd80709
  (is (string=
       "da39a3ee5e6b4b0d3255bfef95601890afd80709"
       (string-downcase (digest:bytes-to-hex (digest:sha1 "")))))
  
  ;; Test "abc": echo -n "abc" | sha1sum -> a9993e364706816aba3e25717850c26c9cd0d89d
  (is (string=
       "a9993e364706816aba3e25717850c26c9cd0d89d"
       (string-downcase (digest:bytes-to-hex (digest:sha1 "abc")))))
  
  ;; Test longer string: echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | sha1sum
  ;; -> 84983e441c3bd26ebaae4aa1f95129e5e54670f1
  (is (string=
       "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
       (string-downcase (digest:bytes-to-hex (digest:sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))))

(deftest sha-1-streaming
  "Test SHA-1 streaming interface"
  ;; Test "abc" using streaming interface
  (let ((hasher (digest:make-sha1)))
    (digest:update hasher "a")
    (digest:update hasher "b")
    (digest:update hasher "c")
    (is (string=
         "a9993e364706816aba3e25717850c26c9cd0d89d"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher))))))
  
  ;; Test reset functionality
  (let ((hasher (digest:make-sha1)))
    (digest:update hasher "abc")
    (digest:reset hasher)
    (digest:update hasher "test")
    ;; echo -n "test" | sha1sum -> a94a8fe5ccb19ba61c4c0873d391e987982fbbd3
    (is (string=
         "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher)))))))

(deftest sha-1-binary-data
  "Test SHA-1 with binary data"
  ;; Test with byte array
  (let ((data (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99)))) ; "abc" as bytes
    (is (string=
         "a9993e364706816aba3e25717850c26c9cd0d89d"
         (string-downcase (digest:bytes-to-hex (digest:sha1 data))))))
  
  ;; Test with vector of integers
  (is (string=
       "a9993e364706816aba3e25717850c26c9cd0d89d"
       (string-downcase (digest:bytes-to-hex (digest:sha1 '(97 98 99)))))))

(deftest sha-1-large-data
  "Test SHA-1 with larger data"
  ;; Test with repeated pattern to stress the block processing
  (let ((data (make-string 1000 :initial-element #\a)))
    ;; This is the SHA-1 of 1000 'a' characters
    ;; perl -E 'print "a" x 1000' | sha1sum -> 291e9a6c66994949b57ba5e650361e98fc36b1ba
    (is (string=
         "291e9a6c66994949b57ba5e650361e98fc36b1ba"
         (string-downcase (digest:bytes-to-hex (digest:sha1 data)))))))

(deftest sha-1-compatibility
  "Test SHA-1 legacy compatibility function"
  (let ((data (map 'vector #'char-code "abc")))
    (is (string=
         "a9993e364706816aba3e25717850c26c9cd0d89d"
         (string-downcase (digest:bytes-to-hex (digest:sha1-digest data)))))))

(deftest md5-basic
  "Test MD5 with known test vectors"
  ;; Test empty string: md5sum /dev/null -> d41d8cd98f00b204e9800998ecf8427e
  (is (string=
       "d41d8cd98f00b204e9800998ecf8427e"
       (string-downcase (digest:bytes-to-hex (digest:md5 "")))))
  
  ;; Test "abc": echo -n "abc" | md5sum -> 900150983cd24fb0d6963f7d28e17f72
  (is (string=
       "900150983cd24fb0d6963f7d28e17f72"
       (string-downcase (digest:bytes-to-hex (digest:md5 "abc")))))
  
  ;; Test "The quick brown fox jumps over the lazy dog"
  ;; echo -n "The quick brown fox jumps over the lazy dog" | md5sum -> 9e107d9d372bb6826bd81d3542a419d6
  (is (string=
       "9e107d9d372bb6826bd81d3542a419d6"
       (string-downcase (digest:bytes-to-hex (digest:md5 "The quick brown fox jumps over the lazy dog")))))
  
  ;; Test longer string: echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | md5sum
  ;; -> 8215ef0796a20bcaaae116d3876c664a
  (is (string=
       "8215ef0796a20bcaaae116d3876c664a"
       (string-downcase (digest:bytes-to-hex (digest:md5 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))))

(deftest md5-streaming
  "Test MD5 streaming interface"
  ;; Test "abc" using streaming interface
  (let ((hasher (digest:make-md5)))
    (digest:update hasher "a")
    (digest:update hasher "b")
    (digest:update hasher "c")
    (is (string=
         "900150983cd24fb0d6963f7d28e17f72"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher))))))
  
  ;; Test reset functionality
  (let ((hasher (digest:make-md5)))
    (digest:update hasher "abc")
    (digest:reset hasher)
    (digest:update hasher "test")
    ;; echo -n "test" | md5sum -> 098f6bcd4621d373cade4e832627b4f6
    (is (string=
         "098f6bcd4621d373cade4e832627b4f6"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher)))))))

(deftest md5-binary-data
  "Test MD5 with binary data"
  ;; Test with byte array
  (let ((data (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99)))) ; "abc" as bytes
    (is (string=
         "900150983cd24fb0d6963f7d28e17f72"
         (string-downcase (digest:bytes-to-hex (digest:md5 data))))))
  
  ;; Test with vector of integers
  (is (string=
       "900150983cd24fb0d6963f7d28e17f72"
       (string-downcase (digest:bytes-to-hex (digest:md5 '(97 98 99)))))))

(deftest md5-large-data
  "Test MD5 with larger data"
  ;; Test with repeated pattern to stress the block processing
  (let ((data (make-string 1000 :initial-element #\a)))
    ;; This is the MD5 of 1000 'a' characters
    ;; perl -E 'print "a" x 1000' | md5sum -> cabe45dcc9ae5b66ba86600cca6b8ba8
    (is (string=
         "cabe45dcc9ae5b66ba86600cca6b8ba8"
         (string-downcase (digest:bytes-to-hex (digest:md5 data)))))))

(deftest md5-known-vectors
  "Test MD5 with additional known test vectors"
  ;; Test "message digest"
  ;; echo -n "message digest" | md5sum -> f96b697d7cb7938d525a2f31aaf161d0
  (is (string=
       "f96b697d7cb7938d525a2f31aaf161d0"
       (string-downcase (digest:bytes-to-hex (digest:md5 "message digest")))))
  
  ;; Test "abcdefghijklmnopqrstuvwxyz"
  ;; echo -n "abcdefghijklmnopqrstuvwxyz" | md5sum -> c3fcd3d76192e4007dfb496cca67e13b
  (is (string=
       "c3fcd3d76192e4007dfb496cca67e13b"
       (string-downcase (digest:bytes-to-hex (digest:md5 "abcdefghijklmnopqrstuvwxyz")))))
  
  ;; Test "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  ;; echo -n "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" | md5sum -> d174ab98d277d9f5a5611c2c9f419d9f
  (is (string=
       "d174ab98d277d9f5a5611c2c9f419d9f"
       (string-downcase (digest:bytes-to-hex (digest:md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))))))

(deftest sha3-256-basic
  "Test SHA3-256 with known test vectors"
  ;; Test empty string: echo -n "" | sha3sum -a 256 -> a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
  (is (string=
       "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "")))))
  
  ;; Test "abc": echo -n "abc" | sha3sum -a 256 -> 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
  (is (string=
       "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "abc")))))
  
  ;; Test "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  ;; echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | sha3sum -a 256
  ;; -> 41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376
  (is (string=
       "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))))

(deftest sha3-256-streaming
  "Test SHA3-256 streaming interface"
  ;; Test "abc" using streaming interface
  (let ((hasher (digest:make-sha3-256)))
    (digest:update hasher "a")
    (digest:update hasher "b")
    (digest:update hasher "c")
    (is (string=
         "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher))))))
  
  ;; Test reset functionality
  (let ((hasher (digest:make-sha3-256)))
    (digest:update hasher "abc")
    (digest:reset hasher)
    (digest:update hasher "test")
    ;; echo -n "test" | sha3sum -a 256 -> 36f028580bb02cc8272a9a020f4200e346e276ae664e45ee80745574e2f5ab80
    (is (string=
         "36f028580bb02cc8272a9a020f4200e346e276ae664e45ee80745574e2f5ab80"
         (string-downcase (digest:bytes-to-hex (digest:finalize hasher)))))))

(deftest sha3-256-binary-data
  "Test SHA3-256 with binary data"
  ;; Test with byte array
  (let ((data (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99)))) ; "abc" as bytes
    (is (string=
         "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
         (string-downcase (digest:bytes-to-hex (digest:sha3-256 data))))))
  
  ;; Test with vector of integers
  (is (string=
       "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 '(97 98 99)))))))

(deftest sha3-256-large-data
  "Test SHA3-256 with larger data"
  ;; Test with repeated pattern to stress the block processing
  (let ((data (make-string 1000 :initial-element #\a)))
    ;; This is the SHA3-256 of 1000 'a' characters
    ;; perl -E 'print "a" x 1000' | openssl dgst -sha3-256 -> 8f3934e6f7a15698fe0f396b95d8c4440929a8fa6eae140171c068b4549fbf81
    (is (string=
         "8f3934e6f7a15698fe0f396b95d8c4440929a8fa6eae140171c068b4549fbf81"
         (string-downcase (digest:bytes-to-hex (digest:sha3-256 data)))))))

(deftest sha3-256-known-vectors
  "Test SHA3-256 with additional known test vectors"
  ;; Test "The quick brown fox jumps over the lazy dog"
  ;; echo -n "The quick brown fox jumps over the lazy dog" | sha3sum -a 256 -> 69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04
  (is (string=
       "69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "The quick brown fox jumps over the lazy dog")))))
  
  ;; Test "message digest"
  ;; echo -n "message digest" | sha3sum -a 256 -> edcdb2069366e75243860c18c3a11465eca34bce6143d30c8665cefcfd32bffd
  (is (string=
       "edcdb2069366e75243860c18c3a11465eca34bce6143d30c8665cefcfd32bffd"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "message digest")))))
  
  ;; Test "abcdefghijklmnopqrstuvwxyz"
  ;; echo -n "abcdefghijklmnopqrstuvwxyz" | sha3sum -a 256 -> 7cab2dc765e21b241dbc1c255ce620b29f527c6d5e7f5f843e56288f0d707521
  (is (string=
       "7cab2dc765e21b241dbc1c255ce620b29f527c6d5e7f5f843e56288f0d707521"
       (string-downcase (digest:bytes-to-hex (digest:sha3-256 "abcdefghijklmnopqrstuvwxyz"))))))
