(package epsilon.binary-tests
  (use epsilon.test)
  (import
   (epsilon.binary binary)
   (epsilon.test test)
   (epsilon.io io)))

(deftest uint16-conversion
  "Test uint16 to/from octets"
  (test:with-label "Big-endian"
    (is-equalp #(#x12 #x34) (binary:uint16-to-octets #x1234 :big-endian))
    (is-equal #x1234 (binary:octets-to-uint16 #(#x12 #x34) 0 :big-endian)))

  (test:with-label "Little-endian"
    (is-equalp #(#x34 #x12) (binary:uint16-to-octets #x1234 :little-endian))
    (is-equal #x1234 (binary:octets-to-uint16 #(#x34 #x12) 0 :little-endian)))

  (test:with-label "Round-trip"
    (let ((value #xABCD))
      (is-equal value (binary:octets-to-uint16
                       (binary:uint16-to-octets value :big-endian)
                       0 :big-endian))
      (is-equal value (binary:octets-to-uint16
                       (binary:uint16-to-octets value :little-endian)
                       0 :little-endian))))

  (test:with-label "Offset"
    (let ((bytes #(#xFF #xFF #x12 #x34 #xFF)))
      (is-equal #x1234 (binary:octets-to-uint16 bytes 2 :big-endian)))))

(deftest uint64-conversion
  "Test uint64 to/from octets"
  (test:with-label "Big-endian"
    (is-equalp #(#x00 #x00 #x00 #x00 #x00 #x00 #x12 #x34)
               (binary:uint64-to-octets #x1234 :big-endian))
    (is-equal #x1234 (binary:octets-to-uint64
                      #(#x00 #x00 #x00 #x00 #x00 #x00 #x12 #x34)
                      0 :big-endian)))

  (test:with-label "Little-endian"
    (is-equalp #(#x34 #x12 #x00 #x00 #x00 #x00 #x00 #x00)
               (binary:uint64-to-octets #x1234 :little-endian))
    (is-equal #x1234 (binary:octets-to-uint64
                      #(#x34 #x12 #x00 #x00 #x00 #x00 #x00 #x00)
                      0 :little-endian)))

  (test:with-label "Large value round-trip"
    (let ((value #xDEADBEEFCAFEBABE))
      (is-equal value (binary:octets-to-uint64
                       (binary:uint64-to-octets value :big-endian)
                       0 :big-endian))
      (is-equal value (binary:octets-to-uint64
                       (binary:uint64-to-octets value :little-endian)
                       0 :little-endian)))))

(deftest with-output-to-octets-test
  "Test with-output-to-octets macro"
  (test:with-label "Write single byte"
    (let ((result (binary:with-output-to-octets (w)
                    (binary:write w #x42 :u8))))
      (is-equalp #(#x42) result)))

  (test:with-label "Write uint16"
    (let ((result (binary:with-output-to-octets (w)
                    (binary:write w #x1234 :u16 :big-endian))))
      (is-equalp #(#x12 #x34) result)))

  (test:with-label "Write multiple values"
    (let ((result (binary:with-output-to-octets (w)
                    (binary:write w #x01 :u8)
                    (binary:write w #x1234 :u16 :big-endian)
                    (binary:write w #x05 :u8))))
      (is-equalp #(#x01 #x12 #x34 #x05) result))))

(deftest write-bytes-test
  "Test write-bytes function"
  (test:with-label "Write byte array"
    (let ((result (binary:with-output-to-octets (w)
                    (binary:write-bytes w #(#x01 #x02 #x03 #x04)))))
      (is-equalp #(#x01 #x02 #x03 #x04) result)))

  (test:with-label "Write with start/end"
    (let ((result (binary:with-output-to-octets (w)
                    (binary:write-bytes w #(#x01 #x02 #x03 #x04)
                                        :start 1 :end 3))))
      (is-equalp #(#x02 #x03) result))))

(deftest endianness-matters
  "Verify that endianness actually changes the output"
  (let ((value #x1234))
    (let ((be (binary:uint16-to-octets value :big-endian))
          (le (binary:uint16-to-octets value :little-endian)))
      (is-equalp #(#x12 #x34) be)
      (is-equalp #(#x34 #x12) le)
      (is-not (equalp be le)))))

(deftest with-endian-macro
  "Test the with-endian macro"
  (test:with-label "Big-endian default"
    (binary:with-endian (:big-endian)
      (let ((result (binary:with-output-to-octets (w)
                      (binary:write w #x1234 :u16))))
        (is-equalp #(#x12 #x34) result))))

  (test:with-label "Little-endian default"
    (binary:with-endian (:little-endian)
      (let ((result (binary:with-output-to-octets (w)
                      (binary:write w #x1234 :u16))))
        (is-equalp #(#x34 #x12) result)))))

(deftest endian-aliases
  "Test that endian aliases work"
  (let ((value #x1234))
    (is-equalp (binary:uint16-to-octets value :big-endian)
               (binary:uint16-to-octets value :be))
    (is-equalp (binary:uint16-to-octets value :big-endian)
               (binary:uint16-to-octets value :network))
    (is-equalp (binary:uint16-to-octets value :little-endian)
               (binary:uint16-to-octets value :le))))
