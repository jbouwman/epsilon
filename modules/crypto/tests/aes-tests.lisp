;;;; Tests for AES (FIPS 197)

(defpackage epsilon.crypto.aes-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.aes aes)))

(in-package :epsilon.crypto.aes-tests)

;;; FIPS 197 Appendix B - AES-128 Test Vector
(deftest test-aes128-fips197-appendix-b
  "FIPS 197 Appendix B: AES-128 encrypt"
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (plaintext (hex-to-bytes "3243f6a8885a308d313198a2e0370734"))
         (expected (hex-to-bytes "3925841d02dc09fbdc118597196a0b32"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; NIST AES-128 Known Answer Test (AESAVS)
(deftest test-aes128-kat-1
  "AES-128 KAT: all-zero key and plaintext"
  (let* ((key (hex-to-bytes "00000000000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000"))
         (expected (hex-to-bytes "66e94bd4ef8a2c3b884cfa59ca342b2e"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; NIST AES-256 Known Answer Test
(deftest test-aes256-kat-1
  "AES-256 KAT: all-zero key and plaintext"
  (let* ((key (hex-to-bytes "0000000000000000000000000000000000000000000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000"))
         (expected (hex-to-bytes "dc95c078a2408989ad48a21492842087"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; AES-256 FIPS 197 Appendix C.3
(deftest test-aes256-fips197-c3
  "FIPS 197 Appendix C.3: AES-256 encrypt"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (expected (hex-to-bytes "8ea2b7ca516745bfeafc49904b496089"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; Decrypt round-trip
(deftest test-aes128-decrypt-roundtrip
  "AES-128: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (plaintext (hex-to-bytes "3243f6a8885a308d313198a2e0370734"))
         (enc-keys (aes:make-aes-round-keys key))
         (dec-keys (aes:make-aes-decrypt-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext enc-keys))
         (recovered (aes:aes-decrypt-block ciphertext dec-keys)))
    (assert-true (equalp recovered plaintext))))

(deftest test-aes256-decrypt-roundtrip
  "AES-256: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (enc-keys (aes:make-aes-round-keys key))
         (dec-keys (aes:make-aes-decrypt-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext enc-keys))
         (recovered (aes:aes-decrypt-block ciphertext dec-keys)))
    (assert-true (equalp recovered plaintext))))

;;; AES-CTR (legacy 96-bit nonce form) -- round trip only.
(deftest test-aes128-ctr-roundtrip-96bit
  "Legacy AES-CTR (12-byte nonce + 4-byte counter) round trip."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (nonce (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         (plaintext (hex-to-bytes "6bc1bee22e409f96e93d7e117393172a"))
         (ct (aes:aes-ctr-encrypt plaintext key (subseq nonce 0 12)))
         (pt (aes:aes-ctr-encrypt ct key (subseq nonce 0 12))))
    (assert-true (equalp pt plaintext))))

;;; AES-CTR Test (NIST SP 800-38A Section F.5.1)
;;;
;;; Now using the standalone `aes-ctr` with the full 16-byte initial counter
;;; block, the NIST vector can be checked against the reference output bytes.
(deftest test-aes128-ctr-nist-f5.1
  "NIST SP 800-38A F.5.1: AES-128 CTR encrypt against the published vector."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (counter (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         (plaintext (hex-to-bytes
                     (concatenate 'string
                                  "6bc1bee22e409f96e93d7e117393172a"
                                  "ae2d8a571e03ac9c9eb76fac45af8e51"
                                  "30c81c46a35ce411e5fbc1191a0a52ef"
                                  "f69f2445df4f9b17ad2b417be66c3710")))
         (expected (hex-to-bytes
                    (concatenate 'string
                                 "874d6191b620e3261bef6864990db6ce"
                                 "9806f66b7970fdff8617187bb9fffdff"
                                 "5ae4df3edbd5d35e5b4f09020db03eab"
                                 "1e031dda2fbe03d1792170a0f3009cee")))
         (ct (aes:aes-ctr plaintext key counter))
         (pt (aes:aes-ctr ct key counter)))
    (assert-true (equalp ct expected))
    (assert-true (equalp pt plaintext))))

(defun %check-f5.1-block (counter-hex pt-hex expected-ct-hex)
  "Encrypt one NIST F.5.1 block: returns (values ok actual-keystream
   expected-keystream) where actual/expected are byte vectors."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (rk (aes:make-aes-round-keys key))
         (counter (hex-to-bytes counter-hex))
         (pt (hex-to-bytes pt-hex))
         (expected-ct (hex-to-bytes expected-ct-hex))
         (keystream (aes:aes-encrypt-block counter rk))
         (actual-ct (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 16
          do (setf (aref actual-ct i) (logxor (aref pt i) (aref keystream i))))
    (let ((expected-keystream (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 16
            do (setf (aref expected-keystream i)
                     (logxor (aref pt i) (aref expected-ct i))))
      (values (equalp actual-ct expected-ct) keystream expected-keystream))))

(deftest test-aes128-ctr-nist-f5.1-block-1
  "NIST SP 800-38A F.5.1 block 1 (counter f0...feff)."
  (assert-true (%check-f5.1-block
                "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
                "6bc1bee22e409f96e93d7e117393172a"
                "874d6191b620e3261bef6864990db6ce")))

(deftest test-aes128-ctr-nist-f5.1-block-2
  "NIST SP 800-38A F.5.1 block 2 (counter f0...ff00)."
  (assert-true (%check-f5.1-block
                "f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"
                "ae2d8a571e03ac9c9eb76fac45af8e51"
                "9806f66b7970fdff8617187bb9fffdff")))

(deftest test-aes128-ctr-nist-f5.1-block-3
  "NIST SP 800-38A F.5.1 block 3 (counter f0...ff01)."
  (assert-true (%check-f5.1-block
                "f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"
                "30c81c46a35ce411e5fbc1191a0a52ef"
                "5ae4df3edbd5d35e5b4f09020db03eab")))

(deftest test-aes128-ctr-nist-f5.1-block-4
  "NIST SP 800-38A F.5.1 block 4 (counter f0...ff02)."
  (assert-true (%check-f5.1-block
                "f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"
                "f69f2445df4f9b17ad2b417be66c3710"
                "1e031dda2fbe03d1792170a0f3009cee")))

(deftest test-aes-ctr-roundtrip-multi-block
  "Standalone aes-ctr is its own inverse over multi-block input,
   independent of whether the published NIST vector matches byte for byte."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (counter (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         (pt (hex-to-bytes
              (concatenate 'string
                           "6bc1bee22e409f96e93d7e117393172a"
                           "ae2d8a571e03ac9c9eb76fac45af8e51"
                           "30c81c46a35ce411e5fbc1191a0a52ef"
                           "f69f2445df4f9b17ad2b417be66c3710")))
         (ct (aes:aes-ctr pt key counter))
         (recovered (aes:aes-ctr ct key counter)))
    (assert-true (equalp recovered pt))))

(deftest test-aes-ctr-partial-block
  "AES-CTR handles arbitrary input lengths, including non-multiples of 16."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (counter (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         ;; First 17 bytes of the F.5.1 plaintext / ciphertext: spans one full
         ;; block plus one byte into the next block, exercising the partial
         ;; block tail of the keystream loop.
         (plaintext (hex-to-bytes "6bc1bee22e409f96e93d7e117393172aae"))
         (expected  (hex-to-bytes "874d6191b620e3261bef6864990db6ce98"))
         (ct (aes:aes-ctr plaintext key counter)))
    (assert-true (equalp ct expected))
    (assert-true (equalp (aes:aes-ctr ct key counter) plaintext))))

;;; ---- AES-CBC (NIST SP 800-38A Section F.2) ----

(deftest test-aes128-cbc-nist-f2.1
  "NIST SP 800-38A F.2.1/F.2.2: AES-128 CBC encrypt + decrypt against the
   published vector. Note this is the raw CBC vector (no padding); we
   strip the PKCS#7 padding block our encrypt path adds before comparing,
   and only check the four ciphertext blocks that the spec defines."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (iv  (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (plaintext (hex-to-bytes
                     (concatenate 'string
                                  "6bc1bee22e409f96e93d7e117393172a"
                                  "ae2d8a571e03ac9c9eb76fac45af8e51"
                                  "30c81c46a35ce411e5fbc1191a0a52ef"
                                  "f69f2445df4f9b17ad2b417be66c3710")))
         (expected-cbc-blocks
           (hex-to-bytes
            (concatenate 'string
                         "7649abac8119b246cee98e9b12e9197d"
                         "5086cb9b507219ee95db113a917678b2"
                         "73bed6b8e3c1743b7116e69e22229516"
                         "3ff1caa1681fac09120eca307586e1a7")))
         (ct (aes:aes-cbc-encrypt plaintext key iv)))
    ;; aes-cbc-encrypt always appends a full PKCS#7 padding block when the
    ;; plaintext length is already a multiple of 16, so the output is one
    ;; block longer than the NIST reference. Compare just the first 64 bytes.
    (assert-= (length ct) 80)
    (assert-true (equalp (subseq ct 0 64) expected-cbc-blocks))
    ;; Round trip recovers the original plaintext exactly.
    (assert-true (equalp (aes:aes-cbc-decrypt ct key iv) plaintext))))

(deftest test-aes256-cbc-nist-f2.5
  "NIST SP 800-38A F.2.5/F.2.6: AES-256 CBC encrypt + decrypt against the
   published vector."
  (let* ((key (hex-to-bytes
               "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"))
         (iv  (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (plaintext (hex-to-bytes
                     (concatenate 'string
                                  "6bc1bee22e409f96e93d7e117393172a"
                                  "ae2d8a571e03ac9c9eb76fac45af8e51"
                                  "30c81c46a35ce411e5fbc1191a0a52ef"
                                  "f69f2445df4f9b17ad2b417be66c3710")))
         (expected-cbc-blocks
           (hex-to-bytes
            (concatenate 'string
                         "f58c4c04d6e5f1ba779eabfb5f7bfbd6"
                         "9cfc4e967edb808d679f777bc6702c7d"
                         "39f23369a9d9bacfa530e26304231461"
                         "b2eb05e2c39be9fcda6c19078c6a9d1b")))
         (ct (aes:aes-cbc-encrypt plaintext key iv)))
    (assert-true (equalp (subseq ct 0 64) expected-cbc-blocks))
    (assert-true (equalp (aes:aes-cbc-decrypt ct key iv) plaintext))))

(deftest test-aes-cbc-pkcs7-padding-lengths
  "Encrypt/decrypt round trips at every length from 0 to 33 bytes,
   exercising every possible PKCS#7 padding length (1-16)."
  (let ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
        (iv  (hex-to-bytes "0f0e0d0c0b0a09080706050403020100")))
    (loop for n from 0 to 33 do
      (let* ((pt (make-array n :element-type '(unsigned-byte 8))))
        (loop for i from 0 below n do (setf (aref pt i) (mod (* i 7) 256)))
        (let* ((ct (aes:aes-cbc-encrypt pt key iv))
               (recovered (aes:aes-cbc-decrypt ct key iv)))
          ;; Ciphertext is padded up to the next multiple of 16, with a
          ;; full extra block when n is already block-aligned.
          (assert-= (length ct) (* 16 (1+ (floor n 16))))
          (assert-true (equalp recovered pt)))))))

(deftest test-aes-cbc-invalid-padding-rejected
  "Decrypting a ciphertext whose final byte is not a valid PKCS#7
   padding length must signal an error rather than silently returning
   garbage. We construct a 1-block ciphertext where the decrypted last
   byte is 0 (an invalid PKCS#7 padding length)."
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (iv  (hex-to-bytes "00000000000000000000000000000000"))
         ;; Encrypt an all-zero block in ECB mode (i.e. CBC with zero IV
         ;; and zero plaintext). The decrypted "plaintext" will then be
         ;; all-zero, which has a trailing 0 byte and so is not valid
         ;; PKCS#7 padding.
         (zero-block (make-array 16 :element-type '(unsigned-byte 8)
                                    :initial-element 0))
         (round-keys (aes:make-aes-round-keys key))
         (encrypted-zero (aes:aes-encrypt-block zero-block round-keys)))
    (assert-condition (error)
                      (aes:aes-cbc-decrypt encrypted-zero key iv))))

;;; AES-CTR round-trip with multi-block
(deftest test-aes-ctr-roundtrip-multiblock
  "AES-CTR: round-trip with multiple blocks"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (make-array 100 :element-type '(unsigned-byte 8)))
         (_ (loop for i from 0 below 100 do (setf (aref plaintext i) (mod i 256))))
         (ct (aes:aes-ctr-encrypt plaintext key nonce))
         (pt (aes:aes-ctr-encrypt ct key nonce)))
    (declare (ignore _))
    (assert-true (equalp pt plaintext))))

;;; AES-CTR produces different output from plaintext
(deftest test-aes-ctr-encrypts
  "AES-CTR: ciphertext differs from plaintext"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x41))
         (ct (aes:aes-ctr-encrypt plaintext key nonce)))
    (assert-not (equalp ct plaintext))))

;;; ---------------------------------------------------------------------------
;;; Constant-time S-box: bitwise GF(2^8) computation matches the table
;;; ---------------------------------------------------------------------------

(deftest test-aes-sbox-ct-matches-table
  "aes-sbox-ct over GF(2^8) matches the standard 256-entry +sbox+
   table on every byte (0..255)."
  (loop for x from 0 below 256
        do (assert-= (aref aes::+sbox+ x)
                     (aes::aes-sbox-ct x))))

(deftest test-aes-inv-sbox-ct-matches-table
  "aes-inv-sbox-ct matches +inv-sbox+ on every byte."
  (loop for x from 0 below 256
        do (assert-= (aref aes::+inv-sbox+ x)
                     (aes::aes-inv-sbox-ct x))))

(deftest test-aes-gf-mul-ct-matches
  "Branchless gf-mul-ct matches the existing gf-mul on representative
   operand pairs covering small, large, and zero inputs."
  (dolist (pair '((0 0) (1 1) (1 255) (255 1) (2 128)
                  (#x53 #xca) (#x7e #x42) (#xff #xff)
                  (#x9a #x37) (#x80 #x80) (#x01 #x53)))
    (destructuring-bind (a b) pair
      (assert-= (aes::gf-mul a b)
                (aes::gf-mul-ct a b)))))

(deftest test-aes-xtime-ct-matches
  "xtime-ct matches the existing xtime on every byte."
  (loop for x from 0 below 256
        do (assert-= (aes::xtime x) (aes::xtime-ct x))))

;;; ---------------------------------------------------------------------------
;;; CT-mode encryption matches table-mode encryption
;;; ---------------------------------------------------------------------------

(deftest test-aes-128-encrypt-block-ct-matches-table
  "AES-128 encrypt-block under *aes-constant-time* = T produces the
   same ciphertext as the table-based path on the FIPS 197 Appendix
   B vector."
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (plaintext (hex-to-bytes "3243f6a8885a308d313198a2e0370734"))
         (round-keys (aes:make-aes-round-keys key))
         (ct-table (aes:aes-encrypt-block plaintext round-keys))
         (ct-ct (let ((aes:*aes-constant-time* t))
                  (aes:aes-encrypt-block plaintext
                                         (aes:make-aes-round-keys key)))))
    (assert-equalp ct-table ct-ct)
    (assert-equalp ct-table
                   (hex-to-bytes "3925841d02dc09fbdc118597196a0b32"))))

(deftest test-aes-128-decrypt-block-ct-roundtrip
  "AES-128 encrypt under CT followed by decrypt under CT recovers
   the plaintext."
  (let ((aes:*aes-constant-time* t))
    (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
           (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
           (enc-rks (aes:make-aes-round-keys key))
           (dec-rks (aes:make-aes-decrypt-round-keys key))
           (ct (aes:aes-encrypt-block plaintext enc-rks))
           (pt (aes:aes-decrypt-block ct dec-rks)))
      (assert-equalp pt plaintext))))

(deftest test-aes-256-encrypt-block-ct-matches-table
  "AES-256 encrypt-block under CT matches the FIPS 197 Appendix C.3
   vector."
  (let* ((key (hex-to-bytes
               "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (round-keys (aes:make-aes-round-keys key))
         (ct-table (aes:aes-encrypt-block plaintext round-keys))
         (ct-ct (let ((aes:*aes-constant-time* t))
                  (aes:aes-encrypt-block plaintext
                                         (aes:make-aes-round-keys key)))))
    (assert-equalp ct-table ct-ct)
    (assert-equalp ct-ct
                   (hex-to-bytes "8ea2b7ca516745bfeafc49904b496089"))))

(deftest test-aes-ct-mode-cross-decrypt
  "An AES block encrypted under *aes-constant-time* = NIL (table)
   decrypts correctly under *aes-constant-time* = T (CT). The CT
   path is a drop-in replacement for the table path."
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (ct (aes:aes-encrypt-block plaintext (aes:make-aes-round-keys key)))
         (pt (let ((aes:*aes-constant-time* t))
               (aes:aes-decrypt-block ct (aes:make-aes-decrypt-round-keys key)))))
    (assert-equalp pt plaintext)))
