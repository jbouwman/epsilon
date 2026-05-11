;;;; Algorithm Availability Tests
;;;;
;;;; Tests to verify which cryptographic algorithms are available
;;;; via the native pure-Lisp backend (no OpenSSL dependency).

(defpackage epsilon.crypto.tests.availability
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.crypto crypto)))

(deftest test-hash-algorithm-availability
  "Test that required hash algorithms produce correct-length output"
  (let ((data (crypto:random-bytes 32)))
    (assert-= (length (crypto:sha256 data)) 32)
    (assert-= (length (crypto:sha384 data)) 48)
    (assert-= (length (crypto:sha512 data)) 64)
    (assert-= (length (crypto:md5 data)) 16)
    (assert-= (length (crypto:sha1 data)) 20)))

(deftest test-random-bytes
  "Test that PRNG produces bytes of correct length"
  (let ((bytes (crypto:random-bytes 32)))
    (assert-= (length bytes) 32)
    (assert-true (typep bytes '(simple-array (unsigned-byte 8) (*))))))

#@(:timeout 60)
(deftest test-native-key-generation
  "Test native key generation algorithms"
  ;; Ed25519
  (let ((key (crypto:generate-ed25519-key)))
    (assert-true (not (null key))))

  ;; RSA (use small key size for speed in tests)
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    (assert-true (not (null key)))))

#@(:timeout 120)
(deftest test-sign-verify-format-ec-p256
  "ECDSA P-256 sign/verify round trip in both :der and :raw formats,
   and cross-format verification must fail."
  (let* ((key (crypto:generate-ec-p256-key))
         (msg (crypto:random-bytes 48)))
    ;; DER round trip
    (let ((sig-der (crypto:sign-message key msg :format :der)))
      (assert-true (crypto:verify-message key msg sig-der :format :der))
      ;; Raw is exactly 64 bytes, DER is longer; cross-format should not verify.
      (assert-not (crypto:verify-message key msg sig-der :format :raw)))
    ;; Raw round trip
    (let ((sig-raw (crypto:sign-message key msg :format :raw)))
      (assert-= (length sig-raw) 64)
      (assert-true (crypto:verify-message key msg sig-raw :format :raw))
      (assert-not (crypto:verify-message key msg sig-raw :format :der)))))

#@(:timeout 180)
(deftest test-sign-verify-format-rsa
  "RSA sign/verify round trip in both :pss and :pkcs1 formats. The
   :default verify path must accept either signature format, but strict
   :pss / :pkcs1 verification must reject the other format."
  (let* ((key (crypto:generate-rsa-key :bits 2048))
         (msg (crypto:random-bytes 48))
         (sig-pss   (crypto:sign-message key msg :format :pss))
         (sig-pkcs1 (crypto:sign-message key msg :format :pkcs1)))
    (assert-true (crypto:verify-message key msg sig-pss   :format :pss))
    (assert-true (crypto:verify-message key msg sig-pkcs1 :format :pkcs1))
    ;; :default is lenient and accepts both
    (assert-true (crypto:verify-message key msg sig-pss   :format :default))
    (assert-true (crypto:verify-message key msg sig-pkcs1 :format :default))
    ;; Strict verify must reject the other format
    (assert-not (crypto:verify-message key msg sig-pss   :format :pkcs1))
    (assert-not (crypto:verify-message key msg sig-pkcs1 :format :pss))))

#@(:timeout 60)
(deftest test-key-der-roundtrip-auto-detect
  "key-from-der auto-detects PKCS#8, SEC1, and raw Ed25519 seed formats
   without requiring a PEM label hint. Sign/verify round-trips prove
   that the recovered key material matches the original."
  (let ((msg (crypto:random-bytes 48)))
    ;; Ed25519 PKCS#8 round trip
    (let* ((key (crypto:generate-ed25519-key))
           (der (crypto:key-to-der key :private-p t :format :pkcs8))
           (imported (crypto:key-from-der der)))
      (assert-eq (crypto:native-key-type imported) :ed25519)
      (assert-true (crypto:native-key-private-p imported))
      (assert-true (crypto:verify-message imported msg
                                          (crypto:sign-message imported msg))))
    ;; EC P-256 PKCS#8 round trip
    (let* ((key (crypto:generate-ec-p256-key))
           (der (crypto:key-to-der key :private-p t :format :pkcs8))
           (imported (crypto:key-from-der der)))
      (assert-eq (crypto:native-key-type imported) :ec-p256)
      (assert-true (crypto:verify-message imported msg
                                          (crypto:sign-message imported msg))))
    ;; EC P-256 SEC1 round trip (bare, unwrapped)
    (let* ((key (crypto:generate-ec-p256-key))
           (der (crypto:key-to-der key :private-p t :format :sec1))
           (imported (crypto:key-from-der der)))
      (assert-eq (crypto:native-key-type imported) :ec-p256)
      (assert-true (crypto:verify-message imported msg
                                          (crypto:sign-message imported msg))))))

#@(:timeout 180)
(deftest test-key-der-roundtrip-rsa
  "RSA key round-trips through both PKCS#1 (`RSA PRIVATE KEY` label)
   and PKCS#8 (`PRIVATE KEY` label) containers, with auto-detection
   recovering the correct type in each case."
  (let* ((key (crypto:generate-rsa-key :bits 2048))
         (msg (crypto:random-bytes 48)))
    ;; PKCS#1 -- bare RSAPrivateKey, emitted with the `RSA PRIVATE KEY` label.
    (let* ((pem (crypto:key-to-pem key :private-p t :format :pkcs1)))
      (assert-true (search "BEGIN RSA PRIVATE KEY" pem))
      (let ((imported (crypto:key-from-pem pem :private-p t)))
        (assert-eq (crypto:native-key-type imported) :rsa)
        (assert-true (crypto:verify-message imported msg
                                            (crypto:sign-message imported msg)))))
    ;; PKCS#8 -- algorithm-wrapped, emitted with the `PRIVATE KEY` label.
    (let* ((pem (crypto:key-to-pem key :private-p t :format :pkcs8))
           (der (crypto:key-to-der key :private-p t :format :pkcs8)))
      (assert-true (search "BEGIN PRIVATE KEY" pem))
      (assert-not (search "BEGIN RSA PRIVATE KEY" pem))
      (let ((imported (crypto:key-from-der der)))
        (assert-eq (crypto:native-key-type imported) :rsa)
        (assert-true (crypto:verify-message imported msg
                                            (crypto:sign-message imported msg)))))))

(deftest test-encrypted-key-pem-roundtrip-ec
  "EC P-256 private key exports to an ENCRYPTED PRIVATE KEY PEM block
   and re-imports to a key that signs identically to the original."
  (let* ((key (crypto:generate-ec-p256-key))
         (pem (crypto:key-to-encrypted-pem key "passw0rd" :iterations 1000))
         (imported (crypto:key-from-encrypted-pem pem "passw0rd"))
         (msg (crypto:random-bytes 48)))
    (assert-true (search "BEGIN ENCRYPTED PRIVATE KEY" pem))
    (assert-eq (crypto:native-key-type imported) :ec-p256)
    ;; Sign with the imported key, verify against the original -- proves
    ;; that the private scalar round-tripped correctly.
    (let ((sig (crypto:sign-message imported msg)))
      (assert-true (crypto:verify-message key msg sig)))))

(deftest test-encrypted-key-pem-roundtrip-ed25519
  "Ed25519 round trip through ENCRYPTED PRIVATE KEY PEM."
  (let* ((key (crypto:generate-ed25519-key))
         (pem (crypto:key-to-encrypted-pem key "pw" :iterations 1000))
         (imported (crypto:key-from-encrypted-pem pem "pw"))
         (msg (crypto:random-bytes 32)))
    (assert-eq (crypto:native-key-type imported) :ed25519)
    (assert-true (crypto:verify-message
                  key msg (crypto:sign-message imported msg)))))

(deftest test-encrypted-key-pem-wrong-password
  "Decrypting with the wrong password fails loudly."
  (let* ((key (crypto:generate-ec-p256-key))
         (pem (crypto:key-to-encrypted-pem key "correct" :iterations 1000)))
    (assert-condition (error)
                      (crypto:key-from-encrypted-pem pem "incorrect"))))

(deftest test-encrypted-key-pem-public-key-rejected
  "Public keys cannot be password-encrypted (there is no private
   material to protect)."
  (let ((key (crypto:generate-ec-p256-key)))
    ;; Construct a public-key variant by exporting/importing the
    ;; public half via the unencrypted PEM path.
    (let* ((pub-pem (crypto:key-to-pem key :private-p nil))
           (pub (crypto:key-from-pem pub-pem)))
      (assert-not (crypto:native-key-private-p pub))
      (assert-condition (error)
                        (crypto:key-to-encrypted-pem pub "pw")))))

(deftest test-pkcs12-roundtrip-ec-key-with-cert
  "End-to-end PKCS#12 round trip with a real EC P-256 key and a
   cert-shaped byte vector. The recovered key must sign a message that
   the original key verifies, proving the private scalar survived."
  (let* ((key (crypto:generate-ec-p256-key))
         (cert-der (make-array 64 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 64 do (setf (aref cert-der i) (mod i 256)))
    (let ((pfx (crypto:pkcs12-encode-key key "p12-pass"
                                          :certificates (list cert-der)
                                          :iterations 200
                                          :mac-iterations 200)))
      (multiple-value-bind (certs recovered)
          (crypto:pkcs12-decode-key pfx "p12-pass")
        (assert-= (length certs) 1)
        (assert-true (equalp (first certs) cert-der))
        (assert-eq (crypto:native-key-type recovered) :ec-p256)
        (let* ((msg (crypto:random-bytes 32))
               (sig (crypto:sign-message recovered msg)))
          (assert-true (crypto:verify-message key msg sig)))))))

(deftest test-pkcs12-roundtrip-ed25519-key
  "Ed25519 key round trips through PKCS#12."
  (let* ((key (crypto:generate-ed25519-key))
         (pfx (crypto:pkcs12-encode-key key "pw"
                                         :certificates nil
                                         :iterations 200
                                         :mac-iterations 200)))
    (multiple-value-bind (certs recovered) (crypto:pkcs12-decode-key pfx "pw")
      (assert-= (length certs) 0)
      (assert-eq (crypto:native-key-type recovered) :ed25519)
      (let* ((msg (crypto:random-bytes 48))
             (sig (crypto:sign-message recovered msg)))
        (assert-true (crypto:verify-message key msg sig))))))

(deftest test-native-key-pem-roundtrip
  "Test PEM export and re-import for native keys"
  ;; Ed25519 round-trip
  (let* ((key (crypto:generate-ed25519-key))
         (private-pem (crypto:key-to-pem key :private-p t))
         (public-pem (crypto:key-to-pem key :private-p nil))
         (imported-priv (crypto:key-from-pem private-pem :private-p t))
         (imported-pub (crypto:key-from-pem public-pem)))
    (assert-true (search "PRIVATE KEY" private-pem))
    (assert-true (search "PUBLIC KEY" public-pem))
    ;; Sign with imported private key, verify with imported public key
    (let* ((msg (crypto:random-bytes 64))
           (sig (crypto:sign-message imported-priv msg)))
      (assert-true (crypto:verify-message imported-pub msg sig)))))
