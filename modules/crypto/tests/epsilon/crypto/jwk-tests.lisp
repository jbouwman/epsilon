;;;; JWK Tests

(defpackage epsilon.crypto.tests.jwk
  (:use :cl :epsilon.test)
  (:require (epsilon.crypto.jwk jwk)
            (epsilon.crypto.native native)
            (epsilon.base-encode enc))
  (:enter t))

;;; ============================================================================
;;; EC P-256 JWK Tests
;;; ============================================================================

(deftest test-ec-p256-key-to-jwk
  "EC P-256 private key produces valid JWK with kty, crv, x, y"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key)))
    (assert-equal "EC" (cdr (assoc :kty j)))
    (assert-equal "P-256" (cdr (assoc :crv j)))
    (assert-true (stringp (cdr (assoc :x j))))
    (assert-true (stringp (cdr (assoc :y j))))))

(deftest test-ec-p256-jwk-roundtrip
  "EC P-256 key survives JWK roundtrip (public key)"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key))
         (restored (jwk:key-from-jwk j)))
    (assert-equal :ec-p256 (native:native-key-type restored))
    (assert-not (native:native-key-private-p restored))
    ;; Material should be 65-byte SEC1 uncompressed point
    (assert-= 65 (length (native:native-key-material restored)))))

(deftest test-ec-p256-jwk-metadata
  "JWK includes optional metadata fields"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key :kid "my-key" :use "sig" :alg "ES256")))
    (assert-equal "my-key" (cdr (assoc :kid j)))
    (assert-equal "sig" (cdr (assoc :use j)))
    (assert-equal "ES256" (cdr (assoc :alg j)))))

(deftest test-ec-p256-x-y-are-32-bytes
  "EC P-256 x and y coordinates decode to 32 bytes each"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key))
         (x-bytes (enc:base64-decode-url (cdr (assoc :x j))))
         (y-bytes (enc:base64-decode-url (cdr (assoc :y j)))))
    (assert-= 32 (length x-bytes))
    (assert-= 32 (length y-bytes))))

;;; ============================================================================
;;; RSA JWK Tests
;;; ============================================================================

(deftest test-rsa-key-to-jwk
  "RSA key produces valid JWK with kty, n, e"
  (let* ((key (native:generate-rsa-key :bits 2048))
         (j (jwk:key-to-jwk key)))
    (assert-equal "RSA" (cdr (assoc :kty j)))
    (assert-true (stringp (cdr (assoc :n j))))
    (assert-true (stringp (cdr (assoc :e j))))))

#@(:timeout 30)
(deftest test-rsa-jwk-roundtrip
  "RSA key survives JWK roundtrip (public key)"
  (let* ((key (native:generate-rsa-key :bits 2048))
         (j (jwk:key-to-jwk key))
         (restored (jwk:key-from-jwk j)))
    (assert-equal :rsa (native:native-key-type restored))
    (assert-not (native:native-key-private-p restored))))

;;; ============================================================================
;;; JWKS Tests
;;; ============================================================================

(deftest test-keys-to-jwks
  "keys-to-jwks wraps list in :keys"
  (let* ((key1 (native:generate-ec-p256-key))
         (key2 (native:generate-ec-p256-key))
         (j1 (jwk:key-to-jwk key1 :kid "k1"))
         (j2 (jwk:key-to-jwk key2 :kid "k2"))
         (jwks (jwk:keys-to-jwks (list j1 j2))))
    (assert-true (assoc :keys jwks))
    (assert-= 2 (length (cdr (assoc :keys jwks))))))

(deftest test-keys-from-jwks
  "keys-from-jwks extracts key list"
  (let* ((key1 (native:generate-ec-p256-key))
         (j1 (jwk:key-to-jwk key1 :kid "k1"))
         (jwks (jwk:keys-to-jwks (list j1)))
         (keys (jwk:keys-from-jwks jwks)))
    (assert-= 1 (length keys))
    (assert-equal "EC" (cdr (assoc :kty (first keys))))))

;;; ============================================================================
;;; JSON Serialization Tests
;;; ============================================================================

(deftest test-jwk-to-json-produces-string
  "jwk-to-json returns a JSON string"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key :kid "test"))
         (json-str (jwk:jwk-to-json j)))
    (assert-true (stringp json-str))
    (assert-true (search "EC" json-str))
    (assert-true (search "P-256" json-str))))

(deftest test-jwks-to-json-has-keys-array
  "jwks-to-json returns JSON with keys array"
  (let* ((key (native:generate-ec-p256-key))
         (j (jwk:key-to-jwk key))
         (json-str (jwk:jwks-to-json (list j))))
    (assert-true (stringp json-str))
    (assert-true (search "keys" json-str))))
