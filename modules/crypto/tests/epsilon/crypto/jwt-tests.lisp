;;;; JWT Tests

(defpackage epsilon.crypto.tests.jwt
  (:use :cl :epsilon.test)
  (:require (epsilon.crypto.jwt jwt)
            (epsilon.crypto.native native))
  (:enter t))

;;;; Base64url Tests

(deftest test-base64url-encode-no-padding
  "Base64url encoding produces no padding characters"
  (let ((token (jwt:jwt-encode '((:sub . "test")) "secret")))
    (assert-not (position #\= token))
    (assert-not (position #\+ token))
    (assert-not (position #\/ token))))

(deftest test-base64url-roundtrip
  "Base64url encode then decode preserves data"
  (let* ((claims '((:sub . "user-123") (:name . "Alice")))
         (token (jwt:jwt-encode claims "my-secret"))
         (decoded (jwt:jwt-decode token "my-secret")))
    (assert-equal "user-123" (cdr (assoc :sub decoded)))
    (assert-equal "Alice" (cdr (assoc :name decoded)))))

;;;; HS256 Encode/Decode Tests

(deftest test-hs256-roundtrip
  "Encode then decode roundtrip with HS256"
  (let* ((claims '((:sub . "user-456") (:iss . "kreisler") (:role . "admin")))
         (secret "test-secret-key-that-is-long-enough")
         (token (jwt:jwt-encode claims secret :algorithm :hs256))
         (decoded (jwt:jwt-decode token secret :algorithm :hs256)))
    (assert-equal "user-456" (cdr (assoc :sub decoded)))
    (assert-equal "kreisler" (cdr (assoc :iss decoded)))
    (assert-equal "admin" (cdr (assoc :role decoded)))))

(deftest test-hs256-different-secrets-differ
  "Different secrets produce different tokens"
  (let* ((claims '((:sub . "user")))
         (token1 (jwt:jwt-encode claims "secret-1"))
         (token2 (jwt:jwt-encode claims "secret-2")))
    (assert-not (string= token1 token2))))

(deftest test-decode-wrong-secret
  "Decode with wrong secret signals jwt-invalid-signature-error"
  (let ((token (jwt:jwt-encode '((:sub . "user")) "correct-secret")))
    (assert-condition (jwt:jwt-invalid-signature-error)
      (jwt:jwt-decode token "wrong-secret"))))

;;;; Expiration Tests

(deftest test-decode-expired-token
  "Decode token with expired exp signals jwt-expired-error"
  (let* ((past-time (- (get-universal-time) 3600))
         (claims `((:sub . "user") (:exp . ,past-time)))
         (token (jwt:jwt-encode claims "secret")))
    (assert-condition (jwt:jwt-expired-error)
      (jwt:jwt-decode token "secret"))))

(deftest test-decode-valid-exp
  "Decode token with future exp succeeds"
  (let* ((future-time (+ (get-universal-time) 3600))
         (claims `((:sub . "user") (:exp . ,future-time)))
         (token (jwt:jwt-encode claims "secret"))
         (decoded (jwt:jwt-decode token "secret")))
    (assert-equal "user" (cdr (assoc :sub decoded)))))

(deftest test-clock-skew-allows-near-expiry
  "Clock skew parameter allows recently-expired tokens"
  (let* ((past-time (- (get-universal-time) 5))
         (claims `((:sub . "user") (:exp . ,past-time)))
         (token (jwt:jwt-encode claims "secret"))
         (decoded (jwt:jwt-decode token "secret" :clock-skew 60)))
    (assert-equal "user" (cdr (assoc :sub decoded)))))

;;;; Malformed Token Tests

(deftest test-malformed-missing-segments
  "Token with fewer than 3 segments signals jwt-malformed-error"
  (assert-condition (jwt:jwt-malformed-error)
    (jwt:jwt-decode "only.two" "secret")))

(deftest test-malformed-single-segment
  "Single segment signals jwt-malformed-error"
  (assert-condition (jwt:jwt-malformed-error)
    (jwt:jwt-decode "nosegments" "secret")))

(deftest test-malformed-non-string
  "Non-string token signals jwt-malformed-error"
  (assert-condition (jwt:jwt-malformed-error)
    (jwt:jwt-decode 12345 "secret")))

(deftest test-malformed-bad-base64
  "Invalid base64 in payload signals jwt-malformed-error"
  (assert-condition (jwt:jwt-malformed-error)
    (jwt:jwt-decode "eyJhbGciOiJIUzI1NiJ9.!!!invalid!!!.sig" "secret")))

;;;; jwt-decode-unsafe Tests

(deftest test-decode-unsafe-returns-header-and-payload
  "jwt-decode-unsafe returns header and payload without verification"
  (let ((token (jwt:jwt-encode '((:sub . "user-789")) "secret")))
    (multiple-value-bind (header payload)
        (jwt:jwt-decode-unsafe token)
      (assert-equal "HS256" (cdr (assoc :alg header)))
      (assert-equal "JWT" (cdr (assoc :typ header)))
      (assert-equal "user-789" (cdr (assoc :sub payload))))))

(deftest test-decode-unsafe-with-wrong-signature
  "jwt-decode-unsafe succeeds even with wrong signature"
  (let* ((token (jwt:jwt-encode '((:sub . "user")) "secret"))
         ;; Corrupt the signature
         (parts (loop with parts = nil
                      with start = 0
                      for i from 0 to (length token)
                      when (or (= i (length token)) (char= (char token i) #\.))
                      do (push (subseq token start i) parts)
                         (setf start (1+ i))
                      finally (return (nreverse parts))))
         (corrupted (format nil "~A.~A.corrupted" (first parts) (second parts))))
    (multiple-value-bind (header payload)
        (jwt:jwt-decode-unsafe corrupted)
      (assert-equal "user" (cdr (assoc :sub payload)))
      (assert-equal "HS256" (cdr (assoc :alg header))))))

;;;; Claims Handling Tests

(deftest test-claims-keyword-keys
  "Claims are returned with keyword keys"
  (let* ((claims '((:sub . "user") (:iss . "issuer") (:aud . "audience")))
         (token (jwt:jwt-encode claims "secret"))
         (decoded (jwt:jwt-decode token "secret")))
    (assert-true (keywordp (car (first decoded))))
    (assert-true (assoc :sub decoded))
    (assert-true (assoc :iss decoded))
    (assert-true (assoc :aud decoded))))

(deftest test-empty-claims
  "Empty claims encode/decode correctly"
  (let* ((token (jwt:jwt-encode nil "secret"))
         (decoded (jwt:jwt-decode token "secret")))
    (assert-true (null decoded))))

(deftest test-numeric-claims
  "Numeric claim values are preserved"
  (let* ((future-time (+ (get-universal-time) 86400))
         (now (get-universal-time))
         (claims `((:iat . ,now) (:exp . ,future-time) (:nbf . ,now)))
         (token (jwt:jwt-encode claims "secret"))
         (decoded (jwt:jwt-decode token "secret")))
    (assert-true (numberp (cdr (assoc :iat decoded))))
    (assert-true (numberp (cdr (assoc :exp decoded))))))

;;;; Extra Headers Tests

(deftest test-extra-headers
  "Custom header fields via extra-headers"
  (let* ((token (jwt:jwt-encode '((:sub . "user")) "secret"
                                :extra-headers '((:kid . "key-1"))))
         (header (jwt:jwt-decode-unsafe token)))
    (assert-equal "key-1" (cdr (assoc :kid header)))))

;;;; Token Structure Tests

(deftest test-token-has-three-segments
  "Encoded token has exactly three dot-separated segments"
  (let ((token (jwt:jwt-encode '((:sub . "test")) "secret")))
    (assert-= 2 (count #\. token))))

(deftest test-token-header-contains-alg
  "Token header contains alg field"
  (let ((token (jwt:jwt-encode '((:sub . "test")) "secret")))
    (let ((header (jwt:jwt-decode-unsafe token)))
      (assert-true (assoc :alg header)))))

;;;; Decode Without Verification

(deftest test-decode-verify-nil
  "jwt-decode with :verify nil skips signature check"
  (let* ((token (jwt:jwt-encode '((:sub . "user")) "correct-secret"))
         (decoded (jwt:jwt-decode token "wrong-secret" :verify nil)))
    (assert-equal "user" (cdr (assoc :sub decoded)))))

;;;; Condition Hierarchy Tests

(deftest test-condition-hierarchy
  "JWT conditions inherit from jwt-error"
  (assert-true (subtypep 'jwt:jwt-expired-error 'jwt:jwt-error))
  (assert-true (subtypep 'jwt:jwt-invalid-signature-error 'jwt:jwt-error))
  (assert-true (subtypep 'jwt:jwt-malformed-error 'jwt:jwt-error))
  (assert-true (subtypep 'jwt:jwt-error 'error)))

(deftest test-condition-predicates
  "JWT error predicates work correctly"
  (let ((expired (make-condition 'jwt:jwt-expired-error :message "test"))
        (sig-err (make-condition 'jwt:jwt-invalid-signature-error :message "test"))
        (malformed (make-condition 'jwt:jwt-malformed-error :message "test")))
    (assert-true (jwt:jwt-error-p expired))
    (assert-true (jwt:jwt-expired-error-p expired))
    (assert-not (jwt:jwt-malformed-error-p expired))
    (assert-true (jwt:jwt-error-p sig-err))
    (assert-true (jwt:jwt-invalid-signature-error-p sig-err))
    (assert-true (jwt:jwt-error-p malformed))
    (assert-true (jwt:jwt-malformed-error-p malformed))))

;;;; Cross-validation with known tokens

(deftest test-deterministic-header
  "Header for HS256 JWT always encodes the same way"
  (let* ((token1 (jwt:jwt-encode '((:sub . "a")) "key"))
         (token2 (jwt:jwt-encode '((:sub . "b")) "key"))
         (header1 (subseq token1 0 (position #\. token1)))
         (header2 (subseq token2 0 (position #\. token2))))
    ;; Same algorithm -> same header encoding
    (assert-equal header1 header2)))

;;;; ES256 Tests

(deftest test-es256-roundtrip
  "Encode then decode roundtrip with ES256 (sign with private, verify with private)"
  (let* ((key (native:generate-ec-p256-key))
         (claims '((:sub . "user-es256") (:iss . "kreisler")))
         (token (jwt:jwt-encode claims key :algorithm :es256))
         (decoded (jwt:jwt-decode token key :algorithm :es256)))
    (assert-equal "user-es256" (cdr (assoc :sub decoded)))
    (assert-equal "kreisler" (cdr (assoc :iss decoded)))))

(deftest test-es256-header-contains-es256
  "ES256 token header contains alg=ES256"
  (let* ((key (native:generate-ec-p256-key))
         (token (jwt:jwt-encode '((:sub . "test")) key :algorithm :es256))
         (header (jwt:jwt-decode-unsafe token)))
    (assert-equal "ES256" (cdr (assoc :alg header)))))

(deftest test-es256-signature-is-64-bytes
  "ES256 signature is exactly 64 bytes (r||s, 32 each)"
  (let* ((key (native:generate-ec-p256-key))
         (token (jwt:jwt-encode '((:sub . "test")) key :algorithm :es256))
         ;; Extract signature part
         (parts (loop with parts = nil
                      with start = 0
                      for i from 0 to (length token)
                      when (or (= i (length token)) (char= (char token i) #\.))
                      do (push (subseq token start i) parts)
                         (setf start (1+ i))
                      finally (return (nreverse parts))))
         (sig-bytes (epsilon.base-encode:base64-decode-url (third parts))))
    (assert-= 64 (length sig-bytes))))

(deftest test-es256-different-keys-differ
  "Different EC keys produce different tokens"
  (let* ((key1 (native:generate-ec-p256-key))
         (key2 (native:generate-ec-p256-key))
         (claims '((:sub . "user")))
         (token1 (jwt:jwt-encode claims key1 :algorithm :es256))
         (token2 (jwt:jwt-encode claims key2 :algorithm :es256)))
    (assert-not (string= token1 token2))))

(deftest test-es256-wrong-key-fails
  "Decode with wrong EC key signals jwt-invalid-signature-error"
  (let* ((key1 (native:generate-ec-p256-key))
         (key2 (native:generate-ec-p256-key))
         (token (jwt:jwt-encode '((:sub . "user")) key1 :algorithm :es256)))
    (assert-condition (jwt:jwt-invalid-signature-error)
      (jwt:jwt-decode token key2 :algorithm :es256))))

(deftest test-es256-verify-with-public-key
  "Sign with private key, verify with derived public key"
  (let* ((private-key (native:generate-ec-p256-key))
         (pem (native:key-to-pem private-key))
         (public-key (native:key-from-pem pem))
         (claims '((:sub . "user-pub") (:iss . "kreisler")))
         (token (jwt:jwt-encode claims private-key :algorithm :es256))
         (decoded (jwt:jwt-decode token public-key :algorithm :es256)))
    (assert-equal "user-pub" (cdr (assoc :sub decoded)))))

(deftest test-es256-with-extra-headers
  "ES256 token supports extra headers like kid"
  (let* ((key (native:generate-ec-p256-key))
         (token (jwt:jwt-encode '((:sub . "user")) key
                                :algorithm :es256
                                :extra-headers '((:kid . "key-2024"))))
         (header (jwt:jwt-decode-unsafe token)))
    (assert-equal "ES256" (cdr (assoc :alg header)))
    (assert-equal "key-2024" (cdr (assoc :kid header)))))

(deftest test-es256-expiration
  "ES256 token with expired exp signals jwt-expired-error"
  (let* ((key (native:generate-ec-p256-key))
         (past-time (- (get-universal-time) 3600))
         (claims `((:sub . "user") (:exp . ,past-time)))
         (token (jwt:jwt-encode claims key :algorithm :es256)))
    (assert-condition (jwt:jwt-expired-error)
      (jwt:jwt-decode token key :algorithm :es256))))

(deftest test-parse-algorithm-es256
  "ES256 algorithm string parses correctly"
  (let* ((key (native:generate-ec-p256-key))
         (token (jwt:jwt-encode '((:sub . "test")) key :algorithm :es256))
         (decoded (jwt:jwt-decode token key :algorithm :es256)))
    (assert-true decoded)))
