;;;; Tests for X.509 certificate parsing and validation

(defpackage epsilon.ssl.x509-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:x509 #:epsilon.ssl.x509)
   (#:pem #:epsilon.ssl.pem)
   (#:pkcs #:epsilon.ssl.pkcs)
   (#:asn1 #:epsilon.ssl.asn1)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ec-p256 #:epsilon.ssl.ec-p256)
   (#:drbg #:epsilon.ssl.drbg))
  (:enter t))

(in-package :epsilon.ssl.x509-tests)

(defun random-ecdsa-private-key ()
  "Generate a random ECDSA P-256 private key (integer)."
  (let ((bytes (drbg:random-bytes 32)))
    (loop with n = 0
          for b across bytes
          do (setf n (logior (ash n 8) b))
          finally (return n))))

;;; ---------------------------------------------------------------------------
;;; Time tests
;;; ---------------------------------------------------------------------------

(deftest test-x509-time-comparison
  "x509-time comparison"
  ;; Same time
  (let ((t1 (x509:make-x509-time :year 2025 :month 3 :day 15))
        (t2 (x509:make-x509-time :year 2025 :month 3 :day 15)))
    (assert-not (x509:x509-time-before-p t1 t2))
    (assert-not (x509:x509-time-before-p t2 t1)))
  ;; Different years
  (let ((t1 (x509:make-x509-time :year 2024 :month 12 :day 31))
        (t2 (x509:make-x509-time :year 2025 :month 1 :day 1)))
    (assert-true (x509:x509-time-before-p t1 t2))
    (assert-not (x509:x509-time-before-p t2 t1)))
  ;; Different months
  (let ((t1 (x509:make-x509-time :year 2025 :month 1 :day 31))
        (t2 (x509:make-x509-time :year 2025 :month 2 :day 1)))
    (assert-true (x509:x509-time-before-p t1 t2)))
  ;; Different seconds
  (let ((t1 (x509:make-x509-time :year 2025 :month 3 :day 15 :hour 12 :minute 30 :second 0))
        (t2 (x509:make-x509-time :year 2025 :month 3 :day 15 :hour 12 :minute 30 :second 1)))
    (assert-true (x509:x509-time-before-p t1 t2))))

(deftest test-x509-time-now
  "x509-time-now returns a reasonable time"
  (let ((now (x509:x509-time-now)))
    (assert-true (>= (x509:x509-time-year now) 2025))
    (assert-true (<= 1 (x509:x509-time-month now) 12))
    (assert-true (<= 1 (x509:x509-time-day now) 31))))

;;; ---------------------------------------------------------------------------
;;; Distinguished Name tests
;;; ---------------------------------------------------------------------------

(deftest test-x509-name-common-name
  "x509-name common name extraction"
  (let ((name (x509:make-x509-name
               :entries (list (cons '(2 5 4 10) "Example Inc")
                             (cons '(2 5 4 3) "example.com")))))
    (assert-equal (x509:x509-name-common-name name) "example.com")))

(deftest test-x509-name-to-string
  "x509-name formatting"
  (let ((name (x509:make-x509-name
               :entries (list (cons '(2 5 4 3) "example.com")
                             (cons '(2 5 4 10) "Example Inc")))))
    (let ((str (x509:x509-name-to-string name)))
      (assert-true (search "CN=example.com" str))
      (assert-true (search "O=Example Inc" str)))))

;;; ---------------------------------------------------------------------------
;;; Hostname matching tests (RFC 6125)
;;; ---------------------------------------------------------------------------

(deftest test-hostname-exact-match
  "Hostname exact match"
  (assert-true (x509::hostname-matches-pattern-p "example.com" "example.com"))
  (assert-true (x509::hostname-matches-pattern-p "EXAMPLE.COM" "example.com"))
  (assert-not (x509::hostname-matches-pattern-p "example.com" "other.com")))

(deftest test-hostname-wildcard-match
  "Hostname wildcard matching"
  ;; Basic wildcard
  (assert-true (x509::hostname-matches-pattern-p "foo.example.com" "*.example.com"))
  (assert-true (x509::hostname-matches-pattern-p "bar.example.com" "*.example.com"))
  ;; Wildcard must not match bare domain
  (assert-not (x509::hostname-matches-pattern-p "example.com" "*.example.com"))
  ;; Wildcard must not span dots
  (assert-not (x509::hostname-matches-pattern-p "a.b.example.com" "*.example.com"))
  ;; Wildcard must have at least 2 labels in suffix
  (assert-not (x509::hostname-matches-pattern-p "foo.com" "*.com")))

(deftest test-hostname-case-insensitive
  "Hostname matching is case-insensitive"
  (assert-true (x509::hostname-matches-pattern-p "Foo.Example.COM" "*.example.com"))
  (assert-true (x509::hostname-matches-pattern-p "EXAMPLE.COM" "Example.Com")))

;;; ---------------------------------------------------------------------------
;;; Self-signed certificate generation and parsing (Ed25519)
;;; ---------------------------------------------------------------------------

(deftest test-self-signed-ed25519-roundtrip
  "Generate and parse a self-signed Ed25519 certificate"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (not-before (x509:make-x509-time :year 2025 :month 1 :day 1))
         (not-after (x509:make-x509-time :year 2026 :month 1 :day 1))
         (cert-der (x509:make-self-signed-certificate
                    :subject "test.example.com"
                    :serial 1
                    :not-before not-before
                    :not-after not-after
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :dns-names '("test.example.com" "*.example.com")))
         (cert (x509:parse-x509-certificate cert-der)))
    ;; Check parsed fields
    (assert-= (x509:x509-cert-version cert) 3)
    (assert-= (x509:x509-cert-serial cert) 1)
    (assert-equal (x509:x509-name-common-name (x509:x509-cert-subject cert))
                  "test.example.com")
    ;; Issuer = subject (self-signed)
    (assert-equal (x509:x509-name-common-name (x509:x509-cert-issuer cert))
                  "test.example.com")
    ;; Time
    (assert-= (x509:x509-time-year (x509:x509-cert-not-before cert)) 2025)
    (assert-= (x509:x509-time-year (x509:x509-cert-not-after cert)) 2026)
    ;; Public key algorithm
    (assert-equal (x509:x509-cert-public-key-algorithm cert) pkcs:+oid-ed25519+)
    ;; Public key bytes match
    (assert-equalp (x509:x509-cert-public-key-bytes cert) pk)))

(deftest test-self-signed-ed25519-signature-verify
  "Verify self-signed Ed25519 certificate signature"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :subject "verify.example.com"
                    :serial 42
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk))
         (cert (x509:parse-x509-certificate cert-der)))
    ;; Self-signed: verify against itself
    (assert-true (x509:verify-certificate-signature cert cert))))

;;; ---------------------------------------------------------------------------
;;; Self-signed ECDSA certificate
;;; ---------------------------------------------------------------------------

(deftest test-self-signed-ecdsa-roundtrip
  "Generate and parse a self-signed ECDSA certificate"
  (let* ((sk (random-ecdsa-private-key))
         (pk-point (ecdsa:ecdsa-public-key-from-private sk))
         (pk-bytes (ec-p256:p256-point-encode-uncompressed pk-point))
         (cert-der (x509:make-self-signed-certificate
                    :subject "ecdsa.example.com"
                    :serial 2
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ecdsa-p256
                    :private-key sk
                    :public-key-bytes pk-bytes
                    :dns-names '("ecdsa.example.com")))
         (cert (x509:parse-x509-certificate cert-der)))
    (assert-= (x509:x509-cert-version cert) 3)
    (assert-equal (x509:x509-name-common-name (x509:x509-cert-subject cert))
                  "ecdsa.example.com")
    (assert-equal (x509:x509-cert-public-key-algorithm cert) pkcs:+oid-ec-public-key+)))

(deftest test-self-signed-ecdsa-signature-verify
  "Verify self-signed ECDSA certificate signature"
  (let* ((sk (random-ecdsa-private-key))
         (pk-point (ecdsa:ecdsa-public-key-from-private sk))
         (pk-bytes (ec-p256:p256-point-encode-uncompressed pk-point))
         (cert-der (x509:make-self-signed-certificate
                    :subject "verify-ecdsa.example.com"
                    :serial 3
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ecdsa-p256
                    :private-key sk
                    :public-key-bytes pk-bytes))
         (cert (x509:parse-x509-certificate cert-der)))
    (assert-true (x509:verify-certificate-signature cert cert))))

;;; ---------------------------------------------------------------------------
;;; Certificate chain validation
;;; ---------------------------------------------------------------------------

(deftest test-certificate-chain-ed25519
  "Verify a 2-certificate chain (CA -> leaf) with Ed25519"
  (let* (;; CA key pair
         (ca-sk (drbg:random-bytes 32))
         (ca-pk (ed-sign:ed25519-public-key-from-private ca-sk))
         ;; CA cert
         (ca-der (x509:make-self-signed-certificate
                  :subject "Test CA"
                  :serial 1
                  :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                  :not-after (x509:make-x509-time :year 2030 :month 1 :day 1)
                  :key-type :ed25519
                  :private-key ca-sk
                  :public-key-bytes ca-pk
                  :is-ca t))
         (ca-cert (x509:parse-x509-certificate ca-der))
         ;; Leaf key pair
         (leaf-sk (drbg:random-bytes 32))
         (leaf-pk (ed-sign:ed25519-public-key-from-private leaf-sk))
         ;; Leaf cert signed by CA
         (leaf-der (x509:make-ca-signed-certificate
                    :subject "leaf.example.com"
                    :serial 100
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ed25519
                    :public-key-bytes leaf-pk
                    :ca-private-key ca-sk
                    :ca-key-type :ed25519
                    :ca-cert ca-cert
                    :dns-names '("leaf.example.com")))
         (leaf-cert (x509:parse-x509-certificate leaf-der)))
    ;; Verify leaf -> CA chain
    (assert-true (x509:verify-certificate-signature leaf-cert ca-cert))
    ;; Verify full chain
    (multiple-value-bind (valid reason)
        (x509:verify-certificate-chain
         (list leaf-cert ca-cert)
         :verify-time nil)
      (declare (ignore reason))
      (assert-true valid))))

;;; ---------------------------------------------------------------------------
;;; Hostname verification with certificates
;;; ---------------------------------------------------------------------------

(deftest test-hostname-verification-with-san
  "Hostname verification uses SAN extension"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :subject "example.com"
                    :serial 1
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :dns-names '("example.com" "www.example.com" "*.cdn.example.com")))
         (cert (x509:parse-x509-certificate cert-der)))
    ;; Exact SAN match
    (assert-true (x509:hostname-matches-p cert "example.com"))
    (assert-true (x509:hostname-matches-p cert "www.example.com"))
    ;; Wildcard SAN match
    (assert-true (x509:hostname-matches-p cert "img.cdn.example.com"))
    ;; Non-matching
    (assert-not (x509:hostname-matches-p cert "other.example.com"))))

;;; ---------------------------------------------------------------------------
;;; Trust store
;;; ---------------------------------------------------------------------------

(deftest test-trust-store-basic
  "Trust store add and lookup"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :subject "Root CA"
                    :serial 1
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2035 :month 1 :day 1)
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :is-ca t))
         (cert (x509:parse-x509-certificate cert-der))
         (store (x509:make-trust-store)))
    (x509:trust-store-add store cert)
    (assert-= (length (x509:trust-store-certificates store)) 1)))

;;; ---------------------------------------------------------------------------
;;; PEM certificate parsing
;;; ---------------------------------------------------------------------------

(deftest test-pem-certificate-roundtrip
  "Generate certificate, encode as PEM, parse back"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :subject "pem.example.com"
                    :serial 99
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2026 :month 1 :day 1)
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk))
         ;; Encode as PEM
         (pem-text (pem:pem-encode (pem:make-pem-block "CERTIFICATE" cert-der)))
         ;; Parse back
         (cert (x509:parse-x509-pem pem-text)))
    (assert-true cert)
    (assert-= (x509:x509-cert-serial cert) 99)
    (assert-equal (x509:x509-name-common-name (x509:x509-cert-subject cert))
                  "pem.example.com")))

;;; ---------------------------------------------------------------------------
;;; Certificate time validation
;;; ---------------------------------------------------------------------------

(deftest test-certificate-chain-time-validation
  "Certificate chain rejects expired certificates"
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         ;; Expired certificate
         (cert-der (x509:make-self-signed-certificate
                    :subject "expired.example.com"
                    :serial 1
                    :not-before (x509:make-x509-time :year 2020 :month 1 :day 1)
                    :not-after (x509:make-x509-time :year 2021 :month 1 :day 1)
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk))
         (cert (x509:parse-x509-certificate cert-der)))
    ;; Should fail with current time
    (multiple-value-bind (valid reason)
        (x509:verify-certificate-chain (list cert))
      (assert-not valid)
      (assert-true (search "expired" reason)))))
