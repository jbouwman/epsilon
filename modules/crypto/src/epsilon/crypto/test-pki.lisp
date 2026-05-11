;;;; Test PKI helpers
;;;;
;;;; Generate complete certificate chains (root CA + optional intermediate
;;;; + leaf) for tests that need real, signed certificate material:
;;;; client-auth/mTLS, intermediate-CA path validation, hostname
;;;; verification through SAN, etc.
;;;;
;;;; Lives in epsilon.crypto (rather than under tests/) so tests in any
;;;; consumer module (proxy, cert-manager, hemidemi-server, ...) can
;;;; (:import) it without copying boilerplate. Runtime cost: a fresh
;;;; key generation per call, so callers should hoist into a fixture
;;;; if they need many tests against the same chain.

(defpackage epsilon.crypto.test-pki
  (:use :cl)
  (:import
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.ed25519-sign ed25519)
   (epsilon.crypto.ec-p256 ec)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.rsa rsa))
  (:export
   ;; Test-key abstraction
   #:test-key
   #:test-key-type
   #:test-key-private-key
   #:test-key-public-key-bytes
   #:generate-test-key
   ;; Chain builder
   #:test-chain
   #:test-chain-root-cert
   #:test-chain-root-key
   #:test-chain-intermediate-cert
   #:test-chain-intermediate-key
   #:test-chain-leaf-cert
   #:test-chain-leaf-key
   #:make-test-chain
   #:test-chain-p))

(in-package :epsilon.crypto.test-pki)

(defstruct test-key
  "A generated keypair in the form `make-self-signed-certificate' /
   `make-ca-signed-certificate' expect:
     PRIVATE-KEY      -- raw private material for signing (32-byte seed
                         for Ed25519, integer scalar for EC P-256,
                         rsa-private-key struct for RSA).
     PUBLIC-KEY-BYTES -- raw bytes for the SPKI BIT STRING (32-byte point
                         for Ed25519, 65-byte SEC1 point for EC P-256,
                         DER-encoded RSAPublicKey for RSA)."
  (type nil :type (member :ed25519 :ecdsa-p256 :rsa))
  private-key
  public-key-bytes)

(defun %rsa-spki-bytes (rsa-priv)
  "DER-encode the RFC 8017 RSAPublicKey { modulus, publicExponent }
   so it can be dropped into a SubjectPublicKeyInfo BIT STRING."
  (asn1:der-encode-sequence
   (asn1:der-encode-integer (rsa:rsa-private-key-n rsa-priv))
   (asn1:der-encode-integer (rsa:rsa-private-key-e rsa-priv))))

(defun %random-ec-p256-scalar ()
  "Sample a uniformly random non-zero scalar mod the EC P-256 group order."
  (loop
    (let* ((bytes (drbg:random-bytes 32))
           (n 0))
      (loop for b across bytes do (setf n (logior (ash n 8) b)))
      (let ((d (mod n ec:+n+)))
        (unless (zerop d) (return d))))))

(defun generate-test-key (key-type &key (rsa-bits 2048))
  "Generate a fresh keypair of the requested type for use in test certs.
   KEY-TYPE: :ed25519, :ecdsa-p256, or :rsa.
   RSA-BITS controls modulus size for :rsa (defaults to 2048; tests can
   shrink this to e.g. 1024 to trade safety for speed)."
  (ecase key-type
    (:ed25519
     (let* ((seed (drbg:random-bytes 32))
            (pub (ed25519:ed25519-public-key-from-private seed)))
       (make-test-key :type :ed25519 :private-key seed :public-key-bytes pub)))
    (:ecdsa-p256
     (let* ((d (%random-ec-p256-scalar))
            (point (ecdsa:ecdsa-public-key-from-private d))
            (pub-bytes (ec:p256-point-encode-uncompressed point)))
       (make-test-key :type :ecdsa-p256 :private-key d
                      :public-key-bytes pub-bytes)))
    (:rsa
     (multiple-value-bind (pub priv) (rsa:rsa-generate-key rsa-bits)
       (declare (ignore pub))
       (make-test-key :type :rsa :private-key priv
                      :public-key-bytes (%rsa-spki-bytes priv))))))

(defstruct (test-chain (:constructor %make-test-chain))
  "A bundle of signed certificates and keys for tests. INTERMEDIATE-* are
   nil when the chain has no intermediate (root signs leaf directly)."
  root-cert        ; DER bytes
  root-key         ; test-key
  intermediate-cert
  intermediate-key
  leaf-cert
  leaf-key)

(defun %default-not-after ()
  (let ((now (x509:x509-time-now)))
    (x509:make-x509-time
     :year (+ (x509:x509-time-year now) 1)
     :month (x509:x509-time-month now)
     :day (x509:x509-time-day now))))

(defun make-test-chain (&key
                          (root-key-type :ed25519)
                          (intermediate-key-type nil)
                          (leaf-key-type :ed25519)
                          (root-subject "Test Root CA")
                          (intermediate-subject "Test Intermediate CA")
                          (leaf-subject "leaf.test.example")
                          (leaf-dns-names '("leaf.test.example"))
                          (not-before (x509:x509-time-now))
                          (not-after (%default-not-after))
                          (rsa-bits 2048))
  "Build a complete test certificate chain.
   When INTERMEDIATE-KEY-TYPE is non-NIL, the chain is
     root -> intermediate -> leaf
   otherwise it is
     root -> leaf

   All certs in the returned `test-chain' are DER bytes; the keys are
   `test-key' structs. The leaf has both digitalSignature key usage and
   serverAuth EKU, so it is acceptable to current TLS clients.

   The default NOT-AFTER is one year out, which matches typical TLS
   server-cert lifetimes and avoids time-validation surprises in tests
   that run far into the future."
  (let* ((root-key (generate-test-key root-key-type :rsa-bits rsa-bits))
         (root-cert (x509:make-self-signed-certificate
                     :subject root-subject
                     :serial 1
                     :not-before not-before
                     :not-after not-after
                     :key-type (test-key-type root-key)
                     :private-key (test-key-private-key root-key)
                     :public-key-bytes (test-key-public-key-bytes root-key)
                     :is-ca t))
         (root-cert-parsed (x509:parse-x509-certificate root-cert)))
    (cond
      ;; root -> intermediate -> leaf
      (intermediate-key-type
       (let* ((int-key (generate-test-key intermediate-key-type
                                          :rsa-bits rsa-bits))
              (int-cert (x509:make-ca-signed-certificate
                         :subject intermediate-subject
                         :serial 2
                         :not-before not-before
                         :not-after not-after
                         :key-type (test-key-type int-key)
                         :public-key-bytes (test-key-public-key-bytes int-key)
                         :ca-private-key (test-key-private-key root-key)
                         :ca-key-type (test-key-type root-key)
                         :ca-cert root-cert-parsed
                         :is-ca t))
              (int-cert-parsed (x509:parse-x509-certificate int-cert))
              (leaf-key (generate-test-key leaf-key-type :rsa-bits rsa-bits))
              (leaf-cert (x509:make-ca-signed-certificate
                          :subject leaf-subject
                          :serial 3
                          :not-before not-before
                          :not-after not-after
                          :key-type (test-key-type leaf-key)
                          :public-key-bytes (test-key-public-key-bytes leaf-key)
                          :ca-private-key (test-key-private-key int-key)
                          :ca-key-type (test-key-type int-key)
                          :ca-cert int-cert-parsed
                          :dns-names leaf-dns-names
                          :key-usage '(:digital-signature)
                          :extended-key-usage '(:server-auth))))
         (%make-test-chain :root-cert root-cert
                          :root-key root-key
                          :intermediate-cert int-cert
                          :intermediate-key int-key
                          :leaf-cert leaf-cert
                          :leaf-key leaf-key)))
      ;; root -> leaf
      (t
       (let* ((leaf-key (generate-test-key leaf-key-type :rsa-bits rsa-bits))
              (leaf-cert (x509:make-ca-signed-certificate
                          :subject leaf-subject
                          :serial 2
                          :not-before not-before
                          :not-after not-after
                          :key-type (test-key-type leaf-key)
                          :public-key-bytes (test-key-public-key-bytes leaf-key)
                          :ca-private-key (test-key-private-key root-key)
                          :ca-key-type (test-key-type root-key)
                          :ca-cert root-cert-parsed
                          :dns-names leaf-dns-names
                          :key-usage '(:digital-signature)
                          :extended-key-usage '(:server-auth))))
         (%make-test-chain :root-cert root-cert
                          :root-key root-key
                          :leaf-cert leaf-cert
                          :leaf-key leaf-key))))))
