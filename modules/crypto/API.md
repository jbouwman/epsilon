# Epsilon.Crypto API Documentation

## Overview

The `epsilon.crypto` module provides  cryptographic functionality including:
- Public key cryptography (RSA, EC, Ed25519)
- TLS/SSL client and server operations
- X.509 certificate management
- Digital signatures and verification
- Encryption and decryption
- Cryptographic hash functions
- Secure random number generation

This module uses OpenSSL 3.0+ as its underlying cryptographic provider.

## Security Guidelines

### General Principles

1. **Key Size Requirements**
   - RSA: Minimum 2048 bits, recommended 3072 bits
   - EC: Minimum P-256 curve, recommended P-384
   - Always use cryptographically secure random generation

2. **Certificate Validation**
   - Always verify certificates in production (`+TLS-VERIFY-PEER+`)
   - Check certificate expiration dates
   - Validate certificate chains to trusted roots
   - Consider certificate pinning for high-security applications

3. **Private Key Protection**
   - Store private keys with restricted permissions (0600)
   - Consider hardware security modules (HSM) for production keys
   - Clear key material from memory promptly after use
   - Never log or transmit private key material

4. **Algorithm Selection**
   - Prefer modern algorithms (Ed25519, AES-GCM, SHA-256+)
   - Avoid deprecated algorithms (MD5, SHA-1, DES)
   - Use appropriate algorithms for use case (signing vs encryption)

## Key Generation

### RSA Keys

```lisp
;; Generate 2048-bit RSA key (default)
(crypto:generate-rsa-key)

;; Generate 3072-bit RSA key (recommended)
(crypto:generate-rsa-key :bits 3072)

;; Generate 4096-bit RSA key (high security)
(crypto:generate-rsa-key :bits 4096)
```

**Security Notes:**
- 2048-bit keys provide ~112 bits of security (acceptable until ~2030)
- 3072-bit keys provide ~128 bits of security (recommended)
- 4096-bit keys provide ~152 bits of security (future-proof)

**Performance:**
- 2048-bit: ~50-200ms generation
- 3072-bit: ~200-800ms generation
- 4096-bit: ~500-2000ms generation

### Elliptic Curve Keys

```lisp
;; Generate P-256 key (default)
(crypto:generate-ec-key)

;; Generate P-384 key (higher security)
(crypto:generate-ec-key :curve :p384)

;; Generate P-521 key (maximum security)
(crypto:generate-ec-key :curve :p521)

;; Generate secp256k1 key (Bitcoin/Ethereum)
(crypto:generate-ec-key :curve :secp256k1)
```

**Security Notes:**
- P-256: ~128-bit security (standard choice)
- P-384: ~192-bit security (high security)
- P-521: ~256-bit security (maximum security)
- All EC algorithms vulnerable to quantum attacks

### Ed25519 Keys

```lisp
;; Generate Ed25519 key for signatures
(crypto:generate-ed25519-key)
```

**Security Notes:**
- ~128-bit security level
- Deterministic signatures (reproducible)
- Built-in side-channel resistance
- Recommended for new applications

## TLS/SSL Operations

### Creating TLS Contexts

```lisp
;; Client context with certificate validation
(crypto:create-tls-context 
  :server-p nil
  :verify-mode crypto:+tls-verify-peer+)

;; Server context with certificate and key
(crypto:create-tls-context
  :server-p t
  :cert-file "/path/to/server.crt"
  :key-file "/path/to/server.key")

;; Server with mutual TLS (client certificates required)
(crypto:create-openssl-context
  :server-p t
  :cert-file "/path/to/server.crt"
  :key-file "/path/to/server.key"
  :ca-file "/path/to/client-ca.crt"
  :require-client-cert t)
```

### TLS Connection Management

```lisp
;; Client connection
(let* ((ctx (crypto:create-tls-context :server-p nil))
       (socket (net:tcp-connect "example.com" 443))
       (conn (crypto:make-tls-connection :socket socket :context ctx)))
  (crypto:tls-connect conn)
  ;; Use connection...
  (crypto:tls-close conn))

;; Server connection
(let* ((ctx (crypto:create-tls-context 
             :server-p t
             :cert-file "server.crt"
             :key-file "server.key"))
       (listener (net:tcp-bind "::" 8443))
       (socket (net:tcp-accept listener))
       (conn (crypto:make-tls-connection :socket socket :context ctx)))
  (crypto:tls-accept conn)
  ;; Handle client...
  (crypto:tls-close conn))
```

## Digital Signatures

### Signing Messages

```lisp
;; Generate key pair
(let ((key (crypto:generate-rsa-key :bits 3072)))
  ;; Sign message
  (let ((signature (crypto:sign-message "Hello, World!" key)))
    ;; Verify signature
    (crypto:verify-message "Hello, World!" signature 
                          (crypto:derive-public-key key))))
```

### Signature Algorithms

- RSA with SHA-256/384/512 (PKCS#1 v1.5)
- RSA-PSS with SHA-256/384/512 (recommended)
- ECDSA with SHA-256/384/512
- Ed25519 (EdDSA)

## X.509 Certificates

### Creating Self-Signed Certificates

```lisp
(let* ((key (crypto:generate-rsa-key :bits 3072))
       (subject-info (map:make-map
                      "CN" "example.com"
                      "O" "Example Corp"
                      "C" "US"))
       (cert (crypto:create-certificate 
              :subject-key key
              :issuer-key key  ; self-signed
              :subject subject-info
              :days 365)))
  ;; Save certificate and key
  (crypto:save-certificate cert "cert.pem")
  (crypto:save-private-key key "key.pem"))
```

### Certificate Validation

```lisp
;; Load and verify certificate
(let ((cert (crypto:load-certificate "cert.pem"))
      (ca-cert (crypto:load-certificate "ca.pem")))
  (crypto:verify-certificate cert ca-cert))
```

## Key Management

### Key Import/Export

```lisp
;; Export to PEM format
(let ((key (crypto:generate-rsa-key)))
  ;; Export public key
  (crypto:key-to-pem key :private-p nil)
  ;; Export private key
  (crypto:key-to-pem key :private-p t))

;; Import from PEM
(let ((key (crypto:key-from-pem pem-string)))
  ;; Use imported key...
  )

;; DER format (binary)
(crypto:key-to-der key)
(crypto:key-from-der der-bytes)
```

### Secure Key Storage

1. **File Permissions**
   ```bash
   chmod 600 private-key.pem  # Owner read/write only
   chmod 644 certificate.pem  # Public certificate readable
   ```

2. **Encrypted Storage**
   ```lisp
   ;; Save with password encryption (future feature)
   (crypto:save-private-key key "key.pem" 
                           :password "strong-password"
                           :cipher "AES-256-CBC")
   ```

## Random Number Generation

```lisp
;; Generate random bytes
(crypto:crypto-random-bytes 32)  ; 32 random bytes

;; Generate random integer
(crypto:crypto-random-integer 1000000)  ; Random integer 0-999999
```

**Security Notes:**
- Uses OpenSSL's cryptographically secure RNG
- Automatically seeded from system entropy
- Safe for key generation and nonces

## Error Handling

```lisp
(handler-case
    (crypto:generate-rsa-key :bits 1024)  ; Invalid size
  (crypto:crypto-error (e)
    (format t "Crypto error ~A: ~A~%" 
            (crypto:crypto-error-code e)
            (crypto:crypto-error-string e))))
```

## Performance Considerations

### Key Generation Benchmarks

| Operation | Time | Notes |
|-----------|------|-------|
| RSA 2048 | 50-200ms | Standard security |
| RSA 3072 | 200-800ms | Recommended |
| RSA 4096 | 500-2000ms | High security |
| EC P-256 | 1-5ms | Fast, standard |
| EC P-384 | 2-8ms | Higher security |
| Ed25519 | 0.1-1ms | Very fast |

### Operation Performance

| Operation | Time | Notes |
|-----------|------|-------|
| RSA Sign (2048) | 2-10ms | Slower |
| RSA Verify (2048) | 0.1-0.5ms | Fast |
| ECDSA Sign (P-256) | 0.5-2ms | Fast |
| ECDSA Verify (P-256) | 1-3ms | Fast |
| Ed25519 Sign | 10-50μs | Very fast |
| Ed25519 Verify | 20-100μs | Very fast |

## Common Patterns

### TLS Client with Certificate Validation

```lisp
(defun secure-https-request (host path)
  (let* ((ctx (crypto:create-tls-context 
               :server-p nil
               :verify-mode crypto:+tls-verify-peer+
               :ca-file "/etc/ssl/certs/ca-certificates.crt"))
         (socket (net:tcp-connect host 443))
         (conn (crypto:make-tls-connection 
                :socket socket 
                :context ctx)))
    (unwind-protect
        (progn
          (crypto:tls-connect conn)
          ;; Verify hostname matches certificate
          (let ((peer-cert (crypto:get-peer-certificate conn)))
            (unless (crypto:verify-hostname peer-cert host)
              (error "Certificate hostname mismatch")))
          ;; Send HTTPS request
          (crypto:tls-write conn 
            (format nil "GET ~A HTTP/1.1~%Host: ~A~%~%" path host))
          ;; Read response
          (crypto:tls-read conn))
      (crypto:tls-close conn))))
```

### Mutual TLS Server

```lisp
(defun start-mutual-tls-server (port)
  (let* ((ctx (crypto:create-openssl-context
               :server-p t
               :cert-file "server.crt"
               :key-file "server.key"
               :ca-file "client-ca.crt"
               :require-client-cert t
               :verify-depth 4))
         (listener (net:tcp-bind "::" port)))
    (loop
      (let* ((socket (net:tcp-accept listener))
             (conn (crypto:make-tls-connection 
                    :socket socket 
                    :context ctx)))
        (handler-case
            (progn
              (crypto:tls-accept conn)
              ;; Get client certificate
              (let ((client-cert (crypto:get-peer-certificate conn)))
                (format t "Client: ~A~%" 
                        (crypto:x509-certificate-subject client-cert)))
              ;; Handle client...
              )
          (error (e)
            (format t "TLS error: ~A~%" e))
          (finally
            (crypto:tls-close conn)))))))
```

## Migration from Other Libraries

### From cl-openssl

```lisp
;; Old (cl-openssl)
(cl-openssl:generate-rsa-key 2048)

;; New (epsilon.crypto)
(crypto:generate-rsa-key :bits 2048)

;; Old (cl-openssl)
(cl-openssl:sign message key :digest :sha256)

;; New (epsilon.crypto)
(crypto:sign-message message key :algorithm crypto:+digest-sha256+)
```

### From ironclad

```lisp
;; Old (ironclad)
(ironclad:generate-key-pair :rsa :size 2048)

;; New (epsilon.crypto)
(crypto:generate-rsa-key :bits 2048)

;; Old (ironclad)
(ironclad:digest-sequence :sha256 data)

;; New (epsilon.crypto)
(crypto:digest data :algorithm crypto:+digest-sha256+)
```

## Future Enhancements

### Planned Features

1. **Symmetric Encryption**
   - AES-GCM authenticated encryption
   - ChaCha20-Poly1305
   - Key wrapping

2. **Key Derivation**
   - PBKDF2
   - Argon2
   - HKDF

3. **Advanced Protocols**
   - ECDH key exchange
   - Certificate transparency
   - OCSP stapling

4. **Post-Quantum Cryptography**
   - Lattice-based signatures
   - Code-based encryption
   - Hybrid classical/quantum schemes

## Security Checklist

- [ ] Use minimum 2048-bit RSA or P-256 EC keys
- [ ] Enable certificate verification in TLS (`+TLS-VERIFY-PEER+`)
- [ ] Validate certificate hostnames
- [ ] Check certificate expiration
- [ ] Protect private keys with proper file permissions
- [ ] Use secure random generation for all nonces/IVs
- [ ] Clear sensitive data from memory after use
- [ ] Log security events without exposing sensitive data
- [ ] Implement rate limiting for cryptographic operations
- [ ] Regular security updates of OpenSSL library

## Support and Resources

- OpenSSL Documentation: https://www.openssl.org/docs/
- NIST Cryptographic Standards: https://csrc.nist.gov/publications
- RFC 5246 (TLS 1.2): https://tools.ietf.org/html/rfc5246
- RFC 8446 (TLS 1.3): https://tools.ietf.org/html/rfc8446
- Epsilon Crypto Tests: `/modules/crypto/tests/`