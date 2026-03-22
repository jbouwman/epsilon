# epsilon.crypto

Cryptographic operations, TLS/SSL, and X.509 certificate handling via OpenSSL.

## Installation

```lisp
;; In your module definition
(package my-module
  (import (epsilon.crypto crypto)))
```

**System Requirement:** OpenSSL 1.1.0 or later must be installed.

## Features

- **TLS/SSL** - Secure connections with certificate validation
- **X.509 Certificates** - Parse, generate, and validate certificates
- **Key Generation** - RSA and EC key pairs
- **Hashing** - SHA-256, SHA-512, and other algorithms (via epsilon.digest)
- **Random** - Cryptographically secure random numbers

## Quick Start

```lisp
;; Generate a key pair
(let ((key (crypto:generate-rsa-key 2048)))
  (crypto:key-to-pem key :private))

;; Create a self-signed certificate
(let* ((key (crypto:generate-rsa-key 2048))
       (cert (crypto:make-self-signed-certificate
               key
               :subject '(:CN "localhost")
               :days 365)))
  (crypto:cert-to-pem cert))
```

## TLS/SSL Connections

### Client Connection

```lisp
;; Connect with certificate validation (default)
(let ((socket (crypto:tls-connect "example.com" 443)))
  (unwind-protect
      (progn
        (crypto:tls-write socket "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
        (crypto:tls-read socket 4096))
    (crypto:tls-close socket)))

;; Connect with custom CA bundle
(crypto:tls-connect host port :ca-file "/path/to/ca-bundle.crt")

;; Connect with client certificate
(crypto:tls-connect host port
  :cert-file "/path/to/client.crt"
  :key-file "/path/to/client.key")

;; Skip verification (NOT recommended for production)
(crypto:tls-connect host port :verify nil)
```

### Server Socket

```lisp
(let ((server (crypto:tls-listen 8443
                :cert-file "/path/to/server.crt"
                :key-file "/path/to/server.key")))
  (loop
    (let ((client (crypto:tls-accept server)))
      (handle-client client))))
```

### TLS Options

```lisp
;; Specify TLS version
(crypto:tls-connect host port :min-version :tls-1.2)
(crypto:tls-connect host port :min-version :tls-1.3)

;; Specify cipher suites
(crypto:tls-connect host port
  :ciphers "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256")

;; Enable ALPN for HTTP/2
(crypto:tls-connect host port :alpn '("h2" "http/1.1"))
```

## X.509 Certificates

### Parsing Certificates

```lisp
;; Parse PEM certificate
(let ((cert (crypto:parse-certificate-pem pem-string)))
  (crypto:certificate-subject cert)      ; Subject name
  (crypto:certificate-issuer cert)       ; Issuer name
  (crypto:certificate-not-before cert)   ; Valid from
  (crypto:certificate-not-after cert)    ; Valid until
  (crypto:certificate-serial cert)       ; Serial number
  (crypto:certificate-fingerprint cert)) ; SHA-256 fingerprint

;; Parse from file
(crypto:parse-certificate-file "/path/to/cert.pem")

;; Parse DER format
(crypto:parse-certificate-der der-bytes)
```

### Generating Certificates

```lisp
;; Self-signed certificate
(let* ((key (crypto:generate-rsa-key 2048))
       (cert (crypto:make-self-signed-certificate key
               :subject '(:CN "localhost"
                          :O "My Organization"
                          :C "US")
               :days 365
               :san '(:dns "localhost" :ip "127.0.0.1"))))
  (values (crypto:cert-to-pem cert)
          (crypto:key-to-pem key :private)))

;; Certificate Signing Request (CSR)
(let* ((key (crypto:generate-rsa-key 2048))
       (csr (crypto:make-csr key
              :subject '(:CN "example.com"
                         :O "Example Inc")
              :san '(:dns "example.com"
                     :dns "www.example.com"))))
  (crypto:csr-to-pem csr))

;; Sign CSR with CA
(let ((cert (crypto:sign-csr csr ca-key ca-cert
              :days 365)))
  (crypto:cert-to-pem cert))
```

### Certificate Validation

```lisp
;; Validate certificate chain
(crypto:verify-certificate cert
  :ca-file "/path/to/ca-bundle.crt"
  :hostname "example.com")  ; Also checks hostname

;; Check if certificate is expired
(crypto:certificate-expired-p cert)

;; Check certificate purpose
(crypto:certificate-can-sign-p cert)      ; Can sign other certs
(crypto:certificate-for-server-p cert)    ; Server authentication
(crypto:certificate-for-client-p cert)    ; Client authentication
```

## Key Generation

### RSA Keys

```lisp
;; Generate RSA key pair
(let ((key (crypto:generate-rsa-key 2048)))
  ;; Export private key (PEM)
  (crypto:key-to-pem key :private)

  ;; Export public key (PEM)
  (crypto:key-to-pem key :public)

  ;; Export encrypted private key
  (crypto:key-to-pem key :private :password "secret"))

;; Parse existing key
(crypto:parse-key-pem pem-string)
(crypto:parse-key-pem encrypted-pem :password "secret")
(crypto:parse-key-file "/path/to/key.pem")
```

### EC Keys

```lisp
;; Generate EC key pair
(let ((key (crypto:generate-ec-key :p-256)))
  (crypto:key-to-pem key :private))

;; Supported curves
:p-256      ; NIST P-256 (secp256r1)
:p-384      ; NIST P-384 (secp384r1)
:p-521      ; NIST P-521 (secp521r1)
```

## Random Numbers

```lisp
;; Generate random bytes
(crypto:random-bytes 32)  ; 32 random bytes

;; Generate random integer in range
(crypto:random-integer 1 100)  ; Random integer 1-100

;; Generate random UUID
(crypto:random-uuid)
```

## Error Handling

```lisp
(handler-case
    (crypto:tls-connect host port)
  (crypto:ssl-error (e)
    (format t "SSL error: ~A" (crypto:ssl-error-message e)))
  (crypto:certificate-error (e)
    (format t "Certificate error: ~A" (crypto:certificate-error-reason e))))
```

### Common Error Types

| Error | Description |
|-------|-------------|
| `ssl-error` | General SSL/TLS error |
| `certificate-error` | Certificate validation failed |
| `certificate-expired` | Certificate has expired |
| `certificate-not-yet-valid` | Certificate not yet valid |
| `hostname-mismatch` | Certificate doesn't match hostname |
| `self-signed` | Self-signed certificate (when validation required) |

## Security Considerations

1. **Always validate certificates in production** - Never set `:verify nil` except for testing
2. **Use TLS 1.2 or later** - Set `:min-version :tls-1.2`
3. **Keep OpenSSL updated** - Security fixes are released regularly
4. **Protect private keys** - Use appropriate file permissions and encryption
5. **Use strong key sizes** - RSA 2048+ bits, EC P-256 or stronger

## Examples

### Generate Development Certificates

```lisp
(defun make-dev-certs (output-dir)
  "Generate self-signed certificates for local development."
  (let* ((key (crypto:generate-rsa-key 2048))
         (cert (crypto:make-self-signed-certificate key
                 :subject '(:CN "localhost")
                 :days 365
                 :san '(:dns "localhost"
                        :ip "127.0.0.1"
                        :ip "::1"))))
    (with-open-file (s (path:join output-dir "server.key")
                       :direction :output)
      (write-string (crypto:key-to-pem key :private) s))
    (with-open-file (s (path:join output-dir "server.crt")
                       :direction :output)
      (write-string (crypto:cert-to-pem cert) s))
    (format t "Generated certificates in ~A~%" output-dir)))
```

### Mutual TLS (mTLS)

```lisp
;; Server requiring client certificates
(let ((server (crypto:tls-listen 8443
                :cert-file "server.crt"
                :key-file "server.key"
                :ca-file "client-ca.crt"  ; CA that signed client certs
                :verify-client t)))
  (let ((client (crypto:tls-accept server)))
    (let ((client-cert (crypto:peer-certificate client)))
      (format t "Client: ~A~%"
              (getf (crypto:certificate-subject client-cert) :cn)))))
```

## See Also

- [epsilon.http](../http/) - HTTPS client and server
- `epsilon.digest` - Hash functions (defined within this module)
- [examples/mtls.lisp](../../examples/mtls.lisp) - mTLS example
