# Security Best Practices for epsilon.crypto

## Overview

The epsilon.crypto module provides cryptographic operations with
security-first design. This document outlines security best practices
and features.

## Security Features

### 1. Automatic OpenSSL Initialization
- OpenSSL is automatically initialized when the module loads
- Thread-safe initialization through mutex protection
- Automatic PRNG seeding from system entropy

### 2. Memory Security
- **Secure Memory Zeroing**: Use `secure-zero-memory` to clear sensitive data
- Prevents compiler optimizations from removing memory clearing
- Applied automatically to temporary key material

### 3. Timing Attack Prevention
- **Constant-Time Comparison**: Use `constant-time-compare` for MACs and hashes
- Prevents timing side-channel attacks
- Always compares full length regardless of differences

### 4. Input Validation
- All key sizes are validated against secure minimums
- RSA keys must be at least 2048 bits
- Proper bounds checking on all buffer operations

## Secure Usage Guidelines

### Key Generation
```lisp
;; Always use adequate key sizes
(crypto:generate-rsa-key :bits 2048)    ; Minimum recommended
(crypto:generate-rsa-key :bits 3072)    ; Better security
(crypto:generate-rsa-key :bits 4096)    ; Maximum security

;; For EC keys, use standard curves
(crypto:generate-ec-key :curve :p256)   ; 128-bit security
(crypto:generate-ec-key :curve :p384)   ; 192-bit security
(crypto:generate-ec-key :curve :p521)   ; 256-bit security
```

### Password Hashing
```lisp
;; Use PBKDF2 with sufficient iterations
(crypto:pbkdf2 password salt 100000)    ; Minimum 100k iterations

;; Or use scrypt for memory-hard hashing
(crypto:scrypt password salt 16384 8 1 32)  ; Standard parameters
```

### Authenticated Encryption
```lisp
;; Always use authenticated encryption for data at rest
(crypto:aes-gcm-encrypt plaintext key)     ; AES-GCM
(crypto:chacha20-poly1305-encrypt plaintext key)  ; ChaCha20-Poly1305
```

### Secure Random Numbers
```lisp
;; Use crypto-random for all security-sensitive randomness
(crypto:crypto-random-bytes 32)         ; For keys
(crypto:crypto-random-integer 1000000)  ; For nonces
```

## Security Checklist

### DO:
- ✓ Use minimum 2048-bit RSA keys
- ✓ Use authenticated encryption (AES-GCM or ChaCha20-Poly1305)
- ✓ Use PBKDF2/scrypt with high iteration counts
- ✓ Clear sensitive memory after use
- ✓ Use constant-time comparison for MACs
- ✓ Validate all inputs and check return values
- ✓ Use SHA-256 or SHA-3 for hashing

### DON'T:
- ✗ Use MD5 or SHA-1 for security (broken)
- ✗ Use RSA keys smaller than 2048 bits
- ✗ Store passwords in plain text
- ✗ Use ECB mode for encryption
- ✗ Reuse nonces/IVs
- ✗ Compare secrets with standard equality
- ✗ Use non-crypto random for keys

## Algorithm Recommendations

### Recommended Algorithms (2024)
- **Hashing**: SHA-256, SHA-3, BLAKE2
- **Symmetric**: AES-256-GCM, ChaCha20-Poly1305
- **Asymmetric**: RSA-2048+, EC P-256+, Ed25519
- **KDF**: PBKDF2 (100k+ iterations), scrypt, Argon2
- **MAC**: HMAC-SHA256, Poly1305

### Deprecated Algorithms
- **MD5**: Completely broken, collision attacks
- **SHA-1**: Collision attacks demonstrated
- **DES/3DES**: Insufficient key size
- **RC4**: Biased output, broken
- **RSA-1024**: Factorizable with current technology

## Compliance Notes

### FIPS 140-2
- Use FIPS-approved algorithms when required
- RSA 2048+, AES, SHA-256+ are FIPS approved
- Ed25519 and ChaCha20 are not FIPS approved

### PCI DSS
- Minimum RSA 2048-bit keys required
- Strong cryptography for data at rest and in transit

## Error Handling

Always handle cryptographic errors appropriately:

```lisp
(handler-case
    (crypto:some-operation ...)
  (crypto:crypto-error (e)
    ;; Log error securely (don't leak sensitive info)
    (log-error "Crypto operation failed: ~A" 
               (crypto:crypto-error-code e))
    ;; Fail securely
    (error "Security operation failed")))
```
