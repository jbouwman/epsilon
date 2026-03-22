# Changelog

All notable changes to the epsilon.crypto module will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2025

### Breaking Changes

This major version update reflects migration to OpenSSL 3.0 APIs and removal of deprecated functionality.

#### Removed APIs (OpenSSL 3.0 Deprecations)

The following low-level RSA functions have been removed:
- `%rsa-new` - Use EVP_PKEY APIs instead
- `%rsa-free` - Use EVP_PKEY APIs instead
- `%rsa-generate-key-ex` - Use `EVP_PKEY_keygen` instead
- `%evp-pkey-assign-rsa` - Use `EVP_PKEY_fromdata` instead
- `%evp-pkey-get1-rsa` - Use `EVP_PKEY_get_bn_param` instead

The following low-level EC key functions have been removed:
- `%ec-key-new-by-curve-name` - Use EVP_PKEY APIs instead
- `%ec-key-free` - Use EVP_PKEY APIs instead
- `%ec-key-generate-key` - Use `EVP_PKEY_keygen` instead
- `%evp-pkey-assign-ec-key` - Use `EVP_PKEY_fromdata` instead
- `%evp-pkey-get1-ec-key` - Use `EVP_PKEY_get_bn_param` instead

### Migration Guide

#### RSA Key Generation

Before (1.x):
```lisp
;; Direct RSA key creation
(let ((rsa (%rsa-new)))
  (%rsa-generate-key-ex rsa 2048 exponent nil)
  ...)
```

After (2.x):
```lisp
;; Use EVP_PKEY high-level APIs
(let ((ctx (%evp-pkey-ctx-new-id +evp-pkey-rsa+ nil)))
  (%evp-pkey-keygen-init ctx)
  (%evp-pkey-ctx-set-rsa-keygen-bits ctx 2048)
  (%evp-pkey-keygen ctx pkey))
```

#### EC Key Generation

Before (1.x):
```lisp
;; Direct EC key creation
(let ((ec (%ec-key-new-by-curve-name nid)))
  (%ec-key-generate-key ec)
  ...)
```

After (2.x):
```lisp
;; Use EVP_PKEY high-level APIs
(let ((ctx (%evp-pkey-ctx-new-id +evp-pkey-ec+ nil)))
  (%evp-pkey-keygen-init ctx)
  (%evp-pkey-ctx-set-ec-paramgen-curve-nid ctx nid)
  (%evp-pkey-keygen ctx pkey))
```

### Security Notes

- TLS 1.0 and TLS 1.1 constants are still defined but marked as deprecated/insecure
- MD5 and SHA-1 hash algorithms are still available but marked as deprecated for security-sensitive uses
- Recommend using TLS 1.2+ and SHA-256/SHA-3 for new code

### Added

- Full OpenSSL 3.0 EVP_PKEY provider API support
- Modern key generation through EVP_PKEY_keygen
- TLS 1.3 ciphersuite configuration

### Changed

- All key generation now uses the high-level EVP_PKEY APIs
- Internal implementation uses OpenSSL 3.0 recommended patterns

## [1.0.0] - Initial Release

### Added

- TLS/SSL client and server support
- X.509 certificate parsing and validation
- RSA and EC key generation
- Hash algorithms: MD5, SHA-1, SHA-256, SHA-384, SHA-512, SHA-3
- HMAC support
- Certificate chain verification
