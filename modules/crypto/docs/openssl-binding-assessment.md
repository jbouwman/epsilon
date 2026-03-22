# OpenSSL Binding Assessment

## Overview

This assessment analyzes the epsilon.crypto module's 196 OpenSSL bindings to determine
which could use auto-binding vs which require manual treatment.

## Current Binding Inventory

| Category          | Count   | Notes                      |
|-------------------|---------|----------------------------|
| SSL/TLS Context   | 25      | SSL_CTX_* functions        |
| SSL Connection    | 20      | SSL_* connection functions |
| EVP Crypto        | 45      | High-level crypto API      |
| X509 Certificates | 40      | Certificate handling       |
| BIO I/O           | 12      | Buffer I/O operations      |
| ASN1 Types        | 15      | ASN.1 data types           |
| Error Handling    | 5       | ERR_* functions            |
| Random            | 4       | RAND_* functions           |
| Key Derivation    | 10      | KDF functions              |
| Other             | 20      | BIGNUM, OBJ, etc.          |
| **Total**         | **196** |                            |

## Macro vs Function Analysis

### True Functions (Visible to libclang)

Most OpenSSL functions are true C functions and can be discovered by libclang:

```c
// These ARE real functions
SSL_CTX *SSL_CTX_new(const SSL_METHOD *method);
int SSL_connect(SSL *ssl);
int EVP_DigestUpdate(EVP_MD_CTX *ctx, const void *data, size_t len);
```

### Macro-Based "Functions" (NOT Visible to libclang)

Some common OpenSSL operations are implemented as macros calling `*_ctrl`:

```c
// ssl.h - These are MACROS, not functions!
#define SSL_set_mode(ssl,op) SSL_ctrl((ssl),SSL_CTRL_MODE,(op),NULL)
#define SSL_get_mode(ssl) SSL_ctrl((ssl),SSL_CTRL_MODE,0,NULL)
#define SSL_set_mtu(ssl, mtu) SSL_ctrl((ssl),SSL_CTRL_SET_MTU,(mtu),NULL)
#define SSL_set_msg_callback_arg(ssl, arg) SSL_ctrl((ssl), SSL_CTRL_SET_MSG_CALLBACK_ARG, 0, (arg))
```

The current epsilon.crypto module correctly handles this:
```lisp
;; ffi.lisp:394-412
;; SSL_set_tlsext_host_name is a macro in OpenSSL, not a function.
;; It expands to: SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, name)
;; We use SSL_ctrl directly instead.
(ffi:defshared %ssl-ctrl "SSL_ctrl" :ssl :long
  (ssl :pointer) (cmd :int) (larg :long) (parg :pointer))

(defun %ssl-set-tlsext-host-name (ssl hostname)
  "Set SNI hostname using SSL_ctrl (workaround for SSL_set_tlsext_host_name macro)"
  ...)
```

### Macro Inventory

Function-like macros in OpenSSL headers (requires manual wrappers):

| Macro Category | Count | Example |
|----------------|-------|---------|
| SSL mode/options | 8 | SSL_set_mode, SSL_get_mode |
| SSL certificates | 4 | SSL_CTX_set_tmp_dh |
| DTLS timeouts | 3 | DTLS_get_timeout |
| Ex data indexes | 6 | SSL_get_ex_new_index |
| Certificate transparency | 2 | SSL_disable_ct |
| **Total macros** | **~23** | |

## Auto-Binding Candidates

### High Confidence (160+ functions, ~82%)

These are true functions with simple signatures:

#### EVP Crypto (45 functions)
```lisp
;; All EVP functions are real functions
EVP_MD_CTX_new, EVP_MD_CTX_free
EVP_DigestInit_ex, EVP_DigestUpdate, EVP_DigestFinal_ex
EVP_PKEY_new, EVP_PKEY_free
EVP_EncryptInit_ex, EVP_EncryptUpdate, EVP_EncryptFinal_ex
```

#### X509 Certificates (40 functions)
```lisp
;; X509 functions are real functions
X509_new, X509_free
X509_set_version, X509_get_version
X509_sign, X509_verify
```

#### SSL Core (30 functions)
```lisp
;; Core SSL functions are real
SSL_CTX_new, SSL_CTX_free
SSL_new, SSL_free
SSL_connect, SSL_accept
SSL_read, SSL_write
SSL_shutdown
```

#### BIO Operations (12 functions)
```lisp
;; BIO functions are real
BIO_new, BIO_free
BIO_read, BIO_write
BIO_new_mem_buf
```

### Manual Binding Required (~36 functions, ~18%)

#### String Parameters (15 functions)
Functions taking C strings need wrappers:
```lisp
SSL_CTX_use_certificate_file  ; filename string
SSL_CTX_load_verify_locations ; path strings
EVP_get_digestbyname          ; algorithm name string
X509_NAME_add_entry_by_txt    ; field name string
```

#### Macro Wrappers (23 functions)
Need manual implementation via `*_ctrl`:
```lisp
SSL_set_mode, SSL_get_mode
SSL_set_tlsext_host_name
SSL_CTX_set_tmp_dh
```

## OpenSSL 1.1 vs 3.0 Differences

### Deprecated in 3.0 (Removed from current bindings)

```lisp
;; These are commented out in ffi.lisp as deprecated:
RSA_new, RSA_free, RSA_generate_key_ex
EC_KEY_new_by_curve_name, EC_KEY_free, EC_KEY_generate_key
EVP_PKEY_assign_RSA, EVP_PKEY_assign_EC_KEY
```

### Renamed in 3.0

```lisp
;; 1.1: SSL_get_peer_certificate
;; 3.0: SSL_get1_peer_certificate (reference counting change)
(ffi:defshared %ssl-get-peer-certificate "SSL_get1_peer_certificate" ...)
```

### New in 3.0

```lisp
;; Modern key generation
EVP_PKEY_CTX_new_from_name  ; New in 3.0
EVP_PKEY_generate           ; Replaces RSA_generate_key_ex
```

## Hybrid Approach Recommendation

### Structure

```
epsilon/modules/crypto/src/
  ffi.lisp              ; Current: All manual bindings (keep)
  ffi-auto-gen.lisp     ; Future: Auto-generated simple bindings
  ffi-manual.lisp       ; Future: Manual wrappers for strings/macros
```

### Implementation Plan

1. **Keep existing ffi.lisp** - It's working and tested
2. **Future auto-gen option** - For bulk additions, could auto-generate EVP/X509 bindings
3. **Document macro handling** - Ensure ctrl-based workarounds are documented

### Why NOT Auto-Bind OpenSSL Currently

1. **Working bindings exist** - 196 tested bindings
2. **Complex error handling** - Integrated with crypto-error condition
3. **Memory management** - Explicit alloc/free patterns
4. **Version handling** - 1.1 vs 3.0 differences need human judgment
5. **Macro workarounds** - Already implemented correctly

## Conclusion

| Metric | Value |
|--------|-------|
| Total bindings | 196 |
| Could auto-bind | ~160 (82%) |
| Require manual | ~36 (18%) |
| Recommended action | Keep manual bindings |

The current manual binding approach is appropriate for OpenSSL because:
1. Bindings are stable and tested
2. Error handling is integrated
3. Version differences are handled
4. Macro workarounds are in place

Auto-binding would be useful for:
- Adding new OpenSSL 3.x APIs in bulk
- Generating initial bindings for new crypto libraries
- Reducing boilerplate for simple function additions
