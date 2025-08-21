# HPACK Library Options for Epsilon HTTP/2

## Overview
HPACK (HTTP/2 Header Compression) is a complex specification (RFC 7541) that includes:
- Huffman encoding with a specific static table
- Dynamic header table management
- Reference set management
- Integer encoding with specific bit patterns

## Options for Integration

### 1. Use Existing C Libraries via FFI (Recommended)

#### nghttp2 (Most Complete)
- **Pros**: 
  - Industry standard, used by curl, Apache, nginx
  - Full HPACK implementation
  - Well-tested with h2spec
  - Active maintenance
- **Cons**: 
  - Requires C library dependency
  - Need FFI bindings
- **Integration**: Via epsilon.foreign FFI

```lisp
;; Example FFI binding
(lib:defshared nghttp2-hd-deflate-new "nghttp2_hd_deflate_new" "libnghttp2" :pointer
  (deflater-ptr :pointer) (mem-user-data :pointer))
```

#### ls-hpack (Lightweight)
- **Pros**: 
  - Lightweight, header-only C library
  - Simpler to integrate
  - Good performance
- **Cons**: 
  - Less widely used
  - May need more testing

### 2. Pure Lisp Implementation

#### Existing CL Libraries
Unfortunately, there are very few (if any) complete HPACK implementations in Common Lisp. Most HTTP/2 work in CL is incomplete.

#### Port from Another Language
Could port from:
- **Go's golang.org/x/net/http2/hpack**: Clean, well-documented
- **Python's hyperframe/hpack**: Simple, readable
- **Rust's hpack crate**: Type-safe, efficient

### 3. Hybrid Approach (Pragmatic)

Start with a simplified implementation for testing, then integrate a proper library:

```lisp
;; Simplified HPACK for initial testing
(defun simple-hpack-encode (headers)
  "Minimal HPACK encoding - just literal headers without compression"
  (let ((output (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (header headers)
      ;; Literal header field without indexing (0x00)
      (vector-push-extend #x00 output)
      ;; Name length and name
      (let ((name-bytes (epsilon.string:string-to-octets (car header))))
        (vector-push-extend (length name-bytes) output)
        (loop for byte across name-bytes
              do (vector-push-extend byte output)))
      ;; Value length and value
      (let ((value-bytes (epsilon.string:string-to-octets (cdr header))))
        (vector-push-extend (length value-bytes) output)
        (loop for byte across value-bytes
              do (vector-push-extend byte output))))
    output))
```

## Recommendation

### Short Term (For h2spec testing)
1. Use simplified HPACK that passes basic tests
2. Focus on literal header encoding without Huffman
3. Implement minimal dynamic table support

### Long Term (Production)
1. **Integrate nghttp2 via FFI**
   - Most robust solution
   - Battle-tested
   - Maintained by HTTP/2 experts

### Implementation Plan

```lisp
;; modules/http2/src/hpack-simple.lisp
(defpackage :epsilon.http2.hpack
  (:use :cl)
  (:export #:encode-headers
           #:decode-headers
           #:make-encoder
           #:make-decoder))

;; Start with simple implementation
(defstruct hpack-encoder
  dynamic-table
  max-table-size)

(defstruct hpack-decoder
  dynamic-table
  max-table-size)

;; Later: modules/http2/src/hpack-nghttp2.lisp
;; FFI bindings to nghttp2's HPACK implementation
```

## Example Integration with nghttp2

```bash
# Install nghttp2
sudo apt-get install libnghttp2-dev  # Debian/Ubuntu
brew install nghttp2                  # macOS

# Check for the library
pkg-config --libs libnghttp2
```

```lisp
;; FFI bindings
(defpackage :epsilon.http2.hpack.nghttp2
  (:use :cl :epsilon.foreign)
  (:export #:create-deflater
           #:create-inflater
           #:deflate-headers
           #:inflate-headers))

(lib:defshared %nghttp2-hd-deflate-new "nghttp2_hd_deflate_new" "libnghttp2" :int
  (deflater-ptr :pointer)
  (mem-user-data :pointer))

(lib:defshared %nghttp2-hd-deflate-hd "nghttp2_hd_deflate_hd" "libnghttp2" :ssize-t
  (deflater :pointer)
  (buf :pointer)
  (buflen :size-t)
  (nva :pointer)
  (nvlen :size-t))
```

## Conclusion

For a production HTTP/2 implementation, using nghttp2's HPACK via FFI is the most pragmatic choice. It's:
- Well-tested against h2spec
- Actively maintained
- Used in production by major projects
- Saves significant development time

For initial testing and development, a simplified HPACK implementation is sufficient to make progress with h2spec conformance.