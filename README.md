# Epsilon

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)

Epsilon is a Lisp programming environment built on SBCL that provides functional
data structures, networking, cryptography, and a module system.

- **Functional Data Structures** - Persistent maps (HAMT), sets, and vectors with structural sharing
- **HTTP Client/Server** - HTTP with TLS, connection pooling, and streaming
- **Cryptography** - TLS/SSL, X.509 certificates, and cryptographic operations via OpenSSL
- **Modern Module System** - Versioning and dependency resolution
- **Testing Framework** - Property-based testing, snapshots, benchmarks, and parallel execution

## Quick Start

### Install

```bash
curl -fsSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash
```

### Hello World

```bash
./epsilon --eval '(format t "Hello, Epsilon!~%")'
```

### Start the REPL

```bash
./epsilon
epsilon> (+ 1 2 3)
6
epsilon> (json:encode '(:name "Epsilon" :version "0.15.0"))
"{\"name\":\"Epsilon\",\"version\":\"0.15.0\"}"
```

## Examples

### HTTP Client

```lisp
;; Simple GET request
(http:get "https://api.github.com/users/octocat")

;; POST JSON data
(http:post "https://httpbin.org/post"
  :json '(:message "Hello from Epsilon"))

;; With headers and timeout
(http:get "https://example.com"
  :headers '(("Authorization" . "Bearer token"))
  :timeout 30)
```

### HTTP Server

```lisp
(defun handle-request (request)
  (http:response 200
    :headers '(("Content-Type" . "application/json"))
    :body (json:encode '(:status "ok"))))

(http:serve #'handle-request :port 8080)
```

### JSON Processing

```lisp
;; Parse JSON
(json:parse "{\"name\": \"Epsilon\", \"features\": [\"fast\", \"functional\"]}")
;; => (:NAME "Epsilon" :FEATURES ("fast" "functional"))

;; Encode to JSON
(json:encode '(:name "Epsilon" :version 14))
;; => "{\"name\":\"Epsilon\",\"version\":14}"
```

### Functional Data Structures

```lisp
;; Persistent map
(let* ((m1 (map:of :a 1 :b 2))
       (m2 (map:assoc m1 :c 3)))  ; m1 unchanged
  (map:get m2 :c))
;; => 3

;; Persistent set
(let* ((s1 (set:of 1 2 3))
       (s2 (set:conj s1 4)))
  (set:contains? s2 4))
;; => T
```

## Documentation

- [Installation Guide](INSTALL.md) - Detailed installation instructions
- [Changelog](CHANGELOG.md) - Version history and release notes
- [Examples](examples/) - Working example programs

## Platform Support

| Platform     | Status                   |
|--------------|--------------------------|
| Linux x86_64 | Supported                |
| macOS ARM64  | Supported                |

## Project Commands

```bash
# Run tests
./epsilon --test epsilon

# Create a new project
./epsilon --init my-project

# Create a new module
./epsilon --new my-module --template library

# Update epsilon
epsilon update
```

## Contributing

Run `make test` to verify changes, follow existing code conventions, and open a pull request.

## Security

To report a security issue, please email the maintainer directly rather than opening a public issue.

## License

MIT License - see [LICENSE](LICENSE) for details.
