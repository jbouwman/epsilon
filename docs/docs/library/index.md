# Libraries Overview

Epsilon provides functional libraries organized into several packages:

## Core Data Structures

- **[Data Structures](../core/data-structures.md)** - Immutable maps, sets, and collections
- **[JSON & YAML](../core/json-yaml.md)** - Data serialization and parsing
- **[Cryptography](../core/cryptography.md)** - Hashing and checksums
- **[String Processing](../core/strings.md)** - Unicode-aware text operations

## System Integration

- **[Networking](../network/http.md)** - HTTP client/server and socket programming
- **[Threading](../system/threading.md)** - Concurrency primitives and thread pools
- **[Filesystem](../system/filesystem.md)** - File and directory operations

## Design Philosophy

All libraries follow these principles:

- **Immutable by default**: Data structures don't modify in place
- **Functional interfaces**: Pure functions where possible
- **Local nicknames**: Clean integration with your code
- **No external dependencies**: Built on SBCL primitives only

## Usage Pattern

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:json :epsilon.lib.json)
   (:http :epsilon.net.http)))

(in-package :my-app)

;; Use the libraries with clean syntax
(defun api-handler (request)
  (let ((data (map:make-map :status "ok" :timestamp (get-universal-time))))
    (http:respond request 200 (json:encode data))))
```