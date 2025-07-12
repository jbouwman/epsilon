# Functional Data Structures

Epsilon provides a complete suite of immutable, functional data structures designed for high-performance Common Lisp development. All collections support structural sharing for memory efficiency and provide O(log n) operations for most common use cases.

## Overview

Epsilon's functional collections are built on solid computer science foundations:

- **Hash Array Mapped Tries (HAMT)** for maps and sets
- **Lazy evaluation** with memoization for sequences  
- **Structural sharing** for memory efficiency
- **Immutable by design** with persistent updates
- **SBCL optimized** for maximum performance

## Core Collection Types

### Maps (`epsilon.lib.map`)

Immutable key-value associations implemented as Hash Array Mapped Tries.

```lisp
;; Create maps with literal syntax
{:name "Epsilon" :version 1.0 :stable true}

;; Or programmatically
(map:make-map :a 1 :b 2 :c 3)
;=> {:a 1, :b 2, :c 3}

;; Functional updates return new maps
(map:assoc my-map :d 4)
(map:dissoc my-map :a)
(map:merge map1 map2)
```

**Key Features:**
- O(log n) lookup, insert, and delete
- Collision-resistant hashing
- Nested access with `get-in`, `assoc-in`, `update-in`
- Functional transformations: `map`, `filter`, `reduce`

### Sets (`epsilon.lib.set`)

Immutable collections of unique values, also HAMT-based.

```lisp
;; Create sets
(set:make-set 1 2 3 4 5)
;=> #{1 2 3 4 5}

;; Set operations
(set:union set1 set2)
(set:intersection set1 set2)  
(set:difference set1 set2)

;; Membership testing
(set:contains-p my-set value)
```

**Key Features:**
- O(log n) add, remove, and membership testing
- Mathematical set operations (union, intersection, difference)
- Functional transformations over set elements

### Sequences (`epsilon.lib.sequence`)

Lazy, infinite sequences with functional operations.

```lisp
;; Create sequences
(seq:seq 1 2 3 4 5)
(seq:from-list '(a b c d))

;; Lazy operations
(seq:map #'1+ (seq:iterate #'1+ 0))  ; Infinite sequence of naturals
(seq:take 10 (seq:filter #'evenp naturals))

;; Realize when needed
(seq:realize (seq:take 5 my-sequence))
;=> (0 1 2 3 4)
```

**Key Features:**
- Lazy evaluation with memoization
- Support for infinite sequences
- Memory-efficient processing of large datasets
- Rich functional operation library

### Lists (`epsilon.lib.list`)

Enhanced operations on standard Common Lisp lists.

```lisp
;; Association list operations
(list:alist-plist '((a . 1) (b . 2)))
;=> (:a 1 :b 2)

;; Property list operations  
(list:remove-from-plist '(:a 1 :b 2 :c 3) :b)
;=> (:a 1 :c 3)

;; Circular list detection
(list:circular-list-p my-list)
```

**Key Features:**
- Alist/plist conversion utilities
- Circular list detection and handling
- Property list manipulations
- List set operations

## Design Principles

### Immutability

All data structures are immutable by design. Operations return new instances rather than modifying existing ones:

```lisp
(let ((original {:a 1 :b 2}))
  (let ((updated (map:assoc original :c 3)))
    ;; original is unchanged: {:a 1, :b 2}
    ;; updated is new: {:a 1, :b 2, :c 3}
    ))
```

### Structural Sharing

Collections share structure where possible to minimize memory usage:

```lisp
;; These maps share most of their internal structure
(let* ((map1 {:a 1 :b 2 :c 3 :d 4})
       (map2 (map:assoc map1 :e 5)))
  ;; map2 reuses internal nodes from map1
  )
```

### Performance Characteristics

| Operation | Maps/Sets | Sequences | Lists |
|-----------|-----------|-----------|-------|
| Lookup | O(log n) | O(n) | O(n) |
| Insert | O(log n) | O(1) | O(1) |
| Delete | O(log n) | N/A | O(n) |
| Iteration | O(n) | O(n) | O(n) |

### Local Nicknames

All packages use local nicknames for clean integration:

```lisp
(defpackage #:my-package
  (:use #:common-lisp)
  (:local-nicknames 
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)
    (#:set #:epsilon.lib.set)))

(in-package #:my-package)

;; Clean, readable code
(map:get my-map :key)
(seq:map #'process data)
(set:union set1 set2)
```

## Collection Interoperation

Collections are designed to work together consistently:

```lisp
;; Convert between collection types
(map:from-pairs (seq:realize my-sequence))
(set:from-sequence (map:keys my-map))
(seq:from-list (set:realize my-set))

;; Chain operations
(->> data
     (seq:map #'process)
     (seq:filter #'valid-p)
     (seq:group-by #'category)
     (map:map (lambda (k v) (seq:realize v))))
```

## Memory Management

Epsilon's collections are designed for efficient memory usage:

- **Structural sharing** reduces memory overhead
- **Lazy sequences** process data on-demand
- **SBCL integration** leverages compiler optimizations
- **Garbage collection friendly** with minimal allocation

## Thread Safety

All functional data structures are inherently thread-safe due to immutability:

```lisp
;; Safe to share between threads
(defparameter *shared-map* {:config "value"})

;; Each thread can update without coordination
(map:assoc *shared-map* :thread-id (thread:current-thread-id))
```

## Reader Syntax

Epsilon provides convenient reader syntax for common collections:

```lisp
;; Maps use curly braces
{:key "value" :count 42}

;; Sets use hash-set notation  
#{1 2 3 4 5}

;; Vectors use square brackets (when available)
[1 2 3 4 5]
```

## Best Practices

### 1. Prefer Immutable Collections

Use Epsilon collections instead of mutable alternatives:

```lisp
;; Instead of hash tables
(let ((ht (make-hash-table)))
  (setf (gethash :key ht) "value"))

;; Use maps
(let ((m (map:assoc (map:empty) :key "value")))
  ...)
```

### 2. Leverage Structural Sharing

Design data structures to maximize sharing:

```lisp
;; Efficient: shares structure
(let ((base-config {:host "localhost" :port 8080}))
  (list
    (map:assoc base-config :env "dev")
    (map:assoc base-config :env "prod")))
```

### 3. Use Lazy Sequences for Large Data

Process large datasets efficiently with lazy evaluation:

```lisp
;; Memory-efficient processing
(->> (fs:file-lines "large-dataset.txt")
     (seq:map #'parse-line)
     (seq:filter #'valid-record-p)
     (seq:take 1000)
     (seq:realize))
```

### 4. Chain Operations Functionally

Build complex transformations through composition:

```lisp
(defun process-data (data)
  (->> data
       (seq:map #'normalize)
       (seq:filter #'meets-criteria-p)
       (seq:group-by #'category)
       (map:map #'aggregate-group)))
```

## Integration with SBCL

Epsilon collections are optimized for SBCL:

- **Type declarations** for compiler optimization
- **Efficient compilation** of functional operations  
- **Memory layout** optimized for SBCL's GC
- **SBCL-specific features** like weak references where beneficial

---

*Next: [Sequences & Maps](sequences-maps.md) - Detailed API reference for sequences and maps*
