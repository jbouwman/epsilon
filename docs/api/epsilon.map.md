# epsilon.map

Immutable, persistent hash maps using Hash Array Mapped Trie (HAMT) data structure.

## Overview

The `epsilon.map` package provides immutable, persistent hash maps with O(log32 n) operations for get, assoc, and dissoc. The implementation uses structural sharing for memory efficiency, making it ideal for functional programming patterns where data immutability is important.

## Quick Start

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames (:map :epsilon.map)))

(in-package :my-app)

;; Create and manipulate maps
(defparameter *m1* (map:assoc map:+empty+ :name "Alice" :age 30))
(defparameter *m2* (map:assoc *m1* :city "NYC"))

;; Access values
(map:get *m2* :name)     ; => "Alice"
(map:get *m2* :missing "default") ; => "default"

;; Check membership
(map:contains-p *m2* :age) ; => T

;; Remove keys
(map:dissoc *m2* :city)  ; Returns new map without :city
```

## Function Reference

### Map Creation

#### +empty+
`+empty+`

The empty map constant. Use this as the starting point for building maps.

```lisp
map:+empty+ ; => #<HAMT (empty)>
```

#### make-map
`(make-map &rest key-value-pairs) => map`

Create a map from alternating keys and values.

```lisp
(make-map :a 1 :b 2 :c 3)
; => map with {:a 1, :b 2, :c 3}
```

#### from-pairs
`(from-pairs pairs) => map`

Create a map from a list of (key . value) pairs.

```lisp
(from-pairs '((:a . 1) (:b . 2)))
; => map with {:a 1, :b 2}
```

### Map Access

#### get
`(get map key &optional default) => value`

Get value for key from map, returning default if not found.

```lisp
(get (assoc +empty+ :name "Bob") :name) ; => "Bob"
(get +empty+ :missing "none")           ; => "none"
```

#### get-in
`(get-in map keys &optional default) => value`

Get value from nested maps using a sequence of keys.

```lisp
(defparameter *nested* 
  (assoc +empty+ :user (assoc +empty+ :name "Alice")))
(get-in *nested* '(:user :name)) ; => "Alice"
```

#### keys
`(keys map) => list`

Return a list of all keys in the map.

```lisp
(keys (make-map :a 1 :b 2)) ; => (:A :B)
```

#### vals
`(vals map) => list`

Return a list of all values in the map.

```lisp
(vals (make-map :a 1 :b 2)) ; => (1 2)
```

#### seq
`(seq map) => alist`

Return an association list of key-value pairs from map.

```lisp
(seq (make-map :a 1 :b 2))
; => ((:A . 1) (:B . 2))
```

### Map Modification

#### assoc
`(assoc map key value &rest more-kvs) => map`

Associate key-value pairs in map, returning a new map.

```lisp
(assoc +empty+ :a 1)          ; => map with {:a 1}
(assoc +empty+ :a 1 :b 2 :c 3) ; => map with {:a 1, :b 2, :c 3}
```

#### assoc!
`(assoc! map key value &rest more-kvs) => map`

Destructive version of assoc (for transient maps).

#### assoc-in
`(assoc-in map keys value) => map`

Associate value in nested map structure.

```lisp
(assoc-in +empty+ '(:user :name) "Alice")
; => {:user {:name "Alice"}}
```

#### dissoc
`(dissoc map key &rest more-keys) => map`

Remove keys from map, returning a new map.

```lisp
(dissoc (make-map :a 1 :b 2 :c 3) :b)
; => map with {:a 1, :c 3}
```

#### dissoc!
`(dissoc! map key &rest more-keys) => map`

Destructive version of dissoc (for transient maps).

#### update
`(update map key fn &rest args) => map`

Update value at key by applying function.

```lisp
(update (make-map :count 1) :count #'1+)
; => map with {:count 2}
```

#### update-in
`(update-in map keys fn &rest args) => map`

Update value in nested map by applying function.

```lisp
(update-in (make-map :stats (make-map :visits 0))
           '(:stats :visits) #'1+)
; => {:stats {:visits 1}}
```

### Map Queries

#### size
`(size map) => integer`

Return the number of key-value pairs in the map.

```lisp
(size (assoc +empty+ :a 1 :b 2)) ; => 2
```

#### count
`(count map) => integer`

Return the number of key-value pairs (alias for size).

```lisp
(count (assoc +empty+ :x 10)) ; => 1
```

#### contains-p
`(contains-p map key) => boolean`

Return T if map contains key.

```lisp
(contains-p (assoc +empty+ :id 42) :id) ; => T
(contains-p +empty+ :missing)           ; => NIL
```

#### contains-key-p
`(contains-key-p map key) => boolean`

Return T if map contains key (alias for contains-p).

```lisp
(contains-key-p (assoc +empty+ :x 1) :x) ; => T
```

#### map-p
`(map-p object) => boolean`

Test if object is a HAMT map.

```lisp
(map-p +empty+)      ; => T
(map-p '((:a . 1)))  ; => NIL
```

#### map=
`(map= map1 map2) => boolean`

Test if two maps are equal (same keys and values).

```lisp
(map= (make-map :a 1) (make-map :a 1)) ; => T
(map= (make-map :a 1) (make-map :a 2)) ; => NIL
```

#### subset-p
`(subset-p map1 map2) => boolean`

Test if map1 is a subset of map2.

```lisp
(subset-p (make-map :a 1) 
          (make-map :a 1 :b 2)) ; => T
```

### Map Transformation

#### map
`(map fn map) => map`

Apply function to each key-value pair, building new map.

```lisp
(map (lambda (k v) (values k (* v 2)))
     (make-map :a 1 :b 2))
; => map with {:a 2, :b 4}
```

#### filter
`(filter pred map) => map`

Keep only entries where (pred key value) returns true.

```lisp
(filter (lambda (k v) (> v 10))
        (make-map :a 5 :b 15 :c 20))
; => map with {:b 15, :c 20}
```

#### reduce
`(reduce fn init map) => value`

Reduce map with function of (accumulator key value).

```lisp
(reduce (lambda (sum k v) (+ sum v))
        0
        (make-map :a 1 :b 2 :c 3))
; => 6
```

#### select-keys
`(select-keys map keys) => map`

Return map with only the specified keys.

```lisp
(select-keys (make-map :a 1 :b 2 :c 3 :d 4)
             '(:a :c))
; => map with {:a 1, :c 3}
```

#### invert
`(invert map) => map`

Swap keys and values in map.

```lisp
(invert (make-map :a 1 :b 2))
; => map with {1 :a, 2 :b}
```

### Map Operations

#### merge
`(merge &rest maps) => map`

Merge multiple maps, later values override earlier.

```lisp
(merge (make-map :a 1 :b 2)
       (make-map :b 3 :c 4))
; => map with {:a 1, :b 3, :c 4}
```

#### difference
`(difference map1 map2) => map`

Return map1 without keys present in map2.

```lisp
(difference (make-map :a 1 :b 2 :c 3)
            (make-map :b 99 :d 100))
; => map with {:a 1, :c 3}
```

### Iteration

#### each
`(each fn map) => nil`

Call function on each key-value pair for side effects.

```lisp
(each (lambda (k v)
        (format t "~A: ~A~%" k v))
      (make-map :name "Alice" :age 30))
; Prints:
; NAME: Alice
; AGE: 30
```

## Internal Details

### HAMT Structure

The Hash Array Mapped Trie uses:
- 32-way branching for balanced depth
- Bitmap compression for space efficiency
- Path copying for persistence
- Structure sharing for memory efficiency

### Performance Characteristics

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| get       | O(log32 n)     | O(1)            |
| assoc     | O(log32 n)     | O(log32 n)      |
| dissoc    | O(log32 n)     | O(log32 n)      |
| count     | O(1)           | O(1)            |
| seq       | O(n)           | O(n)            |

The log32 base means:
- Maps with < 32 entries: 1 level deep
- Maps with < 1024 entries: 2 levels deep
- Maps with < 32768 entries: 3 levels deep

## Best Practices

1. **Start with +empty+**: Always build maps starting from the empty constant
2. **Use structural sharing**: Modified maps share structure with originals
3. **Batch updates**: Use multiple key-value pairs in single `assoc` call
4. **Prefer get-in/assoc-in**: For nested structures, use specialized functions
5. **Consider transients**: For bulk operations, transient maps can improve performance

## Thread Safety

All operations on HAMT maps are thread-safe due to immutability. Multiple threads can safely:
- Read from the same map
- Create derived maps
- Share map references

## Examples

### Configuration Management

```lisp
(defparameter *default-config*
  (make-map :host "localhost"
            :port 8080
            :debug nil))

(defparameter *prod-config*
  (merge *default-config*
         (make-map :host "prod.example.com"
                   :debug nil
                   :ssl t)))
```

### Nested Data Structures

```lisp
(defparameter *app-state*
  (-> +empty+
      (assoc-in '(:users :alice) 
                (make-map :name "Alice" :role :admin))
      (assoc-in '(:users :bob)
                (make-map :name "Bob" :role :user))
      (assoc :settings
            (make-map :theme "dark" :lang "en"))))

(get-in *app-state* '(:users :alice :role)) ; => :ADMIN
```

### Functional Updates

```lisp
(defun add-to-cart (state user-id item)
  (update-in state 
             (list :users user-id :cart)
             (lambda (cart)
               (cons item (or cart '())))))

(defparameter *state* (make-map :users (make-map)))
(setf *state* (add-to-cart *state* :alice "book"))
(setf *state* (add-to-cart *state* :alice "pen"))
(get-in *state* '(:users :alice :cart))
; => ("pen" "book")
```

## See Also

- [epsilon.set](epsilon.set.md) - Immutable sets using HAMT
- [epsilon.sequence](epsilon.sequence.md) - Lazy sequences
- [Architecture: HAMT](../architecture/map.md) - Technical details