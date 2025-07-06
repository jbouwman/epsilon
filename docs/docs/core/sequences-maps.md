# Sequences & Maps API Reference

Detailed API documentation for Epsilon's core functional collections: sequences and maps.

## Maps (`epsilon.lib.map`)

Immutable key-value associations implemented as Hash Array Mapped Tries (HAMT).

### Construction

#### `(make-map &rest key-value-pairs)`

Create a new map from alternating keys and values.

```lisp
(map:make-map :a 1 :b 2 :c 3)
;=> {:a 1, :b 2, :c 3}

(map:make-map "name" "Epsilon" "version" 1.0)
;=> {"name" "Epsilon", "version" 1.0}
```

#### `(empty)`

Return an empty map.

```lisp
(map:empty)
;=> {}
```

#### `(from-pairs pairs)`

Create a map from a sequence of key-value pairs.

```lisp
(map:from-pairs '((:a . 1) (:b . 2) (:c . 3)))
;=> {:a 1, :b 2, :c 3}
```

### Basic Operations

#### `(get map key &optional default)`

Retrieve a value by key, returning `default` if not found.

```lisp
(map:get {:a 1 :b 2} :a)
;=> 1

(map:get {:a 1 :b 2} :c "not found")
;=> "not found"
```

#### `(assoc map key value &rest more-kvs)`

Return a new map with key(s) associated to value(s).

```lisp
(map:assoc {:a 1} :b 2)
;=> {:a 1, :b 2}

(map:assoc {:a 1} :b 2 :c 3 :d 4)
;=> {:a 1, :b 2, :c 3, :d 4}
```

#### `(dissoc map &rest keys)`

Return a new map with key(s) removed.

```lisp
(map:dissoc {:a 1 :b 2 :c 3} :b)
;=> {:a 1, :c 3}

(map:dissoc {:a 1 :b 2 :c 3} :a :c)
;=> {:b 2}
```

#### `(contains-p map key)`

Test if map contains a key.

```lisp
(map:contains-p {:a 1 :b 2} :a)
;=> T

(map:contains-p {:a 1 :b 2} :c)
;=> NIL
```

### Nested Operations

#### `(get-in map path &optional default)`

Get a value from a nested map structure.

```lisp
(let ((nested {:user {:profile {:name "Alice" :age 30}}}))
  (map:get-in nested '(:user :profile :name)))
;=> "Alice"

(map:get-in nested '(:user :settings :theme) "dark")
;=> "dark"
```

#### `(assoc-in map path value)`

Associate a value in a nested map structure.

```lisp
(let ((nested {:user {:profile {:name "Alice"}}}))
  (map:assoc-in nested '(:user :profile :age) 30))
;=> {:user {:profile {:name "Alice", :age 30}}}
```

#### `(update-in map path function &rest args)`

Update a value in a nested map structure by applying a function.

```lisp
(let ((nested {:counters {:clicks 5 :views 10}}))
  (map:update-in nested '(:counters :clicks) #'1+))
;=> {:counters {:clicks 6, :views 10}}
```

### Collection Operations

#### `(merge &rest maps)`

Merge multiple maps, with rightmost values taking precedence.

```lisp
(map:merge {:a 1 :b 2} {:b 3 :c 4} {:c 5 :d 6})
;=> {:a 1, :b 3, :c 5, :d 6}
```

#### `(select-keys map keys)`

Return a new map with only the specified keys.

```lisp
(map:select-keys {:a 1 :b 2 :c 3 :d 4} '(:a :c))
;=> {:a 1, :c 3}
```

#### `(keys map)`

Return a sequence of all keys in the map.

```lisp
(seq:realize (map:keys {:a 1 :b 2 :c 3}))
;=> (:a :b :c)  ; order not guaranteed
```

#### `(vals map)`

Return a sequence of all values in the map.

```lisp
(seq:realize (map:vals {:a 1 :b 2 :c 3}))
;=> (1 2 3)  ; order not guaranteed
```

#### `(seq map)`

Return a sequence of key-value pairs.

```lisp
(seq:realize (map:seq {:a 1 :b 2}))
;=> ((:a . 1) (:b . 2))
```

### Functional Transformations

#### `(map function map)`

Transform values using a function that takes key and value.

```lisp
(map:map (lambda (k v) (* v 2)) {:a 1 :b 2 :c 3})
;=> {:a 2, :b 4, :c 6}
```

#### `(filter predicate map)`

Return a new map with entries that satisfy the predicate.

```lisp
(map:filter (lambda (k v) (> v 2)) {:a 1 :b 2 :c 3 :d 4})
;=> {:c 3, :d 4}
```

#### `(reduce function initial-value map)`

Reduce over key-value pairs.

```lisp
(map:reduce (lambda (acc k v) (+ acc v)) 0 {:a 1 :b 2 :c 3})
;=> 6
```

### Queries

#### `(count map)`

Return the number of key-value pairs.

```lisp
(map:count {:a 1 :b 2 :c 3})
;=> 3
```

#### `(empty-p map)`

Test if the map is empty.

```lisp
(map:empty-p (map:empty))
;=> T

(map:empty-p {:a 1})
;=> NIL
```

---

## Sequences (`epsilon.lib.sequence`)

Lazy, functional sequences with efficient operations.

### Construction

#### `(seq &rest elements)`

Create a sequence from elements.

```lisp
(seq:seq 1 2 3 4 5)
;=> <sequence: (1 2 3 4 5)>
```

#### `(from-list list)`

Create a sequence from a list.

```lisp
(seq:from-list '(a b c d))
;=> <sequence: (a b c d)>
```

#### `(from-vector vector)`

Create a sequence from a vector.

```lisp
(seq:from-vector #(1 2 3 4))
;=> <sequence: (1 2 3 4)>
```

#### `(empty)`

Return an empty sequence.

```lisp
(seq:empty)
;=> <empty sequence>
```

### Basic Operations

#### `(first sequence)`

Return the first element of the sequence.

```lisp
(seq:first (seq:seq 1 2 3))
;=> 1
```

#### `(rest sequence)`

Return a new sequence with all elements except the first.

```lisp
(seq:rest (seq:seq 1 2 3))
;=> <sequence: (2 3)>
```

#### `(cons element sequence)`

Return a new sequence with element prepended.

```lisp
(seq:cons 0 (seq:seq 1 2 3))
;=> <sequence: (0 1 2 3)>
```

#### `(empty-p sequence)`

Test if the sequence is empty.

```lisp
(seq:empty-p (seq:empty))
;=> T
```

### Functional Operations

#### `(map function sequence)`

Apply function to each element, returning a new lazy sequence.

```lisp
(seq:map #'1+ (seq:seq 1 2 3 4))
;=> <sequence: (2 3 4 5)>

;; Lazy evaluation - function not called until realized
(seq:map #'expensive-computation (seq:seq 1 2 3))
```

#### `(filter predicate sequence)`

Return a lazy sequence of elements satisfying the predicate.

```lisp
(seq:filter #'evenp (seq:seq 1 2 3 4 5 6))
;=> <sequence: (2 4 6)>
```

#### `(reduce function initial-value sequence)`

Reduce the sequence using function and initial value.

```lisp
(seq:reduce #'+ 0 (seq:seq 1 2 3 4 5))
;=> 15

(seq:reduce #'max most-negative-fixnum (seq:seq 3 1 4 1 5))
;=> 5
```

### Sequence Manipulation

#### `(take n sequence)`

Return a lazy sequence of the first n elements.

```lisp
(seq:realize (seq:take 3 (seq:seq 1 2 3 4 5)))
;=> (1 2 3)
```

#### `(drop n sequence)`

Return a lazy sequence with the first n elements removed.

```lisp
(seq:realize (seq:drop 2 (seq:seq 1 2 3 4 5)))
;=> (3 4 5)
```

#### `(take-while predicate sequence)`

Take elements while predicate returns true.

```lisp
(seq:realize (seq:take-while (lambda (x) (< x 4)) (seq:seq 1 2 3 4 5)))
;=> (1 2 3)
```

#### `(drop-while predicate sequence)`

Drop elements while predicate returns true.

```lisp
(seq:realize (seq:drop-while (lambda (x) (< x 4)) (seq:seq 1 2 3 4 5)))
;=> (4 5)
```

### Advanced Operations

#### `(group-by function sequence)`

Group elements by the result of applying function.

```lisp
(map:map #'seq:realize 
         (seq:group-by #'evenp (seq:seq 1 2 3 4 5 6)))
;=> {T (2 4 6), NIL (1 3 5)}
```

#### `(partition-when predicate sequence)`

Partition sequence when predicate returns true.

```lisp
(map:map #'seq:realize
         (seq:partition-when #'zerop (seq:seq 1 2 0 3 4 0 5)))
;=> ((1 2) (3 4) (5))
```

#### `(iterate function initial-value)`

Generate an infinite sequence by repeatedly applying function.

```lisp
(seq:realize (seq:take 5 (seq:iterate #'1+ 0)))
;=> (0 1 2 3 4)

(seq:realize (seq:take 5 (seq:iterate (lambda (x) (* x 2)) 1)))
;=> (1 2 4 8 16)
```

#### `(cycle sequence)`

Create an infinite sequence by repeating the input sequence.

```lisp
(seq:realize (seq:take 8 (seq:cycle (seq:seq 1 2 3))))
;=> (1 2 3 1 2 3 1 2)
```

### Realization

#### `(realize sequence)`

Force evaluation of a lazy sequence, returning a list.

```lisp
(seq:realize (seq:map #'1+ (seq:seq 1 2 3)))
;=> (2 3 4)
```

#### `(realize-to-vector sequence)`

Force evaluation to a vector.

```lisp
(seq:realize-to-vector (seq:seq 1 2 3 4))
;=> #(1 2 3 4)
```

## Integration Examples

### Processing Data Pipelines

```lisp
;; Process user data with maps and sequences
(defun process-users (user-data)
  (->> user-data
       (seq:from-list)
       (seq:map (lambda (user)
                  (map:update-in user '(:profile :age) #'calculate-age)))
       (seq:filter (lambda (user)
                     (> (map:get-in user '(:profile :age)) 18)))
       (seq:group-by (lambda (user)
                       (map:get-in user '(:profile :country))))
       (map:map #'seq:realize)))
```

### Configuration Management

```lisp
;; Merge configuration with environment overrides
(defun load-config (base-config env-overrides)
  (let ((config (map:merge base-config env-overrides)))
    (map:update-in config '(:database :url) 
                   (lambda (url) 
                     (if (map:get config :debug)
                         (concatenate 'string url "?debug=true")
                         url)))))
```

### Lazy Data Processing

```lisp
;; Process large datasets efficiently
(defun analyze-log-files (file-paths)
  (->> file-paths
       (seq:from-list)
       (seq:map #'read-log-file)      ; Lazy file reading
       (seq:mapcat #'parse-log-lines) ; Flatten log lines
       (seq:filter #'error-line-p)    ; Only error lines
       (seq:group-by #'extract-error-type)
       (map:map (lambda (k v) 
                  {:count (seq:count v)
                   :examples (seq:realize (seq:take 5 v))}))))
```

---

*Next: [JSON & YAML](json-yaml.md) - Data serialization and parsing*