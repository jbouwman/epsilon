# Epsilon Transducer API Documentation

## Overview

The `epsilon.transducer` namespace provides composable algorithmic transformations based on Clojure's transducer design. Transducers provide efficient, composable transformations that are decoupled from the data structures they operate on, eliminating intermediate allocations and enabling early termination.

## Core Concepts

### Transducers
A transducer is a transformation that can be applied to different collection types without creating intermediate collections. They compose left-to-right but apply right-to-left.

### Reducing Functions
A reducing function takes an accumulator and an input, returning a new accumulator. Transducers transform reducing functions.

### Early Termination
The `reduced` protocol allows transducers to signal early termination of the reduction process.

## Core Protocol

### `reduced`
```lisp
(reduced value) → tx-reduced
```
Wrap a value to signal early termination of reduction.

### `reduced-p`
```lisp
(reduced-p x) → boolean
```
Test if a value is reduced (wrapped for early termination).

### `reduced-value`
```lisp
(reduced-value x) → value
```
Extract value from reduced wrapper. Returns the value unwrapped if not reduced.

### `ensure-reduced`
```lisp
(ensure-reduced x) → tx-reduced
```
Ensure value is wrapped as reduced. If already reduced, returns as-is.

### `preserving-reduced`
```lisp
(preserving-reduced rf) → function
```
Wrap reducing function to preserve reduced state through composition.

## Basic Transducers

### `map`
```lisp
(map f) → transducer
```
Returns a transducer that applies function `f` to each input.

**Example:**
```lisp
(into '() (map #'1+) '(1 2 3 4 5))
;; → (6 5 4 3 2)
```

### `filter`
```lisp
(filter pred) → transducer
```
Returns a transducer that passes through items where `(pred item)` is true.

**Example:**
```lisp
(into '() (filter #'evenp) '(1 2 3 4 5))
;; → (4 2)
```

### `remove`
```lisp
(remove pred) → transducer
```
Returns a transducer that removes items where `(pred item)` is true. Equivalent to `(filter (complement pred))`.

**Example:**
```lisp
(into '() (remove #'evenp) '(1 2 3 4 5))
;; → (5 3 1)
```

### `take`
```lisp
(take n) → transducer
```
Returns a transducer that passes through at most `n` items, then terminates.

**Example:**
```lisp
(into '() (take 3) '(1 2 3 4 5))
;; → (3 2 1)
```

### `take-while`
```lisp
(take-while pred) → transducer
```
Returns a transducer that passes through items while `(pred item)` is true, then terminates.

**Example:**
```lisp
(into '() (take-while (lambda (x) (< x 4))) '(1 2 3 4 5))
;; → (3 2 1)
```

### `take-nth`
```lisp
(take-nth n) → transducer
```
Returns a transducer that takes every nth item (1-based indexing).

**Example:**
```lisp
(into '() (take-nth 2) '(1 2 3 4 5 6 7 8 9 10))
;; → (9 7 5 3 1)
```

### `drop`
```lisp
(drop n) → transducer
```
Returns a transducer that drops the first `n` items, then passes through the rest.

**Example:**
```lisp
(into '() (drop 3) '(1 2 3 4 5))
;; → (5 4)
```

### `drop-while`
```lisp
(drop-while pred) → transducer
```
Returns a transducer that drops items while `(pred item)` is true, then passes through the rest.

**Example:**
```lisp
(into '() (drop-while (lambda (x) (< x 4))) '(1 2 3 4 5))
;; → (5 4)
```

### `dedupe`
```lisp
(dedupe &optional keyfn) → transducer
```
Returns a transducer that removes consecutive duplicate items. Optional `keyfn` is used to compute comparison key (defaults to `#'identity`).

**Example:**
```lisp
(into '() (dedupe) '(1 1 2 3 3 3 2 1 1))
;; → (1 2 3 2 1)
```

### `partition-by`
```lisp
(partition-by f) → transducer
```
Returns a transducer that partitions consecutive items based on the result of applying `f`.

**Example:**
```lisp
(into '() (partition-by #'identity) '(1 1 2 2 2 3 1 1))
;; → ((1 1) (2 2 2) (3) (1 1))
```

### `partition-all`
```lisp
(partition-all n &optional step) → transducer
```
Returns a transducer that partitions items into chunks of size `n`. Optional `step` parameter controls overlap (defaults to `n`).

**Example:**
```lisp
(into '() (partition-all 3) '(1 2 3 4 5 6 7 8))
;; → ((7 8) (4 5 6) (1 2 3))
```

### `keep`
```lisp
(keep f) → transducer
```
Returns a transducer that applies `f` to each input and keeps non-nil results.

**Example:**
```lisp
(into '() (keep (lambda (x) (when (oddp x) x))) '(1 2 3 4))
;; → (3 1)
```

### `keep-indexed`
```lisp
(keep-indexed f) → transducer
```
Returns a transducer that applies `f` to each input with its index and keeps non-nil results. Function `f` receives `(index item)`.

**Example:**
```lisp
(into '() (keep-indexed (lambda (i x) (when (evenp i) (list i x)))) '(:a :b :c :d))
;; → ((2 :c) (0 :a))
```

### `replace`
```lisp
(replace replacement-map) → transducer
```
Returns a transducer that replaces items found in `replacement-map` with their corresponding values.

**Example:**
```lisp
(let ((replacements (map:make-map 1 :one 2 :two 3 :three)))
  (into '() (replace replacements) '(1 2 3 4 5)))
;; → (:one :two :three 4 5)
```

### `mapcat`
```lisp
(mapcat f) → transducer
```
Returns a transducer that applies `f` to each input and concatenates the results.

**Example:**
```lisp
(into '() (mapcat (lambda (x) (make-list x :initial-element x))) '(1 2 3))
;; → (3 3 3 2 2 1)
```

### `halt-when`
```lisp
(halt-when pred &optional retf) → transducer
```
Returns a transducer that stops when `pred` returns true. Optional `retf` function can transform the final result.

**Example:**
```lisp
(into '() (halt-when (lambda (x) (= x 4))) '(1 2 3 4 5 6))
;; → (1 2 3)
```

### `cat`
```lisp
(cat) → transducer
```
Returns a transducer that concatenates collections (flattens one level).

**Example:**
```lisp
(into '() (cat) '((1 2) (3 4) (5 6)))
;; → (6 5 4 3 2 1)
```

## Transducer Composition

### `comp`
```lisp
(comp &rest xforms) → transducer
```
Compose transducers right-to-left (same order as function composition). The rightmost transducer is applied first.

**Example:**
```lisp
(into '() 
      (comp (filter #'evenp)
            (map (lambda (x) (* x 2))))
      '(1 2 3 4 5))
;; → (16 8 4)
```

## Reduction Functions

### `transduce`
```lisp
(transduce xform f init coll) → value
```
Transform `coll` with `xform`, then reduce with function `f` starting with `init`.

**Example:**
```lisp
(transduce (comp (filter #'evenp)
                 (map #'identity))
           #'+
           0
           '(1 2 3 4 5))
;; → 6  ; sum of even numbers
```

### `into`
```lisp
(into to xform from) → collection
```
Transform elements from `from` with `xform` and add to `to`. For lists, elements are added via `cons` so order is reversed.

**Example:**
```lisp
(into '() (map #'1+) '(1 2 3 4 5))
;; → (6 5 4 3 2)

(into #() (filter #'evenp) '(1 2 3 4 5))
;; → #(2 4)
```

### `sequence`
```lisp
(sequence xform coll) → list
```
Create a list by transforming `coll` with `xform`. Result is in forward order.

**Example:**
```lisp
(sequence (comp (filter #'evenp)
                (map (lambda (x) (* x 2))))
          '(1 2 3 4 5))
;; → (4 8)
```

## Advanced Constructs

### `eduction`
```lisp
(eduction xform coll) → eduction
```
Create an eduction that lazily applies `xform` to `coll`. An eduction represents a lazy transformation that can be realized later.

### `eduction->seq`
```lisp
(eduction->seq eduction) → epsilon-sequence
```
Convert an eduction to a lazy epsilon sequence that can be processed incrementally.

**Example:**
```lisp
(let* ((calls 0)
       (ed (eduction (map (lambda (x) 
                           (incf calls)
                           (* x 2)))
                     '(1 2 3 4 5))))
  ;; No computation yet, calls = 0
  (seq:take 3 (eduction->seq ed)))
;; → Epsilon sequence of (2 4 6), calls >= 3
```

## Data Structure Support

### Lists
Standard Lisp lists work with all transducers. Results are typically in reverse order due to `cons`-based building.

### Vectors
Vectors can be used as both input and output collections.

### Epsilon Maps
Maps can be used as replacement tables and as output collections.

### Epsilon Sequences
Lazy sequences integrate seamlessly with the transducer system.

## Performance Characteristics

1. **No Intermediate Collections**: Transducers avoid creating intermediate collections during composition
2. **Early Termination**: Operations like `take` can terminate processing early
3. **Lazy Evaluation**: Eductions provide lazy evaluation semantics
4. **Memory Efficiency**: Single-pass processing with minimal allocation

## Common Patterns

### Filter and Transform
```lisp
(into '() 
      (comp (filter #'evenp)
            (map #'sqrt))
      '(1 2 3 4 5 6 7 8 9))
```

### Take While Processing
```lisp
(into '()
      (comp (map #'process-item)
            (take-while #'valid-p))
      items)
```

### Partition and Process
```lisp
(into '()
      (comp (partition-all 3)
            (map #'process-batch))
      large-dataset)
```

### Flatten and Filter
```lisp
(into '()
      (comp (cat)
            (filter #'interesting-p))
      nested-lists)
```

## Error Handling

Transducers propagate exceptions normally. If a transducer function throws an exception, the entire reduction stops and the exception bubbles up.

## Thread Safety

Transducers themselves are stateless and thread-safe, but stateful transducers like `take`, `drop`, and `dedupe` create private state that is not shared between applications.
