# epsilon.frame

A minimal, powerful columnar data frame implementation for Epsilon with planned support for zero-copy operations, copy-on-write semantics, and lazy evaluation.

## Current Status: Phase 1 Complete

The core abstractions are implemented and functional:
- Typed columns with efficient storage
- Basic frame operations (select, filter, slice)
- Property list and list conversions
- Immutable operations by default

## Quick Start

```lisp
;; Load the module
(epsilon.loader:load-module (epsilon.loader:environment) "epsilon.frame")

;; Create a frame
(defparameter *df* 
  (epsilon.frame:frame
   :name '("Alice" "Bob" "Charlie")
   :age '(25 30 35)
   :score '(92.5 87.3 95.0)))

;; Basic operations
(epsilon.frame:shape *df*)                    ; => (3 3)
(epsilon.frame:column-names *df*)             ; => ("NAME" "AGE" "SCORE")
(epsilon.frame:get-row *df* 0)                ; => (NAME "Alice" AGE 25 SCORE 92.5)

;; Select columns
(epsilon.frame:select *df* :name :score)      ; New frame with 2 columns

;; Filter rows
(epsilon.frame:where *df* 
  (lambda (row) (> (getf row :age) 28)))     ; Rows where age > 28

;; Slice rows
(epsilon.frame:head *df* 2)                   ; First 2 rows
(epsilon.frame:slice *df* 1 3)                ; Rows 1-2 (exclusive end)
```

## Data Types

Supported dtypes:
- Boolean: `:bool`
- Integers: `:int8`, `:int16`, `:int32`, `:int64`
- Unsigned: `:uint8`, `:uint16`, `:uint32`, `:uint64`
- Floats: `:float32`, `:float64`
- Text: `:string`
- Generic: `:any`

Types are automatically inferred when creating columns from lists.

## API Reference

### Frame Construction
- `(frame &rest column-specs)` - Create frame with alternating names and data
- `(make-frame &rest column-specs)` - Same as frame
- `(lists->frame column-names row-data)` - Create from row-oriented lists
- `(plists->frame plists)` - Create from property lists

### Frame Access
- `(get-column frame name)` - Get column by name
- `(get-row frame index)` - Get row as property list
- `(nrows frame)` - Number of rows
- `(ncols frame)` - Number of columns
- `(column-names frame)` - List of column names
- `(shape frame)` - Returns (nrows ncols)

### Frame Operations
- `(select frame &rest columns)` - Select columns
- `(where frame predicate)` - Filter rows by predicate
- `(slice frame start &optional end)` - Slice rows
- `(head frame &optional n)` - First n rows (default 5)
- `(tail frame &optional n)` - Last n rows (default 5)

### Frame Modification (returns new frame)
- `(add-column frame name column-or-data)` - Add column
- `(drop-column frame name)` - Remove column
- `(rename-column frame old-name new-name)` - Rename column

### Conversions
- `(frame->lists frame)` - Convert to row-oriented lists
- `(frame->plists frame)` - Convert to property lists

### Column Operations
- `(column dtype &rest values)` - Create column
- `(column-get column index)` - Get element
- `(column-slice column start &optional end)` - Slice column
- `(column-map function column)` - Map function over column
- `(column-filter predicate column)` - Filter column

## Roadmap

### Phase 2: Computation Engine (In Progress)
- Expression-based lazy operations
- Zero-copy views
- Basic aggregations
- Computation protocol

### Phase 3: Advanced Indexing
- Named indices
- Label-based access
- Group-by operations
- Joins

### Phase 4: Memory Optimization
- Copy-on-write semantics
- Buffer sharing
- Memory pools

### Phase 5: I/O and Integration
- CSV reader/writer
- Arrow format support
- Integration with epsilon.table for display

## Design Principles

1. **Small API Surface** - Core operations only, compose for complexity
2. **Immutable by Default** - Operations return new frames
3. **Column-Oriented** - Efficient for analytical workloads
4. **Type Safety** - Strong typing with automatic inference
5. **Zero-Copy Future** - Architecture supports future zero-copy operations

## Examples

### Creating from Various Sources

```lisp
;; From columns
(frame :a (column :int32 1 2 3)
       :b (column :float64 1.0 2.0 3.0))

;; From lists
(frame :x '(1 2 3)
       :y #(4 5 6))

;; From property lists
(plists->frame 
  '((:name "Alice" :age 25)
    (:name "Bob" :age 30)))

;; From row-oriented data
(lists->frame '(:a :b :c)
              '((1 2 3)
                (4 5 6)))
```

### Data Analysis Workflow

```lisp
;; Load and filter data
(defparameter *sales*
  (-> (frame :product '("A" "B" "C" "A" "B")
             :region '("East" "West" "East" "West" "East")
             :amount '(100 200 150 120 180))
      (where (lambda (row) 
               (string= (getf row :region) "East")))))

;; Add computed column (Phase 2 will make this easier)
(defparameter *with-tax*
  (add-column *sales* :tax
    (column-map (lambda (amt) (* amt 0.1))
                (get-column *sales* :amount))))
```

## Contributing

This module is under active development. Key areas for contribution:
- Performance optimizations
- Additional data types
- I/O formats
- Statistical functions

## License

Part of the Epsilon project.