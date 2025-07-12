# EPSILON.LIB.MAP

## Overview

Reader syntax

Public functions

Bitmap node operations

node protocol - collision-node

leaf-node operations

;  TODO
;
;  1. HAMT Over-Engineering for Small Maps
;  The Hash Array Mapped Trie implementation creates overhead for small maps (1-10 elements).
;  Consider specialized small-map types (array-map, linear-map) that switch to HAMT above
;  a threshold, similar to Clojure's implementation strategy.
;
;  2. Bulk Operation Performance
;  Operations like merge, difference, and from-pairs could use internal mutable builders
;  for efficiency, then convert to persistent structure at the end.
;
;  3. Extensive Symbol Shadowing
;  Shadowing many core CL functions (assoc, get, map, reduce, etc.) creates potential
;  conflicts and confusion when integrating with other Common Lisp code.
;
;  Reusable Components for Epsilon System
;
;  Unified Collection Interface
;  Rather than separate packages for each protocol, consider a unified epsilon.lib.core
;  approach similar to Clojure's core namespace, providing consistent collection operations
;  across maps, vectors, sequences, and sets in a single coherent interface.
;
;  Path-Based Data Access
;  The nested access functions (get-in, assoc-in, update-in) could work consistently
;  across all collection types in epsilon.lib.core rather than being map-specific.
;
;  Small Collection Optimization
;  Implement array-based maps for small sizes (1-10 elements) with automatic promotion
;  to HAMT structure when size threshold is exceeded.
;
;  Transient/Mutable Builders
;  Add transient map support for efficient bulk operations - create mutable builder,
;  perform operations, then convert to persistent map for final result.
;

## API Reference

### +EMPTY+

**Type**: Variable

---

### ASSOC

**Type**: Function

**Signature**: `(ASSOC MAP KEY VALUE)`

Return a new map with key-value pair added/updated

---

### ASSOC!

**Type**: Macro

**Signature**: `(ASSOC! PLACE KEY VALUE)`

Destructively associate KEY with VALUE in the map at PLACE

---

### ASSOC-IN

**Type**: Function

**Signature**: `(ASSOC-IN MAP KEYS VALUE)`

Associate a value in a nested map structure, where keys is a sequence of keys
   and value is the new value to be associated and return a new nested structure.
   Maps are created for nonexistent intermediate levels.

---

### CONTAINS-P

**Type**: Function

**Signature**: `(CONTAINS-P MAP KEY)`

Return true if map contains key

---

### COUNT

**Type**: Function

**Signature**: `(COUNT HAMT)`

Get the number of key-value pairs in the map

---

### DIFFERENCE

**Type**: Function

**Signature**: `(DIFFERENCE MAP1 MAP2)`

Return a map of key/value pairs in map1 but not in map2

---

### DISSOC

**Type**: Function

**Signature**: `(DISSOC MAP KEY)`

Return a new map with key removed

---

### DISSOC!

**Type**: Macro

**Signature**: `(DISSOC! PLACE KEY)`

Destructively remove KEY from the map at PLACE

---

### ENABLE-SYNTAX

**Type**: Symbol

---

### FILTER

**Type**: Function

**Signature**: `(FILTER PRED MAP)`

Return a new map containing only entries satisfying pred

---

### FROM-PAIRS

**Type**: Function

**Signature**: `(FROM-PAIRS PAIRS)`

Create a map from a list of cons pairs

---

### GET

**Type**: Function

**Signature**: `(GET MAP KEY &OPTIONAL DEFAULT)`

Get value for key from map, returning default if not found

---

### GET-IN

**Type**: Function

**Signature**: `(GET-IN MAP KEYS &OPTIONAL DEFAULT)`

Get value in nested map structure following key sequence

---

### INVERT

**Type**: Function

**Signature**: `(INVERT MAP)`

Return a new map with keys and values swapped

---

### KEYS

**Type**: Function

**Signature**: `(KEYS MAP)`

Return a list of all keys in the map

---

### MAKE-MAP

**Type**: Function

**Signature**: `(MAKE-MAP &REST KVS)`

Create a map from alternating keys and values

---

### MAP

**Type**: Function

**Signature**: `(MAP FN MAP)`

Apply FN to each value in MAP, returning a new map with the same keys but transformed values.
   FN should take two arguments: key and value.

---

### MAP=

**Type**: Function

**Signature**: `(MAP= MAP1 MAP2)`

Return true if maps are equal

---

### MERGE

**Type**: Function

**Signature**: `(MERGE MAP1 MAP2)`

Merge two maps, with map2 values taking precedence

---

### REDUCE

**Type**: Function

**Signature**: `(REDUCE FUNCTION MAP &OPTIONAL (INITIAL-VALUE +EMPTY+))`

Reduce over key-value pairs in the map

---

### SELECT-KEYS

**Type**: Function

**Signature**: `(SELECT-KEYS MAP KEYS)`

Return a new map containing only the specified keys

---

### SEQ

**Type**: Function

**Signature**: `(SEQ MAP)`

Return a sequence of key-value pairs from the map

---

### SIZE

**Type**: Function

**Signature**: `(SIZE HAMT)`

---

### SUBSET-P

**Type**: Function

**Signature**: `(SUBSET-P MAP1 MAP2)`

Return true if map1 is a subset of map2

---

### UPDATE

**Type**: Function

**Signature**: `(UPDATE MAP KEY F &REST ARGS)`

Update value in map at key by applying f

---

### UPDATE!

**Type**: Macro

**Signature**: `(UPDATE! PLACE KEY FUNCTION &REST ARGS)`

Destructively update the value at KEY in the map at PLACE using FUNCTION

---

### UPDATE-IN

**Type**: Function

**Signature**: `(UPDATE-IN MAP KEYS F &REST ARGS)`

Update value in nested map structure at key sequence by applying f

---

### VALS

**Type**: Function

**Signature**: `(VALS MAP)`

Return a list of all values in the map

---

