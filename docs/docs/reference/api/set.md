# EPSILON.LIB.SET

## API Reference

### +EMPTY+

**Type**: Variable

---

### ADD

**Type**: Function

**Signature**: `(ADD SET VALUE)`

Return a new set with value added

---

### CONTAINS-P

**Type**: Function

**Signature**: `(CONTAINS-P SET VALUE)`

Return true if set contains value

---

### COUNT

**Type**: Function

**Signature**: `(COUNT SET)`

Get the number of elements in the set

---

### DIFFERENCE

**Type**: Function

**Signature**: `(DIFFERENCE SET1 SET2)`

Return the difference of two sets (values in set1 but not in set2)

---

### DISJ

**Type**: Function

**Signature**: `(DISJ SET VALUE)`

Return a new set with value removed

---

### ENABLE-SYNTAX

**Type**: Symbol

---

### FILTER

**Type**: Function

**Signature**: `(FILTER PRED SET)`

Return a new set containing only values satisfying pred

---

### INTERSECTION

**Type**: Function

**Signature**: `(INTERSECTION SET1 SET2)`

Return the intersection of two sets

---

### MAKE-SET

**Type**: Function

**Signature**: `(MAKE-SET &REST VALUES)`

Create a set from values

---

### MAP

**Type**: Function

**Signature**: `(MAP SET FN)`

Apply FN to each value in SET, returning a new set with transformed values

---

### REDUCE

**Type**: Function

**Signature**: `(REDUCE FUNCTION SET &OPTIONAL (INITIAL-VALUE +EMPTY+))`

Reduce over values in the set

---

### REMOVE

**Type**: Function

**Signature**: `(REMOVE SET VALUE)`

Alias for disj - return a new set with value removed

---

### SEQ

**Type**: Function

**Signature**: `(SEQ SET)`

Return a sequence of values from the set

---

### SET-P

**Type**: Function

**Signature**: `(SET-P X)`

Returns true if x is a set

---

### SET=

**Type**: Function

**Signature**: `(SET= SET1 SET2)`

Return true if sets are equal

---

### SIZE

**Type**: Function

**Signature**: `(SIZE SET)`

---

### SUBSET-P

**Type**: Function

**Signature**: `(SUBSET-P SET1 SET2)`

Return true if set1 is a subset of set2

---

### UNION

**Type**: Function

**Signature**: `(UNION SET1 SET2)`

Return the union of two sets

---

