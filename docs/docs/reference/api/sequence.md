# EPSILON.LIB.SEQUENCE

## API Reference

### *EMPTY*

**Type**: Variable

---

### CONS

**Type**: Macro

**Signature**: `(CONS HEAD TAIL-EXPR)`

---

### COUNT

**Type**: Function

**Signature**: `(COUNT SEQ)`

---

### DROP

**Type**: Function

**Signature**: `(DROP N SEQ)`

Returns a lazy sequence of all but the first n items in seq

---

### EACH

**Type**: Function

**Signature**: `(EACH FUNCTION SEQ)`

Applies function to each element of seq for side effects, consuming the entire sequence.
Returns no value.

---

### EMPTY-P

**Type**: Function

**Signature**: `(EMPTY-P SEQUENCE)`

---

### FILTER

**Type**: Function

**Signature**: `(FILTER PREDICATE SEQ)`

Returns a lazy sequence of items from seq for which predicate returns true

---

### FIRST

**Type**: Function

**Signature**: `(FIRST SEQ)`

---

### FROM-LIST

**Type**: Function

**Signature**: `(FROM-LIST LIST)`

---

### GROUP-BY

**Type**: Function

**Signature**: `(GROUP-BY KEY-FN SEQUENCE)`

Group elements of sequence by the result of applying key-fn to each element.
Returns a map where keys are the group keys and values are lists of elements.

---

### ITERATE

**Type**: Function

**Signature**: `(ITERATE FUNCTION INITIAL-VALUE)`

Returns a lazy sequence of (initial-value, (f initial-value), (f (f initial-value)), ...).

---

### MAP

**Type**: Function

**Signature**: `(MAP FUNCTION &REST SEQUENCES)`

Returns a lazy sequence consisting of applying FUNCTION to the elements
of the sequences. If more than one sequence is provided, FUNCTION should
accept as many arguments as there are sequences.

---

### PARTITION-WHEN

**Type**: Function

**Signature**: `(PARTITION-WHEN PREDICATE SEQUENCE)`

Returns a lazy sequence of subsequences, split when predicate returns true.

The element that matches the predicate starts a new partition.

---

### REALIZE

**Type**: Function

**Signature**: `(REALIZE SEQ)`

---

### REDUCE

**Type**: Function

**Signature**: `(REDUCE FUNCTION SEQ &KEY (INITIAL-VALUE NIL INITIAL-VALUE-P))`

Reduces a sequence using function. If initial-value is provided, it is used
as the first value, otherwise the first element of the sequence is used.

---

### REST

**Type**: Function

**Signature**: `(REST SEQ)`

---

### SEQ

**Type**: Function

**Signature**: `(SEQ LIST)`

---

### SEQUENCE-P

**Type**: Function

**Signature**: `(SEQUENCE-P OBJ)`

Returns true if OBJ is a lazy sequence

---

### TAKE

**Type**: Function

**Signature**: `(TAKE N SEQ)`

---

