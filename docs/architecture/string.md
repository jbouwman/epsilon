# EPSILON.LIB.STRING

## API Reference

### CONCAT

**Type**: Function

**Signature**: `(CONCAT &REST STRINGS)`

---

### DIGIT-CHAR-P

**Type**: Function

**Signature**: `(DIGIT-CHAR-P CHR)`

Tests whether a character is a decimal digit, i.e. the same as
Perl's [\d].  Note that this function shadows the standard Common
Lisp function CL:DIGIT-CHAR-P.

---

### EMPTY-P

**Type**: Function

**Signature**: `(EMPTY-P S)`

Return t if S is a string and non-zero-length

---

### ENDS-WITH-P

**Type**: Function

**Signature**: `(ENDS-WITH-P SEQ SUFFIX &KEY (TEST #'CHAR-EQUAL))`

Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST.

---

### FIRST-CHAR

**Type**: Function

**Signature**: `(FIRST-CHAR S)`

Return the first character of a non-empty string S, or NIL

---

### JOIN

**Type**: Function

**Signature**: `(JOIN DELIMITER STRINGS)`

---

### LAST-CHAR

**Type**: Function

**Signature**: `(LAST-CHAR S)`

Return the last character of a non-empty string S, or NIL

---

### NSUBSEQ

**Type**: Function

**Signature**: `(NSUBSEQ SEQUENCE START &OPTIONAL (END (LENGTH SEQUENCE)))`

Returns a subsequence by pointing to location in original sequence.

---

### RANDOM-STRING

**Type**: Function

**Signature**: `(RANDOM-STRING &OPTIONAL (LENGTH 12))`

---

### SPLIT

**Type**: Function

**Signature**: `(SPLIT DELIMITER STRING)`

Returns a lazy sequence of substrings of string, split by delimiter.

---

### STARTS-WITH-P

**Type**: Function

**Signature**: `(STARTS-WITH-P SEQ PREFIX &KEY (TEST #'CHAR-EQUAL))`

Returns true if the sequence SEQ starts with the sequence
PREFIX whereby the elements are compared using TEST.

---

### STRIP

**Type**: Function

**Signature**: `(STRIP STRING C)`

---

### STRIP-LEFT

**Type**: Function

**Signature**: `(STRIP-LEFT STRING C)`

---

### STRIP-RIGHT

**Type**: Function

**Signature**: `(STRIP-RIGHT STRING C)`

---

### WHITESPACEP

**Type**: Function

**Signature**: `(WHITESPACEP CHR)`

Tests whether a character is whitespace, i.e. whether it would
match [\s] in Perl.

---

### WORD-CHAR-P

**Type**: Function

**Signature**: `(WORD-CHAR-P CHR)`

Tests whether a character is a "word" character.  In the ASCII
charset this is equivalent to a-z, A-Z, 0-9, or _, i.e. the same as
Perl's [\w].

---

