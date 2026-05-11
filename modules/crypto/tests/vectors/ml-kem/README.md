# ML-KEM test vectors

This directory holds canonical test vectors for the three FIPS 203
parameter sets (ML-KEM-512, ML-KEM-768, ML-KEM-1024). When populated,
the loader test in `../../ml-kem-vectors-tests.lisp` picks them up
automatically and runs each vector through the corresponding
deterministic entry point (`keygen-internal`, `encaps-internal`, or
`decaps`), asserting byte-level equality against the reference output.

## File format

Each vector file is a line-oriented, `key: value` plaintext format
where byte values are lowercase hex. Blank lines and `#` comments are
ignored. Files live under `<set>/<kind>/*.vec` where:

- `<set>` is one of `ml-kem-512`, `ml-kem-768`, `ml-kem-1024`.
- `<kind>` is one of `keygen`, `encaps`, `decaps`.

### `keygen` vectors

```
d: 0001020304...   ; 32 bytes
z: 1011121314...   ; 32 bytes
ek: ...            ; expected encapsulation key (byte length per set)
dk: ...            ; expected decapsulation key (byte length per set)
```

### `encaps` vectors

```
ek: ...            ; input public key
m: ...             ; 32-byte encapsulation coin
k: ...             ; expected 32-byte shared secret
c: ...             ; expected ciphertext (byte length per set)
```

### `decaps` vectors (both valid and implicit-rejection branches)

```
dk: ...            ; input private key
c: ...             ; input ciphertext
k: ...             ; expected 32-byte recovered shared secret
; optional, default "valid"; set to "rejection" to document that this
; vector exercises the implicit-rejection branch (the implementation
; does not need to know which branch it is — decaps output is compared
; by equality either way).
kind: valid|rejection
```

## Sources

The loader is format-agnostic — any converter that produces the
key:value layout above will work. Recommended sources:

- **NIST ACVP**: the ML-KEM Key Generation / Encapsulation /
  Decapsulation JSON files from
  <https://pages.nist.gov/ACVP/draft-celi-acvp-ml-kem.html>.
- **PQClean**: the KAT files shipped with the reference
  implementation at
  <https://github.com/PQClean/PQClean/tree/master/crypto_kem>.

When importing, pin at minimum:

- one KeyGen vector per parameter set
- one Encaps vector per parameter set
- one Decaps vector (valid) per parameter set
- one Decaps vector (rejection) per parameter set

This matches the set called out in IMPL-329 Phase A1 as the minimum
interop evidence. More vectors are welcome — the loader iterates over
every `*.vec` file it finds.

## Current state

**Empty.** Until canonical vectors are imported, the regression pins
in `ml-kem-tests.lisp` provide byte-level coverage under the all-zero
seed tuple. Those pins catch drift but do not prove interop.
