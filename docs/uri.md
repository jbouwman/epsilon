# EPSILON.LIB.URI

## API Reference

### AUTHORITY

**Type**: Function

**Signature**: `(AUTHORITY URI)`

Get the authority part of a URI

---

### ENSURE-DIRECTORY-PATH

**Type**: Function

**Signature**: `(ENSURE-DIRECTORY-PATH PATH)`

Ensure path ends with '/' to indicate it's a directory.
   Returns the path with trailing '/' added if not present.

---

### ENSURE-FILE-PATH

**Type**: Function

**Signature**: `(ENSURE-FILE-PATH PATH)`

Ensure path does not end with '/' to indicate it's a file.
   Returns the path with trailing '/' removed if present.

---

### FILE-URI

**Type**: Function

**Signature**: `(FILE-URI PATH)`

---

### FRAGMENT

**Type**: Function

**Signature**: `(FRAGMENT INSTANCE)`

---

### HOST

**Type**: Function

**Signature**: `(HOST INSTANCE)`

---

### MAKE-URI

**Type**: Function

**Signature**: `(MAKE-URI &KEY ((SCHEME SCHEME) NIL) ((USERINFO USERINFO) NIL) ((HOST HOST) NIL) ((PORT
                                                                                   PORT)
                                                                                  NIL) ((PATH
                                                                                         PATH)
                                                                                        NIL) ((QUERY
                                                                                               QUERY)
                                                                                              NIL) ((FRAGMENT
                                                                                                     FRAGMENT)
                                                                                                    NIL))`

---

### MERGE

**Type**: Function

**Signature**: `(MERGE URI REL-PATH)`

Merge a URI with a path string. Discards query and fragment.
   
   URI Path Handling Best Practice:
   - Directories should end with '/' in paths 
   - Files should NOT end with '/'
   - Double slashes '//' are avoided by checking separators
   - Empty relative paths are handled gracefully

---

### PARENT

**Type**: Function

**Signature**: `(PARENT URI)`

Get the parent URI (one directory level up)

---

### PATH

**Type**: Function

**Signature**: `(PATH INSTANCE)`

---

### PATH-JOIN

**Type**: Function

**Signature**: `(PATH-JOIN &REST COMPONENTS)`

Join path components with proper separator handling.
   Avoids double slashes and handles empty components gracefully.
   
   Example: (path-join "src" "lib/" "uri.lisp") → "src/lib/uri.lisp"

---

### PORT

**Type**: Function

**Signature**: `(PORT INSTANCE)`

---

### QUERY

**Type**: Function

**Signature**: `(QUERY INSTANCE)`

---

### SCHEME

**Type**: Function

**Signature**: `(SCHEME INSTANCE)`

---

### TO-STRING

**Type**: Function

**Signature**: `(TO-STRING URI)`

Convert a URI structure to a string

---

### URI

**Type**: Function

**Signature**: `(URI URI-STRING)`

Parse a URI string into a URI structure

---

### URI-P

**Type**: Function

**Signature**: `(URI-P OBJECT)`

---

### URL-DECODE

**Type**: Function

**Signature**: `(URL-DECODE STRING)`

URL decode a string

---

### URL-ENCODE

**Type**: Function

**Signature**: `(URL-ENCODE STRING)`

URL encode a string

---

### URL-ENCODE-PARAMS

**Type**: Function

**Signature**: `(URL-ENCODE-PARAMS PARAMS)`

Encode a list of key-value pairs for use in URL query strings

---

### USERINFO

**Type**: Function

**Signature**: `(USERINFO INSTANCE)`

---

