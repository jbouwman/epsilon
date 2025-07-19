# Epsilon Type Catalog Module

## Overview

The epsilon.catalog module provides a type catalog and registry system for managing structured type definitions. It allows you to define, register, and track complex type schemas with version control through SHA-256 hashing.

## What It Does

The catalog system serves several purposes:

1. **Type Registration**: Define and register custom types with structured field definitions
2. **Schema Management**: Track type schemas with names, descriptions, and field specifications
3. **Version Tracking**: Uses SHA-256 hashing to track changes to the catalog
4. **Type References**: Parse and resolve type references, including arrays and nested types

## Key Components

### type-catalog
A registry that stores type definitions with:
- Serial number (SHA-256 hash for version tracking)
- Array of type definitions
- Name lookup table for quick access

### type-definition
Represents a structured type with:
- Name
- Description
- Fields (each with name, type, and description)

### Reference Parsing
The system can parse type references like:
- Simple types: `"string"`
- Array types: `"string*"`
- Fixed-size arrays: `"byte(16)"`
- Complex types: `"string, integer"`

## Usage Example

```lisp
;; Create a type catalog
(defparameter *catalog* (make-instance 'type-catalog))

;; Define a new type
(defparameter *user-type*
  (make-type-definition
    '(:name "User"
      :description "A user record"
      :fields ((:name "id" :type "integer" :description "User ID")
               (:name "name" :type "string" :description "User name")
               (:name "email" :type "string" :description "Email address")))))

;; Parse type references
(parse-reference "string*")        ; => ("string" NIL T)  ; array of strings
(parse-reference "byte(16)")       ; => ("byte" 16 NIL)   ; 16 bytes
(parse-reference "string, integer") ; => (:TYPE (("string" NIL NIL) ("integer" NIL NIL)))
```

## Use Cases

1. **API Schema Definition**: Define request/response schemas for APIs
2. **Data Validation**: Validate data structures against registered types
3. **Code Generation**: Generate code from type definitions
4. **Documentation**: Auto-generate documentation from type schemas
5. **Protocol Definition**: Define binary or text protocol structures

## Integration with Epsilon

This module is designed to work with other Epsilon modules:
- Can be used by the build system to validate module interfaces
- Could be extended for FFI (Foreign Function Interface) type definitions
- Useful for serialization/deserialization systems
- Could support IDL (Interface Definition Language) capabilities

## Future Enhancements

The catalog system could be extended to:
- Support inheritance and type composition
- Add validation functions for each type
- Generate serialization/deserialization code
- Export to various schema formats (JSON Schema, Protocol Buffers, etc.)
- Support for generic types and type parameters