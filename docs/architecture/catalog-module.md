# Epsilon Catalog Module

## Overview

The `epsilon.catalog` module provides a type catalog and registry system for managing structured type definitions. It was originally part of epsilon.core but has been extracted into its own module for better modularity.

## Purpose

The catalog system is designed to:

1. **Define and register custom types** with structured schemas
2. **Track type versions** using SHA-256 hashing
3. **Parse type references** including arrays and complex types
4. **Provide a foundation** for code generation, validation, and serialization

## Architecture

### Core Classes

**type-catalog**
- Maintains a registry of type definitions
- Tracks changes via SHA-256 serial numbers
- Provides name-based lookup for quick access

**type-definition**
- Represents a structured type with:
  - Name and description
  - Fields with their own types and descriptions
  - Support for nested and complex types

### Type Reference Syntax

The catalog supports parsing various type reference formats:

```lisp
"string"          ; Simple type
"string*"         ; Array of strings
"byte(16)"        ; Fixed-size type (16 bytes)
"string, integer" ; Composite type
```

## Example Usage

```lisp
;; Load the catalog module
(ql:quickload :epsilon.catalog)

;; Define a structured type
(defparameter *user-type*
  (epsilon.tool.catalog:make-type-definition
    '(:name "User"
      :description "User account information"
      :fields ((:name "id" :type "uuid" :description "Unique identifier")
               (:name "username" :type "string" :description "Login name")
               (:name "email" :type "string" :description "Email address")
               (:name "roles" :type "string*" :description "User roles")
               (:name "metadata" :type "map" :description "Additional data")))))

;; Parse type references
(epsilon.tool.catalog:parse-reference "string*")
; => ("string" NIL T)  ; array type

(epsilon.tool.catalog:parse-reference "byte(32)")
; => ("byte" 32 NIL)   ; fixed-size type
```

## Use Cases

1. **API Schema Definition**
   - Define request/response structures
   - Generate API documentation
   - Validate incoming data

2. **Binary Protocol Definition**
   - Define packet structures
   - Generate serialization code
   - Document wire formats

3. **Database Schema Management**
   - Define table structures
   - Track schema versions
   - Generate migration scripts

4. **Code Generation**
   - Generate data classes
   - Create validation functions
   - Build serialization routines

5. **Type-Safe Configuration**
   - Define configuration schemas
   - Validate config files
   - Generate config documentation

## Integration Points

The catalog module can integrate with:

- **Build System**: Validate module interfaces
- **FFI System**: Define foreign type mappings
- **Serialization**: MessagePack, JSON, binary formats
- **Documentation**: Auto-generate type documentation
- **Testing**: Generate test data from type definitions

## Future Enhancements

Potential extensions include:

- Type inheritance and composition
- Generic types with parameters
- Validation function generation
- Schema migration support
- Export to standard formats (JSON Schema, Protocol Buffers)
- Type inference from data samples
- GraphQL schema generation

## Module Structure

```
module/catalog/
├── package.edn          # Module metadata
├── README.md           # Module documentation
├── src/
│   └── catalog.lisp    # Main implementation
└── tests/
    └── catalog-tests.lisp  # Test suite
```

## Dependencies

- `epsilon.core` - For basic types and utilities
- Uses epsilon's digest, hex, regex, sequence, and string libraries

The catalog module provides a foundation for type-driven development in Epsilon, enabling safer and more maintainable code through explicit type definitions and schema management.