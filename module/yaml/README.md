# Epsilon YAML Module

A YAML parser for Common Lisp supporting a subset of the YAML 1.2 specification.

## Features

- Basic scalar values
- Sequences (lists) with '-' marker
- Mappings (key-value pairs)
- Nested structures (sequences and mappings)
- Multi-line scalar values
- Proper indentation-based structure parsing

## Usage

```lisp
;; Load the module
(epsilon.lib.module:load-module "epsilon.yaml")

;; Parse YAML from string
(yaml:parse-string "key: value")

;; Parse YAML from file
(yaml:parse-file #P"config.yml")
```

## Example

Input YAML:
```yaml
name: Irv
details:
  - age: 30
  - city: Peoria
```

Will produce:
```lisp
(("name" . "Irv")
 ("details" . (("age" . "30")
               ("city" . "Peoria"))))
```

## Limitations

- No support for YAML directives or tags
- No support for anchors and aliases
- No support for complex scalar types (timestamps, binary, etc.)
- No support for flow style (inline arrays/objects)
- No support for document separators

## Dependencies

- epsilon.core