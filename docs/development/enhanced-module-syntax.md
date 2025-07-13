# Enhanced Module Syntax Guide

The Epsilon Module System 2.0 provides a cleaner, more modern syntax for defining packages and managing dependencies. This guide demonstrates the enhanced `module` macro and its capabilities.

## Basic Syntax

### Traditional Common Lisp Package Definition

```lisp
(defpackage :my-app.core
  (:use :cl :epsilon.lib.syntax)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:http :epsilon.http.server))
  (:export
   #:main
   #:config
   #:start-server))

(in-package :my-app.core)
```

### Enhanced Module Definition

```lisp
(module my-app.core
  :use (cl epsilon.lib.syntax)
  :import ((map epsilon.lib.map)
           (str epsilon.lib.string)
           (http epsilon.http.server))
  :export (main config start-server))
```

The enhanced syntax provides:

- **Cleaner keyword-based clauses** instead of verbose s-expressions
- **Automatic package creation and switching** with `in-package`
- **Simplified import syntax** for local nicknames
- **Selective symbol imports** with `from` syntax
- **Automatic symbol conversion** without manual `#:` prefixes

## Import Patterns

### 1. Aliased Imports (Local Nicknames)

```lisp
(module my-app.data
  :use (cl)
  :import ((map epsilon.lib.map)        ; Creates (map epsilon.lib.map) nickname
           (seq epsilon.lib.sequence)   ; Creates (seq epsilon.lib.sequence) nickname
           (json epsilon.lib.json))     ; Creates (json epsilon.lib.json) nickname
  :export (process-data))
```

This creates local nicknames, allowing you to use `map:get`, `seq:filter`, `json:encode`, etc.

### 2. Selective Symbol Imports

```lisp
(module my-app.utilities
  :use (cl)
  :import (((make-map get assoc dissoc) from epsilon.lib.map)
           ((split join trim uppercase) from epsilon.lib.string))
  :export (text-processor data-builder))
```

This imports specific symbols directly into the package namespace, allowing you to use `make-map`, `get`, `split`, `join` without prefixes.

### 3. Mixed Import Styles

```lisp
(module my-app.service
  :use (cl epsilon.lib.syntax)
  :import ((http epsilon.http.server)           ; Aliased import
           (log epsilon.lib.log)                ; Aliased import  
           ((encode decode) from epsilon.lib.json))  ; Selective import
  :export (start-service handle-request))
```

## Advanced Features

### 1. Symbol Shadowing

```lisp
(module my-app.collections
  :use (cl)
  :shadow (map reduce filter count)  ; Shadow CL symbols
  :import ((map epsilon.lib.map))
  :export (process-collection))
```

### 2. Re-exports

```lisp
(module my-app.api
  :use (cl)
  :import ((core my-app.core)
           (db my-app.database))
  :export (start-server)
  :re-export (core:config core:logger db:connect db:query))
```

### 3. Conditional Compilation

```lisp
(module my-app.debug-tools
  :use (cl)
  :when (member :debug *features*)
  :import ((inspector epsilon.tool.inspector))
  :export (debug-session))
```

## Real-World Examples

### Web Application Module Structure

```lisp
;; Configuration module
(module my-web-app.config
  :use (cl)
  :import ((map epsilon.lib.map)
           (json epsilon.lib.json)
           (env epsilon.sys.env))
  :export (load-config get-setting))

;; Database module
(module my-web-app.database
  :use (cl)
  :import ((config my-web-app.config)
           (log epsilon.lib.log))
  :export (connect query insert update))

;; API handlers module
(module my-web-app.handlers
  :use (cl)
  :import ((http epsilon.http.server)
           (json epsilon.lib.json)
           (db my-web-app.database)
           ((validate sanitize) from my-web-app.validation))
  :export (setup-routes user-handler post-handler))

;; Main application module
(module my-web-app.main
  :use (cl)
  :import ((config my-web-app.config)
           (db my-web-app.database)
           (api my-web-app.handlers)
           (log epsilon.lib.log))
  :export (start-application stop-application))
```

### Library Module with Multiple Import Patterns

```lisp
(module my-library.core
  :use (cl epsilon.lib.syntax)
  :import (
    ;; Full module aliases
    (map epsilon.lib.map)
    (seq epsilon.lib.sequence)
    
    ;; Selective symbol imports
    ((encode decode) from epsilon.lib.json)
    ((format-time parse-time) from epsilon.lib.time)
    ((uuid-generate uuid-parse) from epsilon.lib.uuid)
    
    ;; Standard aliases
    (log epsilon.lib.log)
    (str epsilon.lib.string))
  :shadow (time)  ; Shadow CL:TIME
  :export (
    ;; Main API functions
    process-data
    transform-data
    export-data
    
    ;; Utility functions
    current-timestamp
    generate-id
    format-output))
```

## Migration from Traditional Syntax

### Step 1: Convert `defpackage` to `module`

**Before:**
```lisp
(defpackage :my-package
  (:use :cl :epsilon.lib.syntax)
  (:export #:function-1 #:function-2))
(in-package :my-package)
```

**After:**
```lisp
(module my-package
  :use (cl epsilon.lib.syntax)
  :export (function-1 function-2))
```

### Step 2: Simplify Local Nicknames

**Before:**
```lisp
(defpackage :my-package
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)))
```

**After:**
```lisp
(module my-package
  :import ((map epsilon.lib.map)
           (str epsilon.lib.string)))
```

### Step 3: Use Selective Imports Where Appropriate

**Before:**
```lisp
(defpackage :my-package
  (:import-from :epsilon.lib.map #:make-map #:get #:assoc)
  (:import-from :epsilon.lib.string #:split #:join))
```

**After:**
```lisp
(module my-package
  :import (((make-map get assoc) from epsilon.lib.map)
           ((split join) from epsilon.lib.string)))
```

## Best Practices

### 1. Module Organization

- **One module per file** for clarity
- **Use descriptive module names** that reflect functionality
- **Group related functionality** in the same module
- **Keep modules focused** and avoid large, monolithic modules

### 2. Import Strategy

- **Use aliased imports** for modules you'll reference frequently
- **Use selective imports** for just a few symbols from a module
- **Be consistent** with import patterns across your application
- **Avoid importing too many symbols** directly to maintain clarity

### 3. Export Strategy

- **Export only what's needed** by other modules
- **Use clear, descriptive names** for exported symbols
- **Group related exports** together in the export list
- **Document your module's public API**

### 4. Naming Conventions

- **Use hierarchical names** like `my-app.component.subcomponent`
- **Be consistent** with naming patterns across modules
- **Use descriptive names** that indicate purpose
- **Follow Common Lisp conventions** for symbol names

## Integration with Build System

The enhanced module syntax works seamlessly with Epsilon's build system:

```bash
# Build modules using enhanced syntax
./run.sh build --module my-app.core

# Test modules
./run.sh test --module my-app.core --package my-app.*

# Run specific module
./run.sh run --module my-app.main
```

The build system automatically handles dependency resolution and compilation order based on your import declarations.

## Backward Compatibility

The enhanced module system is fully backward compatible:

- **Existing `defpackage` forms** continue to work unchanged
- **`defmodule` alias** provided for gradual migration
- **All Common Lisp package features** remain available
- **Mix and match** old and new syntax as needed

You can migrate gradually by converting one module at a time to the new syntax.