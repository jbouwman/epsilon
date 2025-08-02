# Package Source Protocol Design

## Overview

This document outlines the design for a generic-function based protocol system in Epsilon, starting with package sources as the primary example. The goal is to provide clean extension points that enable different implementations (local filesystem, network sources, etc.) while maintaining a consistent interface.

## Motivation

The current module/package system in Epsilon has several limitations:

1. **Tight coupling** - The build system is directly tied to filesystem operations
2. **Limited extensibility** - Adding network package sources requires modifying core code
3. **Terminology confusion** - "Module" and "package" are used interchangeably
4. **No abstraction** - Direct filesystem access throughout the codebase

By introducing protocols, we can:
- Support multiple package sources transparently
- Enable testing with mock implementations
- Allow third-party extensions
- Maintain backward compatibility
- Clarify terminology and concepts

## Core Concepts

### Terminology

**Package**: A set of related source files and resources
- Defined by a `package.lisp` or `package.edn` file
- Has a name, version, dependencies, and metadata
- The unit of distribution, compilation, and dependency management

**Source**: A location that provides access to packages
- Can provide a single package (e.g., a project directory)
- Can provide multiple packages (e.g., epsilon's src/, a package server)
- Provides discovery, retrieval, and metadata services

**Protocol**: A set of generic functions defining an interface
- Allows multiple implementations
- Enables polymorphic behavior
- Self-documenting through generic function documentation

## Architecture

### 1. Core Protocol System (epsilon.protocol)

A new core module that provides protocol infrastructure:

```lisp
;; src/core/src/lib/protocol.lisp
(defpackage :epsilon.protocol
  (:use :cl)
  (:export
   ;; Protocol definition
   #:define-protocol
   #:protocol-version
   #:protocol-documentation
   
   ;; Protocol introspection
   #:list-protocols
   #:protocol-methods
   #:protocol-implementations
   
   ;; Utilities
   #:ensure-protocol-method
   #:default-method-for))
```

### 2. Package Source Protocol

The primary protocol for package access:

```lisp
;; src/core/src/tool/build-protocol.lisp
(defpackage :epsilon.build.protocol
  (:use :cl :epsilon.protocol)
  (:export
   ;; Package access
   #:find-package
   #:list-packages
   #:package-exists-p
   
   ;; Package metadata
   #:load-package-metadata
   #:resolve-package-location
   
   ;; Dependencies
   #:resolve-dependencies
   
   ;; Package retrieval
   #:retrieve-package))

(in-package :epsilon.build.protocol)

(define-protocol package-source-protocol
  (:version "1.0")
  (:documentation "Protocol for accessing packages from various sources"))

;; Core generic functions
(defgeneric find-package (source package-name &key version)
  (:documentation "Find a package by name and optional version"))

(defgeneric list-packages (source &key filter)
  (:documentation "List all packages available from this source"))

(defgeneric package-exists-p (source package-name &key version)
  (:documentation "Check if a package exists in this source"))

(defgeneric load-package-metadata (source package-name)
  (:documentation "Load metadata for a package without loading code"))

(defgeneric resolve-package-location (source package-name)
  (:documentation "Resolve the physical location of a package"))

(defgeneric resolve-dependencies (source package-name &key recursive)
  (:documentation "Resolve all dependencies for a package"))

(defgeneric retrieve-package (source package-name target-location)
  (:documentation "Retrieve a package to a local location"))
```

### 3. Source Implementations

#### Filesystem Source
For single packages (like user projects):

```lisp
(defclass filesystem-source ()
  ((path :initarg :path :accessor source-path)
   (metadata :initform nil :accessor source-metadata)))

(defmethod find-package ((source filesystem-source) package-name &key version)
  ;; Check if package.lisp exists and matches name
  ...)

(defmethod list-packages ((source filesystem-source) &key filter)
  ;; Return single package if it matches filter
  ...)
```

#### Directory Source
For directories containing multiple packages:

```lisp
(defclass directory-source ()
  ((path :initarg :path :accessor source-path)
   (cache :initform nil :accessor source-cache)))

(defmethod find-package ((source directory-source) package-name &key version)
  ;; Search subdirectories for matching package
  ...)

(defmethod list-packages ((source directory-source) &key filter)
  ;; Scan subdirectories for packages
  ...)
```

#### Network Source
For remote package servers:

```lisp
(defclass network-source ()
  ((url :initarg :url :accessor source-url)
   (auth :initarg :auth :initform nil :accessor source-auth)))

(defmethod find-package ((source network-source) package-name &key version)
  ;; Query remote API
  ...)

(defmethod retrieve-package ((source network-source) package-name target-location)
  ;; Download package archive
  ...)
```

#### Composite Source
For combining multiple sources:

```lisp
(defclass composite-source ()
  ((sources :initarg :sources :accessor source-list)
   (strategy :initarg :strategy :initform :first-found)))

(defmethod find-package ((source composite-source) package-name &key version)
  ;; Try each source based on strategy
  ...)
```

### 4. Package Transfer Protocol

Separate protocol for moving packages between sources:

```lisp
(defgeneric publish-package (package source target-source &key metadata)
  (:documentation "Publish a package from one source to another"))

(defgeneric sync-packages (source target-source &key filter)
  (:documentation "Synchronize packages between sources"))
```

## Implementation Phases

### Phase 1: Core Infrastructure
1. Create epsilon.protocol module
2. Implement basic protocol definition macros
3. Add minimal protocol support to boot process
4. Write comprehensive tests

### Phase 2: Package Source Protocol
1. Define package source protocol
2. Implement filesystem-source for single packages
3. Implement directory-source for current behavior
4. Migrate build.lisp to use protocols
5. Ensure boot process remains unchanged

### Phase 3: Extended Sources
1. Implement network-source with HTTP/HTTPS support
2. Add composite-source for source layering
3. Create cache-source for performance
4. Build package publishing tools

### Phase 4: Additional Protocols
1. Test runner protocol
2. Build system protocol
3. Documentation extraction protocol
4. Plugin/extension protocol

## Usage Examples

### Basic Usage

```lisp
;; A user project - single package source
(defparameter *project-source* 
  (make-instance 'filesystem-source :path "/home/user/my-project"))

;; Epsilon's src directory - multiple packages
(defparameter *epsilon-source* 
  (make-instance 'directory-source :path "src/"))

;; Access packages uniformly
(find-package *project-source* "my-app")        ; => <package: my-app>
(find-package *epsilon-source* "epsilon.core")  ; => <package: epsilon.core>
```

### Advanced Usage

```lisp
;; Composite source with fallback
(defparameter *runtime-source*
  (make-instance 'composite-source
    :sources (list *epsilon-source*
                   *project-source*
                   (make-instance 'network-source
                     :url "https://packages.epsilon-lang.org"))
    :strategy :first-found))

;; Retrieve remote package
(retrieve-package *runtime-source* "cool-lib" "/tmp/cool-lib/")

;; Publish local package
(publish-package (find-package *project-source* "my-app")
                 *project-source*
                 (make-instance 'network-source 
                   :url "https://my-packages.com"
                   :auth *my-credentials*))
```

## Backward Compatibility

1. **Current APIs preserved** - Existing functions continue to work
2. **Boot process unchanged** - Simple filesystem access for bootstrap
3. **Opt-in adoption** - Protocols used only where beneficial
4. **Gradual migration** - Convert subsystems incrementally

## Benefits

1. **Extensibility** - Add new source types without modifying core
2. **Testability** - Mock sources for testing
3. **Clarity** - Clear interfaces and documentation
4. **Performance** - Caching and optimization opportunities
5. **Future-proof** - Ready for distributed package ecosystems

## Future Considerations

1. **Package signing** - Cryptographic verification protocol
2. **Version resolution** - Semantic versioning protocol
3. **Binary packages** - Pre-compiled package support
4. **Package search** - Full-text search protocol
5. **Mirror support** - Automatic failover sources

## Conclusion

This protocol-based approach provides the flexibility needed for Epsilon's growth while maintaining simplicity for basic use cases. By starting with package sources and gradually expanding to other areas, we can validate the design and ensure it meets real-world needs.