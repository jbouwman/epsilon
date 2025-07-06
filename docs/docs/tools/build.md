# Build System

Epsilon's dependency-tracking build system with incremental compilation and content-based change detection.

## Overview

The Epsilon build system (`epsilon.tool.build`) provides:

- **Content-based change detection** using SHA-256 hashing
- **Incremental compilation** - only rebuild what changed
- **Dependency resolution** with topological sorting
- **Module-based organization** with `package.yaml` configuration
- **Parallel builds** where dependencies allow

## Basic Usage

### Command Line Interface

```bash
# Build a specific module
./run.sh build epsilon.core

# Build with verbose output
./run.sh build epsilon.core --verbose

# Force rebuild (ignore cache)
./run.sh build epsilon.core --force

# Clean build artifacts
./run.sh build epsilon.core --clean
```

### Programmatic Interface

```lisp
(defpackage #:build-example
  (:use #:common-lisp)
  (:local-nicknames
    (#:build #:epsilon.tool.build)))

(in-package #:build-example)

;; Build a module
(build:build "epsilon.core")

;; Build with options
(build:build "my-module" :force t :verbose t)

;; Get build status
(build:build-status "epsilon.core")
```

## Module Configuration

### package.yaml Structure

Each module requires a `package.yaml` file:

```yaml
name: my-module
description: Example module
version: 1.0.0

# Source directories
sources:
  - src
  - extra-src

# Test directories  
tests:
  - tests
  - integration-tests

# Module dependencies
dependencies:
  - core
  - another-module

# Build configuration
build:
  target-dir: target
  optimize: true
  debug: false
```

### Directory Structure

Standard module layout:

```
my-module/
├── package.yaml          # Module configuration
├── src/                  # Source files
│   ├── package.lisp     # Package definition
│   └── implementation.lisp
├── tests/               # Test files
│   └── tests.lisp
└── target/              # Build artifacts
    ├── fasls/          # Compiled files
    └── cache/          # Build cache
```

## Dependency Management

### Dependency Resolution

The build system automatically resolves dependencies:

```lisp
;; Dependencies are built in correct order
(build:build "app")  ; Builds core → utils → app
```

### Circular Dependency Detection

```lisp
;; Error: Circular dependency detected
(build:build "module-a")  ; → module-b → module-a
```

### Module Graph Visualization

```lisp
(build:show-dependency-graph "epsilon.core")
;; Outputs graphical representation of dependencies
```

## Change Detection

### Content Hashing

Files are hashed to detect changes:

```lisp
(defstruct source-file
  path          ; Full file path
  content-hash  ; SHA-256 of file content
  last-modified ; File modification time
  dependencies) ; Other files this depends on
```

### Incremental Builds

Only changed files and their dependents are rebuilt:

```
File Change: src/utils.lisp
├── Rebuild: utils.lisp → utils.fasl
├── Rebuild: app.lisp (depends on utils)
└── Skip: other-module.lisp (no dependency)
```

### Build Cache

```lisp
;; Cache structure
(defstruct build-cache
  module-name    ; Module being built
  source-hashes  ; Hash of each source file
  dependency-hashes ; Hash of each dependency
  build-time     ; When build completed
  artifacts)     ; Generated files
```

## Advanced Features

### Parallel Compilation

```lisp
;; Configure parallel builds
(build:set-build-options :parallel-jobs 4)

;; Dependencies are built in parallel when possible
(build:build "large-project")
```

### Custom Build Steps

```yaml
# package.yaml
build:
  pre-build:
    - generate-version-file
    - copy-resources
  
  post-build:
    - run-tests
    - create-documentation
```

```lisp
;; Define custom build step
(build:define-build-step generate-version-file (module)
  (with-open-file (stream "src/version.lisp" 
                          :direction :output
                          :if-exists :supersede)
    (format stream "(defconstant +version+ \"~A\")~%" 
            (module-version module))))
```

### Build Profiles

```yaml
# Different configurations for different environments
profiles:
  development:
    build:
      optimize: false
      debug: true
      features: [debugging, profiling]
  
  production:
    build:
      optimize: true
      debug: false
      features: [release]
```

```bash
# Build with specific profile
./run.sh build my-module --profile production
```

## Build Hooks

### Pre/Post Build Actions

```lisp
;; Register build hooks
(build:add-hook :pre-build 
  (lambda (module)
    (format t "Starting build of ~A~%" (module-name module))))

(build:add-hook :post-build
  (lambda (module success-p)
    (if success-p
        (format t "Successfully built ~A~%" (module-name module))
        (format t "Build failed for ~A~%" (module-name module)))))
```

### File Watchers

```lisp
;; Watch for file changes and rebuild automatically
(build:watch "my-module" 
  :callback (lambda (changed-files)
              (format t "Files changed: ~A~%" changed-files)
              (build:build "my-module")))
```

## Integration with Development Tools

### REPL Integration

```lisp
;; Hot reload during development
(build:enable-hot-reload "my-module")

;; Changes are automatically compiled and loaded
;; when files are modified
```

### Editor Integration

```lisp
;; Generate compilation database for IDE support
(build:generate-compile-commands "my-module")
;; Creates compile_commands.json for editors
```

### Test Integration

```lisp
;; Build and run tests
(build:build-and-test "my-module")

;; Run tests only if build succeeds
(when (build:build "my-module")
  (test:run "my-module"))
```

## Performance Optimization

### Build Metrics

```lisp
(build:build "my-module" :collect-metrics t)

;; View build performance
(build:show-metrics "my-module")
```

Sample output:
```
Build Metrics for my-module:
├── Total time: 2.5s
├── Compilation: 2.1s (84%)
├── Dependency resolution: 0.3s (12%)
├── Cache operations: 0.1s (4%)
└── Files processed: 15 (6 files/second)
```

### Memory Usage

```lisp
;; Monitor memory during build
(build:build "large-module" :monitor-memory t)

;; Configure memory limits
(build:set-memory-limit (* 2 1024 1024 1024))  ; 2GB limit
```

### Distributed Builds

```lisp
;; Build across multiple machines (future feature)
(build:build "huge-project" 
  :distributed t
  :workers '("build-server-1" "build-server-2"))
```

## Error Handling

### Build Failures

```lisp
(handler-case
  (build:build "problematic-module")
  (build:compilation-error (e)
    (format t "Compilation failed in ~A:~%~A~%" 
            (build:error-file e)
            (build:error-message e)))
  (build:dependency-error (e)
    (format t "Missing dependency: ~A~%" 
            (build:missing-dependency e))))
```

### Recovery Strategies

```lisp
;; Attempt to fix common issues automatically
(build:build "my-module" :auto-fix t)

;; Clean and rebuild on persistent errors
(build:clean-rebuild "my-module")
```

## Configuration

### Global Build Settings

```lisp
;; Configure global build behavior
(build:configure
  :default-optimize-level 2
  :parallel-jobs (build:cpu-count)
  :cache-directory "~/.epsilon/build-cache"
  :max-cache-size (* 1024 1024 1024))  ; 1GB
```

### Project-Specific Settings

```yaml
# .epsilon-build.yaml (project root)
global:
  cache-dir: .build-cache
  parallel-jobs: 8
  optimize: true

modules:
  core:
    optimize: false  # Override for debugging
  tests:
    compile: false   # Don't compile test files
```

## Examples

### Simple Library Build

```yaml
# mylibrary/package.yaml
name: mylibrary
description: Simple utility library
version: 0.1.0

sources: [src]
tests: [tests]
dependencies: [core]
```

```bash
./run.sh build mylibrary
```

### Complex Application Build

```yaml
# myapp/package.yaml  
name: myapp
description: Web application
version: 2.0.0

sources: 
  - src
  - generated

dependencies:
  - core
  - web-framework
  - database-driver

build:
  pre-build:
    - generate-api-client
    - compile-assets
  post-build:
    - run-integration-tests
    - package-distribution
```

### Multi-Module Project

```
project/
├── core/package.yaml
├── utils/package.yaml
├── web/package.yaml
├── cli/package.yaml
└── tests/package.yaml
```

```bash
# Build entire project
./run.sh build-all

# Build specific component
./run.sh build web
```

---

*Next: [Test Framework](testing.md) - Running and organizing tests*