# Build System

Epsilon uses a content-based build system with dependency tracking and incremental compilation.

## Quick Start

```bash
# Test specific module
./epsilon --test epsilon.core

# Self-test all modules
./epsilon --exec epsilon.release:selftest

# Test with verbose output
./epsilon --test epsilon.core --verbose

# List available modules
./epsilon --modules
```

## Module Structure

Each module requires a `module.lisp` file defining dependencies and exports:

```lisp
(:name "mymodule"
 :version "1.0.0"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core"))
```

Directory layout:
```
mymodule/
├── module.lisp      # Module configuration
├── src/             # Source files
├── tests/           # Test files  
└── target/          # Build artifacts (generated)
```

## Build Process

### Dependency Resolution

Modules are loaded in dependency order:
1. Parse `module.lisp` files
2. Construct dependency graph
3. Topologically sort modules
4. Load in order

Circular dependencies cause build failure.

### Change Detection

Files are hashed with SHA-256 to detect changes:

```
src/foo.lisp (hash: abc123) → target/foo.fasl (hash: def456)
```

Only changed files and their dependents are rebuilt.

### Compilation

SBCL compiles each file:
```lisp
(compile-file "src/foo.lisp" 
              :output-file "target/fasls/foo.fasl"
              :print nil
              :verbose nil)
```

Compilation errors halt the build with file location and error message.

## Build Cache

The build system maintains a cache in `target/build-cache/`:

```
target/build-cache/
├── hashes.sexp      # File content hashes
├── deps.sexp        # Inter-file dependencies
└── times.sexp       # Build timestamps
```

Cache is invalidated when:
- File content changes
- Dependencies change
- Build configuration changes

## Configuration

### Command Line Options

```bash
./epsilon [options]

Options:
  --test MODULE           Test specific module
  --verbose              Show verbose output
  --modules              List available modules
  --eval EXPRESSION      Evaluate expression
  --module MODULE        Load specific module
```

### Testing Profiles

Run tests with different output formats:

```bash
# JUnit XML output
./epsilon --exec epsilon.release:selftest --format junit --file target/TEST-results.xml

# CLI smoke tests
./scripts/smoke.sh
```

## Parallel Builds

Modules without dependencies are built in parallel:

```
epsilon.core ─┬─> epsilon.json ──┐
              ├─> epsilon.yaml ──┼─> epsilon.http
              └─> epsilon.msgpack┘
```

Control with: `EPSILON_BUILD_JOBS=4 ./epsilon build`

## Integration

### With Testing

```bash
# Test specific module
./epsilon --test epsilon.core

# Test with verbose output
./epsilon --test epsilon.core --verbose

# Test specific package within module
./epsilon --test epsilon.core:epsilon.log.tests
```

### Module Development

For development with Epsilon's module system:

```bash
# Load specific module and evaluate
./epsilon --module epsilon.json --eval "(json:encode '(:foo \"bar\"))"

# Interactive REPL with modules loaded
./epsilon

# List all available modules
./epsilon --modules
```

## Troubleshooting

### Common Issues

**Module not found**
```
Error: Module 'foo' not found in registry
```
Check module name in `module.lisp` and ensure module is in the modules directory.

**Circular dependency**
```
Error: Circular dependency: foo -> bar -> foo
```
Restructure modules to eliminate circular references.

**Compilation failure**
```
Error in foo.lisp:42: Undefined function BAR
```
Check function definitions and package exports.

### Debug Testing

```bash
# Test with verbose output
./epsilon --test epsilon.core --verbose

# Test specific test by name
./epsilon --test epsilon.core:epsilon.log.tests:test-detailed-formatter --verbose
```

### Clean Testing

When encountering persistent issues:
```bash
# Remove all artifacts
rm -rf modules/*/target

# Self-test all modules
./epsilon --exec epsilon.release:selftest
```
