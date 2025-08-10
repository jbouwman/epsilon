# Build System

Epsilon uses a content-based build system with dependency tracking and incremental compilation.

## Quick Start

```bash
# Build a module
./epsilon build epsilon.core

# Build all modules
./epsilon build

# Clean and rebuild
./epsilon build --clean epsilon.core

# Run a local package during development
./epsilon run package-name [args...]
```

## Module Structure

Each module requires a `package.edn` file:

```edn
{
  "name" "mymodule"
  "version" "1.0.0"
  "sources" ["src"]
  "tests" ["tests"]
  "dependencies" ["epsilon.core"]
}
```

Directory layout:
```
mymodule/
├── package.edn      # Module configuration
├── src/             # Source files
├── tests/           # Test files  
└── target/          # Build artifacts (generated)
```

## Build Process

### Dependency Resolution

Modules are built in dependency order:
1. Parse `package.edn` files
2. Construct dependency graph
3. Topologically sort modules
4. Build in order

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
./epsilon build [module] [options]

Options:
  --verbose        Show compilation output
  --force          Ignore cache, rebuild everything
  --clean          Delete artifacts before building
  --test           Run tests after building
```

### Build Profiles

Create `.epsilon-build.edn` in project root:

```edn
{
  "profiles" {
    "debug" {
      "optimize" 0
      "debug" 3
    }
    "release" {
      "optimize" 3
      "debug" 0
    }
  }
}
```

Use with: `./epsilon build --profile release`

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
# Build and test
./epsilon build --test epsilon.core

# Test specific module after build
./epsilon test --module epsilon.core
```

### Local Package Development

For development of local packages without full module builds:

```bash
# Run package in current directory (auto-detects name from module.lisp)
./epsilon run [args...]

# module.lisp format for run command
# (:name "package-name"
#  :sources ("src")
#  :dependencies ("epsilon.core") 
#  :main "package-name:main")
```

## Troubleshooting

### Common Issues

**Module not found**
```
Error: Module 'foo' not found in registry
```
Check module name in `package.edn` and ensure module is registered.

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

### Debug Build

```bash
# Maximum verbosity
./epsilon --log 'trace:epsilon.tool.build' --build epsilon.core

# Show dependency graph
./epsilon build --show-deps epsilon.core
```

### Clean Build

When encountering persistent issues:
```bash
# Remove all artifacts
rm -rf module/*/target

# Full rebuild
./epsilon build --force
```
