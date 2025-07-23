# Module Management in Epsilon

## Overview

Epsilon provides a powerful module management system that combines fast boot capabilities with flexible build and test commands. The system supports:

- Multiple module builds with a single command
- Force rebuild when external dependencies change
- Module discovery and listing
- Integration with boot cache and EPK packages
- CI/CD-friendly command structure

## Command Line Interface

The new command structure follows the pattern:
```
epsilon [global-options] <command> [command-options]
```

### Build Command

Build one or more modules with dependency tracking:

```bash
# Build a single module
./epsilon build --module epsilon.core

# Build multiple modules
./epsilon build --module epsilon.core,lsp,http

# Force rebuild (ignoring timestamps)
./epsilon build --module epsilon.core --force

# CI-friendly: exit with error code on failure
./epsilon build --module epsilon.core,package || exit 1
```

#### Build Options

- `--module MODULE1[,MODULE2,...]` - Required. Comma-separated list of modules to build
- `--force` - Force rebuild even if sources haven't changed

### Test Command

Test modules with the same pattern as build:

```bash
# Test all available tests
./epsilon test

# Test specific modules
./epsilon test --module epsilon.core,lsp

# Test with pattern matching
./epsilon test --module epsilon.core --test 'parse-*'

# Generate JUnit XML for CI
./epsilon test --module epsilon.core --format junit --file results.xml
```

#### Test Options

- `--module MODULE1[,MODULE2,...]` - Test specific modules
- `--test PATTERN` - Filter tests by name pattern (supports wildcards)
- `--package PATTERN` - Filter tests by package pattern
- `--format FORMAT` - Output format: shell, repl, junit
- `--file FILE` - Write results to file

### Module Command

Manage and inspect modules:

```bash
# List available modules and cache status
./epsilon module --list
```

Output shows:
- All discovered modules in the `module/` directory
- Cache status for each module
- Boot cache location
- EPK search paths

Example output:
```
;;; Available modules:
  core [cached]
  http
  lsp [cached]
  package

;;; Boot cache location: ~/.epsilon/boot-cache/
;;; EPK search paths: ~/.epsilon/packages/, ./target/packages/, ./packages/
```

## Module Discovery

Modules are automatically discovered by scanning the `module/` directory for subdirectories containing `package.edn` files. Platform-specific modules are filtered based on the current OS.

### Module Structure

Each module must have:
```
module/
└── mymodule/
    ├── package.edn      # Module metadata
    ├── src/            # Source code
    └── tests/          # Test files
```

### Package Metadata

The `package.edn` file defines module properties:
```edn
{"name" "mymodule"
 "version" "1.0.0"
 "description" "My module description"
 "platform" "darwin"     ; Optional: darwin, linux, windows
 "sources" ["src"]
 "tests" ["tests"]
 "dependencies" ["epsilon.core"]}
```

## Boot Cache Integration

The module system integrates with the boot cache for fast startup:

1. **First Run**: Compiles and creates cache
2. **Subsequent Runs**: Uses cached combined FASL
3. **Force Rebuild**: Updates cache with `--force`

Cache locations:
- Boot cache: `~/.epsilon/boot-cache/`
- EPK packages: `~/.epsilon/packages/`

## CI/CD Integration

The new command structure is designed for CI/CD pipelines:

### GitHub Actions Example

```yaml
- name: Build modules
  run: |
    ./epsilon build --module epsilon.core,lsp,http
    
- name: Run tests
  run: |
    ./epsilon test --module epsilon.core,lsp,http \
                  --format junit \
                  --file test-results.xml
                  
- name: Upload test results
  uses: actions/upload-artifact@v3
  with:
    name: test-results
    path: test-results.xml
```

### Jenkins Example

```groovy
stage('Build') {
    sh './epsilon build --module epsilon.core,package,http'
}

stage('Test') {
    sh './epsilon test --module epsilon.core,package,http --format junit --file results.xml'
    junit 'results.xml'
}
```

## Force Rebuild Scenarios

Use `--force` when:

1. **External Dependencies Change**:
   - System libraries updated
   - SBCL version changed
   - Environment variables modified

2. **Build Configuration Changes**:
   - Compiler optimization settings
   - Feature flags
   - Build scripts modified

3. **Cache Corruption**:
   - Unexpected build errors
   - Runtime errors after system updates

Example:
```bash
# After updating SBCL
./epsilon build --module epsilon.core --force

# After changing optimization settings
EPSILON_OPTIMIZE=3 ./epsilon build --module epsilon.core --force
```

## Performance Considerations

### Build Performance

- **Incremental Builds**: Only changed files are recompiled
- **Parallel Compilation**: Multiple cores utilized when possible
- **Content Hashing**: Accurate change detection

### Boot Performance

With the optimized boot system:
- **Cold Start**: 2-3 seconds (builds cache)
- **Warm Start**: 0.1-0.2 seconds (uses cache)
- **EPK Boot**: <0.1 second (pre-built package)

## Troubleshooting

### Module Not Found

```bash
./epsilon build --module mymodule
;;; Error: Unknown module: mymodule
```

Solution:
1. Check module exists: `./epsilon module --list`
2. Ensure `package.edn` exists in module directory
3. Verify platform compatibility in `package.edn`

### Force Rebuild Not Working

If `--force` doesn't rebuild:
1. Clear boot cache: `rm -rf ~/.epsilon/boot-cache/`
2. Check file permissions
3. Verify disk space

### Test Failures in CI

For CI environments:
```bash
# Verbose output for debugging
./epsilon --verbose test --module epsilon.core

# Separate build and test steps
./epsilon build --module epsilon.core || exit 1
./epsilon test --module epsilon.core || exit 1
```

## Future Enhancements

Planned improvements:

1. **Module Dependencies**: Automatic dependency resolution
2. **Package Creation**: `./epsilon package --module mymodule`
3. **Module Templates**: `./epsilon new --template library mymodule`
4. **Remote Repositories**: Download modules from registry
5. **Version Management**: Multiple versions of same module
