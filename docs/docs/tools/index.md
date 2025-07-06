# Tools Overview

Epsilon includes development tools for building, testing, and maintaining Lisp projects.

## Available Tools

- **[Build System](build.md)** - Dependency tracking and incremental compilation
- **[Test Framework](testing.md)** - Unit testing with metrics and reporting
- **[Benchmarks](benchmarks.md)** - Performance measurement and comparison

## Command Line Interface

All tools are accessible through the `epsilon` command:

```bash
# Build your project
epsilon build

# Run tests
epsilon test

# Run specific test suite
epsilon test --package my.package.tests

# Run benchmarks
epsilon benchmark --suite performance

# Get help
epsilon --help
```

## Project Structure

Tools work with projects organized as modules:

```
my-project/
├── package.yaml          # Module configuration
├── src/
│   └── my-package.lisp   # Source code
└── tests/
    └── my-package-tests.lisp  # Tests
```

## Integration

The tools integrate with:

- **CI/CD systems** - JUnit XML output for test results
- **IDEs** - Language Server Protocol support
- **Package managers** - Standard project layouts
- **Version control** - Git-aware build system