# Getting Started

Quick guide to installing and using Epsilon.

## Installation

### Quick Install (Recommended)

Install Epsilon as a standalone runtime:

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

This installs a complete SBCL runtime with Epsilon preloaded, providing fast startup and zero external dependencies.

### Manual Installation

Download the appropriate release for your platform from [GitHub Releases](https://github.com/jbouwman/epsilon/releases):

- `epsilon-macos-arm64.tar.gz` - macOS Apple Silicon
- `epsilon-macos-x86_64.tar.gz` - macOS Intel  
- `epsilon-linux-x86_64.tar.gz` - Linux x86_64

Extract and add to your PATH:

```bash
tar -xzf epsilon-*.tar.gz
export PATH="$PWD/epsilon:$PATH"
```

## Basic Usage

### Interactive REPL

Start an interactive session with Epsilon loaded:

```bash
epsilon
```

### Command Line Evaluation

```bash
# Simple evaluation
epsilon --eval "(format t \"Hello, Epsilon!\")" --eval "(sb-ext:quit)"

# Using Epsilon libraries
epsilon --eval "(epsilon.lib.map:make-map :a 1 :b 2)" --eval "(sb-ext:quit)"
```

## Your First Epsilon Program

Create a file `hello.lisp`:

```lisp
(defpackage #:hello
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)
    (#:json #:epsilon.lib.json)))

(in-package #:hello)

(defun main ()
  (let ((data (map:make-map 
                :name "Epsilon"
                :features (seq:from-list '("functional" "fast" "modern")))))
    (format t "~A~%" (json:encode data))))

(main)
```

Run it:

```bash
epsilon --load hello.lisp --eval "(sb-ext:quit)"
```

## Core Concepts

### Local Nicknames

Epsilon packages use local nicknames for clean, readable code:

```lisp
(defpackage #:my-app
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)))
```

### Immutable Data Structures

All Epsilon collections are immutable and persistent:

```lisp
(let ((m1 (map:make-map :a 1 :b 2))
      (m2 (map:assoc m1 :c 3)))
  ;; m1 is unchanged, m2 has the new key
  (format t "m1: ~A~%" m1)  ; {:a 1, :b 2}
  (format t "m2: ~A~%" m2)) ; {:a 1, :b 2, :c 3}
```

### Lazy Sequences

Sequences support lazy evaluation and infinite streams:

```lisp
(let ((nums (seq:take 10 (seq:iterate #'1+ 0))))
  (seq:to-list nums))  ; (0 1 2 3 4 5 6 7 8 9)
```

## Development Workflow

### Building from Source

```bash
git clone https://github.com/jbouwman/epsilon.git
cd epsilon
./run.sh build epsilon.core
```

### Running Tests

```bash
./run.sh test epsilon.core
```

### Creating a Distribution

```bash
./scripts/build-runtime.sh
```

## Next Steps

- [Architecture](architecture.md) - Understanding Epsilon's design
- [Core Library](core/data-structures.md) - Functional data structures
- [API Reference](reference/api.md) - Complete function documentation
- [Examples](reference/examples.md) - Real-world usage patterns