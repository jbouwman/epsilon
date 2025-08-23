# Epsilon Emacs Mode

An Emacs minor mode for developing with the Epsilon Lisp framework. Provides project management, module loading, and test execution integrated with SLIME/SWANK.

## Features

- **Project Management**: Automatic discovery of Epsilon projects (directories with `module.lisp` files)
- **Module Search Path Configuration**: Set up and manage module search paths for the Epsilon loader
- **Test Execution**: Run individual tests at point or all tests for a module
- **SLIME Integration**: Extends SLIME/SWANK with Epsilon-specific functionality
- **Module Loading**: Load Epsilon modules through the integrated loader

## Installation

1. Clone or copy the `epsilon-mode` files to your Emacs configuration directory:

```bash
# Copy to your Emacs config directory
cp -r epsilon-mode ~/.emacs.d/lisp/
```

2. Add to your Emacs configuration:

```elisp
;; Add to load path
(add-to-list 'load-path "~/.emacs.d/lisp/epsilon-mode")

;; Load epsilon-mode
(require 'epsilon-mode)

;; Optional: Set up automatic activation for Lisp files in Epsilon projects
(add-hook 'lisp-mode-hook 
          (lambda ()
            (when (epsilon-find-projects default-directory)
              (epsilon-mode 1))))
```

3. Start SLIME and load the Epsilon SWANK extensions:

```elisp
M-x slime
M-x epsilon-setup
```

## Quick Start

1. **Select a Project**: `C-c C-e p` to choose an Epsilon project
2. **Run a Test**: Position cursor on a `deftest` form and press `C-c C-e t`
3. **Load Module**: `C-c C-e l` to load the current module
4. **Run All Module Tests**: `C-c C-e T` to run all tests for current module

## Key Bindings

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c C-e p` | `epsilon-select-project` | Select an Epsilon project |
| `C-c C-e t` | `epsilon-run-test-at-point` | Run test at cursor |
| `C-c C-e T` | `epsilon-run-module-tests` | Run all module tests |
| `C-c C-e l` | `epsilon-load-current-module` | Load current module |
| `C-c C-e s` | `epsilon-configure-search-path` | Configure module search path |
| `C-c C-e j` | `epsilon-jump-to-test` | Jump to test definition |
| `C-c C-e r` | `epsilon-refresh-projects` | Refresh project list |

## Project Structure

Epsilon projects are identified by the presence of a `module.lisp` file. A typical project structure:

```
my-epsilon-project/
├── module.lisp           # Project metadata
├── src/
│   └── my-module.lisp    # Source files
└── tests/
    └── my-tests.lisp     # Test files with (deftest ...) forms
```

### Example `module.lisp`:

```lisp
(:name "my-project"
 :version "1.0.0"
 :description "An example Epsilon project"
 :author "Your Name")
```

## Test Integration

The mode integrates with the Epsilon test framework and recognizes `deftest` forms:

```lisp
(deftest test-my-function
  "Test description"
  (is (= (my-function 2 3) 5))
  (is (string= (my-function "hello") "HELLO")))
```

### Test Results

Test results are displayed in a dedicated `*Epsilon Tests*` buffer with:
- Pass/fail status with ✓/✗ indicators
- Execution time for each test
- Error messages for failed tests
- Navigation back to test definitions

## Configuration

### Customization Variables

```elisp
;; Path to epsilon executable
(setq epsilon-executable "/path/to/epsilon")

;; Default module search paths (relative to project root)
(setq epsilon-default-module-paths '("./modules" "../modules" "~/epsilon/modules"))

;; Test buffer name
(setq epsilon-test-buffer-name "*My Epsilon Tests*")
```

### Module Search Path

Configure the module search path either:

1. Interactively: `C-c C-e s`
2. Via configuration:

```elisp
(setq epsilon-default-module-paths 
      '("./modules" 
        "../modules" 
        "~/my-epsilon-libs"
        "/usr/local/lib/epsilon/modules"))
```

## SWANK Backend

The mode includes a SWANK backend (`epsilon-swank.lisp`) that provides:

- **Environment Configuration**: Set up Epsilon environment in the REPL
- **Module Loading**: Load modules via the Epsilon loader
- **Test Execution**: Execute tests and return structured results
- **Test Discovery**: Find test definitions in files

### Backend Functions

Available RPC functions (called automatically by the Emacs mode):

- `epsilon-swank:configure-environment`
- `epsilon-swank:load-module`
- `epsilon-swank:run-test`
- `epsilon-swank:run-module-tests`
- `epsilon-swank:find-test-definitions`

## Testing the Mode

The mode includes  automated tests using ERT (Emacs Lisp Regression Testing):

```bash
# Run tests from command line
emacs -batch -l test/epsilon-mode-test.el -f epsilon-run-tests

# Or interactively in Emacs
M-x epsilon-run-tests-interactively
```

### Test Project

A test project is included in `test/test-project/` with:
- Sample module structure
- Test functions for various scenarios
- Mock data for testing project discovery

## Development

### File Structure

```
epsilon-mode/
├── epsilon-mode.el           # Main mode implementation
├── epsilon-swank.lisp        # SWANK backend extensions
├── README.md                 # This file
└── test/
    ├── epsilon-mode-test.el  # Automated tests
    └── test-project/         # Test project structure
        ├── module.lisp
        ├── src/test.lisp
        └── tests/test-tests.lisp
```

### Contributing

1. Write tests for new functionality
2. Ensure all tests pass: `M-x epsilon-run-tests-interactively`
3. Follow Emacs Lisp coding conventions
4. Update documentation

## Troubleshooting

### Common Issues

**"SLIME not connected"**
- Start SLIME: `M-x slime`
- Load SWANK extensions: `M-x epsilon-setup`

**"No Epsilon projects found"**
- Ensure your directory contains a `module.lisp` file
- Refresh project list: `C-c C-e r`

**"Module could not be loaded"**
- Check module search path: `C-c C-e s`
- Verify epsilon executable path: check `epsilon-executable` variable

**Test execution fails**
- Ensure the test module is loaded
- Check that test functions follow `deftest` convention
- Verify SWANK backend is loaded: `M-x epsilon-setup`

### Debug Mode

Enable debug information:

```elisp
(setq debug-on-error t)
(setq slime-log-events t)
```

## License

Same license as the Epsilon framework.

## See Also

- [Epsilon Framework Documentation](../docs/)
- [SLIME Documentation](https://slime.common-lisp.dev/)
- [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)