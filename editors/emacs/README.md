# epsilon.el - Epsilon Language Server Integration for Emacs

An Emacs package for using Epsilon Lisp via the Epsilon Language Server (ELS).

## Features

- **epsilon-mode**: Major mode for Epsilon source files with syntax highlighting and indentation
- **epsilon-repl**: Interactive REPL with comint integration
- **Flymake**: Live diagnostics and error checking
- **Xref**: Go-to-definition (M-.) and find-references (M-?)
- **Eldoc**: Inline function signatures
- **Completion**: Symbol completion via completion-at-point (works with Company, Corfu, etc.)
- **Debugger**: Full debugger with restart handling and stack inspection
- **Project.el**: Workspace and module integration
- **Imenu**: Buffer outline navigation
- **Inspector**: Interactive object inspection
- **Test Runner**: Test discovery and execution

## Requirements

- Emacs 28.1 or later
- Running Epsilon Language Server (ELS)

## Installation

### Manual Installation

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/epsilon/editors/emacs")
(require 'epsilon)
```

### With use-package

```elisp
(use-package epsilon
  :load-path "/path/to/epsilon/editors/emacs"
  :mode ("\\.lisp\\'" . epsilon-mode)
  :commands (epsilon-repl epsilon-connect))
```

## Quick Start

1. Start the ELS server:
   ```bash
   ./epsilon --module epsilon.els --eval "(epsilon.els:start-server)"
   ```

2. In Emacs:
   ```
   M-x epsilon-connect    ; Connect to server
   M-x epsilon-repl       ; Start REPL
   ```

3. Open a `.lisp` file and epsilon-mode activates automatically.

## Key Bindings

### Source Buffer (epsilon-mode)

| Key         | Command                     | Description                |
|-------------|-----------------------------|----------------------------|
| `C-x C-e`   | `epsilon-eval-last-sexp`    | Evaluate sexp before point |
| `C-M-x`     | `epsilon-eval-defun`        | Evaluate top-level form    |
| `C-c C-r`   | `epsilon-eval-region`       | Evaluate region            |
| `C-c C-k`   | `epsilon-compile-file`      | Compile current file       |
| `C-c C-l`   | `epsilon-load-file`         | Load current file          |
| `C-c C-z`   | `epsilon-switch-to-repl`    | Switch to REPL             |
| `M-.`       | `xref-find-definitions`     | Go to definition           |
| `M-,`       | `xref-go-back`              | Go back                    |
| `M-?`       | `xref-find-references`      | Find references            |
| `C-c C-d d` | `epsilon-describe-symbol`   | Describe symbol            |
| `C-c C-d a` | `epsilon-apropos`           | Apropos search             |
| `C-c C-m`   | `epsilon-macroexpand`       | Macroexpand                |
| `C-c C-i`   | `epsilon-inspect`           | Inspect object             |
| `C-c t t`   | `epsilon-run-test-at-point` | Run test at point          |

### REPL (epsilon-repl-mode)

| Key       | Command                         | Description          |
|-----------|---------------------------------|----------------------|
| `C-c C-c` | `epsilon-repl-interrupt`        | Interrupt evaluation |
| `C-c C-z` | `epsilon-repl-switch-to-source` | Switch to source     |
| `C-c C-p` | `epsilon-repl-set-package`      | Set current package  |
| `C-c M-o` | `epsilon-repl-clear-buffer`     | Clear buffer         |

### Debugger (epsilon-debug-mode)

| Key     | Command                          | Description            |
|---------|----------------------------------|------------------------|
| `a`     | `epsilon-debug-abort`            | Abort to top level     |
| `c`     | `epsilon-debug-continue`         | Continue execution     |
| `0-9`   | `epsilon-debug-invoke-restart-N` | Invoke restart N       |
| `s`     | `epsilon-debug-step`             | Step into              |
| `n`     | `epsilon-debug-next`             | Step over              |
| `o`     | `epsilon-debug-step-out`         | Step out               |
| `p/TAB` | Frame navigation                 | Move between frames    |
| `e`     | `epsilon-debug-eval-in-frame`    | Eval in frame context  |
| `v`     | `epsilon-debug-show-locals`      | Toggle local variables |
| `RET`   | `epsilon-debug-goto-source`      | Go to source           |
| `q`     | `epsilon-debug-quit`             | Quit debugger          |

### Inspector (epsilon-inspector-mode)

| Key   | Command                         | Description            |
|-------|---------------------------------|------------------------|
| `RET` | `epsilon-inspector-action`      | Invoke action at point |
| `TAB` | `epsilon-inspector-next-action` | Next action            |
| `l`   | `epsilon-inspector-back`        | Go back                |
| `e`   | `epsilon-inspector-eval`        | Eval with object       |
| `g`   | `epsilon-inspector-refresh`     | Refresh                |
| `q`   | `epsilon-inspector-quit`        | Quit                   |

## Configuration

```elisp
;; Server settings
(setq epsilon-server-host "127.0.0.1")
(setq epsilon-server-port 4141)
(setq epsilon-auto-start-server t)

;; REPL settings
(setq epsilon-repl-prompt "epsilon> ")
(setq epsilon-repl-history-size 500)

;; Flymake settings
(setq epsilon-flymake-check-on-save t)
(setq epsilon-flymake-check-delay 0.5)

;; Eldoc settings
(setq epsilon-eldoc-show-types t)

;; Completion
(setq epsilon-complete-annotation-style 'full)
```

## Running Tests

```elisp
(require 'ert)
(load-file "tests/epsilon-client-tests.el")
(load-file "tests/epsilon-mode-tests.el")
(ert-run-tests-interactively t)
```

## License

MIT License - See LICENSE file for details.
