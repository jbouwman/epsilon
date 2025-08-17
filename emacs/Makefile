# Makefile for Epsilon Emacs Mode

EMACS ?= emacs
TEST_FILES = test/epsilon-mode-test.el
SOURCE_FILES = epsilon-mode.el
TEST_PROJECT = test/test-project

.PHONY: all test test-interactive clean install help

# Default target
all: test

# Run automated tests in batch mode
test:
	@echo "Running Epsilon mode tests..."
	$(EMACS) -batch \
		-L . \
		-l $(TEST_FILES) \
		-f epsilon-run-tests

# Run tests interactively
test-interactive:
	@echo "Running Epsilon mode tests interactively..."
	$(EMACS) -L . -l $(TEST_FILES) \
		--eval "(epsilon-run-tests-interactively)"

# Check syntax of Emacs Lisp files
check-syntax:
	@echo "Checking syntax..."
	$(EMACS) -batch \
		-L . \
		--eval "(progn \
			(setq byte-compile-error-on-warn t) \
			(batch-byte-compile))" \
		$(SOURCE_FILES)

# Byte compile the mode
compile:
	@echo "Byte compiling..."
	$(EMACS) -batch \
		-L . \
		-f batch-byte-compile \
		$(SOURCE_FILES)

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	rm -f *.elc test/*.elc

# Install to user's Emacs directory
install: $(SOURCE_FILES) epsilon-swank.lisp
	@echo "Installing to ~/.emacs.d/lisp/epsilon-mode/..."
	mkdir -p ~/.emacs.d/lisp/epsilon-mode
	cp $(SOURCE_FILES) epsilon-swank.lisp ~/.emacs.d/lisp/epsilon-mode/
	@echo "Installation complete!"
	@echo "Add to your .emacs:"
	@echo "(add-to-list 'load-path \"~/.emacs.d/lisp/epsilon-mode\")"
	@echo "(require 'epsilon-mode)"

# Test the test project structure
test-project:
	@echo "Validating test project structure..."
	@test -f $(TEST_PROJECT)/module.lisp || (echo "Missing module.lisp" && exit 1)
	@test -f $(TEST_PROJECT)/src/test.lisp || (echo "Missing src/test.lisp" && exit 1)
	@test -f $(TEST_PROJECT)/tests/test-tests.lisp || (echo "Missing tests/test-tests.lisp" && exit 1)
	@echo "Test project structure is valid"

# Run CI tests (non-interactive)
ci: test-project check-syntax test
	@echo "All CI tests passed!"

# Package for distribution
package: clean
	@echo "Creating epsilon-mode package..."
	mkdir -p dist/epsilon-mode
	cp $(SOURCE_FILES) epsilon-swank.lisp README.md dist/epsilon-mode/
	cd dist && tar czf epsilon-mode.tar.gz epsilon-mode/
	@echo "Package created: dist/epsilon-mode.tar.gz"

# Development setup
dev-setup:
	@echo "Setting up development environment..."
	@echo "Creating symlink to current directory in ~/.emacs.d/lisp/"
	mkdir -p ~/.emacs.d/lisp
	ln -sf $(PWD) ~/.emacs.d/lisp/epsilon-mode-dev
	@echo "Development setup complete!"
	@echo "Add to your .emacs for development:"
	@echo "(add-to-list 'load-path \"~/.emacs.d/lisp/epsilon-mode-dev\")"

# Help target
help:
	@echo "Available targets:"
	@echo "  test            - Run automated tests"
	@echo "  test-interactive - Run tests interactively"
	@echo "  check-syntax    - Check Emacs Lisp syntax"
	@echo "  compile         - Byte compile files"
	@echo "  clean           - Remove compiled files"
	@echo "  install         - Install to user's Emacs directory"
	@echo "  test-project    - Validate test project structure"
	@echo "  ci              - Run all CI tests"
	@echo "  package         - Create distribution package"
	@echo "  dev-setup       - Set up development environment"
	@echo "  help            - Show this help"