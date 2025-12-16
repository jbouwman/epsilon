.PHONY: build smoke test release clean ci-setup benchmark

# Build all modules
build:
	./epsilon --exec epsilon.release:build

# Run only smoke tests
smoke:
	./epsilon --exec epsilon.release:run-smoke-tests

# Run all tests in all modules
test:
	./epsilon --exec epsilon.release:test

# Create release package
release:
	./epsilon --exec epsilon.release:create-package

# Run benchmarks
benchmark:
	./epsilon --exec epsilon.benchmark:run

# Clean build artifacts
clean:
	rm -rf modules/*/target
	rm -rf target
	rm -rf releases

# CI setup - install platform dependencies
ci-setup:
ifeq ($(shell uname -s),Linux)
	apt-get update && apt-get install -y sbcl libffi-dev libssl-dev build-essential git tar gzip
endif
ifeq ($(shell uname -s),Darwin)
	command -v sbcl >/dev/null || brew install sbcl
	brew list libffi >/dev/null 2>&1 || brew install libffi
	brew list openssl@3 >/dev/null 2>&1 || brew install openssl@3
endif
	$(MAKE) -C modules/foreign/c clean all
