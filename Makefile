.PHONY: build smoke test release clean

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

# Clean build artifacts
clean:
	rm -rf modules/*/target
	rm -rf target
	rm -rf releases
