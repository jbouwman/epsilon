.PHONY: test release build smoke version clean setup

# Run all tests in all modules
test:
	./epsilon --exec epsilon.release:test

# Run only smoke tests
smoke:
	./epsilon --exec epsilon.release:run-smoke-tests

# Build all modules
build:
	./epsilon --exec epsilon.release:build

# Create release package
release:
	./epsilon --exec epsilon.release:create-package

# Verify release
verify:
	./epsilon --exec epsilon.release:verify

# Show version
version:
	@./epsilon --exec epsilon.release:version

# Bump version (use with BUMP=major|minor|patch)
bump-version:
	./epsilon --exec "epsilon.release:version :bump-type \"$(BUMP)\""

# Setup CI environment
setup:
	./scripts/ci-setup.sh

# Clean build artifacts
clean:
	rm -rf modules/*/target
	rm -rf target
	rm -rf releases
