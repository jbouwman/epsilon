.PHONY: test release build smoke version clean setup benchmark benchmark-quick benchmark-all benchmark-baseline benchmark-compare benchmark-list benchmark-help

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
	rm -rf benchmarks/baselines

# Benchmarking targets

# Run default benchmarks
benchmark:
	@./scripts/benchmark.sh run

# Run quick benchmark suite for smoke testing
benchmark-quick:
	@./scripts/benchmark.sh quick

# Run all benchmark suites
benchmark-all:
	@./scripts/benchmark.sh all

# Save current benchmarks as baseline
benchmark-baseline:
	@./scripts/benchmark.sh baseline

# Compare current benchmarks with baseline
benchmark-compare:
	@./scripts/benchmark.sh compare

# List available benchmark suites
benchmark-list:
	@./scripts/benchmark.sh list

# Show benchmark help
benchmark-help:
	@./scripts/benchmark.sh help
