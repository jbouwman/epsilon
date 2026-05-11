.PHONY: build smoke test release clean ci-setup

# Build the root module
build:
	./epsilon eval t

# Test the root module
test:
	./epsilon test epsilon

# Clean build artifacts
clean:
	rm -rf _build
	rm -rf releases

# Detect platform for vendored SBCL
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_S),Darwin)
  PLATFORM_OS := darwin
else ifeq ($(UNAME_S),Linux)
  PLATFORM_OS := linux
else
  PLATFORM_OS := unknown
endif
ifeq ($(UNAME_M),x86_64)
  PLATFORM_ARCH := x86_64
else ifeq ($(UNAME_M),amd64)
  PLATFORM_ARCH := x86_64
else ifeq ($(UNAME_M),arm64)
  PLATFORM_ARCH := arm64
else ifeq ($(UNAME_M),aarch64)
  PLATFORM_ARCH := arm64
else
  PLATFORM_ARCH := unknown
endif
VENDOR_SBCL := vendor/sbcl/$(PLATFORM_OS)-$(PLATFORM_ARCH)/sbcl
HAS_VENDOR_SBCL := $(shell test -x $(VENDOR_SBCL) && echo yes || echo no)

# CI setup - install platform dependencies
# Requires the custom SBCL fork (github:jbouwman/sbcl). Not vendored; build
# locally with vendor/sbcl/build.sh or rely on Nix (sbcl-epsilon derivation).
ci-setup:
ifeq ($(HAS_VENDOR_SBCL),yes)
	@echo "Using locally-built SBCL: $(VENDOR_SBCL)"
else
	$(error SBCL not found at $(VENDOR_SBCL). Run ./vendor/sbcl/build.sh to build it, or use 'nix develop' which provides sbcl-epsilon automatically.)
endif
ifeq ($(UNAME_S),Linux)
	apt-get update && apt-get install -y libssl-dev build-essential git tar gzip
endif
