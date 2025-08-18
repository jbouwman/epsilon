# Simple Makefile for epsilon-libffi C extension

# Platform detection
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Platform-specific settings
ifeq ($(UNAME_S),Linux)
    PLATFORM = linux
    EXT = so
    LDFLAGS = -shared
endif
ifeq ($(UNAME_S),Darwin)
    PLATFORM = darwin
    EXT = dylib
    LDFLAGS = -dynamiclib -undefined dynamic_lookup
endif

# Compiler selection
ifdef NIX_CC
    # In Nix environment, use the wrapped gcc
    CC = $(NIX_CC)/bin/gcc
else
    # Outside Nix, use clang
    CC = clang
endif

# Build flags
CFLAGS = -fPIC -Wall -O2

# libffi configuration
ifdef NIX_CC
    # Find libffi headers and libs in Nix store
    LIBFFI_DEV := $(shell find /nix/store -maxdepth 1 -name "*libffi*-dev" -type d 2>/dev/null | sort | tail -1)
    LIBFFI_LIB_DIR := $(shell find /nix/store -maxdepth 1 -type d -name "*libffi-3.4*" 2>/dev/null | xargs -I{} sh -c 'test -f {}/lib/libffi.so && echo {}/lib' | head -1)

    ifneq ($(LIBFFI_DEV),)
        CFLAGS += -I$(LIBFFI_DEV)/include
    endif

    ifneq ($(LIBFFI_LIB_DIR),)
        LIBS = -L$(LIBFFI_LIB_DIR) -lffi
    else
        LIBS = -lffi
    endif
else ifeq ($(UNAME_S),Darwin)
    # On macOS, detect Homebrew installation
    # Try brew --prefix first (most reliable)
    BREW_PREFIX := $(shell brew --prefix 2>/dev/null)
    ifneq ($(BREW_PREFIX),)
        # Use brew to find libffi installation
        LIBFFI_PREFIX := $(shell brew --prefix libffi 2>/dev/null)
        ifneq ($(LIBFFI_PREFIX),)
            # Found libffi via Homebrew
            CFLAGS += -I$(LIBFFI_PREFIX)/include
            LIBS = -L$(LIBFFI_PREFIX)/lib -lffi
        else
            # Homebrew exists but libffi not installed, try system
            LIBS = -lffi
        endif
    else
        # No Homebrew, try common locations
        ifneq ($(wildcard /opt/homebrew/opt/libffi),)
            # Apple Silicon Homebrew location
            CFLAGS += -I/opt/homebrew/opt/libffi/include
            LIBS = -L/opt/homebrew/opt/libffi/lib -lffi
        else ifneq ($(wildcard /usr/local/opt/libffi),)
            # Intel Mac Homebrew location
            CFLAGS += -I/usr/local/opt/libffi/include
            LIBS = -L/usr/local/opt/libffi/lib -lffi
        else
            # Fall back to system
            LIBS = -lffi
        endif
    endif
else
    # Other platforms, assume standard locations
    LIBS = -lffi
endif

TARGET = ../lib/libepsilon-libffi-$(PLATFORM)-$(UNAME_M).$(EXT)

# Default target
all: $(TARGET)

# Build the extension
$(TARGET): epsilon-libffi.c epsilon-libffi.h
	@mkdir -p ../lib
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ epsilon-libffi.c $(LIBS)
	@echo "Built: $@"

# Clean
clean:
	rm -f ../lib/libepsilon-libffi-*.so
	rm -f ../lib/libepsilon-libffi-*.dylib

# Show configuration
info:
	@echo "Platform: $(PLATFORM)"
	@echo "Architecture: $(UNAME_M)"
	@echo "Target: $(TARGET)"
	@echo "Compiler: $(CC)"

.PHONY: all clean info
