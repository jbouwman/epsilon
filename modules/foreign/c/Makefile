# Makefile for epsilon-libffi extension

# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -fPIC -O2 -g
LDFLAGS = -shared
LIBS = -lffi

# Target library
TARGET = libepsilon-libffi.so
SOURCES = epsilon-libffi.c
OBJECTS = $(SOURCES:.c=.o)
HEADERS = epsilon-libffi.h

# Default target
all: $(TARGET)

# Build shared library
$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

# Compile object files
%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Test the library
test: $(TARGET)
	@echo "Testing libffi availability..."
	@pkg-config --exists libffi && echo "libffi found" || echo "libffi not found"
	@echo "Library built successfully: $(TARGET)"

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(TARGET)

# Install (placeholder)
install: $(TARGET)
	@echo "Install target not implemented yet"

# Show build info
info:
	@echo "CC: $(CC)"
	@echo "CFLAGS: $(CFLAGS)"
	@echo "LDFLAGS: $(LDFLAGS)"
	@echo "LIBS: $(LIBS)"
	@echo "TARGET: $(TARGET)"
	@echo "SOURCES: $(SOURCES)"

.PHONY: all test clean install info