# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an anagram generation web service written in Common Lisp using the Epsilon framework. The service provides both a web interface and REST API for generating anagrams from input text.

## Essential Commands

### Running the Service
```bash
epsilon run                    # Start on default port 8080
epsilon run --port 3000       # Start on custom port
```

### Testing
```bash
epsilon test                   # Run the complete test suite
```

### Development Environment
```bash
nix develop                    # Enter Nix development shell (if using Nix)
```

## Architecture

### Core Components

- **Main Service** (`src/anagram.lisp`): Single-file web service containing:
  - Anagram generation logic using character shuffling
  - Web handlers for HTML interface, health check, and API endpoints
  - Route definitions and middleware setup
  - Server startup and configuration

- **Test Suite** (`tests/anagram-tests.lisp`): Comprehensive unit and integration tests covering:
  - Core anagram generation functions
  - HTTP endpoints and error handling
  - JSON request/response validation

### Epsilon Framework Usage

The service uses several Epsilon libraries:
- `epsilon.web` - Web framework with routing and middleware
- `epsilon.http` - HTTP server and client functionality  
- `epsilon.json` - JSON parsing and generation
- `epsilon.string` - String manipulation utilities
- `epsilon.sequence` - Sequence operations
- `epsilon.map` - Hash map data structures

### API Endpoints

- `GET /` - HTML interface for interactive anagram generation
- `GET /health` - Health check returning `{"status": "healthy"}`
- `POST /api/anagram` - Generate anagram from JSON `{"text": "input"}`

### Package Structure

The main package (`anagram`) exports:
- `main` - Service entry point
- `compute-anagram` - Core anagram generation function

## Dependencies

- Epsilon runtime (installed via install script or available in PATH)
- SBCL (Steel Bank Common Lisp)
- Node.js/npm (for development environment only)

## Testing Strategy

Tests run against a live service instance on port 8081 and verify:
- Core algorithm correctness
- HTTP endpoint behavior
- Error handling and validation
- JSON response formats