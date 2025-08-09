# Anagram Service

[![Linux](https://github.com/jbouwman/anagram/actions/workflows/test-linux.yml/badge.svg)](https://github.com/jbouwman/anagram/actions/workflows/test-linux.yml)
[![macOS](https://github.com/jbouwman/anagram/actions/workflows/test-macos.yml/badge.svg)](https://github.com/jbouwman/anagram/actions/workflows/test-macos.yml)

A web service for generating anagrams.

## Overview

This project implements an anagram generation web service using
epsilon's libraries for string manipulation, JSON handling, network
programming and data structures.

## Project Structure

```
anagram/
├── src/
│   └── anagram.lisp        # Service implementation
├── tests/
│   └── anagram-tests.lisp  # Test suite
└── module.lisp             # Module definition
```

## Prerequisites

- Epsilon runtime (either in PATH or checked out locally in `epsilon/`)

## Running

```bash
epsilon run

# With custom port
epsilon run --port 3000
```

## API Endpoints

### GET /
HTML interface for generating anagrams.

### GET /health
Health check endpoint.

**Response:**
```json
{"status": "healthy"}
```

### POST /api/anagram
Generate an anagram from input text.

**Request:**
```json
{"text": "hello world"}
```

**Response:**
```json
{
  "original": "hello world",
  "anagram": "doll howler"
}
```

## Testing

Run the test suite:

```bash
epsilon test
```
