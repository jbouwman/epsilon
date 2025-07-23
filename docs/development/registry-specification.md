# Epsilon Package Registry Specification

The package registry provides package management with versioning, hash
pinning, caching, and reproducible builds.

## Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   CLI Client    │    │  Local Cache    │    │ Package Registry│
│                 │    │                 │    │                 │
│ epsilon add     │◄──►│  ~/.epsilon/    │◄──►│ packages.       │
│ epsilon update  │    │  cache/         │    │ epsilon-lang.   │
│ epsilon install │    │                 │    │ org             │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                       │                       │
        │                       │                       │
        ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ package.edn     │    │ Content Store   │    │ Mirror/Backup   │
│ epsilon-lock.   │    │ (SHA-256)       │    │ Registries      │
│ edn             │    │                 │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Core Components

### 1. Package Registry Client

The registry client handles communication with package repositories:

```lisp
;; Configure registries
(reg:add-registry "official" 
                  "https://packages.epsilon-lang.org/api/v1"
                  :primary t)

(reg:add-registry "corporate" 
                  "https://packages.company.com/api/v1"
                  :auth-token "your-token")

;; Search packages
(reg:search-packages "json")
;; Returns: list of package-info structures

;; Get package information
(reg:get-package-info "epsilon.json")
;; Returns: detailed package metadata

;; Get available versions
(reg:get-package-versions "epsilon.core")
;; Returns: list of version information
```

### 2. Local Package Cache

Efficient local caching with content-addressed storage:

```
~/.epsilon/cache/
├── packages/
│   ├── epsilon.core/
│   │   ├── 2.1.0/          # Package content
│   │   ├── 2.1.1/
│   │   └── 2.1.5/
│   └── epsilon.json/
│       ├── 1.2.0/
│       └── 1.2.3/
├── metadata/
│   ├── epsilon.core.json   # Cached metadata
│   └── epsilon.json.json
└── git-repos/
    └── github.com/
        └── user/
            └── repo/       # Git dependencies
```

Cache operations:

```lisp
;; Check if package is cached
(reg:is-cached-p "epsilon.core" "2.1.0")

;; Cache management
(reg:cache-size)           ; Get cache size in bytes
(reg:clear-cache)          ; Clear entire cache
(reg:gc-cache :max-age-days 30)  ; Remove old entries
```

### 3. Dependency Resolution Engine

Advanced constraint solving with support for:

- **Semantic versioning**: `^1.2.3`, `~1.2.3`, `>=1.0.0 <2.0.0`
- **Git dependencies**: `git+https://github.com/user/repo.git@commit`
- **Conflict detection**: Identify incompatible version constraints
- **Optimal selection**: Choose best versions satisfying all constraints

```lisp
;; Resolve all dependencies
(reg:resolve-dependencies package-definition)
;; Returns: map of package-name -> resolved-version

;; Check for conflicts
(reg:check-conflicts resolved-dependencies)
;; Returns: list of conflicts or nil
```

## Registry API Protocol

### Package Discovery

```http
GET /api/v1/search?q=json&limit=20
Accept: application/json

Response:
{
  "packages": [
    {
      "name": "epsilon.json",
      "description": "Fast JSON parsing",
      "authors": ["Epsilon Team"],
      "versions": ["1.2.3", "1.2.2"],
      "homepage": "https://epsilon-lang.org/json",
      "license": "MIT",
      "keywords": ["json", "parsing"]
    }
  ],
  "total": 1
}
```

### Package Information

```http
GET /api/v1/packages/epsilon.json
Accept: application/json

Response:
{
  "name": "epsilon.json",
  "description": "Fast JSON parsing and generation",
  "authors": ["Epsilon Team <team@epsilon-lang.org>"],
  "homepage": "https://epsilon-lang.org/json",
  "repository": "https://github.com/epsilon-lang/json",
  "license": "MIT",
  "versions": ["1.2.3", "1.2.2", "1.2.1"],
  "keywords": ["json", "parsing", "serialization"],
  "created_at": "2024-01-01T00:00:00Z",
  "updated_at": "2024-01-15T10:30:00Z"
}
```

### Version Information

```http
GET /api/v1/packages/epsilon.json/versions
Accept: application/json

Response:
{
  "versions": [
    {
      "version": "1.2.3",
      "checksum": "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4",
      "dependencies": {
        "epsilon.core": "^2.1.0"
      },
      "archive_url": "https://packages.epsilon-lang.org/epsilon.json/1.2.3/archive.tar.gz",
      "published_at": 1705320600,
      "yanked": false
    }
  ]
}
```

### Package Publishing

```http
POST /api/v1/packages
Authorization: Bearer your-token
Content-Type: application/json

{
  "name": "my-package",
  "version": "1.0.0",
  "checksum": "sha256:...",
  "dependencies": {
    "epsilon.core": "^2.1.0"
  },
  "description": "My awesome package",
  "authors": ["Developer <dev@example.com>"],
  "license": "MIT",
  "archive_url": "https://uploads.epsilon-lang.org/my-package-1.0.0.tar.gz"
}

Response:
{
  "status": "published",
  "package": "my-package",
  "version": "1.0.0"
}
```

## CLI Commands

### Dependency Management

```bash
# Search for packages
epsilon search json
epsilon search --author epsilon-team

# Add dependencies
epsilon add epsilon.json --version "^1.2.0"
epsilon add custom-lib --git https://github.com/user/lib.git@main
epsilon add dev-tools --dev
epsilon add redis-client --optional

# Remove dependencies
epsilon remove redis-client
epsilon remove dev-tools --dev

# Update dependencies
epsilon update                    # Update all to latest compatible
epsilon update epsilon.core       # Update specific package
epsilon update --major           # Allow major version updates
epsilon update --dry-run         # Show what would be updated

# Install dependencies
epsilon install                  # Install from package.edn
epsilon install --frozen         # Install exact versions from lock file
epsilon install my-package       # Install specific package
```

### Registry Management

```bash
# Add registries
epsilon registry add corporate https://packages.company.com/api/v1
epsilon registry add backup https://backup.epsilon.org/api/v1

# Authenticate with registry
epsilon registry auth corporate your-token

# List registries
epsilon registry list

# Remove registry
epsilon registry remove corporate
```

### Cache Management

```bash
# Show cache information
epsilon cache info

# Clear cache
epsilon cache clear

# Garbage collect old entries
epsilon cache gc --days 30
```

### Lock File Management

```bash
# Generate lock file
epsilon lock

# Verify lock file integrity
epsilon verify

# Show outdated packages
epsilon outdated
```

## Configuration

### Registry Configuration

Registries are configured in `~/.epsilon/config.edn`:

```edn
{
  "registries" [
    {
      "name" "official"
      "url" "https://packages.epsilon-lang.org/api/v1"
      "primary" true
    }
    {
      "name" "corporate"
      "url" "https://packages.company.com/api/v1"
      "auth-token-env" "CORPORATE_REGISTRY_TOKEN"
      "mirrors" [
        "https://mirror1.company.com/api/v1"
        "https://mirror2.company.com/api/v1"
      ]
    }
  ]
  
  "cache" {
    "dir" "~/.epsilon/cache"
    "max-size-gb" 10
    "gc-days" 30
  }
  
  "network" {
    "timeout" 30
    "retries" 3
    "parallel-downloads" 4
  }
  
  "security" {
    "verify-checksums" true
    "require-https" true
  }
}
```

### Environment Variables

```bash
# Override cache directory
export EPSILON_CACHE=/custom/cache/path

# Registry authentication
export CORPORATE_REGISTRY_TOKEN=your-token

# Offline mode
export EPSILON_OFFLINE=true

# Parallel downloads
export EPSILON_PARALLEL_DOWNLOADS=8
```

## Security Features

### 1. Checksum Verification

All packages include SHA-256 checksums for integrity verification:

```lisp
;; Automatic verification during download
(reg:download-package "epsilon.json" "1.2.3")
;; Verifies: sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4

;; Manual verification
(reg:verify-checksum content expected-checksum)
```

### 2. HTTPS Enforcement

- All registry communication requires HTTPS
- TLS certificate validation enforced
- Man-in-the-middle attack prevention

### 3. Authentication

- Token-based authentication for private registries
- Environment variable support for CI/CD
- Per-registry authentication configuration

### 4. Reproducible Builds

Lock files ensure exact same dependencies across environments:

```edn
;; epsilon-lock.edn
{
  "version" 1
  "dependencies" {
    "epsilon.core" {
      "version" "2.1.5"
      "checksum" "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4"
      "source" "https://packages.epsilon-lang.org/"
    }
    "custom-lib" {
      "version" "1.0.0+git.abc123"
      "source" "git+https://github.com/user/lib.git"
      "git-hash" "abc123def4567890abcdef1234567890abcdef12"
    }
  }
  "build-metadata" {
    "timestamp" "2024-01-15T10:30:00Z"
    "epsilon-version" "2.1.0"
    "platform" "linux-x86_64"
  }
}
```

## Performance Optimizations

### 1. Parallel Downloads

Multiple packages downloaded concurrently:

```lisp
(setf reg:*parallel-downloads* 4)  ; Download 4 packages at once
```

### 2. Content-Addressed Caching

- Packages cached by content hash
- Eliminates duplicate downloads
- Efficient cache lookups

### 3. Incremental Updates

- Only download changed packages
- Metadata caching reduces API calls
- Delta updates for large packages

### 4. Offline Mode

Complete offline development support:

```lisp
;; Enable offline mode
(setf reg:*offline-mode* t)

;; All operations use cache only
(reg:search-packages "json")      ; Searches cached metadata
(reg:install-dependencies pkg)    ; Uses only cached packages
```

## Error Handling

### Network Errors

- Automatic retry with exponential backoff
- Fallback to mirror registries
- Graceful degradation to offline mode

### Version Conflicts

```lisp
;; Clear conflict reporting
(reg:resolve-dependencies package-def)
;; Error: Dependency conflicts:
;;   epsilon.core: ^2.0.0 (required by app) conflicts with ~1.9.0 (required by lib)
```

### Missing Dependencies

```lisp
;; Helpful error messages
(reg:download-package "nonexistent" "1.0.0")
;; Error: Package 'nonexistent' not found in any configured registry
;;   Searched: official, corporate
;;   Suggestion: Check package name spelling
```

## Registry Server Implementation

### Database Schema

```sql
-- Packages table
CREATE TABLE packages (
    name VARCHAR(255) PRIMARY KEY,
    description TEXT,
    homepage VARCHAR(500),
    repository VARCHAR(500),
    license VARCHAR(100),
    created_at TIMESTAMP,
    updated_at TIMESTAMP
);

-- Package versions table  
CREATE TABLE package_versions (
    package_name VARCHAR(255),
    version VARCHAR(100),
    checksum VARCHAR(100),
    archive_url VARCHAR(500),
    dependencies JSONB,
    published_at TIMESTAMP,
    yanked BOOLEAN DEFAULT FALSE,
    PRIMARY KEY (package_name, version)
);

-- Package authors table
CREATE TABLE package_authors (
    package_name VARCHAR(255),
    author_name VARCHAR(255),
    author_email VARCHAR(255)
);
```
