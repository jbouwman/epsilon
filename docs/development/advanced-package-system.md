# Advanced Package System Specification

Epsilon's package system provides dependency management with
versioning, hash pinning, and reproducible builds.

## Core Features

### 1. Versioning

Epsilon supports versioning with flexible version constraints:

```edn
"dependencies" {
  ;; Exact version
  "epsilon.json" "1.2.3"
  
  ;; Caret: Compatible versions (>=1.2.3 <2.0.0)
  "epsilon.core" "^1.2.3"
  
  ;; Tilde: Patch-level changes (>=1.2.3 <1.3.0)
  "epsilon.http" "~1.2.3"
  
  ;; Range constraints
  "epsilon.database" ">=1.0.0 <2.0.0"
  
  ;; Any version (latest)
  "epsilon.testing" "*"
}
```

### 2. Git Hash Pinning

Like Go modules, Epsilon supports pinning dependencies to specific git commits for reproducible builds:

```edn
"dependencies" {
  ;; Git dependency with tag
  "custom-lib" "git+https://github.com/user/repo.git@v1.2.3"
  
  ;; Git dependency with commit hash (reproducible)
  "stable-lib" "git+https://github.com/org/lib.git@abc123def456"
  
  ;; Git dependency with branch
  "dev-lib" "git+https://github.com/team/lib.git@develop"
}
```

### 3. Lock Files for Reproducibility

The `epsilon-lock.edn` file ensures reproducible builds across environments:

```edn
{"version" 1
 "dependencies" {
   "epsilon.core" {
     "version" "2.1.5"
     "checksum" "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4"
     "source" "https://packages.epsilon-lang.org/"
   }
   "custom-lib" {
     "version" "1.2.3"
     "source" "git+https://github.com/user/repo.git"
     "git-hash" "abc123def4567890abcdef1234567890abcdef12"
   }
 }
 "build-metadata" {
   "timestamp" "2024-01-15T10:30:00Z"
   "epsilon-version" "2.1.0"
   "platform" "linux-x86_64"
 }}
```

## Enhanced package.edn Format

### Basic Structure

```edn
{"name" "my-package"
 "version" "1.0.0"
 "description" "Package description"
 "authors" ["Alice <alice@example.com>" "Bob <bob@example.com>"]
 "license" "MIT"
 "homepage" "https://mypackage.example.com"
 "repository" {"type" "git"
               "url" "https://github.com/example/mypackage.git"}
 
 "sources" ["src"]
 "tests" ["tests"]
 "resources" ["resources"]}
```

### Dependency Types

```edn
{;; Regular dependencies
 "dependencies" {
   "epsilon.core" "^2.1.0"
   "epsilon.http" "~1.5.2"
 }
 
 ;; Development dependencies (not included in production)
 "dev-dependencies" {
   "epsilon.testing" "^1.0.0"
   "mock-server" "^2.0.0"
 }
 
 ;; Optional dependencies (installed only when requested)
 "optional-dependencies" {
   "redis-client" "^3.0.0"
   "postgres-driver" "^4.2.0"
 }}
```

### Feature Flags

Control optional functionality and dependencies:

```edn
{"features" {
   ;; Default features
   "default" ["json" "logging"]
   
   ;; All available features
   "full" ["json" "logging" "metrics" "cache" "database"]
   
   ;; Feature definitions
   "json" {
     "description" "JSON serialization support"
     "dependencies" {"epsilon.json" "^1.0.0"}
   }
   
   "cache" {
     "description" "Redis caching support"
     "dependencies" {"redis-client" "^3.0.0"}
   }
 }}
```

### Build Profiles

Different configurations for development, testing, and production:

```edn
{"build" {
   "profiles" {
     "dev" {
       "features" ["default" "dev-tools"]
       "optimization" 0
       "debug" true
     }
     
     "test" {
       "features" ["full"]
       "optimization" 1
       "debug" true
     }
     
     "release" {
       "features" ["default"]
       "optimization" 3
       "debug" false
       "compress" true
     }
   }
   
   "tasks" {
     "test" {
       "command" "run-tests"
       "args" ["--format" "junit"]
     }
     
     "deploy" {
       "command" "deploy-app"
       "env" ["DEPLOY_ENV" "API_KEY"]
     }
   }
 }}
```

## Version Constraints

### Caret Requirements (^)

The caret requirement `^1.2.3` allows SemVer compatible updates:

- `^1.2.3` := `>=1.2.3, <2.0.0`
- `^1.2` := `>=1.2.0, <2.0.0`
- `^1` := `>=1.0.0, <2.0.0`
- `^0.2.3` := `>=0.2.3, <0.3.0` (0.x.y versions are special)
- `^0.0.3` := `>=0.0.3, <0.0.4` (0.0.x versions are special)

### Tilde Requirements (~)

The tilde requirement `~1.2.3` allows patch-level changes:

- `~1.2.3` := `>=1.2.3, <1.3.0`
- `~1.2` := `>=1.2.0, <1.3.0`
- `~1` := `>=1.0.0, <2.0.0`

### Range Requirements

Explicit version ranges:

- `>=1.0.0` - At least version 1.0.0
- `>1.0.0` - Greater than version 1.0.0
- `<2.0.0` - Less than version 2.0.0
- `<=2.0.0` - At most version 2.0.0
- `>=1.0.0, <2.0.0` - Between 1.0.0 and 2.0.0
- `>=1.0.0 <2.0.0` - Same as above (comma optional)

## Dependency Resolution

### Resolution Algorithm

1. **Parse Constraints**: Parse all version constraints from package.edn
2. **Fetch Metadata**: Retrieve available versions from registry/git
3. **Solve Constraints**: Find versions satisfying all constraints
4. **Handle Conflicts**: Report conflicts that cannot be resolved
5. **Generate Lock**: Create epsilon-lock.edn with exact versions

### Git Dependencies

Git dependencies are resolved to specific commits:

1. **Parse Git URL**: Extract repository URL and reference
2. **Clone/Fetch**: Clone repository or fetch updates
3. **Resolve Reference**: Convert branch/tag to commit hash
4. **Pin Hash**: Store full commit hash in lock file

Example resolution:

```edn
;; package.edn
"my-lib" "git+https://github.com/user/lib.git@main"

;; epsilon-lock.edn (after resolution)
"my-lib" {
  "version" "0.0.0+abc123def456"
  "source" "git+https://github.com/user/lib.git"
  "git-hash" "abc123def4567890abcdef1234567890abcdef12"
}
```

## Command Line Interface

### Dependency Management

```bash
# Add a dependency
epsilon add epsilon.http --version "^1.5.0"
epsilon add custom-lib --git https://github.com/user/lib.git@v1.0.0

# Update dependencies
epsilon update                    # Update all to latest compatible
epsilon update epsilon.core       # Update specific dependency
epsilon update --major           # Allow major version updates

# Remove dependencies
epsilon remove redis-client

# Install from lock file
epsilon install --frozen         # Use exact versions from lock file
```

### Feature Management

```bash
# Build with specific features
epsilon build --features "default,metrics"
epsilon build --all-features
epsilon build --no-default-features

# Test with features
epsilon test --features "full"
```

### Lock File Management

```bash
# Generate/update lock file
epsilon lock

# Verify lock file integrity
epsilon verify

# Show outdated dependencies
epsilon outdated
```

## Registry Protocol

### Package Discovery

```bash
# Search packages
GET /api/v1/search?q=json
GET /api/v1/search?author=epsilon-team

# Get package metadata
GET /api/v1/packages/epsilon.json
GET /api/v1/packages/epsilon.json/versions
GET /api/v1/packages/epsilon.json/1.2.3
```

### Package Publishing

```bash
# Publish to registry
POST /api/v1/packages
Authorization: Bearer <token>
Content-Type: application/edn

{"name" "my-package"
 "version" "1.0.0"
 "checksum" "sha256:..."
 "archive_url" "https://..."}
```

## Security Considerations

### Checksum Verification

All dependencies include SHA-256 checksums for integrity:

```edn
"epsilon.core" {
  "version" "2.1.5"
  "checksum" "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4"
}
```

### Supply Chain Security

1. **Lock Files**: Commit epsilon-lock.edn for reproducible builds
2. **Checksum Verification**: Verify all downloaded packages
3. **HTTPS Only**: Require HTTPS for registry communication
4. **Signed Packages**: Support for GPG-signed packages (future)

## Migration Guide

### From Simple package.edn

```edn
;; Old format
{"name" "my-package"
 "version" "1.0.0"
 "dependencies" ["epsilon.core" "epsilon.http"]}

;; New format
{"name" "my-package"
 "version" "1.0.0"
 "dependencies" {
   "epsilon.core" "^2.1.0"
   "epsilon.http" "^1.5.0"
 }}
```

### From Git Submodules

```bash
# Old: Git submodule
git submodule add https://github.com/user/lib.git libs/custom-lib

# New: package.edn dependency
"dependencies" {
  "custom-lib" "git+https://github.com/user/lib.git@main"
}
```

## Best Practices

### Version Constraints

1. **Libraries**: Use flexible constraints (`^1.0.0`)
2. **Applications**: Commit lock files for exact versions
3. **Git Dependencies**: Pin to commit hashes in production

### Dependency Hygiene

1. **Minimize Dependencies**: Only add what you need
2. **Regular Updates**: Keep dependencies current
3. **Security Audits**: Run `epsilon audit` regularly
4. **Feature Flags**: Use optional dependencies for optional features

### Performance

1. **Parallel Downloads**: Dependencies fetched in parallel
2. **Local Cache**: ~/.epsilon/cache for offline development
3. **Incremental Updates**: Only fetch changed dependencies

## Future Enhancements

### Planned Features

1. **Workspace Support**: Monorepo management
2. **Private Registries**: Corporate package hosting
3. **Dependency Vendoring**: Bundle dependencies
4. **Cross-Compilation**: Target different platforms
5. **Package Signing**: GPG signatures for packages

This advanced package system provides Epsilon with modern dependency management capabilities matching or exceeding those found in Go, Rust, and other contemporary languages.
