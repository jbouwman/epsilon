# Epsilon Module System 2.0 Specification

This document specifies the next-generation module system for Epsilon, designed to provide a world-class developer experience that surpasses existing package managers while maintaining Epsilon's performance and simplicity principles.

## Core Design Principles

1. **Zero-Friction Development** - Should "just work" with minimal configuration
2. **Incremental Adoption** - Backward compatible with current system
3. **Performance First** - Fast builds, fast startup, minimal overhead
4. **Reproducible Builds** - Deterministic results across environments
5. **Developer Experience** - Modern tooling and excellent error messages
6. **Composable Architecture** - Support for complex multi-module applications

## 1. Enhanced Module Syntax

### 1.1 The `module` Macro

Replace verbose `defpackage` with a concise `module` macro:

```lisp
;; Current (verbose)
(defpackage my-app.core
  (:use cl epsilon.lib.syntax)
  (:local-nicknames
   (map epsilon.lib.map)
   (str epsilon.lib.string)
   (http epsilon.http.server))
  (:export #:main #:config))

;; New (concise)
(module my-app.core
  :use (cl epsilon.lib.syntax)
  :import ((map epsilon.lib.map)
           (str epsilon.lib.string)
           (http epsilon.http.server))
  :export (main config))
```

### 1.2 Advanced Import/Export Patterns

```lisp
;; Conditional imports
(module my-app.database
  :import ((pg postgresql:*) :when :postgresql)
          ((sqlite sqlite3:*) :when :sqlite)
          ((redis redis:*) :optional))

;; Re-exports
(module my-app.api
  :import (my-app.core)
  :re-export (my-app.core:config my-app.core:logger)
  :export (start-server stop-server))

;; Selective imports
(module my-app.utils
  :import ((map:make-map map:get map:assoc) from epsilon.lib.map)
          ((str:split str:join) from epsilon.lib.string))
```

### 1.3 Feature Flags and Conditional Compilation

```lisp
(module my-app.auth
  :features (oauth ldap saml)
  :import ((oauth oauth-lib:*) :when :oauth)
          ((ldap ldap-client:*) :when :ldap)
          ((saml saml-lib:*) :when :saml)
  :export (authenticate authorize))
```

## 2. Advanced Package Definition (package.edn)

### 2.1 Enhanced Metadata Format

```edn
{
  "name" "my-app.core"
  "version" "1.2.3"
  "description" "Core application logic for MyApp"
  "authors" ["John Doe <john@example.com>"]
  "license" "MIT"
  "homepage" "https://github.com/myorg/myapp"
  "repository" {
    "type" "git"
    "url" "https://github.com/myorg/myapp.git"
  }
  
  "dependencies" {
    "epsilon.core" "^2.1.0"
    "epsilon.http" "~1.5.2"
    "epsilon.database" ">=1.0.0 <2.0.0"
  }
  
  "dev-dependencies" {
    "epsilon.testing" "^1.0.0"
    "epsilon.benchmarks" "*"
  }
  
  "optional-dependencies" {
    "redis-client" "^3.0.0"
    "metrics-client" "^1.2.0"
  }
  
  "features" {
    "default" ["json", "logging"]
    "full" ["json", "logging", "metrics", "redis"]
    "minimal" ["json"]
    
    "json" {
      "description" "JSON processing support"
      "dependencies" {"epsilon.json" "^1.0.0"}
    }
    "redis" {
      "description" "Redis caching support"
      "dependencies" {"redis-client" "^3.0.0"}
    }
  }
  
  "build" {
    "profiles" {
      "dev" {
        "features" ["default", "dev-tools"]
        "optimization" 0
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
        "args" ["--format", "junit"]
      }
      "deploy" {
        "command" "deploy-app"
        "env" ["DEPLOY_ENV"]
      }
    }
  }
  
  "sources" ["src/"]
  "tests" ["tests/"]
  "resources" ["resources/"]
  
  "target" {
    "platforms" ["linux", "darwin", "windows"]
    "architectures" ["x86_64", "aarch64"]
  }
}
```

### 2.2 Version Constraints

Support semantic versioning with flexible constraints:

- `"1.2.3"` - Exact version
- `"^1.2.3"` - Compatible version (>=1.2.3 <2.0.0)
- `"~1.2.3"` - Patch-level changes (>=1.2.3 <1.3.0)
- `">=1.0.0 <2.0.0"` - Range constraints
- `"*"` - Any version (latest)
- `"latest"` - Latest stable version

## 3. Module Registry and Package Management

### 3.1 Central Module Registry

```bash
# Search for modules
epsilon search json
epsilon search --tag database
epsilon search --author "john-doe"

# Install modules
epsilon add epsilon.database@^2.1.0
epsilon add redis-client --optional
epsilon add testing-utils --dev

# Update modules
epsilon update
epsilon update epsilon.core
epsilon update --major  # Allow major version updates

# Remove modules
epsilon remove redis-client
epsilon remove --dev testing-utils
```

### 3.2 Private Registries

```bash
# Configure private registry
epsilon registry add mycompany https://packages.mycompany.com/
epsilon registry auth mycompany --token abc123

# Publish to registry
epsilon publish
epsilon publish --registry mycompany
```

### 3.3 Module Resolution Cache

```bash
# Registry mirrors for reliability
epsilon registry add backup https://backup.epsilon-lang.org/

# Offline mode
epsilon build --offline
epsilon test --offline
```

## 4. Workspace and Monorepo Support

### 4.1 Workspace Configuration

```edn
;; epsilon-workspace.edn
{
  "workspace" {
    "name" "myapp-workspace"
    "members" [
      "apps/web-server/*"
      "apps/cli-tool"
      "libs/*"
      "plugins/*"
    ]
    "exclude" [
      "legacy/*"
      "tmp/*"
    ]
  }
  
  "shared-dependencies" {
    "epsilon.core" "^2.1.0"
    "epsilon.testing" "^1.0.0"
  }
  
  "build" {
    "parallel" true
    "cache-shared" true
    "incremental" true
  }
}
```

### 4.2 Workspace Commands

```bash
# Build all workspace members
epsilon build --workspace

# Test specific members
epsilon test --workspace --members "apps/web-server,libs/auth"

# Run task across workspace
epsilon run test --workspace
epsilon run lint --workspace --continue-on-error
```

## 5. Development Experience Enhancements

### 5.1 Hot Reload and Live Development

```bash
# Start development mode with hot reload
epsilon dev
epsilon dev --watch src/ --reload-on-change

# Interactive development server
epsilon repl --hot-reload
epsilon repl --workspace --load-all
```

### 5.2 Module Templates and Scaffolding

```bash
# Create new module from template
epsilon new my-app.auth --template web-service
epsilon new my-lib --template library
epsilon new my-plugin --template plugin

# Custom templates
epsilon template create my-template
epsilon template publish my-template
epsilon new project --template my-org/my-template
```

### 5.3 Dependency Visualization

```bash
# Visualize dependency graph
epsilon deps graph
epsilon deps graph --output deps.svg
epsilon deps graph --format dot

# Analyze dependencies
epsilon deps tree
epsilon deps outdated
epsilon deps audit
epsilon deps why epsilon.json
```

## 6. Build System Enhancements

### 6.1 Build Profiles and Environments

```bash
# Build with specific profile
epsilon build --profile release
epsilon build --profile dev --features "full,debug"

# Environment-specific builds
epsilon build --env production
epsilon build --env staging --profile release
```

### 6.2 Custom Build Tasks

```bash
# Run custom tasks
epsilon run test
epsilon run deploy --env production
epsilon run db:migrate --version latest

# List available tasks
epsilon tasks
epsilon tasks --workspace
```

### 6.3 Distributed and Incremental Builds

```bash
# Enable build cache
epsilon build --cache
epsilon build --cache-remote https://build-cache.mycompany.com/

# Parallel builds
epsilon build --parallel 8
epsilon build --parallel auto

# Remote build execution
epsilon build --remote --workers 4
```

## 7. Integration and Ecosystem

### 7.1 IDE Integration

- **Language Server Protocol (LSP)** integration for module navigation
- **Module dependency visualization** in IDEs
- **Auto-completion** for module imports and exports
- **Real-time error checking** for dependency issues

### 7.2 CI/CD Integration

```yaml
# .github/workflows/epsilon.yml
- name: Build Epsilon Project
  uses: epsilon-lang/build-action@v2
  with:
    profile: release
    cache: true
    tests: true
    
- name: Publish Module
  uses: epsilon-lang/publish-action@v2
  with:
    registry: https://packages.epsilon-lang.org/
    token: ${{ secrets.EPSILON_TOKEN }}
```

### 7.3 Docker Integration

```dockerfile
# Automatic Dockerfile generation
FROM epsilon:2.1-slim
COPY epsilon-lock.edn .
RUN epsilon install --frozen
COPY . .
RUN epsilon build --profile release
CMD ["epsilon", "run", "start"]
```

## 8. Security and Reliability

### 8.1 Dependency Security

```bash
# Security auditing
epsilon audit
epsilon audit --fix
epsilon audit --severity high

# Dependency verification
epsilon verify
epsilon verify --checksum
```

### 8.2 Reproducible Builds

```edn
;; epsilon-lock.edn (auto-generated)
{
  "version" 1
  "dependencies" {
    "epsilon.core" {
      "version" "2.1.5"
      "checksum" "sha256:abc123..."
      "source" "https://packages.epsilon-lang.org/"
    }
  }
  "build-metadata" {
    "timestamp" "2024-01-15T10:30:00Z"
    "epsilon-version" "2.1.0"
    "platform" "linux-x86_64"
  }
}
```

## 9. Migration Path

### 9.1 Backward Compatibility

- Existing `package.edn` files continue to work
- Gradual migration with compatibility warnings
- Legacy import syntax supported alongside new syntax

### 9.2 Migration Tools

```bash
# Migrate existing project
epsilon migrate to-2.0
epsilon migrate check-compatibility
epsilon migrate update-syntax
```

## 10. Implementation Phases

### Phase 1: Core Infrastructure (3-4 weeks)
- Module macro implementation
- Enhanced package.edn parser
- Basic registry client
- Dependency resolution engine

### Phase 2: Developer Experience (3-4 weeks)
- Hot reload system
- Module templates
- Dependency visualization
- Enhanced CLI commands

### Phase 3: Advanced Features (4-5 weeks)
- Workspace support
- Build profiles and tasks
- Registry server implementation
- Security auditing

### Phase 4: Ecosystem Integration (3-4 weeks)
- IDE plugins
- CI/CD integrations
- Docker tooling
- Documentation and examples

This specification provides a comprehensive roadmap for creating a world-class module system that addresses all current pain points while providing features that rival or exceed the best package managers in the industry.