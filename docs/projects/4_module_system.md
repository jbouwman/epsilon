# Module System Enhancement Roadmap

## Current Status

Epsilon uses standard Common Lisp packages with local nicknames for organization. The system works but could benefit from additional module syntax and developer features.

## Phase 1: Simplified Package Definition (1-2 weeks)

### Low Difficulty Implementation
Replace verbose `defpackage` with cleaner `module` syntax that provides:

- **Regular imports**: `(import package...)` for standard package usage
- **Aliased imports**: `(import (package alias)...)` for shortened references  
- **Selective imports**: `(from package symbols...)` for specific symbol importing
- **Export management**: `(export symbols...)` and `(shadow symbols...)` with clear syntax
- **Automatic package entry**: `in-package` handled automatically after definition
- **Pretty-printing support**: Parentheses around all clauses for consistent formatting

### Example Syntax
```lisp
(module epsilon.lsp.server
  "Language Server Protocol implementation"
  
  (import common-lisp
          (epsilon.lib.map map)
          (epsilon.lib.json json)
          (epsilon.sys.thread thread))
  
  (from epsilon.lsp.protocol
        protocol-handler make-jsonrpc-handler
        read-message write-message)
  
  (export lsp-server start-lsp-server stop-lsp-server)
  
  (shadow run)) ; if needed

;; Automatic (in-package epsilon.lsp.server) here
```

### Implementation Details
- **Macro expansion**: `module` expands to appropriate `defpackage` and `in-package` forms
- **Validation**: Check for symbol conflicts and missing dependencies at compile time
- **Documentation**: Extract docstrings and generate module documentation
- **IDE integration**: Support for symbol resolution and completion

## Phase 2: Additional Module Features (2-3 weeks)

### Additional Module Capabilities
- **Conditional imports**: Platform or feature-specific imports
- **Re-exports**: Export symbols from imported packages
- **Module metadata**: Version, author, description in module definition
- **Dependency specification**: Version constraints and optional dependencies
- **Module templates**: Common patterns for different module types

### Example Extended Syntax
```lisp
(module epsilon.net.client
  "HTTP client with platform-specific implementations"
  :version "1.0.0"
  :author "Epsilon Project"
  :depends-on ((epsilon.core ">=1.0.0")
               (epsilon.lib.json ">=0.9.0"))
  
  (import common-lisp
          (epsilon.lib.map map)
          (epsilon.lib.json json))
  
  #+unix
  (from epsilon.sys.posix socket-connect)
  
  #+windows  
  (from epsilon.sys.windows winsock-connect)
  
  (export make-request get post put delete)
  
  (re-export (epsilon.lib.json encode decode)))
```

## Phase 3: Module Discovery and Management (3-4 weeks)

### Module Registry System
- **Module discovery**: Automatic discovery of modules in workspace
- **Dependency resolution**: Calculate load order and detect circular dependencies  
- **Module hot-reloading**: Reload changed modules without restarting
- **Module unloading**: Clean removal of modules and their symbols
- **Dependency tracking**: Track which modules depend on each other

### Development Tools
- **Module analyzer**: Analyze module structure and dependencies
- **Dependency visualizer**: Graphical dependency trees and cycles
- **Module templates**: Code generation for common module patterns
- **Import optimizer**: Automatic cleanup and organization of imports

## Phase 4: Package.yaml Integration (2-3 weeks)

### Unified Module Definition
- **Single source of truth**: Extend package.yaml to include module definitions
- **Build system integration**: Automatic module processing during builds
- **IDE support**: Rich editing experience for module definitions
- **Validation**: Comprehensive checking of module consistency

### Enhanced Package.yaml
```yaml
name: epsilon.lsp
version: 0.2.0
description: Language Server Protocol implementation

modules:
  epsilon.lsp:
    file: src/main.lisp
    exports: [start-server, stop-server]
    imports:
      - common-lisp
      - epsilon.lib.map: map
      - epsilon.lib.json: json
  
  epsilon.lsp.protocol:
    file: src/protocol.lisp
    exports: [protocol-handler, read-message, write-message]
    
dependencies:
  - name: epsilon.core
    version: ">=1.0.0"
  - name: epsilon.net
    version: ">=0.9.0"
    optional: true

sources:
  - src
tests:
  - tests
```

## Technical Implementation

### Core Components
1. **Module macro**: Preprocessor for converting modern syntax to defpackage
2. **Dependency resolver**: Calculate load order and detect issues  
3. **Module registry**: Runtime tracking of loaded modules and their state
4. **Import analyzer**: Static analysis of symbol usage and dependencies
5. **Hot-reload system**: Safe reloading with proper cleanup

### Integration Points
- **epsilon.tool.build**: Integration with build system for automatic processing
- **epsilon.lsp**: IDE support for module editing and navigation
- **epsilon.lib.yaml**: Enhanced package.yaml parsing and validation
- **epsilon.tool.format**: Pretty-printing for module definitions

## Migration Strategy

### Backward Compatibility
- **Gradual adoption**: Existing defpackage forms continue to work
- **Migration tools**: Automatic conversion from defpackage to module syntax
- **Documentation**: Clear migration guides with examples
- **Validation**: Tools to verify migration correctness

### Adoption Path
1. **Phase 1**: Introduce module macro alongside existing defpackage
2. **Phase 2**: Convert core modules to demonstrate best practices
3. **Phase 3**: Provide migration tools for external modules  
4. **Phase 4**: Eventually deprecate raw defpackage in favor of module

## Success Metrics

### Phase 1 Success Criteria
- Module macro provides cleaner syntax than defpackage
- All existing functionality works with new syntax
- Good error messages for common mistakes

### Phase 2 Success Criteria  
- Additional features reduce boilerplate in modules
- Conditional imports handle platform differences
- Re-exports simplify module interfaces

### Phase 3 Success Criteria
- Module discovery works reliably across different project structures
- Hot-reloading enables rapid development iteration
- Dependency analysis catches issues before runtime

### Phase 4 Success Criteria
- Package.yaml serves as single source of truth for module information
- Build system integration eliminates manual module management
- IDE support provides development features

## Future Enhancements

### Additional Features
- **Module federation**: Distributed module loading across networks
- **Sandboxed modules**: Security boundaries between untrusted modules
- **Module versioning**: Side-by-side loading of different module versions
- **Dynamic imports**: Runtime module loading with lazy evaluation

### Development Experience
- **Module playground**: Interactive module development and testing
- **Dependency impact analysis**: Understand effects of module changes
- **Module optimization**: Automatic dead code elimination and tree shaking
- **Cross-module refactoring**: Safe renaming and restructuring across modules