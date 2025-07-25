# LSP Server Implementation Roadmap

## Current Status: Phase 1 Complete

The Language Server Protocol implementation for Epsilon has successfully completed its foundational phase and is ready for advanced feature development.

### Phase 1 Completed: Foundation & Core Protocol (2-3 weeks)

**Implemented Components:**

1. **Protocol Handler** - Complete JSON-RPC 2.0 implementation
   - Message parsing, serialization, and LSP transport (Content-Length headers)
   - Request, response, notification, and error message handling  
   - Abstract protocol handler supporting multiple RPC protocols
   - Error codes and proper LSP specification compliance

2. **Core LSP Server** - Basic lifecycle and document management
   - Initialize/shutdown lifecycle with capability negotiation
   - Text document synchronization (didOpen, didChange, didClose)
   - Method dispatch system for LSP requests and notifications
   - Separation between protocol and implementation logic

3. **Code Analysis Engine** - Foundation for semantic understanding
   - Symbol extraction from Lisp code (functions, variables, packages)
   - Document analysis with position tracking and range information
   - Syntax error detection using the reader
   - Utility functions for position/offset conversion

4. **Workspace Management** - Multi-project support
   - Multi-folder workspace tracking
   - Document-to-analysis mapping with symbol indexing
   - Search capabilities for symbols across the workspace
   - Foundation for cross-file analysis

5. **Build Integration** - Integration with Epsilon
   - Proper module structure with package.yaml configuration
   - Builds successfully within Epsilon's build system (8 files compiled)
   - Test infrastructure with unit tests for core components
   - Distributed with standalone runtime distributions

**Current Capabilities:**
- Basic LSP client communication via JSON-RPC 2.0
- Document synchronization and change tracking
- Symbol extraction and workspace indexing
- Syntax error detection and reporting
- Ready for IDE feature implementation

---

## 🚧 Phase 2: Code Analysis & IDE Features (3-4 weeks)

**Goal**: IDE features with semantic analysis of Lisp code

### 2.1 Code Analysis Improvements (1-2 weeks)
- **AST-based parsing**: Replace regex-based symbol extraction with proper AST analysis
- **Scope analysis**: Track lexical scopes, let bindings, and symbol visibility
- **Package system integration**: Understand package boundaries and symbol resolution
- **Dependency tracking**: Track requires/uses relationships between files
- **Incremental analysis**: Update analysis on document changes without full reparse

### 2.2 Core IDE Features (2-3 weeks)
- **Go to definition** (`textDocument/definition`)
  - Function definitions, variable bindings, package definitions
  - Cross-file definition lookup using workspace symbol index
- **Find references** (`textDocument/references`)
  - All usages of symbols across workspace
  - Distinguish between definition and reference locations
- **Document symbols** (`textDocument/documentSymbol`) 
  - Hierarchical symbol tree for current document
  - Support for outline view in editors
- **Workspace symbols** (`workspace/symbol`)
  - Global symbol search across all open documents
  - Fuzzy matching and ranking for symbol suggestions
- **Hover information** (`textDocument/hover`)
  - Symbol type information, documentation strings
  - Argument lists for functions, value information for variables

### 2.3 Real-time Analysis (1 week)
- **Background analysis**: Worker threads for non-blocking analysis
- **Incremental updates**: Only re-analyze changed portions of documents
- **Error reporting**: Real-time diagnostics via `textDocument/publishDiagnostics`
- **Performance optimization**: Efficient data structures for large codebases

---

## 🔮 Phase 3: Additional Features & REPL Integration (2-3 weeks)

**Goal**: Interactive development features equivalent to SLIME

### 3.1 Interactive Development (1-2 weeks)
- **Code completion** (`textDocument/completion`)
  - Context-aware symbol completion within scope
  - Package-qualified symbol suggestions
  - Completion for keywords, literals, and special forms
- **Signature help** (`textDocument/signatureHelp`)
  - Function argument hints during typing
  - Multiple signature support for generic functions
- **Code actions** (`textDocument/codeAction`)
  - Quick fixes for common issues
  - Refactoring suggestions (extract function, rename symbol)
- **Formatting** (`textDocument/formatting`)
  - Integration with epsilon.tool.format
  - Range-based formatting for selections

### 3.2 REPL Integration (1 week)
- **Evaluation commands**: LSP extensions for interactive evaluation
  - Evaluate expression at cursor
  - Evaluate current form or selection
  - Load file into REPL
- **Interactive debugging**: Debugging protocol extensions
  - Breakpoint support with conditional breakpoints
  - Step debugging with local variable inspection
  - Stack trace navigation
- **Macro expansion**: Expand macros inline with diff view
- **Inspector integration**: Browse complex objects in editor

### 3.3 Project Management (1 week)
- **Build system integration**: Connection to epsilon.tool.build
  - Live build status in editor
  - Build error highlighting and navigation
- **Test runner integration**: Connection to epsilon.tool.test
  - Run tests from editor with inline results
  - Coverage highlighting and metrics
- **File watching**: Automatic reload on external file changes
- **Dependency management**: Understanding of inter-module dependencies

---

## 🎨 Phase 4: Emacs Integration (2 weeks)

**Goal**: Emacs package with LSP integration

### 4.1 Emacs Package Development (1 week)
- **epsilon-mode**: Derived-mode for Lisp syntax with Epsilon extensions
  - Syntax highlighting for Epsilon-specific constructs
  - Indentation rules for Epsilon macros and special forms
  - Integration with Emacs' electric features
- **LSP client integration**: Support for both `lsp-mode` and `eglot`
  - Automatic server startup and management
  - Custom commands for Epsilon-specific features
  - Configuration management for different project types

### 4.2 SLIME Compatibility Bridge (1 week)
- **Parallel operation**: Run LSP and SLIME side-by-side during transition
- **Feature parity matrix**: Document equivalent features between systems
- **Migration utilities**: Help users transition from SLIME workflows
- **User preference system**: Toggle between SLIME and LSP features
- **Compatibility interface**: Gradual migration path for existing users

---

## 🚀 Phase 5: Distribution & Polish (1-2 weeks)

**Goal**: Production-ready LSP server with broad editor support

### 5.1 Cross-Platform Distribution (1 week)
- **Standalone LSP server binary**: Self-contained executable
- **Editor extensions**: 
  - Emacs package for MELPA distribution
  - VS Code extension (future expansion)
  - Vim/Neovim integration documentation
- **Installation documentation**: Multiple installation methods
- **Configuration examples**: Project setup and customization guides

### 5.2 Performance & Reliability (1 week)  
- **Benchmarking suite**: Performance testing for large codebases
- **Memory optimization**: Efficient data structures for symbol storage
- **Error handling**: Graceful degradation and recovery
- **Logging and diagnostics**: Configurable logging for troubleshooting
- **Configuration management**: User settings and workspace preferences

---

## Technical Architecture

### Core Design Principles
- **Incremental approach**: Build alongside SLIME without immediate replacement
- **Standards compliance**: LSP specification adherence for compatibility
- **Performance focus**: Real-time analysis for large projects
- **Extensibility**: Plugin architecture for custom language features
- **Protocol flexibility**: Support both JSON-RPC and MessagePack-RPC

### Key Components
1. **Protocol Handler**: Multi-transport RPC communication (JSON/MessagePack)
2. **Analysis Engine**: Real-time semantic analysis with incremental updates
3. **Workspace Manager**: Project-aware file and dependency tracking
4. **REPL Bridge**: Interactive evaluation and debugging integration
5. **Editor Clients**: Integrations for Emacs, VS Code, and others

### Integration Points
- **epsilon.lib.json**: Message serialization and parsing
- **epsilon.lib.msgpack**: Alternative transport protocol
- **epsilon.lib.reader**: AST parsing and syntax analysis
- **epsilon.tool.build**: Project structure and dependency management
- **epsilon.tool.test**: Test execution and result reporting
- **epsilon.net.http**: Network communication and WebSocket support

---

## Success Metrics

### Phase 2 Success Criteria
- Go-to-definition works across files and packages
- Error highlighting with sub-second response
- Symbol search returns results in under 100ms for typical projects
- Hover information includes type and documentation

### Phase 3 Success Criteria  
- Code completion provides suggestions within 50ms
- REPL evaluation works with error handling and output capture
- Build integration shows errors inline with navigation
- Test runner provides feedback with coverage visualization

### Phase 4 Success Criteria
- Emacs package loads and connects automatically
- Feature parity with core SLIME functionality
- Side-by-side operation allows gradual migration
- Documentation covers all migration scenarios

### Final Success Criteria
- LSP server handles 1000+ file projects efficiently
- Supports major editors through standard LSP protocol
- Zero-configuration setup for new Epsilon projects
- Community adoption and feedback

---

## Future Enhancements

### Additional Language Features
- **Semantic highlighting**: Type-based syntax coloring
- **Call hierarchy**: Function call trees and dependency visualization  
- **Symbol renaming**: Safe refactoring across entire workspace
- **Import optimization**: Automatic cleanup and organization

### Development Experience
- **Live documentation**: Inline documentation with examples
- **Visual debugging**: Graphical debugging with data flow visualization
- **Performance profiling**: Integrated profiler with editor annotations
- **Collaboration features**: Shared editing and debugging sessions

This roadmap provides a path from the current foundation to a development environment that can replace SLIME while maintaining compatibility during the transition period.

---

# SLIME Replacement Implementation Plan

## Overview

This section provides a comprehensive implementation plan to complete the epsilon.lsp server as a full replacement for SLIME in Emacs and other editors. Based on the analysis of the current implementation, we have a solid foundation with basic LSP features and can focus on the critical missing components.

## Current Implementation Analysis

### ✅ Already Implemented
- Core LSP protocol (JSON-RPC 2.0) with stdio transport
- Basic language features (go-to-definition, hover, completion within documents)
- Document synchronization and workspace management
- Advanced evaluation system with security sandboxing
- Document symbols and workspace symbol search
- Basic diagnostics and syntax error reporting

### ❌ Critical Missing Features
- **TCP/IP Transport**: Required for real editor connections (currently stdio only)
- **Cross-file Analysis**: Symbol resolution across workspace
- **REPL Integration**: Interactive development via custom LSP commands
- **Incremental Parsing**: Performance optimization for large files
- **Network Transport**: TCP server for client connections

## Implementation Phases

### Phase 1: Network Transport & Enhanced Workspace (Weeks 1-2)

#### 1.1 TCP Transport Implementation
- Add TCP server using epsilon.net
- Support multiple simultaneous client connections  
- Maintain compatibility with existing stdio transport
- Connection lifecycle management

#### 1.2 Enhanced Workspace Management
- File watching for external changes using platform APIs
- Cross-file symbol indexing with dependency tracking
- Project root detection (.asd files, .git directories)
- Incremental symbol database updates

**Key Files to Create:**
- `src/lsp/src/transport-tcp.lisp` - TCP transport layer
- `src/lsp/src/workspace-index.lisp` - Cross-file analysis
- `src/lsp/src/file-watcher.lisp` - File change monitoring

### Phase 2: REPL Integration (Weeks 3-4)

#### 2.1 Custom LSP Protocol Extensions
Define non-standard LSP commands for REPL operations:
```
lisp/createRepl - Create new REPL session
lisp/evalExpression - Evaluate expression
lisp/evalDefun - Evaluate current defun
lisp/evalRegion - Evaluate selected text
lisp/interruptEvaluation - Interrupt running code
```

#### 2.2 Session Management
- Extend existing evaluation.lisp for interactive REPL
- Multiple REPL sessions per workspace
- Session state preservation (package, variables)
- Output streaming for long operations

#### 2.3 Client Integration
- Emacs package with REPL buffer management
- VS Code extension for REPL panel
- Neovim configuration for REPL integration

**Key Files to Create:**
- `src/lsp/src/repl-integration.lisp` - REPL session manager
- `clients/emacs/epsilon-lsp-repl.el` - Emacs REPL client
- `clients/vscode/src/repl.ts` - VS Code REPL panel

### Phase 3: Advanced Analysis (Weeks 5-6)

#### 3.1 Cross-file Symbol Resolution
- Parse ASDF system definitions for dependencies
- Build workspace dependency graph
- Resolve symbols across package boundaries
- Track symbol usage across files

#### 3.2 Incremental Parsing Performance
- Implement diff-based parsing for large files
- Cache parsed forms with content hashing
- Background analysis worker threads
- Memory-efficient symbol storage

#### 3.3 Macro Expansion Support
- Track macro definitions and expansions
- Show expanded forms on hover
- Step-by-step macro expansion viewer
- Handle compiler macros correctly

**Key Files to Modify:**
- `src/lsp/src/analysis.lisp` - Enhanced analysis engine
- `src/lsp/src/workspace.lisp` - Cross-file indexing
- `src/lsp/src/server.lisp` - Performance optimizations

### Phase 4: Debugging & Inspection (Weeks 7-8)

#### 4.1 Debugger Integration
Custom LSP extensions for debugging:
```
lisp/getBacktrace - Capture stack trace
lisp/inspectFrame - Examine stack frame
lisp/invokeRestart - Select restart option
lisp/setBreakpoint - Add conditional breakpoint
```

#### 4.2 Interactive Debugging
- Capture and forward conditions to client
- Present restarts as code actions
- Support step-by-step debugging
- Local variable inspection

#### 4.3 Object Inspector
```
lisp/inspect - Inspect object structure
lisp/inspectPart - Navigate object components
lisp/modifySlot - Edit object state
```

**Key Files to Create:**
- `src/lsp/src/debug-adapter.lisp` - Debug protocol implementation
- `src/lsp/src/inspector.lisp` - Object inspection system

### Phase 5: Refactoring & Code Actions (Weeks 9-10)

#### 5.1 Symbol Renaming
- Project-wide symbol renaming with conflict detection
- Preserve symbol meaning across package boundaries
- Integration with version control for rollback

#### 5.2 Code Actions
- **Add missing import**: Auto-import undefined symbols
- **Generate defgeneric**: Create generic from defmethod
- **Extract function**: Extract selection to new function
- **Inline function**: Replace calls with function body

#### 5.3 Advanced Completions
- Context-aware completions (function vs variable context)
- Snippet expansion for common patterns
- Auto-import suggestions for external symbols
- Parameter hints with type information

### Phase 6: Performance & Production (Weeks 11-12)

#### 6.1 Performance Optimization
- Parallel analysis using worker threads
- Memory-mapped file handling for large projects
- Result caching with TTL expiration
- Profiling and bottleneck elimination

#### 6.2 Configuration & Deployment
- User/workspace configuration system
- Standalone binary distribution
- Editor integration packages
- Comprehensive documentation

## Client Integration Strategies

### Emacs Integration
```elisp
;; Enhanced lsp-mode integration
(use-package epsilon-lsp
  :after lsp-mode
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection "localhost" 7777)
    :activation-fn (lsp-activate-on "lisp")
    :server-id 'epsilon-lsp
    :custom-capabilities '((repl . t) (debugging . t))))
  :bind
  (:map lisp-mode-map
   ("C-c C-e" . epsilon-eval-expression)
   ("C-c C-k" . epsilon-compile-file)
   ("C-c C-z" . epsilon-switch-to-repl)))
```

### VS Code Extension
- Language server configuration in package.json
- Custom commands for REPL operations
- Debug adapter protocol integration
- Syntax highlighting for Epsilon extensions

### Neovim Integration
```lua
-- Built-in LSP client configuration
require'lspconfig'.epsilon_lsp.setup{
  cmd = {"epsilon", "lsp", "--tcp", "--port", "7777"},
  filetypes = {"lisp"},
  root_dir = require'lspconfig'.util.root_pattern("*.asd", ".git"),
  settings = {
    epsilon = {
      repl = { enabled = true },
      analysis = { crossFile = true }
    }
  }
}
```

## Migration from SLIME

### Feature Mapping
| SLIME Feature | epsilon.lsp Equivalent |
|---------------|------------------------|
| `slime-eval-defun` | `lisp/evalDefun` command |
| `slime-compile-file` | `textDocument/diagnostics` |
| `slime-repl` | Custom REPL panel |
| `slime-inspect` | `lisp/inspect` command |
| `slime-who-calls` | `textDocument/references` |
| `slime-macroexpand-1` | `lisp/macroexpand` command |
| `slime-debugger` | Debug adapter protocol |

### Compatibility Layer
Create `slime-compat.el` to provide SLIME keybindings that map to LSP commands:
```elisp
(defun slime-eval-defun ()
  "SLIME compatibility: evaluate defun"
  (interactive)
  (lsp-request "lisp/evalDefun" 
               (list :textDocument (lsp--text-document-identifier)
                     :position (lsp--cur-position))))
```

## Success Metrics

### Performance Targets
- **Response time**: <100ms for common operations (completion, hover)
- **Startup time**: <2 seconds for medium projects (1000 files)
- **Memory usage**: <500MB for large projects (10,000 files)
- **Network latency**: <10ms additional overhead vs stdio

### Feature Completeness
- **Core LSP**: 100% compliance with LSP 3.17 specification
- **SLIME parity**: 90% of commonly used SLIME features
- **Editor support**: Emacs, VS Code, Neovim working configurations
- **Stability**: <1 crash per 100 hours of usage

### Adoption Metrics
- **Community feedback**: Beta testing with 10+ experienced Lisp developers
- **Documentation coverage**: Setup guides for all supported editors
- **Issue resolution**: <48 hour response time for bug reports
- **Performance benchmarks**: Public results vs SLIME baseline

## Timeline Summary

- **Weeks 1-2**: TCP transport and enhanced workspace management
- **Weeks 3-4**: REPL integration with custom LSP commands
- **Weeks 5-6**: Cross-file analysis and incremental parsing
- **Weeks 7-8**: Debugging and inspection support
- **Weeks 9-10**: Refactoring tools and code actions
- **Weeks 11-12**: Performance optimization and production deployment

**Total Implementation Time**: 12 weeks for complete SLIME replacement

This implementation plan builds on the existing solid foundation to create a modern, secure, and editor-agnostic development environment for Common Lisp that surpasses SLIME's capabilities while maintaining familiar workflows.
