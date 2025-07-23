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
