# Epsilon Development Roadmaps

Strategic initiatives for expanding Epsilon's capabilities, organized by priority and technical scope.

## Active Roadmaps

### [001 - Language Server Protocol (LSP)](roadmaps/001_lsp.md)
**Status**: Phase 1 Complete ✅ | **Timeline**: 8-12 weeks total

Modern IDE integration through Language Server Protocol implementation. Provides go-to-definition, hover, completion, and interactive development features. Foundation complete with JSON-RPC transport, basic server lifecycle, and code analysis engine.

**Next Phase**: Enhanced code analysis and core IDE features (go-to-definition, find references, document symbols).

### [002 - Testing Framework Enhancement](roadmaps/002_testing_framework.md)
**Status**: Planning | **Timeline**: 6-9 weeks

Complete and enhance the existing testing framework with TAP/JUnit output, metrics collection, and advanced testing capabilities. Focus on professional development workflows and CI/CD integration.

**Key Features**: Parameterized tests, fixtures, mocking, property-based testing, parallel execution.

### [003 - Interactive Development Environment](roadmaps/003_interactive_development.md)
**Status**: Planning | **Timeline**: 25-35 weeks

Rich web-based development environment with notebook-style REPL, object inspection, live system monitoring, and time-travel debugging. Builds on LSP foundation for comprehensive development experience.

**Progressive Phases**: Smart REPL → Inspector → Notebook → System Browser → Time-travel Debugging.

### [004 - Module System Enhancement](roadmaps/004_module_system.md)
**Status**: Planning | **Timeline**: 8-12 weeks

Modern module syntax to replace verbose `defpackage` forms with cleaner import/export syntax. Includes dependency management, hot-reloading, and integration with package.yaml configuration.

**Key Benefits**: Simplified syntax, better dependency tracking, enhanced developer experience.

## Implementation Priority

### High Priority (Active Development)
1. **LSP Phase 2**: Core IDE features for immediate productivity gains
2. **Testing Framework**: Foundation for reliable development practices
3. **Module System Phase 1**: Developer experience improvements

### Medium Priority (Near-term)
1. **Interactive Development Quick Wins**: REPL enhancements and pretty printers
2. **LSP Phase 3**: Advanced features and REPL integration
3. **Module System Phase 2**: Advanced features and hot-reloading

### Long-term Vision
1. **Interactive Development Advanced Features**: Notebook interface and system browser
2. **LSP Production Polish**: Cross-platform distribution and editor integrations
3. **Module System Integration**: Package.yaml unification and advanced dependency management

## Cross-cutting Concerns

### Architecture Integration
All roadmaps leverage Epsilon's core libraries:
- **epsilon.lib.map**: Efficient data structures
- **epsilon.lib.json**: Serialization and communication
- **epsilon.net.http**: Web interfaces and real-time communication
- **epsilon.tool.build**: Build system integration

### Migration Strategy
Each roadmap includes backward compatibility and gradual adoption:
- **LSP**: Parallel operation with SLIME during transition
- **Module System**: Gradual migration from defpackage to module syntax
- **Testing**: Extension of existing framework rather than replacement
- **Interactive Development**: Progressive enhancement of current REPL

## Success Metrics

### Technical Excellence
- Sub-second response times for interactive features
- Scalability to 1000+ file projects
- Zero-configuration setup for new projects
- Comprehensive test coverage and documentation

### Developer Experience
- Reduced time-to-productivity for new contributors
- Modern IDE features matching contemporary development environments
- Seamless integration between different development tools
- Clear migration paths from existing workflows

---

*Last updated: 2025-07-06*
*Total estimated effort: 57-80 weeks across all initiatives*