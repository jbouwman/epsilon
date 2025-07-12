# Epsilon Wishlist

Future capabilities and enhancements organized by difficulty and priority.

## Testing Framework

### Medium Difficulty
- **TAP formatter**: Output test results in Test Anything Protocol format for integration with TAP consumers
- **JUnit XML formatter**: Generate JUnit-compatible XML reports for CI/CD integration and IDE test runners

## Module System

### Low Difficulty
- **Simplified package definition macro**: Replace verbose `defpackage` with cleaner `module` syntax
  - Support `(import package...)` for regular imports
  - Support `(import (package alias)...)` for aliased imports  
  - Support `(from package symbols...)` for selective imports
  - Support `(export symbols...)` and `(shadow symbols...)`
  - Automatic `in-package` after definition
  - Good pretty-printing with parentheses around all clauses

## Test Framework Completion

### Low Difficulty  
- **Complete missing definitions**: Fix `test` class, `timing-metric` class, and `sort-into-suites` function
- **Fix export inconsistencies**: Resolve `equal`/`equal-p` vs `is-equal`/`is-equalp` mismatches
- **Complete formatter factory**: Implement consistent `formatter:make` interface

### Medium Difficulty
- **Extract metrics system**: Move to `src/tool/test/metrics.lisp` with expanded capabilities
  - Memory usage, GC statistics, assertion count metrics
  - Metric aggregation and reporting utilities
- **Extract results management**: Move to `src/tool/test/results.lisp`
  - Result filtering, sorting, analysis functions
  - Separate execution from presentation logic
- **Create test registry**: Extract to `src/tool/test/registry.lisp`
  - Test metadata (tags, descriptions, dependencies)
  - Suite dependency tracking

## Performance & Benchmarking

### Medium Difficulty
- **Data structure benchmarks**: Performance testing suite for map, set, and sequence implementations
  - Add/remove operations across different sizes (10, 100, 1K, 10K, 100K elements)
  - Lookup performance comparison with native hash tables and lists
  - Memory usage profiling for different data structure sizes
  - Iteration and traversal performance measurements

## Interactive Development Environment

### Low Difficulty (Quick Wins)
- **Symbol aliases**: Short aliases for common operations
  - `(m ...)` for `(map:make-map ...)`
  - `(s seq fn)` for `(seq:map fn seq)`
  - Context-specific abbreviations
- **Pretty printers**: Better PRINT-OBJECT methods for all data structures
  - Maps print as `{:key value, ...}`
  - Sets print as `#{element ...}`
  - Sequences with truncation for large collections
- **REPL commands**: Common operations as REPL shortcuts
  - `:i object` - inspect object
  - `:t function` - trace function  
  - `:p expression` - profile expression
  - `:h symbol` - help/documentation

### Medium Difficulty
- **Smart REPL package** (`epsilon.tool.repl`)
  - Context-aware symbol completion
  - Auto-import based on current working module
  - Symbol search across all loaded packages
  - Recent symbol history and favorites
  - Package alias management (avoid long prefixes)
- **Inspector framework** (`epsilon.tool.inspector`)
  - Hierarchical object inspection
  - Expandable/collapsible views
  - Type-specific inspectors for maps, sequences, etc.
  - Edit-in-place for mutable slots
  - Navigation history (back/forward)
  - Export to various formats (JSON, EDN, etc.)

### High Difficulty
- **Notebook-style REPL** (`epsilon.tool.notebook`)
  - Cell-based evaluation with persistent results
  - Markdown cells for documentation
  - Inline visualizations (tables, graphs, trees)
  - Cell dependencies and auto-rerun
  - Export to HTML/PDF reports
  - WebSocket-based for remote access
- **Live system browser** (`epsilon.tool.browser`)
  - Real-time module dependency visualization
  - Thread activity monitor with stack traces
  - Memory allocation heatmaps
  - Network connection status and metrics
  - Interactive debugging with breakpoints
  - Performance profiling overlay
- **Time-travel debugging** (`epsilon.tool.history`)
  - Record all state changes during execution
  - Step backwards through execution history
  - Replay with modifications ("what if" debugging)
  - Diff view between states
  - Persistent history for post-mortem analysis
  - Integration with test framework for replay

### Implementation Roadmap
1. **Phase 1** (2-3 weeks): Smart REPL with context management
2. **Phase 2** (3-4 weeks): Inspector framework with rich visualizations
3. **Phase 3** (4-6 weeks): Notebook interface using epsilon.http
4. **Phase 4** (6-8 weeks): System browser with D3.js visualizations
5. **Phase 5** (8-10 weeks): History system with efficient snapshots

### Technical Requirements
- WebSocket support for live updates
- Browser-based UI using epsilon.http
- Efficient serialization for large objects
- Incremental updates to avoid full redraws
- Extensible architecture for custom inspectors

## Low Priority Items

*Additional items to be added as requirements are identified*