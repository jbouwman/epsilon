# Architecture Documentation

This section contains technical specifications, design documents, and architectural details for the Epsilon project.

## Core Architecture

### Data Structures
- **[Data Structures Overview](data-structures.md)** - Core data structure implementations
- **[Maps](map.md)** - Map data structure specification
- **[Sequences](sequence.md)** - Sequence data structure specification
- **[Sequences and Maps](sequences-maps.md)** - Interaction between sequences and maps
- **[Sets](set.md)** - Set data structure specification
- **[Strings](string.md)** - String processing and manipulation

### Data Encoding and Protocols
- **[JSON](json.md)** - JSON processing implementation
- **[JSON & YAML](json-yaml.md)** - Combined JSON and YAML support
- **[URI](uri.md)** - URI parsing and manipulation
- **[Digest](digest.md)** - Cryptographic digest functions

### System Architecture
- **[Module Management](module-management.md)** - Module system architecture
- **[Package Format](package-format.md)** - Package structure and format specification
- **[Packages](packages.md)** - Package management system
- **[Parser](epsilon_lib_parser.md)** - Language parser implementation

## Technical Proposals

### Optimization and Enhancement
- **[Binary Stream Proposal](binary-stream-proposal.md)** - Binary data streaming architecture
- **[Boot Optimization](boot-optimization.md)** - System startup optimization strategies
- **[Codec Modularization](codec-modularization-analysis.md)** - Analysis of codec architecture
- **[Regex Simplification](regex-simplification-plan.md)** - Regular expression system simplification

### Development Tools
- **[Transcript](transcript.md)** - Development session transcripts and analysis

## Design Principles

The Epsilon architecture follows these key principles:

- **Modularity** - Clean separation of concerns
- **Performance** - Optimized for speed and efficiency
- **Simplicity** - Clear and understandable design
- **Extensibility** - Easy to extend and customize