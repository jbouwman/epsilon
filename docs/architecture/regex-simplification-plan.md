# Regex Library Simplification Plan

The current `epsilon.regex` implementation is 4734 lines and includes many advanced Perl-compatible features that add complexity without clear benefit for most use cases.

## Current Complex Features to Remove

### Phase 1: Remove Perl-specific Comment Syntax
- **Extended mode** (`*extended-mode-p*`) - Allows whitespace and `#` comments in patterns
- **Regex comments** `(?#...)` - Inline comments within patterns  
- **End-of-line comments** in extended mode

**Impact**: ~200 lines, removes parsing complexity for comments
**Rationale**: Comments in regex patterns are rarely used and add parsing overhead

### Phase 2: Remove Advanced Lookaround
- **Lookbehind assertions** `(?<=...)` and `(?<!...)`
- **Complex lookahead** beyond basic `(?=...)` and `(?!...)`

**Impact**: ~400 lines, removes backtracking complexity
**Rationale**: Lookbehind requires complex reverse matching. Basic lookahead covers most needs.

### Phase 3: Remove Named Groups and Advanced Grouping
- **Named capture groups** `(?<name>...)`
- **Atomic groups** `(?>...)` 
- **Conditional expressions** `(?(...)...)`

**Impact**: ~300 lines
**Rationale**: Numbered groups are sufficient for most use cases

### Phase 4: Remove Unicode Property Support
- **Character properties** `\p{...}` and `\P{...}`
- **Property resolver** framework
- **Unicode categories**

**Impact**: ~500 lines
**Rationale**: Basic character classes cover most needs. Unicode support adds significant complexity.

### Phase 5: Remove Advanced Flags and Modes
- **Multi-line mode** (`m` flag)
- **Single-line mode** (`s` flag) 
- **Case-insensitive mode** complexity beyond basic flag
- **Multiple mode combinations**

**Impact**: ~200 lines
**Rationale**: Keep basic case-insensitive matching but remove mode interaction complexity

### Phase 6: Simplify Repetition
- **Possessive quantifiers** `*+`, `++`, `?+`, `{n,m}+`
- **Complex lazy/greedy interaction**
- **Nested repetition optimization**

**Impact**: ~300 lines
**Rationale**: Basic `*`, `+`, `?`, `{n,m}` with greedy/lazy variants are sufficient

## Target Simplified Feature Set

After simplification, the regex library should support:

### Core Features (Keep)
- **Basic matching**: literal characters, `.`, `^`, `$`
- **Character classes**: `[abc]`, `[^abc]`, `[a-z]`
- **Predefined classes**: `\d`, `\w`, `\s` and negations
- **Basic quantifiers**: `*`, `+`, `?`, `{n}`, `{n,m}`
- **Greedy/lazy**: `*?`, `+?`, `??`, `{n,m}?`
- **Groups**: `(...)` with numbered capture
- **Alternation**: `|`
- **Basic lookahead**: `(?=...)`, `(?!...)`
- **Anchors**: `^`, `$`, `\b`, `\B`
- **Basic case-insensitive** flag

### Estimated Result
- **Target size**: ~1500 lines (down from 4734)
- **Complexity reduction**: 65%
- **Performance**: Faster compilation, smaller memory footprint
- **Maintainability**: Much easier to understand and debug

## Implementation Strategy

1. **Create new simplified parser** using parser combinators
2. **Implement core feature set** from scratch 
3. **Build test suite** covering simplified features
4. **Benchmark performance** against current implementation

## Benefits

- **Faster compilation**: Less complex parsing and optimization
- **Smaller memory footprint**: Simpler matcher structures
- **Better maintainability**: Easier to understand and debug
- **Educational value**: Clear implementation of regex fundamentals
- **Integration**: Better alignment with parser combinator architecture
