# Web Services Integration Todo

## Current Status
- HTTP client/server implementation exists and is feature-complete
- Foreign interface module exists with core FFI functionality  
- Working web service examples exist but use SBCL sockets directly
- **Primary issue**: Module system integration prevents using epsilon.http

## Todo List

### High Priority - Module Integration
- [ ] Fix epsilon.foreign compilation warnings and type conflicts
- [ ] Resolve epsilon.net package reference issues in TLS modules
- [ ] Enable epsilon.http to load properly through module system
- [ ] Test HTTP module loading via `./epsilon build epsilon.http`

### Medium Priority - Examples Enhancement  
- [ ] Update anagram service to use epsilon.http instead of sb-bsd-sockets
- [ ] Create tutorial showing epsilon.http client/server usage
- [ ] Add WebSocket example using epsilon.websocket module
- [ ] Add middleware example for request processing

### Low Priority - Documentation
- [ ] Document HTTP module API and usage patterns
- [ ] Create deployment guide for web services
- [ ] Add troubleshooting section for common issues

## Implementation Notes
- Most functionality is already implemented - this is primarily an integration task
- Focus on build system and module loading rather than new feature development
- Examples should showcase epsilon's functional data structures (maps, sequences)