# Epsilon Changelog

## Unreleased

### Changed
- **Module Registration System**: Moved module registration from `epsilon.tool.boot` to `epsilon.tool.build`. The boot process now only handles core module bootstrapping with standalone dependency resolution. Module discovery and registration is handled by `epsilon.tool.build:register-modules` using `*known-projects*` map instead of hashtables.
- **Module-Specific Build and Test Commands**: Build and test commands now require module names as positional arguments. Usage: `./run.sh build epsilon.core` and `./run.sh test http`. Removed `:file` parameter from build function - all builds are now module-based with automatic directory resolution from the registry.
- **Provides/Requires Module Dependencies**: Module definitions now use `provides`/`requires` pattern for inter-module dependencies instead of package-level dependencies. Modules are the primary coarse-grained unit of functionality.
- **Standardized Collection Count Functions**: All collection types now consistently provide `count` function. Added missing `map:count` implementation and `set:count` for consistency with `seq:count`. Both `count` and `size` are available for backward compatibility.
- **Build System File-Based Configuration**: `epsilon.tool.build` now accepts a `file` argument instead of `dir`. Source paths are resolved relative to the directory containing the specified package file. This enables more flexible project configuration and better support for different project layouts.
- **Container Strategy Simplified**: Replaced complex hybrid container approach with simple platform containers. Linux and Windows use real platform containers with pre-installed SBCL, macOS uses native runners. Eliminates misleading cross-compilation approach.
- **Modular Directory Structure**: Reorganized codebase into `module/` directory containing standardized module layout. Each module (core, darwin, linux, windows, http) has its own directory with `src/`, `tests/`, and `package.yaml`. Package files moved into their respective module directories.

### Added
- **Multi-Platform Networking Architecture**: Implemented modular networking with platform-specific backends:
  - Linux: epoll-based networking in `module/linux/src/sys/epoll.lisp`
  - macOS/Darwin: kqueue-based networking in `module/darwin/src/sys/kqueue.lisp` 
  - Windows: IOCP-based networking in `module/windows/src/sys/iocp.lisp`
- **HTTP Module**: Created new HTTP client and server implementation using platform-specific epsilon.net backends
- **Module Registry System**: Boot system now discovers and registers all modules at startup, enabling flexible module resolution
- **Platform-Specific Build Configurations**: Added separate package files for each platform:
  - `module/core/package.yaml`: Platform-agnostic core functionality
  - `module/linux/package.yaml`: Linux with epoll networking
  - `module/darwin/package.yaml`: macOS with kqueue networking
  - `module/windows/package.yaml`: Windows with IOCP networking
  - `module/http/package.yaml`: HTTP implementation
- **Container-Based CI/CD**: Implemented Docker containers for consistent build environments:
  - Linux container: Ubuntu 22.04 with pre-installed SBCL 2.4.0
  - Windows container: Windows Server Core 2022 with pre-installed SBCL 2.4.0
  - macOS: Uses native GitHub Actions runners with Homebrew SBCL
- **Platform Detection**: Build system automatically detects platform and selects appropriate package configuration
- **Modular Source Organization**: Segregated source code by platform to avoid complex inclusion/exclusion rules:
  - `src-core/`: Platform-agnostic functionality
  - `src-linux/`: Linux-specific implementations
  - `src-darwin/`: macOS-specific implementations
  - `src-windows/`: Windows-specific implementations

### Infrastructure
- **GitHub Actions Workflows**:
  - `ci.yml`: Multi-platform builds and testing using platform containers
  - `build-image.yml`: Automated container builds and registry management
  - `release.yml`: Container-based release package generation
- **Platform-Specific Testing**: Each platform now tests its specific networking implementation (epoll, kqueue, IOCP)

### Architecture
- **No Reader Macros Policy**: Avoided compile-time conditionals in favor of build-time platform selection
- **Single Repository**: All platforms build from one repository with modular configuration
- **Clean Dependencies**: Platform-specific modules properly declare dependencies on core functionality