# Epsilon Platform Containers

For linux and windows CI tasks, docker base containers are used that contains preinsstalled build dependencies.

## CI Container
- **Base**: epsilon-linux container
- **SBCL**: 2.4.0 pre-installed
- **Additional tools**: JUnit, XMLStarlet for CI tasks
- **Purpose**: Fast CI testing with pre-built environment
- **Usage**: `ghcr.io/owner/repo/epsilon-ci:latest`

## Linux Container
- **Base**: Ubuntu 22.04
- **SBCL**: 2.4.0 pre-installed
- **Purpose**: Native Linux builds with epoll networking
- **Usage**: `ghcr.io/owner/repo/epsilon-linux:latest`

## Windows Container  
- **Base**: Windows Server Core 2022
- **SBCL**: 2.4.0 pre-installed
- **Purpose**: Native Windows builds with IOCP networking
- **Usage**: `ghcr.io/owner/repo/epsilon-windows:latest`

## macOS
- **No container**: Uses native macOS runners with Homebrew SBCL + caching
- **Purpose**: Native macOS builds with kqueue networking
