# Operations

Documentation for building, testing, and deploying Epsilon.

## Build System

- [Build System](build.md) - Module builds, dependency resolution, and caching
- [EPK Integration](build-epk-integration.md) - Enhanced package creation

## CI/CD & Releases

- [CI/CD Process](ci-cd.md) - Complete guide to testing, continuous integration, and releases

## Distribution

- [Distribution](distribution.md) - Installation methods and platform support

## Documentation

Documentation is built with MkDocs. Helper scripts are in `/docs/scripts/`:

- `build.sh` - Build documentation locally
- `serve.sh` - Run development server
- `deploy.sh` - Deploy to GitHub Pages

### Building Documentation Locally

```bash
# Install dependencies
pip install -r requirements.txt

# Serve locally (with live reload)
./docs/scripts/serve.sh

# Build static site
./docs/scripts/build.sh

# Deploy to GitHub Pages (automated via workflow)
./docs/scripts/deploy.sh
```