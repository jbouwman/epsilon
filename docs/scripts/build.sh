#!/bin/bash
# Epsilon Documentation Build Script
# Builds static documentation site

set -e

echo "▓▓▓ EPSILON DOCS BUILD SYSTEM ▓▓▓"
echo "Building functional programming documentation..."
echo

# Check if mkdocs is installed
if ! command -v mkdocs &> /dev/null; then
    echo "❌ MkDocs not found. Installing..."
    pip install -r requirements.txt
fi

# Check if in correct directory
if [ ! -f "mkdocs.yml" ]; then
    echo "❌ Error: mkdocs.yml not found. Run from docs/ directory."
    exit 1
fi

# Clean previous build
echo "🧹 Cleaning previous build..."
rm -rf site/

# Build documentation
echo "🔧 Building documentation site..."
mkdocs build --strict --verbose

# Report build results
if [ -d "site" ]; then
    SIZE=$(du -sh site/ | cut -f1)
    FILES=$(find site/ -type f | wc -l)
    echo
    echo "✅ Build completed successfully!"
    echo "   📁 Output: site/ directory"
    echo "   📊 Size: $SIZE"
    echo "   📄 Files: $FILES"
    echo
    echo "To serve locally: ./serve.sh"
    echo "To deploy: ./deploy.sh"
else
    echo "❌ Build failed!"
    exit 1
fi