#!/bin/bash
# Epsilon Documentation Deployment Script
# Deploys to GitHub Pages

set -e

echo "▓▓▓ EPSILON DOCS DEPLOYMENT ▓▓▓"
echo "Deploying functional programming documentation to GitHub Pages..."
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

# Check git status
if ! git diff-index --quiet HEAD --; then
    echo "⚠️  Warning: You have uncommitted changes."
    read -p "Continue with deployment? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "❌ Deployment cancelled."
        exit 1
    fi
fi

# Get current branch and commit info
BRANCH=$(git branch --show-current)
COMMIT=$(git rev-parse --short HEAD)
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')

echo "📋 Deployment info:"
echo "   🌿 Branch: $BRANCH"
echo "   📝 Commit: $COMMIT"
echo "   ⏰ Time: $TIMESTAMP"
echo

# Build first to check for errors
echo "🔧 Building documentation..."
mkdocs build --strict

if [ $? -ne 0 ]; then
    echo "❌ Build failed! Fix errors before deploying."
    exit 1
fi

# Deploy to GitHub Pages
echo "🚀 Deploying to GitHub Pages..."
mkdocs gh-deploy --force --message "Deploy docs from $BRANCH@$COMMIT on $TIMESTAMP

Built with functional programming theme
Features: Lisp syntax highlighting, technical styling, monospace typography"

if [ $? -eq 0 ]; then
    echo
    echo "✅ Deployment completed successfully!"
    echo "   🌐 Site URL: https://jbouwman.github.io/epsilon/"
    echo "   ⏳ Note: GitHub Pages updates may take a few minutes"
    echo
    echo "📊 Deployment stats:"
    echo "   📁 Files deployed: $(find site/ -type f | wc -l)"
    echo "   📦 Total size: $(du -sh site/ | cut -f1)"
else
    echo "❌ Deployment failed!"
    exit 1
fi