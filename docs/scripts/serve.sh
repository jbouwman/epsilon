#!/bin/bash
# Epsilon Documentation Development Server
# Serves documentation with live reload

set -e

echo "Starting Epsilon Documentation Server"
echo

# Check if mkdocs is installed
if ! command -v mkdocs &> /dev/null; then
    echo "- MkDocs not found. Installing..."
    pip install -r requirements.txt
fi

# Check if in correct directory
if [ ! -f "mkdocs.yml" ]; then
    echo "- Error: mkdocs.yml not found. Run from docs/ directory."
    exit 1
fi

# Get local IP for network access
LOCAL_IP=$(hostname -I | awk '{print $1}' 2>/dev/null || echo "localhost")

echo " - Local: http://localhost:8000"
echo " - Network: http://$LOCAL_IP:8000"
echo
echo "Press Ctrl+C to exit"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Start development server
mkdocs serve --dev-addr 0.0.0.0:8000 --livereload
