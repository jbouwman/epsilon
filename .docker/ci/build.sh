#!/bin/bash
# Build script for epsilon-ci container

set -e

echo "Building epsilon-ci container..."

docker build -f .docker/ci/Dockerfile -t epsilon-ci:latest .

echo "CI container built successfully"
echo "Test with: docker run --rm -it epsilon-ci:latest sbcl --version"