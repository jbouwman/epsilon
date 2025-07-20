FROM ubuntu:22.04

# Install SBCL and required tools
RUN apt-get update && \
    apt-get install -y sbcl wget git tar gzip && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]