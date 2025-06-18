FROM ubuntu:22.04

# Install SBCL
RUN apt-get update && \
    apt-get install -y sbcl wget && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]