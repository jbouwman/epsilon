# Linux container with SBCL pre-installed
FROM ubuntu:22.04

LABEL org.opencontainers.image.title="Epsilon Linux Build Environment"
LABEL org.opencontainers.image.description="Ubuntu with SBCL for epsilon Linux builds"

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV SBCL_VERSION=2.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    bzip2 \
    tar \
    gcc \
    libc6-dev \
    make \
    git \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install SBCL
RUN cd /tmp && \
    curl -L -O "https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2" && \
    tar -xjf "sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2" && \
    cd "sbcl-${SBCL_VERSION}-x86-64-linux" && \
    ./install.sh && \
    cd / && rm -rf /tmp/sbcl-*

# Verify installation
RUN sbcl --version

# Set working directory
WORKDIR /workspace

CMD ["/bin/bash"]