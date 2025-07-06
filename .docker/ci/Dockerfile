# CI container with SBCL and testing tools pre-installed
FROM ghcr.io/jbouwman/epsilon/epsilon-linux:latest

LABEL org.opencontainers.image.title="Epsilon CI Environment"
LABEL org.opencontainers.image.description="Ubuntu with SBCL and CI tools for epsilon testing"

# Install additional CI tools
RUN apt-get update && apt-get install -y \
    junit4 \
    xmlstarlet \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set up CI-specific environment
ENV CI=true
ENV SBCL_HOME=/usr/local/lib/sbcl

# Verify SBCL installation and show version info
RUN sbcl --version && \
    echo "SBCL_HOME: $SBCL_HOME" && \
    echo "CI container ready"

# Set working directory
WORKDIR /workspace

CMD ["/bin/bash"]