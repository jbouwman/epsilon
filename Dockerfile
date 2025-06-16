FROM ubuntu:22.04

# Install SBCL
RUN apt-get update && \
    apt-get install -y sbcl wget && \
    rm -rf /var/lib/apt/lists/*

# Install Nushell
RUN wget -q https://github.com/nushell/nushell/releases/download/0.91.0/nu-0.91.0-x86_64-linux-gnu-full.tar.gz && \
    tar xf nu-0.91.0-x86_64-linux-gnu-full.tar.gz && \
    mv nu-0.91.0-x86_64-linux-gnu-full/nu /usr/local/bin/ && \
    rm -rf nu-0.91.0-x86_64-linux-gnu-full*

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]