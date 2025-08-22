{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    go
    git
    gcc
    gnumake
  ];

  shellHook = ''
    echo "Go development environment loaded"
    echo "Building h2spec..."
    
    # Set up Go environment
    export GOPATH=$HOME/go
    export PATH=$GOPATH/bin:$PATH
    
    # Clone and build h2spec if not already built
    if [ ! -f "$GOPATH/bin/h2spec" ]; then
      echo "Installing h2spec..."
      go install github.com/summerwind/h2spec/cmd/h2spec@latest
    fi
    
    echo "h2spec is ready at: $GOPATH/bin/h2spec"
    echo "Run: h2spec --help"
  '';
}