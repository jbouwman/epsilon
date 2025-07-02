{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    sbcl
    nodejs_20
    nodePackages.npm
  ];

  shellHook = ''
    echo "Epsilon development environment"
    echo "Available tools:"
    echo "  - SBCL: $(sbcl --version)"
    echo "  - Node: $(node --version)"
    echo "  - npm: $(npm --version)"
  '';
}