{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    sbcl
    nodejs_20
    nodePackages.npm
    clang
    libffi
    h2spec
    openssl
    ripgrep
    pkg-config
    (pkgs.emacsWithPackages (epkgs: [
      epkgs.magit
      epkgs.projectile
      epkgs.company
      # Add more packages as needed
    ]))
  ];

  shellHook = ''
    echo "Epsilon development environment"
    echo "Available tools:"
    echo "  - SBCL: $(sbcl --version)"
    echo "  - Node: $(node --version)"
    echo "  - npm: $(npm --version)"
  '';
}
