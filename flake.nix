{
  description = "Epsilon SBCL utility library development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl
            nodejs_20
            nodePackages.npm
            git
            ripgrep
          ];

          shellHook = ''
            echo "Epsilon development environment"
            echo "Available tools:"
            echo "  - SBCL: $(sbcl --version)"
            echo "  - Node: $(node --version)"  
            echo "  - npm: $(npm --version)"
            echo "  - git: $(git --version)"
            echo "  - rg: $(rg --version | head -n1)"
          '';
        };
      });
}