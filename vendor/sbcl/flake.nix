{
  description = "SBCL vendor build environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            file
            patchelf
          ];

          shellHook = ''
            # System podman (NixOS virtualisation.podman) and setuid wrappers
            export PATH="$PATH:/run/current-system/sw/bin:/run/wrappers/bin"
            echo "SBCL build environment ready"
            echo "  podman: $(podman --version 2>/dev/null || echo 'not found - enable virtualisation.podman on this host')"
            echo ""
            echo "Run: ./build.sh"
          '';
        };
      });
}
