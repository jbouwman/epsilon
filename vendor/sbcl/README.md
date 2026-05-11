# SBCL Fork (Locally Built)

Epsilon runs on a custom SBCL fork at https://github.com/jbouwman/sbcl,
maintained as a staging ground for features upstream SBCL doesn't yet have
(currently an in-progress green threads / fibers implementation).

**This directory does not ship binaries.** The per-platform subdirectories
(`linux-x86_64/`, `darwin-arm64/`, etc.) are produced locally by `build.sh`
and ignored by git. See `.gitignore`.

Historical deltas that no longer apply:
- **Struct-by-value callback support** was merged into upstream SBCL.
- **PR #22 foreign-thread signal handler preservation** was reverted
  from the fork after being judged wrongheaded.

## Getting SBCL

### Under Nix

`nix develop` provides the fork automatically via the `sbcl-epsilon`
derivation at `nixos/packages/sbcl-epsilon.nix`. Nothing to do.

### Without Nix (macOS, bare Linux)

Run the build script once:

```bash
./epsilon/vendor/sbcl/build.sh              # current platform, master
./epsilon/vendor/sbcl/build.sh --branch X   # specific branch
./epsilon/vendor/sbcl/build.sh --clean      # fresh clone, clean build
```

The script:
1. Clones `github:jbouwman/sbcl` into `.sbcl-build/`
2. Builds with `--fancy --with-sb-core-compression`
3. Installs to `epsilon/vendor/sbcl/<platform>/` (gitignored)

Platform auto-detection covers `linux-x86_64`, `linux-arm64`,
`darwin-arm64`, `darwin-x86_64`. Build requires an existing SBCL (any
recent version) as bootstrap compiler.

## Runtime Resolution

The `epsilon/epsilon` launcher picks an SBCL in this order:

1. `$EPSILON_SBCL` if set and executable (Nix build products)
2. `epsilon/vendor/sbcl/<platform>/sbcl` (output of `build.sh`)
3. Error, with pointer back to this README.

`SBCL_HOME` is set to the platform directory so contribs (sb-posix,
sb-simd, sb-bsd-sockets, etc.) load at runtime.

## Platform Layout

```
vendor/sbcl/
  build.sh                # Build script (auto-detects platform)
  README.md               # This file
  flake.nix               # Dev shell for running build.sh under Nix
  <platform>/             # Locally built, git-ignored
    sbcl                  # Runtime executable
    sbcl.core             # Core image
    VERSION               # Build info + commit hash
    contrib/              # sb-posix, sb-simd, etc.
```
