#!/usr/bin/env bash
#
# Epsilon Installation Script
#
# Installs Epsilon from GitHub releases. Works for both interactive use
# and CI environments (auto-detected).
#
# Usage:
#   curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash
#
# Options:
#   --version VERSION   Install a specific version (e.g., 1.0.0)
#   --source            Install from source via git clone (requires SBCL)
#   --check             Check/verify existing installation
#   --uninstall         Remove epsilon installation
#   --completions       Install shell completions only
#   --manifest PATH     Install dependencies from manifest after install
#   --force             Skip confirmation prompts
#   --quiet             Suppress informational output
#   --help              Show this help message
#
# Environment Variables (useful for CI):
#   EPSILON_VERSION     Version to install (default: latest)
#   EPSILON_SOURCE      Set to 'true' for source installation
#   EPSILON_HOME        Installation directory (default: ~/.epsilon)
#   EPSILON_BIN         Binary symlink directory (default: ~/.local/bin)
#   EPSILON_MANIFEST    Manifest file to process after install
#   GITHUB_TOKEN        Optional token for higher API rate limits
#
# CI Usage:
#   curl -sSL .../install.sh | bash
#   EPSILON_VERSION=0.10.0 curl -sSL .../install.sh | bash
#   EPSILON_MANIFEST=epsilon.manifest curl -sSL .../install.sh | bash

set -euo pipefail

# Configuration
GITHUB_REPO="jbouwman/epsilon"
GITHUB_API_URL="https://api.github.com/repos/$GITHUB_REPO/releases"

# Defaults (can be overridden by env vars or CLI args)
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"
COMPLETIONS_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/epsilon/completions"

# Command line options (env vars as defaults)
INSTALL_VERSION="${EPSILON_VERSION:-}"
MANIFEST_PATH="${EPSILON_MANIFEST:-}"
INSTALL_SOURCE="${EPSILON_SOURCE:-false}"
CHECK_ONLY=false
UNINSTALL=false
COMPLETIONS_ONLY=false
SHOW_HELP=false
FORCE=false
QUIET=false

# Auto-detect CI environment
if [ ! -t 1 ] || [ -n "${CI:-}" ] || [ -n "${GITHUB_ACTIONS:-}" ] || [ -n "${GITLAB_CI:-}" ] || [ -n "${JENKINS_URL:-}" ]; then
    QUIET=true
    FORCE=true
    IS_CI=true
else
    IS_CI=false
fi

# Colors for output (disabled if not a terminal or in CI)
if [ -t 1 ] && [ "$IS_CI" = false ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    BOLD=''
    NC=''
fi

# Output helpers
error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warning() { [ "$QUIET" = false ] && echo -e "${YELLOW}[WARN]${NC} $1" || true; }
info() { [ "$QUIET" = false ] && echo -e "${BLUE}[INFO]${NC} $1" || true; }

show_help() {
    cat << 'EOF'
Epsilon Installation Script

Usage: install.sh [OPTIONS]

Options:
    --version VERSION   Install a specific version (e.g., 1.0.0 or v1.0.0)
    --source            Install from source via git clone (requires SBCL)
    --check             Check/verify existing installation without changes
    --uninstall         Remove epsilon installation completely
    --completions       Install shell completions only (requires existing install)
    --manifest PATH     Install dependencies from manifest after epsilon install
    --force             Skip confirmation prompts
    --quiet             Suppress informational output
    --help              Show this help message

Environment Variables:
    EPSILON_VERSION     Version to install (alternative to --version)
    EPSILON_SOURCE      Set to 'true' for source installation
    EPSILON_HOME        Installation directory (default: ~/.epsilon)
    EPSILON_BIN         Binary symlink directory (default: ~/.local/bin)
    EPSILON_MANIFEST    Manifest file path (alternative to --manifest)
    GITHUB_TOKEN        GitHub token for higher API rate limits

Examples:
    # Install latest stable release
    curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash

    # Install from source (for development)
    ./install.sh --source

    # Install specific version from source
    ./install.sh --source --version 0.10.0

    # Install specific version
    ./install.sh --version 1.0.0

    # Install via environment variable (useful for CI)
    EPSILON_VERSION=1.0.0 ./install.sh

    # Install from source via environment variable
    EPSILON_SOURCE=true ./install.sh

    # Install and process manifest
    ./install.sh --manifest epsilon.manifest

    # Check installation status
    ./install.sh --check

    # Uninstall
    ./install.sh --uninstall

For more information: https://github.com/jbouwman/epsilon
EOF
}

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --version)
                if [ -z "${2:-}" ]; then
                    error "--version requires a version argument"
                    exit 1
                fi
                INSTALL_VERSION="$2"
                shift 2
                ;;
            --check)
                CHECK_ONLY=true
                shift
                ;;
            --uninstall)
                UNINSTALL=true
                shift
                ;;
            --completions)
                COMPLETIONS_ONLY=true
                shift
                ;;
            --manifest)
                if [ -z "${2:-}" ]; then
                    error "--manifest requires a path argument"
                    exit 1
                fi
                MANIFEST_PATH="$2"
                shift 2
                ;;
            --force|-f)
                FORCE=true
                shift
                ;;
            --quiet|-q)
                QUIET=true
                shift
                ;;
            --source|-s)
                INSTALL_SOURCE=true
                shift
                ;;
            --help|-h)
                SHOW_HELP=true
                shift
                ;;
            *)
                error "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
}

# Platform detection
detect_platform() {
    local platform arch
    platform=$(uname -s | tr '[:upper:]' '[:lower:]')
    arch=$(uname -m)

    case "$platform" in
        darwin) platform="macos" ;;
        linux) platform="linux" ;;
        mingw*|msys*|cygwin*)
            error "Windows is not yet supported. Please use WSL2."
            exit 1
            ;;
        *)
            error "Unsupported platform: $platform"
            exit 1
            ;;
    esac

    case "$arch" in
        x86_64|amd64) arch="x86_64" ;;
        arm64|aarch64) arch="arm64" ;;
        *)
            error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac

    echo "${platform}-${arch}"
}

# Check for required tools
check_requirements() {
    local missing=false

    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
        error "Missing required tool: curl or wget"
        missing=true
    fi

    if ! command -v tar >/dev/null 2>&1; then
        error "Missing required tool: tar"
        missing=true
    fi

    if [ "$missing" = true ]; then
        exit 1
    fi
}

# HTTP fetch helper
fetch_url() {
    local url="$1"
    local headers=""

    if [ -n "${GITHUB_TOKEN:-}" ]; then
        headers="Authorization: token $GITHUB_TOKEN"
    fi

    if command -v curl >/dev/null 2>&1; then
        if [ -n "$headers" ]; then
            curl -sSL -H "$headers" "$url"
        else
            curl -sSL "$url"
        fi
    else
        if [ -n "$headers" ]; then
            wget -qO- --header="$headers" "$url"
        else
            wget -qO- "$url"
        fi
    fi
}

# Download file helper
download_file() {
    local url="$1"
    local output="$2"

    if command -v curl >/dev/null 2>&1; then
        curl -sSL "$url" -o "$output"
    else
        wget -qO "$output" "$url"
    fi
}

# Get release info from GitHub API
get_release_info() {
    local version="$1"
    local release_info

    if [ -n "$version" ]; then
        # Normalize version (remove 'v' prefix if present)
        local api_version="${version#v}"
        info "Fetching release v${api_version}..."
        release_info=$(fetch_url "${GITHUB_API_URL}/tags/v${api_version}" 2>/dev/null || true)

        if [ -z "$release_info" ] || echo "$release_info" | grep -q '"message".*"Not Found"'; then
            error "Version v${api_version} not found"
            echo "Available versions: https://github.com/$GITHUB_REPO/releases" >&2
            exit 1
        fi
    else
        info "Fetching latest stable release..."
        release_info=$(fetch_url "${GITHUB_API_URL}/latest")
    fi

    if ! echo "$release_info" | grep -q '"tag_name"'; then
        error "Failed to fetch release information from GitHub"
        exit 1
    fi

    echo "$release_info"
}

# Extract download URL from release info
get_download_url() {
    local release_info="$1"
    local platform_arch="$2"

    # Extract version - portable across BSD and GNU sed
    local version
    version=$(echo "$release_info" | grep -o '"tag_name"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed 's/.*"tag_name"[[:space:]]*:[[:space:]]*"v*//' | sed 's/".*//')
    info "Version: $version"

    # Extract download URL for the platform
    local download_url
    download_url=$(echo "$release_info" | grep -o '"browser_download_url"[[:space:]]*:[[:space:]]*"[^"]*"' | grep "${platform_arch}\.tar\.gz" | head -1 | sed 's/.*"browser_download_url"[[:space:]]*:[[:space:]]*"//' | sed 's/".*//')

    if [ -z "$download_url" ]; then
        error "No release found for platform: $platform_arch"
        exit 1
    fi

    echo "$download_url"
}

# Verify SHA-256 checksum
verify_checksum() {
    local file="$1"
    local checksum_url="$2"

    local expected
    expected=$(fetch_url "$checksum_url" 2>/dev/null | awk '{print tolower($1)}' || true)

    if [ -z "$expected" ]; then
        info "No checksum file found, skipping verification"
        return 0
    fi

    local actual
    if command -v sha256sum >/dev/null 2>&1; then
        actual=$(sha256sum "$file" | awk '{print tolower($1)}')
    elif command -v shasum >/dev/null 2>&1; then
        actual=$(shasum -a 256 "$file" | awk '{print tolower($1)}')
    else
        info "No SHA-256 tool available, skipping verification"
        return 0
    fi

    if [ "$expected" != "$actual" ]; then
        error "Checksum verification failed"
        error "Expected: $expected"
        error "Actual:   $actual"
        exit 1
    fi

    success "Checksum verified"
}

# Download and extract release
download_and_extract() {
    local download_url="$1"

    info "Downloading Epsilon..."

    local temp_dir
    temp_dir=$(mktemp -d)
    trap "rm -rf $temp_dir" EXIT

    local archive_file="$temp_dir/epsilon.tar.gz"
    download_file "$download_url" "$archive_file"
    success "Download complete"

    # Verify checksum
    verify_checksum "$archive_file" "${download_url}.sha256"

    info "Extracting archive..."
    tar -xzf "$archive_file" -C "$temp_dir"

    local extracted_dir
    extracted_dir=$(find "$temp_dir" -maxdepth 1 -type d -name "epsilon-*" | head -1)
    if [ -z "$extracted_dir" ]; then
        error "Failed to find extracted directory"
        exit 1
    fi

    # Handle existing installation
    if [ -d "$INSTALL_DIR" ] && [ "$(ls -A "$INSTALL_DIR" 2>/dev/null)" ]; then
        if [ "$FORCE" = true ]; then
            info "Removing existing installation..."
            rm -rf "${INSTALL_DIR:?}"/*
        else
            warning "Existing installation found at $INSTALL_DIR"
            read -p "Remove existing installation? (y/N) " -n 1 -r < /dev/tty
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                rm -rf "${INSTALL_DIR:?}"/*
            else
                error "Installation cancelled"
                exit 1
            fi
        fi
    fi

    mkdir -p "$INSTALL_DIR"
    cp -r "$extracted_dir"/* "$INSTALL_DIR/"
    success "Extracted to $INSTALL_DIR"
}

# Install from source (git clone)
install_from_source() {
    info "Installing from source..."

    # Check for git
    if ! command -v git >/dev/null 2>&1; then
        error "git is required for source installation"
        exit 1
    fi

    # Check for SBCL (required for source installs)
    if ! command -v sbcl >/dev/null 2>&1; then
        error "SBCL is required for source installation"
        echo "Install SBCL: https://www.sbcl.org/" >&2
        exit 1
    fi

    # Handle existing installation
    if [ -d "$INSTALL_DIR" ]; then
        if [ -d "$INSTALL_DIR/.git" ]; then
            # Existing git clone - update it
            info "Updating existing source installation..."
            cd "$INSTALL_DIR"
            git fetch --tags --quiet
        else
            # Existing non-git installation
            if [ "$FORCE" = true ]; then
                info "Removing existing installation..."
                rm -rf "${INSTALL_DIR:?}"
            else
                warning "Existing installation found at $INSTALL_DIR (not a git clone)"
                read -p "Remove and reinstall from source? (y/N) " -n 1 -r < /dev/tty
                echo
                if [[ $REPLY =~ ^[Yy]$ ]]; then
                    rm -rf "${INSTALL_DIR:?}"
                else
                    error "Installation cancelled"
                    exit 1
                fi
            fi
        fi
    fi

    # Clone if needed
    if [ ! -d "$INSTALL_DIR/.git" ]; then
        info "Cloning repository..."
        git clone --quiet "https://github.com/$GITHUB_REPO.git" "$INSTALL_DIR"
    fi

    cd "$INSTALL_DIR"

    # Checkout specific version if requested
    if [ -n "$INSTALL_VERSION" ]; then
        local tag="v${INSTALL_VERSION#v}"
        info "Checking out $tag..."
        if ! git checkout --quiet "$tag" 2>/dev/null; then
            error "Version $tag not found"
            echo "Available versions:" >&2
            git tag -l 'v*' | tail -5 >&2
            exit 1
        fi
    else
        # Stay on default branch (main)
        git checkout --quiet main 2>/dev/null || true
    fi

    success "Source installed to $INSTALL_DIR"
}

# Create symlinks
create_symlinks() {
    [ "$IS_CI" = true ] && return 0  # Skip symlinks in CI

    info "Creating symlinks..."
    mkdir -p "$BINARY_DIR"

    local epsilon_bin
    if [ "$INSTALL_SOURCE" = true ]; then
        epsilon_bin="$INSTALL_DIR/epsilon"  # Source checkout
    else
        epsilon_bin="$INSTALL_DIR/bin/epsilon"  # Release build
    fi
    local symlink_path="$BINARY_DIR/epsilon"

    if [ ! -f "$epsilon_bin" ]; then
        error "Epsilon binary not found at $epsilon_bin"
        exit 1
    fi

    [ -L "$symlink_path" ] && rm "$symlink_path"
    [ -f "$symlink_path" ] && rm "$symlink_path"

    ln -sf "$epsilon_bin" "$symlink_path"
    success "Created symlink at $symlink_path"
}

# Install shell completions
install_completions() {
    [ "$IS_CI" = true ] && return 0  # Skip completions in CI

    info "Installing shell completions..."
    mkdir -p "$COMPLETIONS_DIR"

    # Generate Bash completion
    cat > "$COMPLETIONS_DIR/epsilon.bash" << 'BASH_COMPLETION'
_epsilon_completions() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="--help --version --version-json --debug --quiet --eval --module --load --test --exec --modules --path --module-dir --init --new --doctor --verbose --log"

    case "$prev" in
        --module|--test)
            local modules
            if command -v epsilon >/dev/null 2>&1; then
                modules=$(epsilon --modules 2>/dev/null | grep -E '^epsilon\.' | awk '{print $1}' || true)
            fi
            COMPREPLY=( $(compgen -W "$modules" -- "$cur") )
            return 0
            ;;
        --load|--path)
            COMPREPLY=( $(compgen -f -- "$cur") )
            return 0
            ;;
    esac

    if [[ "$cur" == -* ]]; then
        COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
    fi
    return 0
}
complete -F _epsilon_completions epsilon
BASH_COMPLETION

    # Generate Zsh completion
    cat > "$COMPLETIONS_DIR/_epsilon" << 'ZSH_COMPLETION'
#compdef epsilon
_epsilon() {
    local -a opts
    opts=(
        '--help[Show help message]'
        '--version[Show version information]'
        '--debug[Enable debugger]'
        '--quiet[Suppress warnings]'
        '--eval[Evaluate expression]:expression:'
        '--module[Load module]:module:'
        '--load[Load file]:file:_files'
        '--test[Run tests]:module:'
        '--exec[Execute function]:function:'
        '--modules[List modules]'
        '--path[Module path]:path:_files -/'
        '--init[Initialize new project]'
        '--new[Create new module]:name:'
        '--doctor[Run diagnostics]'
    )
    _arguments -s $opts
}
_epsilon "$@"
ZSH_COMPLETION

    success "Shell completions installed to $COMPLETIONS_DIR"

    if [ "$QUIET" = false ]; then
        echo ""
        echo "To enable completions, add to your shell config:"
        echo "  Bash: source $COMPLETIONS_DIR/epsilon.bash"
        echo "  Zsh:  fpath=($COMPLETIONS_DIR \$fpath); autoload -Uz compinit && compinit"
    fi
}

# Update shell configuration hints
update_shell_config() {
    [ "$IS_CI" = true ] && return 0

    if [[ ":$PATH:" != *":$BINARY_DIR:"* ]]; then
        warning "$BINARY_DIR is not in your PATH"
        echo ""
        echo "Add to your shell config:"
        echo "  export PATH=\"$BINARY_DIR:\$PATH\""
    else
        success "PATH is correctly configured"
    fi
}

# Verify installation
verify_installation() {
    info "Verifying installation..."

    local epsilon_bin
    if [ "$INSTALL_SOURCE" = true ]; then
        epsilon_bin="$INSTALL_DIR/epsilon"
    else
        epsilon_bin="$INSTALL_DIR/bin/epsilon"
    fi

    if [ ! -x "$epsilon_bin" ]; then
        error "Epsilon binary not found or not executable"
        return 1
    fi
    success "Binary exists and is executable"

    # Test execution
    if "$epsilon_bin" --quiet --eval "(+ 1 2)" >/dev/null 2>&1; then
        success "Execution test passed"
    else
        error "Execution test failed"
        return 1
    fi

    # Show version
    local version_line
    version_line=$("$epsilon_bin" --version 2>&1 | grep -i "version" | head -1 || echo "")
    if [ -n "$version_line" ]; then
        info "Installed: $version_line"
    fi
}

# Install manifest dependencies
install_manifest_deps() {
    if [ ! -f "$MANIFEST_PATH" ]; then
        error "Manifest file not found: $MANIFEST_PATH"
        return 1
    fi

    info "Installing dependencies from $MANIFEST_PATH..."

    local epsilon_bin="$INSTALL_DIR/bin/epsilon"
    if "$epsilon_bin" --install-manifest "$MANIFEST_PATH"; then
        success "Manifest dependencies installed"
    else
        error "Failed to install manifest dependencies"
        return 1
    fi
}

# Check existing installation
check_installation() {
    echo ""
    echo "Epsilon Installation Check"
    echo "=========================="
    echo ""

    local status=0

    if [ -d "$INSTALL_DIR" ]; then
        success "Installation directory: $INSTALL_DIR"
        if [ -f "$INSTALL_DIR/VERSION" ]; then
            info "Version: $(cat "$INSTALL_DIR/VERSION")"
        fi
        if [ -d "$INSTALL_DIR/.git" ]; then
            info "Type: source installation"
        else
            info "Type: release installation"
        fi
    else
        error "Installation directory not found: $INSTALL_DIR"
        status=1
    fi

    # Check for epsilon binary (source: at root, release: in bin/)
    local epsilon_bin
    if [ -x "$INSTALL_DIR/epsilon" ]; then
        epsilon_bin="$INSTALL_DIR/epsilon"
    elif [ -x "$INSTALL_DIR/bin/epsilon" ]; then
        epsilon_bin="$INSTALL_DIR/bin/epsilon"
    fi

    if [ -n "$epsilon_bin" ]; then
        success "Binary: $epsilon_bin"
        if "$epsilon_bin" --quiet --eval "(+ 1 2)" >/dev/null 2>&1; then
            success "Execution test passed"
        else
            error "Execution test failed"
            status=1
        fi
    else
        error "Binary not found or not executable"
        status=1
    fi

    return $status
}

# Uninstall epsilon
uninstall_epsilon() {
    echo ""
    echo "Epsilon Uninstallation"
    echo "======================"
    echo ""

    if [ "$FORCE" != true ]; then
        echo "This will remove:"
        [ -d "$INSTALL_DIR" ] && echo "  - $INSTALL_DIR"
        [ -L "$BINARY_DIR/epsilon" ] && echo "  - $BINARY_DIR/epsilon"
        [ -d "$COMPLETIONS_DIR" ] && echo "  - $COMPLETIONS_DIR"
        echo ""
        read -p "Are you sure? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Uninstallation cancelled"
            exit 0
        fi
    fi

    [ -L "$BINARY_DIR/epsilon" ] && rm "$BINARY_DIR/epsilon" && success "Removed symlink"
    [ -f "$BINARY_DIR/epsilon" ] && rm "$BINARY_DIR/epsilon" && success "Removed binary"
    [ -d "$INSTALL_DIR" ] && rm -rf "$INSTALL_DIR" && success "Removed installation"
    [ -d "$COMPLETIONS_DIR" ] && rm -rf "$COMPLETIONS_DIR" && success "Removed completions"

    success "Epsilon has been uninstalled"
}

# Show post-install message
show_post_install() {
    if [ "$IS_CI" = true ]; then
        # CI-specific output
        local bin_path
        if [ "$INSTALL_SOURCE" = true ]; then
            bin_path="$INSTALL_DIR"  # Source: epsilon script at root
        else
            bin_path="$INSTALL_DIR/bin"  # Release: epsilon in bin/
        fi
        echo ""
        echo "Add to your CI workflow:"
        echo "  export PATH=\"$bin_path:\$PATH\""
        echo "  export EPSILON_HOME=\"$INSTALL_DIR\""
    else
        echo ""
        echo "================================"
        echo "  Installation Complete!"
        echo "================================"
        echo ""
        echo "Quick start:"
        echo "  epsilon                    # Start REPL"
        echo "  epsilon --eval '(+ 1 2)'   # Evaluate expression"
        echo "  epsilon --help             # Show all options"
        echo ""
        echo "Documentation: https://github.com/$GITHUB_REPO"
    fi
}

# Main
main() {
    parse_args "$@"

    if [ "$SHOW_HELP" = true ]; then
        show_help
        exit 0
    fi

    if [ "$CHECK_ONLY" = true ]; then
        check_installation
        exit $?
    fi

    if [ "$UNINSTALL" = true ]; then
        uninstall_epsilon
        exit 0
    fi

    if [ "$COMPLETIONS_ONLY" = true ]; then
        if [ ! -d "$INSTALL_DIR" ]; then
            error "Epsilon not installed. Run install.sh first."
            exit 1
        fi
        install_completions
        exit 0
    fi

    # Full installation
    [ "$IS_CI" = false ] && echo "" && echo "Installing Epsilon..." && echo ""

    check_requirements

    local platform_arch
    platform_arch=$(detect_platform)
    success "Platform: $platform_arch"

    if [ "$INSTALL_SOURCE" = true ]; then
        # Source installation via git clone
        install_from_source
    else
        # Release installation via download
        local release_info
        release_info=$(get_release_info "$INSTALL_VERSION")

        local download_url
        download_url=$(get_download_url "$release_info" "$platform_arch")

        download_and_extract "$download_url"
    fi

    create_symlinks
    install_completions
    update_shell_config
    verify_installation

    if [ -n "$MANIFEST_PATH" ]; then
        install_manifest_deps
    fi

    show_post_install
}

trap 'error "Installation failed on line $LINENO"' ERR
main "$@"
