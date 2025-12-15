#!/usr/bin/env bash
#
# Epsilon Installation Script
#
# This script installs Epsilon from GitHub releases
# Usage: curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash
#
# Options:
#   --version VERSION   Install a specific version (e.g., 1.0.0)
#   --nightly           Install the latest nightly build
#   --check             Check/verify existing installation
#   --uninstall         Remove epsilon installation
#   --completions       Install shell completions only
#   --help              Show this help message
#

set -euo pipefail

# Configuration
GITHUB_REPO="jbouwman/epsilon"
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"
GITHUB_API_URL="https://api.github.com/repos/$GITHUB_REPO/releases"
COMPLETIONS_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/epsilon/completions"

# Colors for output (disabled if not a terminal)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    BOLD=''
    NC=''
fi

# Command line options
INSTALL_VERSION=""
INSTALL_NIGHTLY=false
CHECK_ONLY=false
UNINSTALL=false
COMPLETIONS_ONLY=false
SHOW_HELP=false
FORCE=false
QUIET=false

# Functions for output
error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }
info() { [ "$QUIET" = false ] && echo -e "${BLUE}[INFO]${NC} $1"; }
debug() { [ "$QUIET" = false ] && [ -n "${DEBUG:-}" ] && echo -e "[DEBUG] $1"; }

show_help() {
    cat << 'EOF'
Epsilon Installation Script

Usage: install.sh [OPTIONS]

Options:
    --version VERSION   Install a specific version (e.g., 1.0.0 or v1.0.0)
    --nightly           Install the latest nightly build
    --check             Check/verify existing installation without changes
    --uninstall         Remove epsilon installation completely
    --completions       Install shell completions only (requires existing install)
    --force             Skip confirmation prompts
    --quiet             Suppress informational output
    --help              Show this help message

Environment Variables:
    EPSILON_HOME        Installation directory (default: ~/.epsilon)
    EPSILON_BIN         Binary symlink directory (default: ~/.local/bin)

Examples:
    # Install latest stable release
    curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash

    # Install specific version
    ./install.sh --version 1.0.0

    # Install nightly build
    ./install.sh --nightly

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
            --nightly)
                INSTALL_NIGHTLY=true
                shift
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
            --force|-f)
                FORCE=true
                shift
                ;;
            --quiet|-q)
                QUIET=true
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
        darwin)
            platform="macos"
            ;;
        linux)
            platform="linux"
            ;;
        mingw*|msys*|cygwin*)
            error "Windows is not yet supported"
            echo "Please use WSL2 for Windows installation"
            exit 1
            ;;
        *)
            error "Unsupported platform: $platform"
            exit 1
            ;;
    esac

    case "$arch" in
        x86_64|amd64)
            arch="x86_64"
            ;;
        arm64|aarch64)
            arch="arm64"
            ;;
        *)
            error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac

    echo "${platform}-${arch}"
}

# Check for required tools
check_requirements() {
    local missing_tools=()

    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
        missing_tools+=("curl or wget")
    fi

    if ! command -v tar >/dev/null 2>&1; then
        missing_tools+=("tar")
    fi

    if [ ${#missing_tools[@]} -gt 0 ]; then
        error "Missing required tools: ${missing_tools[*]}"
        echo ""
        echo "Please install the missing tools and try again:"
        for tool in "${missing_tools[@]}"; do
            echo "  - $tool"
        done
        exit 1
    fi
}

# HTTP fetch helper (works with curl or wget)
fetch_url() {
    local url="$1"
    if command -v curl >/dev/null 2>&1; then
        curl -sSL "$url"
    else
        wget -qO- "$url"
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
        # Normalize version (remove 'v' prefix if present for API call)
        local api_version="${version#v}"
        info "Fetching release v${api_version}..."
        release_info=$(fetch_url "${GITHUB_API_URL}/tags/v${api_version}" 2>/dev/null || true)

        if [ -z "$release_info" ] || echo "$release_info" | grep -q '"message".*"Not Found"'; then
            error "Version v${api_version} not found"
            echo ""
            echo "Available versions can be found at:"
            echo "  https://github.com/$GITHUB_REPO/releases"
            exit 1
        fi
    elif [ "$INSTALL_NIGHTLY" = true ]; then
        info "Fetching latest nightly build..."
        # Look for releases tagged with 'nightly'
        release_info=$(fetch_url "${GITHUB_API_URL}/tags/nightly" 2>/dev/null || true)

        if [ -z "$release_info" ] || echo "$release_info" | grep -q '"message".*"Not Found"'; then
            warning "No nightly release found, falling back to latest pre-release..."
            # Fall back to latest pre-release
            local releases
            releases=$(fetch_url "${GITHUB_API_URL}?per_page=10")
            release_info=$(echo "$releases" | grep -A 100 '"prerelease": true' | head -100)

            if [ -z "$release_info" ]; then
                error "No nightly or pre-release builds available"
                exit 1
            fi
        fi
    else
        info "Fetching latest stable release..."
        release_info=$(fetch_url "${GITHUB_API_URL}/latest")
    fi

    # Check if we got valid JSON
    if ! echo "$release_info" | grep -q '"tag_name"'; then
        error "Failed to fetch release information from GitHub"
        echo "Response: $release_info"
        exit 1
    fi

    echo "$release_info"
}

# Extract download URL from release info
get_download_url() {
    local release_info="$1"
    local platform_arch="$2"

    # Extract version
    local version
    version=$(echo "$release_info" | grep -o '"tag_name":[[:space:]]*"[^"]*"' | sed 's/.*"tag_name":[[:space:]]*"v\?\([^"]*\)".*/\1/')
    info "Version: $version"

    # Extract download URL for the platform
    local download_url
    download_url=$(echo "$release_info" | grep -o "\"browser_download_url\":[[:space:]]*\"[^\"]*epsilon-[^\"]*${platform_arch}\.tar\.gz\"" | sed 's/.*"browser_download_url":[[:space:]]*"\([^"]*\)".*/\1/' | head -1)

    if [ -z "$download_url" ]; then
        error "Could not find release for platform: $platform_arch"
        echo ""
        echo "Available releases:"
        echo "$release_info" | grep -o '"name":[[:space:]]*"[^"]*"' | sed 's/.*"name":[[:space:]]*"\([^"]*\)".*/  \1/'
        echo ""
        echo "Your platform: $platform_arch"
        echo ""
        echo "Possible solutions:"
        echo "  1. Check if a release exists for your platform"
        echo "  2. Try a different version with --version"
        echo "  3. Build from source (see INSTALL.md)"
        exit 1
    fi

    echo "$download_url"
}

# Download and extract release
download_and_extract() {
    local download_url="$1"

    info "Downloading Epsilon..."
    debug "URL: $download_url"

    # Create temp directory
    local temp_dir
    temp_dir=$(mktemp -d)
    trap "rm -rf $temp_dir" EXIT

    # Download
    local archive_file="$temp_dir/epsilon.tar.gz"
    download_file "$download_url" "$archive_file"

    success "Download complete"

    # Extract to temp directory first
    info "Extracting archive..."
    tar -xzf "$archive_file" -C "$temp_dir"

    # Find the extracted directory (should be epsilon-VERSION-PLATFORM)
    local extracted_dir
    extracted_dir=$(find "$temp_dir" -maxdepth 1 -type d -name "epsilon-*" | head -1)
    if [ -z "$extracted_dir" ]; then
        error "Failed to find extracted directory"
        exit 1
    fi

    # Handle existing installation
    if [ -d "$INSTALL_DIR" ]; then
        if [ "$FORCE" = true ]; then
            info "Removing existing installation..."
            rm -rf "$INSTALL_DIR"
        else
            warning "Existing installation found at $INSTALL_DIR"
            read -p "Remove existing installation? (y/N) " -n 1 -r < /dev/tty
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                rm -rf "$INSTALL_DIR"
            else
                error "Installation cancelled"
                exit 1
            fi
        fi
    fi

    # Move to installation directory
    mkdir -p "$(dirname "$INSTALL_DIR")"
    mv "$extracted_dir" "$INSTALL_DIR"

    success "Extracted to $INSTALL_DIR"
}

# Create symlinks
create_symlinks() {
    info "Creating symlinks..."

    # Create binary directory if it doesn't exist
    mkdir -p "$BINARY_DIR"

    # Create symlink to epsilon
    local epsilon_bin="$INSTALL_DIR/bin/epsilon"
    local symlink_path="$BINARY_DIR/epsilon"

    if [ ! -f "$epsilon_bin" ]; then
        error "Epsilon binary not found at $epsilon_bin"
        exit 1
    fi

    # Remove existing symlink if present
    [ -L "$symlink_path" ] && rm "$symlink_path"
    [ -f "$symlink_path" ] && rm "$symlink_path"

    # Create new symlink
    ln -sf "$epsilon_bin" "$symlink_path"

    success "Created symlink at $symlink_path"
}

# Install shell completions
install_completions() {
    info "Installing shell completions..."

    mkdir -p "$COMPLETIONS_DIR"

    # Check if completion files exist in the installation
    local source_completions="$INSTALL_DIR/completions"

    if [ -d "$source_completions" ]; then
        # Copy from installation
        cp -r "$source_completions"/* "$COMPLETIONS_DIR/" 2>/dev/null || true
        success "Shell completions installed to $COMPLETIONS_DIR"
    else
        # Generate basic completions
        generate_completions
    fi

    # Show setup instructions
    echo ""
    echo "To enable completions, add to your shell config:"
    echo ""

    # Bash
    if [ -f "$COMPLETIONS_DIR/epsilon.bash" ]; then
        echo "  ${BOLD}Bash${NC} (~/.bashrc):"
        echo "    source $COMPLETIONS_DIR/epsilon.bash"
        echo ""
    fi

    # Zsh
    if [ -f "$COMPLETIONS_DIR/_epsilon" ]; then
        echo "  ${BOLD}Zsh${NC} (~/.zshrc):"
        echo "    fpath=($COMPLETIONS_DIR \$fpath)"
        echo "    autoload -Uz compinit && compinit"
        echo ""
    fi

    # Fish
    if [ -f "$COMPLETIONS_DIR/epsilon.fish" ]; then
        echo "  ${BOLD}Fish${NC}:"
        echo "    cp $COMPLETIONS_DIR/epsilon.fish ~/.config/fish/completions/"
        echo ""
    fi
}

# Generate basic shell completions
generate_completions() {
    mkdir -p "$COMPLETIONS_DIR"

    # Generate Bash completion
    cat > "$COMPLETIONS_DIR/epsilon.bash" << 'BASH_COMPLETION'
# Epsilon bash completion
_epsilon_completions() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Main options
    opts="--help --version --version-json --debug --quiet --eval --module --load --test --exec --modules --path --module-dir --init --new --doctor --verbose --log"

    case "$prev" in
        --module|--test)
            # Try to get module list from epsilon
            local modules
            if command -v epsilon >/dev/null 2>&1; then
                modules=$(epsilon --modules 2>/dev/null | grep -E '^epsilon\.' | awk '{print $1}' || true)
            fi
            COMPREPLY=( $(compgen -W "$modules" -- "$cur") )
            return 0
            ;;
        --load|--path)
            # File/directory completion
            COMPREPLY=( $(compgen -f -- "$cur") )
            return 0
            ;;
        --exec)
            # Package:function completion (limited)
            COMPREPLY=()
            return 0
            ;;
        --new)
            # No completion for module name
            COMPREPLY=()
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
        '--version-json[Show version as JSON]'
        '--debug[Enable debugger]'
        '--quiet[Suppress warnings]'
        '--verbose[Enable verbose output]'
        '--log[Configure logging]:spec:'
        '--eval[Evaluate expression]:expression:'
        '--module[Load module]:module:_epsilon_modules'
        '--load[Load file]:file:_files'
        '--test[Run tests]:module:_epsilon_modules'
        '--exec[Execute function]:function:'
        '--modules[List modules]'
        '--path[Module path]:path:_files -/'
        '--module-dir[Module directory]:directory:_files -/'
        '--init[Initialize new project]'
        '--new[Create new module]:name:'
        '--doctor[Run environment diagnostics]'
    )

    _arguments -s $opts
}

_epsilon_modules() {
    local -a modules
    if (( $+commands[epsilon] )); then
        modules=(${(f)"$(epsilon --modules 2>/dev/null | grep -E '^epsilon\.' | awk '{print $1}')"})
    fi
    _describe -t modules 'modules' modules
}

_epsilon "$@"
ZSH_COMPLETION

    # Generate Fish completion
    cat > "$COMPLETIONS_DIR/epsilon.fish" << 'FISH_COMPLETION'
# Epsilon fish completion

complete -c epsilon -s h -l help -d 'Show help message'
complete -c epsilon -l version -d 'Show version information'
complete -c epsilon -l version-json -d 'Show version as JSON'
complete -c epsilon -l debug -d 'Enable debugger'
complete -c epsilon -l quiet -d 'Suppress warnings'
complete -c epsilon -l verbose -d 'Enable verbose output'
complete -c epsilon -l log -d 'Configure logging' -r
complete -c epsilon -l eval -d 'Evaluate expression' -r
complete -c epsilon -l module -d 'Load module' -r -a '(epsilon --modules 2>/dev/null | grep -E "^epsilon\\\\." | awk "{print \$1}")'
complete -c epsilon -l load -d 'Load file' -r -F
complete -c epsilon -l test -d 'Run tests' -r -a '(epsilon --modules 2>/dev/null | grep -E "^epsilon\\\\." | awk "{print \$1}")'
complete -c epsilon -l exec -d 'Execute function' -r
complete -c epsilon -l modules -d 'List modules'
complete -c epsilon -l path -d 'Module path' -r -a '(__fish_complete_directories)'
complete -c epsilon -l module-dir -d 'Module directory' -r -a '(__fish_complete_directories)'
complete -c epsilon -l init -d 'Initialize new project'
complete -c epsilon -l new -d 'Create new module' -r
complete -c epsilon -l doctor -d 'Run environment diagnostics'
FISH_COMPLETION

    success "Generated shell completions"
}

# Update shell configuration
update_shell_config() {
    info "Checking PATH configuration..."

    # Check if binary directory is in PATH
    if [[ ":$PATH:" != *":$BINARY_DIR:"* ]]; then
        warning "$BINARY_DIR is not in your PATH"

        # Detect shell configuration file
        local shell_config=""
        local shell_name=""

        if [ -n "${BASH_VERSION:-}" ]; then
            shell_config="$HOME/.bashrc"
            shell_name="bash"
        elif [ -n "${ZSH_VERSION:-}" ]; then
            shell_config="$HOME/.zshrc"
            shell_name="zsh"
        elif [ -n "${FISH_VERSION:-}" ]; then
            shell_config="$HOME/.config/fish/config.fish"
            shell_name="fish"
        else
            shell_config="$HOME/.profile"
            shell_name="shell"
        fi

        echo ""
        echo "Add the following to your $shell_config:"
        echo ""
        if [ "$shell_name" = "fish" ]; then
            echo "  set -gx PATH $BINARY_DIR \$PATH"
        else
            echo "  export PATH=\"$BINARY_DIR:\$PATH\""
        fi
        echo ""
        echo "Then reload your shell configuration:"
        if [ "$shell_name" = "fish" ]; then
            echo "  source $shell_config"
        else
            echo "  source $shell_config"
        fi
    else
        success "PATH is correctly configured"
    fi
}

# Verify installation
verify_installation() {
    info "Verifying installation..."

    local epsilon_cmd="$BINARY_DIR/epsilon"
    local errors=0

    # Check binary exists
    if [ ! -x "$epsilon_cmd" ]; then
        error "Epsilon executable not found or not executable at $epsilon_cmd"
        return 1
    fi
    success "Binary exists and is executable"

    # Check installation directory
    if [ ! -d "$INSTALL_DIR" ]; then
        error "Installation directory not found: $INSTALL_DIR"
        return 1
    fi
    success "Installation directory exists"

    # Check core modules
    if [ ! -d "$INSTALL_DIR/modules/core" ]; then
        error "Core modules not found"
        return 1
    fi
    success "Core modules present"

    # Test epsilon with a simple command
    info "Testing epsilon execution..."
    if "$epsilon_cmd" --quiet --eval "(+ 1 2)" >/dev/null 2>&1; then
        success "Execution test passed"
    else
        error "Execution test failed"
        errors=$((errors + 1))
    fi

    # Show version
    local version
    version=$("$epsilon_cmd" --version 2>&1 | grep -i "version" | head -1 || echo "unknown")
    info "Installed version: $version"

    return $errors
}

# Check existing installation (--check mode)
check_installation() {
    echo ""
    echo "========================================="
    echo "   Epsilon Installation Check"
    echo "========================================="
    echo ""

    local status=0

    # Check binary
    echo "Checking binary..."
    if [ -L "$BINARY_DIR/epsilon" ]; then
        local target
        target=$(readlink "$BINARY_DIR/epsilon")
        if [ -x "$BINARY_DIR/epsilon" ]; then
            success "Binary symlink: $BINARY_DIR/epsilon -> $target"
        else
            error "Binary symlink exists but target is not executable"
            status=1
        fi
    elif [ -x "$BINARY_DIR/epsilon" ]; then
        success "Binary: $BINARY_DIR/epsilon"
    else
        error "Binary not found at $BINARY_DIR/epsilon"
        status=1
    fi

    # Check installation directory
    echo ""
    echo "Checking installation directory..."
    if [ -d "$INSTALL_DIR" ]; then
        success "Installation directory: $INSTALL_DIR"

        # Check VERSION file
        if [ -f "$INSTALL_DIR/VERSION" ]; then
            local version
            version=$(cat "$INSTALL_DIR/VERSION")
            info "Version: $version"
        fi

        # Count modules
        if [ -d "$INSTALL_DIR/modules" ]; then
            local module_count
            module_count=$(find "$INSTALL_DIR/modules" -name "module.lisp" -type f | wc -l | tr -d ' ')
            info "Modules found: $module_count"
        fi
    else
        error "Installation directory not found: $INSTALL_DIR"
        status=1
    fi

    # Check PATH
    echo ""
    echo "Checking PATH..."
    if [[ ":$PATH:" == *":$BINARY_DIR:"* ]]; then
        success "Binary directory in PATH"
    else
        warning "Binary directory not in PATH: $BINARY_DIR"
    fi

    # Check shell completions
    echo ""
    echo "Checking shell completions..."
    if [ -d "$COMPLETIONS_DIR" ]; then
        local comp_count
        comp_count=$(find "$COMPLETIONS_DIR" -type f | wc -l | tr -d ' ')
        if [ "$comp_count" -gt 0 ]; then
            success "Shell completions installed: $comp_count files"
        else
            warning "Completions directory exists but is empty"
        fi
    else
        warning "Shell completions not installed"
    fi

    # Test execution
    echo ""
    echo "Testing execution..."
    if command -v epsilon >/dev/null 2>&1; then
        if epsilon --quiet --eval "(+ 1 2)" >/dev/null 2>&1; then
            success "Execution test passed"
        else
            error "Execution test failed"
            status=1
        fi
    else
        error "epsilon command not found in PATH"
        status=1
    fi

    # Summary
    echo ""
    echo "========================================="
    if [ $status -eq 0 ]; then
        echo -e "   ${GREEN}Installation OK${NC}"
    else
        echo -e "   ${RED}Installation has issues${NC}"
    fi
    echo "========================================="

    return $status
}

# Uninstall epsilon
uninstall_epsilon() {
    echo ""
    echo "========================================="
    echo "   Epsilon Uninstallation"
    echo "========================================="
    echo ""

    local removed_something=false

    # Confirm unless --force
    if [ "$FORCE" != true ]; then
        echo "This will remove:"
        [ -d "$INSTALL_DIR" ] && echo "  - $INSTALL_DIR"
        [ -L "$BINARY_DIR/epsilon" ] && echo "  - $BINARY_DIR/epsilon (symlink)"
        [ -d "$COMPLETIONS_DIR" ] && echo "  - $COMPLETIONS_DIR"
        echo ""
        read -p "Are you sure? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Uninstallation cancelled"
            exit 0
        fi
    fi

    # Remove binary symlink
    if [ -L "$BINARY_DIR/epsilon" ]; then
        rm "$BINARY_DIR/epsilon"
        success "Removed symlink: $BINARY_DIR/epsilon"
        removed_something=true
    elif [ -f "$BINARY_DIR/epsilon" ]; then
        rm "$BINARY_DIR/epsilon"
        success "Removed binary: $BINARY_DIR/epsilon"
        removed_something=true
    fi

    # Remove installation directory
    if [ -d "$INSTALL_DIR" ]; then
        rm -rf "$INSTALL_DIR"
        success "Removed installation: $INSTALL_DIR"
        removed_something=true
    fi

    # Remove completions
    if [ -d "$COMPLETIONS_DIR" ]; then
        rm -rf "$COMPLETIONS_DIR"
        success "Removed completions: $COMPLETIONS_DIR"
        removed_something=true
    fi

    if [ "$removed_something" = true ]; then
        echo ""
        success "Epsilon has been uninstalled"
        echo ""
        echo "Note: You may want to remove PATH entries from your shell config"
    else
        warning "Nothing to uninstall - epsilon was not found"
    fi
}

# Show usage information after installation
show_usage() {
    echo ""
    echo "========================================="
    echo "   Epsilon Installation Complete!"
    echo "========================================="
    echo ""
    success "Installation directory: $INSTALL_DIR"
    success "Binary location: $BINARY_DIR/epsilon"
    echo ""
    echo "Quick start:"
    echo "  epsilon                           # Start REPL"
    echo "  epsilon --eval '(+ 1 2)'          # Evaluate expression"
    echo "  epsilon --modules                 # List available modules"
    echo "  epsilon --help                    # Show all options"
    echo ""
    echo "Update epsilon:"
    echo "  epsilon update                    # Update to latest version"
    echo "  epsilon update --check            # Check for updates"
    echo ""
    echo "Documentation: https://github.com/$GITHUB_REPO"
}

# Main installation flow
main() {
    parse_args "$@"

    # Handle --help
    if [ "$SHOW_HELP" = true ]; then
        show_help
        exit 0
    fi

    # Handle --check
    if [ "$CHECK_ONLY" = true ]; then
        check_installation
        exit $?
    fi

    # Handle --uninstall
    if [ "$UNINSTALL" = true ]; then
        uninstall_epsilon
        exit 0
    fi

    # Handle --completions only
    if [ "$COMPLETIONS_ONLY" = true ]; then
        if [ ! -d "$INSTALL_DIR" ]; then
            error "Epsilon not installed. Run install.sh first."
            exit 1
        fi
        install_completions
        exit 0
    fi

    # Full installation
    echo "========================================="
    echo "   Epsilon Installation"
    echo "========================================="
    echo ""

    # Check requirements
    check_requirements

    # Detect platform
    local platform_arch
    platform_arch=$(detect_platform)
    success "Detected platform: $platform_arch"

    # Get release info
    local release_info
    release_info=$(get_release_info "$INSTALL_VERSION")

    # Get download URL
    local download_url
    download_url=$(get_download_url "$release_info" "$platform_arch")
    success "Found release: $(basename "$download_url")"

    # Download and install
    download_and_extract "$download_url"

    # Create symlinks
    create_symlinks

    # Install completions
    install_completions

    # Update PATH if needed
    update_shell_config

    # Verify
    verify_installation

    # Show usage
    show_usage
}

# Handle errors
trap 'error "Installation failed on line $LINENO"' ERR

# Run main function
main "$@"
