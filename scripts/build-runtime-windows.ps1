#
# Build script for creating Epsilon runtime distribution on Windows
#
# This script builds a complete Epsilon runtime package that includes:
# - SBCL core image with Epsilon preloaded
# - Wrapper script for running Epsilon
# - Windows-specific packaging
#

$ErrorActionPreference = "Stop"

# Configuration
$EPSILON_DIR = Split-Path -Parent $PSScriptRoot
$TARGET_DIR = Join-Path $EPSILON_DIR "target"
$DIST_DIR = Join-Path $TARGET_DIR "dist"
$PLATFORM_NAME = "windows"
$ARCH = if ([Environment]::Is64BitOperatingSystem) { "x86_64" } else { "x86" }

Write-Host "Building Epsilon runtime for $PLATFORM_NAME-$ARCH..."
Write-Host "Working directory: $(Get-Location)"

# Find SBCL executable
$SBCL_PATH = Get-Command sbcl -ErrorAction SilentlyContinue
if (-not $SBCL_PATH) {
    Write-Error "SBCL not found in PATH. Please install SBCL first."
    exit 1
}

Write-Host "SBCL executable: $($SBCL_PATH.Source)"

# Create distribution directory
New-Item -ItemType Directory -Force -Path $DIST_DIR | Out-Null

# Build core image
Write-Host "Building SBCL core image with Epsilon..."
Set-Location $EPSILON_DIR

# Create target directory first
New-Item -ItemType Directory -Force -Path $TARGET_DIR | Out-Null

Write-Host "Building SBCL core image..."
Write-Host "Current directory: $(Get-Location)"
Write-Host "Target directory: $TARGET_DIR"

# Build the core image
& sbcl --noinform `
       --non-interactive `
       --no-sysinit `
       --no-userinit `
       --load "scripts/boot.lisp" `
       --eval "(epsilon.tool.boot:boot)" `
       --eval "(sb-ext:save-lisp-and-die `"target/epsilon-core`" :executable nil :save-runtime-options t :compression t)"

if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to build core image"
    exit 1
}

# Create standalone SBCL runtime
Write-Host "Creating standalone SBCL runtime..."

# Find SBCL installation directory
$SBCL_BIN_DIR = Split-Path -Parent $SBCL_PATH.Source
$SBCL_ROOT = Split-Path -Parent $SBCL_BIN_DIR

Write-Host "SBCL installation root: $SBCL_ROOT"

# Copy SBCL runtime components
Copy-Item $SBCL_PATH.Source (Join-Path $DIST_DIR "sbcl.exe")

# Copy SBCL core and runtime files
$SBCL_CORE_PATH = Join-Path $SBCL_ROOT "lib\sbcl\sbcl.core"
if (Test-Path $SBCL_CORE_PATH) {
    $DIST_LIB_DIR = Join-Path $DIST_DIR "lib\sbcl"
    New-Item -ItemType Directory -Force -Path $DIST_LIB_DIR | Out-Null
    Copy-Item $SBCL_CORE_PATH $DIST_LIB_DIR
    
    $SBCL_RC_PATH = Join-Path $SBCL_ROOT "lib\sbcl\sbclrc"
    if (Test-Path $SBCL_RC_PATH) {
        Copy-Item $SBCL_RC_PATH $DIST_LIB_DIR
    }
}

# Create epsilon wrapper scripts
Write-Host "Creating wrapper scripts..."

# PowerShell wrapper
$PS_WRAPPER = @'
#
# Epsilon runtime wrapper (PowerShell)
#
# This script provides a convenient way to run Epsilon with the preloaded core
#

param([Parameter(ValueFromRemainingArguments=$true)]$Args)

$EPSILON_HOME = Split-Path -Parent $MyInvocation.MyCommand.Path
$CORE_IMAGE = Join-Path $EPSILON_HOME "epsilon-core"

# Check if core image exists
if (-not (Test-Path $CORE_IMAGE)) {
    Write-Error "Error: Epsilon core image not found at $CORE_IMAGE"
    exit 1
}

# Set SBCL_HOME to use embedded runtime
$env:SBCL_HOME = Join-Path $EPSILON_HOME "lib\sbcl"

# Run embedded SBCL with Epsilon core
$SBCL_EXE = Join-Path $EPSILON_HOME "sbcl.exe"
& $SBCL_EXE --core $CORE_IMAGE @Args
'@

$PS_WRAPPER | Out-File -FilePath (Join-Path $DIST_DIR "epsilon.ps1") -Encoding UTF8

# Batch file wrapper for compatibility
$BATCH_WRAPPER = @'
@echo off
rem Epsilon runtime wrapper for Windows
rem
rem This script provides a convenient way to run Epsilon with the preloaded core

set EPSILON_HOME=%~dp0
set CORE_IMAGE=%EPSILON_HOME%epsilon-core
set SBCL_HOME=%EPSILON_HOME%lib\sbcl

if not exist "%CORE_IMAGE%" (
    echo Error: Epsilon core image not found at %CORE_IMAGE%
    exit /b 1
)

rem Run embedded SBCL with Epsilon core
"%EPSILON_HOME%sbcl.exe" --core "%CORE_IMAGE%" %*
'@

$BATCH_WRAPPER | Out-File -FilePath (Join-Path $DIST_DIR "epsilon.bat") -Encoding ASCII

# Executable wrapper (for CI compatibility)
$EXE_WRAPPER = @'
#!/bin/bash
# Unix-style wrapper for Windows build in CI environments

EPSILON_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_IMAGE="$EPSILON_HOME/epsilon-core"

if [ ! -f "$CORE_IMAGE" ]; then
    echo "Error: Epsilon core image not found at $CORE_IMAGE"
    exit 1
fi

# Set SBCL_HOME to use embedded runtime
export SBCL_HOME="$EPSILON_HOME/lib/sbcl"

exec "$EPSILON_HOME/sbcl.exe" --core "$CORE_IMAGE" "$@"
'@

$EXE_WRAPPER | Out-File -FilePath (Join-Path $DIST_DIR "epsilon.exe") -Encoding UTF8 -NoNewline

# Copy core image and source code to distribution
Write-Host "Checking for core image at: $TARGET_DIR\epsilon-core"
$CORE_IMAGE_PATH = Join-Path $TARGET_DIR "epsilon-core"
if (Test-Path $CORE_IMAGE_PATH) {
    Write-Host "Core image found, copying to distribution..."
    Copy-Item $CORE_IMAGE_PATH $DIST_DIR
    Remove-Item $CORE_IMAGE_PATH
} else {
    Write-Error "ERROR: Core image not found at $CORE_IMAGE_PATH"
    Write-Host "Listing target directory contents:"
    Get-ChildItem $TARGET_DIR -ErrorAction SilentlyContinue
    exit 1
}

# Include source code for navigation
Write-Host "Including source code for navigation..."
$MODULE_SRC = Join-Path $EPSILON_DIR "module"
$MODULE_DEST = Join-Path $DIST_DIR "module"
Copy-Item $MODULE_SRC $MODULE_DEST -Recurse

# Copy essential project files
$FILES_TO_COPY = @("CLAUDE.md", "README.md", "run.sh", "run.ps1")
foreach ($file in $FILES_TO_COPY) {
    $src = Join-Path $EPSILON_DIR $file
    if (Test-Path $src) {
        Copy-Item $src $DIST_DIR
    }
}

# Verify distribution contents
Write-Host "Distribution directory contents:"
Get-ChildItem $DIST_DIR

# Create package archive
$PACKAGE_NAME = "epsilon-$PLATFORM_NAME-$ARCH"
$PACKAGE_FILE = Join-Path $TARGET_DIR "$PACKAGE_NAME.zip"
Write-Host "Creating package: $PACKAGE_FILE"

# Use PowerShell's built-in compression
Compress-Archive -Path "$DIST_DIR\*" -DestinationPath $PACKAGE_FILE -Force

Write-Host "Epsilon runtime package created: $PACKAGE_FILE"
Write-Host "Package contents:"
$archive = [System.IO.Compression.ZipFile]::OpenRead($PACKAGE_FILE)
$archive.Entries | ForEach-Object { Write-Host "  $($_.FullName)" }
$archive.Dispose()