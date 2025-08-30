#!/usr/bin/env pwsh
# Install script for epsilon on Windows

param(
    [string]$Version = "latest",
    [string]$InstallDir = "$env:LOCALAPPDATA\epsilon",
    [switch]$Help
)

if ($Help) {
    Write-Host "Usage: .\install.ps1 [-Version <version>] [-InstallDir <path>]"
    Write-Host "Installs epsilon from GitHub releases"
    Write-Host ""
    Write-Host "Options:"
    Write-Host "  -Version     Version to install (default: latest)"
    Write-Host "  -InstallDir  Installation directory (default: $env:LOCALAPPDATA\epsilon)"
    exit 0
}

# Set strict mode and error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "=== Installing Epsilon $Version ===" -ForegroundColor Cyan
Write-Host "Installation directory: $InstallDir" -ForegroundColor Blue
Write-Host ""

try {
    # Create installation directory
    if (-not (Test-Path $InstallDir)) {
        Write-Host "Creating installation directory..." -ForegroundColor Yellow
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
        Write-Host "✓ Installation directory created" -ForegroundColor Green
    }
    
    # Determine download URL
    $repo = "epsilon-lang/epsilon"  # Adjust this to the actual repo
    
    if ($Version -eq "latest") {
        Write-Host "Fetching latest release information..." -ForegroundColor Yellow
        $releaseUrl = "https://api.github.com/repos/$repo/releases/latest"
    } else {
        $releaseUrl = "https://api.github.com/repos/$repo/releases/tags/$Version"
    }
    
    # For now, provide instructions since we don't have actual releases yet
    Write-Host "NOTE: Automated installation from releases not yet implemented." -ForegroundColor Yellow
    Write-Host ""
    Write-Host "To install epsilon manually:" -ForegroundColor Cyan
    Write-Host "1. Clone the repository: git clone https://github.com/your-repo/epsilon.git" -ForegroundColor White
    Write-Host "2. Ensure SBCL is installed and in PATH" -ForegroundColor White
    Write-Host "3. Run: .\epsilon.ps1 --version to test" -ForegroundColor White
    Write-Host ""
    Write-Host "Or copy the epsilon directory to: $InstallDir" -ForegroundColor White
    Write-Host "And add $InstallDir to your PATH environment variable" -ForegroundColor White
    
    Write-Host ""
    Write-Host "=== Installation instructions provided ===" -ForegroundColor Green
    
} catch {
    Write-Host ""
    Write-Host "=== Installation failed with error: $_" -ForegroundColor Red
    exit 1
}