# Minimal CI Setup Script for Windows
param([switch]$Help)

if ($Help) {
    Write-Host "Usage: .\ci-setup.ps1"
    Write-Host "Sets up CI environment for Windows builds"
    exit 0
}

$ErrorActionPreference = "Stop"
$OS = "Windows"
$ARCH = $env:PROCESSOR_ARCHITECTURE

Write-Host "CI Setup for $OS $ARCH" -ForegroundColor Cyan

# Check if we're in a GitHub Actions environment
if ($env:GITHUB_WORKSPACE) {
    Write-Host "Running in GitHub Actions environment" -ForegroundColor Blue
    git config --global --add safe.directory $env:GITHUB_WORKSPACE
    Write-Host "✓ Git configuration completed" -ForegroundColor Green
}

# Check for required tools
Write-Host "Checking for required tools..." -ForegroundColor Yellow

# Check for SBCL
try {
    $sbclVersion = & sbcl --version 2>&1 | Out-String
    Write-Host "✓ SBCL found: $($sbclVersion.Trim())" -ForegroundColor Green
} catch {
    Write-Host "✗ SBCL not found" -ForegroundColor Red
    exit 1
}

# Check for Git
try {
    $gitVersion = & git --version 2>&1 | Out-String
    Write-Host "✓ Git found: $($gitVersion.Trim())" -ForegroundColor Green
} catch {
    Write-Host "✗ Git not found" -ForegroundColor Red
    exit 1
}

Write-Host "✓ PowerShell $($PSVersionTable.PSVersion) found" -ForegroundColor Green
Write-Host "=== Windows CI setup completed successfully ===" -ForegroundColor Green