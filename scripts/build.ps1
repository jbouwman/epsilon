# Epsilon Documentation Build Script for Windows PowerShell
# Builds static documentation site

param(
    [switch]$Serve,
    [switch]$Deploy
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "EPSILON DOCS BUILD SYSTEM" -ForegroundColor Cyan
Write-Host "Building functional programming documentation..." -ForegroundColor Yellow
Write-Host

# Check if mkdocs is installed
$mkdocs = Get-Command mkdocs -ErrorAction SilentlyContinue
if (-not $mkdocs) {
    Write-Host "ERROR: MkDocs not found. Installing..." -ForegroundColor Red
    
    # Check if pip is available
    $pip = Get-Command pip -ErrorAction SilentlyContinue
    if (-not $pip) {
        Write-Host "ERROR: pip not found. Please install Python and pip first." -ForegroundColor Red
        exit 1
    }
    
    pip install -r requirements.txt
}

# Check if in correct directory
if (-not (Test-Path "mkdocs.yml")) {
    Write-Host "ERROR: mkdocs.yml not found. Run from project root directory." -ForegroundColor Red
    exit 1
}

# Clean previous build
Write-Host "Cleaning previous build..." -ForegroundColor Yellow
if (Test-Path "site") {
    Remove-Item -Path "site" -Recurse -Force
}

# Build documentation
Write-Host "Building documentation site..." -ForegroundColor Yellow
mkdocs build --strict --verbose

# Report build results
if (Test-Path "site") {
    $size = (Get-ChildItem -Path "site" -Recurse | Measure-Object -Property Length -Sum).Sum / 1MB
    $files = (Get-ChildItem -Path "site" -Recurse -File).Count
    
    Write-Host
    Write-Host "Build completed successfully!" -ForegroundColor Green
    Write-Host "   Output: site\ directory" -ForegroundColor Cyan
    Write-Host "   Size: $([Math]::Round($size, 2)) MB" -ForegroundColor Cyan
    Write-Host "   Files: $files" -ForegroundColor Cyan
    Write-Host
    
    if ($Serve) {
        Write-Host "Starting local server..." -ForegroundColor Yellow
        mkdocs serve
    } elseif ($Deploy) {
        Write-Host "Deploying to GitHub Pages..." -ForegroundColor Yellow
        mkdocs gh-deploy
    } else {
        Write-Host "To serve locally: .\scripts\build.ps1 -Serve" -ForegroundColor Yellow
        Write-Host "To deploy: .\scripts\build.ps1 -Deploy" -ForegroundColor Yellow
    }
} else {
    Write-Host "ERROR: Build failed!" -ForegroundColor Red
    exit 1
}