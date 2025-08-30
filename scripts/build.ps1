#!/usr/bin/env pwsh
# Build script for epsilon on Windows

param(
    [string]$Platform = "windows",
    [switch]$Help
)

if ($Help) {
    Write-Host "Usage: .\build.ps1 [-Platform <platform>]"
    Write-Host "Builds epsilon modules for the specified platform (default: windows)"
    exit 0
}

# Set strict mode and error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "=== Building epsilon for platform: $Platform ===" -ForegroundColor Cyan
Write-Host ""

try {
    # Build all modules
    Write-Host "Building all modules..." -ForegroundColor Yellow
    & ".\epsilon.ps1" --exec "epsilon.release:build"
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Build completed successfully" -ForegroundColor Green
    } else {
        Write-Host "✗ Build failed" -ForegroundColor Red
        exit $LASTEXITCODE
    }
    
    Write-Host ""
    Write-Host "=== Build completed ===" -ForegroundColor Green
    
} catch {
    Write-Host ""
    Write-Host "=== Build failed with error: $_" -ForegroundColor Red
    exit 1
}