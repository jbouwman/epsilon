#!/usr/bin/env pwsh
# Release script for epsilon on Windows

param(
    [Parameter(Position=0)]
    [string]$Version,
    [switch]$DryRun,
    [switch]$Help
)

if ($Help -or -not $Version) {
    Write-Host "Usage: .\release.ps1 <version> [--dry-run]"
    Write-Host "Creates and tags a release for the specified version"
    Write-Host ""
    Write-Host "Arguments:"
    Write-Host "  version    Version to release (e.g., 0.11.0)"
    Write-Host ""
    Write-Host "Options:"
    Write-Host "  --dry-run  Preview release process without making changes"
    exit 0
}

# Set strict mode and error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($DryRun) {
    Write-Host "=== DRY RUN: Creating release $Version ===" -ForegroundColor Yellow
} else {
    Write-Host "=== Creating release $Version ===" -ForegroundColor Cyan
}
Write-Host ""

try {
    # Run tests first
    Write-Host "Running tests before release..." -ForegroundColor Yellow
    & ".\scripts\test.ps1"
    
    if ($LASTEXITCODE -ne 0) {
        Write-Host "✗ Tests failed - aborting release" -ForegroundColor Red
        exit 1
    }
    
    Write-Host "✓ Tests passed" -ForegroundColor Green
    Write-Host ""
    
    if ($DryRun) {
        Write-Host "[DRY RUN] Would create release package for version $Version" -ForegroundColor Yellow
        Write-Host "[DRY RUN] Would tag git repository with $Version" -ForegroundColor Yellow
        Write-Host "[DRY RUN] Would push tags to remote" -ForegroundColor Yellow
    } else {
        # Create release package
        Write-Host "Creating release package..." -ForegroundColor Yellow
        & ".\epsilon.ps1" --exec "epsilon.release:create-package" --eval ":version `"$Version`""
        
        if ($LASTEXITCODE -ne 0) {
            Write-Host "✗ Failed to create release package" -ForegroundColor Red
            exit 1
        }
        
        Write-Host "✓ Release package created" -ForegroundColor Green
        
        # Tag release
        Write-Host "Tagging release..." -ForegroundColor Yellow
        git tag -a $Version -m "Release $Version"
        
        Write-Host "✓ Tagged release $Version" -ForegroundColor Green
        
        # Push tags
        Write-Host "Pushing tags to remote..." -ForegroundColor Yellow
        git push origin --tags
        
        Write-Host "✓ Tags pushed to remote" -ForegroundColor Green
    }
    
    Write-Host ""
    Write-Host "=== Release $Version completed successfully ===" -ForegroundColor Green
    
} catch {
    Write-Host ""
    Write-Host "=== Release failed with error: $_" -ForegroundColor Red
    exit 1
}