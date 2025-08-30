#!/usr/bin/env pwsh
# CLI smoke tests for epsilon on Windows

param(
    [switch]$Help
)

if ($Help) {
    Write-Host "Usage: .\smoke.ps1"
    Write-Host "Runs CLI smoke tests to verify basic functionality"
    exit 0
}

# Set strict mode and error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "=== Running CLI smoke tests ===" -ForegroundColor Cyan
Write-Host ""

try {
    # Run the built-in smoke tests via epsilon
    & ".\epsilon.ps1" --exec "epsilon.release:run-smoke-tests"
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host ""
        Write-Host "=== CLI smoke tests completed successfully ===" -ForegroundColor Green
        exit 0
    } else {
        Write-Host ""
        Write-Host "=== CLI smoke tests failed ===" -ForegroundColor Red
        exit $LASTEXITCODE
    }
} catch {
    Write-Host ""
    Write-Host "=== CLI smoke tests failed with error: $_" -ForegroundColor Red
    exit 1
}