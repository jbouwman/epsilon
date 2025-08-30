#!/usr/bin/env pwsh
# CI test runner for epsilon - runs all tests for release validation

param(
    [switch]$Help
)

if ($Help) {
    Write-Host "Usage: .\test.ps1"
    Write-Host "Runs comprehensive test suite including smoke tests and module self-tests"
    exit 0
}

# Set strict mode and error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Continue"

Write-Host "=== Running all tests before building release ===" -ForegroundColor Cyan
Write-Host ""

# Track overall status
$ExitCode = 0

# 1. Run smoke tests
Write-Host "Running CLI smoke tests..." -ForegroundColor Yellow
try {
    & ".\scripts\smoke.ps1"
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ CLI smoke tests passed" -ForegroundColor Green
    } else {
        Write-Host "✗ CLI smoke tests failed" -ForegroundColor Red
        $ExitCode = 1
    }
} catch {
    Write-Host "✗ CLI smoke tests failed with error: $_" -ForegroundColor Red
    $ExitCode = 1
}

Write-Host ""

# 2. Run self-test for all modules
Write-Host "Running module self-tests..." -ForegroundColor Yellow
try {
    & ".\epsilon.ps1" --exec "epsilon.release:selftest"
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Module self-tests passed" -ForegroundColor Green
    } else {
        Write-Host "✗ Module self-tests failed" -ForegroundColor Red
        $ExitCode = 1
    }
} catch {
    Write-Host "✗ Module self-tests failed with error: $_" -ForegroundColor Red
    $ExitCode = 1
}

Write-Host ""

# 3. Verify epsilon executable works
Write-Host "Verifying epsilon executable..." -ForegroundColor Yellow
try {
    $null = & ".\epsilon.ps1" --version 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Epsilon executable verified" -ForegroundColor Green
    } else {
        Write-Host "✗ Epsilon executable verification failed" -ForegroundColor Red
        $ExitCode = 1
    }
} catch {
    Write-Host "✗ Epsilon executable verification failed with error: $_" -ForegroundColor Red
    $ExitCode = 1
}

Write-Host ""

# Summary
if ($ExitCode -eq 0) {
    Write-Host "=== All tests passed ===" -ForegroundColor Green
} else {
    Write-Host "=== Some tests failed ===" -ForegroundColor Red
}

exit $ExitCode