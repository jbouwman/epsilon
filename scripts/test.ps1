# CI test runner for epsilon - runs all tests for release validation
# PowerShell version for Windows

param(
    [switch]$Verbose
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Continue"

Write-Host "=== Running all tests before building release ===" -ForegroundColor Cyan
Write-Host ""

# Track overall status
$EXIT_CODE = 0

# Get script directory and epsilon path
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$EpsilonPath = Split-Path -Parent $ScriptDir
$EpsilonExe = Join-Path $EpsilonPath "epsilon.ps1"

# Check if we should use epsilon.ps1 or epsilon
if (-not (Test-Path $EpsilonExe)) {
    $EpsilonExe = Join-Path $EpsilonPath "epsilon"
}

# 1. Run smoke tests
Write-Host "Running CLI smoke tests..." -ForegroundColor Yellow
$smokeScript = Join-Path $ScriptDir "smoke.ps1"
if (Test-Path $smokeScript) {
    & $smokeScript
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ CLI smoke tests passed" -ForegroundColor Green
    } else {
        Write-Host "✗ CLI smoke tests failed" -ForegroundColor Red
        $EXIT_CODE = 1
    }
} else {
    # Fallback to bash script if PowerShell version doesn't exist
    $smokeBash = Join-Path $ScriptDir "smoke.sh"
    if (Test-Path $smokeBash) {
        & bash $smokeBash
        if ($LASTEXITCODE -eq 0) {
            Write-Host "✓ CLI smoke tests passed" -ForegroundColor Green
        } else {
            Write-Host "✗ CLI smoke tests failed" -ForegroundColor Red
            $EXIT_CODE = 1
        }
    }
}

Write-Host ""

# 2. Run self-test for all modules
Write-Host "Running module self-tests..." -ForegroundColor Yellow
if ($Verbose) {
    & $EpsilonExe --exec epsilon.release:selftest
} else {
    & $EpsilonExe --exec epsilon.release:selftest --format none
}

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Module self-tests passed" -ForegroundColor Green
} else {
    Write-Host "✗ Module self-tests failed" -ForegroundColor Red
    $EXIT_CODE = 1
}

Write-Host ""

# 3. Verify epsilon executable works
Write-Host "Verifying epsilon executable..." -ForegroundColor Yellow
& $EpsilonExe --version | Out-Null
if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Epsilon executable verified" -ForegroundColor Green
} else {
    Write-Host "✗ Epsilon executable verification failed" -ForegroundColor Red
    $EXIT_CODE = 1
}

Write-Host ""

# Summary
if ($EXIT_CODE -eq 0) {
    Write-Host "=== All tests passed ===" -ForegroundColor Green
} else {
    Write-Host "=== Some tests failed ===" -ForegroundColor Red
}

exit $EXIT_CODE