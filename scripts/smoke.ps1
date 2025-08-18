# CLI smoke test suite for epsilon
# PowerShell version for Windows

Set-StrictMode -Version Latest
$ErrorActionPreference = "Continue"

# Track test results
$PASSED = 0
$FAILED = 0
$SKIPPED = 0

# Test output directory
$TEST_DIR = Join-Path $env:TEMP "epsilon-cli-test-$(Get-Random)"
New-Item -ItemType Directory -Path $TEST_DIR -Force | Out-Null

# Clean up on exit
trap {
    if (Test-Path $TEST_DIR) {
        Remove-Item -Path $TEST_DIR -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# Get epsilon path
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$EpsilonPath = Split-Path -Parent $ScriptDir
$Epsilon = Join-Path $EpsilonPath "epsilon.ps1"

# Check if we should use epsilon.ps1 or epsilon
if (-not (Test-Path $Epsilon)) {
    $Epsilon = Join-Path $EpsilonPath "epsilon"
}

# Test function
function Run-Test {
    param(
        [string]$Name,
        [string]$Command,
        [int]$ExpectedExit = 0,
        [string]$GrepPattern = ""
    )
    
    Write-Host -NoNewline "Testing $Name... "
    
    # Run command and capture output
    $outputFile = Join-Path $TEST_DIR "output.txt"
    $errorFile = Join-Path $TEST_DIR "error.txt"
    
    # Execute the command
    $process = Start-Process -FilePath "powershell" -ArgumentList "-Command", $Command -Wait -PassThru -RedirectStandardOutput $outputFile -RedirectStandardError $errorFile -WorkingDirectory $EpsilonPath
    $exitCode = $process.ExitCode
    
    # Read output
    $output = Get-Content $outputFile -Raw -ErrorAction SilentlyContinue
    
    # Check exit code
    if ($exitCode -ne $ExpectedExit) {
        Write-Host "FAILED (exit code: $exitCode, expected: $ExpectedExit)" -ForegroundColor Red
        Write-Host "  Command: $Command"
        Write-Host "  Output:"
        if ($output) {
            $output -split "`n" | ForEach-Object { Write-Host "    $_" }
        }
        $script:FAILED++
        return $false
    }
    
    # Check output pattern if provided
    if ($GrepPattern -and $output) {
        if ($output -notmatch $GrepPattern) {
            Write-Host "FAILED (pattern not found: $GrepPattern)" -ForegroundColor Red
            Write-Host "  Command: $Command"
            Write-Host "  Output:"
            $output -split "`n" | ForEach-Object { Write-Host "    $_" }
            $script:FAILED++
            return $false
        }
    }
    
    Write-Host "PASSED" -ForegroundColor Green
    $script:PASSED++
    return $true
}

# Test cases
Write-Host "=== Epsilon CLI Smoke Tests ===" -ForegroundColor Cyan
Write-Host

# Basic flags
Run-Test "help flag" "& '$Epsilon' --help" 0 "Epsilon development environment"
Run-Test "version flag" "& '$Epsilon' --version" 0 "EPSILON"

# Module operations
Run-Test "list modules" "& '$Epsilon' --modules" 0 "epsilon.core"
Run-Test "list modules (finds epsilon.test)" "& '$Epsilon' --modules" 0 "epsilon.test"

# Build operations
Run-Test "load epsilon.json" "& '$Epsilon' --module epsilon.json --eval 'success'" 0 "SUCCESS"
Run-Test "load invalid module" "& '$Epsilon' --module nonexistent.module" 1 "Unknown module"

# Test operations
Write-Host
Write-Host "=== Testing --test flag ===" -ForegroundColor Cyan
Run-Test "test epsilon.json" "& '$Epsilon' --test epsilon.json" 0 "test"
Run-Test "test invalid module" "& '$Epsilon' --test nonexistent.module" 1 "Unknown"

# Eval operations
Run-Test "eval simple expression" "& '$Epsilon' --eval '(+ 1 2)'" 0 "3"
Run-Test "eval with module load" "& '$Epsilon' --module epsilon.core --eval '(epsilon.map:make-map)'" 0

# Path operations
$TEST_PKG_DIR = Join-Path $TEST_DIR "test-module"
New-Item -ItemType Directory -Path $TEST_PKG_DIR -Force | Out-Null
New-Item -ItemType Directory -Path (Join-Path $TEST_PKG_DIR "src") -Force | Out-Null

@"
(:name "test.module"
 :version "1.0.0"
 :author "Test"
 :description "Test module"
 :sources ("src")
 :dependencies ())
"@ | Out-File -FilePath (Join-Path $TEST_PKG_DIR "module.lisp") -Encoding UTF8

"(defpackage test.module)" | Out-File -FilePath (Join-Path $TEST_PKG_DIR "src\main.lisp") -Encoding UTF8

Run-Test "path to module" "& '$Epsilon' --path '$TEST_PKG_DIR' --modules" 0 "test.module"
Run-Test "path without module.lisp" "& '$Epsilon' --path '$env:TEMP' --modules" 0 "Found \d+ modules"

# Combined operations
Run-Test "module and eval" "& '$Epsilon' --module epsilon.core --eval '(format t `"loaded`")'" 0 "loaded"

# Debug and logging
Run-Test "debug flag" "& '$Epsilon' --debug --eval '(+ 1 1)'" 0 "2"
Run-Test "log flag" "& '$Epsilon' --log debug --eval '(+ 1 1)'" 0 "2"

# Summary
Write-Host
Write-Host "=== Test Summary ===" -ForegroundColor Cyan
Write-Host "Passed: $PASSED" -ForegroundColor Green
Write-Host "Failed: $FAILED" -ForegroundColor $(if ($FAILED -eq 0) { "Green" } else { "Red" })
Write-Host "Skipped: $SKIPPED" -ForegroundColor Yellow
Write-Host "Total: $($PASSED + $FAILED + $SKIPPED)"

if ($FAILED -eq 0) {
    exit 0
} else {
    exit 1
}