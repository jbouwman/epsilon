#!/usr/bin/env pwsh
#
# Epsilon command-line interface for Windows PowerShell
#
# Works from fresh checkout or system installation.
# Requires SBCL to be installed and available in PATH.
#

param(
    [switch]$DebugMode,
    [switch]$Rebuild, 
    [switch]$Quiet,
    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$SbclArgs
)

# Set strict mode for better error handling
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# The user's working directory
$env:EPSILON_USER = Get-Location
# The directory where epsilon is located
$env:EPSILON_HOME = Split-Path -Parent $MyInvocation.MyCommand.Path
$EPSILON_BOOT = Join-Path $env:EPSILON_HOME "modules\core\src\epsilon.lisp"

# Detect boot script
if (-not (Test-Path $EPSILON_BOOT)) {
    Write-Error "Error: epsilon boot script not found in $env:EPSILON_HOME"
    exit 1
}

# Check if SBCL is available
try {
    $null = Get-Command sbcl -ErrorAction Stop
} catch {
    Write-Error "Error: SBCL not found in PATH"
    Write-Error "Please install SBCL: https://www.sbcl.org/"
    exit 1
}

# Force rebuild of bootstrap.fasl if requested
if ($Rebuild) {
    Write-Host "Forcing rebuild of bootstrap cache..."
    $bootstrapFile = Join-Path $env:EPSILON_HOME "modules\core\target\bootstrap.fasl"
    if (Test-Path $bootstrapFile) {
        Remove-Item $bootstrapFile
    }
}

# Set SBCL arguments
$SbclRuntimeArgs = @("--noinform")

if ($DebugMode) {
    $SbclToplevelArgs = @("--no-sysinit", "--no-userinit", "--quit")
} else {
    $SbclToplevelArgs = @("--no-sysinit", "--no-userinit", "--disable-debugger", "--quit")
}

# Change to epsilon directory
Set-Location $env:EPSILON_HOME

# Pass --quiet flag back to the Lisp side if quiet mode is enabled
if ($Quiet) {
    if ($SbclArgs.Count -eq 0) {
        $SbclArgs = @("--quiet")
    } else {
        $SbclArgs = @("--quiet") + $SbclArgs
    }
}

# Combine all arguments - include the CLI entry point and separator
if ($SbclArgs.Count -eq 0) {
    $AllArgs = $SbclRuntimeArgs + $SbclToplevelArgs + @("--load", $EPSILON_BOOT, "--eval", "(epsilon.main:cli-run)", "--")
} else {
    $AllArgs = $SbclRuntimeArgs + $SbclToplevelArgs + @("--load", $EPSILON_BOOT, "--eval", "(epsilon.main:cli-run)", "--") + $SbclArgs
}

# Run SBCL with epsilon
try {
    & sbcl @AllArgs
    exit $LASTEXITCODE
} catch {
    Write-Error "Failed to execute SBCL: $_"
    exit 1
}