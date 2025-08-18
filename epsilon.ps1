# Epsilon command-line interface for Windows PowerShell
#
# Works from fresh checkout or system installation.
# Requires SBCL to be installed and available in PATH.
#

param(
    [Parameter(Position=0, ValueFromRemainingArguments=$true)]
    [string[]]$Arguments
)

# Enable strict mode
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# The user's working directory
$env:EPSILON_USER = $PWD.Path

# The directory where epsilon is located
$EPSILON_HOME = Split-Path -Parent $MyInvocation.MyCommand.Path
$env:EPSILON_HOME = $EPSILON_HOME
$EPSILON_BOOT = Join-Path $EPSILON_HOME "modules\core\src\epsilon.lisp"

# Detect boot script
if (-not (Test-Path $EPSILON_BOOT)) {
    Write-Error "Error: epsilon boot script not found in $EPSILON_HOME"
    exit 1
}

# Check if SBCL is available
$sbclPath = Get-Command sbcl -ErrorAction SilentlyContinue
if (-not $sbclPath) {
    Write-Error "Error: SBCL not found in PATH"
    Write-Host "Please install SBCL: https://www.sbcl.org/" -ForegroundColor Yellow
    exit 1
}

# Parse debug and rebuild flags
$DEBUG_MODE = $false
$REBUILD_BOOT = $false
$remainingArgs = @()
$skipNext = $false

for ($i = 0; $i -lt $Arguments.Count; $i++) {
    if ($skipNext) {
        $skipNext = $false
        continue
    }
    
    switch ($Arguments[$i]) {
        "--debug" {
            $DEBUG_MODE = $true
        }
        "--rebuild" {
            $REBUILD_BOOT = $true
        }
        default {
            # Collect this and all remaining arguments
            $remainingArgs += $Arguments[$i]
            # Special handling for arguments that take values
            if ($Arguments[$i] -in @("--eval", "--module", "--test", "--exec", "--path", "--log")) {
                if (($i + 1) -lt $Arguments.Count) {
                    $remainingArgs += $Arguments[$i + 1]
                    $skipNext = $true
                }
            }
        }
    }
}

# Force rebuild of bootstrap.fasl if requested
if ($REBUILD_BOOT) {
    Write-Host "Forcing rebuild of bootstrap cache..." -ForegroundColor Yellow
    $bootstrapPath = Join-Path $EPSILON_HOME "modules\core\target\bootstrap.fasl"
    if (Test-Path $bootstrapPath) {
        Remove-Item $bootstrapPath -Force
    }
}

# Build SBCL arguments
$sbclArgs = @("--noinform")

if ($DEBUG_MODE) {
    $sbclArgs += @("--no-sysinit", "--no-userinit", "--quit")
} else {
    $sbclArgs += @("--no-sysinit", "--no-userinit", "--disable-debugger", "--quit")
}

$sbclArgs += @(
    "--load", $EPSILON_BOOT,
    "--eval", "(epsilon.main:cli-run)"
)

# Add separator and user arguments if any
if ($remainingArgs.Count -gt 0) {
    $sbclArgs += "--"
    $sbclArgs += $remainingArgs
} else {
    $sbclArgs += "--"
}

# Change to epsilon home directory
Push-Location $EPSILON_HOME
try {
    # Execute SBCL with arguments
    & sbcl $sbclArgs
    $exitCode = $LASTEXITCODE
} finally {
    Pop-Location
}

exit $exitCode