# Automated release script for Epsilon
# PowerShell version for Windows
# Handles version validation, tagging, and release workflow

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$Version,
    
    [string]$Branch = "",
    [switch]$Force,
    [switch]$DryRun,
    [switch]$Help
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# Colors for output
function Write-ErrorMsg { param($msg) Write-Host "✗ $msg" -ForegroundColor Red }
function Write-SuccessMsg { param($msg) Write-Host "✓ $msg" -ForegroundColor Green }
function Write-WarningMsg { param($msg) Write-Host "⚠ $msg" -ForegroundColor Yellow }
function Write-InfoMsg { param($msg) Write-Host "ℹ $msg" -ForegroundColor Blue }

# Show usage
function Show-Usage {
    Write-Host @"
Usage: .\release.ps1 [OPTIONS] VERSION

Create and push a release tag for Epsilon.

VERSION should be in format: X.Y.Z[-prerelease]
Examples: 0.11.0, 0.11.0-beta.1, 0.11.0-rc.1

OPTIONS:
    -Branch BRANCH    Release from specific branch (default: current branch)
    -Force           Skip confirmation prompts
    -DryRun          Show what would be done without making changes
    -Help            Show this help message

EXAMPLES:
    .\release.ps1 0.11.0                    # Create stable release v0.11.0
    .\release.ps1 0.11.0-beta.1            # Create beta release
    .\release.ps1 -Branch release/v0.11 0.11.1  # Create patch release from branch
    .\release.ps1 -DryRun 0.12.0           # Preview release process

"@ -ForegroundColor Cyan
    exit 0
}

# Check if help was requested
if ($Help) {
    Show-Usage
}

# Validate semantic version format
function Test-SemanticVersion {
    param([string]$Ver)
    
    # Pattern for semantic versioning with optional prerelease
    $pattern = '^\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?$'
    
    if ($Ver -match $pattern) {
        return $true
    } else {
        Write-ErrorMsg "Invalid version format: $Ver"
        Write-InfoMsg "Expected format: X.Y.Z[-prerelease]"
        Write-InfoMsg "Examples: 0.11.0, 0.11.0-beta.1, 0.11.0-rc.1"
        return $false
    }
}

# Check git is available
function Test-GitAvailable {
    $git = Get-Command git -ErrorAction SilentlyContinue
    if (-not $git) {
        Write-ErrorMsg "git is not installed or not in PATH"
        exit 1
    }
}

# Get current branch
function Get-CurrentBranch {
    $branch = git rev-parse --abbrev-ref HEAD 2>$null
    if ($LASTEXITCODE -ne 0) {
        Write-ErrorMsg "Failed to get current branch"
        exit 1
    }
    return $branch
}

# Check if working directory is clean
function Test-CleanWorkingDirectory {
    $status = git status --porcelain
    if ($status) {
        Write-ErrorMsg "Working directory is not clean. Please commit or stash changes:"
        Write-Host $status
        return $false
    }
    return $true
}

# Update VERSION file
function Update-VersionFile {
    param([string]$Ver)
    
    if ($DryRun) {
        Write-InfoMsg "[DRY RUN] Would update VERSION file to: $Ver"
    } else {
        $Ver | Out-File -FilePath "VERSION" -Encoding ASCII -NoNewline
        Write-SuccessMsg "Updated VERSION file to: $Ver"
    }
}

# Commit VERSION file
function New-VersionCommit {
    param([string]$Ver)
    
    if ($DryRun) {
        Write-InfoMsg "[DRY RUN] Would commit: v$Ver"
    } else {
        git add VERSION
        git commit -m "v$Ver"
        if ($LASTEXITCODE -ne 0) {
            Write-ErrorMsg "Failed to commit VERSION file"
            return $false
        }
        Write-SuccessMsg "Committed VERSION file"
    }
    return $true
}

# Create git tag
function New-GitTag {
    param([string]$Ver)
    
    $tag = "v$Ver"
    
    if ($DryRun) {
        Write-InfoMsg "[DRY RUN] Would create tag: $tag"
    } else {
        git tag -a $tag -m "Release $tag"
        if ($LASTEXITCODE -ne 0) {
            Write-ErrorMsg "Failed to create tag: $tag"
            return $false
        }
        Write-SuccessMsg "Created tag: $tag"
    }
    return $true
}

# Push to remote
function Push-Release {
    param([string]$Branch, [string]$Ver)
    
    $tag = "v$Ver"
    
    if ($DryRun) {
        Write-InfoMsg "[DRY RUN] Would push branch '$Branch' and tag '$tag' to origin"
    } else {
        # Push branch
        git push origin $Branch
        if ($LASTEXITCODE -ne 0) {
            Write-ErrorMsg "Failed to push branch to origin"
            return $false
        }
        
        # Push tag
        git push origin $tag
        if ($LASTEXITCODE -ne 0) {
            Write-ErrorMsg "Failed to push tag to origin"
            return $false
        }
        
        Write-SuccessMsg "Pushed branch and tag to origin"
    }
    return $true
}

# Main release process
function Main {
    Write-Host "╔════════════════════════════════════════════╗" -ForegroundColor Cyan
    Write-Host "║           Epsilon Release Script           ║" -ForegroundColor Cyan
    Write-Host "╚════════════════════════════════════════════╝" -ForegroundColor Cyan
    Write-Host
    
    # Validate version format
    if (-not (Test-SemanticVersion -Ver $Version)) {
        exit 1
    }
    
    # Check git is available
    Test-GitAvailable
    
    # Get current branch if not specified
    if (-not $Branch) {
        $Branch = Get-CurrentBranch
    }
    
    Write-InfoMsg "Release version: $Version"
    Write-InfoMsg "Release branch: $Branch"
    Write-InfoMsg "Dry run: $DryRun"
    Write-Host
    
    # Check working directory is clean
    if (-not (Test-CleanWorkingDirectory)) {
        if (-not $Force) {
            exit 1
        }
        Write-WarningMsg "Proceeding with uncommitted changes (--force)"
    }
    
    # Check if tag already exists
    $existingTag = git tag -l "v$Version"
    if ($existingTag) {
        Write-ErrorMsg "Tag v$Version already exists"
        if (-not $Force) {
            exit 1
        }
        Write-WarningMsg "Overwriting existing tag (--force)"
        if (-not $DryRun) {
            git tag -d "v$Version"
        }
    }
    
    # Confirm release
    if (-not $Force -and -not $DryRun) {
        Write-Host "About to create release v$Version from branch '$Branch'" -ForegroundColor Yellow
        $confirm = Read-Host "Continue? (y/N)"
        if ($confirm -ne 'y' -and $confirm -ne 'Y') {
            Write-Host "Release cancelled" -ForegroundColor Yellow
            exit 0
        }
    }
    
    # Run tests
    Write-InfoMsg "Running tests before release..."
    if ($DryRun) {
        Write-InfoMsg "[DRY RUN] Would run: .\scripts\test.ps1"
    } else {
        & ".\scripts\test.ps1"
        if ($LASTEXITCODE -ne 0) {
            Write-ErrorMsg "Tests failed - aborting release"
            exit 1
        }
        Write-SuccessMsg "All tests passed"
    }
    
    # Update VERSION file
    Update-VersionFile -Ver $Version
    
    # Commit VERSION file
    if (-not (New-VersionCommit -Ver $Version)) {
        exit 1
    }
    
    # Create tag
    if (-not (New-GitTag -Ver $Version)) {
        exit 1
    }
    
    # Push to remote
    if (-not (Push-Release -Branch $Branch -Ver $Version)) {
        exit 1
    }
    
    Write-Host
    if ($DryRun) {
        Write-Host "╔════════════════════════════════════════════╗" -ForegroundColor Green
        Write-Host "║          DRY RUN COMPLETED                ║" -ForegroundColor Green
        Write-Host "╚════════════════════════════════════════════╝" -ForegroundColor Green
        Write-InfoMsg "No changes were made"
    } else {
        Write-Host "╔════════════════════════════════════════════╗" -ForegroundColor Green
        Write-Host "║          RELEASE SUCCESSFUL!              ║" -ForegroundColor Green
        Write-Host "╚════════════════════════════════════════════╝" -ForegroundColor Green
        Write-SuccessMsg "Released version v$Version"
        Write-InfoMsg "GitHub Actions will now build and publish the release"
        Write-InfoMsg "Monitor progress at: https://github.com/jbouwman/epsilon/actions"
    }
}

# Run main
Main