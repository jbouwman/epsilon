# Epsilon Installation Script for Windows PowerShell
#
# This script installs Epsilon from GitHub releases
# Usage: Invoke-WebRequest -Uri https://github.com/jbouwman/epsilon/releases/latest/download/install.ps1 | Invoke-Expression
#

param(
    [string]$InstallDir = "$env:USERPROFILE\.epsilon",
    [string]$BinaryDir = "$env:USERPROFILE\.local\bin",
    [string]$Version = "latest"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# Configuration
$GITHUB_REPO = "jbouwman/epsilon"
$GITHUB_API_URL = "https://api.github.com/repos/$GITHUB_REPO/releases/latest"

# Colors for output (PowerShell style)
function Write-ErrorMessage { param($msg) Write-Host "✗ $msg" -ForegroundColor Red }
function Write-SuccessMessage { param($msg) Write-Host "✓ $msg" -ForegroundColor Green }
function Write-WarningMessage { param($msg) Write-Host "⚠ $msg" -ForegroundColor Yellow }
function Write-InfoMessage { param($msg) Write-Host "ℹ $msg" -ForegroundColor Blue }

# Platform detection
function Get-Platform {
    $arch = [System.Environment]::Is64BitOperatingSystem
    if ($arch) {
        return "windows-x86_64"
    } else {
        Write-ErrorMessage "32-bit Windows is not supported"
        exit 1
    }
}

# Check for required tools
function Test-Requirements {
    # Check if SBCL is installed
    $sbcl = Get-Command sbcl -ErrorAction SilentlyContinue
    if (-not $sbcl) {
        Write-WarningMessage "SBCL not found in PATH"
        Write-InfoMessage "Please install SBCL from: https://www.sbcl.org/"
        Write-InfoMessage "Or use: choco install sbcl"
    }
    
    # Check if tar is available (comes with Windows 10+)
    $tar = Get-Command tar -ErrorAction SilentlyContinue
    if (-not $tar) {
        Write-ErrorMessage "tar command not found. Please ensure you're using Windows 10 or later."
        exit 1
    }
}

# Get latest release info
function Get-LatestRelease {
    param([string]$PlatformArch)
    
    Write-InfoMessage "Fetching latest release information..."
    
    try {
        # Use TLS 1.2 for GitHub API
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        
        $headers = @{
            "User-Agent" = "Epsilon-Installer-PowerShell"
        }
        
        if ($Version -eq "latest") {
            $response = Invoke-RestMethod -Uri $GITHUB_API_URL -Headers $headers
            $version = $response.tag_name
        } else {
            $version = $Version
            if (-not $version.StartsWith("v")) {
                $version = "v$version"
            }
            $specificUrl = "https://api.github.com/repos/$GITHUB_REPO/releases/tags/$version"
            $response = Invoke-RestMethod -Uri $specificUrl -Headers $headers
        }
        
        # Find the right asset for Windows
        $assetName = "epsilon-$($version.TrimStart('v'))-$PlatformArch.tar.gz"
        $asset = $response.assets | Where-Object { $_.name -eq $assetName }
        
        if (-not $asset) {
            Write-ErrorMessage "No release found for $PlatformArch"
            Write-InfoMessage "Available assets:"
            $response.assets | ForEach-Object { Write-Host "  - $($_.name)" }
            exit 1
        }
        
        return @{
            Version = $version
            DownloadUrl = $asset.browser_download_url
            FileName = $asset.name
        }
    } catch {
        Write-ErrorMessage "Failed to fetch release information: $_"
        exit 1
    }
}

# Download release
function Get-Release {
    param($ReleaseInfo, $TargetDir)
    
    $downloadPath = Join-Path $env:TEMP $ReleaseInfo.FileName
    
    Write-InfoMessage "Downloading $($ReleaseInfo.FileName)..."
    
    try {
        # Download with progress
        $ProgressPreference = 'Continue'
        Invoke-WebRequest -Uri $ReleaseInfo.DownloadUrl -OutFile $downloadPath
        
        Write-SuccessMessage "Downloaded to $downloadPath"
        return $downloadPath
    } catch {
        Write-ErrorMessage "Failed to download: $_"
        exit 1
    }
}

# Install epsilon
function Install-Epsilon {
    param($TarFile, $InstallDir)
    
    Write-InfoMessage "Installing to $InstallDir..."
    
    # Create install directory if it doesn't exist
    if (Test-Path $InstallDir) {
        Write-WarningMessage "Installation directory already exists. Backing up..."
        $backup = "$InstallDir.backup.$(Get-Date -Format 'yyyyMMdd-HHmmss')"
        Move-Item $InstallDir $backup
        Write-InfoMessage "Previous installation backed up to $backup"
    }
    
    New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
    
    # Extract archive
    try {
        Push-Location $InstallDir
        tar -xzf $TarFile
        
        # Find the extracted directory (should be epsilon-VERSION)
        $extractedDir = Get-ChildItem -Directory | Select-Object -First 1
        
        if ($extractedDir) {
            # Move contents up one level
            Get-ChildItem -Path $extractedDir.FullName | Move-Item -Destination $InstallDir
            Remove-Item $extractedDir.FullName -Force
        }
        
        Pop-Location
        Write-SuccessMessage "Extracted epsilon to $InstallDir"
    } catch {
        Pop-Location
        Write-ErrorMessage "Failed to extract archive: $_"
        exit 1
    }
}

# Create launcher scripts
function Install-Launchers {
    param($InstallDir, $BinaryDir)
    
    Write-InfoMessage "Creating launcher scripts..."
    
    # Create binary directory if it doesn't exist
    if (-not (Test-Path $BinaryDir)) {
        New-Item -ItemType Directory -Path $BinaryDir -Force | Out-Null
    }
    
    # Create PowerShell launcher
    $psLauncher = Join-Path $BinaryDir "epsilon.ps1"
    @"
# Epsilon launcher script
`$EPSILON_HOME = "$InstallDir"
& "`$EPSILON_HOME\epsilon.ps1" `$args
"@ | Out-File -FilePath $psLauncher -Encoding UTF8
    
    # Create batch file launcher for cmd.exe
    $cmdLauncher = Join-Path $BinaryDir "epsilon.cmd"
    @"
@echo off
set EPSILON_HOME=$InstallDir
powershell -ExecutionPolicy Bypass -File "%EPSILON_HOME%\epsilon.ps1" %*
"@ | Out-File -FilePath $cmdLauncher -Encoding ASCII
    
    Write-SuccessMessage "Created launchers in $BinaryDir"
    
    # Check if binary directory is in PATH
    $currentPath = [Environment]::GetEnvironmentVariable("Path", "User")
    if ($currentPath -notlike "*$BinaryDir*") {
        Write-WarningMessage "$BinaryDir is not in your PATH"
        Write-InfoMessage "To add it to your PATH, run:"
        Write-Host "[Environment]::SetEnvironmentVariable('Path', `$env:Path + ';$BinaryDir', 'User')" -ForegroundColor Cyan
    }
}

# Main installation flow
function Main {
    Write-Host "╔════════════════════════════════════════════╗" -ForegroundColor Cyan
    Write-Host "║        Epsilon Installation Script         ║" -ForegroundColor Cyan
    Write-Host "╚════════════════════════════════════════════╝" -ForegroundColor Cyan
    Write-Host
    
    # Check requirements
    Test-Requirements
    
    # Detect platform
    $platform = Get-Platform
    Write-InfoMessage "Detected platform: $platform"
    
    # Get release info
    $releaseInfo = Get-LatestRelease -PlatformArch $platform
    Write-InfoMessage "Installing version: $($releaseInfo.Version)"
    
    # Download release
    $tarFile = Get-Release -ReleaseInfo $releaseInfo -TargetDir $env:TEMP
    
    # Install epsilon
    Install-Epsilon -TarFile $tarFile -InstallDir $InstallDir
    
    # Create launchers
    Install-Launchers -InstallDir $InstallDir -BinaryDir $BinaryDir
    
    # Clean up
    Remove-Item $tarFile -Force -ErrorAction SilentlyContinue
    
    # Test installation
    Write-InfoMessage "Testing installation..."
    $epsilonExe = Join-Path $InstallDir "epsilon.ps1"
    if (Test-Path $epsilonExe) {
        & $epsilonExe --version
        Write-SuccessMessage "Epsilon installed successfully!"
    } else {
        # Fallback to bash script
        $epsilonBash = Join-Path $InstallDir "epsilon"
        if (Test-Path $epsilonBash) {
            & bash $epsilonBash --version
            Write-SuccessMessage "Epsilon installed successfully!"
        } else {
            Write-ErrorMessage "Installation verification failed"
            exit 1
        }
    }
    
    Write-Host
    Write-Host "Installation complete!" -ForegroundColor Green
    Write-Host "Epsilon home: $InstallDir" -ForegroundColor Cyan
    Write-Host "Launchers in: $BinaryDir" -ForegroundColor Cyan
    Write-Host
    Write-Host "To get started, run:" -ForegroundColor Yellow
    Write-Host "  epsilon --help" -ForegroundColor White
}

# Run main installation
Main