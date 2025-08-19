# Windows container with SBCL pre-installed
FROM mcr.microsoft.com/windows/servercore:ltsc2022

LABEL org.opencontainers.image.title="Epsilon Windows Build Environment"
LABEL org.opencontainers.image.description="Windows Server Core with SBCL for epsilon builds"

# Set environment variables
ENV SBCL_VERSION=2.4.0

# Install PowerShell and basic tools
RUN powershell -Command \
    Set-ExecutionPolicy Bypass -Scope Process -Force; \
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; \
    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# Install Git and other tools via Chocolatey
RUN choco install -y git curl

# Download and install SBCL
RUN powershell -Command \
    $url = 'https://downloads.sourceforge.net/project/sbcl/sbcl/2.4.0/sbcl-2.4.0-x86-64-windows-binary.msi'; \
    $output = 'C:\temp\sbcl-installer.msi'; \
    New-Item -ItemType Directory -Force -Path C:\temp; \
    Invoke-WebRequest -Uri $url -OutFile $output; \
    Start-Process msiexec.exe -Wait -ArgumentList '/I', $output, '/quiet', '/norestart'; \
    Remove-Item C:\temp -Recurse -Force

# Add SBCL to PATH
RUN setx PATH "%PATH%;C:\Program Files\Steel Bank Common Lisp\"

# Verify installation and find actual SBCL location
RUN powershell -Command \
    Write-Host 'Searching for SBCL installation...'; \
    $sbclPath = Get-ChildItem 'C:\Program Files' -Recurse -Filter sbcl.exe -ErrorAction SilentlyContinue | Select-Object -First 1; \
    if ($sbclPath) { \
        Write-Host "Found SBCL at: $($sbclPath.FullName)"; \
        & $sbclPath.FullName --version; \
        $sbclDir = Split-Path -Parent $sbclPath.FullName; \
        [Environment]::SetEnvironmentVariable('SBCL_HOME', $sbclDir, 'Machine'); \
        [Environment]::SetEnvironmentVariable('PATH', "$sbclDir;$env:PATH", 'Machine') \
    } else { \
        Write-Host 'ERROR: SBCL not found after installation'; \
        exit 1 \
    }

# Set working directory
WORKDIR C:\workspace

CMD ["cmd"]