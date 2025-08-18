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

# Verify installation - Note: path might be different after install
RUN powershell -Command \
    if (Test-Path 'C:\Program Files\Steel Bank Common Lisp\sbcl.exe') { \
        & 'C:\Program Files\Steel Bank Common Lisp\sbcl.exe' --version \
    } else { \
        Write-Host 'SBCL not found at expected location'; \
        Get-ChildItem 'C:\Program Files' -Recurse -Filter sbcl.exe | Select-Object -First 1 \
    }

# Set working directory
WORKDIR C:\workspace

CMD ["cmd"]