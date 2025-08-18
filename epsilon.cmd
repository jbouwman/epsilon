@echo off
REM Epsilon launcher for Windows Command Prompt
REM Delegates to PowerShell script with bypass execution policy

powershell -ExecutionPolicy Bypass -File "%~dp0epsilon.ps1" %*