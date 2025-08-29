@echo off
REM Epsilon command-line interface for Windows
REM
REM Works from fresh checkout or system installation.
REM Requires SBCL to be installed and available in PATH.
REM

setlocal EnableDelayedExpansion

REM The user's working directory
set EPSILON_USER=%CD%
REM The directory where epsilon is located  
set EPSILON_HOME=%~dp0
set EPSILON_HOME=%EPSILON_HOME:~0,-1%
set EPSILON_BOOT=%EPSILON_HOME%\modules\core\src\epsilon.lisp

REM Detect boot script
if not exist "%EPSILON_BOOT%" (
    echo Error: epsilon boot script not found in %EPSILON_HOME% >&2
    exit /b 1
)

REM Check if SBCL is available
where sbcl >nul 2>&1
if errorlevel 1 (
    echo Error: SBCL not found in PATH >&2
    echo Please install SBCL: https://www.sbcl.org/ >&2
    exit /b 1
)

REM Parse debug, rebuild, and quiet flags before passing to SBCL
set DEBUG_MODE=false
set REBUILD_BOOT=false
set QUIET_MODE=false
set SBCL_ARGS=

:parse_args
if "%~1"=="" goto run_sbcl
if "%~1"=="--debug" (
    set DEBUG_MODE=true
    shift
    goto parse_args
)
if "%~1"=="--rebuild" (
    set REBUILD_BOOT=true
    shift
    goto parse_args
)
if "%~1"=="--quiet" (
    set QUIET_MODE=true
    shift
    goto parse_args
)
if "%SBCL_ARGS%"=="" (
    set SBCL_ARGS=%1
) else (
    set SBCL_ARGS=%SBCL_ARGS% %1
)
shift
goto parse_args

:run_sbcl
REM Set SBCL options based on flags
set SBCL_OPTIONS=--noinform --disable-ldb --lose-on-corruption --end-runtime-options

if "%DEBUG_MODE%"=="true" (
    set SBCL_OPTIONS=--disable-ldb --lose-on-corruption --end-runtime-options
)

if "%QUIET_MODE%"=="true" (
    set SBCL_OPTIONS=%SBCL_OPTIONS% --script
) else (
    set SBCL_OPTIONS=%SBCL_OPTIONS% --load
)

REM Run SBCL with epsilon
if "%SBCL_ARGS%"=="" (
    sbcl %SBCL_OPTIONS% "%EPSILON_BOOT%" --eval "(epsilon.main:cli-run)" --
) else (
    sbcl %SBCL_OPTIONS% "%EPSILON_BOOT%" --eval "(epsilon.main:cli-run)" -- %SBCL_ARGS%
)