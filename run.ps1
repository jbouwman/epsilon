# PowerShell script for booting Epsilon and running development steps on Windows

# Set error action to stop on errors
$ErrorActionPreference = "Stop"

# Build the sbcl command arguments
$sbclArgs = @(
    "--noinform",
    "--non-interactive", 
    "--no-sysinit",
    "--no-userinit",
    "--load", "scripts/epsilon.lisp",
    "--eval", "(epsilon.tool.dev:main)"
)

# Add any additional arguments passed to this script
$sbclArgs += $args

# Execute sbcl with the arguments
& sbcl @sbclArgs