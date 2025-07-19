# Build Commands Update

## Summary

Added `build-all` and `test-all` as explicit commands to the epsilon development tool, removing the special-case handling of 'all' as a module name in the build command.

## Changes Made

1. **Added new commands to tool registry**:
   - `build-all` - Build all epsilon modules
   - `test-all` - Run all test suites

2. **Fixed argument processing**:
   - Changed from using `assoc` on options (which expected a list) to using `map:get` on the options map
   - This fixed the TYPE-ERROR that was occurring when processing command options

3. **Removed special case handling**:
   - The `build` command no longer treats 'all' as a special module name
   - Users should now use `build-all` to build all modules

## Usage Examples

### Before (broken):
```bash
./run.sh build all                    # This didn't work properly
./run.sh build all --include-platform # This didn't work properly
```

### After (working):
```bash
./run.sh build epsilon.core          # Build specific module
./run.sh build-all                   # Build all non-platform modules
./run.sh build-all --include-platform # Build all modules including platform-specific
./run.sh test-all                    # Run all test suites
```

## Command Help

Each command has its own help:
```bash
./run.sh build --help      # Help for building specific modules
./run.sh build-all --help  # Help for building all modules
```

The commands are now cleaner and more explicit, avoiding the ambiguity of whether 'all' is a module name or a special keyword.