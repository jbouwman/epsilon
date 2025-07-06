# EPSILON.TOOL.BUILD

## API Reference

### BUILD

**Type**: Function

**Signature**: `(BUILD MODULE &KEY FORCE (ERROR-BEHAVIOR HALT) (WARNING-BEHAVIOR IGNORE) (REPORTER
                                                                          (MAKE-INSTANCE
                                                                           'SHELL-BUILD-REPORT)))`

Build module sources and optionally tests.
  
  MODULE - Module name to build (e.g., 'epsilon.core', 'http'). Looks up module directory from registry.
  FORCE - Force compilation of all build steps regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  REPORTER - Reporter instance for build progress

---

### REGISTER-MODULE

**Type**: Function

**Signature**: `(REGISTER-MODULE MODULE-SPEC)`

Register a single module for building.
   
MODULE-SPEC can be:
- A string pathname to a directory containing package.yaml

---

### REGISTER-MODULES

**Type**: Function

**Signature**: `(REGISTER-MODULES &KEY (BASE-DIR
                        (FILE-URI (NAMESTRING *DEFAULT-PATHNAME-DEFAULTS*))))`

Discover and register all applicable modules found under base-dir/module/

---

