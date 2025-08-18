# Windows libffi Support for epsilon.foreign

## Overview

This document outlines the plan for implementing complete Windows support for the epsilon.foreign FFI system's libffi integration, enabling real C-callable callbacks on Windows platforms.

## Current State Analysis

### epsilon.foreign Architecture

The epsilon.foreign module provides a sophisticated multi-layered FFI system:

1. **Core FFI Layer** (`foreign.lisp`): SBCL `sb-alien` integration with hardcoded function signatures
2. **Trampoline System** (`trampoline.lisp`): Compiled trampolines for performance
3. **libffi Integration** (`libffi-bridge.lisp`): Bridge to C extension for real callbacks
4. **C Extension** (`epsilon-libffi.c`): Native libffi closure implementation

### Current libffi Status

**✅ Implemented Components:**
- Complete C extension with libffi closure API
- Lisp-side bridge with type mapping
- Thread-safe callback registry
- Graceful fallback to SBCL infrastructure
- Linux/macOS platform support

**Architecture Flow:**
```
make-callback()
├── libffi available? 
├── YES: epsilon-libffi.c extension
│   ├── ffi_closure_alloc() + ffi_prep_closure_loc()
│   ├── Real C-callable function pointers
│   └── Cross-platform libffi support
└── NO: SBCL alien-lambda fallback
    └── Infrastructure testing mode
```

### Current Windows Support

**✅ Existing Cross-Platform Code:**
```lisp
;; Platform detection implemented
#+win32 (Windows-specific paths)
#-win32 (Unix paths)

;; Windows library naming partially implemented
(defun platform-library-name (name)
  ;; Handles .dll extensions and some Windows conventions
  )
```

**⚠️ Windows-Specific Gaps:**
1. **Build System**: No Windows build configuration for C extension
2. **libffi Detection**: No Windows-specific libffi location detection
3. **Library Loading**: Incomplete Windows DLL loading support
4. **Testing**: No Windows-specific test coverage

## libffi Windows Compatibility

### libffi Platform Support

libffi has **excellent Windows support**:
- **Architectures**: x86, x86-64, ARM64
- **Compilers**: MSVC, MinGW-w64, Clang
- **ABIs**: Win32, Win64, ARM calling conventions
- **Availability**: vcpkg, MSYS2, pre-built binaries

### Installation Options

**Method 1: vcpkg (Recommended)**
```cmd
vcpkg install libffi:x64-windows
# Headers: vcpkg_root/installed/x64-windows/include/
# Libraries: vcpkg_root/installed/x64-windows/lib/
```

**Method 2: MSYS2**
```bash
pacman -S mingw-w64-x86_64-libffi
# Headers: /mingw64/include/
# Libraries: /mingw64/lib/
```

**Method 3: Manual Installation**
- Download from [libffi releases](https://github.com/libffi/libffi/releases)
- Extract to standard Windows paths

## Implementation Plan

### Phase 1: Build System

**Task 1.1: Windows Build Configuration**
- Create `modules/foreign/c/Makefile.win` or CMakeLists.txt
- Support MSVC and MinGW-w64 compilers
- Handle Windows-specific compiler flags and library linking

**Example Build Commands:**
```batch
# MSVC
cl /I%VCPKG_ROOT%\installed\x64-windows\include ^
   /DDLL_EXPORT epsilon-libffi.c ^
   /link %VCPKG_ROOT%\installed\x64-windows\lib\ffi.lib ^
   /DLL /OUT:epsilon-libffi.dll

# MinGW-w64
gcc -shared -fPIC -I/mingw64/include ^
    epsilon-libffi.c -lffi -o epsilon-libffi.dll
```

**Task 1.2: Automated Build Detection**
- Detect available compilers (MSVC, MinGW-w64)
- Auto-detect libffi installation paths
- Provide clear error messages for missing dependencies

### Phase 2: Library Integration Enhancement

**Task 2.1: Windows Library Path Resolution**
```lisp
;; Enhance *library-search-paths* for Windows
(defparameter *library-search-paths*
  (append
    ;; vcpkg paths
    (when (probe-file "vcpkg/installed/x64-windows/bin/")
      (list "vcpkg/installed/x64-windows/bin/"))
    ;; MSYS2 paths
    #+win32 (when (probe-file "C:/msys64/mingw64/bin/")
              (list "C:/msys64/mingw64/bin/"))
    ;; System paths
    #+win32 '("C:/Windows/System32" "C:/Program Files/")
    ;; Unix paths for other platforms
    #-win32 '("/usr/lib" "/usr/local/lib" ...)))
```

**Task 2.2: Windows DLL Loading**
```lisp
;; Enhance platform-library-name for Windows
(defun platform-library-name (name)
  #+win32
  (cond
    ((string-suffix-p ".dll" name) name)
    ((string= name "libc") "msvcrt.dll")  ; Windows C runtime
    ((and (>= (length name) 3) (string= (subseq name 0 3) "lib"))
     (concatenate 'string (subseq name 3) ".dll"))  ; Remove "lib" prefix
    (t (concatenate 'string name ".dll")))
  #-win32 
  (existing-unix-logic))
```

**Task 2.3: libffi Extension Loading**
```lisp
;; Enhance libffi extension detection for Windows
(defun find-libffi-extension-path ()
  #+win32
  (or (probe-file "modules/foreign/c/epsilon-libffi.dll")
      (probe-file "epsilon-libffi.dll"))  ; Current directory fallback
  #-win32
  (existing-unix-logic))
```

### Phase 3: Windows-Specific libffi Detection

**Task 3.1: Auto-detection Strategy**
```lisp
(defun detect-windows-libffi ()
  "Detect libffi installation on Windows"
  (or
    ;; vcpkg detection
    (find-vcpkg-libffi)
    ;; MSYS2 detection
    (find-msys2-libffi)
    ;; Manual installation detection
    (find-manual-libffi)
    ;; System PATH detection
    (find-system-libffi)))

(defun find-vcpkg-libffi ()
  "Find vcpkg-installed libffi"
  (let ((vcpkg-root (or (sb-ext:posix-getenv "VCPKG_ROOT")
                        "vcpkg")))
    (when (probe-file (merge-pathnames "installed/x64-windows/include/ffi.h" 
                                       vcpkg-root))
      (merge-pathnames "installed/x64-windows/" vcpkg-root))))
```

**Task 3.2: Build Integration**
```lisp
;; Update load-libffi-extension for Windows paths
(defun load-libffi-extension ()
  "Load libffi C extension with Windows support"
  (let ((lib-path #+win32 (find-windows-libffi-dll)
                  #-win32 (find-unix-libffi-so)))
    (when lib-path
      (handler-case
          (setf *libffi-library* (sb-alien:load-shared-object lib-path))
        (error (e)
          (warn "Failed to load libffi extension: ~A" e))))))
```

### Phase 4: Testing and Validation

**Task 4.1: Windows-Specific Tests**
```lisp
;; Add to libffi-integration-tests.lisp
(deftest test-windows-libffi-loading
  "Test libffi extension loads on Windows"
  #+win32
  (progn
    (is (load-libffi-extension) "libffi extension should load on Windows")
    (is (test-libffi-extension) "libffi test function should work"))
  #-win32
  (skip "Windows-specific test"))

(deftest test-windows-callback-with-msvcrt
  "Test callbacks work with Windows system libraries"
  #+win32
  (let* ((compare-fn (lambda (a b) (- a b)))
         (callback-ptr (make-libffi-callback compare-fn :int '(:int :int))))
    (is (not (sb-alien:null-alien callback-ptr))
        "Should create valid callback on Windows"))
  #-win32
  (skip "Windows-specific test"))
```

**Task 4.2: Integration Testing**
```lisp
;; Test with Windows-specific C functions
(deftest test-windows-system-integration
  "Test FFI with Windows system functions"
  #+win32
  (progn
    ;; Test with GetTickCount or similar Windows API
    (defshared get-tick-count "GetTickCount" "kernel32" :unsigned-long)
    (is (> (get-tick-count) 0) "Windows API call should succeed"))
  #-win32
  (skip "Windows API test"))
```

**Task 4.3: Performance Validation**
- Benchmark callback creation/invocation on Windows
- Compare performance with Linux/macOS
- Validate memory usage patterns

### Phase 5: Documentation and Tooling

**Task 5.1: Installation Documentation**
```markdown
## Windows Installation

### Prerequisites
- SBCL for Windows
- Visual Studio Build Tools OR MinGW-w64
- libffi (via vcpkg recommended)

### Setup with vcpkg
1. Install vcpkg: `git clone https://github.com/Microsoft/vcpkg.git`
2. Install libffi: `vcpkg install libffi:x64-windows`
3. Set environment: `set VCPKG_ROOT=C:\path\to\vcpkg`
4. Build extension: `cd modules\foreign\c && make -f Makefile.win`

### Setup with MSYS2
1. Install MSYS2 from https://www.msys2.org/
2. Install libffi: `pacman -S mingw-w64-x86_64-libffi`
3. Add to PATH: `C:\msys64\mingw64\bin`
4. Build extension: `cd modules/foreign/c && make`
```

**Task 5.2: Troubleshooting Guide**
```markdown
## Windows Troubleshooting

### Common Issues

**libffi not found**
- Verify vcpkg installation: `vcpkg list | findstr ffi`
- Check VCPKG_ROOT environment variable
- Ensure compiler can find ffi.h header

**DLL loading fails**
- Check PATH includes libffi.dll location
- Verify architecture match (x64 vs x86)
- Use Dependency Walker to check DLL dependencies

**Compilation errors**
- Ensure correct compiler toolchain
- Check for conflicting headers in include path
- Verify libffi version compatibility
```

**Task 5.3: Development Scripts**
```batch
REM build-windows.bat - Automated Windows build script
@echo off
if exist "%VCPKG_ROOT%\installed\x64-windows\include\ffi.h" (
    echo Building with vcpkg libffi...
    cl /I"%VCPKG_ROOT%\installed\x64-windows\include" ^
       epsilon-libffi.c ^
       /link "%VCPKG_ROOT%\installed\x64-windows\lib\ffi.lib" ^
       /DLL /OUT:epsilon-libffi.dll
) else (
    echo libffi not found. Please install via vcpkg.
    exit /b 1
)
```

## Implementation Timeline

### Phase 1: Build System (1-2 days)
- Windows Makefile/CMake configuration
- Compiler detection and libffi path resolution
- Basic build automation

### Phase 2: Library Integration (2-3 days)
- Windows library search path enhancement
- DLL loading improvements
- libffi extension detection

### Phase 3: Testing (1-2 days)
- Windows-specific test cases
- Integration testing with Windows APIs
- Performance validation

### Phase 4: Documentation (1 day)
- Installation guides
- Troubleshooting documentation
- Development tooling

**Total Estimated Effort: 5-8 days**

## Risk Mitigation

### Potential Issues

1. **Compiler Compatibility**: Different behavior between MSVC and MinGW-w64
   - **Mitigation**: Test with both toolchains, provide specific build instructions

2. **libffi Version Differences**: Windows packages may have different versions
   - **Mitigation**: Version detection and compatibility checking

3. **Path Resolution**: Windows path conventions differ from Unix
   - **Mitigation**: Robust path detection with multiple fallback strategies

4. **DLL Dependencies**: Missing runtime dependencies
   - **Mitigation**: Clear dependency documentation and detection scripts

### Fallback Strategy

If Windows libffi integration proves problematic:
- Existing SBCL fallback continues to work
- Graceful degradation maintains core FFI functionality
- Windows users can still use epsilon.foreign without real callbacks

## Success Criteria

### Functional Requirements
- [ ] libffi C extension builds successfully on Windows
- [ ] epsilon.foreign loads and initializes correctly
- [ ] Real C-callable callbacks work with Windows system libraries
- [ ] Tests pass on Windows development environment
- [ ] Performance comparable to Linux/macOS implementations

### Documentation Requirements
- [ ] Complete Windows installation guide
- [ ] Troubleshooting documentation
- [ ] Developer setup instructions
- [ ] Integration examples with Windows APIs

### Quality Requirements
- [ ] No regression in existing cross-platform functionality
- [ ] Graceful fallback when libffi unavailable
- [ ] Clear error messages for configuration issues
- [ ] Thread-safe operation on Windows

## Future Enhancements

### Advanced Windows Integration
- Windows-specific calling conventions (stdcall, fastcall)
- COM interface support via libffi
- Integration with Windows structured exception handling

### Performance Optimizations
- Windows-specific memory management optimizations
- NUMA-aware callback allocation on Windows Server
- Integration with Windows performance monitoring

### Development Tooling
- Visual Studio project files for C extension
- Windows-specific debugging utilities
- Automated CI/CD pipeline for Windows builds

## Conclusion

The epsilon.foreign FFI system is well-architected for cross-platform support, with existing Windows awareness in the codebase. Adding complete Windows libffi support requires primarily build system configuration and path resolution enhancements, rather than fundamental architectural changes.

The implementation can be completed incrementally while maintaining full backward compatibility and graceful degradation. This approach ensures that Windows users gain access to real C-callable callbacks while preserving the existing robust FFI functionality for all users.