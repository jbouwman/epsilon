# Build Environment Design

## Problem Statement

The current build system has proliferated global variables and scattered state:
- `*package-source*` - The current package source
- `*additional-package-repos*` - Additional repositories
- `*loaded-packages*` - Registry of loaded modules
- `*error-behavior*` - Error handling policy
- `*warning-behavior*` - Warning handling policy
- `*parallel-compilation*` - Parallel build flag
- `*build-timeout*` - Compilation timeout

This makes it difficult to:
1. Understand the complete build configuration
2. Run multiple builds with different configurations
3. Test the build system effectively
4. Add new features cleanly

## Proposed Design

### Build Environment Object

```lisp
(defclass build-environment ()
  ((package-source :accessor environment-package-source
                   :documentation "Composite package source for this environment")
   (package-repos :initform '()
                  :accessor environment-package-repos
                  :documentation "List of additional package repository paths")
   (loaded-modules :initform (map:make-map)
                   :accessor environment-loaded-modules
                   :documentation "Registry of loaded modules")
   (policies :initarg :policies
             :accessor environment-policies
             :documentation "Build policies (errors, warnings, etc.)")
   (options :initarg :options
            :accessor environment-options
            :documentation "Build options (parallel, timeout, etc.)")))

(defclass build-policies ()
  ((error-behavior :initarg :error-behavior :initform :halt)
   (warning-behavior :initarg :warning-behavior :initform :ignore)
   (optimization :initarg :optimization :initform '(speed 1 safety 3))))

(defclass build-options ()
  ((parallel :initarg :parallel :initform nil)
   (timeout :initarg :timeout :initform 60)
   (force :initarg :force :initform nil)
   (verbose :initarg :verbose :initform nil)))
```

### API Changes

All build functions would take an environment as their first parameter:

```lisp
(defgeneric build (environment module &key force))
(defgeneric add-package-repo (environment path))
(defgeneric get-module (environment name))
(defgeneric mark-module-loaded (environment name))
```

### Usage Example

```lisp
;; Create a build environment with custom settings
(let ((env (make-build-environment
            :policies (make-instance 'build-policies
                                     :error-behavior :print
                                     :warning-behavior :print)
            :options (make-instance 'build-options
                                    :parallel t
                                    :verbose t))))
  ;; Add additional package repositories
  (add-package-repo env "/path/to/external/packages")
  
  ;; Build a module
  (build env "myapp"))
```

### Command Integration

Commands would create and configure an environment based on parsed arguments:

```lisp
(defmethod run-command ((command build) parsed-args)
  (let* ((env (make-build-environment))
         (repos (map:get (argparse:parsed-options parsed-args) "package-repo"))
         (parallel (map:get (argparse:parsed-options parsed-args) "parallel")))
    ;; Configure environment from command line
    (dolist (repo repos)
      (add-package-repo env repo))
    (when parallel
      (setf (parallel (environment-options env)) t))
    ;; Build the package
    (let ((package (resolve-package command parsed-args)))
      (build env package))))
```

### Backward Compatibility

For backward compatibility, maintain a default environment and thread-local binding:

```lisp
(defvar *default-environment* nil)
(defvar *current-environment* nil)

(defun current-environment ()
  (or *current-environment*
      *default-environment*
      (setf *default-environment* (make-build-environment))))

;; Old API delegates to current environment
(defun build (module &rest args)
  (apply #'build (current-environment) module args))
```

## Benefits

1. **Cleaner API**: All configuration in one place
2. **Testability**: Can create isolated environments for testing
3. **Flexibility**: Easy to add new configuration options
4. **Thread Safety**: Each build can have its own environment
5. **Clarity**: Clear what configuration affects a build

## Migration Path

1. Create new environment classes
2. Add environment-aware versions of all functions
3. Update commands to use environments
4. Deprecate global variables
5. Remove backward compatibility layer in future version