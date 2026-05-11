;;;; This module provides the loader infrastructure for Epsilon modules.
;;;; All source files use standard defpackage (or Epsilon's extended defpackage
;;;; with :require/:enter support).
(cl:defpackage epsilon.loader
  (:use cl)
  (:local-nicknames (fs epsilon.file)
                    (lock epsilon.sys.lock)
                    (str epsilon.string)
                    (path epsilon.path)
                    (reader epsilon.readtable)
                    (env epsilon.sys.env)
                    (map epsilon.map)
                    (match epsilon.match)
                    (log epsilon.log)
                    (compile epsilon.compile)
                    (graph epsilon.graph))
  (:export
    ;; Module registry (from loader.lisp consolidation)
    module-info
    module-name
    module-location
    module-loaded-p
    module-load-time
    module-source-newest
    module-content-hash
    module-metadata
    build-environment
    make-build-environment
    modules
    environment-config
    get-module
    find-module
    query-modules
    register-module
    scan-module-directory
    mark-module-loaded
    validate-module-metadata
    validate-module-metadata-basic
    ;; Build output root
    *build-root*
    ;; Environment
    *environment*
    environment
    epsilon-module-provider
    install-module-provider
    module-uri
    ;; Module loading
    load-module
    ;; File discovery
    lisp-file-p
    list-lisp-files
    find-all-sources
    ;; Path/package utilities
    derive-package-from-path
    package-to-path
    validate-package-path
    package-name-compatible-p
    ;; Package management
    public-symbol-p
    auto-export-public-symbols
    sync-package-exports
    ;; Form analysis
    has-package-form-p
    ;; Compile and load API
    find-package-form
    extract-package-name
    ;; Module-level data structures
    source-info
    make-source-info
    source-info-p
    copy-source-info
    source-info-uri
    source-info-package-name
    source-info-declared-name
    source-info-src-root
    source-info-requires
    source-info-explicit-exports
    module-project
    make-module-project
    module-project-p
    copy-module-project
    module-project-name
    module-project-path
    module-project-src-root
    module-project-sources
    module-project-tests
    module-project-integration-tests
    module-project-requires
    module-project-metadata
    ;; Module discovery
    load-module-project
    load-ambient-modules
    ;; Module building
    source-build-order
    fasl-path-for-source
    build-module-project
    build-lisp-source
    build-dir-for-module
    clean-orphaned-fasls
    ;; Module loading
    load-module-resources
    ;; Module resource resolution
    module-resource
    *current-module-name*
    *building-module*
    ;; Import validation
    *strict-imports*
    validate-imports
    missing-import-warning
    missing-import-filepath
    missing-import-packages
    ;; Content-addressable FASL validation
    *content-hashing-p*
    enable-content-hashing
    local-fasl-valid-p
    key-sidecar-path
    read-key-sidecar
    write-key-sidecar
    compute-source-content-hash
    compute-content-key
    compute-module-hash
    current-fasl-file-version
    fasl-load-failure-p
    load-fasl-with-recovery
    ;; Reference extraction and cache-miss diagnosis
    extract-references
    extract-source-abi
    read-source-forms-with-package-context
    compute-content-key-v2
    diagnose-cache-miss
    why-recompiled?
    record-rebuild-reason
    *rebuild-reasons*
    ;; Consolidated module loading (IMPL-086)
    parse-epsilon-header
    has-epsilon-header-p
    unified-source-info
    make-unified-source-info
    unified-source-info-p
    copy-unified-source-info
    unified-source-info-uri
    unified-source-info-package-name
    unified-source-info-file-type
    unified-source-info-requires
    unified-source-info-provides
    unified-source-info-imports
    unified-source-info-exports
    unified-source-info-use
    unified-source-info-shadow
    unified-source-info-header-p
    extract-defpackage-info
    extract-header-info
    extract-unified-source-info
    unified-build-order))

(in-package epsilon.loader)

;;; ---------------------------------------------------------------------------
;;; Core Classes - must be available at compile time for other modules
;;; ---------------------------------------------------------------------------
(defclass module-info
  ()
  ((name :initarg :name :reader module-name :type string :documentation "Module name") (location :initarg
                                                                                                 :location
                                                                                                 :reader
                                                                                                 module-location
                                                                                                 :documentation
                                                                                                 "Module location (path or URI)")
                                                                                       (loaded-p :initarg
                                                                                                 :loaded-p
                                                                                                 :accessor
                                                                                                 module-loaded-p
                                                                                                 :initform
                                                                                                 nil
                                                                                                 :type
                                                                                                 boolean
                                                                                                 :documentation
                                                                                                 "Whether module has been loaded")
                                                                                       (load-time :initarg
                                                                                                  :load-time
                                                                                                  :accessor
                                                                                                  module-load-time
                                                                                                  :initform
                                                                                                  nil
                                                                                                  :documentation
                                                                                                  "When module was loaded")
                                                                                       (source-newest :initarg
                                                                                                      :source-newest
                                                                                                      :accessor
                                                                                                      module-source-newest
                                                                                                      :initform
                                                                                                      0
                                                                                                      :type
                                                                                                      integer
                                                                                                      :documentation
                                                                                                      "Newest source file-write-date across this module's sources")
                                                                                       (content-hash :initarg
                                                                                                     :content-hash
                                                                                                     :accessor
                                                                                                     module-content-hash
                                                                                                     :initform
                                                                                                     nil
                                                                                                     :type
                                                                                                     (or null
                                                                                                         string)
                                                                                                     :documentation
                                                                                                     "Module content hash (BLAKE3, set after build)")
                                                                                       (metadata :initarg
                                                                                                 :metadata
                                                                                                 :accessor
                                                                                                 module-metadata
                                                                                                 :initform
                                                                                                 nil
                                                                                                 :documentation
                                                                                                 "Full module metadata including provides, requires, etc."))
  (:documentation "Information about a loaded or registered module"))

(defclass build-environment
  ()
  ((modules :initform
            (map:make-map)
            :accessor
            modules
            :documentation
            "Registry of all modules (map of name -> module-info)")
   (config :initarg
                                                                            :config
                                                                            :accessor
                                                                            environment-config
                                                                            :initform
                                                                            (map:make-map)
                                                                            :documentation
                                                                            "Build configuration map containing all settings"))
  (:documentation "Encapsulates all configuration and state for a build session"))

(defun make-build-environment (&key config)
  "Create a new build environment"
  (make-instance 'build-environment
                 :config
                 (or config
                     (map:make-map :error-behavior
                                   :halt
                                   :warning-behavior
                                   :ignore
                                   :optimization
                                   '(speed 1 safety 3 debug 2)
                                   :parallel
                                   nil
                                   :timeout
                                   60
                                   :force
                                   nil
                                   :verbose
                                   nil))))

;;; ---------------------------------------------------------------------------
;;; Global State
;;; ---------------------------------------------------------------------------
(defvar *environment*
  nil
  "The current build environment. Initialized by (environment) function.")

(defvar *build-root*
  nil
  "Root directory for build outputs (_build/).
   Set during environment initialization from EPSILON_HOME.")

(defvar *current-module-name*
  nil
  "Name of the module currently being loaded/compiled.
   Bound during module loading so that module-resource can resolve
   resources without requiring an explicit module name argument.")

(defvar *compile-lock*
  (lock:make-recursive-lock "compile-lock")
  "Serializes all compilation across threads. SBCL's compiler is not
   reentrant, so concurrent compile-file/load calls must be avoided.
   Recursive: a thread may re-enter during defpackage auto-load.")

(defvar *building-module*
  nil
  "Name of the module currently being built, or NIL. Used to prevent
   re-entrant build-module-project calls when defpackage :import forms
   trigger module loading during compilation.")

;;; Module metadata validation
(defun validate-module-metadata (metadata filepath)
  "Validate module.sexp metadata and return detailed error messages for any issues.
   Returns NIL if valid, or a list of error messages if invalid."
  (if (and (find-package "EPSILON.MODULE-SCHEMA")
           (fboundp (find-symbol "VALIDATE-MODULE-METADATA" "EPSILON.MODULE-SCHEMA")))
    (multiple-value-bind (valid-p errors) (funcall (find-symbol "VALIDATE-MODULE-METADATA"
                                                                "EPSILON.MODULE-SCHEMA")
                                                   metadata
                                                   filepath)
      (if valid-p
        nil
        errors))
    (validate-module-metadata-basic metadata filepath)))

(defun validate-module-metadata-basic (metadata filepath)
  "Basic validation for module metadata when advanced validation is not available"
  (let ((errors '())
        (valid-keys '(:name :module-set
                            :description
                            :author
                            :platform
                            :sources
                            :tests
                            :benchmarks
                            :examples
                            :experiments
                            :docs
                            :data
                            :requires
                            :provides
                            :source-type
                            :resources
                            :integration
                            :commands
                            :main
                            :stability)))
    (unless (and (listp metadata) (evenp (length metadata)))
      (push (format nil "Invalid module.sexp format in ~A: must be a property list" filepath)
            errors)
      (return-from validate-module-metadata-basic errors))
    (let ((name (getf metadata :name)))
      (cond
        ((null name)
         (push (format nil "Missing required field :name in ~A" filepath) errors))
        ((not (stringp name))
         (push (format nil "Invalid :name field in ~A: must be a string" filepath) errors))
        ((string= name "")
         (push (format nil "Invalid :name field in ~A: cannot be empty" filepath) errors))))
    (loop for key in metadata by #'cddr
          unless (member key valid-keys)
          do (push (format nil "Unknown key ~A in ~A" key filepath) errors))
    (let ((description (getf metadata :description))
          (requires (getf metadata :requires))
          (provides (getf metadata :provides)))
      (when (and description (not (stringp description)))
        (push (format nil "Invalid :description in ~A: must be a string" filepath) errors))
      (when requires
        (unless (listp requires)
          (push (format nil "Invalid :requires in ~A: must be a list" filepath) errors))
        (when (listp requires)
          (loop for req in requires
                unless (stringp req)
                do (push (format nil "Invalid :requires entry in ~A: must be string" filepath)
                         errors))))
      (when provides
        (unless (listp provides)
          (push (format nil "Invalid :provides in ~A: must be a list" filepath) errors))
        (when (listp provides)
          (loop for prov in provides
                unless (stringp prov)
                do (push (format nil "Invalid :provides entry in ~A: must be string" filepath)
                         errors)))))
    (when errors
      (nreverse errors))))

;;; Module Discovery and Registration
(defun register-module (path-arg)
  "Register a single module directory (contains module.sexp)"
  (let* ((module-path (path:ensure-path path-arg))
         (module-file (path:path-join module-path "module.sexp"))
         (file-string (path:path-string module-file)))
    (log:debug "Checking for module.sexp at: ~A" file-string)
    (if (probe-file file-string)
      (handler-case (let* ((info (with-open-file (stream file-string :if-does-not-exist nil)
                                   (when stream
                                     (read stream))))
                           (validation-errors (validate-module-metadata info file-string)))
        (cond
          (validation-errors
           (log:error "Invalid module.sexp at ~A:" file-string)
           (dolist (err validation-errors) (log:error "  ~A" err))
           (error "Module validation failed: ~A" file-string))
          (t
           (let ((module-name (getf info :name)))
             (let ((pkg-info (make-instance 'module-info
                                            :name
                                            module-name
                                            :location
                                            module-path
                                            :metadata
                                            info
                                            :loaded-p
                                            nil)))
               (log:debug "Registering module: ~A from ~A" module-name path-arg)
               (setf (modules *environment*)
                     (map:assoc (modules *environment*) module-name pkg-info)))))))
        (error
         (e)
         (log:error "Failed to parse module.sexp at ~A: ~A" file-string e)
         (error "Invalid module.sexp format at ~A: ~A" file-string e)))
      (log:debug "No module.sexp found at: ~A" file-string))))

(defun scan-module-directory (path-arg)
  "Scan directory for module subdirectories and register them.
   Returns the number of subdirectories scanned."
  (let ((base-path (path:ensure-path path-arg))
        (count 0))
    (log:debug "Scanning module directory: ~A" (path:path-string base-path))
    (when (probe-file (path:path-string base-path))
      (let ((dirs (path:list-directory base-path :type :directories)))
        (log:debug "Found ~A subdirectories in ~A" (length dirs) path-arg)
        (dolist (entry-path dirs)
          (log:debug "Attempting to register module from: ~A" entry-path)
          (register-module entry-path)
          (incf count))))
    count))

;;; Module Registry Operations
(defun get-module (name &key (error-p nil))
  "Get module-info for a given module name, or NIL if not found"
  (let ((modules-map (modules *environment*)))
    (or (and modules-map (map:get modules-map name))
        (and error-p (error "Module not found: ~A" name)))))

(defun mark-module-loaded (name)
  "Mark a module as loaded"
  (let ((info (get-module name)))
    (when info
      (setf (module-loaded-p info) t)
      (setf (module-load-time info) (get-universal-time)))))

(defun query-modules (&key name provides loaded-only predicate)
  "Query modules matching specified criteria."
  (let ((results '())
        (current-platform (string-downcase (symbol-name (env:platform))))
        (modules-map (modules *environment*)))
    (unless modules-map
      (return-from query-modules nil))
    (loop for info in (map:vals modules-map)
          for pkg-name = (module-name info)
          for metadata = (module-metadata info)
          for pkg-provides = (or (getf metadata :provides) (list pkg-name))
          for pkg-platform = (getf metadata :platform)
          when (and (or (not name) (string= name pkg-name))
                    (or (not provides)
                        (string= provides pkg-name)
                        (member provides pkg-provides :test #'string=))
                    (or (not pkg-platform) (string= pkg-platform current-platform))
                    (or (not predicate) (funcall predicate info)))
          collect info into matched
          finally (setf results matched))
    (when loaded-only
      (setf results
            (remove-if-not (lambda (i)
                             (module-loaded-p i))
                           results)))
    results))

(defun find-module (&key name provides)
  "Find exactly one module matching the given criteria."
  (let ((matches (query-modules :name name :provides provides)))
    (cond
      ((null matches)
       (error "No module found~@[ name=~A~]~@[ provides=~A~]" name provides))
      ((> (length matches) 1)
       (error "Multiple modules found~@[ name=~A~]~@[ provides=~A~]: ~{~A~^, ~}"
              name
              provides
              (mapcar (lambda (m)
                        (module-name m))
                      matches)))
      (t
       (first matches)))))

(defun module-uri (module-info)
  "Get the URI (location) of a module."
  (module-location module-info))

;;; ---------------------------------------------------------------------------
;;; Workspace and Environment
;;; ---------------------------------------------------------------------------
(defparameter *ambient-stdlib-modules*
  '("epsilon.diagnostic"
    "epsilon.datalog.core"
    "epsilon.log"
    "epsilon.fs")
  "Modules loaded automatically after environment setup. These form the
   canonical public stdlib surface -- any module may import them without
   declaring them in its module.sexp. Load order matters: modules later
   in the list may depend on earlier ones (e.g. epsilon.fs imports
   epsilon.path which is in the bootstrap, and epsilon.log uses
   epsilon.typeclass which is in the bootstrap).")

(defun load-ambient-modules ()
  "Load the ambient stdlib modules if available. Silently skips modules
   that are not present (e.g. in a minimal install)."
  (dolist (name *ambient-stdlib-modules*)
    (when (get-module name)
      (handler-case (load-module name)
        (error (e)
          (log:warn "Failed to auto-load ambient module ~A: ~A" name e))))))

(defun environment ()
  "Get or create the default environment.
   The root module is registered directly from EPSILON_HOME (where module.sexp
   lives). If a workspace.sexp is also present, its modules are loaded too."
  (unless *environment*
    (setf *environment* (make-build-environment))
    (let ((epsilon-home (env:getenv "EPSILON_HOME")))
      (unless epsilon-home
        (error "EPSILON_HOME environment variable not set"))
      ;; Set build root for consolidated FASL output
      (setf *build-root* (path:path-string (path:path-join epsilon-home "_build")))
      ;; Register the root module directly from EPSILON_HOME
      (register-module epsilon-home)
      ;; Always scan EPSILON_HOME/modules so the bundled stdlib (epsilon.foreign,
      ;; epsilon.io, ...) is discoverable without needing a workspace.sexp.
      (let ((bundled-modules (path:path-string (path:path-join epsilon-home "modules"))))
        (when (probe-file bundled-modules)
          (scan-module-directory bundled-modules)))
      (unless (get-module "epsilon")
        (error "Core module epsilon not found"))
      (install-module-provider)))
  *environment*)

(defun epsilon-module-provider (module-name)
  "SBCL module provider for epsilon modules.
   Called by REQUIRE when a module is not already loaded."
  (when *environment*
    (let* ((name (string-downcase (string module-name)))
           (info (get-module name)))
      (when info
        (unless (module-loaded-p info)
          (log:info "Loading module: ~A" name)
          (load-module name))
        t))))

(defun install-module-provider ()
  "Install epsilon's module provider into SBCL's require mechanism."
  (pushnew 'epsilon-module-provider sb-ext:*module-provider-functions*))

;;; ---------------------------------------------------------------------------
;;; Import Validation
;;; ---------------------------------------------------------------------------
(defvar *strict-imports*
  nil
  "When T, missing imports signal an error instead of a warning.
   Set to T for production builds to catch import typos at compile time.")

(define-condition missing-import-warning
  (simple-warning)
  ((filepath :initarg :filepath :reader missing-import-filepath) (missing-packages :initarg
                                                                                   :missing-packages
                                                                                   :reader
                                                                                   missing-import-packages))
  (:report (lambda (condition stream)
             (format stream
                     "~A: Missing imports (will fail at runtime): ~{~A~^, ~}"
                     (missing-import-filepath condition)
                     (missing-import-packages condition)))))

(defun validate-imports (imports filepath)
  "Validate that all imported packages exist.
   Returns valid imports. Signals a warning (or error if *strict-imports*) for
   missing packages.

   IMPORTS is an alist of (nickname . package-name).
   FILEPATH is used for error reporting.

   This prevents silent failures where a typo like (import (epsilon.jsom json))
   loads successfully but fails at runtime."
  (let ((valid nil)
        (missing nil))
    (dolist (pair imports)
      (let ((pkg-name (cdr pair)))
        (if (find-package pkg-name)
          (push pair valid)
          (push (string pkg-name) missing))))
    (when missing
      (let ((condition (make-condition 'missing-import-warning
                                       :filepath
                                       filepath
                                       :missing-packages
                                       (nreverse missing)
                                       :format-control
                                       "~A: Missing imports: ~{~A~^, ~}"
                                       :format-arguments
                                       (list filepath missing))))
        (if *strict-imports*
          (error condition)
          (warn condition))))
    (nreverse valid)))

;;; ---------------------------------------------------------------------------
;;; File Discovery
;;; ---------------------------------------------------------------------------
(defun lisp-file-p (path)
  "Return T if PATH is an Epsilon .lisp file."
  (let ((path-str (if (pathnamep path)
                    (namestring path)
                    path)))
    (str:ends-with-p path-str ".lisp")))

(defun list-lisp-files (directory)
  "List all .lisp files in DIRECTORY recursively.
   Returns sorted list of file paths."
  (fs:list-files directory ".lisp"))

;;; ---------------------------------------------------------------------------
;;; Path/Package Utilities
;;; ---------------------------------------------------------------------------
(defun derive-package-from-path (filepath src-root)
  "Derive package name from .lisp file path relative to src-root.

   The package name is derived by:
   1. Computing the relative path from src-root to filepath
   2. Stripping the .lisp extension
   3. Converting path separators to dots

   Examples:
     (derive-package-from-path \"src/epsilon/http/client.lisp\" \"src/\")
     => \"epsilon.http.client\"

     (derive-package-from-path \"/proj/src/epsilon/http.lisp\" \"/proj/src/\")
     => \"epsilon.http\""
  (let* ((fp (path:normalize-separators (namestring filepath)))
         (sr (path:normalize-separators (if (str:ends-with-p src-root "/")
                                          src-root
                                          (concatenate 'string src-root "/"))))
         ;; Get relative path
         (rel (if (str:starts-with-p fp sr)
                (subseq fp (length sr))
                fp))
         ;; Strip .lisp extension
         (sans-ext (if (str:ends-with-p rel ".lisp")
                     (subseq rel 0 (- (length rel) 5))
                     rel)))
    ;; Convert path separators to dots
    (substitute #\. #\/ sans-ext)))

(defun package-to-path (package-name)
  "Convert package name to expected .lisp file path.

   Example:
     (package-to-path \"epsilon.http.client\")
     => \"epsilon/http/client.lisp\""
  (concatenate 'string (substitute #\/ #\. (string-downcase (string package-name))) ".lisp"))

(defun package-name-compatible-p (declared-pkg expected-pkg)
  "Check if declared package name is compatible with path-derived name.
   Returns T if the final segment of declared-pkg matches the end of expected-pkg,
   or if the path-derived name is 'main' (conventional module entry point).

   Examples:
     (package-name-compatible-p \"epsilon.audio.tests\" \"audio-tests\") => T
     (package-name-compatible-p \"foo.bar\" \"bar\") => T
     (package-name-compatible-p \"foo.bar\" \"baz\") => NIL
     (package-name-compatible-p \"epsilon.server\" \"main\") => T"
  (let* ((declared (string-upcase (string declared-pkg)))
         (expected (string-upcase (string expected-pkg)))
         ;; Get last segment of declared package (after final dot)
         (dot-pos (position #\. declared :from-end t))
         (final-segment (if dot-pos
                          (subseq declared (1+ dot-pos))
                          declared)))
    (or
      (string-equal declared expected)
      ;; Check if expected ends with the final segment (with optional hyphen prefix)
      (str:ends-with-p expected final-segment)
      ;; main.lisp is a conventional module entry point that may declare
      ;; the parent module package rather than a .main sub-package
      (string-equal expected "MAIN"))))

(defun validate-package-path (declared-package filepath src-root)
  "Check if declared package matches path-derived package.

   Returns two values:
   1. T if they match, NIL otherwise
   2. The expected package name (derived from path)

   Example:
     (validate-package-path \"epsilon.http.client\"
                            \"src/epsilon/http/client.lisp\"
                            \"src/\")
     => T, \"epsilon.http.client\""
  (let ((expected (derive-package-from-path filepath src-root)))
    (values (string-equal (string declared-package) expected) expected)))

;;; ---------------------------------------------------------------------------
;;; Symbol Visibility
;;; ---------------------------------------------------------------------------
(defun public-symbol-p (symbol)
  "Return T if SYMBOL is public (not prefixed with % or -).

   Public symbols are those whose names do not start with:
   - % (internal/private convention)
   - - (private convention)

   Examples:
     (public-symbol-p 'foo)      => T
     (public-symbol-p '%internal) => NIL
     (public-symbol-p '-private) => NIL"
  (let ((name (symbol-name symbol)))
    (and (> (length name) 0) (not (char= (char name 0) #\%)) (not (char= (char name 0) #\-)))))

(defun symbol-bound-p (symbol)
  "Return T if SYMBOL has any binding (function, macro, variable, class, typeclass, etc.)."
  (or (fboundp symbol)
      (boundp symbol)
      (find-class symbol nil)
      (macro-function symbol)
      (special-operator-p symbol)
      (get symbol :typeclass-p)))

(defun collect-public-symbols (package)
  "Collect all public, bound symbols from PACKAGE.

   Returns a list of symbols that:
   1. Are interned in PACKAGE (not inherited)
   2. Have a public name (not prefixed with % or -)
   3. Are bound (have a function, variable, class, or macro binding)"
  (let ((pkg (find-package package))
        (symbols nil))
    (when pkg
      (do-symbols (sym pkg)
                  (when (and
                    (eq (symbol-package sym) pkg) ; Interned here, not inherited
                    (public-symbol-p sym)
                    (symbol-bound-p sym))
                    (push sym symbols))))
    symbols))

(defun auto-export-public-symbols (package)
  "Export all public, bound symbols from PACKAGE.

   This implements the convention that symbols not prefixed with % or -
   are automatically exported when they have bindings."
  (let ((pkg (find-package package)))
    (when pkg
      (let ((public-symbols (collect-public-symbols pkg)))
        (when public-symbols
          (export public-symbols pkg))
        public-symbols))))

(defun sync-package-exports (package &key explicit-exports)
  "Synchronize package exports with currently bound public symbols.

   This:
   1. Unexports symbols that are no longer bound (only if they're from this package)
   2. Exports new public symbols that are now bound

   When EXPLICIT-EXPORTS is provided (a list of symbol name strings),
   only those symbols are exported and auto-export is disabled.
   This allows modules to explicitly control their public API.

   Note: Imported symbols (from other packages) that were explicitly exported
   are preserved and not unexported. This supports re-export patterns.

   Use this after reloading a .lisp file to ensure exports match reality."
  (let ((pkg (find-package package)))
    (when pkg
      ;; Determine what should be exported
      (let ((should-export (if explicit-exports
                             ;; Explicit exports: intern and collect only specified symbols
                             (loop for name in explicit-exports
                                   for sym = (find-symbol (string-upcase name) pkg)
                                   when (and sym (symbol-bound-p sym))
                                   collect sym)
                             ;; Auto-export: collect all public bound symbols
                             (collect-public-symbols pkg)))
            (currently-exported nil))
        ;; Collect currently exported symbols
        (do-external-symbols (sym pkg) (push sym currently-exported))
        ;; Unexport symbols that shouldn't be exported anymore
        ;; BUT preserve imported symbols - they were explicitly exported via reexport
        (dolist (sym currently-exported)
          (unless (or
            (member sym should-export)
            ;; Don't unexport symbols from other packages (they were re-exported)
            (not (eq (symbol-package sym) pkg)))
            (unexport sym pkg)))
        ;; Export symbols that should be exported
        (dolist (sym should-export)
          (unless (member sym currently-exported)
            (export sym pkg)))
        should-export))))

;;; ---------------------------------------------------------------------------
;;; Form Analysis
;;; ---------------------------------------------------------------------------
(defun find-package-form (forms)
  "Find defpackage form in FORMS.
   Returns the first defpackage form found, or NIL."
  (find-if (lambda (form)
             (and (consp form) (string= "DEFPACKAGE" (symbol-name (car form)))))
           forms))

(defun extract-package-name (pkg-form)
  "Extract package name from a defpackage form."
  (when (and (consp pkg-form) (cdr pkg-form))
    (string (cadr pkg-form))))

(defun has-package-form-p (forms)
  "Return T if FORMS contains a package definition."
  (not (null (find-package-form forms))))

;;; ---------------------------------------------------------------------------
;;; Compile and Load API
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Module-Level Data Structures
;;; ---------------------------------------------------------------------------
(defstruct source-info
  "Metadata for a single source file.
   Package name is derived from defpackage form or file path."
  uri ; Full path to .lisp file
  package-name ; Derived from path (e.g., \"epsilon.http.client\")
  declared-name ; Actual declared name from defpackage form
  src-root ; Root directory for derivation
  requires ; List of required package names
  explicit-exports ; List of explicit export names, or NIL for auto-export
)

(defstruct module-project
  "Module/project representation for .lisp modules.
   Parallels loader.lisp's project class but simplified for .lisp files."
  name ; Module name from module.sexp
  path ; Module directory
  src-root ; src/ subdirectory
  sources ; List of source-info
  tests ; List of source-info for tests
  integration-tests ; List of source-info for integration tests
  requires ; Dependencies from module.sexp
  metadata)
; Full module.sexp plist

;;; ---------------------------------------------------------------------------
;;; Module Discovery
;;; ---------------------------------------------------------------------------

;;; Unified Source Info Structure
;;;
;;; This structure and extract-defpackage-info must be defined before
;;; find-all-sources which uses them.
(defstruct unified-source-info
  "Metadata for a source file (.lisp).
   Provides a common interface for dependency extraction."
  uri ; Full path to source file
  package-name ; Primary package name (derived or declared)
  file-type ; :lisp
  requires ; List of required package names
  provides ; List of provided package names
  imports ; Alist of (nickname . package-name)
  exports ; List of explicit exports (or NIL for auto-export)
  use ; List of packages to use
  shadow ; List of symbols to shadow
  header-p)
; T if file has Epsilon header
(defun extract-defpackage-info (filepath)
  "Extract dependency information from a standard defpackage form.
   Returns a unified-source-info structure."
  (let ((forms nil)
        (pkg-name nil)
        (uses nil)
        (imports nil)
        (import-froms nil) ; Packages from :import-from clauses
        (exports nil)
        (shadows nil))
    ;; Read forms from file
    (with-open-file (stream filepath :direction :input)
      (handler-case (loop for form = (read stream nil :eof)
                          until (eq form :eof)
                          do (push form forms)
                          ;; Stop after finding defpackage and in-package
                          when (and (consp form) (member (car form) '(in-package cl:in-package)))
                          do (return))
        (error
         ()
         nil)))
    (setf forms (nreverse forms))
    ;; Find defpackage form
    ;; Note: Check by symbol name because the reader may produce different symbols
    ;; depending on *package* - could be CL:DEFPACKAGE or EPSILON.MAIN:DEFPACKAGE
    ;; (when CL-USER has the shadowing-import of the extended defpackage)
    (let ((defpkg
    (find-if (lambda (form)
               (and (consp form)
                    (symbolp (car form))
                    (string= (symbol-name (car form)) "DEFPACKAGE")))
             forms)))
      (if defpkg
        (progn
          (setf pkg-name (string (second defpkg)))
          ;; Extract options
          (dolist (clause (cddr defpkg))
            (when (consp clause)
              (case (car clause)
                ((:use)
                 (setf uses (mapcar #'string (cdr clause))))
                ((:local-nicknames)
                 (dolist (spec (cdr clause))
                   (when (and (consp spec) (= (length spec) 2))
                     (push (cons (first spec) (second spec)) imports))))
                ((:import)
                 ;; Extended defpackage :import clause: (pkg nick) or pkg
                 ;; Extract package names as dependencies
                 (dolist (spec (cdr clause))
                   (let ((dep-pkg (if (consp spec)
                                    (first spec)
                                    spec))
                         (nickname (when (consp spec)
                                     (second spec))))
                     (if nickname
                       ;; With nickname: add to imports (like local-nicknames)
                       (push (cons nickname dep-pkg) imports)
                       ;; Without nickname: add to import-froms (just the dependency)
                       (push (string dep-pkg) import-froms)))))
                ((:import-from)
                 ;; First element after :import-from is the package name
                 (when (cdr clause)
                   (push (string (second clause)) import-froms)))
                ((:export)
                 (setf exports (mapcar #'string (cdr clause))))
                ((:shadow)
                 (setf shadows (cdr clause)))))))
        ;; No defpackage - look for in-package form
        (let ((in-pkg (find-if (lambda (form)
                                 (and (consp form) (member (car form) '(in-package cl:in-package))))
                               forms)))
          (when in-pkg
            (setf pkg-name (string (second in-pkg)))))))
    ;; Build unified info
    (make-unified-source-info :uri
                              filepath
                              :package-name
                              pkg-name
                              :file-type
                              :lisp
                              :requires
                              (mapcar #'string
                                      (remove-if (lambda (pkg)
                                                   (member (string-upcase (string pkg))
                                                           '("CL" "COMMON-LISP")
                                                           :test
                                                           #'string=))
                                                 (append uses (mapcar #'cdr imports) import-froms)))
                              :provides
                              (when pkg-name
                                (list pkg-name))
                              :imports
                              (nreverse imports)
                              :exports
                              exports
                              :use
                              uses
                              :shadow
                              shadows
                              :header-p
                              nil)))

(defun find-all-sources (src-dir)
  "Find all source files in SRC-DIR.
   For each file, detects the source type and creates appropriate source-info.
   - Files with epsilon header: Uses header metadata
   - Files with defpackage: Extracts from defpackage form
   Returns a list of source-info structures."
  (let ((sources nil))
    ;; Collect all .lisp files - detect source type from content
    (let ((lisp-files (fs:list-files src-dir "lisp")))
      (dolist (file lisp-files)
        (cond
          ;; Check for epsilon header
          ((parse-epsilon-header file)
           (let* ((header (parse-epsilon-header file))
                  (provides (getf header :provides))
                  (requires (getf header :requires))
                  (exports (getf header :exports))
                  (pkg-name (string (or (first provides) (fs:strip-extension (fs:basename file))))))
             (push (make-source-info :uri
                                     file
                                     :package-name
                                     pkg-name
                                     :declared-name
                                     pkg-name
                                     :src-root
                                     src-dir
                                     :requires
                                     (mapcar #'string requires)
                                     :explicit-exports
                                     (mapcar #'string exports))
                   sources)))
          ;; Extract from defpackage
          (t
           (let ((defpkg-info (extract-defpackage-info file)))
             (when (unified-source-info-package-name defpkg-info)
               (push (make-source-info :uri
                                       file
                                       :package-name
                                       (unified-source-info-package-name defpkg-info)
                                       :declared-name
                                       (unified-source-info-package-name defpkg-info)
                                       :src-root
                                       src-dir
                                       :requires
                                       (unified-source-info-requires defpkg-info)
                                       :explicit-exports
                                       (unified-source-info-exports defpkg-info))
                     sources)))))))
    (nreverse sources)))

(defun load-module-project (module-path)
  "Load any module from MODULE-PATH (directory with module.sexp).
   Returns an module-project structure.

   Collects all source files and detects source type:
   - Files with epsilon header: Uses header metadata
   - Files with defpackage: Extracts from defpackage form

   The :source-type key in module.sexp is ignored (auto-detected from files)."
  (let* ((module-file (path:path-string (path:path-join module-path "module.sexp")))
         (metadata (with-open-file (s module-file) (read s)))
         (name (getf metadata :name))
         (source-dirs (or (getf metadata :sources) '("src")))
         (test-dirs (or (getf metadata :tests) '("tests")))
         (integration-dir (path:path-string (path:path-join module-path "integration"))))
    (flet ((collect-sources (dirs)
                            "Collect all sources from the given directories"
                            (let ((all-sources nil))
                              (dolist (dir dirs)
                                (let ((full-dir (path:path-string (path:path-join module-path dir))))
                                  (when (and (probe-file full-dir) (fs:dir-p full-dir))
                                    (setf all-sources
                                          (append all-sources (find-all-sources full-dir))))))
                              all-sources)))
      (make-module-project :name
                           name
                           :path
                           module-path
                           :src-root
                           (path:path-string (path:path-join module-path (first source-dirs)))
                           :sources
                           (collect-sources source-dirs)
                           :tests
                           (collect-sources test-dirs)
                           :integration-tests
                           (when (and (probe-file integration-dir) (fs:dir-p integration-dir))
                             (find-all-sources integration-dir))
                           :requires
                           (getf metadata :requires)
                           :metadata
                           metadata))))

;;; ---------------------------------------------------------------------------
;;; Module Building
;;; ---------------------------------------------------------------------------
(defun source-build-order (sources)
  "Return SOURCES in topological order based on dependencies.
   Files are sorted so that dependencies are loaded before dependents.
   Returns two values: sorted list and any cyclic dependencies found."
  (let* (;; Map from uri -> source-info
         (nodes (make-hash-table :test 'equal))
         ;; Map from package-name (uppercase) -> source-info that DEFINES the package
         (local-packages (make-hash-table :test 'equalp))
         (uris nil))
    ;; Build lookup tables - all .lisp files define a package
    (dolist (source sources)
      (setf (gethash (source-info-uri source) nodes) source)
      (push (source-info-uri source) uris)
      (let ((pkg-name (source-info-package-name source)))
        (setf (gethash pkg-name local-packages) source)
        ;; Also add entry for declared name if different
        (let ((declared (source-info-declared-name source)))
          (when (and declared (not (string-equal declared pkg-name)))
            (setf (gethash declared local-packages) source)))))
    (setf uris (nreverse uris))
    ;; Topological sort via epsilon.graph
    (flet ((dep-uris (uri)
                     (let ((source (gethash uri nodes)))
                       (when source
                         (loop for pkg in (source-info-requires source)
                               for dep-source = (gethash pkg local-packages)
                               when dep-source
                               collect (source-info-uri dep-source))))))
      (multiple-value-bind (sorted-uris cycles) (graph:topological-sort uris
                                                                        #'dep-uris
                                                                        :on-cycle
                                                                        :collect
                                                                        :test
                                                                        'equal)
        (values (mapcar (lambda (uri)
                          (gethash uri nodes))
                        sorted-uris)
                cycles)))))

(defun fasl-path-for-source (source module-name)
  "Compute the FASL output path for a source-info.
   Structure: _build/{module-name}/{relative-path}.fasl"
  (let* ((uri (source-info-uri source))
         (src-root (source-info-src-root source))
         ;; Get relative path from src root
         (rel-path (if (str:starts-with-p uri src-root)
                     (subseq uri (length src-root))
                     uri))
         ;; Strip leading slash if present
         (clean-rel-path (if (and (> (length rel-path) 0) (char= (char rel-path 0) #\/))
                           (subseq rel-path 1)
                           rel-path))
         ;; Change extension from .lisp to .fasl
         (fasl-rel-path (if (str:ends-with-p clean-rel-path ".lisp")
                          (concatenate 'string
                                       (subseq clean-rel-path 0 (- (length clean-rel-path) 5))
                                       ".fasl")
                          (concatenate 'string clean-rel-path ".fasl")))
         ;; Full target path
         (target-path (path:path-string (path:path-join *build-root* module-name fasl-rel-path))))
    target-path))

;;; ---------------------------------------------------------------------------
;;; Content-Addressable FASL Validation
;;;
;;; Replaces timestamp-based staleness detection with content hashing.
;;; A FASL's validity is determined by BLAKE3 hash of its source content
;;; combined with hashes of all predecessors and upstream module dependencies.
;;; Each local FASL has a .key sidecar containing its content key.
;;; ---------------------------------------------------------------------------
(defvar *content-hashing-p*
  nil
  "Set to T once epsilon.digest.blake3 is loaded and content hashing is available.
   Core modules always compile fresh; content-key validation is used for non-core modules only.")

(defun %blake3-hex (data)
  "Compute BLAKE3 hash of byte array DATA, returning lowercase hex string.
   Requires epsilon.digest.blake3 to be loaded."
  (let ((fn (symbol-function (intern "BLAKE3-HEX" "EPSILON.DIGEST.BLAKE3"))))
    (funcall fn data)))

(defun %blake3-of-string (string)
  "Compute BLAKE3 hash of STRING (encoded as UTF-8 bytes), returning hex string."
  (%blake3-hex (sb-ext:string-to-octets string :external-format :utf-8)))

(defun %module-loaded-p-safely (info)
  (handler-case (module-loaded-p info)
    (error () nil)))

(defun %location->string (location)
  (cond ((stringp location) location)
        ((pathnamep location) (namestring location))
        (t (path:path-string location))))

(defun %rehydrate-one-module-hash (info)
  "Set INFO's MODULE-CONTENT-HASH if it has none, deriving it from
   on-disk source bytes plus any already-populated dep hashes.
   Returns T if this call set the hash, NIL otherwise (already set,
   no sources on disk, or a dep hash is still missing)."
  (when (and (%module-loaded-p-safely info)
             (null (module-content-hash info)))
    (handler-case
        (let* ((proj (load-module-project
                      (%location->string (module-location info))))
               (sources (module-project-sources proj))
               (source-keys (loop for s in sources
                                  for uri = (source-info-uri s)
                                  when (probe-file uri)
                                    collect (compute-source-content-hash uri)))
               (dep-names (module-project-requires proj))
               (dep-hashes (loop for dep-name in dep-names
                                 for dep = (get-module dep-name)
                                 when (and dep (module-content-hash dep))
                                   collect (module-content-hash dep))))
          (when source-keys
            (setf (module-content-hash info)
                  (compute-module-hash (sort source-keys #'string<)
                                       (sort (copy-list dep-hashes) #'string<)))
            t))
      (error () nil))))

(defun rehydrate-loaded-module-hashes ()
  "Walk every loaded module and populate its MODULE-CONTENT-HASH from
   source bytes + dep hashes.  Backfills modules that were built before
   *content-hashing-p* turned on (epsilon.scheduler, epsilon.foreign,
   etc., which load ahead of epsilon.crypto).  Idempotent and order-
   tolerant: runs a fixpoint over QUERY-MODULES so a module whose
   deps weren't yet hashed picks up its hash on a later pass."
  (when *content-hashing-p*
    (loop with max-passes = 8
          for pass from 1 to max-passes
          for modules = (query-modules :loaded-only t)
          for changed = nil
          do (dolist (info modules)
               (when (%rehydrate-one-module-hash info)
                 (setf changed t)))
             (unless changed (return)))))

(defun enable-content-hashing ()
  "Enable content-addressed validation once BLAKE3 is available.
   Called after crypto module loads.  Also backfills MODULE-CONTENT-HASH
   for any module that already loaded in legacy mode -- otherwise the
   test-result cache (and any other consumer of MODULE-CONTENT-HASH)
   never sees a stable identity for those modules."
  (when (find-package "EPSILON.DIGEST.BLAKE3")
    (setf *content-hashing-p* t)
    (rehydrate-loaded-module-hashes)))

(defvar *compiler-enhancement-p* nil
  "Set to T once epsilon.compiler loads and installs the CST compilation hook.
   After this, all compile-file-safely calls get byte-accurate source tracking.
   Also serves as a re-entrancy guard during the loading attempt.")

(defun ensure-compiler-enhancement ()
  "Load epsilon.compiler to install CST source tracking, if available.
   Called during module loading, after dependencies are resolved.
   The compiler module sets compile:*compile-file-around* on load,
   giving all subsequent compilations byte-accurate sub-expression positions."
  (when (and (not *compiler-enhancement-p*)
             (get-module "epsilon.compiler"))
    ;; Set the flag immediately to prevent re-entrance during
    ;; the dependency chain: epsilon.compiler -> epsilon.reader -> ...
    (setf *compiler-enhancement-p* t)
    (handler-case (load-module "epsilon.compiler")
      (error ()
        ;; Reset on failure so we can retry later
        (setf *compiler-enhancement-p* nil)))))

(defun compute-source-content-hash (source-path)
  "Compute BLAKE3 hash of a source file's bytes. Returns hex string."
  (let ((bytes (with-open-file (s source-path :direction :input :element-type '(unsigned-byte 8))
                 (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
                   (read-sequence buf s)
                   buf))))
    (%blake3-hex bytes)))

;;; ---------------------------------------------------------------------------
;;; Source reading and reference extraction
;;; ---------------------------------------------------------------------------
;;;
;;; The content-key derivation needs to know which other packages a file
;;; references, so that the file's key can pick up upstream module-hash
;;; changes. The shape extraction that an earlier design also threaded
;;; through here has been removed; only references survive.

(defun read-source-forms-with-package-context (source-uri)
  "Read SOURCE-URI's top-level forms. Eagerly EVAL each `defpackage` and
   `in-package` form as it is read so subsequent reads bind the right
   *package* and symbols intern with the correct identity. Side effects
   on the running image are limited to creating/updating packages the
   file would create anyway when compiled normally.
   Returns the list of read forms (in order); errors during read or
   package-effect evaluation are caught -- extraction is best-effort.
   Returns NIL if SOURCE-URI is missing (read-only deploys may ship
   FASLs without their sources)."
  (unless (probe-file source-uri)
    (return-from read-source-forms-with-package-context nil))
  (let ((forms '())
        (saved-package *package*))
    (unwind-protect
         (handler-case
             (with-open-file (stream source-uri :direction :input)
               (let ((*read-eval* nil))
                 (loop
                   (let ((form (handler-case (read stream nil :eof)
                                 (error () :read-error))))
                     (when (or (eq form :eof) (eq form :read-error))
                       (return))
                     (push form forms)
                     (when (and (consp form)
                                (symbolp (car form))
                                (let ((n (symbol-name (car form))))
                                  (or (string= n "DEFPACKAGE")
                                      (string= n "IN-PACKAGE"))))
                       (handler-case (eval form) (error () nil)))))))
           (file-error () nil))
      (setf *package* saved-package))
    (nreverse forms)))

(defun head-name (form)
  (and (consp form) (symbolp (car form)) (symbol-name (car form))))

(defun extract-references (forms own-package-name)
  "Walk FORMS, collect (PACKAGE-NAME . SYMBOL-NAME) pairs for every symbol
   whose package is neither the file's own package, nor the COMMON-LISP /
   COMMON-LISP-USER / KEYWORD packages (treated as immutable / metadata).
   Defpackage and in-package forms are skipped: their constituent package
   names get read in CL-USER and would otherwise show up as spurious
   references; the file's own defpackage shape is captured separately."
  (let ((own (and own-package-name (string-upcase (string own-package-name))))
        (refs (make-hash-table :test 'equal)))
    (labels ((visit (node)
               (cond
                 ((symbolp node)
                  (let ((pkg (symbol-package node)))
                    (when pkg
                      (let ((pname (package-name pkg)))
                        (unless (or (null pname)
                                    (string= pname "COMMON-LISP")
                                    (string= pname "COMMON-LISP-USER")
                                    (string= pname "KEYWORD")
                                    (and own (string= pname own)))
                          (setf (gethash (cons (string-upcase pname)
                                               (string-upcase (symbol-name node)))
                                         refs)
                                t))))))
                 ((consp node)
                  (visit (car node))
                  (visit (cdr node))))))
      (dolist (form forms)
        (let ((head (head-name form)))
          (unless (and head
                       (or (string= head "DEFPACKAGE")
                           (string= head "IN-PACKAGE")))
            (visit form)))))
    (let ((out '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k out)) refs)
      (sort out (lambda (a b)
                  (or (string< (car a) (car b))
                      (and (string= (car a) (car b))
                           (string< (cdr a) (cdr b)))))))))

(defun extract-source-abi (source-uri &key own-package-name-hint)
  "Read SOURCE-URI once with package context, derive the cross-package
   references, and return (values refs effective-own-package-name).
   OWN-PACKAGE-NAME-HINT short-circuits package discovery when the caller
   has already extracted defpackage info."
  (let* ((forms (read-source-forms-with-package-context source-uri))
         (own (or own-package-name-hint
                  (loop for f in forms
                        when (and (consp f)
                                  (symbolp (car f))
                                  (string= (symbol-name (car f)) "IN-PACKAGE")
                                  (consp (cdr f)))
                        return (string (second f))
                        finally (return nil))
                  (loop for f in forms
                        when (and (consp f)
                                  (symbolp (car f))
                                  (string= (symbol-name (car f)) "DEFPACKAGE")
                                  (consp (cdr f)))
                        return (string (second f))
                        finally (return nil))))
         (refs (extract-references forms own)))
    (values refs own)))

(defvar *rebuild-reasons* (make-hash-table :test 'equal)
  "Map source-uri (string) -> list of reasons for the most recent rebuild.
   Populated by record-rebuild-reason during a cache miss; queried by
   why-recompiled?.")

(defun record-rebuild-reason (source-uri reasons)
  (setf (gethash (namestring source-uri) *rebuild-reasons*) reasons))

(defun diagnose-cache-miss (fasl-path source-hash refs predecessor-keys
                            fallback-by-package)
  "Compare the previous sidecar's recorded refs to the current refs and
   return a list of reasons the cache key no longer matches.
   Each reason is one of:
     (:added-ref pkg . sym)
     (:removed-ref pkg . sym)
     (:no-prior-sidecar)
     (:source-bytes-changed-or-predecessor-changed ...)"
  (declare (ignore fallback-by-package))
  (let ((stored (read-key-sidecar fasl-path)))
    (cond
      ((not (consp stored)) (list (list :no-prior-sidecar)))
      (t
       (let* ((old-refs (getf stored :symbol-deps))
              (reasons '()))
         (when old-refs
           (let ((added (set-difference refs old-refs :test #'equal))
                 (removed (set-difference old-refs refs :test #'equal)))
             (dolist (r added) (push (cons :added-ref r) reasons))
             (dolist (r removed) (push (cons :removed-ref r) reasons))))
         (when (null reasons)
           (push (list :source-bytes-changed-or-predecessor-changed
                       :source-hash source-hash
                       :predecessor-count (length predecessor-keys))
                 reasons))
         (nreverse reasons))))))

(defun why-recompiled? (source-uri)
  "Return the list of reasons recorded for SOURCE-URI's most recent
   rebuild, or NIL if no cache miss has been observed for it in this
   session. Reasons are produced by diagnose-cache-miss."
  (gethash (namestring source-uri) *rebuild-reasons*))

(defun compute-content-key-v2 (source-hash predecessor-keys symbol-deps fallback-by-package)
  "Content key: source bytes + sorted predecessor keys + sorted module
   hashes of every owning module that this file references. The
   per-symbol shape-hash path was removed: the precision it bought was
   not worth its overhead in measurement, and the simpler module-hash
   propagation matches Bazel's content-addressed model.
   SYMBOL-DEPS is still a list of (PACKAGE . SYMBOL) pairs; we collapse
   it to the set of distinct owning modules via FALLBACK-BY-PACKAGE
   (upcase package-name -> module-hash). Tagged `epsilon-fasl-v3` so
   prior v1/v2 sidecars naturally invalidate."
  (let* ((sorted-preds (sort (copy-list predecessor-keys) #'string<))
         (dep-module-hashes
           (let ((seen (make-hash-table :test 'equal))
                 (out '()))
             (dolist (pair symbol-deps)
               (let* ((pkg (car pair))
                      (mh (and fallback-by-package
                               (gethash (string-upcase pkg) fallback-by-package))))
                 (when (and mh (not (gethash mh seen)))
                   (setf (gethash mh seen) t)
                   (push mh out))))
             (sort out #'string<)))
         (parts (list* "epsilon-fasl-v3"
                       (lisp-implementation-version)
                       (format nil "fasl-version:~A" (current-fasl-file-version))
                       source-hash
                       (append sorted-preds dep-module-hashes)))
         (combined (format nil "~{~A~^|~}" parts)))
    (%blake3-of-string combined)))

(defun compute-module-hash (content-keys dep-module-hashes)
  "Compute the module hash from all content keys and dep module hashes.
   Returns hex string."
  (let* ((parts (list* "epsilon-module-v1"
                       (lisp-implementation-version)
                       (append content-keys dep-module-hashes)))
         (combined (format nil "~{~A~^|~}" parts)))
    (%blake3-of-string combined)))

(defun key-sidecar-path (fasl-path)
  "Return the .key sidecar file path for a FASL."
  (concatenate 'string fasl-path ".key"))

(defun current-fasl-file-version ()
  "The integer FASL format version this SBCL emits and accepts.
   Distinct from (lisp-implementation-version) — a custom SBCL build
   sharing a version string can still emit incompatible FASLs."
  sb-fasl::+fasl-file-version+)

(defun write-key-sidecar (fasl-path content-key &key own-package symbol-deps)
  "Write the content key, FASL format version, and (optionally) the
   file's own-package name and cross-package references to the .key
   sidecar. SYMBOL-DEPS is retained for diagnostic use by
   why-recompiled?: it lets a later cache-miss compare ref sets and
   attribute the miss to added/removed references."
  (let ((key-path (key-sidecar-path fasl-path)))
    (with-open-file (s key-path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (with-standard-io-syntax
        ;; Strings must round-trip as strings, not as #A(...) base-char
        ;; arrays; turn off *print-readably* to get that behaviour.
        (let ((*print-readably* nil)
              (*print-pretty* nil))
          (prin1 (append
                  (list :content-key content-key
                        :fasl-version (current-fasl-file-version))
                  (when own-package
                    (list :own-package own-package))
                  (when symbol-deps
                    (list :symbol-deps symbol-deps)))
                 s))))))

(defun read-key-sidecar (fasl-path)
  "Read the .key sidecar plist. Returns the plist or NIL on any read failure
   (missing file, legacy hex-only format, corruption -- all force recompile).
   Only attempts READ when the file starts with `(', so legacy 64-char hex
   sidecars are rejected without polluting any package with stray symbols."
  (let ((key-path (key-sidecar-path fasl-path)))
    (when (probe-file key-path)
      (handler-case
          (with-open-file (s key-path :direction :input)
            (let ((first (peek-char nil s nil nil)))
              (when (eql first #\()
                (with-standard-io-syntax
                  (let ((*read-eval* nil)
                        (*package* (find-package :keyword)))
                    (read s nil nil))))))
        (error () nil)))))

(defun local-fasl-valid-p (fasl-path content-key)
  "A local FASL is valid only if its sidecar agrees on both the content key
   and the FASL format version. The version check guards against the
   load-time crash that occurs when SBCL meets an incompatible FASL."
  (and (probe-file fasl-path)
       (let ((stored (read-key-sidecar fasl-path)))
         (and (consp stored)
              (equal (getf stored :content-key) content-key)
              (eql (getf stored :fasl-version) (current-fasl-file-version))))))

(defvar *stale-fasl-recovery-active-p* nil
  "Re-entrance guard for load-fasl-with-recovery: once we've recompiled
   and the rebuilt FASL still won't load, the failure is real -- propagate
   it instead of looping.")

(defun fasl-load-failure-p (condition)
  "True when CONDITION looks like SBCL rejecting a FASL file itself
   (bad magic, version mismatch, truncated header) rather than an error
   raised by code inside a successfully-loaded FASL.
   Pattern-matches on condition class package + name to avoid naming
   sb-fasl internals directly (they move between SBCL releases)."
  (let* ((cname (class-name (class-of condition)))
         (pkg (and cname (symbol-package cname))))
    (and pkg
         (member (package-name pkg) '("SB-FASL" "SB-INT" "SB-KERNEL")
                 :test #'string=)
         (let ((n (symbol-name cname)))
           (or (search "FASL" n)
               (search "INVALID-FASL" n))))))

(defun load-fasl-with-recovery (fasl-path compile-thunk)
  "LOAD a cached FASL, recovering once if SBCL rejects the file format.
   On a FASL-format failure: delete the FASL and its sidecar, run
   COMPILE-THUNK to rebuild, then retry LOAD exactly once.
   Safety net behind the sidecar version check -- sidecar is the fast
   path, this catches every other unloadable-FASL case (manual copy,
   partial write, custom SBCL build sharing version string)."
  (handler-case (load fasl-path)
    (error (c)
      (cond
        (*stale-fasl-recovery-active-p* (error c))
        ((fasl-load-failure-p c)
         (log:warn "Stale FASL ~A: ~A -- recompiling" fasl-path c)
         (when (probe-file fasl-path) (delete-file fasl-path))
         (let ((key-path (key-sidecar-path fasl-path)))
           (when (probe-file key-path) (delete-file key-path)))
         (funcall compile-thunk)
         (let ((*stale-fasl-recovery-active-p* t))
           (load fasl-path)))
        (t (error c))))))

(defun %dir-writable-p (path)
  "Return T iff the parent directory of PATH is writable by the current
process.  Used by build-lisp-source to detect read-only deployments
(e.g. FASLs assembled by mkEpsilonPackage live under /nix/store) and
take the trust-the-pre-built-FASL path instead of attempting to
recompile, which would fail at the very first .lock-file write."
  (let* ((parent (directory-namestring path))
         (probe-name (format nil "~A.epsilon-write-probe-~A-~A"
                             parent (sb-posix:getpid) (random most-positive-fixnum))))
    (handler-case
        (with-open-file (s probe-name
                           :direction :output
                           :if-exists :error
                           :if-does-not-exist :create)
          (declare (ignorable s))
          (ignore-errors (delete-file probe-name))
          t)
      (error () nil))))

(defun build-lisp-source (source
  module-name
  &key
  verbose
  force
  predecessor-keys
  fallback-by-package
  ;; Legacy timestamp parameter - ignored when content hashing is active
  deps-newest)
  "Compile and load a .lisp file with epsilon-module header.

   In content-hashing mode (the normal post-bootstrap path) this drives
   the cache key:
     1. Read forms once with package context (defpackage and in-package
        evaluated eagerly so symbol identity is correct).
     2. Extract cross-package symbol references.
     3. Resolve each reference's owning package to its module-hash via
        FALLBACK-BY-PACKAGE.
     4. Hash (source-hash, sorted predecessor keys, distinct sorted
        upstream module-hashes, fasl-version) into the content key.

   FALLBACK-BY-PACKAGE is a hash table from upcase package-name to the
   owning module's module-hash. Per-symbol shape extraction is still
   performed (the shape data is written into the sidecar for
   diagnostic use) but no longer participates in the key.

   In legacy/force mode, falls back to timestamp-based staleness.
   Returns (values content-key layout-signature) in CAS mode, T in
   legacy mode."
  (let* ((uri (source-info-uri source))
         (pkg-name (source-info-package-name source))
         (explicit-exports (source-info-explicit-exports source))
         (fasl-path (fasl-path-for-source source module-name)))
    (when verbose
      (format t "~&; Building ~A (lisp)~%" pkg-name))
    ;; Ensure target directory exists
    (fs:make-dirs (path:path-parent (path:make-path fasl-path)))
    (cond
      ;; Read-only deployment shortcut: take the trust path before any
      ;; computation that would need source files (a deploy may ship
      ;; only the FASL+sidecar, no .lisp).
      ((and *content-hashing-p*
            (not force)
            (probe-file fasl-path)
            (not (%dir-writable-p fasl-path))
            (not (probe-file uri)))
       (when verbose
         (format t "~&;   ~A: read-only deploy without source, trusting fasl~%" pkg-name))
       (load fasl-path)
       (unless explicit-exports
         (auto-export-public-symbols (string-upcase pkg-name)))
       (let ((stored (read-key-sidecar fasl-path)))
         (when (consp stored)
           (return-from build-lisp-source
             (values (getf stored :content-key)
                     (getf stored :layout-signature))))))
      (t nil))
    (if (and *content-hashing-p* (not force))
      ;; Content-hashing mode: content-addressable validation
      (multiple-value-bind (refs effective-pkg)
          (extract-source-abi uri :own-package-name-hint pkg-name)
        (let* ((source-hash (compute-source-content-hash uri))
               (content-key (compute-content-key-v2 source-hash
                                                    predecessor-keys
                                                    refs
                                                    fallback-by-package))
               (sidecar-fields
                 (list :own-package (and effective-pkg (string effective-pkg))
                       :symbol-deps refs))
               (compile-thunk
                 (lambda ()
                   (multiple-value-bind (output-path warnings-p failure-p)
                       (compile:compile-file-safely uri :output-file fasl-path
                                                        :verbose nil :print nil)
                     (declare (ignore output-path warnings-p))
                     (when failure-p (error "Compilation failed for ~A" uri)))
                   (apply #'write-key-sidecar fasl-path content-key sidecar-fields))))
          ;; Check local FASL validity via .key sidecar
          (cond
            ((local-fasl-valid-p fasl-path content-key)
             (when verbose
               (format t "~&;   ~A: local fasl valid~%" pkg-name))
             (load-fasl-with-recovery fasl-path compile-thunk))
            ;; Read-only deployment (Nix store, baked container image).
            ;; If the pre-built FASL exists we have to trust it: there is
            ;; nowhere we can write a .lock or temp-fasl to do a recompile,
            ;; and recompilation would mean the build itself was buggy --
            ;; better to surface that as a load failure than to mask it
            ;; with a doomed compile attempt. Skip recovery here: it would
            ;; try to delete the FASL, which read-only fs rejects.
            ((and (probe-file fasl-path) (not (%dir-writable-p fasl-path)))
             (when verbose
               (format t "~&;   ~A: read-only deploy, trusting pre-built fasl~%" pkg-name))
             (load fasl-path))
            (t
             (when verbose
               (format t "~&;   ~A: compiling~%" pkg-name))
             ;; Diagnose why we're rebuilding before the new sidecar
             ;; clobbers the previous one's snapshot.
             (record-rebuild-reason
              uri
              (diagnose-cache-miss fasl-path source-hash refs
                                   predecessor-keys fallback-by-package))
             (funcall compile-thunk)
             (load fasl-path)))
          ;; Auto-export
          (unless explicit-exports
            (auto-export-public-symbols (string-upcase pkg-name)))
          content-key))
      ;; Legacy/force mode: timestamp-based staleness
      (progn
        (let* ((fasl-time (and (probe-file fasl-path) (file-write-date fasl-path)))
               (needs-compile (or force
                                  (not fasl-time)
                                  (and (probe-file uri)
                                       fasl-time
                                       (> (file-write-date uri) fasl-time))
                                  (and deps-newest fasl-time (> deps-newest fasl-time)))))
          (when needs-compile
            (multiple-value-bind (output-path warnings-p failure-p) (compile:compile-file-safely uri
                                                                                                 :output-file
                                                                                                 fasl-path
                                                                                                 :verbose
                                                                                                 nil
                                                                                                 :print
                                                                                                 nil)
              (declare (ignore output-path warnings-p))
              (when failure-p
                (error "Compilation failed for ~A" uri))))
          (load fasl-path))
        (unless explicit-exports
          (auto-export-public-symbols (string-upcase pkg-name)))
        ;; In legacy mode, write a content-key sidecar once hashing
        ;; becomes available (typically after crypto loads mid-build).
        ;; The sidecar is the v2 shape, but with no manifest data
        ;; available yet -- subsequent passes will fill it in.
        (when *content-hashing-p*
          (let* ((source-hash (compute-source-content-hash uri))
                 (refs nil)
                 (content-key (compute-content-key-v2 source-hash predecessor-keys
                                                      refs nil)))
            (write-key-sidecar fasl-path content-key)
            (return-from build-lisp-source content-key)))
        t))))

(defun build-dir-for-module (module-name)
  "Return the build output directory for MODULE-NAME as a string with
   trailing slash, so SBCL's pathname parser treats it as a directory
   rather than a file (important for names with dots like epsilon.crypto)."
  (let ((dir (path:path-string (path:path-join *build-root* module-name))))
    (if (str:ends-with-p dir "/")
      dir
      (concatenate 'string dir "/"))))

(defun clean-orphaned-fasls (project)
  "Delete fasls that have no corresponding source file.
   Returns T if any orphans were removed (callers should force-recompile)."
  (let* ((module-name (module-project-name project))
         (fasl-dir (build-dir-for-module module-name))
         (source-fasls (make-hash-table :test 'equal))
         (orphans-found nil))
    ;; Build set of expected fasl paths from all sources (src + tests + integration)
    (dolist (source (append (module-project-sources project)
                            (module-project-tests project)
                            (module-project-integration-tests project)))
      (setf (gethash (fasl-path-for-source source module-name) source-fasls) t))
    ;; Scan for actual fasls and remove orphans. Skip transient temp files
    ;; written by epsilon.compile:compile-file-safely (named .tmp-PID-...) --
    ;; another concurrent compiler is mid-rename and deleting them would
    ;; race the rename and break parallel builds.
    (when (probe-file fasl-dir)
      (labels ((scan (dir)
                     (dolist (entry (directory (merge-pathnames (make-pathname :name
                                                                               :wild
                                                                               :type
                                                                               :wild)
                                                                (pathname dir))))
                       (cond
                         ((and (pathname-name entry)
                               (string-equal (pathname-type entry) "fasl")
                               (not (str:starts-with-p (pathname-name entry) ".tmp-")))
                          (let ((fasl-path (namestring entry)))
                            (unless (gethash fasl-path source-fasls)
                              (log:debug "Removing orphaned fasl: ~A" fasl-path)
                              ;; Tolerate races: another concurrent loader
                              ;; (or another build-worker process) may have
                              ;; already removed this entry. Treat ENOENT
                              ;; as success.
                              (when (handler-case
                                        (progn (delete-file entry) t)
                                      (file-error () nil))
                                (let ((key-path (key-sidecar-path fasl-path)))
                                  (when (probe-file key-path)
                                    (handler-case (delete-file key-path)
                                      (file-error () nil))))
                                (setf orphans-found t)))))
                         ((and (not (pathname-name entry)) (not (equal entry (pathname dir))))
                          (scan (namestring entry)))))))
        (scan fasl-dir)))
    orphans-found))

(defun build-module-project (project
  &key
  verbose
  force
  dep-module-hashes
  fallback-by-package
  ; Legacy parameter - passed through when CAS unavailable
  deps-newest)
  "Build all sources in PROJECT in dependency order.
   When CAS is available, content-key v2 keys each source on its
   referenced symbol shapes; FALLBACK-BY-PACKAGE is the unresolved-ref
   bucket. Returns the module hash (CAS mode) or the newest source
   timestamp (legacy mode).
   Detects and removes orphaned fasls, forcing recompile when found.
   Acquires *compile-lock* to serialize compilation across threads."
  (epsilon.sys.lock:with-recursive-lock-held (*compile-lock*)
    ;; Clean orphaned fasls first; if any found, force full recompile
    (when (clean-orphaned-fasls project)
      (log:info "Orphaned fasls found in ~A, forcing recompile" (module-project-name project))
      (setf force t))
    (multiple-value-bind (ordered cycles) (source-build-order (module-project-sources project))
      (when cycles
        (error "Cyclic dependencies detected in ~A: ~A" (module-project-name project) cycles))
      (let ((module-name (module-project-name project))
            (predecessor-keys '())
            (all-content-keys '())
            (newest 0))
        (dolist (source ordered)
          ;; Track timestamps for legacy mode
          (let ((ts (and (probe-file (source-info-uri source))
                         (file-write-date (source-info-uri source)))))
            (when (and ts (> ts newest))
              (setf newest ts)))
          (let ((result (build-lisp-source source
                                           module-name
                                           :verbose verbose
                                           :force force
                                           :predecessor-keys (reverse predecessor-keys)
                                           :fallback-by-package fallback-by-package
                                           :deps-newest deps-newest)))
            (when (stringp result)
              (push result predecessor-keys)
              (push result all-content-keys))))
        (if (and *content-hashing-p* all-content-keys)
          (compute-module-hash (nreverse all-content-keys) dep-module-hashes)
          newest)))))

;;; ---------------------------------------------------------------------------
;;; Module Loading API
;;; ---------------------------------------------------------------------------
(defun %ensure-cl-systems (project)
  "If PROJECT's module.sexp declares :cl-systems, load epsilon.pkg's
   ASDF bridge and stage every locked system so the module's defpackage
   forms can :use or :import-from those packages.

   No-op when :cl-systems is missing or empty, so modules that don't
   opt in never trigger the bridge or its dependencies. The current
   module epsilon.pkg has no :cl-systems, which prevents recursion when
   we recursively load it here."
  (let ((systems (getf (module-project-metadata project) :cl-systems)))
    (when systems
      (let ((bridge-package (find-package "EPSILON.PKG.ASDF-BRIDGE")))
        (unless bridge-package
          (load-module "epsilon.pkg")
          (setf bridge-package (find-package "EPSILON.PKG.ASDF-BRIDGE"))
          (unless bridge-package
            (error ":cl-systems declared but epsilon.pkg.asdf-bridge is unavailable"))))
      (let ((entry (find-symbol "ENSURE-CL-SYSTEMS" "EPSILON.PKG.ASDF-BRIDGE")))
        (unless (and entry (fboundp entry))
          (error ":cl-systems declared but ENSURE-CL-SYSTEMS is missing"))
        (funcall entry systems)))))

(defun %load-module-impl (package &key force verbose)
  "Internal: load a module and its dependencies.
   Collects content hashes from dependencies for CAS validation.
   Signals package-error if stale fasls reference non-existent packages."
  ;; Skip if this module is currently being built (re-entrant load from
  ;; defpackage :import during compilation)
  (when (and *building-module* (string-equal *building-module* package))
    (return-from %load-module-impl t))
  (let ((module-info (get-module package)))
    ;; Check if module is already loaded
    (when (and module-info (module-loaded-p module-info) (not force))
      (log:debug "Module ~A already loaded, skipping" package)
      (return-from %load-module-impl t))
    ;; Get module location
    (unless module-info
      (error "Unknown module: ~A" package))
    (let* ((location (module-location module-info))
           (module-path (if (pathnamep location)
                          (namestring location)
                          (path:path-string location)))
           (project (load-module-project module-path)))
      ;; Load dependencies, collecting both timestamps (legacy) and content hashes (CAS)
      (let ((deps-newest 0)
            (dep-module-hashes '())
            (fallback-by-package (make-hash-table :test 'equal)))
        (dolist (dep-name (module-project-requires project))
          (let ((resolved-dep (find-module :provides dep-name)))
            (when resolved-dep
              (unless (module-loaded-p resolved-dep)
                (load-module (module-name resolved-dep) :force force))
              ;; Collect legacy timestamp
              (let ((dep-ts (module-source-newest resolved-dep)))
                (when (> dep-ts deps-newest)
                  (setf deps-newest dep-ts)))
              ;; Collect content hash for CAS
              (let ((dep-hash (module-content-hash resolved-dep)))
                (when dep-hash
                  (push dep-hash dep-module-hashes)
                  ;; Map every package this dep provides to its module-hash,
                  ;; for the v2 manifest-miss fallback bucket.
                  (let* ((md (module-metadata resolved-dep))
                         (provides (and md (getf md :provides))))
                    (dolist (provided-pkg (or provides (list dep-name)))
                      (setf (gethash (string-upcase (string provided-pkg))
                                     fallback-by-package)
                            dep-hash))))))))
        ;; Enable CAS after crypto module loads (it provides BLAKE3)
        (unless *content-hashing-p*
          (enable-content-hashing))
        ;; Enable CST-based compilation after epsilon.compiler loads
        (ensure-compiler-enhancement)
        ;; Stage external ASDF systems declared in :cl-systems before
        ;; the module's own sources are read. The bridge lives in
        ;; epsilon.pkg; it is loaded on demand so modules that don't opt
        ;; in pay nothing.
        (%ensure-cl-systems project)
        ;; Build and load all sources
        (log:debug "Building module ~A" package)
        (let* ((*current-module-name* package)
               (*building-module* package)
               (build-result (build-module-project
                              project
                              :verbose verbose
                              :force force
                              :dep-module-hashes (nreverse dep-module-hashes)
                              :fallback-by-package fallback-by-package
                              :deps-newest (when (> deps-newest 0) deps-newest))))
          (mark-module-loaded package)
          (cond
            ((stringp build-result)
             (setf (module-content-hash module-info) build-result))
            (t
             (setf (module-source-newest module-info)
                   (max build-result deps-newest))))
          t)))))

(defun load-module (package &key force compile-only verbose)
  "Ensure a package is available in the environment.
   Compiles if necessary, then loads unless compile-only is true.

   PACKAGE - Package name to load (e.g., 'epsilon', 'http')
   FORCE - Force compilation of all build steps regardless of timestamps
   COMPILE-ONLY - Only compile, don't load (not yet supported for unified loading)
   VERBOSE - Print progress information

   Returns T if successfully loaded/compiled, NIL otherwise."
  (declare (ignore compile-only)) ; TODO: implement compile-only for unified loading
  (%load-module-impl package :force force :verbose verbose))

(defun load-module-resources (module-name resource-type &key force verbose)
  "Load resources of a specific type for a module.
   MODULE-NAME - Module name (e.g., 'epsilon')
   RESOURCE-TYPE - Type of resources (:tests, :benchmarks, :examples, etc.)
   FORCE - Force recompilation even if up-to-date
   VERBOSE - Print progress information

   Returns T if successful."
  (let* ((module-info (get-module module-name))
         (location (when module-info
                     (module-location module-info)))
         (module-path (cond
                        ((null location)
                         (error "Unknown module: ~A" module-name))
                        ((pathnamep location)
                         (namestring location))
                        (t
                         (path:path-string location))))
         (project (load-module-project module-path))
         (resources (ecase resource-type
                      (:tests
                       (module-project-tests project))
                      (:integration
                       (module-project-integration-tests project))
                      (:sources
                       (module-project-sources project)))))
    ;; Pre-load any modules required by these resource files.
    ;; This must happen BEFORE taking the compile lock, because module loading
    ;; itself acquires the compile lock.  Without this, :require forms in
    ;; defpackage trigger recursive lock attempts.
    (when resources
      (let ((required-packages (remove-duplicates (loop for src in resources
                                                        append (source-info-requires src))
                                                  :test
                                                  #'string=)))
        (dolist (pkg-name required-packages)
          (let ((mod-name (string-downcase pkg-name)))
            ;; Only attempt to load modules that exist and aren't already loaded
            (when (and (get-module mod-name) (not (module-loaded-p (get-module mod-name))))
              (handler-case (load-module mod-name)
                (error
                 (e)
                 (log:debug "Could not pre-load ~A for ~A resources: ~A" mod-name module-name e))))))))
    ;; Build resources in dependency order (under compile lock for thread safety)
    (when resources
      (epsilon.sys.lock:with-recursive-lock-held (*compile-lock*)
        (let* ((proj-name (module-project-name project))
               ;; The resources reference this module's own packages plus
               ;; everything in module-project-requires. Build a fallback
               ;; table so unresolvable references in test/integration
               ;; files behave no worse than coarse-grained invalidation.
               (fallback-by-package (make-hash-table :test 'equal)))
          (let ((self-info (get-module module-name)))
            (when (and self-info (module-content-hash self-info))
              (let* ((md (module-metadata self-info))
                     (provides (and md (getf md :provides))))
                (dolist (provided-pkg (or provides (list module-name)))
                  (setf (gethash (string-upcase (string provided-pkg))
                                 fallback-by-package)
                        (module-content-hash self-info))))))
          (dolist (dep-name (module-project-requires project))
            (let ((dep (find-module :provides dep-name)))
              (when (and dep (module-content-hash dep))
                (let* ((md (module-metadata dep))
                       (provides (and md (getf md :provides))))
                  (dolist (provided-pkg (or provides (list dep-name)))
                    (setf (gethash (string-upcase (string provided-pkg))
                                   fallback-by-package)
                          (module-content-hash dep)))))))
          (multiple-value-bind (ordered cycles) (source-build-order resources)
            (when cycles
              (error "Cyclic dependencies in ~A ~A: ~A" module-name resource-type cycles))
            (dolist (source ordered)
              (build-lisp-source source proj-name
                                 :verbose verbose
                                 :force force
                                 :fallback-by-package fallback-by-package))))))
    t))

(defun module-resource (module-name-or-path &optional relative-path)
  "Resolve a resource path relative to a module's base directory.

   Two calling conventions:
     (module-resource \"epsilon.sql\" \"bindings/sqlite3.bir.lisp\")
     (module-resource \"bindings/sqlite3.bir.lisp\")  ; uses *current-module-name*

   Returns the absolute pathname, or signals an error if the resource does not exist."
  (let ((module-name (if relative-path
                       module-name-or-path
                       (or *current-module-name*
                           (error "module-resource: no module name and *current-module-name* is nil"))))
        (rel-path (or relative-path module-name-or-path)))
    (let* ((module-info (get-module module-name))
           (location (when module-info
                       (module-location module-info))))
      (unless location
        (error "Unknown module: ~A" module-name))
      (let* ((base-dir (cond
                         ((pathnamep location)
                          (namestring location))
                         (t
                          (path:path-string location))))
             (full-path (path:path-string (path:path-join base-dir rel-path))))
        (unless (probe-file full-path)
          (error "Resource not found: ~A in module ~A (resolved to ~A)"
                 rel-path
                 module-name
                 full-path))
        (namestring (truename full-path))))))

;;; ---------------------------------------------------------------------------
;;; Consolidated Module Loading (IMPL-086)
;;; ---------------------------------------------------------------------------
;;;
;;; Unified loading path for both .lisp files and standard Common Lisp files.
;;; Standard CL files can optionally include an Epsilon header comment block
;;; to express dependencies in a way compatible with .lisp files.
;;;
;;; Header format:
;;;   ;;;; epsilon-module
;;;   ;;;; :requires (epsilon epsilon.json)
;;;   ;;;; :provides (my-package)
;;;   ;;;; :imports ((epsilon.json json) (epsilon.map map))
(defun parse-epsilon-header (filepath)
  "Parse Epsilon header comments from the start of a file.
   Returns a plist of (:requires ... :provides ... :imports ...) or NIL
   if no header present.

   The header must start at line 1 with exactly ';;;; epsilon-module'.
   Subsequent lines with ';;;; :keyword (values)' are parsed as metadata.
   Parsing stops at first non-';;;;' line."
  (with-open-file (stream filepath :direction :input)
    (let ((first-line (read-line stream nil nil)))
      ;; Check for header marker
      (unless (and first-line
                   (string= (string-trim '(#\Space #\Tab) first-line) ";;;; epsilon-module"))
        (return-from parse-epsilon-header nil))
      ;; Parse subsequent ;;;; lines
      (let ((result nil))
        (loop for line = (read-line stream nil nil)
              while (and line (>= (length line) 4) (string= (subseq line 0 4) ";;;;"))
              do (let* ((content (string-trim '(#\Space #\Tab) (subseq line 4)))
                        (form (ignore-errors (read-from-string (concatenate 'string "(" content ")")))))
                (when (and form (keywordp (first form)))
                  (setf (getf result (first form)) (second form)))))
        result))))

(defun has-epsilon-header-p (filepath)
  "Return T if FILEPATH starts with an Epsilon header."
  (not (null (parse-epsilon-header filepath))))

(defun extract-header-info (filepath header)
  "Convert parsed Epsilon header to unified-source-info."
  (locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((requires (getf header :requires))
           (provides (getf header :provides))
           (imports (getf header :imports))
           (exports (getf header :exports))
           (use (getf header :use))
           (shadow (getf header :shadow))
           ;; Also extract defpackage info for package name if not in provides
           (defpkg-info (extract-defpackage-info filepath))
           (pkg-name (or (first provides) (unified-source-info-package-name defpkg-info))))
      (make-unified-source-info :uri
                                filepath
                                :package-name
                                (when pkg-name
                                  (string pkg-name))
                                :file-type
                                :lisp
                                :requires
                                (mapcar #'string (or requires '()))
                                :provides
                                (mapcar #'string (or provides '()))
                                :imports
                                (mapcar (lambda (spec)
                                          (if (consp spec)
                                            (cons (second spec) (first spec))
                                            (cons spec spec)))
                                        (or imports '()))
                                :exports
                                (mapcar #'string (or exports '()))
                                :use
                                (mapcar #'string (or use '()))
                                :shadow
                                shadow
                                :header-p
                                t))))

(defun extract-unified-source-info (filepath &optional src-root)
  "Extract dependency info from any source file.
   Checks for:
   1. Epsilon header comments (;;;; epsilon-module ...)
   2. Standard defpackage (:use ... :local-nicknames ...)
   Returns a unified-source-info structure."
  (declare (ignore src-root))
  (let ((header (parse-epsilon-header filepath)))
    (if header
      (extract-header-info filepath header)
      (extract-defpackage-info filepath))))

(defun unified-build-order (sources)
  "Return SOURCES (unified-source-info structs) in topological order.
   Files are sorted so dependencies are loaded before dependents.
   Returns two values: sorted list and any cyclic dependencies found."
  (let* ((nodes (make-hash-table :test 'equal))
         (pkg-to-source (make-hash-table :test 'equalp))
         (uris nil))
    ;; Build lookup tables
    (dolist (source sources)
      (let ((uri (unified-source-info-uri source)))
        (setf (gethash uri nodes) source)
        (push uri uris)
        ;; Map provided packages to this source
        (dolist (pkg (unified-source-info-provides source))
          (setf (gethash (string-upcase pkg) pkg-to-source) source))
        ;; Also map package-name if set
        (let ((pkg-name (unified-source-info-package-name source)))
          (when pkg-name
            (setf (gethash (string-upcase pkg-name) pkg-to-source) source)))))
    (setf uris (nreverse uris))
    ;; Topological sort via epsilon.graph
    (flet ((dep-uris (uri)
                     (let ((source (gethash uri nodes)))
                       (when source
                         (loop for pkg in (unified-source-info-requires source)
                               for dep-source = (gethash (string-upcase pkg) pkg-to-source)
                               when dep-source
                               collect (unified-source-info-uri dep-source))))))
      (multiple-value-bind (sorted-uris cycles) (graph:topological-sort uris
                                                                        #'dep-uris
                                                                        :on-cycle
                                                                        :collect
                                                                        :test
                                                                        'equal)
        (values (mapcar (lambda (uri)
                          (gethash uri nodes))
                        sorted-uris)
                cycles)))))

