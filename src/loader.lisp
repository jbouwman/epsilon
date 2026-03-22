;;;; Epsilon Module Loader - Implementation
;;;;
;;;; This module provides the loader infrastructure for Epsilon modules.
;;;; All source files use standard defpackage (or Epsilon's extended defpackage
;;;; with :require/:enter support).
;;;;
;;;; Key features:
;;;; - Module discovery from module.plist manifests
;;;; - Dependency resolution and topological build ordering
;;;; - Content-addressable FASL validation via BLAKE3 hashing
;;;; - Timestamp fallback for core bootstrap (before crypto loads)
;;;; - Clean package rewrite semantics on reload
;;;;
;;;; Package and class definitions are in loader-base.lisp (compiled earlier).
;;;; This file contains the full implementation.

(in-package :epsilon.loader)

(defvar *build-root* nil
  "Root directory for build outputs (_build/).
   Set during environment initialization from EPSILON_HOME.")

(defvar *current-module-name* nil
  "Name of the module currently being loaded/compiled.
   Bound during module loading so that module-resource can resolve
   resources without requiring an explicit module name argument.")

(defvar *compile-lock* (lock:make-lock "compile-lock")
  "Serializes all compilation across threads. SBCL's compiler is not
   reentrant, so concurrent compile-file/load calls must be avoided.")

;;; Module metadata validation

(defun validate-module-metadata (metadata filepath)
  "Validate module.plist metadata and return detailed error messages for any issues.
   Returns NIL if valid, or a list of error messages if invalid."
  (if (and (find-package "EPSILON.MODULE-SCHEMA")
           (fboundp (find-symbol "VALIDATE-MODULE-METADATA" "EPSILON.MODULE-SCHEMA")))
      (multiple-value-bind (valid-p errors)
          (funcall (find-symbol "VALIDATE-MODULE-METADATA" "EPSILON.MODULE-SCHEMA")
                   metadata filepath)
        (if valid-p nil errors))
      (validate-module-metadata-basic metadata filepath)))

(defun validate-module-metadata-basic (metadata filepath)
  "Basic validation for module metadata when advanced validation is not available"
  (let ((errors '())
        (valid-keys '(:name :module-set :description :author :platform
                      :sources :tests :benchmarks :examples :experiments
                      :docs :data :requires :provides :source-type
                      :resources :integration :commands :main :shape)))
    (unless (and (listp metadata) (evenp (length metadata)))
      (push (format nil "Invalid module.plist format in ~A: must be a property list" filepath) errors)
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
                  do (push (format nil "Invalid :requires entry in ~A: must be string" filepath) errors))))
      (when provides
        (unless (listp provides)
          (push (format nil "Invalid :provides in ~A: must be a list" filepath) errors))
        (when (listp provides)
          (loop for prov in provides
                unless (stringp prov)
                  do (push (format nil "Invalid :provides entry in ~A: must be string" filepath) errors)))))
    (when errors (nreverse errors))))

;;; Module Discovery and Registration

(defun register-module (environment path-arg)
  "Register a single module directory (contains module.plist)"
  (let* ((module-path (path:ensure-path path-arg))
         (module-file (path:path-join module-path "module.plist"))
         (file-string (path:path-string module-file)))
    (log:debug "Checking for module.plist at: ~A" file-string)
    (if (probe-file file-string)
        (handler-case
            (let* ((info (with-open-file (stream file-string :if-does-not-exist nil)
                           (when stream (read stream))))
                   (validation-errors (validate-module-metadata info file-string)))
              (cond
                (validation-errors
                 (log:error "Invalid module.plist at ~A:" file-string)
                 (dolist (err validation-errors)
                   (log:error "  ~A" err))
                 (error "Module validation failed: ~A" file-string))
                (t
                 (let ((module-name (getf info :name)))
                   (let ((pkg-info (make-instance 'module-info
                                                  :name module-name
                                                  :location module-path
                                                  :metadata info
                                                  :loaded-p nil)))
                     (log:debug "Registering module: ~A from ~A" module-name path-arg)
                     ;; Update the modules slot directly (assoc! modifies the place)
                     (when (typep environment 'build-environment)
                       (setf (modules environment)
                             (map:assoc (modules environment) module-name pkg-info))))))))
          (error (e)
            (log:error "Failed to parse module.plist at ~A: ~A" file-string e)
            (error "Invalid module.plist format at ~A: ~A" file-string e)))
        (log:debug "No module.plist found at: ~A" file-string))))

(defun scan-module-directory (environment path-arg)
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
          (register-module environment entry-path)
          (incf count))))
    count))

;;; Module Registry Operations

(defun get-module (environment name &key (error-p nil))
  "Get module-info for a given module name, or NIL if not found"
  (let ((modules-map (modules environment)))
    (or (and modules-map (map:get modules-map name))
        (and error-p (error "Module not found: ~A" name)))))

(defun mark-module-loaded (environment name)
  "Mark a module as loaded"
  (let ((info (get-module environment name)))
    (when info
      (setf (module-loaded-p info) t)
      (setf (module-load-time info) (get-universal-time)))))

(defun query-modules (environment &key name provides loaded-only predicate)
  "Query modules matching specified criteria."
  (let ((results '())
        (current-platform (string-downcase (symbol-name (env:platform))))
        (modules-map (modules environment)))
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
                    (or (not pkg-platform)
                        (string= pkg-platform current-platform))
                    (or (not predicate) (funcall predicate info)))
            collect info into matched
          finally (setf results matched))
    (when loaded-only
      (setf results (remove-if-not (lambda (i) (module-loaded-p i)) results)))
    results))

(defun find-module (environment &key name provides)
  "Find exactly one module matching the given criteria."
  (let ((matches (query-modules environment :name name :provides provides)))
    (cond
      ((null matches)
       (error "No module found~@[ name=~A~]~@[ provides=~A~]" name provides))
      ((> (length matches) 1)
       (error "Multiple modules found~@[ name=~A~]~@[ provides=~A~]: ~{~A~^, ~}"
              name provides (mapcar (lambda (m) (module-name m)) matches)))
      (t (first matches)))))

(defun module-uri (module-info)
  "Get the URI (location) of a module."
  (module-location module-info))

;;; ---------------------------------------------------------------------------
;;; Workspace and Environment
;;; ---------------------------------------------------------------------------

;; *environment* is defined in loader-base.lisp

(defun load-workspace-modules (environment workspace-path)
  "Load modules from a workspace.plist file into the environment.
   WORKSPACE-PATH is a directory containing workspace.plist.
   Workspaces are loaded only once; subsequent calls with the same path are skipped.
   Returns T if workspace was loaded, NIL if it was already loaded (skipped)."
  ;; Normalize path for consistent deduplication
  (let ((normalized-path (namestring (truename workspace-path))))
    ;; Check if already loaded
    (when (gethash normalized-path (loaded-workspaces environment))
      (log:debug "Workspace already loaded, skipping: ~A" normalized-path)
      (return-from load-workspace-modules nil))
    ;; Mark as loaded before processing (prevents cycles)
    (setf (gethash normalized-path (loaded-workspaces environment)) t)
    (let* ((ws-file (path:path-string
                     (path:path-join workspace-path "workspace.plist")))
           (metadata (handler-case
                         (with-open-file (stream ws-file)
                           (read stream))
                       (error (e)
                         (error "Failed to parse workspace at ~A: ~A" ws-file e))))
           (ws-name (getf metadata :name))
           (scan-dirs (getf metadata :scan))
           (module-paths (getf metadata :modules))
           (workspace-refs (getf metadata :workspaces)))
      (log:debug "Workspace '~A': ~D scan dirs, ~D modules, ~D referenced workspaces"
                 ws-name (length scan-dirs) (length module-paths) (length workspace-refs))
      ;; Scan directories first
      (dolist (rel-dir scan-dirs)
        (let ((abs-dir (path:path-string
                        (path:path-join workspace-path rel-dir))))
          (if (probe-file abs-dir)
              (scan-module-directory environment abs-dir)
              (log:warn "Workspace '~A' scan directory not found: ~A" ws-name abs-dir))))
      ;; Register explicit modules
      (dolist (rel-path module-paths)
        (let ((abs-path (path:path-string
                         (path:path-join workspace-path rel-path))))
          (when (probe-file abs-path)
            (register-module environment abs-path))))
      ;; Load referenced workspaces (for transitive dependencies)
      (dolist (ref-path workspace-refs)
        (let ((abs-ref-path
                (if (path:path-absolute-p (path:make-path ref-path))
                    ref-path
                    (path:path-string
                     (path:path-join workspace-path ref-path)))))
          (let ((ref-ws-file (path:path-string
                              (path:path-join abs-ref-path "workspace.plist"))))
            (when (probe-file ref-ws-file)
              (load-workspace-modules environment abs-ref-path)))))
      ;; Return T to indicate workspace was loaded
      t)))

(defun environment ()
  "Get or create the default environment.
   The root module is registered directly from EPSILON_HOME (where module.plist
   lives). If a workspace.plist is also present, its modules are loaded too."
  (unless *environment*
    (setf *environment* (make-build-environment))
    (let ((epsilon-home (env:getenv "EPSILON_HOME")))
      (unless epsilon-home
        (error "EPSILON_HOME environment variable not set"))
      ;; Set build root for consolidated FASL output
      (setf *build-root* (path:path-string
                          (path:path-join epsilon-home "_build")))
      ;; Register the root module directly from EPSILON_HOME
      (register-module *environment* epsilon-home)
      ;; Optionally load workspace modules if workspace.plist exists
      (let ((home-workspace (path:path-string
                             (path:path-join epsilon-home "workspace.plist"))))
        (when (probe-file home-workspace)
          (log:debug "Loading workspace from EPSILON_HOME: ~A" epsilon-home)
          (load-workspace-modules *environment* epsilon-home)))
      (unless (get-module *environment* "epsilon")
        (error "Core module epsilon not found"))
      (install-module-provider)))
  *environment*)

(defun epsilon-module-provider (module-name)
  "SBCL module provider for epsilon modules.
   Called by REQUIRE when a module is not already loaded."
  (when *environment*
    (let* ((name (string-downcase (string module-name)))
           (info (get-module *environment* name)))
      (when info
        (unless (module-loaded-p info)
          (log:info "Loading module: ~A" name)
          (load-module *environment* name))
        t))))

(defun install-module-provider ()
  "Install epsilon's module provider into SBCL's require mechanism."
  (pushnew 'epsilon-module-provider sb-ext:*module-provider-functions*))

;;; ---------------------------------------------------------------------------
;;; Import Validation
;;; ---------------------------------------------------------------------------

(defvar *strict-imports* nil
  "When T, missing imports signal an error instead of a warning.
   Set to T for production builds to catch import typos at compile time.")

(define-condition missing-import-warning (simple-warning)
  ((filepath :initarg :filepath :reader missing-import-filepath)
   (missing-packages :initarg :missing-packages :reader missing-import-packages))
  (:report (lambda (condition stream)
             (format stream "~A: Missing imports (will fail at runtime): ~{~A~^, ~}"
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
                                        :filepath filepath
                                        :missing-packages (nreverse missing)
                                        :format-control "~A: Missing imports: ~{~A~^, ~}"
                                        :format-arguments (list filepath missing))))
        (if *strict-imports*
            (error condition)
            (warn condition))))
    (nreverse valid)))

;;; ---------------------------------------------------------------------------
;;; File Discovery
;;; ---------------------------------------------------------------------------

(defun lisp-file-p (path)
  "Return T if PATH is an Epsilon .lisp file."
  (let ((path-str (if (pathnamep path) (namestring path) path)))
    (str:ends-with-p path-str ".lisp")))

(defun list-lisp-files (directory)
  "List all .lisp files in DIRECTORY recursively.
   Returns sorted list of file paths."
  (fs:list-files directory ".lisp"))

;;; ---------------------------------------------------------------------------
;;; Path/Package Utilities
;;; ---------------------------------------------------------------------------

(defun normalize-path-separators (path)
  "Normalize path separators to forward slash."
  (substitute #\/ #\\ path))

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
  (let* ((fp (normalize-path-separators (namestring filepath)))
         (sr (normalize-path-separators
              (if (str:ends-with-p src-root "/")
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
  (concatenate 'string
               (substitute #\/ #\. (string-downcase (string package-name)))
               ".lisp"))

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
    (or (string-equal declared expected)
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
    (values (string-equal (string declared-package) expected)
            expected)))

;;; ---------------------------------------------------------------------------
;;; Package Management
;;; ---------------------------------------------------------------------------

(defun redefine-package (name &key (use '(:cl)) local-nicknames shadow)
  "Delete existing package and create fresh.

   This provides clean rewrite semantics: when a .lisp file is reloaded,
   the package is completely recreated rather than merged. This means:
   - Removed exports disappear
   - Renamed symbols are properly updated
   - No stale bindings remain

   Arguments:
   - NAME: Package name (string or symbol)
   - USE: List of packages to use (default: (:cl))
   - LOCAL-NICKNAMES: Alist of (nickname . package-name)
   - SHADOW: List of symbols to shadow"
  (let* ((pkg-name (string name))
         (old-pkg (find-package pkg-name)))
    ;; Clean up existing package if present
    (when old-pkg
      ;; First, unuse this package from all packages that use it
      (dolist (user (package-used-by-list old-pkg))
        (unuse-package old-pkg user))
      ;; Delete the old package
      (delete-package old-pkg))
    ;; Create fresh package
    (let ((pkg (make-package pkg-name :use use)))
      ;; Set up shadowed symbols (before local nicknames, so shadowing works correctly)
      (when shadow
        (cl:shadow (mapcar (lambda (sym) (string sym)) shadow) pkg))
      ;; Set up local nicknames
      (loop for (nickname . target) in local-nicknames
            do (sb-ext:add-package-local-nickname nickname target pkg))
      pkg)))

;;; ---------------------------------------------------------------------------
;;; Dependency Graph
;;; ---------------------------------------------------------------------------
;;;
;;; Tracks which packages depend on which other packages. This enables:
;;; - Invalidation propagation when a package is reloaded
;;; - Stale detection for dependent modules
;;; - Selective recompilation of affected code
;;;
;;; The graph is bidirectional:
;;; - Forward edges: package -> packages it imports from
;;; - Reverse edges: package -> packages that import from it (dependents)

(defvar *package-dependency-graph*
  (make-hash-table :test 'equalp)
  "Hash table mapping package names to dependency-node structures.
   Keys are uppercase package name strings.")

(defstruct (dependency-node (:constructor make-dependency-node (name)))
  "Node in the package dependency graph."
  (name "" :type string)                    ; Package name
  (dependencies nil :type list)              ; Packages this one imports from
  (dependents nil :type list)                ; Packages that import from this one
  (version 0 :type fixnum)                   ; Monotonic version counter
  (dirty-p nil :type boolean))               ; Needs re-import from dependencies

(defun get-or-create-node (name)
  "Get or create a dependency node for package NAME."
  (let ((key (string-upcase (string name))))
    (or (gethash key *package-dependency-graph*)
        (setf (gethash key *package-dependency-graph*)
              (make-dependency-node key)))))

(defun register-package-dependency (dependent-pkg dependency-pkg)
  "Register that DEPENDENT-PKG imports from DEPENDENCY-PKG.
   Updates both forward and reverse edges in the graph."
  (let ((dependent-name (string-upcase (string dependent-pkg)))
        (dependency-name (string-upcase (string dependency-pkg))))
    (unless (string= dependent-name dependency-name)
      (let ((dependent-node (get-or-create-node dependent-name))
            (dependency-node (get-or-create-node dependency-name)))
        ;; Add forward edge: dependent -> dependency
        (pushnew dependency-name (dependency-node-dependencies dependent-node)
                 :test #'string=)
        ;; Add reverse edge: dependency -> dependent
        (pushnew dependent-name (dependency-node-dependents dependency-node)
                 :test #'string=)))))

(defun unregister-package-dependency (dependent-pkg dependency-pkg)
  "Remove the dependency relationship between packages."
  (let ((dependent-name (string-upcase (string dependent-pkg)))
        (dependency-name (string-upcase (string dependency-pkg))))
    (let ((dependent-node (gethash dependent-name *package-dependency-graph*))
          (dependency-node (gethash dependency-name *package-dependency-graph*)))
      (when dependent-node
        (setf (dependency-node-dependencies dependent-node)
              (remove dependency-name (dependency-node-dependencies dependent-node)
                      :test #'string=)))
      (when dependency-node
        (setf (dependency-node-dependents dependency-node)
              (remove dependent-name (dependency-node-dependents dependency-node)
                      :test #'string=))))))

(defun dependency-graph-edges ()
  "Extract all edges from *package-dependency-graph* as ((from to) ...) lists.
   Each edge represents: FROM depends on TO."
  (let ((edges nil))
    (maphash (lambda (name node)
               (dolist (dep (dependency-node-dependencies node))
                 (push (list name dep) edges)))
             *package-dependency-graph*)
    edges))

(defun %transitive-closure (start adjacency-fn)
  "Compute transitive closure from START using ADJACENCY-FN.
   Returns list of all reachable nodes (excluding START)."
  (let ((visited (make-hash-table :test 'equal))
        (result '()))
    (labels ((visit (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (push node result)
                 (dolist (next (funcall adjacency-fn node))
                   (visit next)))))
      (dolist (next (funcall adjacency-fn start))
        (visit next)))
    (nreverse result)))

(defun get-package-dependents (pkg-name &key (transitive nil))
  "Get list of packages that depend on PKG-NAME.
   If TRANSITIVE is T, includes indirect dependents."
  (let ((key (string-upcase (string pkg-name))))
    (let ((node (gethash key *package-dependency-graph*)))
      (if (null node)
          nil
          (if transitive
              (%transitive-closure key
               (lambda (k)
                 (let ((n (gethash k *package-dependency-graph*)))
                   (when n (dependency-node-dependents n)))))
              (copy-list (dependency-node-dependents node)))))))

(defun get-package-dependencies (pkg-name &key (transitive nil))
  "Get list of packages that PKG-NAME depends on.
   If TRANSITIVE is T, includes indirect dependencies."
  (let ((key (string-upcase (string pkg-name))))
    (let ((node (gethash key *package-dependency-graph*)))
      (if (null node)
          nil
          (if transitive
              (%transitive-closure key
               (lambda (k)
                 (let ((n (gethash k *package-dependency-graph*)))
                   (when n (dependency-node-dependencies n)))))
              (copy-list (dependency-node-dependencies node)))))))

(defun invalidate-dependents (pkg-name &key (transitive t))
  "Mark all dependents of PKG-NAME as dirty (needing reload).
   If TRANSITIVE is T (default), propagates to indirect dependents.
   Returns list of invalidated package names."
  (let ((node (gethash (string-upcase (string pkg-name))
                       *package-dependency-graph*))
        (invalidated nil))
    (when node
      ;; Increment version to signal change
      (incf (dependency-node-version node))
      ;; Mark dependents as dirty
      (let ((to-invalidate (get-package-dependents pkg-name :transitive transitive)))
        (dolist (dep-name to-invalidate)
          (let ((dep-node (gethash dep-name *package-dependency-graph*)))
            (when (and dep-node (not (dependency-node-dirty-p dep-node)))
              (setf (dependency-node-dirty-p dep-node) t)
              (push dep-name invalidated))))))
    (nreverse invalidated)))

(defun clear-dependency-graph ()
  "Clear the entire dependency graph. Use when resetting the system."
  (clrhash *package-dependency-graph*))

;;; ---------------------------------------------------------------------------
;;; In-Place Package Updates
;;; ---------------------------------------------------------------------------
;;;
;;; Unlike redefine-package which deletes and recreates packages,
;;; update-package modifies packages in place to maintain symbol identity.
;;; This is critical for:
;;; - Closures that capture function references
;;; - CLOS method dispatch on existing classes
;;; - Cached symbol references in compiled code

;; *preserve-symbol-identity* is defined in loader-base.lisp

(defun update-local-nicknames (pkg new-nicknames)
  "Update package-local nicknames for PKG.
   NEW-NICKNAMES is an alist of (nickname . target-package)."
  ;; Get current nicknames
  (let ((current (sb-ext:package-local-nicknames pkg))
        (desired (make-hash-table :test 'equalp)))
    ;; Build desired nickname table
    (dolist (pair new-nicknames)
      (let ((nick (string (car pair)))
            (target (string (cdr pair))))
        (setf (gethash nick desired) target)))
    ;; Remove nicknames that are no longer wanted or point to different packages
    (dolist (entry current)
      (let* ((nick (string (car entry)))
             (target-pkg (cdr entry))
             (target-name (package-name target-pkg))
             (desired-target (gethash nick desired)))
        (unless (and desired-target (string-equal desired-target target-name))
          (sb-ext:remove-package-local-nickname nick pkg))))
    ;; Add new nicknames
    (dolist (pair new-nicknames)
      (let ((nick (car pair))
            (target (cdr pair)))
        (when (find-package target)
          (sb-ext:add-package-local-nickname nick target pkg))))))

(defun update-package (name &key (use '(:cl)) local-nicknames shadow)
  "Update package in place, maintaining symbol identity.

   Unlike redefine-package which deletes and recreates the package,
   this function modifies an existing package to match the new specification
   while preserving symbol identity for symbols that remain.

   This is essential for maintaining:
   - Function references in closures
   - CLOS method dispatch
   - Compiled code references

   Arguments:
   - NAME: Package name (string or symbol)
   - USE: List of packages to use (default: (:cl))
   - LOCAL-NICKNAMES: Alist of (nickname . package-name)
   - SHADOW: List of symbols to shadow"
  (let* ((pkg-name (string-upcase (string name)))
         (pkg (find-package pkg-name)))
    (if (null pkg)
        ;; Package doesn't exist - create it fresh
        (redefine-package name :use use :local-nicknames local-nicknames :shadow shadow)
        ;; Package exists - update in place
        (progn
          ;; Update use list
          (let ((current-uses (package-use-list pkg))
                (desired-uses (mapcar #'find-package
                                      (remove-if-not #'find-package use))))
            ;; Unuse packages no longer wanted
            (dolist (used current-uses)
              (unless (member used desired-uses)
                (unuse-package used pkg)))
            ;; Use new packages
            (dolist (to-use desired-uses)
              (unless (member to-use current-uses)
                (use-package to-use pkg))))

          ;; Update shadows
          ;; Note: Can't unshadow easily, so we just ensure requested shadows exist
          (when shadow
            (cl:shadow (mapcar (lambda (sym) (string sym)) shadow) pkg))

          ;; Update local nicknames
          (update-local-nicknames pkg local-nicknames)

          ;; Register dependencies in the graph
          (dolist (pair local-nicknames)
            (let ((target (cdr pair)))
              (when (find-package target)
                (register-package-dependency pkg-name (string target)))))

          pkg))))

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
    (and (> (length name) 0)
         (not (char= (char name 0) #\%))
         (not (char= (char name 0) #\-)))))

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
        (when (and (eq (symbol-package sym) pkg)  ; Interned here, not inherited
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
      (let ((should-export
              (if explicit-exports
                  ;; Explicit exports: intern and collect only specified symbols
                  (loop for name in explicit-exports
                        for sym = (find-symbol (string-upcase name) pkg)
                        when (and sym (symbol-bound-p sym))
                          collect sym)
                  ;; Auto-export: collect all public bound symbols
                  (collect-public-symbols pkg)))
            (currently-exported nil))
        ;; Collect currently exported symbols
        (do-external-symbols (sym pkg)
          (push sym currently-exported))
        ;; Unexport symbols that shouldn't be exported anymore
        ;; BUT preserve imported symbols - they were explicitly exported via reexport
        (dolist (sym currently-exported)
          (unless (or (member sym should-export)
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
             (and (consp form)
                  (string= "DEFPACKAGE" (symbol-name (car form)))))
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
  uri              ; Full path to .lisp file
  package-name     ; Derived from path (e.g., \"epsilon.http.client\")
  declared-name    ; Actual declared name from defpackage form
  src-root         ; Root directory for derivation
  requires         ; List of required package names
  explicit-exports ; List of explicit export names, or NIL for auto-export
  )

(defstruct module-project
  "Module/project representation for .lisp modules.
   Parallels loader.lisp's project class but simplified for .lisp files."
  name               ; Module name from module.plist
  path               ; Module directory
  src-root           ; src/ subdirectory
  sources            ; List of source-info
  tests              ; List of source-info for tests
  integration-tests  ; List of source-info for integration tests
  requires           ; Dependencies from module.plist
  metadata)          ; Full module.plist plist

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
  uri              ; Full path to source file
  package-name     ; Primary package name (derived or declared)
  file-type        ; :lisp
  requires         ; List of required package names
  provides         ; List of provided package names
  imports          ; Alist of (nickname . package-name)
  exports          ; List of explicit exports (or NIL for auto-export)
  use              ; List of packages to use
  shadow           ; List of symbols to shadow
  header-p)        ; T if file has Epsilon header

(defun extract-defpackage-info (filepath)
  "Extract dependency information from a standard defpackage form.
   Returns a unified-source-info structure."
  (let ((forms nil)
        (pkg-name nil)
        (uses nil)
        (imports nil)
        (import-froms nil)  ; Packages from :import-from clauses
        (exports nil)
        (shadows nil))
    ;; Read forms from file
    (with-open-file (stream filepath :direction :input)
      (handler-case
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (push form forms)
                ;; Stop after finding defpackage and in-package
                when (and (consp form)
                          (member (car form) '(in-package cl:in-package)))
                  do (return))
        (error () nil)))
    (setf forms (nreverse forms))
    ;; Find defpackage form
    ;; Note: Check by symbol name because the reader may produce different symbols
    ;; depending on *package* - could be CL:DEFPACKAGE or EPSILON.MAIN:DEFPACKAGE
    ;; (when CL-USER has the shadowing-import of the extended defpackage)
    (let ((defpkg (find-if (lambda (form)
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
                  ((:use) (setf uses (mapcar #'string (cdr clause))))
                  ((:local-nicknames)
                   (dolist (spec (cdr clause))
                     (when (and (consp spec) (= (length spec) 2))
                       (push (cons (first spec) (second spec)) imports))))
                  ((:require)
                   ;; Extended defpackage :require clause: (pkg nick) or pkg
                   ;; Extract package names as dependencies
                   (dolist (spec (cdr clause))
                     (let ((pkg-name (if (consp spec) (first spec) spec))
                           (nickname (when (consp spec) (second spec))))
                       (if nickname
                           ;; With nickname: add to imports (like local-nicknames)
                           (push (cons nickname pkg-name) imports)
                           ;; Without nickname: add to import-froms (just the dependency)
                           (push (string pkg-name) import-froms)))))
                  ((:import-from)
                   ;; First element after :import-from is the package name
                   (when (cdr clause)
                     (push (string (second clause)) import-froms)))
                  ((:export) (setf exports (mapcar #'string (cdr clause))))
                  ((:shadow) (setf shadows (cdr clause)))))))
          ;; No defpackage - look for in-package form
          (let ((in-pkg (find-if (lambda (form)
                                   (and (consp form)
                                        (member (car form) '(in-package cl:in-package))))
                                 forms)))
            (when in-pkg
              (setf pkg-name (string (second in-pkg)))))))
    ;; Build unified info
    (make-unified-source-info
     :uri filepath
     :package-name pkg-name
     :file-type :lisp
     :requires (mapcar #'string
                       (remove-if (lambda (pkg)
                                    (member (string-upcase (string pkg))
                                            '("CL" "COMMON-LISP")
                                            :test #'string=))
                                  (append uses
                                          (mapcar #'cdr imports)
                                          import-froms)))
     :provides (when pkg-name (list pkg-name))
     :imports (nreverse imports)
     :exports exports
     :use uses
     :shadow shadows
     :header-p nil)))

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
                  (pkg-name (string (or (first provides)
                                        (fs:strip-extension (fs:basename file))))))
             (push (make-source-info
                    :uri file
                    :package-name pkg-name
                    :declared-name pkg-name
                    :src-root src-dir
                    :requires (mapcar #'string requires)
                    :explicit-exports (mapcar #'string exports))
                   sources)))
          ;; Extract from defpackage
          (t
           (let ((defpkg-info (extract-defpackage-info file)))
             (when (unified-source-info-package-name defpkg-info)
               (push (make-source-info
                      :uri file
                      :package-name (unified-source-info-package-name defpkg-info)
                      :declared-name (unified-source-info-package-name defpkg-info)
                      :src-root src-dir
                      :requires (unified-source-info-requires defpkg-info)
                      :explicit-exports (unified-source-info-exports defpkg-info))
                     sources)))))))
    (nreverse sources)))

(defun load-module-project (module-path)
  "Load any module from MODULE-PATH (directory with module.plist).
   Returns an module-project structure.

   Collects all source files and detects source type:
   - Files with epsilon header: Uses header metadata
   - Files with defpackage: Extracts from defpackage form

   The :source-type key in module.plist is ignored (auto-detected from files)."
  (let* ((module-file (path:path-string (path:path-join module-path "module.plist")))
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
                   (when (and (probe-file full-dir)
                              (fs:dir-p full-dir))
                     (setf all-sources
                           (append all-sources
                                   (find-all-sources full-dir))))))
               all-sources)))
      (make-module-project
       :name name
       :path module-path
       :src-root (path:path-string (path:path-join module-path (first source-dirs)))
       :sources (collect-sources source-dirs)
       :tests (collect-sources test-dirs)
       :integration-tests (when (and (probe-file integration-dir)
                                     (fs:dir-p integration-dir))
                            (find-all-sources integration-dir))
       :requires (getf metadata :requires)
       :metadata metadata))))

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
          (when (and declared
                     (not (string-equal declared pkg-name)))
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
      (multiple-value-bind (sorted-uris cycles)
          (graph:topological-sort uris #'dep-uris :on-cycle :collect :test 'equal)
        (values (mapcar (lambda (uri) (gethash uri nodes)) sorted-uris)
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
         (clean-rel-path (if (and (> (length rel-path) 0)
                                  (char= (char rel-path 0) #\/))
                             (subseq rel-path 1)
                             rel-path))
         ;; Change extension from .lisp to .fasl
         (fasl-rel-path (if (str:ends-with-p clean-rel-path ".lisp")
                            (concatenate 'string
                                         (subseq clean-rel-path 0 (- (length clean-rel-path) 5))
                                         ".fasl")
                            (concatenate 'string clean-rel-path ".fasl")))
         ;; Full target path
         (target-path (path:path-string
                       (path:path-join *build-root* module-name fasl-rel-path))))
    target-path))

;;; ---------------------------------------------------------------------------
;;; Content-Addressable FASL Validation
;;;
;;; Replaces timestamp-based staleness detection with content hashing.
;;; A FASL's validity is determined by BLAKE3 hash of its source content
;;; combined with hashes of all predecessors and upstream module dependencies.
;;; Each local FASL has a .key sidecar containing its content key.
;;; ---------------------------------------------------------------------------

(defvar *content-hashing-p* nil
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

(defun enable-content-hashing ()
  "Enable content-addressed validation once BLAKE3 is available.
   Called after crypto module loads."
  (when (find-package "EPSILON.DIGEST.BLAKE3")
    (setf *content-hashing-p* t)))

(defun compute-source-content-hash (source-path)
  "Compute BLAKE3 hash of a source file's bytes. Returns hex string."
  (let ((bytes (with-open-file (s source-path :direction :input
                                              :element-type '(unsigned-byte 8))
                 (let ((buf (make-array (file-length s)
                                        :element-type '(unsigned-byte 8))))
                   (read-sequence buf s)
                   buf))))
    (%blake3-hex bytes)))

(defun compute-content-key (source-hash predecessor-keys dep-module-hashes)
  "Compute the content key for a single source file.
   Combines source hash, predecessor keys, and dependency module hashes
   via BLAKE3 streaming hasher. Returns hex string."
  (let* ((parts (list* "epsilon-fasl-v1"
                        (lisp-implementation-version)
                        source-hash
                        (append predecessor-keys dep-module-hashes)))
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

(defun write-key-sidecar (fasl-path content-key)
  "Write the content key to the .key sidecar file."
  (let ((key-path (key-sidecar-path fasl-path)))
    (with-open-file (s key-path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (write-string content-key s))))

(defun read-key-sidecar (fasl-path)
  "Read the content key from the .key sidecar file. Returns NIL if not found."
  (let ((key-path (key-sidecar-path fasl-path)))
    (when (probe-file key-path)
      (handler-case
          (with-open-file (s key-path :direction :input)
            (let ((key (make-string 64)))
              (let ((n (read-sequence key s)))
                (when (plusp n)
                  (subseq key 0 n)))))
        (error () nil)))))

(defun local-fasl-valid-p (fasl-path content-key)
  "Check if a local FASL is valid by comparing its .key sidecar to CONTENT-KEY."
  (and (probe-file fasl-path)
       (let ((stored-key (read-key-sidecar fasl-path)))
         (and stored-key (string= stored-key content-key)))))

(defun build-lisp-source (source module-name &key verbose force
                                                   predecessor-keys dep-module-hashes
                                                   ;; Legacy timestamp parameter - ignored when content hashing is active
                                                   deps-newest)
  "Compile and load a .lisp file with epsilon-module header.
   When content hashing is available, uses content-addressable validation:
   - Computes content key from source hash + predecessor keys + dep module hashes
   - Checks local .key sidecar before compiling
   - Returns the content key string on success
   When content hashing is not available (core bootstrap), falls back to timestamp logic.
   Returns the content key (content-hashing mode) or T (legacy mode) on success."
  (let* ((uri (source-info-uri source))
         (pkg-name (source-info-package-name source))
         (explicit-exports (source-info-explicit-exports source))
         (fasl-path (fasl-path-for-source source module-name)))
    (when verbose
      (format t "~&; Building ~A (lisp)~%" pkg-name))
    ;; Ensure target directory exists
    (fs:make-dirs (path:path-parent (path:make-path fasl-path)))
    (if (and *content-hashing-p* (not force))
        ;; Content-hashing mode: content-addressable validation
        (let* ((source-hash (compute-source-content-hash uri))
               (content-key (compute-content-key source-hash
                                                 predecessor-keys
                                                 dep-module-hashes)))
          ;; Check local FASL validity via .key sidecar
          (if (local-fasl-valid-p fasl-path content-key)
              (progn
                (when verbose
                  (format t "~&;   ~A: local fasl valid~%" pkg-name))
                (load fasl-path))
              (progn
                (when verbose
                  (format t "~&;   ~A: compiling~%" pkg-name))
                (multiple-value-bind (output-path warnings-p failure-p)
                    (compile:compile-file-safely uri :output-file fasl-path
                                                 :verbose nil :print nil)
                  (declare (ignore output-path warnings-p))
                  (when failure-p
                    (error "Compilation failed for ~A" uri)))
                (write-key-sidecar fasl-path content-key)
                (load fasl-path)))
          ;; Auto-export
          (unless explicit-exports
            (auto-export-public-symbols (string-upcase pkg-name)))
          content-key)
        ;; Legacy/force mode: timestamp-based staleness
        (progn
          (let* ((fasl-time (and (probe-file fasl-path) (file-write-date fasl-path)))
                 (needs-compile (or force
                                    (not fasl-time)
                                    (and (probe-file uri) fasl-time
                                         (> (file-write-date uri) fasl-time))
                                    (and deps-newest fasl-time
                                         (> deps-newest fasl-time)))))
            (when needs-compile
              (multiple-value-bind (output-path warnings-p failure-p)
                  (compile:compile-file-safely uri :output-file fasl-path
                                               :verbose nil :print nil)
                (declare (ignore output-path warnings-p))
                (when failure-p
                  (error "Compilation failed for ~A" uri))))
            (load fasl-path))
          (unless explicit-exports
            (auto-export-public-symbols (string-upcase pkg-name)))
          ;; In legacy mode, write content key sidecar if hashing just became available
          ;; (e.g., after crypto loads mid-build)
          (when *content-hashing-p*
            (let* ((source-hash (compute-source-content-hash uri))
                   (content-key (compute-content-key source-hash
                                                     predecessor-keys
                                                     dep-module-hashes)))
              (write-key-sidecar fasl-path content-key)
              (return-from build-lisp-source content-key)))
          t))))

(defun build-dir-for-module (module-name)
  "Return the build output directory for MODULE-NAME as a string with
   trailing slash, so SBCL's pathname parser treats it as a directory
   rather than a file (important for names with dots like epsilon.crypto)."
  (let ((dir (path:path-string
              (path:path-join *build-root* module-name))))
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
    ;; Scan for actual fasls and remove orphans
    (when (probe-file fasl-dir)
      (labels ((scan (dir)
                 (dolist (entry (directory (merge-pathnames
                                           (make-pathname :name :wild :type :wild)
                                           (pathname dir))))
                   (cond
                     ((and (pathname-name entry)
                           (string-equal (pathname-type entry) "fasl"))
                      (let ((fasl-path (namestring entry)))
                        (unless (gethash fasl-path source-fasls)
                          (log:debug "Removing orphaned fasl: ~A" fasl-path)
                          (delete-file entry)
                          ;; Also remove .key sidecar if present
                          (let ((key-path (key-sidecar-path fasl-path)))
                            (when (probe-file key-path)
                              (delete-file key-path)))
                          (setf orphans-found t))))
                     ((and (not (pathname-name entry))
                           (not (equal entry (pathname dir))))
                      (scan (namestring entry)))))))
        (scan fasl-dir)))
    orphans-found))


(defun build-module-project (project &key verbose force dep-module-hashes
 ; Legacy parameter - passed through when CAS unavailable
                                          deps-newest)
  "Build all sources in PROJECT in dependency order.
   When CAS is available, uses content-key based caching with DEP-MODULE-HASHES.
   Returns the module hash string (CAS mode) or newest timestamp (legacy mode).
   Detects and removes orphaned fasls, forcing recompile when found.
   Acquires *compile-lock* to serialize compilation across threads."
  (epsilon.sys.lock:with-lock (*compile-lock*)
    ;; Clean orphaned fasls first; if any found, force full recompile
    (when (clean-orphaned-fasls project)
      (log:info "Orphaned fasls found in ~A, forcing recompile"
                (module-project-name project))
      (setf force t))
    (multiple-value-bind (ordered cycles)
        (source-build-order (module-project-sources project))
      (when cycles
        (error "Cyclic dependencies detected in ~A: ~A"
               (module-project-name project) cycles))
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
          (let ((result (build-lisp-source source module-name
                                           :verbose verbose :force force
                                           :predecessor-keys (reverse predecessor-keys)
                                           :dep-module-hashes dep-module-hashes
                                           :deps-newest deps-newest)))
            ;; Accumulate predecessor keys for subsequent sources
            (when (stringp result)
              (push result predecessor-keys)
              (push result all-content-keys))))
        ;; Return module hash (CAS) or newest timestamp (legacy)
        (if (and *content-hashing-p* all-content-keys)
            (compute-module-hash (nreverse all-content-keys)
                                 dep-module-hashes)
            newest)))))

;;; ---------------------------------------------------------------------------
;;; Module Loading API
;;; ---------------------------------------------------------------------------


(defun %load-module-impl (environment package &key force verbose)
  "Internal: load a module and its dependencies.
   Collects content hashes from dependencies for CAS validation.
   Signals package-error if stale fasls reference non-existent packages."
  (let ((module-info (get-module environment package)))
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
            (dep-module-hashes '()))
        (dolist (dep-name (module-project-requires project))
          (let ((resolved-dep (find-module environment :provides dep-name)))
            (when resolved-dep
              (unless (module-loaded-p resolved-dep)
                (load-module environment (module-name resolved-dep) :force force))
              ;; Collect legacy timestamp
              (let ((dep-ts (module-source-newest resolved-dep)))
                (when (> dep-ts deps-newest)
                  (setf deps-newest dep-ts)))
              ;; Collect content hash for CAS
              (let ((dep-hash (module-content-hash resolved-dep)))
                (when dep-hash
                  (push dep-hash dep-module-hashes))))))

        ;; Enable CAS after crypto module loads (it provides BLAKE3)
        (unless *content-hashing-p*
          (enable-content-hashing))

        ;; Build and load all sources
        (log:debug "Building module ~A" package)
        (let* ((*current-module-name* package)
               (build-result (build-module-project project
                                                   :verbose verbose
                                                   :force force
                                                   :dep-module-hashes (nreverse dep-module-hashes)
                                                   :deps-newest (when (> deps-newest 0) deps-newest))))

          ;; Mark as loaded, recording newest source timestamp and content hash
          (mark-module-loaded environment package)
          (if (stringp build-result)
              ;; CAS mode: build-result is the module hash
              (setf (module-content-hash module-info) build-result)
              ;; Legacy mode: build-result is newest timestamp
              (setf (module-source-newest module-info)
                    (max build-result deps-newest)))
          t)))))


(defun load-module (environment package &key force compile-only verbose)
  "Ensure a package is available in the environment.
   Compiles if necessary, then loads unless compile-only is true.

   PACKAGE - Package name to load (e.g., 'epsilon', 'http')
   FORCE - Force compilation of all build steps regardless of timestamps
   COMPILE-ONLY - Only compile, don't load (not yet supported for unified loading)
   VERBOSE - Print progress information

   Returns T if successfully loaded/compiled, NIL otherwise."
  (declare (ignore compile-only)) ; TODO: implement compile-only for unified loading
  (%load-module-impl environment package :force force :verbose verbose))

(defun load-module-resources (environment module-name resource-type &key force verbose)
  "Load resources of a specific type for a module.
   ENVIRONMENT - The build environment
   MODULE-NAME - Module name (e.g., 'epsilon')
   RESOURCE-TYPE - Type of resources (:tests, :benchmarks, :examples, etc.)
   FORCE - Force recompilation even if up-to-date
   VERBOSE - Print progress information

   Returns T if successful."
  (let* ((module-info (get-module environment module-name))
         (location (when module-info (module-location module-info)))
         (module-path (cond
                        ((null location) (error "Unknown module: ~A" module-name))
                        ((pathnamep location) (namestring location))
                        (t (path:path-string location))))
         (project (load-module-project module-path))
         (resources (ecase resource-type
                      (:tests (module-project-tests project))
                      (:integration (module-project-integration-tests project))
                      (:sources (module-project-sources project)))))
    ;; Pre-load any modules required by these resource files.
    ;; This must happen BEFORE taking the compile lock, because module loading
    ;; itself acquires the compile lock.  Without this, :require forms in
    ;; defpackage trigger recursive lock attempts.
    (when resources
      (let ((required-packages
              (remove-duplicates
               (loop for src in resources
                     append (source-info-requires src))
               :test #'string=)))
        (dolist (pkg-name required-packages)
          (let ((mod-name (string-downcase pkg-name)))
            ;; Only attempt to load modules that exist and aren't already loaded
            (when (and (get-module environment mod-name)
                       (not (module-loaded-p (get-module environment mod-name))))
              (handler-case
                  (load-module environment mod-name)
                (error (e)
                  (log:debug "Could not pre-load ~A for ~A resources: ~A"
                             mod-name module-name e))))))))
    ;; Build resources in dependency order (under compile lock for thread safety)
    (when resources
      (epsilon.sys.lock:with-lock (*compile-lock*)
        (let ((proj-name (module-project-name project)))
          (multiple-value-bind (ordered cycles)
              (source-build-order resources)
            (when cycles
              (error "Cyclic dependencies in ~A ~A: ~A"
                     module-name resource-type cycles))
            (dolist (source ordered)
              (build-lisp-source source proj-name :verbose verbose :force force))))))
    t))

(defun module-resource (module-name-or-path &optional relative-path
                                              (environment *environment*))
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
    (let* ((module-info (get-module environment module-name))
           (location (when module-info (module-location module-info))))
      (unless location
        (error "Unknown module: ~A" module-name))
      (let* ((base-dir (cond
                         ((pathnamep location) (namestring location))
                         (t (path:path-string location))))
             (full-path (path:path-string (path:path-join base-dir rel-path))))
        (unless (probe-file full-path)
          (error "Resource not found: ~A in module ~A (resolved to ~A)"
                 rel-path module-name full-path))
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
                   (string= (string-trim '(#\Space #\Tab) first-line)
                            ";;;; epsilon-module"))
        (return-from parse-epsilon-header nil))
      ;; Parse subsequent ;;;; lines
      (let ((result nil))
        (loop for line = (read-line stream nil nil)
              while (and line
                         (>= (length line) 4)
                         (string= (subseq line 0 4) ";;;;"))
              do (let* ((content (string-trim '(#\Space #\Tab)
                                              (subseq line 4)))
                        (form (ignore-errors
                                (read-from-string
                                 (concatenate 'string "(" content ")")))))
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
           (pkg-name (or (first provides)
                         (unified-source-info-package-name defpkg-info))))
      (make-unified-source-info
       :uri filepath
       :package-name (when pkg-name (string pkg-name))
       :file-type :lisp
       :requires (mapcar #'string (or requires '()))
       :provides (mapcar #'string (or provides '()))
       :imports (mapcar (lambda (spec)
                          (if (consp spec)
                              (cons (second spec) (first spec))
                              (cons spec spec)))
                        (or imports '()))
       :exports (mapcar #'string (or exports '()))
       :use (mapcar #'string (or use '()))
       :shadow shadow
       :header-p t))))

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

(defun load-lisp-file-with-header (filepath header &key verbose)
  "Load a .lisp file that has an Epsilon header.
   Sets up local nicknames from the header before loading."
  (let* ((imports (getf header :imports))
         (pkg-info (extract-defpackage-info filepath)))
    (declare (ignore pkg-info))
    (when verbose
      (format t "~&; Loading ~A (with Epsilon header)~%" filepath))
    ;; Validate that imported packages exist
    (let ((import-alist (mapcar (lambda (spec)
                                  (if (consp spec)
                                      (cons (second spec) (first spec))
                                      (cons spec spec)))
                                imports)))
      (validate-imports import-alist filepath))
    ;; Load the file normally - the defpackage will set up the package
    (load filepath :verbose verbose)))

(defun load-unified-source (filepath &key src-root verbose)
  "Load any source file with unified semantics.

   For .lisp files with Epsilon header: Validates dependencies, then loads normally
   For plain .lisp files: Loads with standard CL behavior"
  (declare (ignore src-root))
  (let ((header (parse-epsilon-header filepath)))
    (cond
      ;; .lisp file with Epsilon header
      (header
       (load-lisp-file-with-header filepath header :verbose verbose))
      ;; Plain .lisp file
      (t
       (when verbose
         (format t "~&; Loading ~A~%" filepath))
       (load filepath :verbose verbose)))))

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
      (multiple-value-bind (sorted-uris cycles)
          (graph:topological-sort uris #'dep-uris :on-cycle :collect :test 'equal)
        (values (mapcar (lambda (uri) (gethash uri nodes)) sorted-uris)
                cycles)))))

(defun find-unified-source-info (src-dir &key (extensions '(".lisp")))
  "Find all source files in SRC-DIR and create unified-source-info for each.
   Searches for files with EXTENSIONS (default: .lisp)."
  (let ((all-sources nil))
    (dolist (ext extensions)
      (let ((files (cond
                     ((string= ext ".lisp") (list-lisp-files src-dir))
                     (t (fs:list-files src-dir (subseq ext 1))))))
        (dolist (path files)
          (push (extract-unified-source-info path src-dir) all-sources))))
    (nreverse all-sources)))

(defun build-unified-sources (sources &key src-root verbose force)
  "Build all sources in dependency order.
   Returns T on success, signals error if cyclic dependencies found."
  (declare (ignore force))
  (multiple-value-bind (ordered cycles)
      (unified-build-order sources)
    (when cycles
      (error "Cyclic dependencies detected: ~A" cycles))
    (dolist (source ordered)
      (let ((uri (unified-source-info-uri source)))
        (load-unified-source uri :src-root src-root :verbose verbose)))
    t))
