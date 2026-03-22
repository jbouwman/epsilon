;;;; Epsilon Loader Base - Package and Class Definitions
;;;;
;;;; Package definition and core classes that must be available early in the
;;;; boot order (before main.lisp).
;;;;
;;;; The full implementation is in loader.lisp, which is loaded later.
;;;; This split allows main.lisp and workspace.plist to reference the
;;;; epsilon.loader package and its classes at compile time.

(defpackage epsilon.loader
  (:use cl)
  (:local-nicknames (fs epsilon.file)
                    (lock epsilon.sys.lock)
                    (str epsilon.string)
                    (path epsilon.path)
                    (reader epsilon.reader)
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
   loaded-workspaces
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
   ;; Workspace and environment
   *environment*
   environment
   load-workspace-modules
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
   normalize-path-separators
   package-name-compatible-p

   ;; Package management
   redefine-package
   public-symbol-p
   symbol-bound-p
   collect-public-symbols
   auto-export-public-symbols
   sync-package-exports
   update-local-nicknames

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

   ;; Import validation
   *strict-imports*
   validate-imports
   missing-import-warning
   missing-import-filepath
   missing-import-packages

   ;; In-place package updates (maintains symbol identity)
   update-package
   *preserve-symbol-identity*

   ;; Dependency graph
   *package-dependency-graph*
   dependency-graph-edges
   register-package-dependency
   unregister-package-dependency
   get-package-dependents
   get-package-dependencies
   invalidate-dependents
   clear-dependency-graph
   dependency-node
   make-dependency-node
   dependency-node-p
   copy-dependency-node
   dependency-node-name
   dependency-node-dependencies
   dependency-node-dependents
   dependency-node-version
   dependency-node-dirty-p
   get-or-create-node

   ;; Content-addressable FASL validation
   *content-hashing-p*
   *stale-fasl-recovery*
   enable-content-hashing
   local-fasl-valid-p
   key-sidecar-path
   read-key-sidecar
   write-key-sidecar
   compute-source-content-hash
   compute-content-key
   compute-module-hash

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
   load-lisp-file-with-header
   load-unified-source
   unified-build-order
   find-unified-source-info
   build-unified-sources))

(in-package epsilon.loader)

;;; ---------------------------------------------------------------------------
;;; Forward Declarations - for functions defined in loader.lisp
;;;
;;; These declarations allow main.lisp to compile without undefined function
;;; warnings when referencing epsilon.loader functions that are defined later.
;;; Note: Using T for argument types to avoid conflicts with actual signatures.

(declaim (ftype (function () t) environment))
(declaim (ftype (function (t t &key (:error-p t)) t) get-module))
(declaim (ftype (function (t t &key (:force t) (:compile-only t) (:verbose t)) t) load-module))
(declaim (ftype (function (t t t &key (:force t) (:verbose t)) t) load-module-resources))
(declaim (ftype (function (t t) t) load-workspace-modules))
(declaim (ftype (function (t &key (:name t) (:provides t) (:loaded-only t) (:predicate t)) list) query-modules))
(declaim (ftype (function (t t) t) register-module))
(declaim (ftype (function (t t) t) scan-module-directory))

;;; ---------------------------------------------------------------------------
;;; Core Classes - must be available at compile time for other modules
;;; ---------------------------------------------------------------------------

(defclass module-info ()
  ((name :initarg :name
         :reader module-name
         :type string
         :documentation "Module name")
   (location :initarg :location
             :reader module-location
             :documentation "Module location (path or URI)")
   (loaded-p :initarg :loaded-p
             :accessor module-loaded-p
             :initform nil
             :type boolean
             :documentation "Whether module has been loaded")
   (load-time :initarg :load-time
              :accessor module-load-time
              :initform nil
              :documentation "When module was loaded")
   (source-newest :initarg :source-newest
                  :accessor module-source-newest
                  :initform 0
                  :type integer
                  :documentation "Newest source file-write-date across this module's sources")
   (content-hash :initarg :content-hash
                 :accessor module-content-hash
                 :initform nil
                 :type (or null string)
                 :documentation "Module content hash (BLAKE3, set after build)")
   (metadata :initarg :metadata
             :accessor module-metadata
             :initform nil
             :documentation "Full module metadata including provides, requires, etc."))
  (:documentation "Information about a loaded or registered module"))

(defclass build-environment ()
  ((modules :initform (map:make-map)
            :accessor modules
            :documentation "Registry of all modules (map of name -> module-info)")
   (loaded-workspaces :initform (make-hash-table :test 'equal)
                      :accessor loaded-workspaces
                      :documentation "Set of workspace paths that have been loaded (prevents double loading)")
   (config :initarg :config
           :accessor environment-config
           :initform (map:make-map)
           :documentation "Build configuration map containing all settings"))
  (:documentation "Encapsulates all configuration and state for a build session"))

(defun make-build-environment (&key config)
  "Create a new build environment"
  (make-instance 'build-environment
                 :config (or config
                             (map:make-map
                              :error-behavior :halt
                              :warning-behavior :ignore
                              :optimization '(speed 1 safety 3 debug 2)
                              :parallel nil
                              :timeout 60
                              :force nil
                              :verbose nil))))

;;; ---------------------------------------------------------------------------
;;; Global State
;;; ---------------------------------------------------------------------------

(defvar *environment* nil
  "The current build environment. Initialized by (environment) function.")

(defvar *preserve-symbol-identity* t
  "When T (default), use update-package instead of redefine-package for reloads.
   This maintains symbol identity across reloads.")
