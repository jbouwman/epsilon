;;;; epsilon.nx - Nx monorepo integration
;;;;
;;;; Generates Nx project.json files from Epsilon module.lisp definitions,
;;;; enabling dependency-ordered builds in Nx monorepos.

(defpackage epsilon.nx
  (:use cl)
  (:local-nicknames
   (json epsilon.json)
   (map epsilon.map)
   (fs epsilon.sys.fs)
   (str epsilon.string)
   (log epsilon.log))
  (:export
   ;; Main functions
   setup-nx-workspace
   generate-all-projects
   generate-project-json
   generate-nx-json
   generate-package-json
   ;; Module queries
   get-module-info
   list-all-modules
   get-dependency-order
   ;; Configuration
   *epsilon-root*
   *project-prefix*
   *executor-command*
   ;; CLI entry points
   cli-generate
   cli-setup))

(in-package epsilon.nx)

;;; Configuration

(defvar *epsilon-root* nil
  "Root directory of the Epsilon installation. Auto-detected if nil.")

(defvar *project-prefix* "epsilon-"
  "Prefix for Nx project names (e.g., 'epsilon-core', 'epsilon-http').")

(defvar *executor-command* "./epsilon"
  "Command to run Epsilon CLI from monorepo root.")

;;; Module info parsing

(defun get-epsilon-root ()
  "Get the Epsilon root directory."
  (or *epsilon-root*
      (let ((env (sb-ext:posix-getenv "EPSILON_HOME")))
        (when env
          (pathname (if (char= (char env (1- (length env))) #\/)
                        env
                        (concatenate 'string env "/")))))))

(defun modules-directory ()
  "Get the modules directory path."
  (let* ((root (get-epsilon-root))
         (root-str (namestring root)))
    (pathname (concatenate 'string root-str "modules/"))))

(defun read-module-lisp (module-dir)
  "Read and parse a module.lisp file from MODULE-DIR."
  (let ((path (merge-pathnames "module.lisp" module-dir)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (read s)))))

(defun get-module-info (module-name)
  "Get module info by name (e.g., 'epsilon.core' or 'core')."
  (let* ((short-name (if (str:starts-with-p module-name "epsilon.")
                         (subseq module-name 8)
                         module-name))
         (module-dir (merge-pathnames (format nil "~A/" short-name)
                                      (modules-directory))))
    (read-module-lisp module-dir)))

(defun list-all-modules ()
  "List all modules in the modules directory.
   Returns list of (short-name . module-info) pairs."
  (let ((modules nil))
    (dolist (dir (directory (merge-pathnames "*/" (modules-directory))))
      (let ((info (read-module-lisp dir)))
        (when info
          (let* ((full-name (getf info :name))
                 (short-name (if (and full-name (str:starts-with-p full-name "epsilon."))
                                 (subseq full-name 8)
                                 (car (last (pathname-directory dir))))))
            (push (cons short-name info) modules)))))
    (nreverse modules)))

;;; Virtual dependency resolution

(defun build-provider-map (modules)
  "Build a map from virtual module names to providing modules.
   E.g., 'epsilon.net' -> 'epsilon.darwin' (on macOS)."
  (let ((providers (make-hash-table :test 'equal)))
    (dolist (entry modules)
      (let* ((info (cdr entry))
             (provides (getf info :provides))
             (name (getf info :name)))
        (dolist (provided provides)
          (push name (gethash provided providers)))))
    providers))

(defun current-platform ()
  "Get the current platform identifier."
  #+darwin "darwin"
  #+linux "linux"
  #+windows "windows"
  #-(or darwin linux windows) "unknown")

(defun platform-matches-p (module-platform)
  "Check if MODULE-PLATFORM matches the current platform.
   Returns T if module-platform is NIL (runs on all platforms)
   or matches the current platform."
  (or (null module-platform)
      (string= module-platform (current-platform))))

(defun select-platform-provider (providers platform)
  "Select the appropriate provider for the current platform."
  (let ((platform-module (format nil "epsilon.~A" platform)))
    (find platform-module providers :test #'string=)))

;;; Nx project.json generation

(defun module-name-to-project-name (module-name)
  "Convert epsilon.foo to epsilon-foo for Nx project naming."
  (let ((name (if (str:starts-with-p module-name "epsilon.")
                  (subseq module-name 8)
                  module-name)))
    (concatenate 'string *project-prefix* name)))

(defun resolve-dependency (dep-name provider-map platform)
  "Resolve a dependency name to an Nx project name.
   Handles virtual dependencies via provider-map."
  (let ((providers (gethash dep-name provider-map)))
    (if providers
        ;; Virtual dependency - resolve to platform provider
        (let ((provider (select-platform-provider providers platform)))
          (when provider
            (module-name-to-project-name provider)))
        ;; Direct dependency
        (module-name-to-project-name dep-name))))

(defun make-platform-guard (platform-constraint command)
  "Wrap COMMAND with a platform check if PLATFORM-CONSTRAINT is set.
   Returns a shell command that skips gracefully on wrong platform."
  (if platform-constraint
      ;; Use shell to check uname and skip if platform doesn't match
      (let ((uname-check (cond
                           ((string= platform-constraint "darwin") "Darwin")
                           ((string= platform-constraint "linux") "Linux")
                           ((string= platform-constraint "windows") "Windows")
                           (t platform-constraint))))
        (format nil "if [ \"$(uname -s)\" != \"~A\" ]; then echo 'Skipping: wrong platform (need ~A)'; exit 0; fi && ~A"
                uname-check platform-constraint command))
      command))

(defun generate-project-json (module-info &key provider-map platform relative-root)
  "Generate Nx project.json content for a module.

   MODULE-INFO - Parsed module.lisp plist
   PROVIDER-MAP - Hash table mapping virtual deps to providers
   PLATFORM - Current platform string
   RELATIVE-ROOT - Path from module to monorepo root (e.g., '../../..')"
  (let* ((name (getf module-info :name))
         (project-name (module-name-to-project-name name))
         (requires (getf module-info :requires))
         (platform-constraint (getf module-info :platform))
         (root (or relative-root "{workspaceRoot}/packages/epsilon"))
         (implicit-deps
           (remove nil
                   (mapcar (lambda (dep)
                             (resolve-dependency dep provider-map platform))
                           requires)))
         ;; Base commands
         (build-cmd (format nil "~A --module ~A --eval t" *executor-command* name))
         (test-cmd (format nil "~A --test ~A" *executor-command* name))
         (lint-cmd (format nil "~A --exec \"(epsilon.lint:lint-module \\\"~A\\\")\"" *executor-command* name)))
    (map:make-map
     "name" project-name
     "$schema" "../../node_modules/nx/schemas/project-schema.json"
     "sourceRoot" "."
     "projectType" "library"
     "tags" (remove nil
                    (list "scope:epsilon"
                          "type:library"
                          (when platform-constraint
                            (format nil "platform:~A" platform-constraint))))
     "implicitDependencies" implicit-deps
     "targets" (map:make-map
                "build" (map:make-map
                         "executor" "nx:run-commands"
                         "options" (map:make-map
                                    "cwd" root
                                    "command" (make-platform-guard platform-constraint build-cmd))
                         "inputs" (list "{projectRoot}/**/*.lisp"
                                        "{projectRoot}/module.lisp"))
                "test" (map:make-map
                        "executor" "nx:run-commands"
                        "options" (map:make-map
                                   "cwd" root
                                   "command" (make-platform-guard platform-constraint test-cmd))
                        "dependsOn" (list "build")
                        "inputs" (list "{projectRoot}/**/*.lisp"))
                "lint" (map:make-map
                        "executor" "nx:run-commands"
                        "dependsOn" (list "^build")  ; Depends on epsilon.lint being built
                        "options" (map:make-map
                                   "cwd" root
                                   "command" (make-platform-guard platform-constraint lint-cmd))
                        "inputs" (list "{projectRoot}/**/*.lisp"))))))

(defun write-project-json (module-dir project-json)
  "Write project.json to a module directory."
  (let ((path (merge-pathnames "project.json" module-dir)))
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (json:encode project-json s))
    (log:info "Generated ~A" path)
    path))

;;; Dependency ordering

(defun build-dependency-graph (modules provider-map platform)
  "Build adjacency list for dependency graph.
   Returns hash table: project-name -> list of dependency project-names."
  (let ((graph (make-hash-table :test 'equal)))
    (dolist (entry modules)
      (let* ((info (cdr entry))
             (name (getf info :name))
             (project-name (module-name-to-project-name name))
             (requires (getf info :requires))
             (deps (remove nil
                           (mapcar (lambda (dep)
                                     (resolve-dependency dep provider-map platform))
                                   requires))))
        (setf (gethash project-name graph) deps)))
    graph))

(defun topological-sort (graph)
  "Perform topological sort on dependency graph.
   Returns list of project names in build order (dependencies first).
   GRAPH maps node -> list of dependencies (things node depends on)."
  (let ((in-degree (make-hash-table :test 'equal))
        (dependents (make-hash-table :test 'equal))  ; reverse edges
        (result nil)
        (queue nil))
    ;; Initialize all nodes
    (maphash (lambda (node deps)
               (declare (ignore deps))
               (setf (gethash node in-degree) 0)
               (setf (gethash node dependents) nil))
             graph)
    ;; Build reverse graph and calculate in-degrees
    ;; If A depends on B, then B -> A in build order
    ;; A's in-degree = number of its dependencies
    (maphash (lambda (node deps)
               (setf (gethash node in-degree) (length deps))
               (dolist (dep deps)
                 ;; dep is depended upon by node
                 ;; Only add if dep is a known node in the graph
                 (multiple-value-bind (existing-deps present-p) (gethash dep dependents)
                   (declare (ignore existing-deps))
                   (when present-p
                     (push node (gethash dep dependents))))))
             graph)
    ;; Find nodes with in-degree 0 (no dependencies)
    (maphash (lambda (node degree)
               (when (zerop degree)
                 (push node queue)))
             in-degree)
    ;; Kahn's algorithm: process nodes with no remaining dependencies
    (loop while queue do
      (let ((node (pop queue)))
        (push node result)
        ;; For each node that depends on this one, decrement its in-degree
        (dolist (dependent (gethash node dependents))
          (decf (gethash dependent in-degree))
          (when (zerop (gethash dependent in-degree))
            (push dependent queue)))))
    (nreverse result)))

(defun get-dependency-order ()
  "Get all modules in dependency order (dependencies first)."
  (let* ((modules (list-all-modules))
         (provider-map (build-provider-map modules))
         (platform (current-platform))
         (graph (build-dependency-graph modules provider-map platform)))
    (topological-sort graph)))

;;; Main generation functions

(defun generate-all-projects (&key (relative-root "{workspaceRoot}/packages/epsilon")
                                   dry-run)
  "Generate project.json for all modules.

   RELATIVE-ROOT - Path from module to monorepo root
   DRY-RUN - If true, don't write files, just return what would be generated

   Returns list of (module-name . project-json) pairs."
  (let* ((modules (list-all-modules))
         (provider-map (build-provider-map modules))
         (platform (current-platform))
         (results nil))
    (log:info "Generating Nx project files for ~A modules on ~A"
              (length modules) platform)
    (dolist (entry modules)
      (let* ((short-name (car entry))
             (info (cdr entry))
             (module-dir (merge-pathnames (format nil "~A/" short-name)
                                          (modules-directory)))
             (project-json (generate-project-json info
                                                  :provider-map provider-map
                                                  :platform platform
                                                  :relative-root relative-root)))
        (push (cons (getf info :name) project-json) results)
        (unless dry-run
          (write-project-json module-dir project-json))))
    (log:info "Generated ~A project.json files" (length results))
    (nreverse results)))

;;; Nx workspace configuration

(defun generate-nx-json ()
  "Generate nx.json configuration for the Epsilon workspace."
  (map:make-map
   "$schema" "./node_modules/nx/schemas/nx-schema.json"
   "namedInputs" (map:make-map
                  "default" (list "{projectRoot}/**/*")
                  "lisp" (list "{projectRoot}/**/*.lisp"
                               "{projectRoot}/module.lisp"))
   "targetDefaults" (map:make-map
                     "build" (map:make-map
                              "dependsOn" (list "^build")
                              "inputs" (list "lisp")
                              "cache" t)
                     "test" (map:make-map
                             "dependsOn" (list "build")
                             "inputs" (list "lisp")
                             "cache" t)
                     "lint" (map:make-map
                             "dependsOn" (list "^build")
                             "inputs" (list "lisp")
                             "cache" t))
   "plugins" (list)
   "defaultBase" "main"))

(defun generate-package-json ()
  "Generate package.json for the Epsilon workspace."
  (map:make-map
   "name" "@epsilon/workspace"
   "version" "0.0.0"
   "private" t
   "scripts" (map:make-map
              "build" "nx run-many -t build"
              "test" "nx run-many -t test"
              "lint" "nx run-many -t lint"
              "build:affected" "nx affected -t build"
              "test:affected" "nx affected -t test"
              "lint:affected" "nx affected -t lint"
              "graph" "nx graph"
              "verify" "nx run-many -t build test lint")
   "devDependencies" (map:make-map
                      "nx" "^19.0.0")))

;;; Workspace file writing

(defun write-nx-json (&optional (path (merge-pathnames "nx.json" (get-epsilon-root))))
  "Write nx.json to the workspace root."
  (let ((content (generate-nx-json)))
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (json:encode content s))
    (log:info "Generated ~A" path)
    path))

(defun write-package-json (&optional (path (merge-pathnames "package.json" (get-epsilon-root))))
  "Write package.json to the workspace root (only if it doesn't exist)."
  (if (probe-file path)
      (log:info "Skipping ~A (already exists)" path)
      (let ((content (generate-package-json)))
        (with-open-file (s path :direction :output
                              :if-does-not-exist :create)
          (json:encode content s))
        (log:info "Generated ~A" path)))
  path)

;;; Master setup function

(defun setup-nx-workspace (&key (relative-root ".") force)
  "Set up Epsilon as an Nx workspace.

   RELATIVE-ROOT - Path from modules to workspace root (for cwd in commands)
   FORCE - If true, overwrite existing package.json

   This function:
   1. Generates project.json for all modules
   2. Creates nx.json configuration
   3. Creates package.json (if it doesn't exist or FORCE is true)

   After running, install dependencies with: npm install"
  (log:info "Setting up Epsilon as Nx workspace...")

  ;; Generate all project.json files
  (generate-all-projects :relative-root relative-root)

  ;; Generate nx.json
  (write-nx-json)

  ;; Generate package.json (conditionally)
  (let ((pkg-path (merge-pathnames "package.json" (get-epsilon-root))))
    (if (and (probe-file pkg-path) (not force))
        (log:info "package.json exists, skipping (use :force t to overwrite)")
        (write-package-json pkg-path)))

  (log:info "Nx workspace setup complete!")
  (log:info "Next steps:")
  (log:info "  1. Run: npm install")
  (log:info "  2. View dependency graph: npx nx graph")
  (log:info "  3. Build all: npx nx run-many -t build")
  (log:info "  4. Test all: npx nx run-many -t test")
  t)

;;; CLI integration

(defun cli-generate ()
  "CLI entry point for generating Nx project files."
  (generate-all-projects :relative-root "."))

(defun cli-setup ()
  "CLI entry point for full Nx workspace setup."
  (setup-nx-workspace :relative-root "."))
