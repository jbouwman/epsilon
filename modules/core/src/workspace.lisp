;;;; Workspace-based module discovery system
;;;;
;;;; A workspace defines a collection of modules within a repository
;;;; and can reference other workspaces for composable module paths.
;;;;
;;;; Workspace file format (workspace.lisp at repository root):
;;;;
;;;;   (:name "my-project"
;;;;    :modules ("modules/core"
;;;;              "modules/utils"
;;;;              "lib/extensions")
;;;;    :workspaces ("../shared-libs"
;;;;                 "/opt/vendor-modules"))
;;;;
;;;; :name       - Identifier for this workspace (required)
;;;; :modules    - List of paths to module directories, relative to workspace file
;;;; :workspaces - List of paths to other workspace directories (optional)
;;;;               Each referenced directory must contain a workspace.lisp file

(defpackage epsilon.workspace
  (:use
   cl
   epsilon.tool.common)
  (:local-nicknames
   (env epsilon.sys.env)
   (fs epsilon.sys.fs)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (path epsilon.path)
   (log epsilon.log)
   (loader epsilon.loader))
  (:export
   ;; Core types
   workspace
   workspace-name
   workspace-location
   workspace-modules
   workspace-referenced-workspaces

   ;; Loading and discovery
   load-workspace
   resolve-workspace-modules

   ;; Environment integration
   make-workspace-environment

   ;; Errors
   workspace-error
   workspace-not-found-error
   workspace-parse-error
   circular-workspace-error))

(in-package epsilon.workspace)

;;; Conditions

(define-condition workspace-error (error)
  ((workspace-path :initarg :path :reader workspace-error-path))
  (:report (lambda (condition stream)
             (format stream "Workspace error at ~A"
                     (workspace-error-path condition)))))

(define-condition workspace-not-found-error (workspace-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Workspace file not found: ~A/workspace.lisp"
                     (workspace-error-path condition)))))

(define-condition workspace-parse-error (workspace-error)
  ((details :initarg :details :reader workspace-parse-error-details))
  (:report (lambda (condition stream)
             (format stream "Invalid workspace at ~A: ~A"
                     (workspace-error-path condition)
                     (workspace-parse-error-details condition)))))

(define-condition circular-workspace-error (workspace-error)
  ((cycle :initarg :cycle :reader circular-workspace-error-cycle))
  (:report (lambda (condition stream)
             (format stream "Circular workspace reference detected: ~{~A~^ -> ~}"
                     (circular-workspace-error-cycle condition)))))

;;; Workspace class

(defclass workspace ()
  ((name :initarg :name
         :reader workspace-name
         :type string
         :documentation "Unique identifier for this workspace")
   (location :initarg :location
             :reader workspace-location
             :documentation "Absolute path to workspace directory")
   (module-paths :initarg :module-paths
                 :reader workspace-module-paths
                 :initform nil
                 :documentation "List of relative paths to module directories")
   (workspace-refs :initarg :workspace-refs
                   :reader workspace-workspace-refs
                   :initform nil
                   :documentation "List of paths to referenced workspace directories")
   (resolved-modules :initform nil
                     :accessor workspace-resolved-modules
                     :documentation "Cache of resolved module-info objects")
   (referenced-workspaces :initform nil
                          :accessor workspace-referenced-workspaces
                          :documentation "Cache of loaded referenced workspace objects"))
  (:documentation "A workspace defines a collection of modules and can reference other workspaces"))

(defmethod print-object ((ws workspace) stream)
  (print-unreadable-object (ws stream :type t)
    (format stream "~A (~A modules, ~A refs)"
            (workspace-name ws)
            (length (workspace-module-paths ws))
            (length (workspace-workspace-refs ws)))))

;;; Validation

(defun validate-workspace-metadata (metadata &optional filepath)
  "Validate workspace.lisp metadata. Returns NIL if valid, or list of error messages.
   FILEPATH is optional, used for better error messages in the future."
  (declare (ignore filepath))
  (let ((errors nil)
        (valid-keys '(:name :modules :workspaces)))

    ;; Check that metadata is a property list
    (unless (and (listp metadata) (evenp (length metadata)))
      (push "Must be a property list with even number of elements" errors)
      (return-from validate-workspace-metadata errors))

    ;; Check for required :name field
    (let ((name (getf metadata :name)))
      (cond
        ((null name)
         (push "Missing required field :name" errors))
        ((not (stringp name))
         (push (format nil ":name must be a string, got ~A" (type-of name)) errors))
        ((string= name "")
         (push ":name cannot be empty" errors))))

    ;; Check for unknown keys
    (loop for key in metadata by #'cddr
          unless (member key valid-keys)
            do (push (format nil "Unknown key ~A. Valid keys: ~{~A~^, ~}"
                            key valid-keys) errors))

    ;; Validate :modules if present
    (let ((modules (getf metadata :modules)))
      (when modules
        (unless (listp modules)
          (push (format nil ":modules must be a list, got ~A" (type-of modules)) errors))
        (when (listp modules)
          (loop for mod in modules
                for index from 0
                unless (stringp mod)
                  do (push (format nil ":modules entry ~D must be a string, got ~A"
                                  index (type-of mod)) errors)))))

    ;; Validate :workspaces if present
    (let ((workspaces (getf metadata :workspaces)))
      (when workspaces
        (unless (listp workspaces)
          (push (format nil ":workspaces must be a list, got ~A" (type-of workspaces)) errors))
        (when (listp workspaces)
          (loop for ws in workspaces
                for index from 0
                unless (stringp ws)
                  do (push (format nil ":workspaces entry ~D must be a string, got ~A"
                                  index (type-of ws)) errors)))))

    (nreverse errors)))

;;; Loading

(defun load-workspace (workspace-dir &key (resolve t) (visited nil))
  "Load a workspace from a directory containing workspace.lisp.

   WORKSPACE-DIR - Path to directory containing workspace.lisp
   RESOLVE - If T, also resolve referenced workspaces (default T)
   VISITED - Internal: set of already-visited workspace paths for cycle detection

   Returns a WORKSPACE object."
  (let* ((ws-path (path:ensure-path workspace-dir))
         (ws-file (path:path-join ws-path "workspace.lisp"))
         (ws-file-string (path:path-string ws-file))
         (canonical-path (truename (path:path-string ws-path))))

    ;; Check for circular references
    (when (member canonical-path visited :test #'equal)
      (error 'circular-workspace-error
             :path ws-file-string
             :cycle (reverse (cons canonical-path visited))))

    ;; Check file exists
    (unless (probe-file ws-file-string)
      (error 'workspace-not-found-error :path workspace-dir))

    ;; Parse workspace file
    (let* ((metadata (handler-case
                         (with-open-file (stream ws-file-string)
                           (read stream))
                       (error (e)
                         (error 'workspace-parse-error
                                :path ws-file-string
                                :details (format nil "Failed to parse: ~A" e)))))
           (validation-errors (validate-workspace-metadata metadata ws-file-string)))

      (when validation-errors
        (error 'workspace-parse-error
               :path ws-file-string
               :details (format nil "~{~A~^; ~}" validation-errors)))

      ;; Create workspace object
      (let ((workspace (make-instance 'workspace
                                      :name (getf metadata :name)
                                      :location canonical-path
                                      :module-paths (or (getf metadata :modules) nil)
                                      :workspace-refs (or (getf metadata :workspaces) nil))))

        ;; Optionally resolve referenced workspaces
        (when (and resolve (workspace-workspace-refs workspace))
          (let ((new-visited (cons canonical-path visited)))
            (setf (workspace-referenced-workspaces workspace)
                  (mapcar (lambda (ref-path)
                            (let ((abs-ref-path
                                   (if (path:path-absolute-p (path:make-path ref-path))
                                       ref-path
                                       (path:path-string
                                        (path:path-join ws-path ref-path)))))
                              (load-workspace abs-ref-path
                                             :resolve t
                                             :visited new-visited)))
                          (workspace-workspace-refs workspace)))))

        workspace))))

;;; Module Resolution

(defun workspace-all-module-paths (workspace)
  "Get all module paths from this workspace and all referenced workspaces.
   Returns list of (workspace-name . absolute-module-path) pairs.
   Preserves order: local modules first, then referenced workspace modules in order."
  (let ((results nil))
    ;; Add local modules
    (dolist (rel-path (workspace-module-paths workspace))
      (let ((abs-path (path:path-string
                       (path:path-join (workspace-location workspace) rel-path))))
        (push (cons (workspace-name workspace) abs-path) results)))

    ;; Add modules from referenced workspaces (depth-first)
    (dolist (ref-ws (workspace-referenced-workspaces workspace))
      (dolist (pair (workspace-all-module-paths ref-ws))
        (push pair results)))

    (nreverse results)))

(defun resolve-workspace-modules (workspace)
  "Resolve all modules from workspace and referenced workspaces.
   Returns a map of module-name -> module-info.
   First definition wins for duplicate module names."
  (let ((modules (map:make-map))
        (seen-names (make-hash-table :test 'equal)))

    (dolist (pair (workspace-all-module-paths workspace))
      (destructuring-bind (ws-name . module-dir) pair
        (when (probe-file module-dir)
          (let ((module-file (path:path-string
                              (path:path-join module-dir "module.lisp"))))
            (when (probe-file module-file)
              (handler-case
                  (let* ((metadata (with-open-file (stream module-file)
                                     (read stream)))
                         (module-name (getf metadata :name)))
                    ;; First definition wins
                    (unless (gethash module-name seen-names)
                      (setf (gethash module-name seen-names) t)
                      (let ((module-info
                             (make-instance 'loader:module-info
                                           :name module-name
                                           :location (path:ensure-path module-dir)
                                           :metadata metadata
                                           :loaded-p nil)))
                        (log:debug "Workspace ~A: registered module ~A from ~A"
                                  ws-name module-name module-dir)
                        (map:assoc! modules module-name module-info))))
                (error (e)
                  (log:warn "Failed to load module from ~A: ~A" module-dir e))))))))

    modules))

;;; Environment Integration

(defun make-workspace-environment (workspace-path &key config)
  "Create a build environment from a workspace file.

   WORKSPACE-PATH - Path to directory containing workspace.lisp
   CONFIG - Optional build configuration map

   Returns a build-environment with all modules from the workspace
   (and referenced workspaces) registered."
  (let* ((workspace (load-workspace workspace-path))
         (env (loader:make-build-environment :config config))
         (modules (resolve-workspace-modules workspace)))

    ;; Copy modules into environment
    (loop for module-info in (map:vals modules)
          do (map:assoc! (loader::modules env)
                        (loader:module-name module-info)
                        module-info))

    (log:info "Loaded workspace '~A' with ~A modules"
             (workspace-name workspace)
             (map:count modules))

    env))

;;; Utility Functions

(defun find-workspace-root (&optional (start-dir (path:current-directory)))
  "Search upward from START-DIR for a directory containing workspace.lisp.
   Returns the directory path if found, NIL otherwise."
  (let ((current (path:ensure-path start-dir)))
    (loop
      (let ((ws-file (path:path-join current "workspace.lisp")))
        (when (probe-file (path:path-string ws-file))
          (return (path:path-string current))))
      (let ((parent (path:path-parent current)))
        (when (equal parent current)
          (return nil))
        (setf current parent)))))

(defun workspace-module-names (workspace)
  "Get list of all module names available in workspace and referenced workspaces."
  (mapcar #'car
          (map:seq (resolve-workspace-modules workspace))))
