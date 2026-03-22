;;;; Project Manifest Support
;;;;
;;;; This module provides explicit project manifests for Epsilon via
;;;; project.plist files. A project.plist declares the project's own
;;;; modules and its dependencies, replacing the implicit workspace
;;;; discovery system.
;;;;
;;;; When project.plist is found, workspace discovery does NOT run.
;;;; This avoids confusing double-registration.
;;;;
;;;; Format:
;;;;   (:name "my-project"
;;;;    :version "0.1.0"
;;;;    :description "Project description"
;;;;    :modules ("path/to/module1" "path/to/module2")
;;;;    :dependencies (("epsilon" :version ">=0.14.0"))
;;;;    :registries ("https://pkg.epsilon.dev"))

(defpackage epsilon.project
  (:use cl)
  (:local-nicknames
   (loader epsilon.loader)
   (discovery epsilon.file)
   (path epsilon.path)
   (fs epsilon.file)
   (env epsilon.sys.env)
   (log epsilon.log))
  (:export
   #:*current-project*
   #:find-project-file
   #:load-project
   #:validate-project-metadata
   #:project-name
   #:project-version
   #:project-description
   #:project-modules
   #:project-dependencies
   #:project-registries
   #:project-root))

(in-package epsilon.project)

;;; Project class

(defclass project ()
  ((name :initarg :name
         :reader project-name
         :type string)
   (version :initarg :version
            :reader project-version
            :initform nil)
   (description :initarg :description
                :reader project-description
                :initform nil)
   (root :initarg :root
         :reader project-root
         :type string
         :documentation "Absolute path to directory containing project.plist")
   (modules :initarg :modules
            :reader project-modules
            :initform nil
            :documentation "List of relative module path strings")
   (dependencies :initarg :dependencies
                 :reader project-dependencies
                 :initform nil
                 :documentation "List of dependency specs")
   (registries :initarg :registries
               :reader project-registries
               :initform nil
               :documentation "List of registry URL strings")
   (metadata :initarg :metadata
             :reader project-metadata
             :initform nil
             :documentation "Raw plist from project.plist")))

(defvar *current-project* nil
  "The currently loaded project, or NIL if none.")

;;; Project file discovery

(defun find-project-file (start-dir)
  "Walk up from START-DIR looking for project.plist.
   Returns the directory path (string) containing project.plist, or NIL."
  (let ((current (handler-case
                     (truename (discovery:normalize-directory start-dir))
                   (error () nil)))
        (depth 0)
        (depth-limit 10))
    (when current
      (loop while (and current
                       (< depth depth-limit)
                       (not (discovery:at-root-p current)))
            do (let ((project-file (merge-pathnames "project.plist" current)))
                 (when (probe-file project-file)
                   (return-from find-project-file (namestring current)))
                 (when (discovery:at-home-p current)
                   (return nil))
                 (setf current (discovery:parent-directory current))
                 (incf depth))))
    nil))

;;; Validation

(defvar *valid-project-keys*
  '(:name :version :description :scan :modules :dependencies :registries))

(defun validate-project-metadata (plist filepath)
  "Check that PLIST is a valid project.plist.
   Returns NIL if valid, or a list of error strings if invalid."
  (let ((errors '()))
    ;; Must be a plist
    (unless (and (listp plist) (evenp (length plist)))
      (push (format nil "Invalid project.plist format in ~A: must be a property list" filepath)
            errors)
      (return-from validate-project-metadata (nreverse errors)))
    ;; :name is required and must be a string
    (let ((name (getf plist :name)))
      (cond
        ((null name)
         (push (format nil "Missing required field :name in ~A" filepath) errors))
        ((not (stringp name))
         (push (format nil "Invalid :name in ~A: must be a string" filepath) errors))
        ((string= name "")
         (push (format nil "Invalid :name in ~A: cannot be empty" filepath) errors))))
    ;; Type-check optional fields
    (let ((version (getf plist :version))
          (description (getf plist :description))
          (scan (getf plist :scan))
          (modules (getf plist :modules))
          (dependencies (getf plist :dependencies))
          (registries (getf plist :registries)))
      (when (and version (not (stringp version)))
        (push (format nil "Invalid :version in ~A: must be a string" filepath) errors))
      (when (and description (not (stringp description)))
        (push (format nil "Invalid :description in ~A: must be a string" filepath) errors))
      (when scan
        (unless (listp scan)
          (push (format nil "Invalid :scan in ~A: must be a list" filepath) errors))
        (when (listp scan)
          (dolist (s scan)
            (unless (stringp s)
              (push (format nil "Invalid :scan entry in ~A: must be a string" filepath) errors)))))
      (when modules
        (unless (listp modules)
          (push (format nil "Invalid :modules in ~A: must be a list" filepath) errors))
        (when (listp modules)
          (dolist (m modules)
            (unless (stringp m)
              (push (format nil "Invalid :modules entry in ~A: must be a string" filepath) errors)))))
      (when dependencies
        (unless (listp dependencies)
          (push (format nil "Invalid :dependencies in ~A: must be a list" filepath) errors))
        (when (listp dependencies)
          (dolist (dep dependencies)
            (unless (and (listp dep) (stringp (first dep)))
              (push (format nil "Invalid dependency spec in ~A: ~S" filepath dep) errors)))))
      (when registries
        (unless (listp registries)
          (push (format nil "Invalid :registries in ~A: must be a list" filepath) errors))
        (when (listp registries)
          (dolist (r registries)
            (unless (stringp r)
              (push (format nil "Invalid :registries entry in ~A: must be a string" filepath) errors))))))
    ;; Reject unknown keys
    (loop for key in plist by #'cddr
          unless (member key *valid-project-keys*)
            do (push (format nil "Unknown key ~A in ~A" key filepath) errors))
    (when errors (nreverse errors))))

;;; Version checking

(defun parse-version (version-string)
  "Parse a version string like \"0.14.0\" into a list of integers (0 14 0)."
  (let ((parts '())
        (start 0))
    (loop for i from 0 to (length version-string)
          do (when (or (= i (length version-string))
                       (char= (char version-string i) #\.))
               (let ((part (parse-integer (subseq version-string start i) :junk-allowed t)))
                 (push (or part 0) parts))
               (setf start (1+ i))))
    (nreverse parts)))

(defun version-satisfies-p (actual-version constraint)
  "Check if ACTUAL-VERSION satisfies CONSTRAINT.
   CONSTRAINT is a string like \">=0.14.0\", \">1.0.0\", \"=0.14.0\", etc."
  (let* ((operators '(">=" "<=" ">" "<" "="))
         (op nil)
         (version-part nil))
    ;; Extract operator and version
    (dolist (candidate operators)
      (when (and (>= (length constraint) (length candidate))
                 (string= constraint candidate :end1 (length candidate)))
        (setf op candidate
              version-part (subseq constraint (length candidate)))
        (return)))
    (unless (and op version-part (> (length version-part) 0))
      (return-from version-satisfies-p nil))
    (let ((actual (parse-version actual-version))
          (required (parse-version version-part)))
      ;; Compare lexicographically by version components
      (let ((cmp (loop for a in actual
                       for r in required
                       when (< a r) return -1
                       when (> a r) return 1
                       finally (return (cond
                                         ((< (length actual) (length required)) -1)
                                         ((> (length actual) (length required)) 1)
                                         (t 0))))))
        (cond
          ((string= op ">=") (>= cmp 0))
          ((string= op "<=") (<= cmp 0))
          ((string= op ">")  (> cmp 0))
          ((string= op "<")  (< cmp 0))
          ((string= op "=")  (= cmp 0))
          (t nil))))))

(defun check-runtime-version (dep-spec)
  "Check that the runtime version satisfies a dependency spec.
   DEP-SPEC is like (\"epsilon\" :version \">=0.14.0\").
   Signals an error if the constraint is not satisfied."
  (let ((constraint (getf (rest dep-spec) :version)))
    (when constraint
      (let ((actual (env:version)))
        (unless (version-satisfies-p actual constraint)
          (error "Runtime version ~A does not satisfy ~A ~A"
                 actual (first dep-spec) constraint))))))

;;; Project loading

(defun load-project (environment project-dir)
  "Load a project manifest from PROJECT-DIR into ENVIRONMENT.
   1. Read and validate project.plist
   2. Check runtime version constraint
   3. Register all declared modules
   4. Store project in environment config
   5. Set *current-project*"
  (let* ((project-dir (discovery:normalize-directory project-dir))
         (project-file (namestring (merge-pathnames "project.plist" project-dir)))
         (plist (handler-case
                    (with-open-file (stream project-file)
                      (read stream))
                  (error (e)
                    (error "Failed to read project.plist at ~A: ~A" project-file e))))
         (validation-errors (validate-project-metadata plist project-file)))
    (when validation-errors
      (error "Invalid project.plist at ~A:~%~{  ~A~%~}" project-file validation-errors))
    (let ((proj (make-instance 'project
                               :name (getf plist :name)
                               :version (getf plist :version)
                               :description (getf plist :description)
                               :root (namestring (truename project-dir))
                               :modules (getf plist :modules)
                               :dependencies (getf plist :dependencies)
                               :registries (getf plist :registries)
                               :metadata plist)))
      ;; Check runtime version constraint for "epsilon" dependency
      (dolist (dep (project-dependencies proj))
        (when (string-equal (first dep) "epsilon")
          (check-runtime-version dep)))
      ;; Scan directories first
      (let ((root (project-root proj)))
        (dolist (rel-dir (getf plist :scan))
          (let ((abs-dir (path:path-string
                          (path:path-join root rel-dir))))
            (if (probe-file abs-dir)
                (loader:scan-module-directory environment abs-dir)
                (log:warn "Project scan directory not found: ~A" abs-dir)))))
      ;; Register each explicit module path
      (let ((root (project-root proj))
            (registered 0))
        (dolist (rel-path (project-modules proj))
          (let ((abs-path (path:path-string
                           (path:path-join root rel-path))))
            (if (probe-file abs-path)
                (progn
                  (loader:register-module environment abs-path)
                  (incf registered))
                (log:warn "Project module path not found: ~A" abs-path)))))
      ;; Store in environment config
      (let ((config (loader:environment-config environment)))
        (epsilon.map:assoc! config :project proj))
      ;; Set global
      (setf *current-project* proj)
      proj)))
