;;;; This package provides a module loading system with
;;;; dependency tracking and incremental compilation.

(defpackage epsilon.loader
  (:use
   cl
   epsilon.tool.common)
  (:local-nicknames
   (pkg epsilon.sys.pkg)
   (env epsilon.sys.env)
   (fs epsilon.sys.fs)
   (fn epsilon.function)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (path epsilon.path)
   (log epsilon.log)
   (protocol epsilon.protocol))
  (:export
   ;; Core functions
   get-module
   load-module
   environment
   
   ;; Generic resource handling
   load-module-resources
   sort-sources
   project-resources
   
   ;; REPL environment
   *environment*
   
   find-module
   name
   version
   location
   metadata
   source
   
   ;; From loader.environment
   ;; Environment class
   build-environment
   make-build-environment
   ;; Accessors
   environment-config
   ;; Operations
   register-module
   scan-module-directory
   query-modules
   ;; Module info
   module-info
   module-name
   module-location
   module-loaded-p
   module-load-time
   module-uri
   module-metadata))

(in-package epsilon.loader)

(defparameter *debug-build-order* nil
  "Enable debug logging of build order information")

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
   (metadata :initarg :metadata
             :accessor module-metadata
             :initform nil
             :documentation "Full module metadata including provides, requires, etc."))
  (:documentation "Information about a loaded or registered module"))

;;; Build Environment

(defclass build-environment ()
  ((modules :initform (map:make-map)
             :accessor modules
             :documentation "Registry of all modules (map of name -> module-info)")
   (config :initarg :config
           :accessor environment-config
           :initform (map:make-map)
           :documentation "Build configuration map containing all settings"))
  (:documentation "Encapsulates all configuration and state for a build session and acts as a module source"))

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

;;; Module Discovery and Registration

;;; Module metadata validation

(defun validate-module-metadata (metadata filepath)
  "Validate module.lisp metadata and return detailed error messages for any issues.
   Returns NIL if valid, or a list of error messages if invalid."
  (let ((errors '())
        (valid-keys '(:name :version :description :author :platform
                     :sources :tests :benchmarks :examples :experiments
                     :docs :data :requires :provides)))
    
    ;; Check that metadata is a property list
    (unless (and (listp metadata) (evenp (length metadata)))
      (push (format nil "Invalid module.lisp format in ~A: must be a property list with even number of elements" filepath) errors)
      (return-from validate-module-metadata errors))
    
    ;; Check for required :name field
    (let ((name (getf metadata :name)))
      (cond
        ((null name)
         (push (format nil "Missing required field :name in ~A" filepath) errors))
        ((not (stringp name))
         (push (format nil "Invalid :name field in ~A: must be a string, got ~A" filepath (type-of name)) errors))
        ((string= name "")
         (push (format nil "Invalid :name field in ~A: cannot be empty string" filepath) errors))))
    
    ;; Check for unknown keys
    (loop for key in metadata by #'cddr
          unless (member key valid-keys)
            do (push (format nil "Unknown key ~A in ~A. Valid keys are: ~{~A~^, ~}" 
                           key filepath valid-keys) errors))
    
    ;; Validate optional fields if present
    (let ((version (getf metadata :version))
          (description (getf metadata :description))
          (author (getf metadata :author))
          (platform (getf metadata :platform))
          (requires (getf metadata :requires))
          (provides (getf metadata :provides)))
      
      ;; Version validation
      (when (and version (not (stringp version)))
        (push (format nil "Invalid :version field in ~A: must be a string, got ~A" 
                     filepath (type-of version)) errors))
      
      ;; Description validation
      (when (and description (not (stringp description)))
        (push (format nil "Invalid :description field in ~A: must be a string, got ~A" 
                     filepath (type-of description)) errors))
      
      ;; Author validation
      (when (and author (not (stringp author)))
        (push (format nil "Invalid :author field in ~A: must be a string, got ~A" 
                     filepath (type-of author)) errors))
      
      ;; Platform validation
      (when (and platform (not (stringp platform)))
        (push (format nil "Invalid :platform field in ~A: must be a string, got ~A" 
                     filepath (type-of platform)) errors))
      
      ;; Requires validation - must be list of strings
      (when requires
        (unless (listp requires)
          (push (format nil "Invalid :requires field in ~A: must be a list, got ~A" 
                       filepath (type-of requires)) errors))
        (when (listp requires)
          (loop for req in requires
                for index from 0
                unless (stringp req)
                  do (push (format nil "Invalid :requires entry at position ~D in ~A: must be a string, got ~A" 
                                 index filepath (type-of req)) errors))))
      
      ;; Provides validation - must be list of strings
      (when provides
        (unless (listp provides)
          (push (format nil "Invalid :provides field in ~A: must be a list, got ~A" 
                       filepath (type-of provides)) errors))
        (when (listp provides)
          (loop for prov in provides
                for index from 0
                unless (stringp prov)
                  do (push (format nil "Invalid :provides entry at position ~D in ~A: must be a string, got ~A" 
                                 index filepath (type-of prov)) errors))))
      
      ;; Path list validation helper
      (flet ((validate-path-list (field-name value)
               (when value
                 (unless (listp value)
                   (push (format nil "Invalid ~A field in ~A: must be a list, got ~A" 
                               field-name filepath (type-of value)) errors))
                 (when (listp value)
                   (loop for path in value
                         for index from 0
                         unless (stringp path)
                           do (push (format nil "Invalid ~A entry at position ~D in ~A: must be a string, got ~A" 
                                          field-name index filepath (type-of path)) errors))))))
        
        ;; Validate path lists
        (validate-path-list ":sources" (getf metadata :sources))
        (validate-path-list ":tests" (getf metadata :tests))
        (validate-path-list ":benchmarks" (getf metadata :benchmarks))
        (validate-path-list ":examples" (getf metadata :examples))
        (validate-path-list ":experiments" (getf metadata :experiments))
        (validate-path-list ":docs" (getf metadata :docs))
        (validate-path-list ":data" (getf metadata :data))))
    
    ;; Return errors (NIL if no errors)
    (when errors (nreverse errors))))

(defun register-module (environment path)
  "Register a single module directory (contains module.lisp)"
  (let* ((module-path (path:ensure-path path))
         (module-file (path:path-join module-path "module.lisp"))
         (file-string (path:path-string module-file)))
    (log:debug "Checking for module.lisp at: ~A" file-string)
    (if (probe-file file-string)
        (handler-case
            (let* ((info (with-open-file (stream file-string :if-does-not-exist nil)
                           (when stream (read stream))))
                   (validation-errors (validate-module-metadata info file-string)))
              (cond
                ;; Validation failed - report all errors
                (validation-errors
                 (log:error "Invalid module.lisp at ~A:" file-string)
                 (dolist (error validation-errors)
                   (log:error "  ~A" error))
                 (error "Module validation failed: ~A~%~{  ~A~%~}" 
                        file-string validation-errors))
                ;; Validation passed - register the module
                (t
                 (let ((module-name (getf info :name)))
                   (let ((pkg-info (make-instance 'module-info
                                                  :name module-name
                                                  :location module-path
                                                  :metadata info
                                                  :loaded-p nil)))
                     (log:debug "Registering module: ~A from ~A" module-name path)
                     (map:assoc! (modules environment)
                                 module-name pkg-info))))))
          (error (e)
            (log:error "Failed to parse module.lisp at ~A: ~A" file-string e)
            (error "Invalid module.lisp format at ~A: ~A~%~
                    Make sure the file contains a valid property list starting with (:name \"module-name\" ...)" 
                   file-string e)))
        (log:debug "No module.lisp found at: ~A" file-string))))

(defun scan-module-directory (environment path)
  "Scan directory for module subdirectories and register them"
  (let ((base-path (path:ensure-path path)))
    (log:debug "Scanning module directory: ~A" (path:path-string base-path))
    (when (probe-file (path:path-string base-path))
      (let ((dirs (path:list-directory base-path :type :directories)))
        (log:debug "Found ~A subdirectories in ~A" (length dirs) path)
        (dolist (entry-path dirs)
          (log:debug "Attempting to register module from: ~A" entry-path)
          (register-module environment entry-path))))))

;;; Module Registry

(defun get-module (environment name &key (error-p nil))
  "Get module-info for a given module name, or NIL if not found"
  (or (map:get (modules environment) name)
      (and error-p
           (error "Module not found: ~A" name))))

(defun mark-module-loaded (environment name)
  "Mark a module as loaded"
  (let ((module-info (get-module environment name)))
    (when module-info
      (setf (module-loaded-p module-info) t
            (module-load-time module-info) (get-universal-time)))))

;;; Query Operations

(defun query-modules (environment &key name provides loaded-only predicate)
  "Query modules matching specified criteria. 
   Modules provide themselves by default (e.g., epsilon.core provides epsilon.core)."
  (let ((results '())
        (current-platform (string-downcase (symbol-name (env:platform)))))
    
    ;; Search all modules
    (loop for module-info in (map:vals (modules environment))
          for pkg-name = (module-name module-info)
          for metadata = (module-metadata module-info)
          for pkg-provides = (or (getf metadata :provides) (list pkg-name))
          for pkg-platform = (getf metadata :platform)
          when (and (or (not name)
                        (string= name pkg-name))
                    (or (not provides)
                        ;; Module provides itself by default
                        (string= provides pkg-name)
                        ;; Or explicitly provides the capability
                        (member provides pkg-provides :test #'string=))
                    (or (not pkg-platform)
                        (string= pkg-platform current-platform))
                    (or (not predicate)
                        (funcall predicate module-info)))
            collect module-info into loaded-results
          finally (setf results loaded-results))
    
    ;; Filter by loaded-only if requested
    (when loaded-only
      (setf results (remove-if-not #'module-loaded-p results)))
    
    results))

(defun find-module (environment &key name provides)
  "Find exactly one module matching the given criteria.
   Raises an error if no modules match or if multiple modules match.
   This is a convenience wrapper around query-modules."
  (let ((matches (query-modules environment 
                                 :name name 
                                 :provides provides)))
    (cond
      ((null matches)
       (error "No module found matching criteria~@[ name=~A~]~@[ provides=~A~]"
              name provides))
      ((> (length matches) 1)
       (error "Multiple modules found matching criteria~@[ name=~A~]~@[ provides=~A~]: ~{~A~^, ~}"
              name provides (mapcar #'module-name matches)))
      (t (first matches)))))


;;;; Original loader.lisp code continues here

(defvar *environment* nil
  "The default build environment.")

(defun environment ()
  "Get or create the default environment"
  (unless *environment*
    (setf *environment* (make-build-environment))
    ;; Use proper environment accessor instead of direct posix call
    (let ((epsilon-home (env:getenv "EPSILON_HOME")))
      (unless epsilon-home
        (error "EPSILON_HOME environment variable not set"))
      (let ((modules-path (path:path-join epsilon-home "modules")))
        (unless (probe-file (path:path-string modules-path))
          (error "Epsilon modules directory not found: ~A" modules-path))
        (scan-module-directory *environment* modules-path)
        ;; Verify core modules are available
        (let ((core-modules '("epsilon.core")))
          (dolist (module-name core-modules)
            (unless (get-module *environment* module-name)
              (error "Core module ~A not found in ~A" module-name modules-path)))))))
  *environment*)

(defun module-uri (module-info)
  (module-location module-info))

(defclass locatable ()
  ((uri :initarg :uri :accessor uri)))


(defgeneric build-order (node &key)
  (:documentation "Produce a sequence of steps necessary to build a specific node in the project source tree."))

(defclass source-info (locatable)
  ((defines :initarg :defines :accessor source-info-defines)
   (requires :initarg :requires :accessor source-info-requires)))

(defmethod path ((self locatable))
  (path:path-from-uri (uri self)))


(defmethod print-object ((obj source-info) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a"
            (path obj))))

(defclass target-info (locatable)
  ())

(defclass build-input ()
  ((project :initarg :project :accessor project)
   (source-info :initarg :source-info :accessor source-info)))

(defclass build-result ()
  ((build-input :initarg :build-input :reader build-input)
   (status :initform :not-compiled :accessor compilation-status)
   (warnings :initform nil :accessor compilation-warnings)
   (errors :initform nil :accessor compilation-errors)
   (stdout-output :initform nil :accessor stdout-output)
   (stderr-output :initform nil :accessor stderr-output)
   (stack-trace :initform nil :accessor stack-trace)
   (start-time :initform nil :accessor start-time)
   (end-time :initform nil :accessor end-time)))

(defclass project-build ()
  ((project :initarg :project)
   (results :initarg :results)
   (start-time :initform (get-internal-real-time)
               :reader start-time)
   (end-time :initform nil
             :accessor end-time)))

(defmethod build-order ((node build-input) &key)
  (let ((root (list node)))
    (dolist (r (source-info-requires (source-info node)))
      (let ((p (find-provider (project node) r)))
        (when p
          (setf root (append (build-order p) root)))))
    (remove-duplicates root
                       :key (lambda (node)
                              (source-info node)))))

(defun provided-package (node)
  (source-info-defines (source-info node)))

(defun find-provider (project package)
  (seq:first
   (seq:filter (lambda (node)
                 (string= (provided-package node)
                          package))
               (build-order project))))

(defmethod print-object ((obj build-input) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path (source-info obj))
            (build-input-status obj))))

(defmethod source-uri ((self build-input))
  (uri (source-info self)))

(defmethod target-uri ((self build-input))
  (uri (target-info self)))

(defclass project (locatable)
  ((name :initarg :name :accessor project-name)
   (version :initarg :version :accessor project-version)
   (author :initarg :author :accessor project-author)
   (description :initarg :description :accessor project-description)
   (platform :initarg :platform :accessor project-platform)
   (sources :initarg :sources :accessor project-sources)
   (tests :initarg :tests :accessor project-tests)
   (benchmarks :initarg :benchmarks :accessor project-benchmarks :initform nil)
   (examples :initarg :examples :accessor project-examples :initform nil)
   (experiments :initarg :experiments :accessor project-experiments :initform nil)
   (docs :initarg :docs :accessor project-docs :initform nil)
   (data :initarg :data :accessor project-data :initform nil)
   (requires :initarg :requires :accessor project-requires)
   (provides :initarg :provides :accessor project-provides)
   (modules :initarg :modules :accessor project-modules)))

(defun make-source-info (uri)
  (multiple-value-bind (defines requires)
      (interpret-package (read-first-form uri))
    (when (or defines requires)
      (make-instance 'source-info
                     :uri uri
                     :defines (pkg:normalize defines)
                     :requires (mapcar #'pkg:normalize requires)))))

(defun load-project (uri &optional environment)
  "Parse module definition from module.lisp in directory URI"
  (declare (ignore environment)) ; Currently unused, kept for API compatibility
  (log:debug "load-project, uri = ~A" uri)
  (let* ((module-file "module.lisp")
         (module-path (path:path-string (path:path-merge (path:path-from-uri uri) module-file)))
         (path (cond ((probe-file module-path) module-path)
                     (t (error "No module file found: ~A" module-path)))))
    (let* ((plist (with-open-file (stream path)
                    (read stream)))
           ;; Extract fields from property list
           (name (getf plist :name))
           (version (getf plist :version))
           (description (getf plist :description))
           (author (getf plist :author))
           (source-paths (or (getf plist :sources) '("src")))
           (test-paths (or (getf plist :tests) '("tests")))
           (benchmark-paths (getf plist :benchmarks))
           (example-paths (getf plist :examples))
           (experiment-paths (getf plist :experiments))
           (doc-paths (getf plist :docs))
           (data-paths (getf plist :data))
           (requires-list (getf plist :requires))
           (provides-list (getf plist :provides))
           (platform (getf plist :platform))
           ;; Helper function to load resources from paths
           (load-resources (lambda (paths)
                             (when paths
                               (sort-sources 
                                (mapcan (lambda (resource-path)
                                          (let ((full-uri (path:uri-merge uri resource-path)))
                                            (if (probe-file (path:path-from-uri full-uri))
                                                (find-source-info full-uri)
                                                '())))
                                        paths)))))
           (sources (funcall load-resources source-paths))
           (tests (funcall load-resources test-paths))
           (benchmarks (funcall load-resources benchmark-paths))
           (examples (funcall load-resources example-paths))
           (experiments (funcall load-resources experiment-paths))
           (docs (funcall load-resources doc-paths))
           (data (funcall load-resources data-paths)))
      
      (let ((project (make-instance 'project
                                    :uri uri
                                    :name name
                                    :version version
                                    :author author
                                    :description description
                                    :platform platform
                                    :sources sources
                                    :tests tests
                                    :benchmarks benchmarks
                                    :examples examples
                                    :experiments experiments
                                    :docs docs
                                    :data data
                                    :requires requires-list
                                    :provides provides-list
                                    :modules map:+empty+)))
        project))))

(defun find-source-info (uri)
  (handler-case
      (remove-if #'null
                 (mapcar #'make-source-info
                         (fs:list-files uri ".lisp")))
    (file-error ()
      ;; If directory doesn't exist or can't be read, return empty list
      '())
    ;; Let other errors (like package definition errors) bubble up
    ))

(defun collect-all-sources (project)
  "Collect sources from this project only (dependencies should already be loaded)"
  (let ((all-sources (copy-list (project-sources project)))
        (all-tests (copy-list (project-tests project))))
    ;; Don't include dependency sources - they should be loaded separately
    ;; via load-module's dependency handling
    (values (sort-sources all-sources)
            (sort-sources all-tests))))

(defun target-info (build-input)
  "Create target information for a source file using simplified EPK structure
   
   Output structure:
   - target/package/fasl/lib/... - individual FASLs
   - target/package/module.fasl - concatenated FASL"
  (let* ((project (project build-input))
         (project-path (path project))
         (source-full-path (path (source-info build-input)))
         (source-rel-path (subseq source-full-path (length project-path)))
         ;; Strip "src/" from the beginning if present
         (clean-rel-path (if (and (> (length source-rel-path) 4)
                                  (string= (subseq source-rel-path 0 4) "src/"))
                             (subseq source-rel-path 4)
                             source-rel-path))
         (target-rel-path (fs:replace-extension clean-rel-path "fasl"))
         ;; Use simplified EPK structure
         (target-path (path:string-path-join "target" "package" "fasl" "lib" target-rel-path))
         (target-uri (path:uri-merge (uri project) target-path)))
    (make-instance 'target-info
                   :uri target-uri)))

(defun %make-build-input (project source-info)
  "Create target information for a source file"
  (make-instance 'build-input
                 :project project
                 :source-info source-info))

(defmethod build-order ((project project) &key)
  (multiple-value-bind (all-sources all-tests)
      (collect-all-sources project)     ;TODO make sure this is only collecting the local sources
    (declare (ignore all-tests))
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq all-sources))))

;; test-build-order removed - use project-resources with :tests instead

(defun read-first-form (uri)
  "Read forms from a file until we find a defpackage or in-package form.
   Throws an error if no package definition is found, except for bootstrap files."
  (let ((file-path (path:path-from-uri uri)))
    (with-open-file (stream file-path)
      (loop for form = (ignore-errors (read stream nil :eof))
            until (eq form :eof)
            when (and (consp form) 
                      (member (first form) '(defpackage in-package) :test #'string-equal))
              return form
            finally 
              ;; Allow certain types of files to not have package definitions
              (let ((filename (file-namestring file-path))
                    (path-str (namestring file-path)))
                ;; Bootstrap files, test files, and example files are exempt
                (unless (or (member filename '("boot.lisp" "epsilon.lisp" "main.lisp") :test #'string=)
                            (search "/tests/" path-str)
                            (search "/examples/" path-str)
                            (search "/benchmarks/" path-str)
                            (search "/experiments/" path-str))
                  (error "No package definition (defpackage or in-package) found in file: ~A~%~
                          Regular source files must define a package. Bootstrap, test, example, and experimental files are exempt." 
                         file-path))
                (return nil))))))

(defun interpret-package (form)
  (cond ((string-equal 'defpackage (first form))
         (values (second form)
                 (append (cdr (assoc :use (cddr form)))
                         (mapcar #'second (cdr (assoc :local-nicknames (cddr form))))
                         ;; Extract packages from :import-from clauses
                         (mapcar #'cadr 
                                 (remove-if-not (lambda (clause)
                                                  (and (consp clause)
                                                       (eq (car clause) :import-from)))
                                                (cddr form))))))
        ((string-equal 'in-package (first form))
         (values nil
                 (cdr form)))))

(defun sort-sources (sources)
  "Sort source files topologically based on their intra-package dependencies.
   Only considers dependencies between files in the provided source list.
   External package dependencies are ignored (should be loaded separately).
   Returns two values: sorted list and cyclic dependencies (if any)."
  (let* ((nodes (reduce (lambda (m source-info)
                         (map:assoc m (path:path-from-uri (uri source-info)) source-info))
                       (remove-if #'null sources)
                       :initial-value map:+empty+))
         ;; Only track packages defined within this set of sources
         (local-packages (reduce (lambda (m v)
                                  (if (source-info-defines v)
                                      (map:assoc m (source-info-defines v) v)
                                      m))
                                sources
                                :initial-value map:+empty+))
         (visiting map:+empty+)
         (cycles nil)
         (sorted nil))
    (labels ((dep-paths (source)
               ;; Only include dependencies that are defined in this source set
               (loop :for pkg :in (source-info-requires source)
                     :for dep-source := (map:get local-packages pkg)
                     :when dep-source  ;; Only if it's a local dependency
                       :collect (path:path-from-uri (uri dep-source))))
             (visit (path-key path)
               (when (map:contains-p visiting path-key)
                 (let ((cycle (ldiff path (member path-key path))))
                   (push cycle cycles))
                 (return-from visit nil))
               (when (member path-key sorted :test #'equal)
                 (return-from visit t))
               (let ((node (map:get nodes path-key)))
                 (unless node
                   (return-from visit t))
                 (map:assoc! visiting path-key t)
                 (let ((source (map:get nodes path-key)))
                   (dolist (dep (dep-paths source))
                     (visit dep (cons path-key path))))
                 (setf visiting (map:dissoc visiting path-key))
                 (push path-key sorted)
                 t)))
      (dolist (source-info (map:vals nodes))
        (visit (path:path-from-uri (uri source-info)) nil))
      (values
       (mapcar (lambda (path-key)
                 (map:get nodes path-key))
               (nreverse sorted))
       cycles))))

(defun build-input-status (build-input)
  (cond ((not (fs:exists-p (target-uri build-input)))
         :target-missing)
        ((< (fs:modification-time (path:path-from-uri (target-uri build-input)))
            (fs:modification-time (path:path-from-uri (source-uri build-input))))
         :source-newer)
        (t
         :up-to-date)))

(defun print-build-output (result)
  "Print stdout and stderr output from build result"
  (format *error-output* "~%=== BUILD FAILURE DUMP ===~%")
  (format *error-output* "File: ~A~%" (path:path-from-uri (source-uri (build-input result))))
  (format *error-output* "Operation: ~A~%" (compilation-status (build-input result)))
  
  (when (stdout-output result)
    (let ((output (stdout-output result)))
      (format *error-output* "~%--- Captured STDOUT (~D chars) ---~%" (length output))
      (format *error-output* "~A~%" output)))
  
  (when (stderr-output result)
    (let ((output (stderr-output result)))
      (format *error-output* "~%--- Captured STDERR (~D chars) ---~%" (length output))
      (format *error-output* "~A~%" output)))
  
  (when (compilation-warnings result)
    (format *error-output* "~%--- Warnings (~D) ---~%" (length (compilation-warnings result)))
    (dolist (warning (compilation-warnings result))
      (format *error-output* "  ~A~%" warning)))
  
  (when (compilation-errors result)
    (format *error-output* "~%--- Errors (~D) ---~%" (length (compilation-errors result)))
    (dolist (error (compilation-errors result))
      (format *error-output* "  ~A~%" error)))
  
  (when (stack-trace result)
    (format *error-output* "~%--- Stack Trace ---~%")
    (dolist (frame (stack-trace result))
      (format *error-output* "  ~A~%" frame)))
  
  (format *error-output* "~%=== END BUILD FAILURE DUMP ===~%"))

(defun redefinition-warning-p (warning)
  "Check if warning is about redefinition (which we want to suppress)"
  (let ((warning-string (format nil "~A" warning)))
    (or (search "redefin" warning-string)
        (search "REDEFIN" warning-string))))

(defun handle-warning (warning result)
  "Handle compilation warning"
  (push warning (compilation-warnings result))
  
  ;; Always suppress redefinition warnings for interactive development
  (when (redefinition-warning-p warning)
    (muffle-warning)))
  

(defun handle-error (error result)
  "Handle compilation error"  
  (push error (compilation-errors result))
  ;; Force output to be flushed before halting
  (force-output *standard-output*)
  (force-output *error-output*)
  (format *error-output* "~%========================================~%")
  (format *error-output* "BUILD HALTED: Error in ~A~%" (path:path-from-uri (source-uri (build-input result))))
  (format *error-output* "Error: ~A~%" error)
  (format *error-output* "========================================~%")
  (print-build-output result)
  (force-output *error-output*)
  (error error))


(defun watch-operation (environment build-input fn)
  (let ((result (make-instance 'build-result
                               :build-input build-input))
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream))
        (completed nil))
    (unwind-protect
         (progn
           (setf (start-time result) (get-internal-real-time))
           (let ((*standard-output* stdout-stream)
                 (*error-output* stderr-stream))
             (handler-bind ((warning (lambda (w)
                                       ;; Capture output before handling warning
                                       (setf (stdout-output result) (get-output-stream-string stdout-stream)
                                             (stderr-output result) (get-output-stream-string stderr-stream))
                                       (handle-warning w result)))
                            (error (lambda (e)
                                     ;; Capture output before handling error  
                                     (setf (stdout-output result) (get-output-stream-string stdout-stream)
                                           (stderr-output result) (get-output-stream-string stderr-stream)
                                           (stack-trace result) (sb-debug:list-backtrace))
                                     (handle-error e result))))
               ;; Run without timeout to avoid Windows threading issues
               (funcall fn)
               (setf completed t))
      (setf (end-time result) (get-internal-real-time))
      ;; Always capture final output
      (unless (stdout-output result)
        (setf (stdout-output result) (get-output-stream-string stdout-stream)))
      (unless (stderr-output result)
        (setf (stderr-output result) (get-output-stream-string stderr-stream))))
    ;; Removed direct reporting - now handled by events
    result))

(defun compile-source (environment build-input)
  (watch-operation environment build-input
                   (lambda ()
                     (fs:make-dirs (path:path-parent (path:make-path (path:path-from-uri (target-uri build-input)))))
                     ;; Suppress verbose output - will be shown only on build abort
                     (let ((*compile-verbose* nil)
                           (*compile-print* nil))
                       (log:debug "Compiling: ~A" (path:path-from-uri (source-uri build-input)))
                       (compile-file (path:path-from-uri (source-uri build-input))
                                     :output-file (path:path-from-uri (target-uri build-input))
                                     :verbose nil
                                     :print nil)
                       ;; Load the compiled file immediately
                       (load (path:path-from-uri (target-uri build-input)))))))

(defun load-source (environment build-input)
  (watch-operation environment build-input
                   (lambda ()
                     (load (path:path-from-uri (target-uri build-input))))))

;; Events now handled by logging

(defun create-binary (fasl-files output-path)
  "Create a concatenated FASL file from individual FASL files"
  (when fasl-files
    (fs:make-dirs (path:path-parent (path:make-path output-path)))
    (with-open-file (output output-path :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
      (dolist (fasl-file fasl-files)
        (when (probe-file fasl-file)
          (with-open-file (input fasl-file :direction :input
                                           :element-type '(unsigned-byte 8))
            (loop for byte = (read-byte input nil nil)
                  while byte
                  do (write-byte byte output))))))))

(defun binary-current-p (project binary-path)
  "Check if concatenated FASL is current relative to all source files"
  (when (probe-file binary-path)
    (let ((fasl-time (file-write-date binary-path))
          (sources (append (project-sources project) (project-tests project))))
      (every (lambda (source-info)
               (let ((source-path (path:path-from-uri (uri source-info))))
                 (or (not (probe-file source-path))
                     (<= (file-write-date source-path) fasl-time))))
             sources))))

(defun load-binary (project)
  "Load concatenated FASL if it exists and is current, returns T if loaded"
  (let* ((binary-rel-path (path:string-path-join 
                                "target" "package" "module.fasl"))
         (binary-path (path:path-from-uri 
                            (path:uri-merge (uri project) binary-rel-path))))
    (when (and (probe-file binary-path)
               (binary-current-p project binary-path))
      (load binary-path)
      t)))

;;; TODO unify %build and %build-test

(defun %build (environment project &key force)
  "Build the given build-inputs, optionally forcing compilation of all steps"
  
  (let* ((build (make-instance 'project-build
                               :project project
                               :results '()))
         ;; Force build-inputs to ensure we have a concrete list  
         (build-inputs (seq:seq (seq:realize (build-order project)))))
    
    ;; Log build information for debugging (can be disabled if not needed)
    (when *debug-build-order*
      (log:info "Build order for ~A:" (project-name project))
      (let ((index 0)
            (build-list (seq:realize build-inputs)))
        (dolist (build-input build-list)
          (incf index)
          (log:info "  ~D. ~A (~A)" 
                    index
                    (path:path-from-uri (source-uri build-input))
                    (build-input-status build-input)))))
    
    ;; Force compilation/loading to happen immediately by realizing the sequence
    (let* ((results (let ((index 0))
                      (seq:realize 
                       (seq:map (lambda (build-input)
                                  (incf index)
                                  (let ((result (if force
                                                    (compile-source environment build-input)
                                                    (case (build-input-status build-input)
                                                      ((:target-missing
                                                        :source-newer)
                                                       (compile-source environment build-input))
                                                      (t
                                                       (load-source environment build-input))))))
                                    (when (compilation-errors result)
                                      (log:error "Compilation failed: ~A" 
                                                     (path (source-info (build-input result)))))
                                    result))
                                build-inputs)))))
      ;; Now results is a realized list, safe to store and use
      (setf (slot-value build 'results) results)
      (setf (end-time build) (get-internal-real-time))
      
      ;; results is now a concrete list, so every-p will work correctly
      (when (and (not force)
                 (every (lambda (result)
                          (not (compilation-errors result)))
                        results))
        (let* (;; Force fasl-files to be computed immediately
               (fasl-files (seq:realize 
                            (seq:map (lambda (build-input)
                                       (path:path-from-uri (target-uri build-input)))
                                     build-inputs)))
               (binary-rel-path (path:string-path-join 
                                      "target" "package" "module.fasl"))
               (binary-path (path:path-from-uri 
                                  (path:uri-merge (uri project) binary-rel-path))))
          ;; fasl-files is now a concrete list
          (when fasl-files
            (create-binary fasl-files binary-path))))
      
      build)))

(defun load-module (environment package &key force compile-only)
  "Ensure a package is available in the environment.
  Compiles if necessary, then loads unless compile-only is true.
  
  PACKAGE - Package name to load (e.g., 'epsilon.core', 'http')
  FORCE - Force compilation of all build steps regardless of timestamps
  COMPILE-ONLY - Only compile, don't load (for backward compatibility with build)
  
  Returns T if successfully loaded/compiled, NIL otherwise."
  ;; Check if module is already loaded
  (let ((module-info (get-module environment package)))
    (when (and module-info (module-loaded-p module-info) (not force))
      (log:debug "Module ~A already loaded, skipping" package)
      (return-from load-module t)))
  
  (let* ((module-info (get-module environment package))
         (location (when module-info (module-location module-info)))
         (module-dir (if location
                          location
                          (error "Unknown module: ~A" package)))
         (project (handler-case
                      (progn
                        (log:debug "Calling load-project")
                        (load-project module-dir environment))
                    (error (e)
                      (log:error "Error in load-project for ~A: ~A" package e)
                      (error e)))))
    ;; Build dependencies first
    (dolist (dep-name (project-requires project))
      (let ((resolved-dep (find-module environment :provides dep-name)))
        (unless (module-loaded-p resolved-dep)
          (load-module environment (module-name resolved-dep)))))
    
    ;; Now try to load concatenated FASL if dependencies are resolved and it's up-to-date
    (unless (or force compile-only (map:get (environment-config environment) :force))
      (when (load-binary project)
        (mark-module-loaded environment package)
        (return-from load-module t)))
    
    ;; Build if needed
    (let ((build-result (%build environment project :force (or force (map:get (environment-config environment) :force)))))
      (when build-result
        (unless compile-only
          (mark-module-loaded environment package)))
      build-result)))

(defgeneric project-resources (project resource-type)
  (:documentation "Get resources of the specified type from the project.
  Resource types: :sources :tests :benchmarks :examples :experiments :docs :data"))

(defmethod project-resources ((project project) (resource-type (eql :sources)))
  (project-sources project))

(defmethod project-resources ((project project) (resource-type (eql :tests)))
  (project-tests project))

(defun load-module-resources (environment module resource-type &key force compile-only)
  "Load resources of a specific type for a module.
   ENVIRONMENT - The loader environment
   MODULE - Module name (e.g., 'epsilon.core')
   RESOURCE-TYPE - Type of resources (:tests, :benchmarks, :examples, etc.)
   FORCE - Force recompilation even if up-to-date
   COMPILE-ONLY - Only compile, don't load
   
   Returns T if successful, NIL otherwise."
  (let* ((module-info (get-module environment module))
         (location (when module-info (module-location module-info)))
         (module-dir (if location
                          location
                          (error "Unknown module: ~A" module)))
         (project (load-project module-dir environment))
         (resources (project-resources project resource-type)))
    
    ;; Sort resources if they have dependencies, otherwise use as-is
    ;; Tests, benchmarks, etc. typically don't have inter-file dependencies
    (let ((sorted-resources 
           (if (eq resource-type :sources)
               ;; Source files need topological sorting based on dependencies
               (multiple-value-bind (sorted cycles)
                   (sort-sources resources)
                 (when cycles
                   (error "Circular dependencies detected in ~A sources: ~A" 
                          module cycles))
                 sorted)
               ;; Other resources (tests, benchmarks, etc.) can be processed in any order
               ;; They should be independent and not have inter-file dependencies
               resources)))
      
      ;; Build each resource
      (let ((all-success t))
        (dolist (resource sorted-resources)
          (let* ((build-input (%make-build-input project resource))
                 (status (build-input-status build-input)))
            (cond
              ;; Need to compile
              ((or force (member status '(:target-missing :source-newer)))
               (let ((result (compile-source environment build-input)))
                 (when (compilation-errors result)
                   (setf all-success nil)
                   (log:error "Failed to compile ~A" 
                             (path (source-info build-input))))))
              ;; Just load if not compile-only
              ((not compile-only)
               (let ((result (load-source environment build-input)))
                 (when (compilation-errors result)
                   (setf all-success nil)
                   (log:error "Failed to load ~A" 
                             (path (source-info build-input)))))))))
        all-success))))
