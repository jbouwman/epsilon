;;;; This module provides a build system with content-based dependency
;;;; tracking, incremental compilation, and module management.

(defpackage epsilon.tool.build
  (:use
   cl
   epsilon.tool.common)
  (:local-nicknames
   (pkg epsilon.sys.pkg)
   (env epsilon.sys.env)
   (fs epsilon.sys.fs)
   (digest epsilon.digest)
   (fn epsilon.function)
   (hex epsilon.hex)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (path epsilon.path)
   (log epsilon.log)
   (proto epsilon.build.protocol)
   (build-env epsilon.tool.build-environment))
  (:export
   ;; Core build functions
   build
   build-with-environment
   load-package
   get-test-files
   get-resource-files
   local-packages
   
   ;; Environment management
   make-build-environment
   current-environment
   ensure-package-source
   add-package-repo

   ;; Package registry
   get-package
   list-available-packages
   list-packages
   register-package
   mark-package-loaded
   is-package-loaded
   ;; Package info accessors
   package-loaded-p
   package-uri
   ;; Test compilation
   compile-tests
   build-tests
   ;; Package search
   search-packages
   describe-package
   find-provider-for-dependency
   resolve-virtual-dependency
   ;; Build state
   dump-build-state
   ;; Environment management  
   *current-environment*
   *default-environment*
   ;; Package source
   ensure-package-source
   *package-source*
   ;; Project/source info
   project
   project-name
   project-version
   project-author
   project-description
   project-platform
   project-sources
   project-tests
   project-benchmarks
   project-examples
   project-experiments
   project-docs
   project-data
   project-dependencies
   project-provides
   project-resources
   project-uri
   path
   locatable
   hashable
   ;; Internal - for parallel-build
   source-info
   source-info-defines
   source-info-requires
   target-uri
   source-uri
   build-result
   start-time
   end-time
   compilation-errors
   compilation-warnings
   compilation-status
   build-input-status
   load-source
   compile-source))

(in-package :epsilon.tool.build)

;; Create build logger
(defparameter *log* (log:get-logger "epsilon.tool.build")
  "Logger for build system messages")

;; Global default environment for backward compatibility
(defvar *default-environment* nil
  "Default build environment for backward compatibility")

(defvar *current-environment* nil
  "The current build environment (dynamically bound during builds)")

;; Backward compatibility variables
(defvar *error-behavior* :halt
  "How to handle compilation errors (DEPRECATED - use environment)")

(defvar *warning-behavior* :ignore
  "How to handle compilation warnings (DEPRECATED - use environment)")

(defvar *parallel-compilation* nil
  "Whether to use parallel compilation (DEPRECATED - use environment)")

;; Re-export key functions from build-environment
(defun make-build-environment (&rest args)
  "Create a new build environment"
  (apply #'build-env:make-build-environment args))

(defun current-environment ()
  "Get the current build environment, creating default if needed"
  (or *current-environment*
      *default-environment*
      (setf *default-environment* (make-build-environment))))

(defun local-packages ()
  (proto:make-filesystem-source (path::user-directory)))

;; Backward compatibility delegating to current environment
(defun ensure-package-source ()
  "Ensure package source is initialized for the current environment"
  (build-env:ensure-package-source (current-environment)))

(defun add-package-repo (path)
  "Add an additional package repository to current environment"
  (build-env:add-package-repo (current-environment) path))

;; Module registry - delegating to current environment
(defun get-package (name &key (error-p nil))
  "Get package-info for a given package name, or NIL if not found"
  (build-env:get-package (current-environment) name :error-p error-p))

(defun register-package (name location &key project)
  "Register a package in the current environment"
  (build-env:register-package (current-environment) name location :project project))

(defun mark-package-loaded (package-name)
  "Mark a package as loaded with current timestamp"
  (build-env:mark-package-loaded (current-environment) package-name))

(defun is-package-loaded (package-name)
  "Check if a package is already loaded"
  (build-env:is-package-loaded (current-environment) package-name))

(defun list-packages ()
  "List all registered packages"
  (map:keys (build-env:environment-loaded-packages (current-environment))))

;; Re-export package-info accessors
(defun package-loaded-p (package-info)
  (build-env:package-loaded-p package-info))

(defun package-uri (package-info)
  (build-env:package-location package-info))

(defun find-provider-for-dependency (dep-name)
  "Find a module that provides the given dependency name.
   Returns the module name that provides it, or NIL if none found."
  (let ((current-platform (string-downcase (env::platform))))
    ;; First, check if the dependency is directly available as a module
    (if (is-package-loaded dep-name)
        dep-name
        ;; Otherwise, look for a platform-specific module that provides it
        (loop for package-name in (list-packages)
              for package-info = (get-package package-name)
              for package-dir = (package-uri package-info)
              for package-file = (path:uri-merge package-dir "package.lisp")
              when (fs:exists-p package-file)
                do (let* ((plist (with-open-file (stream (path:path-from-uri package-file))
                                   (read stream)))
                          (platform (getf plist :platform))
                          (provides (getf plist :provides)))
                     ;; Check if this module provides the dependency and matches current platform
                     (when (and provides
                                (or (not platform) ; platform-agnostic
                                    (string= platform current-platform)) ; matches current platform
                                (member dep-name provides :test #'string=))
                       (return package-name)))))))

(defun resolve-virtual-dependency (dep-name)
  "Resolve a virtual dependency to an actual module name that provides it"
  (or (find-provider-for-dependency dep-name)
      dep-name)) ; Return original name if no provider found

(defun search-packages (&key name provides requires platform author description
                           name-pattern provides-pattern requires-pattern
                           loaded-only unloaded-only)
  "Search for modules matching the given criteria.
   
   Exact match parameters:
   - NAME: Module name must match exactly
   - PROVIDES: Module must provide this exact symbol
   - REQUIRES: Module must require this exact dependency
   - PLATFORM: Module must target this exact platform
   - AUTHOR: Module author must match exactly
   
   Pattern match parameters (case-insensitive substring match):
   - NAME-PATTERN: Module name contains this pattern
   - PROVIDES-PATTERN: Any provided symbol contains this pattern
   - REQUIRES-PATTERN: Any dependency contains this pattern
   - DESCRIPTION: Module description contains this pattern
   
   Filter parameters:
   - LOADED-ONLY: Only return loaded modules
   - UNLOADED-ONLY: Only return unloaded modules
   
   Returns a list of package descriptors matching all specified criteria."
  (let* ((source (ensure-package-source))
         (all-packages (proto:list-packages source))
         (results '()))
    
    (dolist (package-name all-packages)
      (let* ((metadata (proto:load-package-metadata source package-name))
             (module-provides (getf metadata :provides))
             (module-requires (getf metadata :dependencies))
             (module-platform (getf metadata :platform))
             (module-author (getf metadata :author))
             (module-desc (getf metadata :description))
             (package-info (get-package package-name))
             (package-loaded (and package-info (package-loaded-p package-info)))
             (matches t))
        
        ;; Check exact matches
        (when (and name (not (string= name package-name)))
          (setf matches nil))
        
        (when (and provides matches)
          (unless (and module-provides
                       (member provides module-provides :test #'string=))
            (setf matches nil)))
        
        (when (and requires matches)
          (unless (and module-requires
                       (member requires module-requires :test #'string=))
            (setf matches nil)))
        
        (when (and platform matches)
          (unless (and module-platform
                       (string= platform module-platform))
            (setf matches nil)))
        
        (when (and author matches)
          (unless (and module-author
                       (string= author module-author))
            (setf matches nil)))
        
        ;; Check pattern matches (case-insensitive)
        (when (and name-pattern matches)
          (unless (search (string-downcase name-pattern)
                          (string-downcase package-name))
            (setf matches nil)))
        
        (when (and provides-pattern matches module-provides)
          (unless (some (lambda (p)
                          (search (string-downcase provides-pattern)
                                  (string-downcase p)))
                        module-provides)
            (setf matches nil)))
        
        (when (and requires-pattern matches module-requires)
          (unless (some (lambda (r)
                          (search (string-downcase requires-pattern)
                                  (string-downcase r)))
                        module-requires)
            (setf matches nil)))
        
        (when (and description matches module-desc)
          (unless (search (string-downcase description)
                          (string-downcase module-desc))
            (setf matches nil)))
        
        ;; Check load status filters
        (when (and loaded-only matches (not package-loaded))
          (setf matches nil))
        
        (when (and unloaded-only matches package-loaded)
          (setf matches nil))
        
        ;; Add to results if all criteria match
        (when matches
          (push (append metadata (list :name package-name :loaded package-loaded)) results))))
    
    (nreverse results)))

(defun describe-package (package-name)
  "Return a detailed description of a package's metadata"
  (let* ((source (ensure-package-source))
         (metadata (proto:load-package-metadata source package-name))
         (location (proto:resolve-package-location source package-name))
         (package-info (get-package package-name)))
    (when metadata
      (append metadata
              (list :name package-name
                    :location (when location (path:path-string location))
                    :loaded (and package-info (package-loaded-p package-info)))))))

(defclass locatable ()
  ((uri :initarg :uri :accessor uri)))

(defclass hashable ()
  ((hash :initarg :hash :accessor hash)))

(defgeneric build-order (node &key)
  (:documentation "Produce a sequence of steps necessary to build a specific node in the project source tree."))

(defclass source-info (locatable hashable)
  ((defines :initarg :defines :accessor source-info-defines)
   (requires :initarg :requires :accessor source-info-requires)))

(defmethod path ((self locatable))
  (path:path-from-uri (uri self)))

(defun calculate-hash (uri)
  (let ((digest (digest:make-digest :sha-256)))
    (with-open-file (stream (path:path-from-uri uri) :element-type 'unsigned-byte)
      (digest:digest-stream digest stream))
    (hex:u8-to-hex
     (digest:get-digest digest))))

(defmethod print-object ((obj source-info) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path obj)
            (subseq (hash obj) 0 10))))

(defclass target-info (locatable hashable)
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
   (dependencies :initarg :dependencies :accessor project-dependencies)
   (provides :initarg :provides :accessor project-provides)
   (modules :initarg :modules :accessor project-modules)))

(defun make-source-info (uri)
  (multiple-value-bind (defines requires)
      (interpret-package (read-first-form uri))
    (when (or defines requires)
      (make-instance 'source-info
                     :uri uri
                     :hash (calculate-hash uri)
                     :defines (pkg:normalize defines)
                     :requires (mapcar #'pkg:normalize requires)))))

(defun load-project (uri)
  "Parse module definition from package.lisp in directory URI"
  (let* ((package-file "package.lisp")
         (package-path (path:path-string (path:path-merge (path:path-from-uri uri) package-file)))
         (path (cond ((probe-file package-path) package-path)
                     (t (error "No package file found: ~A" package-path)))))
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
           (dependencies-list (getf plist :dependencies))
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
                                    :dependencies dependencies-list
                                    :provides provides-list
                                    :modules map:+empty+)))
        ;; Register this package in the environment
        (register-package name uri :project project)
        project))))

(defun find-source-info (uri)
  (handler-case
      (remove-if #'null
                 (mapcar #'make-source-info
                         (fs:list-files uri ".lisp")))
    (error ()
      ;; If directory doesn't exist or can't be read, return empty list
      '())))

(defun resolve-dependencies (project)
  "Resolve and load project dependencies"
  (let ((resolved-deps '()))
    (dolist (dep-name (project-dependencies project))
      (cond
        ;; If it's already registered, load its project
        ((is-package-loaded dep-name)
         (let* ((package-info (get-package dep-name))
                (dep-dir (package-uri package-info)))
           (push (load-project dep-dir) resolved-deps)))
        
        ;; If it's epsilon.core, load it from current directory
        ((string= dep-name "epsilon.core")
         (let ((core-project (load-project (uri project))))
           (push core-project resolved-deps)))
        
        ;; Otherwise, try to find it as a subdirectory or skip
        (t
         (let ((dep-path (path:uri-merge (uri project) (format nil "~A/" dep-name))))
           (when (probe-file (path:path-from-uri dep-path))
             (let ((dep-project (load-project dep-path)))
               (push dep-project resolved-deps)))))))
    
    (nreverse resolved-deps)))

(defun collect-all-sources (project)
  "Collect sources from project and its dependencies"
  (let ((all-sources (copy-list (project-sources project)))
        (all-tests (copy-list (project-tests project))))
    
    ;; Add sources from dependencies
    (dolist (dep (resolve-dependencies project))
      (setf all-sources (append all-sources (project-sources dep)))
      (setf all-tests (append all-tests (project-tests dep))))
    
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
                   :uri target-uri
                   :hash (when (fs:exists-p target-uri)
                           (calculate-hash target-uri)))))

(defun %make-build-input (project source-info)
  "Create target information for a source file"
  (make-instance 'build-input
                 :project project
                 :source-info source-info))

(defmethod build-order ((project project) &key)
  (multiple-value-bind (all-sources all-tests)
      (collect-all-sources project)
    (declare (ignore all-tests))
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq all-sources))))

(defmethod test-build-order ((project project))
  (multiple-value-bind (all-sources all-tests)
      (collect-all-sources project)
    (declare (ignore all-sources))
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq all-tests))))

(defun read-first-form (uri)
  (with-open-file (stream (path:path-from-uri uri))
    (read stream)))

(defun interpret-package (form)
  (cond ((string-equal 'defpackage (first form))
         (values (second form)
                 (append (cdr (assoc :use (cddr form)))
                         (mapcar #'second (cdr (assoc :local-nicknames (cddr form)))))))
        ((string-equal 'in-package (first form))
         (values nil
                 (cdr form)))))

(defun sort-sources (sources)
  "Sort source files topologically based on their dependencies.
   Returns two values: sorted list and cyclic dependencies (if any)."
  (let* ((nodes (reduce (lambda (m source-info)
                         (map:assoc m (hash source-info) source-info))
                       (remove-if #'null sources)
                       :initial-value map:+empty+))
         (packages (reduce (lambda (m v)
                           (if (source-info-defines v)
                               (map:assoc m (source-info-defines v) v)
                               m))
                         sources
                         :initial-value map:+empty+))
         (visiting map:+empty+)
         (cycles nil)
         (sorted nil))
    (labels ((dep-hashes (source)
               (loop :for pkg :in (source-info-requires source)
                     :for source := (map:get packages pkg)
                     :when source
                       :collect (hash source)))
             (visit (hash path)
               (when (map:contains-p visiting hash)
                 (let ((cycle (ldiff path (member hash path))))
                   (push cycle cycles))
                 (return-from visit nil))
               (when (member hash sorted :test #'equal)
                 (return-from visit t))
               (let ((node (map:get nodes hash)))
                 (unless node
                   (return-from visit t))
                 (map:assoc! visiting hash t)
                 (let ((source (map:get nodes hash)))
                   (dolist (dep (dep-hashes source))
                     (visit dep (cons hash path))))
                 (setf visiting (map:dissoc visiting hash))
                 (push hash sorted)
                 t)))
      (dolist (source-info (map:vals nodes))
        (visit (hash source-info) nil))
      (values
       (mapcar (lambda (hash)
                 (map:get nodes hash))
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
  "Handle compilation warning based on *warning-behavior*"
  (push warning (compilation-warnings result))
  
  ;; Always suppress redefinition warnings for interactive development
  (when (redefinition-warning-p warning)
    (muffle-warning))
  
  (case *warning-behavior*
    (:halt 
     ;; Force output to be flushed before halting
     (force-output *standard-output*)
     (force-output *error-output*)
     (format t "~%BUILD HALTED: Warning encountered~%")
     (format t "Warning: ~A~%" warning)
     (print-build-output result)
     ;; Don't muffle so the warning prints normally, then halt
     (error "Build halted due to warning"))
    (:print 
     ;; Let warning print and continue (don't muffle)
     nil)
    (:ignore 
     ;; Suppress the warning
     (muffle-warning))))

(defun handle-error (error result)
  "Handle compilation error based on *error-behavior*"  
  (push error (compilation-errors result))
  (case *error-behavior*
    (:halt 
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
    (:print 
     ;; Print error but continue
     (format *error-output* "~%Build Error in ~A: ~A~%" (path:path-from-uri (source-uri (build-input result))) error)
     (print-build-output result))
    (:ignore nil)))


(defun dump-build-state ()
  "Force dump current build state - useful for debugging hangs"
  (format *error-output* "~%=== BUILD STATE DUMP (MANUAL) ===~%")
  (format *error-output* "Build timeout: ~D seconds~%" 
          (build-env:option-timeout (build-env:environment-options (current-environment))))
  (format *error-output* "Error behavior: ~A~%" *error-behavior*)
  (format *error-output* "Warning behavior: ~A~%" *warning-behavior*)
  (format *error-output* "=== END BUILD STATE DUMP ===~%")
  (force-output *error-output*))

(defun watch-operation (build-input fn)
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
               ;; Try to run with timeout
               (let ((thread (sb-thread:make-thread 
                             (lambda () 
                               (funcall fn)
                               (setf completed t))
                             :name "build-operation")))
                 (sleep 0.1) ; Give thread a moment to start
                 (let ((timeout-count 0)
                       (timeout-secs (build-env:option-timeout 
                                      (build-env:environment-options 
                                       (current-environment)))))
                   (loop while (and (sb-thread:thread-alive-p thread) 
                                    (< timeout-count (* timeout-secs 10)))
                         do (sleep 0.1)
                            (incf timeout-count))
                   (when (sb-thread:thread-alive-p thread)
                     ;; Timeout occurred - capture output and terminate
                     (setf (stdout-output result) (get-output-stream-string stdout-stream)
                           (stderr-output result) (get-output-stream-string stderr-stream))
                     (sb-thread:terminate-thread thread)
                     (error "Build operation timed out after ~D seconds" 
                            (build-env:option-timeout 
                             (build-env:environment-options 
                              (current-environment)))))
                   (sb-thread:join-thread thread))))))
      (setf (end-time result) (get-internal-real-time))
      ;; Always capture final output
      (unless (stdout-output result)
        (setf (stdout-output result) (get-output-stream-string stdout-stream)))
      (unless (stderr-output result)
        (setf (stderr-output result) (get-output-stream-string stderr-stream))))
    ;; Removed direct reporting - now handled by events
    result))

(defun compile-source (build-input)
  (watch-operation build-input
                   (lambda ()
                     (fs:make-dirs (path:path-parent (path:make-path (path:path-from-uri (target-uri build-input)))))
                     ;; Suppress verbose output - will be shown only on build abort
                     (let ((*compile-verbose* nil)
                           (*compile-print* nil))
                       (compile-file (path:path-from-uri (source-uri build-input))
                                     :output-file (path:path-from-uri (target-uri build-input))
                                     :verbose nil
                                     :print nil)
                       ;; Load the compiled file immediately
                       (load (path:path-from-uri (target-uri build-input)))))))

(defun load-source (build-input)
  (watch-operation build-input
                   (lambda ()
                     (load (path:path-from-uri (target-uri build-input))))))

;; Events now handled by logging

(defun create-concatenated-fasl (fasl-files output-path)
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

(defun concat-fasl-current-p (project concat-fasl-path)
  "Check if concatenated FASL is current relative to all source files"
  (when (probe-file concat-fasl-path)
    (let ((fasl-time (file-write-date concat-fasl-path))
          (sources (append (project-sources project) (project-tests project))))
      (every (lambda (source-info)
               (let ((source-path (path:path-from-uri (uri source-info))))
                 (or (not (probe-file source-path))
                     (<= (file-write-date source-path) fasl-time))))
             sources))))

(defun load-concatenated-fasl (project)
  "Load concatenated FASL if it exists and is current, returns T if loaded"
  (let* ((concat-fasl-rel-path (path:string-path-join 
                                "target" "package" "module.fasl"))
         (concat-fasl-path (path:path-from-uri 
                            (path:uri-merge (uri project) concat-fasl-rel-path))))
    (when (and (probe-file concat-fasl-path)
               (concat-fasl-current-p project concat-fasl-path))
      (load concat-fasl-path)
      t)))

;;; TODO unify %build and %build-test

(defun %build (project &key force)
  "Build the given build-inputs, optionally forcing compilation of all steps"
  
  (let* ((build (make-instance 'project-build
                               :project project
                               :results '()))
         (build-inputs (build-order project)))
    (let* ((results (if *parallel-compilation*
                       ;; Use parallel compilation if enabled
                       (progn
                         (unless (find-package :epsilon.tool.parallel-build)
                           (error "Parallel compilation requires epsilon.tool.parallel-build to be loaded"))
                         (funcall (find-symbol "COMPILE-PARALLEL" :epsilon.tool.parallel-build)
                                 build-inputs :force force :parallel t))
                       ;; Sequential compilation (existing behavior)
                       (let ((index 0))
                         (seq:map (lambda (build-input)
                                   (incf index)
                                   (let ((result (if force
                                                     (compile-source build-input)
                                                     (case (build-input-status build-input)
                                                       ((:target-missing
                                                         :source-newer)
                                                        (compile-source build-input))
                                                       (t
                                                        (load-source build-input))))))
                                     (when (compilation-errors result)
                                       (log:error "Compilation failed: ~A" 
                                                  (path (source-info (build-input result)))))
                                     result))
                                 build-inputs)))))
      (setf (slot-value build 'results) results)
      (setf (end-time build) (get-internal-real-time))
      
      (when (and (not force)
                 (seq:every-p (lambda (result)
                                (not (compilation-errors result)))
                              results))
        (let* ((fasl-files (seq:map (lambda (build-input)
                                      (path:path-from-uri (target-uri build-input)))
                                    build-inputs))
               (concat-fasl-rel-path (path:string-path-join 
                                      "target" "package" "module.fasl"))
               (concat-fasl-path (path:path-from-uri 
                                  (path:uri-merge (uri project) concat-fasl-rel-path))))
          (when (seq:not-empty-p fasl-files)
            (create-concatenated-fasl (seq:realize fasl-files) concat-fasl-path))))
      
      build)))

(defun %build-tests (project &key force)
  "Build only the test files for a project"
  (let* ((build (make-instance 'project-build
                               :project project
                               :results '()))
         (test-sources (project-tests project))
         (build-inputs (seq:map (fn:partial #'%make-build-input project)
                                (seq:seq test-sources)))
         (total-count (seq:count build-inputs)))
    (log:info "Building tests for ~A (~D files)" 
              (project-name project) total-count)
    (format t "~&;;; DEBUG: build-inputs count: ~D~%" total-count)
    (format t "~&;;; DEBUG: test-sources count: ~D~%" (length test-sources))
    (let* ((index 0)
           (results '()))
      (handler-case
          (seq:each (lambda (build-input)
                      (handler-case
                          (progn
                            (incf index)
                            (let* ((source-path (path:path-from-uri (source-uri build-input)))
                                   (action (if force
                                               "compiling"
                                               (case (build-input-status build-input)
                                                 ((:target-missing :source-newer) "compiling")
                                                 (t "loading"))))
                                   (result (progn
                                             (format t "~&;;; [~D/~D] ~A test file: ~A~%" 
                                                     index total-count action source-path)
                                             (force-output)
                                             (handler-case
                                                 (if force
                                                     (compile-source build-input)
                                                     (case (build-input-status build-input)
                                                       ((:target-missing :source-newer)
                                                        (compile-source build-input))
                                                       (t
                                                        (load-source build-input))))
                                               (error (e)
                                                 (format t "~&;;; ERROR loading ~A: ~A~%" source-path e)
                                                 (force-output)
                                                 nil)))))
                              (when (and result (compilation-errors result))
                                (log:error "Compilation failed: ~A" 
                                           (path (source-info (build-input result)))))
                              (push result results)))
                        (error (e)
                          (format t "~&;;; FATAL ERROR processing test file ~D: ~A~%" index e)
                          (force-output))))
                    build-inputs)
        (error (e)
          (format t "~&;;; FATAL ERROR in seq:each: ~A~%" e)
          (force-output)))
      (setf results (nreverse results))
      (setf (slot-value build 'results) results)
      (setf (end-time build) (get-internal-real-time))
      build)))

(defgeneric build-with-environment (environment package &key force)
  (:documentation "Build a package using the specified environment"))

(defmethod build-with-environment ((env build-env:build-environment) package &key force)
  "Build package sources using the given environment"
  (let* ((source (build-env:ensure-package-source env))
         (location (proto:resolve-package-location source package))
         (package-dir (if location
                          (path:path-string location)
                          (error "Unknown package: ~A. Available packages: ~A" 
                                 package (proto:list-packages source))))
         (project (load-project package-dir))
         ;; Bind dynamic variables for backward compatibility
         (*error-behavior* (build-env:policy-error-behavior 
                            (build-env:environment-policies env)))
         (*warning-behavior* (build-env:policy-warning-behavior 
                              (build-env:environment-policies env)))
         (*parallel-compilation* (build-env:option-parallel 
                                  (build-env:environment-options env)))
         (*current-environment* env))
    ;; Build dependencies first
    (dolist (dep-name (project-dependencies project))
      (let ((resolved-dep (resolve-virtual-dependency dep-name)))
        (when (not (string= resolved-dep dep-name))
          (log:debug "Resolved virtual dependency ~A to ~A" dep-name resolved-dep))
        (unless (build-env:is-package-loaded env resolved-dep)
          (let* ((dep-location (proto:resolve-package-location source resolved-dep)))
            (when dep-location
              (build-with-environment env resolved-dep :force force)
              (build-env:mark-package-loaded env resolved-dep))))))
    
    ;; Now try to load concatenated FASL if dependencies are resolved and it's up-to-date
    (unless (or force (build-env:option-force (build-env:environment-options env)))
      (when (load-concatenated-fasl project)
        (return-from build-with-environment t)))
    
    (let ((build-result (%build project :force (or force (build-env:option-force 
                                                           (build-env:environment-options env))))))
      (when build-result
        (build-env:mark-package-loaded env package))
      build-result)))

(defun build (package &key force 
                      (error-behavior :halt) 
                      (warning-behavior :ignore)
                      parallel
                      &allow-other-keys)
  "Build package sources only.
  
  PACKAGE - Package name to build (e.g., 'epsilon.core', 'http'). Looks up package from package source.
  FORCE - Force compilation of all build steps regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  PARALLEL - Use parallel compilation (experimental)"
  (let ((env (current-environment)))
    ;; Configure environment based on arguments
    (setf (build-env:policy-error-behavior (build-env:environment-policies env)) error-behavior
          (build-env:policy-warning-behavior (build-env:environment-policies env)) warning-behavior
          (build-env:option-parallel (build-env:environment-options env)) parallel
          (build-env:option-force (build-env:environment-options env)) force)
    (build-with-environment env package :force force)))

(defun build-tests (package &key force 
                           (error-behavior :halt) 
                           (warning-behavior :ignore)
                           parallel)
  "Build only the test files for a package.
  
  PACKAGE - Package name whose tests to build (e.g., 'epsilon.core', 'http').
  FORCE - Force compilation of all test files regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  PARALLEL - Use parallel compilation (experimental)"
  (let* ((source (ensure-package-source))
         (location (proto:resolve-package-location source package))
         (module-dir (if location
                         (path:path-string location)
                         (error "Unknown module: ~A. Available packages: ~A" 
                                package (proto:list-packages source))))
         (project (load-project module-dir))
         (*error-behavior* error-behavior)
         (*warning-behavior* warning-behavior)
         (*parallel-compilation* parallel))
    ;; Ensure the module itself is built and loaded first since test files depend on it
    (unless (is-package-loaded package)
      (build package :force force :parallel parallel)
      (load-package package))
    
    ;; Ensure epsilon.test is built and loaded first since test files depend on it
    (let* ((source (ensure-package-source))
           (test-location (proto:resolve-package-location source "epsilon.test")))
      (when (and test-location (not (is-package-loaded "epsilon.test")))
        (build "epsilon.test" :force force :parallel parallel)
        (load-package "epsilon.test")))
    
    (dolist (dep-name (project-dependencies project))
      (unless (or (string= dep-name "epsilon.core") ; core is already loaded
                  (string= dep-name "epsilon.test") ; test is already loaded above
                  (is-package-loaded dep-name)) ; Skip if already loaded
        (let* ((source (ensure-package-source))
               (dep-location (proto:resolve-package-location source dep-name)))
          (when dep-location
            (build dep-name :force force :parallel parallel)
            (load-package dep-name)))))
    (%build-tests project :force force)))

(defun load-package (package)
  "Load a package that has already been built.
  Returns T if successfully loaded, NIL otherwise."
  (let* ((source (ensure-package-source))
         (location (proto:resolve-package-location source package))
         (package-dir (if location
                         (path:path-string location)
                         (error "Unknown package: ~A" package)))
         (project (load-project package-dir)))
    ;; Load dependencies first
    (dolist (dep (project-dependencies project))
      (unless (or (string= dep "epsilon.core") ; core is already loaded
                  (is-package-loaded dep))
        (build dep)
        (load-package dep)))
    ;; Now load this package
    (when (load-concatenated-fasl project)
      (mark-package-loaded package)
      t)))

(defun get-test-files (package)
  "Get test file information for a package.
  Returns a list of build-input objects for test files."
  (let* ((source (ensure-package-source))
         (location (proto:resolve-package-location source package))
         (package-dir (if location
                          (path:path-string location)
                          (error "Unknown package: ~A" package)))
         (project (load-project package-dir)))
    (test-build-order project)))

(defgeneric project-resources (project resource-type)
  (:documentation "Get resources of the specified type from the project.
  Resource types: :sources :tests :benchmarks :examples :experiments :docs :data"))

(defmethod project-resources ((project project) (resource-type (eql :sources)))
  (project-sources project))

(defmethod project-resources ((project project) (resource-type (eql :tests)))
  (project-tests project))

(defmethod project-resources ((project project) (resource-type (eql :benchmarks)))
  (project-benchmarks project))

(defmethod project-resources ((project project) (resource-type (eql :examples)))
  (project-examples project))

(defmethod project-resources ((project project) (resource-type (eql :experiments)))
  (project-experiments project))

(defmethod project-resources ((project project) (resource-type (eql :docs)))
  (project-docs project))

(defmethod project-resources ((project project) (resource-type (eql :data)))
  (project-data project))

(defun get-resource-files (package resource-type)
  "Get resource file information for a package.
  Returns a list of source-info objects for the specified resource type."
  (let* ((source (ensure-package-source))
         (location (proto:resolve-package-location source package))
         (package-dir (if location
                          (path:path-string location)
                          (error "Unknown package: ~A" package)))
         (project (load-project package-dir)))
    (project-resources project resource-type)))

(defun compile-tests (package &key force parallel)
  "Compile test files for a package without running them.
  
  PACKAGE - Package name whose tests to compile (e.g., 'epsilon.core', 'http').
  FORCE - Force compilation of all test files regardless of timestamps
  PARALLEL - Use parallel compilation (experimental)"
  (let* ((source (ensure-package-source))
         (location (proto:resolve-package-location source package))
         (package-dir (if location
                          (path:path-string location)
                          (error "Unknown package: ~A" package)))
         (project (load-project package-dir))
         (*parallel-compilation* parallel))
    ;; Ensure the package itself is built first since test files depend on it
    (unless (is-package-loaded package)
      (build package :force force :parallel parallel)
      (mark-package-loaded package))
    ;; Now compile just the test files
    (let* ((build (make-instance 'project-build
                                :project project
                                :results '()))
           (test-inputs (test-build-order project)))
      (when test-inputs
        (let ((results (if *parallel-compilation*
                          (progn
                            (unless (find-package :epsilon.tool.parallel-build)
                              (error "Parallel compilation requires epsilon.tool.parallel-build to be loaded"))
                            (funcall (find-symbol "COMPILE-PARALLEL" :epsilon.tool.parallel-build)
                                    test-inputs :force force :parallel t))
                          (seq:map (lambda (build-input)
                                    (compile-source build-input))
                                  test-inputs))))
          (setf (slot-value build 'results) (seq:seq results))
          (setf (end-time build) (get-internal-real-time))
          build)))))

(defun list-available-packages (&key include-platform)
  "List all available modules from the package source.
   
   INCLUDE-PLATFORM - When T, show platform-specific modules too"
  (let* ((source (ensure-package-source))
         (all-packages (proto:list-packages source))
         (platform-packages '()))
    
    (log:debug *log* "Package source: ~A" source)
    (log:debug *log* "Raw packages found: ~A" all-packages)
    
    ;; Sort packages alphabetically
    (setf all-packages (sort all-packages #'string<))
    
    ;; Filter out platform-specific packages unless requested
    (unless include-platform
      (setf all-packages
            (remove-if (lambda (package-name)
                         (let* ((metadata (proto:load-package-metadata source package-name))
                                (platform (getf metadata :platform)))
                           (when platform
                             (push (cons package-name platform) platform-packages)
                             t)))
                       all-packages)))
    
    (log:debug *log* "Filtered packages: ~A" all-packages)
    (log:debug *log* "Platform packages: ~A" platform-packages)
    
    ;; Return the list
    (values all-packages platform-packages)))
