;;;; This package provides a package loading system with
;;;; dependency tracking and incremental compilation.

(defpackage epsilon.loader
  (:use
   cl
   epsilon.tool.common)
  (:shadow
   find-package)
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
   (protocol epsilon.protocol))
  (:export
   ;; Core functions
   get-package
   load-package
   environment
   
   ;; Environment management
   add-package-path
   
   ;; Generic resource handling
   load-package-resources
   sort-sources
   project-resources
   
   ;; REPL environment
   *environment*
   
   #:find-package
   #:name
   #:version
   #:location
   #:metadata
   #:source
   
   ;; From loader.environment
   ;; Environment class
   #:build-environment
   #:make-build-environment
   ;; Accessors
   #:environment-packages
   #:environment-config
   ;; Operations
   #:register-package-directory
   #:scan-directory-for-packages
   #:register-package
   #:is-package-loaded
   #:query-packages
   ;; Package info
   #:package-info
   #:package-info-name
   #:package-location
   #:package-loaded-p
   #:package-load-time
   #:package-metadata))

(in-package epsilon.loader)

(defclass package-info ()
  ((name :initarg :name 
         :reader package-info-name 
         :type string
         :documentation "Package name")
   (location :initarg :location 
             :reader package-location
             :documentation "Package location (path or URI)")
   (loaded-p :initarg :loaded-p 
             :accessor package-loaded-p 
             :initform nil 
             :type boolean
             :documentation "Whether package has been loaded")
   (load-time :initarg :load-time 
              :accessor package-load-time 
              :initform nil
              :documentation "When package was loaded")
   (metadata :initarg :metadata
             :accessor package-metadata
             :initform nil
             :documentation "Full package metadata including provides, dependencies, etc."))
  (:documentation "Information about a loaded or registered package"))

;;; Build Environment

(defclass build-environment ()
  ((packages :initform (map:make-map)
             :accessor environment-packages
             :documentation "Registry of all packages (map of name -> package-info)")
   (config :initarg :config
           :accessor environment-config
           :initform (map:make-map)
           :documentation "Build configuration map containing all settings"))
  (:documentation "Encapsulates all configuration and state for a build session and acts as a package source"))

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

;;; Package Discovery and Registration

(defun register-package-directory (environment path)
  "Register a single package directory (contains package.lisp)"
  (let* ((package-path (path:ensure-path path))
         (package-file (path:path-join package-path "package.lisp"))
         (file-string (path:path-string package-file)))
    (when (probe-file file-string)
      (let* ((info (with-open-file (stream file-string :if-does-not-exist nil)
                     (when stream (read stream))))
             (package-name (getf info :name)))
        (when package-name
          (let ((pkg-info (make-instance 'package-info
                                         :name package-name
                                         :location package-path
                                           :metadata info
                                         :loaded-p nil)))
            (setf (environment-packages environment)
                  (map:assoc (environment-packages environment)
                             package-name pkg-info))))))))

(defun scan-directory-for-packages (environment path)
  "Scan directory for package subdirectories and register them"
  (let ((base-path (path:ensure-path path)))
    (when (probe-file (path:path-string base-path))
      (dolist (entry-path (path:list-directory base-path :type :directories))
        (register-package-directory environment entry-path)))))

;;; Module Registry

(defun get-package (environment name &key (error-p nil))
  "Get package-info for a given package name, or NIL if not found"
  (or (map:get (environment-packages environment) name)
      (and error-p
           (error "Package not found: ~A" name))))

(defun register-package (environment name location metadata)
  "Register a package in the environment registry"
  (let ((existing (map:get (environment-packages environment) name)))
    (if existing
        ;; Update existing package info
        (progn
          (setf (slot-value existing 'location) location
                (slot-value existing 'metadata) metadata)
          existing)
        ;; Create new package info
        (let ((package-info (make-instance 'package-info
                                           :name name
                                           :location location
                                           :metadata metadata
                                           :loaded-p nil)))
          (map:assoc! (environment-packages environment) name package-info)
          package-info))))

(defun is-package-loaded (environment name)
  "Check if a package has been loaded"
  (let ((package-info (get-package environment name)))
    (and package-info (package-loaded-p package-info))))

(defun mark-package-loaded (environment name)
  "Mark a package as loaded"
  (let ((package-info (get-package environment name)))
    (when package-info
      (setf (package-loaded-p package-info) t
            (package-load-time package-info) (get-universal-time)))))

;;; Query Operations

(defun query-packages (environment &key name provides loaded-only predicate)
  "Query packages matching specified criteria. 
   Packages provide themselves by default (e.g., epsilon.core provides epsilon.core)."
  (let ((results '())
        (current-platform (string-downcase (symbol-name (env:platform)))))
    
    ;; Search all packages
    (loop for package-info in (map:vals (environment-packages environment))
          for pkg-name = (package-info-name package-info)
          for metadata = (package-metadata package-info)
          for pkg-provides = (or (getf metadata :provides) (list pkg-name))
          for pkg-platform = (getf metadata :platform)
          when (and (or (not name)
                        (string= name pkg-name))
                    (or (not provides)
                        ;; Package provides itself by default
                        (string= provides pkg-name)
                        ;; Or explicitly provides the capability
                        (member provides pkg-provides :test #'string=))
                    (or (not pkg-platform)
                        (string= pkg-platform current-platform))
                    (or (not predicate)
                        (funcall predicate package-info)))
            collect package-info into loaded-results
          finally (setf results loaded-results))
    
    ;; Filter by loaded-only if requested
    (when loaded-only
      (setf results (remove-if-not #'package-loaded-p results)))
    
    results))

(defun find-package (environment &key name provides)
  "Find exactly one package matching the given criteria.
   Raises an error if no packages match or if multiple packages match.
   This is a convenience wrapper around query-packages."
  (let ((matches (query-packages environment 
                                 :name name 
                                 :provides provides)))
    (cond
      ((null matches)
       (error "No package found matching criteria~@[ name=~A~]~@[ provides=~A~]"
              name provides))
      ((> (length matches) 1)
       (error "Multiple packages found matching criteria~@[ name=~A~]~@[ provides=~A~]: ~{~A~^, ~}"
              name provides (mapcar #'package-info-name matches)))
      (t (first matches)))))


;;;; Original loader.lisp code continues here

(defvar *environment* nil
  "The default build environment.")

(defun environment ()
  "Get or create the default environment"
  (unless *environment*
    (setf *environment* (make-build-environment))
    ;; Scan default Epsilon src directory
    (let ((epsilon-home (or (sb-ext:posix-getenv "EPSILON_HOME") ".")))
      (scan-directory-for-packages *environment* 
                                               (path:path-join epsilon-home "src"))))
  *environment*)

(defun add-package-path (environment path)
  "Add a package directory to the environment - can be a single package or directory of packages"
  (scan-directory-for-packages environment path))


(defun package-uri (package-info)
  (package-location package-info))

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

(defun load-project (uri &optional environment)
  "Parse module definition from package.lisp in directory URI"
  (log:debug "load-project, uri = ~A" uri)
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
        ;; Register this package in the environment with full metadata
        ;; Note: environment parameter is optional for backward compatibility
        (when environment
          (register-package environment name uri plist))
        project))))

(defun find-source-info (uri)
  (handler-case
      (remove-if #'null
                 (mapcar #'make-source-info
                         (fs:list-files uri ".lisp")))
    (error ()
      ;; If directory doesn't exist or can't be read, return empty list
      '())))

(defun collect-all-sources (project)
  "Collect sources from this project only (dependencies should already be loaded)"
  (let ((all-sources (copy-list (project-sources project)))
        (all-tests (copy-list (project-tests project))))
    ;; Don't include dependency sources - they should be loaded separately
    ;; via load-package's dependency handling
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
      (collect-all-sources project)     ;TODO make sure this is only collecting the local sources
    (declare (ignore all-tests))
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq all-sources))))

;; test-build-order removed - use project-resources with :tests instead

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
  "Sort source files topologically based on their intra-package dependencies.
   Only considers dependencies between files in the provided source list.
   External package dependencies are ignored (should be loaded separately).
   Returns two values: sorted list and cyclic dependencies (if any)."
  (let* ((nodes (reduce (lambda (m source-info)
                         (map:assoc m (hash source-info) source-info))
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
    (labels ((dep-hashes (source)
               ;; Only include dependencies that are defined in this source set
               (loop :for pkg :in (source-info-requires source)
                     :for dep-source := (map:get local-packages pkg)
                     :when dep-source  ;; Only if it's a local dependency
                       :collect (hash dep-source)))
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
               ;; Try to run with timeout
               (let ((thread (sb-thread:make-thread 
                             (lambda () 
                               (funcall fn)
                               (setf completed t))
                             :name "build-operation")))
                 (sleep 0.1) ; Give thread a moment to start
                 (let ((timeout-count 0)
                       (timeout-secs (map:get (environment-config environment) :timeout 60)))
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
                            (map:get (environment-config environment) :timeout 60)))
                   (sb-thread:join-thread thread))))))
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
                       (log:info "Compiling: ~A" (path:path-from-uri (source-uri build-input)))
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
         (build-inputs (build-order project))
         (results (let ((index 0))
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
                             build-inputs))))
    (setf (slot-value build 'results) results)
    (setf (end-time build) (get-internal-real-time))
      
    (when (and (not force)
               (seq:every-p (lambda (result)
                              (not (compilation-errors result)))
                            results))
      (let* ((fasl-files (seq:map (lambda (build-input)
                                    (path:path-from-uri (target-uri build-input)))
                                  build-inputs))
             (binary-rel-path (path:string-path-join 
                                    "target" "package" "module.fasl"))
             (binary-path (path:path-from-uri 
                                (path:uri-merge (uri project) binary-rel-path))))
        (when (seq:not-empty-p fasl-files)
          (create-binary (seq:realize fasl-files) binary-path))))
      
    build))

(defun load-package (environment package &key force compile-only)
  "Ensure a package is available in the environment.
  Compiles if necessary, then loads unless compile-only is true.
  
  PACKAGE - Package name to load (e.g., 'epsilon.core', 'http')
  FORCE - Force compilation of all build steps regardless of timestamps
  COMPILE-ONLY - Only compile, don't load (for backward compatibility with build)
  
  Returns T if successfully loaded/compiled, NIL otherwise."
  (let* ((package-info (get-package environment package))
         (location (when package-info (package-location package-info)))
         (package-dir (if location
                          location
                          (error "Unknown package: ~A" package)))
         (project (handler-case
                      (progn
                        (log:debug "Calling load-project")
                        (load-project package-dir environment))
                    (error (e)
                      (log:error "Error in load-project for ~A: ~A" package e)
                      (error e)))))
    ;; Build dependencies first
    (dolist (dep-name (project-dependencies project))
      (let ((resolved-dep (find-package environment :provides dep-name)))
        (unless (package-loaded-p resolved-dep)
          (load-package environment (package-info-name resolved-dep)))))
    
    ;; Now try to load concatenated FASL if dependencies are resolved and it's up-to-date
    (unless (or force compile-only (map:get (environment-config environment) :force))
      (when (load-binary project)
        (mark-package-loaded environment package)
        (return-from load-package t)))
    
    ;; Build if needed
    (let ((build-result (%build environment project :force (or force (map:get (environment-config environment) :force)))))
      (when build-result
        (unless compile-only
          (mark-package-loaded environment package)))
      build-result)))

(defgeneric project-resources (project resource-type)
  (:documentation "Get resources of the specified type from the project.
  Resource types: :sources :tests :benchmarks :examples :experiments :docs :data"))

(defmethod project-resources ((project project) (resource-type (eql :sources)))
  (project-sources project))

(defmethod project-resources ((project project) (resource-type (eql :tests)))
  (project-tests project))

(defun load-package-resources (environment package resource-type &key force compile-only)
  "Load resources of a specific type for a package.
   ENVIRONMENT - The loader environment
   PACKAGE - Package name (e.g., 'epsilon.core')
   RESOURCE-TYPE - Type of resources (:tests, :benchmarks, :examples, etc.)
   FORCE - Force recompilation even if up-to-date
   COMPILE-ONLY - Only compile, don't load
   
   Returns T if successful, NIL otherwise."
  (let* ((package-info (get-package environment package))
         (location (when package-info (package-location package-info)))
         (package-dir (if location
                          location
                          (error "Unknown package: ~A" package)))
         (project (load-project package-dir environment))
         (resources (project-resources project resource-type)))
    
    ;; Sort resources topologically
    (multiple-value-bind (sorted-resources cycles)
        (sort-sources resources)
      (when cycles
        (error "Circular dependencies detected in ~A ~A: ~A" 
               package resource-type cycles))
      
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
