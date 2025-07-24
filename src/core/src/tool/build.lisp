;;;; This module provides a build system with content-based dependency
;;;; tracking, incremental compilation, and module management.

(defpackage epsilon.tool.build
  (:use
   cl
   epsilon.tool.common)
  (:shadow
   *modules*)
  (:local-nicknames
   (pkg epsilon.sys.pkg)
   (fs epsilon.sys.fs)
   (digest epsilon.digest)
   (fn epsilon.function)
   (hex epsilon.hex)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (path epsilon.path)
   (log epsilon.log))
  (:export
   ;; Core build functions
   build
   build-tests
   detect-platform
   configure-build-logging
   ;; Module registry
   register-module
   register-modules
   get-module
   list-modules
   list-all-modules-with-status
   ;; Module info
   module-info
   module-name
   module-uri
   module-loaded-p
   mark-module-loaded
   is-module-loaded
   ;; Module search
   search-modules
   describe-module
   find-provider-for-dependency
   resolve-virtual-dependency
   ;; Build state
   dump-build-state
   ;; Global variables
   *modules*
   *build-timeout*
   *error-behavior*
   *warning-behavior*
   ;; Project/source info
   project-name
   project-uri
   path
   locatable
   hashable))

(in-package :epsilon.tool.build)

;; Create build logger
(defparameter *build-logger* (log:get-logger "epsilon.tool.build")
  "Logger for build system messages")

;; Configure build logging
(defun configure-build-logging (&key (level :info) (verbose nil))
  "Configure logging for build operations.
   LEVEL - Log level (:trace :debug :info :warn :error)
   VERBOSE - If true, use debug level and show more details"
  (when verbose
    (setf level :debug))
  (log:set-level "epsilon.tool.build" level))

(defclass module-info ()
  ((name :initarg :name :accessor module-name :type string)
   (uri :initarg :uri :accessor module-uri :type string)
   (project :initarg :project :accessor module-project :initform nil)
   (loaded :initarg :loaded :accessor module-loaded-p :initform nil :type boolean)
   (load-time :initarg :load-time :accessor module-load-time :initform nil)))

(defvar *modules*
  map:+empty+
  "Registry of known modules as a map from module name to module-info objects")

(defvar *error-behavior* :halt
  "How to handle compilation errors: :halt, :ignore, or :print")

(defvar *warning-behavior* :ignore  
  "How to handle compilation warnings: :halt, :ignore, or :print")

(defun detect-platform ()
  "Detect current platform"
  #+darwin :darwin
  #+linux :linux
  #+win32 :windows
  #-(or darwin linux win32) :unknown)

(defun make-module-info (name uri &optional project)
  "Create a new module-info instance"
  (make-instance 'module-info
                 :name name
                 :uri uri
                 :project project
                 :loaded nil))

(defun get-module (name &key (error-p nil))
  "Get module-info for a given module name, or NIL if not found"
  (or (map:get *modules* name)
      (and error-p
           (error "Module not found: ~A" name))))

(defun register-module-info (module-info)
  "Register a module-info in the global registry"
  (map:assoc! *modules* (module-name module-info) module-info))

(defun mark-module-loaded (module-name)
  "Mark a module as loaded with current timestamp"
  (let ((info (get-module module-name)))
    (when info
      (setf (module-loaded-p info) t
            (module-load-time info) (get-internal-real-time)))))

(defun is-module-loaded (module-name)
  "Check if a module is already loaded"
  (let ((info (get-module module-name)))
    (and info (module-loaded-p info))))

(defun find-provider-for-dependency (dep-name)
  "Find a module that provides the given dependency name.
   Returns the module name that provides it, or NIL if none found."
  (let ((current-platform (string-downcase (detect-platform))))
    ;; First, check if the dependency is directly available as a module
    (if (map:contains-p *modules* dep-name)
        dep-name
        ;; Otherwise, look for a platform-specific module that provides it
        (loop for module-name in (map:keys *modules*)
              for module-info = (get-module module-name)
              for module-dir = (module-uri module-info)
              for package-file = (path:uri-merge module-dir "package.lisp")
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
                       (return module-name)))))))

(defun resolve-virtual-dependency (dep-name)
  "Resolve a virtual dependency to an actual module name that provides it"
  (or (find-provider-for-dependency dep-name)
      dep-name)) ; Return original name if no provider found

(defun search-modules (&key name provides requires platform author description
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
   
   Returns a list of module-info objects matching all specified criteria."
  (let ((results '()))
    (loop for module-name in (map:keys *modules*)
          for module-info = (get-module module-name)
          for module-dir = (module-uri module-info)
          for package-file = (path:uri-merge module-dir "package.lisp")
          when (fs:exists-p package-file)
            do (let* ((plist (with-open-file (stream (path:path-from-uri package-file))
                               (read stream)))
                      (module-provides (getf plist :provides))
                      (module-requires (getf plist :dependencies))
                      (module-platform (getf plist :platform))
                      (module-author (getf plist :author))
                      (module-desc (getf plist :description))
                      (module-loaded (module-loaded-p module-info))
                      (matches t))
                 
                 ;; Check exact matches
                 (when (and name (not (string= name module-name)))
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
                                   (string-downcase module-name))
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
                 (when (and loaded-only matches (not module-loaded))
                   (setf matches nil))
                 
                 (when (and unloaded-only matches module-loaded)
                   (setf matches nil))
                 
                 ;; Add to results if all criteria match
                 (when matches
                   (push module-info results))))
    (nreverse results)))

(defun describe-module (module-name)
  "Return a detailed description of a module's metadata"
  (let ((module-info (get-module module-name)))
    (if module-info
        (let* ((module-dir (module-uri module-info))
               (package-file (path:uri-merge module-dir "package.lisp")))
          (if (fs:exists-p package-file)
              (let* ((plist (with-open-file (stream (path:path-from-uri package-file))
                              (read stream))))
                (list :name module-name
                      :uri module-dir
                      :loaded (module-loaded-p module-info)
                      :version (getf plist :version)
                      :author (getf plist :author)
                      :description (getf plist :description)
                      :platform (getf plist :platform)
                      :provides (getf plist :provides)
                      :dependencies (getf plist :dependencies)
                      :sources (getf plist :sources)
                      :tests (getf plist :tests)))
              (list :name module-name
                    :uri module-dir
                    :loaded (module-loaded-p module-info)
                    :error "Package file not found")))
        nil)))

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
           (dependencies-list (getf plist :dependencies))
           (provides-list (getf plist :provides))
           (platform (getf plist :platform))
           (sources (sort-sources (mapcan (lambda (source-path)
                                            (let ((full-uri (path:uri-merge uri source-path)))
                                              (if (probe-file (path:path-from-uri full-uri))
                                                  (find-source-info full-uri)
                                                  '())))
                                          source-paths)))
           (tests (sort-sources (mapcan (lambda (test-path)
                                          (let ((full-uri (path:uri-merge uri test-path)))
                                            (if (probe-file (path:path-from-uri full-uri))
                                                (find-source-info full-uri)
                                                '())))
                                        test-paths))))
      
      (let ((project (make-instance 'project
                                    :uri uri
                                    :name name
                                    :version version
                                    :author author
                                    :description description
                                    :platform platform
                                    :sources sources
                                    :tests tests
                                    :dependencies dependencies-list
                                    :provides provides-list
                                    :modules map:+empty+)))
        (let ((module-info (make-module-info name uri project)))
          (register-module-info module-info))
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
        ((map:contains-p *modules* dep-name)
         (let* ((module-info (map:get *modules* dep-name))
                (dep-dir (module-uri module-info)))
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

(defmethod build-order ((project project) &key include-tests)
  (multiple-value-bind (all-sources all-tests)
      (collect-all-sources project)
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq (if include-tests
                          (append all-sources all-tests)
                          all-sources)))))

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

(defvar *build-timeout* 60
  "Timeout in seconds for individual file operations")

(defun dump-build-state ()
  "Force dump current build state - useful for debugging hangs"
  (format *error-output* "~%=== BUILD STATE DUMP (MANUAL) ===~%")
  (format *error-output* "Build timeout: ~D seconds~%" *build-timeout*)
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
                 (let ((timeout-count 0))
                   (loop while (and (sb-thread:thread-alive-p thread) 
                                    (< timeout-count (* *build-timeout* 10)))
                         do (sleep 0.1)
                            (incf timeout-count))
                   (when (sb-thread:thread-alive-p thread)
                     ;; Timeout occurred - capture output and terminate
                     (setf (stdout-output result) (get-output-stream-string stdout-stream)
                           (stderr-output result) (get-output-stream-string stderr-stream))
                     (sb-thread:terminate-thread thread)
                     (error "Build operation timed out after ~D seconds" *build-timeout*))
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

(defun %build (project &key force include-tests)
  "Build the given build-inputs, optionally forcing compilation of all steps"
  (unless force
    (when (load-concatenated-fasl project)
      (return-from %build nil)))
  
  (let* ((build (make-instance 'project-build
                               :project project
                               :results '()))
         (build-inputs (build-order project :include-tests include-tests)))
    (let* ((index 0)
           (results (seq:map (lambda (build-input)
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
                                  (log:log-error "Compilation failed: ~A" 
                                                 (path (source-info (build-input result)))))
                                result))
                            build-inputs)))
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
    (let* ((index 0)
           (results (seq:map (lambda (build-input)
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
                                  (log:log-error "Compilation failed: ~A" 
                                                 (path (source-info (build-input result)))))
                                result))
                            build-inputs)))
      (setf (slot-value build 'results) results)
      (setf (end-time build) (get-internal-real-time))
      build)))

(defun build (module &key force 
                      (error-behavior :halt) 
                      (warning-behavior :ignore)
                      include-tests)
  "Build module sources and optionally tests.
  
  MODULE - Module name to build (e.g., 'epsilon.core', 'http'). Looks up module directory from registry.
  FORCE - Force compilation of all build steps regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  INCLUDE-TESTS - Also build test files (default NIL)"
  (let* ((module-info (get-module module))
         (module-dir (if module-info
                         (module-uri module-info)
                         (error "Unknown module: ~A. Available modules: ~A" 
                                module (map:keys *modules*))))
         (project (load-project module-dir))
         (*error-behavior* error-behavior)
         (*warning-behavior* warning-behavior))
    (dolist (dep-name (project-dependencies project))
      (let ((resolved-dep (resolve-virtual-dependency dep-name)))
        (when (not (string= resolved-dep dep-name))
          (log:debug "Resolved virtual dependency ~A to ~A" dep-name resolved-dep))
        (when (and (map:contains-p *modules* resolved-dep)
                   (not (is-module-loaded resolved-dep))) ; Skip if already loaded
          (let* ((dep-info (get-module resolved-dep))
                 (dep-dir (module-uri dep-info))
                 (dep-project (load-project dep-dir)))
            (unless (load-concatenated-fasl dep-project)
              (build resolved-dep :force force :include-tests nil))
            (mark-module-loaded resolved-dep)))))
    (let ((build-result (%build project :force force :include-tests include-tests)))
      (when build-result
        (mark-module-loaded module))
      build-result)))

(defun build-tests (module &key force 
                           (error-behavior :halt) 
                           (warning-behavior :ignore))
  "Build only the test files for a module.
  
  MODULE - Module name whose tests to build (e.g., 'epsilon.core', 'http').
  FORCE - Force compilation of all test files regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print"
  (let* ((module-info (get-module module))
         (module-dir (if module-info
                         (module-uri module-info)
                         (error "Unknown module: ~A. Available modules: ~A" 
                                module (map:keys *modules*))))
         (project (load-project module-dir))
         (*error-behavior* error-behavior)
         (*warning-behavior* warning-behavior))
    (dolist (dep-name (project-dependencies project))
      (when (and (map:contains-p *modules* dep-name)
                 (not (string= dep-name "epsilon.core")) ; core is already loaded
                 (not (is-module-loaded dep-name))) ; Skip if already loaded
        (let* ((dep-info (get-module dep-name))
               (dep-dir (module-uri dep-info))
               (dep-project (load-project dep-dir)))
          (unless (load-concatenated-fasl dep-project)
            (build dep-name :force force :include-tests nil))
          (mark-module-loaded dep-name))))
    (%build-tests project :force force)))

;;; Module registration

(defun find-module-directories (base-dir)
  "Find all directories containing package.lisp files under base-dir/src/
   Returns a list of directory path strings suitable for register-module"
  (let ((module-base (path:uri-merge base-dir "src/"))
        (module-dirs '()))
    (when (fs:exists-p module-base)
      ;; Debug output for Windows
      #+win32 (format t ";;; Searching for modules in: ~A~%" (path:path-from-uri module-base))
      ;; List all entries in the module directory
      (dolist (entry-name (fs:list-dir (path:path-from-uri module-base)))
        (let* ((entry-path (path:string-path-join "src" entry-name))
               (entry-uri (path:uri-merge base-dir entry-path))
               (package-file (path:uri-merge entry-uri "package.lisp")))
          (when (and (fs:dir-p (path:path-from-uri entry-uri))
                     (fs:exists-p package-file))
            (push entry-path module-dirs)))))
    (nreverse module-dirs)))

(defun module-applicable-p (module-dir)
  "Check if module is applicable to current platform"
  (let* ((package-file (path:uri-merge module-dir "package.lisp")))
    (when (fs:exists-p package-file)
      ;; TODO validating module loader. Read a sexp into a hamt +
      ;; vector structure, and check it using catalog.
      (let* ((plist (with-open-file (stream (path:path-from-uri package-file))
                      (read stream)))
             (platform (getf plist :platform)))
        ;; Module is applicable if:
        ;; 1. No platform specified (platform-agnostic)
        ;; 2. Platform matches current platform
        (or (not platform)
            (string= platform (string-downcase (detect-platform))))))))

(defun register-modules (&key base-dir)
  "Discover and register all applicable modules found under base-dir/src/"
  ;; Default base-dir at runtime, not compile time
  (unless base-dir
    (setf base-dir (path:make-path
                    #+win32 (namestring (truename "."))
                    #-win32 (sb-unix:posix-getcwd))))
  
  (let ((module-paths (find-module-directories base-dir))
        (registered-count 0)
        (skipped-count 0))
    
    ;; Register each module using register-module for list-modules functionality
    (dolist (module-path module-paths)
      (handler-case 
          (progn
            (register-module module-path :silent t)
            (incf registered-count))
        (error ()
          ;; Skip modules that can't be registered (e.g., malformed package.lisp)
          (incf skipped-count))))
    
    ;; Return count of successfully registered modules
    registered-count))

(defun register-module (module-spec &key silent)
  "Register a single module regardless of platform compatibility.
   
   MODULE-SPEC can be:
   - A string pathname to a directory containing package.lisp
   
   SILENT suppresses individual registration messages."
  (let* ((module-dir-path (cond
                           ((stringp module-spec)
                            (if (or (char= (char module-spec 0) #\/)
                                    #+(or windows win32)
                                    (and (>= (length module-spec) 3)
                                         (char= (char module-spec 1) #\:)
                                         (member (char module-spec 2) '(#\/ #\\))))
                                (substitute #\/ #\\ module-spec)  ; absolute path - normalize separators
                                (let ((cwd #+win32 (substitute #\/ #\\ (sb-ext:native-namestring (truename ".")))
                                           #-win32 (sb-unix:posix-getcwd)))
                                  (path:string-path-join cwd (substitute #\/ #\\ module-spec))))) ; relative path - normalize separators
                           (t 
                            (error "Unsupported module-spec type: ~A" module-spec))))
         (module-dir (path:make-path module-dir-path))
         (package-file (path:uri-merge module-dir "package.lisp")))
    
    ;; Validate package file exists
    (unless package-file
      (error "Package file not found: ~A" (path:path-from-uri package-file)))
    
    ;; Parse package file and register module (skip platform check)
    (let* ((plist (with-open-file (stream (path:path-from-uri package-file))
                    (read stream)))
           (module-name (getf plist :name)))
      (unless module-name
        (error "Module name not found in package file: ~A" (path:path-from-uri package-file)))
      
      ;; Register the module regardless of platform compatibility
      (let ((module-info (make-module-info module-name module-dir)))
        (register-module-info module-info))
      (unless silent
        (let ((platform (getf plist :platform))
              (applicable (module-applicable-p module-dir)))
          (format t ";;   Registered module: ~a -> ~a~@[ (~A~@[, unsupported~])~]~%" 
                  module-name 
                  (path:path-from-uri module-dir)
                  platform
                  (not applicable))))
      
      ;; Return the module name
      module-name)))

(defun list-modules (&key include-platform)
  "List all registered modules.
   
   INCLUDE-PLATFORM - When T, show platform-specific modules too"
  (let ((all-modules (seq:seq (map:keys *modules*)))
        (platform-modules '()))
    
    ;; Sort modules alphabetically - convert to list, sort, then back to seq
    (setf all-modules (seq:from-list (sort (seq:realize all-modules) #'string<)))
    
    ;; Filter out platform-specific modules unless requested
    (unless include-platform
      (setf all-modules
            (seq:filter (lambda (module-name)
                          (let* ((module-info (get-module module-name))
                                 (module-dir (module-uri module-info))
                                 (package-file (path:uri-merge module-dir "package.lisp"))
                                 (plist (with-open-file (stream (path:path-from-uri package-file))
                                          (read stream)))
                                 (platform (getf plist :platform)))
                            (if platform
                                (progn
                                  (push (cons module-name platform) platform-modules)
                                  nil)
                                t)))
                        all-modules)))
    
    ;; Return the list
    (values (seq:realize all-modules)
            platform-modules)))

(defun list-all-modules-with-status ()
  "List all modules with their platform compatibility status.
   Returns a list of (name . status) where status is :supported, :unsupported, or :unknown"
  (let ((all-modules (seq:seq (map:keys *modules*)))
        (current-platform (detect-platform))
        (module-status '()))
    
    ;; Sort modules alphabetically
    (setf all-modules (seq:from-list (sort (seq:realize all-modules) #'string<)))
    
    ;; Check each module's platform compatibility
    (dolist (module-name (seq:realize all-modules))
      (let* ((module-info (get-module module-name))
             (module-dir (module-uri module-info))
             (package-file (path:uri-merge module-dir "package.lisp"))
             (plist (with-open-file (stream (path:path-from-uri package-file))
                      (read stream)))
             (platform (getf plist :platform))
             (status (cond
                       ((not platform) :supported)  ; platform-agnostic
                       ((string= platform (string-downcase current-platform)) :supported)
                       (t :unsupported))))
        (push (cons module-name status) module-status)))
    
    (nreverse module-status)))
