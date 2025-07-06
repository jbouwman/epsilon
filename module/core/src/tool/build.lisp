(defpackage epsilon.tool.build
  (:use
   cl
   epsilon.tool.common)
  (:shadow
   *modules*)
  (:local-nicknames
   (pkg epsilon.sys.pkg)
   (fs epsilon.sys.fs)
   (digest epsilon.lib.digest)
   (fn epsilon.lib.function)
   (hex epsilon.lib.hex)
   (map epsilon.lib.map)
   (seq epsilon.lib.sequence)
   (str epsilon.lib.string)
   (uri epsilon.lib.uri)
   (yaml epsilon.lib.yaml))
  (:export
   build
   register-module
   register-modules))

(in-package :epsilon.tool.build)

(defvar *modules*
  map:+empty+
  "Registry of known modules as a map from module name to directory URI")

(defvar *error-behavior* :halt
  "How to handle compilation errors: :halt, :ignore, or :print")

(defvar *warning-behavior* :ignore  
  "How to handle compilation warnings: :halt, :ignore, or :print")

(defun detect-platform ()
  "Detect current platform"
  #+darwin :darwin
  #+linux :linux
  #+windows :windows
  #-(or darwin linux windows) :unknown)

(defun platform-package-file (platform)
  "Get package file name for platform"
  (case platform
    (:core "package-core.yaml")
    (:darwin "package-darwin.yaml")
    (:linux "package-linux.yaml")
    (:windows "package-windows.yaml")
    (t "package.yaml")))

(defclass locatable ()
  ((uri :initarg :uri :accessor uri)))

(defclass hashable ()
  ((hash :initarg :hash :accessor hash)))

(defgeneric build-order (node)
  (:documentation "Produce a sequence of steps necessary to build a specific node in the project source tree."))

(defclass source-info (locatable hashable)
  ((defines :initarg :defines :accessor source-info-defines)
   (requires :initarg :requires :accessor source-info-requires)))

(defmethod path ((self locatable))
  (uri:path (uri self)))

(defun calculate-hash (uri)
  (let ((digest (digest:make-digest :sha-256)))
    (with-open-file (stream (uri:path uri) :element-type 'unsigned-byte)
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
   (reporter :initarg :reporter
             :reader reporter)
   (start-time :initform (get-internal-real-time)
               :reader start-time)
   (end-time :initform nil
             :accessor end-time)))

(defmethod build-order ((node build-input))
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
  "Parse module definition from package.yaml in directory URI"
  (let* ((package-file "package.yaml")
         (path (uri:path (uri:merge uri package-file))))
    
    (unless (probe-file path)
      (error "No package file found: ~A" path))
    
    (let* ((yaml-value (yaml:parse-file path))
           (_ (format t "~&;;   DEBUG: Loading YAML from ~A~%" path))
           (_ (format t "~&;;   DEBUG: YAML has ~A top-level entries~%" (length yaml-value)))
           (defs (map:from-pairs yaml-value))
           (sources (when (map:get defs "sources")
                      (sort-sources (mapcan (lambda (source-path)
                                              (find-source-info (uri:merge uri source-path)))
                                            (map:get defs "sources")))))
           (tests (when (map:get defs "tests")
                    (sort-sources (mapcan (lambda (test-path)
                                            (find-source-info (uri:merge uri test-path)))
                                          (map:get defs "tests")))))
           (dependencies (map:get defs "dependencies"))
           (modules (map:get defs "modules")))
      
      
      (let ((project (make-instance 'project
                                    :uri uri
                                    :name (map:get defs "name")
                                    :version (map:get defs "version")
                                    :author (map:get defs "author")
                                    :description (map:get defs "description")
                                    :platform (map:get defs "platform")
                                    :sources sources
                                    :tests tests
                                    :dependencies (or dependencies '())
                                    :modules (or modules map:+empty+))))
        (map:assoc! *modules* (map:get defs "name") uri)
        project))))

(defun find-source-info (uri)
  (remove-if #'null
             (mapcar #'make-source-info
                     (fs:list-files uri ".lisp"))))

(defun resolve-dependencies (project)
  "Resolve and load project dependencies"
  (let ((resolved-deps '()))
    (dolist (dep-name (project-dependencies project))
      (cond
        ;; If it's already registered, load its project
        ((map:contains-p *modules* dep-name)
         (let ((dep-dir (map:get *modules* dep-name)))
           (push (load-project dep-dir) resolved-deps)))
        
        ;; If it's epsilon.core, load it from current directory
        ((string= dep-name "epsilon.core")
         (let ((core-project (load-project (uri project))))
           (push core-project resolved-deps)))
        
        ;; Otherwise, try to find it as a subdirectory or skip
        (t
         (let ((dep-path (uri:merge (uri project) (format nil "~A/" dep-name))))
           (when (probe-file (uri:path dep-path))
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
  "Create target information for a source file
   
   Follows URI path best practices using uri:path-join utility:
   - Proper directory/file path distinction
   - Avoids double slashes in path construction  
   - Clean, readable path construction"
  (let* ((project (project build-input))
         (project-path (path project))
         (source-rel-path (subseq (path (source-info build-input))
                                  (length project-path)))
         (target-rel-path (fs:replace-extension source-rel-path "fasl"))
         (target-path (uri:path-join "target" "lisp" target-rel-path))
         (target-uri (uri:merge (uri project) target-path)))
    (make-instance 'target-info
                   :uri target-uri
                   :hash (when (fs:exists-p target-uri)
                           (calculate-hash target-uri)))))

(defun %make-build-input (project source-info)
  "Create target information for a source file"
  (make-instance 'build-input
                 :project project
                 :source-info source-info))

(defmethod build-order ((project project))
  (multiple-value-bind (all-sources all-tests)
      (collect-all-sources project)
    (seq:map (fn:partial #'%make-build-input project)
             (seq:seq (append all-sources all-tests)))))

(defun read-first-form (uri)
  (with-open-file (stream (uri::path uri))
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
        ((< (fs:modification-time (uri:path (target-uri build-input)))
            (fs:modification-time (uri:path (source-uri build-input))))
         :source-newer)
        (t
         :up-to-date)))

(defun print-build-output (result)
  "Print stdout and stderr output from build result"
  (when (stdout-output result)
    (format *error-output* "~%--- Build Output ---~%~A~%" (stdout-output result)))
  (when (stderr-output result)
    (format *error-output* "~%--- Error Output ---~%~A~%" (stderr-output result))))

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
    (muffle-warning)
    (return-from handle-warning))
  
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
     (format t "~%BUILD HALTED: Error encountered~%")
     (format t "Error: ~A~%" error)
     (print-build-output result)
     (error error))
    (:print 
     ;; Print error but continue
     (format t "Error: ~A~%" error))
    (:ignore nil)))

(defun watch-operation (build-input fn)
  (let ((result (make-instance 'build-result
                               :build-input build-input))
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream)))
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
               (funcall fn))))
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
                     (fs:make-dirs (uri:parent (target-uri build-input)))
                     ;; Set compiler policy to ensure warnings are visible
                     (let ((*compile-verbose* t)
                           (*compile-print* t))
                       (compile-file (uri:path (source-uri build-input))
                                     :output-file (uri:path (target-uri build-input))
                                     :verbose t
                                     :print t)))))

(defun load-source (build-input)
  (watch-operation build-input
                   (lambda ()
                     (load (uri:path (target-uri build-input))))))

(defmethod event ((build project-build) type data)
  (event (reporter build) type data))

(defun %build (project &key force reporter)
  "Build the given build-inputs, optionally forcing compilation of all steps"
  (let* ((build (make-instance 'project-build
                               :project project
                               :reporter reporter
                               :results '()))
         (build-inputs (build-order project)))
    (event build :start build)
    (let ((results (seq:seq (seq:realize (seq:map (lambda (build-input)
                                                    (event build :start-compile build-input)
                                                    (let ((result (if force
                                                                      (compile-source build-input)
                                                                      (case (build-input-status build-input)
                                                                        ((:target-missing
                                                                          :source-newer)
                                                                         (compile-source build-input))
                                                                        (t
                                                                         (load-source build-input))))))
                                                      (event build :end-compile result)
                                                      result))
                                                  build-inputs)))))
      (setf (slot-value build 'results) results)
      (setf (end-time build) (get-internal-real-time))
      (event build :end build)
      build)))

(defun build (module &key force 
                      (error-behavior :halt) 
                      (warning-behavior :ignore)
                      (reporter (make-instance 'shell-build-report)))
  "Build module sources and optionally tests.
  
  MODULE - Module name to build (e.g., 'epsilon.core', 'http'). Looks up module directory from registry.
  FORCE - Force compilation of all build steps regardless of timestamps
  ERROR-BEHAVIOR - How to handle compilation errors: :halt (default), :ignore, :print
  WARNING-BEHAVIOR - How to handle compilation warnings: :ignore (default), :halt, :print
  REPORTER - Reporter instance for build progress"
  ;; Register modules first to populate *modules*
  (register-modules)
  (let* ((module-dir (map:get *modules* module))
         (project (progn
                    (unless module-dir
                      (error "Unknown module: ~A. Available modules: ~A" 
                             module (map:keys *modules*)))
                    (load-project module-dir)))
         (*error-behavior* error-behavior)
         (*warning-behavior* warning-behavior))
    (%build project :force force :reporter reporter)))

(defun operation-wall-time (result)
  (if (and (start-time result) (end-time result))
      (/ (- (end-time result) (start-time result))
         internal-time-units-per-second)
      0))

(defun report-result (result)
  (let* ((build-input (build-input result))
         (source-path (path (source-info build-input)))
         (action (if (compilation-errors result) "FAILED" 
                     (if (compilation-warnings result) "COMPILED" "LOADED")))
         (warning-count (length (compilation-warnings result)))
         (error-count (length (compilation-errors result))))
    (format t "  ~a ~a" action source-path)
    (when (or (> warning-count 0) (> error-count 0))
      (format t " (")
      (when (> warning-count 0)
        (format t "~d warning~:p" warning-count))
      (when (> error-count 0)
        (when (> warning-count 0)
          (format t " "))
        (format t "~d error~:p" error-count))
      (format t ")"))
    (format t "~%")
    
    ;; Show detailed errors/warnings based on behavior settings
    (when (and (compilation-errors result) 
               (eq *error-behavior* :print))
      (dolist (error (compilation-errors result))
        (format t "    ERROR: ~a~%" error)))
    
    (when (and (compilation-warnings result)
               (eq *warning-behavior* :print))
      (dolist (warning (compilation-warnings result))
        (format t "    WARNING: ~a~%" warning)))
    
    (when (compilation-errors result)
      (when (stdout-output result)
        (format t "    stdout: ~a~%" (stdout-output result)))
      (when (stderr-output result)
        (format t "    stderr: ~a~%" (stderr-output result))))))

;;; Build Reporter classes

(defclass shell-build-report ()
  ((failure-count :initform 0)
   (max-failures :initform 10)))

(defmethod event ((formatter shell-build-report) (event-type (eql :start)) build)
  (format t "~&Building project: ~a~%~%" (uri:path (uri (slot-value build 'project)))))

(defmethod event ((formatter shell-build-report) (event-type (eql :start-compile)) build-input)
  (let* ((source-path (path (source-info build-input)))
         (status (build-input-status build-input))
         (action (case status
                   ((:target-missing :source-newer) "COMPILING")
                   (t "LOADING"))))
    (format t ";;   ~a ~a~%" action source-path)))

(defmethod event ((formatter shell-build-report) (event-type (eql :end-compile)) result)
  (let* ((build-input (build-input result))
         (source-path (path (source-info build-input)))
         (status (compilation-status result))
         (warning-count (length (compilation-warnings result)))
         (error-count (length (compilation-errors result)))
         (time (operation-wall-time result)))
    (format t ";;     ~a ~,3fs" 
            (if (compilation-errors result) "FAILED" "OK")
            time)
    (when (or (> warning-count 0) (> error-count 0))
      (format t " (")
      (when (> warning-count 0)
        (format t "~d warning~:p" warning-count))
      (when (> error-count 0)
        (when (> warning-count 0)
          (format t " "))
        (format t "~d error~:p" error-count))
      (format t ")"))
    (format t "~%")
    
    ;; Show errors if they exist
    (when (compilation-errors result)
      (dolist (error (compilation-errors result))
        (format t ";;       ERROR: ~a~%" error)))))

(defmethod event ((formatter shell-build-report) (event-type (eql :end)) build)
  (with-slots (project results start-time end-time) build
    (let* ((total-time (/ (- end-time start-time) internal-time-units-per-second))
           (total-files (seq:count results))
           (results (seq:realize results))
           (failed-files (count-if (lambda (r) (compilation-errors r)) results))
           (warning-files (count-if (lambda (r) (compilation-warnings r)) results)))
      (format t "~%Build Complete:~%")
      (format t ";;   Files: ~D~%" total-files)
      (when (> failed-files 0)
        (format t ";;   Failed: ~D~%" failed-files))
      (when (> warning-files 0)
        (format t ";;   Warnings: ~D~%" warning-files))
      (format t ";;   Time: ~,2F seconds~%" total-time))))

(defun report (project-build)
  (with-slots (project results) project-build
    (let ((total-time (seq:reduce #'+ (seq:map #'operation-wall-time results)
                                  :initial-value 0)))
      (format t "~&Project: ~a (build time: ~,2fs)~%"
              (uri:path (uri project))
              total-time)
      (seq:each #'report-result results))))

;;; Module Registration System

(defun find-module-directories (base-dir)
  "Find all directories containing package.yaml files under base-dir/module/
   Returns a list of directory path strings suitable for register-module"
  (let ((module-base (uri:merge base-dir "module/"))
        (module-dirs '()))
    (when (fs:exists-p module-base)
      ;; List all entries in the module directory
      (dolist (entry-name (fs:list-dir (uri:path module-base)))
        (let* ((entry-path (uri:path-join "module" entry-name))
               (entry-uri (uri:merge base-dir entry-path))
               (package-file (uri:merge entry-uri "package.yaml")))
          (when (and (fs:dir-p (uri:path entry-uri))
                     (fs:exists-p package-file))
            (push entry-path module-dirs)))))
    (nreverse module-dirs)))

(defun module-applicable-p (module-dir)
  "Check if module is applicable to current platform"
  (let ((package-file (uri:merge module-dir "package.yaml")))
    (when (fs:exists-p package-file)
      (let* ((defs (map:from-pairs (yaml:parse-file (uri:path package-file))))
             (platform (map:get defs "platform")))
        ;; Module is applicable if:
        ;; 1. No platform specified (platform-agnostic)
        ;; 2. Platform matches current platform
        (or (not platform)
            (string= platform (string-downcase (detect-platform))))))))

(defun register-modules (&key (base-dir (uri:file-uri (namestring *default-pathname-defaults*))))
  "Discover and register all applicable modules found under base-dir/module/"
  (let ((module-paths (find-module-directories base-dir))
        (registered-count 0))
    
    ;; Register each module using register-module
    (dolist (module-path module-paths)
      (handler-case 
          (progn
            (register-module module-path)
            (incf registered-count))
        (error (e)
          ;; Skip modules that can't be registered (e.g., platform incompatible)
          (format t ";;   Skipping module ~a: ~a~%" module-path e))))
    
    ;; Return count of successfully registered modules
    registered-count))

(defun register-module (module-spec)
  "Register a single module for building.
   
   MODULE-SPEC can be:
   - A string pathname to a directory containing package.yaml"
  (let* ((module-dir-path (cond
                           ((stringp module-spec)
                            (namestring (merge-pathnames module-spec)))
                           (t 
                            (error "Unsupported module-spec type: ~A" module-spec))))
         (module-dir (uri:file-uri module-dir-path))
         (package-file (uri:merge module-dir "package.yaml")))
    
    ;; Validate package file exists
    (unless (fs:exists-p package-file)
      (error "Package file not found: ~A" (uri:path package-file)))
    
    ;; Check if module is applicable to current platform
    (unless (module-applicable-p module-dir)
      (error "Module not applicable to current platform: ~A" (uri:path module-dir)))
    
    ;; Parse package.yaml and register module
    (let* ((defs (map:from-pairs (yaml:parse-file (uri:path package-file))))
           (module-name (map:get defs "name")))
      (unless module-name
        (error "Module name not found in package file: ~A" (uri:path package-file)))
      
      ;; Register the module
      (map:assoc! *modules* module-name module-dir)
      (format t ";;   Registered module: ~a -> ~a~%" 
              module-name 
              (uri:path module-dir))
      
      ;; Return the module name
      module-name)))
