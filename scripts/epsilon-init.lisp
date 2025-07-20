;;;; Epsilon initialization and custom toplevel
;;;; This provides custom banner and environment setup for Epsilon runtime

(defpackage #:epsilon.init
  (:use #:cl)
  (:export #:epsilon-toplevel))

(in-package #:epsilon.init)

(defparameter *version-info* nil
  "Cached version information from build time")

(defparameter *epsilon-module-directory* 
  (or (sb-ext:posix-getenv "EPSILON_MODULE_DIR")
      (namestring (merge-pathnames "modules/" (user-homedir-pathname))))
  "Directory where Epsilon modules are stored")

(defparameter *epsilon-home* nil
  "Epsilon installation directory")

(defparameter *bundled-repository* nil
  "Path to bundled repository if available")

(defun load-version-info ()
  "Load version information from embedded EDN file"
  (unless *version-info*
    (handler-case
        (let ((version-file (merge-pathnames "epsilon-version.edn" 
                                           (sb-ext:posix-getenv "SBCL_HOME"))))
          (when (probe-file version-file)
            (let ((edn-read (find-symbol "READ-STRING" "EPSILON.LIB.EDN"))
                  (map-pkg (find-package "EPSILON.LIB.MAP")))
              (when (and edn-read (fboundp edn-read))
                (with-open-file (in version-file)
                  (setf *version-info* 
                        (funcall edn-read 
                                (with-output-to-string (s)
                                  (loop for line = (read-line in nil)
                                        while line do (write-line line s))))))))))
      (error () 
        ;; If we can't load version info, use defaults
        (let ((map-pkg (find-package "EPSILON.LIB.MAP")))
          (if map-pkg
              (let ((make-map (find-symbol "MAKE-MAP" map-pkg)))
                (when (and make-map (fboundp make-map))
                  (setf *version-info* 
                        (funcall make-map :version "unknown"))))
              (setf *version-info* nil))))))
  *version-info*)

(defun get-version-string ()
  "Get the epsilon version string"
  (let ((info (load-version-info))
        (map-pkg (find-package "EPSILON.LIB.MAP")))
    (cond
      ((and info map-pkg)
       (let ((map-get (find-symbol "GET" map-pkg)))
         (when (and map-get (fboundp map-get))
           (or (funcall map-get info :full-version)
               (funcall map-get info :version)
               "unknown"))))
      (t "unknown"))))

(defun detect-epsilon-home ()
  "Detect Epsilon installation directory"
  (let ((core-path (or (first sb-ext:*posix-argv*) ; Path to executable
                       (sb-ext:posix-getenv "EPSILON_HOME"))))
    (when core-path
      (let ((dir (make-pathname :directory (pathname-directory 
                                           (truename (pathname core-path))))))
        (when (probe-file (merge-pathnames "epsilon-core" dir))
          (setf *epsilon-home* (namestring dir))))))
  *epsilon-home*)

(defun detect-bundled-repository ()
  "Check for bundled repository in Epsilon installation"
  (when (detect-epsilon-home)
    (let ((repo-path (merge-pathnames "repository/" *epsilon-home*)))
      (when (probe-file repo-path)
        (setf *bundled-repository* (namestring repo-path))
        (format t "Found bundled repository at: ~A~%" *bundled-repository*))))
  *bundled-repository*)

(defun epsilon-banner ()
  "Display Epsilon banner instead of SBCL banner"
  (format t "~&Epsilon ~A~%" (get-version-string))
  (format t "A Common Lisp Programming Environment~%")
  (format t "Module directory: ~A~%" *epsilon-module-directory*)
  (when *bundled-repository*
    (format t "Bundled repository: ~A~%" *bundled-repository*))
  (format t "~%"))

(defun epsilon-toplevel ()
  "Custom toplevel for Epsilon runtime"
  ;; Suppress SBCL's default banner
  (setf (symbol-value (find-symbol "*BANNER-PRINTED*" "SB-IMPL")) t)
  
  ;; Detect bundled repository
  (detect-bundled-repository)
  
  ;; Display our banner
  (epsilon-banner)
  
  ;; Ensure epsilon is available
  (when (find-package "EPSILON.TOOL.BUILD")
    ;; Register modules from the module directory
    (let ((register-fn (find-symbol "REGISTER-MODULES" "EPSILON.TOOL.BUILD")))
      (when (and register-fn (fboundp register-fn))
        (handler-case
            (funcall register-fn)
          (error (e)
            (format t "~&Warning: Failed to register modules: ~A~%" e))))))
  
  ;; Load bundled repository index if available
  (when *bundled-repository*
    (load-bundled-repository))
  
  ;; Load package manager
  (load-package-manager)
  
  ;; Run the default SBCL toplevel
  (sb-impl::toplevel-init))

;; Set up the environment when this file is loaded
(defun setup-epsilon-environment ()
  "Initialize Epsilon environment variables and settings"
  ;; Ensure module directory exists
  (ensure-directories-exist *epsilon-module-directory*)
  
  ;; Set up module directory environment variable if not already set
  ;; Note: SBCL doesn't support setf on posix-getenv, so we skip this
  )

(defvar *bundled-modules* nil
  "Index of modules available in bundled repository")

(defvar *loaded-modules* nil
  "Set of modules that have been loaded")

(defun load-bundled-repository ()
  "Load the bundled repository index"
  (let ((index-file (merge-pathnames "index.lisp" *bundled-repository*)))
    (when (probe-file index-file)
      (handler-case
          (with-open-file (in index-file)
            (setf *bundled-modules* (read in))
            (setf *loaded-modules* '())
            (format t "Loaded ~D bundled modules~%" (length *bundled-modules*)))
        (error (e)
          (format t "Warning: Failed to load repository index: ~A~%" e))))))

(defun list-available-modules ()
  "List all modules available from bundled repository"
  (cond
    (*bundled-modules*
     (format t "~&Available bundled modules:~%")
     (dolist (module-entry *bundled-modules*)
       (let* ((name (first module-entry))
              (info (rest module-entry))
              (versions (getf info :versions))
              (latest (getf info :latest))
              (loaded (member name *loaded-modules* :test #'string=)))
         (format t "  ~A (~A)~:[~; [LOADED]~]~%" name latest loaded)
         (when (> (length versions) 1)
           (format t "    Available versions: ~{~A~^, ~}~%" 
                   (mapcar #'first versions)))
         (let ((latest-version (first versions)))
           (when latest-version
             (let ((deps (getf (rest latest-version) :dependencies)))
               (when deps
                 (format t "    Dependencies: ~{~A~^, ~}~%" deps)))))))
     (format t "~%Use (load-bundled-module \"module-name\") to load a module.~%"))
    (t
     (format t "No bundled modules available.~%"))))

(defun find-bundled-module (name &optional version)
  "Find a module in the bundled repository, optionally with specific version"
  (when *bundled-modules*
    (let ((module-entry (find name *bundled-modules* 
                             :key (lambda (m) (first m)) 
                             :test #'string-equal)))
      (when module-entry
        (let ((versions (getf (rest module-entry) :versions)))
          (if version
              ;; Find specific version
              (find version versions :key (lambda (v) (first v)) :test #'string=)
              ;; Return latest version
              (first versions)))))))

(defun resolve-dependencies (module-name &optional visited)
  "Resolve module dependencies in correct load order"
  (when (member module-name visited :test #'string=)
    (error "Circular dependency detected: ~A" module-name))
  
  (let ((module-version (find-bundled-module module-name)))
    (unless module-version
      (error "Module not found: ~A" module-name))
    
    (let ((deps (getf (rest module-version) :dependencies))
          (load-order '()))
      
      ;; Recursively resolve dependencies
      (dolist (dep deps)
        (unless (member dep *loaded-modules* :test #'string=)
          (setf load-order 
                (append load-order
                        (resolve-dependencies dep (cons module-name visited))))))
      
      ;; Add this module if not already loaded
      (unless (member module-name *loaded-modules* :test #'string=)
        (setf load-order (append load-order (list module-name))))
      
      load-order)))

(defun load-module-fasl (module-name version-info)
  "Load a single module's FASL file or EPK package"
  (let* ((version (first version-info))
         (info (rest version-info))
         (fasl-name (getf info :fasl))
         (epk-file (getf info :file)))
    (cond
      ;; Try EPK first if available
      (epk-file
       (let ((epk-path (merge-pathnames 
                       (merge-pathnames epk-file "packages/")
                       *bundled-repository*)))
         (if (probe-file epk-path)
             (handler-case
                 (progn
                   ;; Load EPK loader if not already loaded
                   (unless (find-package "EPSILON.EPK-LOADER")
                     (load (merge-pathnames "scripts/epk-loader.lisp" 
                                          (detect-epsilon-home))))
                   
                   ;; Load from EPK
                   (let ((epk-loader (find-package "EPSILON.EPK-LOADER")))
                     (when epk-loader
                       (let ((load-fn (find-symbol "LOAD-EPK-MODULE" epk-loader)))
                         (when (and load-fn (fboundp load-fn))
                           (funcall load-fn epk-path :verify nil :cleanup t)
                           (push module-name *loaded-modules*)
                           (format t "  Loaded ~A (~A) from EPK~%" module-name version)
                           t))))
               (error (e)
                 (format t "  Error loading EPK ~A: ~A~%" epk-file e)
                 ;; Fall back to FASL if EPK fails
                 (load-module-from-fasl module-name version-info)))
             ;; EPK file not found, try FASL
             (load-module-from-fasl module-name version-info))))
      
      ;; Fall back to FASL
      (fasl-name
       (load-module-from-fasl module-name version-info))
      
      (t
       (format t "  Error: No loadable artifact for ~A~%" module-name)
       nil))))

(defun load-module-from-fasl (module-name version-info)
  "Load module from FASL file (fallback method)"
  (let* ((version (first version-info))
         (info (rest version-info))
         (fasl-name (getf info :fasl)))
    (if fasl-name
        (let ((fasl-path (merge-pathnames 
                         (merge-pathnames fasl-name "fasls/")
                         *bundled-repository*)))
          (if (probe-file fasl-path)
              (progn
                (load fasl-path)
                (push module-name *loaded-modules*)
                (format t "  Loaded ~A (~A) from FASL~%" module-name version)
                t)
              (format t "  Error: FASL not found: ~A~%" fasl-path)))
        (format t "  Error: No FASL specified for ~A~%" module-name))))

(defun load-bundled-module (name)
  "Load a module and its dependencies from the bundled repository"
  (handler-case
      (let ((load-order (resolve-dependencies name)))
        (format t "Loading ~A with dependencies: ~{~A~^, ~}~%" name load-order)
        
        (dolist (module-name load-order)
          (unless (member module-name *loaded-modules* :test #'string=)
            (let ((module-version (find-bundled-module module-name)))
              (if module-version
                  (load-module-fasl module-name module-version)
                  (format t "  Warning: Dependency ~A not found~%" module-name)))))
        
        (if (member name *loaded-modules* :test #'string=)
            (format t "Successfully loaded ~A~%" name)
            (format t "Failed to load ~A~%" name)))
    
    (error (e)
      (format t "Error loading ~A: ~A~%" name e))))

;; Load package manager
(defun load-package-manager ()
  "Load the package manager if available"
  (when *epsilon-home*
    (let ((pm-file (merge-pathnames "scripts/package-manager.lisp" *epsilon-home*)))
      (when (probe-file pm-file)
        (handler-case
            (progn
              (load pm-file)
              (format t "Package manager loaded~%"))
          (error (e)
            (format t "Warning: Failed to load package manager: ~A~%" e)))))))

;; Export public functions to CL-USER for convenience
(eval-when (:load-toplevel :execute)
  (export '(list-available-modules load-bundled-module) :epsilon.init)
  (import '(list-available-modules load-bundled-module) :cl-user)
  (export '(list-available-modules load-bundled-module) :cl-user))

;; Run setup when loaded
(setup-epsilon-environment)