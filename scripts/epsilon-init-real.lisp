;;;; Epsilon initialization with real module loading
;;;; This provides full functionality for the binary distribution

(defpackage #:epsilon.init
  (:use #:cl)
  (:export #:epsilon-toplevel
           #:load-bundled-module
           #:list-available-modules))

(in-package #:epsilon.init)

(defparameter *epsilon-version* "1.0.0-dev"
  "Epsilon version string")

(defparameter *epsilon-home* nil
  "Epsilon installation directory")

(defparameter *bundled-repository* nil
  "Path to bundled repository if available")

(defvar *bundled-modules* nil
  "Index of modules available in bundled repository")

(defvar *loaded-modules* nil
  "Set of modules that have been loaded")

(defun epsilon-banner ()
  "Display Epsilon banner"
  (format t "~&Epsilon ~A~%" *epsilon-version*)
  (format t "A self-contained Common Lisp Programming Environment~%")
  (when *bundled-repository*
    (format t "Bundled repository: ~A~%" *bundled-repository*))
  (format t "~%"))

(defun detect-epsilon-home ()
  "Detect Epsilon installation directory"
  (let ((core-path (or (first sb-ext:*posix-argv*))))
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
              (latest (getf info :latest))
              (loaded (member name *loaded-modules* :test #'string=)))
         (format t "  ~A (~A)~:[~; [LOADED]~]~%" name latest loaded)))
     (format t "~%Use (load-bundled-module \"module-name\") to load a module.~%"))
    (t
     (format t "No bundled modules available. Repository may not be built yet.~%"))))

(defun find-bundled-module (name &optional version)
  "Find a module in the bundled repository"
  (when *bundled-modules*
    (let ((module-entry (find name *bundled-modules* 
                             :key (lambda (m) (first m)) 
                             :test #'string-equal)))
      (when module-entry
        (let ((versions (getf (rest module-entry) :versions)))
          (if version
              (find version versions :key (lambda (v) (first v)) :test #'string=)
              (first versions)))))))

(defun load-module-from-fasl (module-name version-info)
  "Load module from FASL file"
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
              (progn
                (format t "  Warning: FASL not found: ~A~%" fasl-path)
                nil)))
        (progn
          (format t "  Warning: No FASL specified for ~A~%" module-name)
          nil))))

(defun load-bundled-module (name)
  "Load a module from the bundled repository"
  (cond
    ((member name *loaded-modules* :test #'string=)
     (format t "Module ~A is already loaded~%" name)
     t)
    
    ((not *bundled-repository*)
     (format t "No bundled repository available~%")
     nil)
    
    ((not *bundled-modules*)
     (format t "Repository index not loaded. Trying to load...~%")
     (load-bundled-repository)
     (if *bundled-modules*
         (load-bundled-module name)
         nil))
    
    (t
     (let ((module-version (find-bundled-module name)))
       (if module-version
           (handler-case
               (load-module-from-fasl name module-version)
             (error (e)
               (format t "Error loading ~A: ~A~%" name e)
               nil))
           (progn
             (format t "Module ~A not found in repository~%" name)
             nil))))))

(defun epsilon-toplevel ()
  "Custom toplevel for Epsilon runtime"
  ;; Suppress SBCL's default banner
  (let ((banner-symbol (find-symbol "*BANNER-PRINTED*" "SB-IMPL")))
    (when banner-symbol
      (setf (symbol-value banner-symbol) t)))
  
  ;; Detect bundled repository
  (detect-bundled-repository)
  
  ;; Display our banner
  (epsilon-banner)
  
  ;; Load bundled repository index if available
  (when *bundled-repository*
    (load-bundled-repository))
  
  ;; Run the default SBCL toplevel
  (sb-impl::toplevel-init))

;; Export to CL-USER for convenience
(import '(load-bundled-module list-available-modules) :cl-user)
(export '(load-bundled-module list-available-modules) :cl-user)