;;;; Package Management Tool
;;;;
;;;; Handles installation, verification, and management of EPK packages.
;;;; Consolidates functionality from package-manager.lisp and related scripts.

(defpackage epsilon.tool.package
  (:use cl epsilon.tool.common)
  (:local-nicknames
   (fs epsilon.sys.fs)
   (map epsilon.map)
   (str epsilon.string)
   (path epsilon.path)
   (build epsilon.tool.build)
   (proto epsilon.build.protocol))
  (:export
   list-packages
   install-package
   verify-package
   search-packages
   package-info
   clean-cache
   *user-repository*))

(in-package :epsilon.tool.package)

(defvar *user-repository* nil
  "Path to user's package repository")

(defvar *installed-packages* nil
  "Cache of installed package information")

(defun initialize-package-manager ()
  "Initialize package manager with user repository"
  (unless *user-repository*
    (setf *user-repository* 
          (merge-pathnames ".epsilon/repository/" (user-homedir-pathname)))
    (ensure-directories-exist *user-repository*))
  
  ;; Load installed packages index
  (load-installed-packages-index)
  
  *user-repository*)

(defun load-installed-packages-index ()
  "Load index of installed packages"
  (let ((index-file (merge-pathnames "installed.edn" *user-repository*)))
    (setf *installed-packages*
          (if (probe-file index-file)
              (with-open-file (in index-file)
                (read in))
              map:+empty+))))

(defun save-installed-packages-index ()
  "Save index of installed packages"
  (let ((index-file (merge-pathnames "installed.edn" *user-repository*)))
    (with-open-file (out index-file :direction :output :if-exists :supersede)
      (format out "~S~%" *installed-packages*))))

(defun list-packages (&key installed-only available-only)
  "List packages based on filter criteria"
  (initialize-package-manager)
  
  (let* ((source (build:ensure-package-source))
         (all-packages (proto:list-packages source)))
    (cond
      (installed-only
       ;; List only installed packages
       (map:keys *installed-packages*))
      (available-only
       ;; List only available (not installed) packages
       (let ((installed (map:keys *installed-packages*)))
         (remove-if (lambda (pkg) (member pkg installed :test #'string=)) all-packages)))
      (t
       ;; List all packages with status
       (mapcar (lambda (package-name)
                 (if (map:contains-p *installed-packages* package-name)
                     (cons package-name :installed)
                     (cons package-name :available)))
               all-packages)))))

(defun package-info (package-name)
  "Get information about a package"
  (initialize-package-manager)
  
  (let* ((source (build:ensure-package-source))
         (metadata (proto:load-package-metadata source package-name))
         (location (proto:resolve-package-location source package-name))
         (installed-info (map:get *installed-packages* package-name)))
    
    (when metadata
      (append metadata
              (list :location (when location (path:path-string location)))
              (when installed-info
                (list :installed t
                      :install-date (map:get installed-info "install-date")
                      :install-path (map:get installed-info "install-path")))))))

(defun install-package (package-name &key force verbose)
  "Install a package from EPK"
  (initialize-package-manager)
  
  ;; Check if already installed
  (when (and (map:contains-p *installed-packages* package-name)
             (not force))
    (error "Package ~A is already installed. Use :force t to reinstall." package-name))
  
  ;; Verify package exists
  (let ((source (build:ensure-package-source)))
    (unless (proto:package-exists-p source package-name)
      (error "Package ~A not found in available packages." package-name)))
  
  (when verbose
    (format t ";;; Installing package: ~A~%" package-name))
  
  ;; For now, installation means building the package locally
  ;; In a full implementation, this would download and extract EPK files
  (handler-case
      (progn
        (build:build package-name :force force)
        
        ;; Record installation
        (let ((install-info (map:from-pairs
                            `(("install-date" . ,(get-universal-time))
                              ("version" . ,(or (getf (package-info package-name) :version) "unknown"))
                              ("install-path" . ,(format nil "~A" package-name))))))
          (map:assoc! *installed-packages* package-name install-info)
          (save-installed-packages-index))
        
        (when verbose
          (format t ";;; Package ~A installed successfully~%" package-name))
        t)
    (error (e)
      (format *error-output* ";;; Failed to install package ~A: ~A~%" package-name e)
      nil)))


(defun verify-package (package-name &key verbose)
  "Verify package integrity"
  (initialize-package-manager)
  
  (let ((installed-info (map:get *installed-packages* package-name)))
    (unless installed-info
      (error "Package ~A is not installed." package-name))
    
    (when verbose
      (format t ";;; Verifying package: ~A~%" package-name))
    
    ;; Basic verification - check if module can be described
    (handler-case
        (let ((desc (build:describe-package package-name)))
          (when verbose
            (format t ";;;   Package ~A verified successfully~%" package-name))
          desc)
      (error (e)
        (when verbose
          (format t ";;;   Package ~A verification failed: ~A~%" package-name e))
        nil))))

(defun search-packages (query &key name-only description-only)
  "Search for packages matching query"
  (initialize-package-manager)
  
  (let* ((source (build:ensure-package-source))
         (all-packages (proto:list-packages source))
         (matches '()))
    
    (dolist (package-name all-packages)
      (let* ((metadata (proto:load-package-metadata source package-name))
             (name (or package-name ""))
             (description (or (getf metadata :description) "")))
        
        (when (or (and (or (not name-only) (not description-only))
                       (or (search (string-downcase query) (string-downcase name))
                           (search (string-downcase query) (string-downcase description))))
                  (and name-only
                       (search (string-downcase query) (string-downcase name)))
                  (and description-only
                       (search (string-downcase query) (string-downcase description))))
          (push (append metadata (list :name package-name)) matches))))
    
    (nreverse matches)))

(defun clean-cache (&key verbose)
  "Clean package manager cache"
  (initialize-package-manager)
  
  (when verbose
    (format t ";;; Cleaning package cache~%"))
  
  ;; Reload installed packages index
  (load-installed-packages-index)
  
  ;; Could add more cleanup operations here
  (when verbose
    (format t ";;; Cache cleaned~%"))
  t)
