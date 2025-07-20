(defpackage :epsilon.package.builder
  (:use
   :cl
   :epsilon.package)
  (:local-nicknames
   (:repo :epsilon.package.repository)
   (:dep :epsilon.package.dependency)
   (:boot :epsilon.package.boot)
   (:map :epsilon.map)
   (:seq :epsilon.sequence)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.uri))
  (:export
   ;; Multi-module building
   :build-modules
   :build-module-set
   
   ;; Incremental building
   :needs-rebuild-p
   :compute-changed-modules
   
   ;; Package building
   :build-and-package
   :build-to-repository
   
   ;; Module discovery
   :discover-modules
   :register-all-modules))

(in-package :epsilon.package.builder)

;;;; ==========================================================================
;;;; Module Discovery
;;;; ==========================================================================

(defun discover-modules (&optional (root-path (uri:make-uri :scheme "file" :path "src/")))
  "Discover all modules under root path"
  (let ((modules (map:make-map)))
    
    (when (fs:exists-p root-path)
      (dolist (entry (fs:list-contents root-path))
        (when (fs:dir-p entry)
          (let ((package-file (uri:merge entry "package.lisp")))
            (when (fs:exists-p package-file)
              (handler-case
                  (let* ((module-info (dep:load-module-info entry))
                         (module-name (map:get module-info "name")))
                    (when module-name
                      (setf modules (map:assoc modules module-name entry))))
                (error (e)
                  (warn "Failed to load module at ~A: ~A" entry e))))))))
    
    modules))

(defun register-all-modules (&optional (root-path (uri:make-uri :scheme "file" :path "src/")))
  "Register all discovered modules in the repository"
  (let ((modules (discover-modules root-path)))
    (map:do-map (lambda (name path)
                  (format t "Registering module: ~A -> ~A~%" name (uri:path path)))
                modules)
    modules))

;;;; ==========================================================================
;;;; Change Detection
;;;; ==========================================================================

(defun get-module-timestamp (module-path)
  "Get the latest modification timestamp for module sources"
  (let ((latest 0))
    (fs:walk-uri module-path
                 (lambda (uri)
                   (when (str:ends-with-p (uri:path uri) ".lisp")
                     (let ((mtime (file-write-date (uri:path uri))))
                       (when (> mtime latest)
                         (setf latest mtime))))))
    latest))

(defun needs-rebuild-p (module-name module-path)
  "Check if module needs rebuild"
  (let ((cache-entry (repo:get-cached-package module-name "latest")))
    (or (null cache-entry)
        (> (get-module-timestamp module-path)
           (getf cache-entry :timestamp)))))

(defun compute-changed-modules (modules)
  "Compute which modules have changed and need rebuilding"
  (let ((changed '()))
    (map:do-map (lambda (name path)
                  (when (needs-rebuild-p name path)
                    (push name changed)))
                modules)
    changed))

;;;; ==========================================================================
;;;; Module Building
;;;; ==========================================================================

(defun build-module (module-name module-path &key verbose package)
  "Build a single module"
  (when verbose
    (format t "~&Building module: ~A~%" module-name))
  
  ;; Use boot mechanism for efficient building
  (boot:boot-module module-name module-path :force t)
  
  ;; Package if requested
  (when package
    (let* ((module-info (dep:load-module-info module-path))
           (version (map:get module-info "version" "0.0.0"))
           (output-dir (repo:get-package-path module-name version)))
      
      (build-package module-path 
                     :output-dir output-dir))))

(defun build-module-set (module-names all-modules &key verbose package)
  "Build a specific set of modules in dependency order"
  (let* ((build-order (dep:compute-build-order module-names all-modules))
         (total (length build-order))
         (current 0))
    
    (when verbose
      (format t "~&Building ~D modules in order: ~{~A~^, ~}~%" total build-order))
    
    (dolist (module-name build-order)
      (incf current)
      (when verbose
        (format t "~&[~D/~D] " current total))
      (let ((module-path (map:get all-modules module-name)))
        (build-module module-name module-path 
                     :verbose verbose 
                     :package package)))))

(defun build-modules (modules &key 
                             (incremental t) 
                             (package t)
                             (verbose t))
  "Build multiple modules with smart dependency handling.
   MODULES can be:
   - A list of module names to build
   - :all to build all modules
   - :changed to build only changed modules"
  (let* ((all-modules (discover-modules))
         (target-modules
          (cond
            ((eq modules :all)
             (map:keys all-modules))
            ((eq modules :changed)
             (compute-changed-modules all-modules))
            ((listp modules)
             modules)
            (t
             (list modules)))))
    
    ;; Handle incremental builds
    (when (and incremental (not (eq modules :all)))
      (let ((changed (compute-changed-modules all-modules)))
        ;; Add changed dependencies
        (dolist (changed-module changed)
          (unless (member changed-module target-modules :test #'string=)
            (push changed-module target-modules)))))
    
    ;; Build modules
    (build-module-set target-modules all-modules
                     :verbose verbose
                     :package package)))

;;;; ==========================================================================
;;;; Repository Building
;;;; ==========================================================================

(defun build-and-package (module-name &key 
                                     (repository repo:*default-repository*)
                                     sign-key)
  "Build a module and package it into the repository"
  (let* ((all-modules (discover-modules))
         (module-path (map:get all-modules module-name)))
    
    (unless module-path
      (error "Unknown module: ~A" module-name))
    
    ;; Build module
    (build-module module-name module-path :verbose t :package nil)
    
    ;; Create package
    (let* ((module-info (dep:load-module-info module-path))
           (version (map:get module-info "version" "0.0.0"))
           (package-path (build-package module-path
                                       :output-dir (repo:repository-path repository)
                                       :sign-key sign-key)))
      
      ;; Register in repository
      (repo:register-module-package module-name package-path module-info repository)
      
      ;; Cache the package
      (let ((cache-path (uri:merge (repo:repository-path repository)
                                  (format nil "cache/~A-~A.fasl" module-name version))))
        (boot:create-boot-cache module-name module-path)
        (repo:cache-package module-name version module-path cache-path module-info repository))
      
      package-path)))

(defun build-to-repository (modules &key 
                                   (repository repo:*default-repository*)
                                   sign-key
                                   verbose)
  "Build modules and install them into the repository"
  (let ((module-list (if (eq modules :all)
                         (map:keys (discover-modules))
                         (if (listp modules) modules (list modules)))))
    
    (when verbose
      (format t "~&Building ~D modules to repository...~%" (length module-list)))
    
    (dolist (module-name module-list)
      (when verbose
        (format t "~&Processing: ~A~%" module-name))
      (build-and-package module-name 
                        :repository repository
                        :sign-key sign-key))))
