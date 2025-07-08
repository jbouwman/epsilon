(defpackage :epsilon.lib.package.system
  (:use
   :cl)
  (:local-nicknames
   (:pkg :epsilon.lib.package)
   (:repo :epsilon.lib.package.repository)
   (:dep :epsilon.lib.package.dependency)
   (:boot :epsilon.lib.package.boot)
   (:builder :epsilon.lib.package.builder)
   (:map :epsilon.lib.map)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.lib.uri))
  (:export
   ;; High-level API
   :initialize-package-system
   :second-stage-boot
   :load-module-packages
   
   ;; Package management
   :install-module
   :update-module
   :remove-module
   :list-installed-modules
   
   ;; Development workflow
   :dev-build
   :dev-clean
   :dev-rebuild
   
   ;; System configuration
   :*package-system-initialized*
   :*development-mode*))

(in-package :epsilon.lib.package.system)

;;;; ==========================================================================
;;;; System State
;;;; ==========================================================================

(defvar *package-system-initialized* nil
  "Whether the package system has been initialized")

(defvar *development-mode* t
  "Whether running in development mode (affects caching behavior)")

;;;; ==========================================================================
;;;; System Initialization
;;;; ==========================================================================

(defun initialize-package-system (&key 
                                  (repository repo:*default-repository*)
                                  (force nil))
  "Initialize the package system"
  (when (or force (not *package-system-initialized*))
    
    ;; Initialize repository
    (repo:initialize-repository repository)
    
    ;; Register all modules
    (builder:register-all-modules)
    
    ;; Initialize boot cache
    (boot:ensure-boot-cache-dir)
    
    (setf *package-system-initialized* t)
    
    (format t "~&Package system initialized: ~A~%" (uri:path repository))))

;;;; ==========================================================================
;;;; Second Stage Boot
;;;; ==========================================================================

(defun second-stage-boot (&key 
                         (modules '("epsilon.core"))
                         (force nil)
                         (verbose boot:*boot-verbose*))
  "Perform second-stage boot loading modules efficiently"
  (unless *package-system-initialized*
    (initialize-package-system))
  
  (let ((all-modules (builder:discover-modules))
        (boot:*boot-verbose* verbose))
    
    (when verbose
      (format t "~&;;; Second-stage boot: ~{~A~^, ~}~%" modules))
    
    ;; Build dependency-ordered module list
    (let ((build-order (dep:compute-build-order modules all-modules)))
      
      ;; Boot each module
      (dolist (module-name build-order)
        (let ((module-path (map:get all-modules module-name)))
          (if module-path
              (boot:boot-module module-name module-path :force force)
              (warn "Module not found: ~A" module-name)))))))

(defun load-module-packages (modules &key verbose)
  "Load multiple modules using the package system"
  (second-stage-boot :modules modules :verbose verbose))

;;;; ==========================================================================
;;;; Package Management
;;;; ==========================================================================

(defun install-module (module-name &key 
                                  version
                                  (repository repo:*default-repository*)
                                  force)
  "Install a module package from repository"
  (unless *package-system-initialized*
    (initialize-package-system))
  
  ;; TODO: Implement package installation from remote repositories
  ;; For now, build locally if needed
  (let* ((all-modules (builder:discover-modules))
         (module-path (map:get all-modules module-name)))
    
    (if module-path
        (progn
          (format t "~&Building and installing module: ~A~%" module-name)
          (builder:build-and-package module-name :repository repository))
        (error "Module not found: ~A" module-name))))

(defun update-module (module-name &key (repository repo:*default-repository*))
  "Update an installed module"
  (format t "~&Updating module: ~A~%" module-name)
  (install-module module-name :repository repository :force t))

(defun remove-module (module-name &key (repository repo:*default-repository*))
  "Remove an installed module"
  (format t "~&Removing module: ~A~%" module-name)
  ;; TODO: Implement module removal
  (warn "Module removal not yet implemented"))

(defun list-installed-modules (&key (repository repo:*default-repository*))
  "List all installed modules"
  (unless *package-system-initialized*
    (initialize-package-system))
  
  (repo:list-module-packages repository))

;;;; ==========================================================================
;;;; Development Workflow
;;;; ==========================================================================

(defun dev-build (&key 
                  (modules :changed)
                  (incremental t)
                  (verbose t))
  "Development build - build changed modules"
  (unless *package-system-initialized*
    (initialize-package-system))
  
  (format t "~&Development build...~%")
  (builder:build-modules modules 
                        :incremental incremental
                        :package *development-mode*
                        :verbose verbose))

(defun dev-clean (&key 
                  (modules :all)
                  (cache t)
                  (packages t))
  "Clean development artifacts"
  (when cache
    (format t "~&Clearing boot cache...~%")
    (boot:clear-boot-cache))
  
  (when packages
    ;; TODO: Clear package artifacts
    (format t "~&Clearing package artifacts...~%"))
  
  (format t "~&Clean complete~%"))

(defun dev-rebuild (&key 
                   (modules :all)
                   (verbose t))
  "Clean rebuild of modules"
  (dev-clean :modules modules)
  (dev-build :modules modules :incremental nil :verbose verbose))

;;;; ==========================================================================
;;;; Integration with Existing Boot Process
;;;; ==========================================================================

(defun enhanced-epsilon-boot (&key 
                             (quick-boot t)
                             (verbose nil)
                             (force nil))
  "Enhanced boot process that can use cached epsilon.core"
  (if quick-boot
      (progn
        (unless *package-system-initialized*
          (initialize-package-system))
        (boot:quick-boot :force force :verbose verbose))
      
      ;; Fall back to traditional boot
      (progn
        (format t "~&;;; Traditional boot process~%")
        ;; This would call the original boot process
        )))

;;;; ==========================================================================
;;;; Utility Functions
;;;; ==========================================================================

(defun show-package-status (&key (repository repo:*default-repository*))
  "Show status of package system"
  (unless *package-system-initialized*
    (initialize-package-system))
  
  (format t "~&Package System Status:~%")
  (format t "  Repository: ~A~%" (uri:path repository))
  (format t "  Development mode: ~A~%" *development-mode*)
  (format t "  Boot cache: ~A~%" (uri:path boot:*boot-cache-dir*))
  
  (let ((installed (list-installed-modules :repository repository))
        (all-modules (builder:discover-modules)))
    (format t "  Discovered modules: ~D~%" (map:size all-modules))
    (format t "  Installed packages: ~D~%" (length installed))
    
    ;; Show changed modules
    (let ((changed (builder:compute-changed-modules all-modules)))
      (when changed
        (format t "  Changed modules: ~{~A~^, ~}~%" changed)))))

(defun dependency-status (module-name)
  "Show dependency status for a module"
  (let* ((all-modules (builder:discover-modules))
         (module-path (map:get all-modules module-name)))
    
    (unless module-path
      (error "Module not found: ~A" module-name))
    
    (let* ((module-info (dep:load-module-info module-path))
           (deps (dep:get-module-dependencies module-info))
           (provides (dep:get-module-provides module-info)))
      
      (format t "~&Module: ~A~%" module-name)
      (format t "  Path: ~A~%" (uri:path module-path))
      (format t "  Dependencies: ~{~A~^, ~}~%" deps)
      (format t "  Provides: ~{~A~^, ~}~%" provides)
      
      ;; Show build order
      (let ((build-order (dep:compute-build-order (list module-name) all-modules)))
        (format t "  Build order: ~{~A~^, ~}~%" build-order)))))