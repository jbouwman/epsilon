;;;; This package provides two main functions:
;;;; - selftest: Discover and test all modules sequentially
;;;; - generate: Create standalone release packages

(defpackage epsilon.release
  (:use cl)
  (:local-nicknames
   (loader epsilon.loader)
   (env epsilon.sys.env)
   (fs epsilon.sys.fs)
   (process epsilon.process)
   (path epsilon.path)
   (log epsilon.log)
   (seq epsilon.sequence)
   (map epsilon.map)
   (str epsilon.string)
   (digest epsilon.digest))
  (:export
   selftest
   generate))

(in-package epsilon.release)

;;; Utility Functions

(defun get-version ()
  "Get the version from VERSION file or environment."
  (let ((version-file (path:path-string (path:path-join (namestring (fs:current-directory)) "VERSION"))))
    (if (probe-file version-file)
        (str:trim (fs:read-file version-file))
        (env:version))))

(defun detect-platform ()
  "Detect the current platform and architecture."
  (let ((os (env:platform))
        (arch (machine-type)))
    (format nil "~(~A~)-~A" 
            (case os
              (:darwin "macos")
              (t os))
            (cond 
              ((search "X86-64" arch) "x86_64")
              ((search "ARM64" arch) "arm64")
              (t arch)))))

(defun make-path (&rest components)
  "Convenience function to create path strings."
  (path:path-string (apply #'path:path-join components)))

(defun print-status (message &key (success t) (indent 2))
  "Print a status message with consistent formatting."
  (let ((prefix (if success "✓" "✗"))
        (spaces (make-string indent :initial-element #\Space)))
    (if success
        (log:info "~A~A ~A" spaces prefix message)
        (log:warn "~A~A ~A" spaces prefix message))))

(defun ensure-directory (dir-path)
  "Create directory if it doesn't exist and print status."
  (fs:make-dirs dir-path)
  (log:debug "Directory created: ~A" dir-path))

(defun get-modules (environment)
  "Get list of all modules."
  (let ((all-descriptors (loader:query-modules environment))
        (modules '()))
    (dolist (descriptor all-descriptors)
      (let* ((name (loader:module-name descriptor))
             (metadata (loader:module-metadata descriptor))
             (platform (getf metadata :platform)))
        ;; Include if no platform restriction or matches current platform
        (when (or (not platform)
                  (string-equal platform (string-downcase (symbol-name (env:platform)))))
          (push name modules))))
    (sort modules #'string<)))

;;; Self-test Function

(defun selftest (&optional environment)
  "Discover all modules and test them sequentially."
  (unless environment
    (setf environment (loader:environment)))
  (log:info "Starting Epsilon Release Self-Test")
  
  (let ((modules (get-modules environment))
        (total-tested 0)
        (total-passed 0)
        (failed-modules '()))
    
    (log:info "Found ~D modules to test" (length modules))
    
    (dolist (module modules)
      (log:info "Testing ~A..." module)
      (incf total-tested)
      
      (handler-case
          (progn
            ;; Load the module
            (loader:load-module environment module)
            
            ;; Ensure epsilon.test is loaded
            ;;; FIXME move this out of the loop, and provide standard functions
            (unless (find-package "EPSILON.TEST")
              (loader:load-module environment "epsilon.test"))
            
            ;; Run tests
            (let* ((test-run-fn (find-symbol "RUN" (find-package "EPSILON.TEST")))
                   (success-p-fn (find-symbol "SUCCESS-P" (find-package "EPSILON.TEST")))
                   (clear-tests-fn (find-symbol "CLEAR-TESTS" (find-package "EPSILON.TEST.SUITE")))
                   (result (funcall test-run-fn environment module)))
              
              (if (funcall success-p-fn result)
                  (progn
                    (format t "  ✓ PASSED~%")
                    (incf total-passed))
                  (progn
                    (format t "  ✗ FAILED~%")
                    (push module failed-modules)))
              
              ;; Clear tests after each package to free memory and prevent accumulation
              (when (and clear-tests-fn (fboundp clear-tests-fn))
                (funcall clear-tests-fn))))
        
        (error (e)
          (format t "  ✗ ERROR: ~A~%" e)
          (push module failed-modules))))
    
    ;; Summary
    (format t "~%Test Summary:~%")
    (format t "=============~%")
    (format t "Total modules: ~D~%" total-tested)
    (format t "Passed: ~D~%" total-passed)
    (format t "Failed: ~D~%" (- total-tested total-passed))
    
    (when failed-modules
      (format t "~%Failed modules:~%")
      (dolist (module (reverse failed-modules))
        (format t "  - ~A~%" module)))
    
    (format t "~%")
    (= total-passed total-tested)))

;;; Release Generation Functions

(defun get-platform-modules (environment platform-arch)
  "Get list of modules to include for a specific platform."
  (let ((platform (seq:first (str:split #\- platform-arch))))
    
    ;; Use the same logic as get-modules to query available modules
    (let ((all-descriptors (loader:query-modules environment))
          (platform-modules '()))
      (dolist (descriptor all-descriptors)
        (let* ((name (loader:module-name descriptor))
               (metadata (loader:module-metadata descriptor))
               (module-platform (getf metadata :platform)))
          ;; Include if no platform restriction or matches current platform
          (when (or (not module-platform)
                    (string-equal module-platform platform)
                    ;; Also include modules for the current running platform
                    (string-equal module-platform (string-downcase (symbol-name (env:platform)))))
            (push (str:replace-first name "epsilon." "") platform-modules))))
      (sort platform-modules #'string<))))

(defun build-modules-for-release (environment platform-arch)
  "Build all modules needed for the release."
  (format t "Building modules for ~A...~%" platform-arch)
  
  (let ((modules (get-platform-modules environment platform-arch)))
    (dolist (module modules)
      (let ((module-name (format nil "epsilon.~A" module)))
        (handler-case
            (progn
              (format t "  Building ~A...~%" module-name)
              (loader:load-module environment module-name :compile-only t)
              (format t "    ✓ Built successfully~%"))
          (error (e)
            (log:error "Failed to build ~A" module-name)
            (format t "    Warning: Failed to build ~A: ~A~%" module-name e)))))))

(defun copy-source-tree (release-dir)
  "Copy source tree to release directory."
  (format t "Copying source tree...~%")
  
  (let ((src-dir (path:path-string (path:path-join (namestring (fs:current-directory)) "src")))
        (target-src (path:path-string (path:path-join release-dir "src"))))
        
    ;; Copy source tree using standard epsilon.sys.fs functions
    (fs:copy-directory src-dir target-src)
    (format t "  ✓ Source tree copied successfully~%")))

(defun copy-epsilon-script (release-dir)
  "Copy the existing epsilon script to the release bin directory."
  (format t "Copying epsilon script...~%")
  
  (let ((bin-dir (path:path-string (path:path-join release-dir "bin")))
        (epsilon-script (path:path-string (path:path-join (namestring (fs:current-directory)) "epsilon")))
        (target-script (path:path-string (path:path-join release-dir "bin" "epsilon"))))
    
    (fs:make-dirs bin-dir)
    
    ;; Copy epsilon script using standard epsilon.sys.fs functions
    (fs:copy-file epsilon-script target-script)
    ;; Make executable (assuming Unix-like system)
    #+unix
    (handler-case
        (progn
          (format t "  Making script executable...~%")
          (process:run-sync "/bin/chmod" 
                            :args (list "+x" target-script)
                            :check-executable nil))
      (process:command-not-found ()
        (log:warn "chmod command not found - script may not be executable"))
      (error (e)
        (log:warn "Could not make epsilon script executable: ~A" e)))
    (format t "  ✓ Epsilon script copied successfully~%")))

(defun create-sbcl-bundle (release-dir)
  "Bundle SBCL with the release."
  (format t "Creating SBCL bundle...~%")
  
  (let ((bin-dir (path:path-string (path:path-join release-dir "bin")))
        (lib-dir (path:path-string (path:path-join release-dir "lib" "sbcl-libs"))))
    
    (fs:make-dirs bin-dir)
    (fs:make-dirs lib-dir)
    
    ;; Copy SBCL binary using built-in runtime path
    (handler-case
        (let ((sbcl-runtime sb-ext:*runtime-pathname*))
          (when (and sbcl-runtime (probe-file sbcl-runtime))
            (let ((target-sbcl (path:path-string (path:path-join bin-dir "sbcl"))))
              (fs:copy-file (namestring sbcl-runtime) target-sbcl)
              ;; Make the copied binary executable
              (sb-posix:chmod target-sbcl #o755)
              (format t "  ✓ SBCL binary copied from ~A~%" sbcl-runtime))))
      (error (e)
        (log:error e "Could not copy SBCL binary")
        (format t "Warning: Could not copy SBCL binary: ~A~%" e)))
    
    ;; Copy SBCL libraries using SBCL_HOME
    (handler-case
        (let ((sbcl-home (sb-posix:getenv "SBCL_HOME")))
          (when (and sbcl-home (probe-file sbcl-home))
            ;; Copy all files from SBCL_HOME to lib-dir
            (fs:copy-directory sbcl-home lib-dir)
            (format t "  ✓ SBCL libraries copied from ~A~%" sbcl-home))
          
          ;; Also copy the core file if it exists
          (let ((sbcl-core sb-ext:*core-pathname*))
            (when (and sbcl-core (probe-file sbcl-core))
              (let ((target-core (path:path-string (path:path-join lib-dir "sbcl.core"))))
                (fs:copy-file (namestring sbcl-core) target-core)
                (format t "  ✓ SBCL core copied from ~A~%" sbcl-core)))))
      (error (e)
        (log:error e "Could not bundle SBCL libraries")
        (format t "Warning: Could not bundle SBCL libraries: ~A~%" e)))))

(defun copy-additional-files (release-dir)
  "Copy additional files like scripts, documentation, etc."
  (format t "Copying additional files...~%")
  
  (let ((project-root (namestring (fs:current-directory))))
    ;; Copy scripts
    (let ((scripts-src (path:path-string (path:path-join project-root "scripts")))
          (scripts-dst (path:path-string (path:path-join release-dir "scripts"))))
      (when (probe-file scripts-src)
        (handler-case
            (fs:copy-directory scripts-src scripts-dst)
          (error (e)
            (log:error "Could not copy scripts")
            (format t "Warning: Could not copy scripts: ~A~%" e)))))
    
    ;; Copy VERSION file
    (let ((version-src (path:path-string (path:path-join project-root "VERSION")))
          (version-dst (path:path-string (path:path-join release-dir "VERSION"))))
      (when (probe-file version-src)
        (handler-case
            (fs:copy-file version-src version-dst)
          (error (e)
            (log:error "Could not copy VERSION file")
            (format t "Warning: Could not copy VERSION file: ~A~%" e)))))
    
    ;; Copy README and LICENSE if they exist
    (dolist (file '("README.md" "LICENSE"))
      (let ((src-file (path:path-string (path:path-join project-root file)))
            (dst-file (path:path-string (path:path-join release-dir file))))
        (when (probe-file src-file)
          (handler-case
              (fs:copy-file src-file dst-file)
            (error (e)
              (log:error "Could not copy ~A" file)
              (format t "Warning: Could not copy ~A: ~A~%" file e))))))))

(defun copy-install-instructions (release-dir)
  "Copy installation instructions from external resource."
  (let ((source-file (make-path (namestring (fs:current-directory)) 
                                "docs" "development" "installation.md"))
        (target-file (make-path release-dir "INSTALL.md")))
    (fs:copy-file source-file target-file)))

(defun create-zip-archive (release-dir release-name)
  "Create a ZIP archive for Windows releases."
  (process:run-sync "/usr/bin/zip" 
                    :args (list "-r" 
                                (format nil "~A.zip" release-name)
                                (path:path-name (path:make-path release-dir)))
                    :stream-output (lambda (line)
                                     (when (search "adding:" line)
                                       (format t ".")))
                    :check-executable nil)
  (format t "~%"))

(defun create-checksum-file (archive-name)
  "Create SHA256 checksum file for archive using built-in SHA-256 implementation."
  (format t "  Computing SHA-256 checksum...~%")
  (let* ((file-bytes (fs:read-file-bytes archive-name))
         (sha256-digest (digest:sha256 file-bytes))
         (hex-string (format nil "~{~2,'0x~}" (coerce sha256-digest 'list)))
         (checksum-content (format nil "~A  ~A~%" hex-string archive-name)))
    (fs:write-file-string (format nil "~A.sha256" archive-name) checksum-content)
    (format t "  ✓ Checksum file created: ~A.sha256~%" archive-name)))

(defun create-tar-archive (release-dir release-name)
  "Create a tar.gz archive for Unix releases with progress feedback."
  (process:run-sync "/usr/bin/tar" 
                    :args (list "-czf" 
                                (format nil "~A.tar.gz" release-name)
                                (path:path-name (path:make-path release-dir)))
                    :stream-output (lambda (line)
                                     (when (> (length line) 0)
                                       (format t ".")))
                    :check-executable nil)
  (format t "~%")
  ;; Create checksum file
  (create-checksum-file (format nil "~A.tar.gz" release-name)))

(defun create-release-archive (release-dir release-name platform-arch)
  "Create the final release archive."
  (let ((platform (seq:first (str:split #\- platform-arch)))
        (parent-dir (path:path-string (path:path-parent (path:make-path release-dir))))
        (old-cwd (namestring (fs:current-directory))))
    
    (log:info "Creating release archive for ~A" platform-arch)
    
    (unwind-protect
         (progn
           (fs:change-directory parent-dir)
           (if (string-equal platform "windows")
               (create-zip-archive release-dir release-name)
               (create-tar-archive-with-working-directory release-dir release-name parent-dir)))
      (fs:change-directory old-cwd))))

(defun create-tar-archive-with-working-directory (release-dir release-name working-dir)
  "Create a tar.gz archive for Unix releases with explicit working directory."
  (process:run-sync "/usr/bin/tar" 
                    :args (list "-czf" 
                                (format nil "~A.tar.gz" release-name)
                                (path:path-name (path:make-path release-dir)))
                    :working-directory working-dir
                    :stream-output (lambda (line)
                                     (when (> (length line) 0)
                                       (format t ".")))
                    :check-executable nil)
  (format t "~%")
  ;; Create checksum file
  (create-checksum-file (format nil "~A.tar.gz" release-name)))

(defun generate (&optional environment-or-version version-override)
  "Generate a standalone release package."
  (let ((environment (if (and environment-or-version
                              (not (stringp environment-or-version)))
                         environment-or-version
                         (loader:environment)))
        (version-arg (if (stringp environment-or-version)
                         environment-or-version
                         version-override)))
    (format t "~%Epsilon Release Generator~%")
    (format t "========================~%~%")
    
    (let* ((version (or version-arg (get-version)))
           (platform-arch (detect-platform))
           (release-name (format nil "epsilon-~A-~A" version platform-arch))
           (release-dir (path:path-string 
                         (path:path-join (namestring (fs:current-directory)) "releases" release-name))))
      
      (format t "Version: ~A~%" version)
      (format t "Platform: ~A~%" platform-arch)
      (format t "Release name: ~A~%" release-name)
      (format t "Output directory: ~A~%~%" release-dir)
      
      ;; Create release directory
      (when (probe-file release-dir)
        (format t "Removing existing release directory...~%")
        (handler-case
            (fs:delete-directory release-dir)
          (error (e)
            (log:error "Could not remove directory")
            (format t "Directory will be overwritten instead~%"))))
      
      (fs:make-dirs release-dir)
      
      ;; Build all modules
      (build-modules-for-release environment platform-arch)
      
      ;; Copy source tree
      (copy-source-tree release-dir)
      
      ;; Create SBCL bundle
      (create-sbcl-bundle release-dir)
      
      ;; Copy epsilon script
      (copy-epsilon-script release-dir)
      
      ;; Copy additional files
      (copy-additional-files release-dir)
      
      ;; Copy installation instructions
      (copy-install-instructions release-dir)
      
      ;; Create archive
      (create-release-archive release-dir release-name platform-arch))))
