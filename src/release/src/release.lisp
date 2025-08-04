;;;; This package provides two main functions:
;;;; - selftest: Discover and test all packages sequentially
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
   (str epsilon.string))
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

(defun safe-operation (description operation &key (on-error :warn))
  "Execute operation with consistent error handling."
  (handler-case
      (progn
        (funcall operation)
        (log:info "~A completed successfully" description))
    (error (e)
      (case on-error
        (:warn (log:warn "~A failed (continuing)" description))
        (:error (log:error "~A failed" description)
         (error e))
        (t (log:warn "~A failed" description))))))

(defun get-concrete-packages (environment)
  "Get list of concrete packages (excluding virtual packages)."
  (let ((all-descriptors (loader:query-packages environment))
        (concrete-packages '()))
    (dolist (descriptor all-descriptors)
      (let* ((name (loader:package-info-name descriptor))
             (metadata (loader:package-metadata descriptor))
             (platform (getf metadata :platform)))
        ;; Include if no platform restriction or matches current platform
        (when (or (not platform)
                  (string-equal platform (string-downcase (symbol-name (env:platform)))))
          (push name concrete-packages))))
    (sort concrete-packages #'string<)))

;;; Self-test Function

(defun selftest (&optional environment)
  "Discover all packages and test them sequentially."
  (unless environment
    (setf environment (loader:environment)))
  (log:info "Starting Epsilon Release Self-Test")
  
  (let ((packages (get-concrete-packages environment))
        (total-tested 0)
        (total-passed 0)
        (failed-packages '()))
    
    (log:info "Found ~D packages to test" (length packages))
    
    (dolist (pkg packages)
      (log:info "Testing ~A..." pkg)
      (incf total-tested)
      
      (handler-case
          (progn
            ;; Load the package
            (loader:load-package environment pkg)
            
            ;; Ensure epsilon.test is loaded
            (unless (find-package "EPSILON.TEST")
              (loader:load-package environment "epsilon.test"))
            
            ;; Run tests
            (let* ((test-run-fn (find-symbol "RUN" (find-package "EPSILON.TEST")))
                   (success-p-fn (find-symbol "SUCCESS-P" (find-package "EPSILON.TEST")))
                   (clear-tests-fn (find-symbol "CLEAR-TESTS" (find-package "EPSILON.TEST.SUITE")))
                   (result (funcall test-run-fn environment pkg)))
              
              (if (funcall success-p-fn result)
                  (progn
                    (format t "  ✓ PASSED~%")
                    (incf total-passed))
                  (progn
                    (format t "  ✗ FAILED~%")
                    (push pkg failed-packages)))
              
              ;; Clear tests after each package to free memory and prevent accumulation
              (when (and clear-tests-fn (fboundp clear-tests-fn))
                (funcall clear-tests-fn))))
        
        (error (e)
          (format t "  ✗ ERROR: ~A~%" e)
          (push pkg failed-packages))))
    
    ;; Summary
    (format t "~%Test Summary:~%")
    (format t "=============~%")
    (format t "Total packages: ~D~%" total-tested)
    (format t "Passed: ~D~%" total-passed)
    (format t "Failed: ~D~%" (- total-tested total-passed))
    
    (when failed-packages
      (format t "~%Failed packages:~%")
      (dolist (pkg (reverse failed-packages))
        (format t "  - ~A~%" pkg)))
    
    (format t "~%")
    (= total-passed total-tested)))

;;; Release Generation Functions

(defun get-platform-packages (environment platform-arch)
  "Get list of packages to include for a specific platform."
  (let ((platform (seq:first (str:split #\- platform-arch))))
    
    ;; Use the same logic as get-concrete-packages to query available packages
    (let ((all-descriptors (loader:query-packages environment))
          (platform-packages '()))
      (dolist (descriptor all-descriptors)
        (let* ((name (loader:package-info-name descriptor))
               (metadata (loader:package-metadata descriptor))
               (pkg-platform (getf metadata :platform)))
          ;; Include if no platform restriction or matches current platform
          (when (or (not pkg-platform)
                    (string-equal pkg-platform platform)
                    ;; Also include packages for the current running platform
                    (string-equal pkg-platform (string-downcase (symbol-name (env:platform)))))
            (push (str:replace-first name "epsilon." "") platform-packages))))
      (sort platform-packages #'string<))))

(defun build-packages-for-release (environment platform-arch)
  "Build all packages needed for the release."
  (format t "Building packages for ~A...~%" platform-arch)
  
  (let ((packages (get-platform-packages environment platform-arch)))
    (dolist (pkg packages)
      (let ((pkg-name (format nil "epsilon.~A" pkg)))
        (handler-case
            (progn
              (format t "  Building ~A...~%" pkg-name)
              (loader:load-package environment pkg-name :compile-only t)
              (format t "    ✓ Built successfully~%"))
          (error (e)
            (log:error "Failed to build ~A" pkg-name)
            (format t "    Warning: Failed to build ~A: ~A~%" pkg-name e)))))))

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
        (process:run-sync "chmod" :args (list "+x" target-script))
      (error (e)
        (log:warn "Could not make epsilon script executable")))
    (format t "  ✓ Epsilon script copied successfully~%")))

(defun create-sbcl-bundle (release-dir)
  "Bundle SBCL with the release."
  (format t "Creating SBCL bundle...~%")
  
  (let ((bin-dir (path:path-string (path:path-join release-dir "bin")))
        (lib-dir (path:path-string (path:path-join release-dir "lib" "sbcl-libs"))))
    
    (fs:make-dirs bin-dir)
    (fs:make-dirs lib-dir)
    
    ;; Find and copy SBCL binary
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (process:run-sync "which" :args (list "sbcl"))
          (declare (ignore error-output))
          (when (zerop exit-code)
            (let ((sbcl-path (str:trim output)))
              (when (probe-file sbcl-path)
                (process:run-sync "cp" :args (list sbcl-path (path:path-string (path:path-join bin-dir "sbcl"))))))))
      (error (e)
        (log:error "Could not find SBCL binary")
        (format t "Warning: Could not find SBCL binary: ~A~%" e)))
    
    ;; Find and copy SBCL libraries
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (process:run-sync "sbcl" 
                              :args (list "--noinform" "--no-sysinit" "--no-userinit" 
                                          "--eval" "(princ (sb-ext:posix-getenv \"SBCL_HOME\"))"
                                          "--eval" "(sb-ext:quit)"))
          (declare (ignore error-output))
          (when (zerop exit-code)
            (let ((sbcl-home (str:trim output)))
              (when (and sbcl-home (probe-file sbcl-home))
                (process:run-sync "cp" :args (list "-r" 
                                                   (path:path-string (path:path-join sbcl-home "*"))
                                                   lib-dir))))))
      (error (e)
        (log:error "Could not bundle SBCL libraries")
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
            (process:run-sync "cp" :args (list "-r" scripts-src scripts-dst))
          (error (e)
            (log:error "Could not copy scripts")
            (format t "Warning: Could not copy scripts: ~A~%" e)))))
    
    ;; Copy VERSION file
    (let ((version-src (path:path-string (path:path-join project-root "VERSION")))
          (version-dst (path:path-string (path:path-join release-dir "VERSION"))))
      (when (probe-file version-src)
        (handler-case
            (process:run-sync "cp" :args (list version-src version-dst))
          (error (e)
            (log:error "Could not copy VERSION file")
            (format t "Warning: Could not copy VERSION file: ~A~%" e)))))
    
    ;; Copy README and LICENSE if they exist
    (dolist (file '("README.md" "LICENSE"))
      (let ((src-file (path:path-string (path:path-join project-root file)))
            (dst-file (path:path-string (path:path-join release-dir file))))
        (when (probe-file src-file)
          (handler-case
              (process:run-sync "cp" :args (list src-file dst-file))
            (error (e)
              (log:error "Could not copy ~A" file)
              (format t "Warning: Could not copy ~A: ~A~%" file e))))))))

(defun copy-install-instructions (release-dir)
  "Copy installation instructions from external resource."
  (safe-operation "Copying installation instructions"
                  (lambda ()
                    (let ((source-file (make-path (namestring (fs:current-directory)) 
                                                  "docs" "development" "installation.md"))
                          (target-file (make-path release-dir "INSTALL.md")))
                      (fs:copy-file source-file target-file)))))

(defun create-zip-archive (release-dir release-name)
  "Create a ZIP archive for Windows releases."
  (safe-operation (format nil "Creating ZIP archive ~A.zip" release-name)
                  (lambda ()
                    (process:run-sync "zip" :args (list "-r" 
                                                        (format nil "~A.zip" release-name)
                                                        (path:path-name (path:make-path release-dir)))))))

(defun create-checksum-file (archive-name)
  "Create SHA256 checksum file for archive."
  (safe-operation (format nil "Creating checksum for ~A" archive-name)
                  (lambda ()
                    (multiple-value-bind (output error-output exit-code)
                        (process:run-sync "sha256sum" :args (list archive-name))
                      (declare (ignore error-output))
                      (when (zerop exit-code)
                        (fs:write-file-string (format nil "~A.sha256" archive-name) output))))))

(defun create-tar-archive (release-dir release-name)
  "Create a tar.gz archive for Unix releases."
  (safe-operation (format nil "Creating tar.gz archive ~A.tar.gz" release-name)
                  (lambda ()
                    (process:run-sync "tar" :args (list "-czf" 
                                                        (format nil "~A.tar.gz" release-name)
                                                        (path:path-name (path:make-path release-dir))))
                    ;; Create checksum file
                    (create-checksum-file (format nil "~A.tar.gz" release-name)))))

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
               (create-tar-archive release-dir release-name)))
      (fs:change-directory old-cwd))))

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
            (process:run-sync "rm" :args (list "-rf" release-dir))
          (error (e)
            (log:error "Could not remove directory with rm")
            (format t "Directory will be overwritten instead~%"))))
      
      (fs:make-dirs release-dir)
      
      ;; Build all packages
      (build-packages-for-release environment platform-arch)
      
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
