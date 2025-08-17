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

(defun generate-module-testsuite (module-name result)
  "Generate a testsuite XML element for a module's test results."
  (declare (ignore module-name))
  (when (and result (find-package "EPSILON.TEST.SUITE") (find-package "EPSILON.TEST.REPORT"))
    ;; Use the test report's function to generate the testsuite
    (let ((suite-tests-fn (find-symbol "SUITE-TESTS" "EPSILON.TEST.SUITE"))
          (list-suites-fn (find-symbol "LIST-SUITES" "EPSILON.TEST.SUITE"))
          (make-junit-testsuite-fn (find-symbol "MAKE-JUNIT-TESTSUITE" "EPSILON.TEST.REPORT")))
      (when (and suite-tests-fn list-suites-fn make-junit-testsuite-fn)
        ;; Get all test suites in this module
        (let ((suites (funcall list-suites-fn result)))
          ;; Create XML for each suite
          (when suites
            (mapcar (lambda (suite-name)
                      (funcall make-junit-testsuite-fn 
                               suite-name 
                               (funcall suite-tests-fn result suite-name)))
                    suites)))))))

(defun count-module-tests (result)
  "Count total tests in a module result."
  (if (and result (find-package "EPSILON.TEST.SUITE"))
      (let ((tests-fn (find-symbol "TESTS" "EPSILON.TEST.SUITE")))
        (if tests-fn
            (map:size (funcall tests-fn result))
            0))
      0))

(defun count-module-failures (result)
  "Count failures in a module result."
  (if (and result (find-package "EPSILON.TEST.SUITE"))
      (let ((failures-fn (find-symbol "FAILURES" "EPSILON.TEST.SUITE")))
        (if failures-fn
            (length (funcall failures-fn result))
            0))
      0))

(defun count-module-errors (result)
  "Count errors in a module result."
  (if (and result (find-package "EPSILON.TEST.SUITE"))
      (let ((errors-fn (find-symbol "ERRORS" "EPSILON.TEST.SUITE")))
        (if errors-fn
            (length (funcall errors-fn result))
            0))
      0))

(defun get-module-time (result)
  "Get total time for a module's tests."
  (if (and result (find-package "EPSILON.TEST.SUITE"))
      (let ((start-time-fn (find-symbol "START-TIME" "EPSILON.TEST.SUITE"))
            (end-time-fn (find-symbol "END-TIME" "EPSILON.TEST.SUITE")))
        (if (and start-time-fn end-time-fn)
            (/ (- (funcall end-time-fn result)
                  (funcall start-time-fn result))
               internal-time-units-per-second)
            0.0))
      0.0))

(defun merge-test-results (aggregate-run module-run)
  "Merge test results from a module run into the aggregate run."
  (when module-run
    (let ((tests-accessor (find-symbol "TESTS" "EPSILON.TEST.SUITE"))
          (failures-accessor (find-symbol "FAILURES" "EPSILON.TEST.SUITE"))
          (errors-accessor (find-symbol "ERRORS" "EPSILON.TEST.SUITE"))
          (skipped-accessor (find-symbol "SKIPPED" "EPSILON.TEST.SUITE")))
      
      ;; Copy all tests from module-run to aggregate-run
      (when tests-accessor
        (map:each (lambda (k v)
                    (map:assoc! (funcall tests-accessor aggregate-run) k v))
                  (funcall tests-accessor module-run)))
      
      ;; Append failures, errors, and skipped tests
      (when failures-accessor
        (setf (slot-value aggregate-run (intern "FAILURES" "EPSILON.TEST.SUITE"))
              (append (funcall failures-accessor aggregate-run)
                      (funcall failures-accessor module-run))))
      
      (when errors-accessor
        (setf (slot-value aggregate-run (intern "ERRORS" "EPSILON.TEST.SUITE"))
              (append (funcall errors-accessor aggregate-run)
                      (funcall errors-accessor module-run))))
      
      (when skipped-accessor
        (setf (slot-value aggregate-run (intern "SKIPPED" "EPSILON.TEST.SUITE"))
              (append (funcall skipped-accessor aggregate-run)
                      (funcall skipped-accessor module-run)))))))

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

(defun generate-junit-report (all-results file total-tested total-passed failed-modules)
  "Generate an aggregated JUnit XML report from all test results."
  (declare (ignore failed-modules))
  (ensure-directories-exist file)
  
  ;; Check if we have actual test results with details
  (let ((has-detailed-results nil))
    (dolist (module-result all-results)
      (when (and (cdr module-result)
                 (find-package "EPSILON.TEST.SUITE"))
        (let* ((result (cdr module-result))
               (tests-fn (find-symbol "TESTS" "EPSILON.TEST.SUITE")))
          (when (and tests-fn (plusp (map:size (funcall tests-fn result))))
            (setf has-detailed-results t)))))
    
    (if (and has-detailed-results
             (find-package "EPSILON.TEST.REPORT")
             (find-package "EPSILON.XML"))
      ;; Generate detailed JUnit report with all test cases
      (with-open-file (stream file
                              :direction :output 
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (let ((xml-emit (find-symbol "EMIT" "EPSILON.XML"))
              (xml-element (find-symbol "ELEMENT" "EPSILON.XML"))
              (all-testsuites '())
              (total-tests 0)
              (total-failures 0)
              (total-errors 0)
              (total-time 0.0))
          
          ;; Process each module's results
          (dolist (module-result (reverse all-results))
            (let* ((module (car module-result))
                   (result (cdr module-result)))
              (when result
                (let ((suite-xmls (generate-module-testsuite module result)))
                  (when suite-xmls
                    ;; Add all testsuites from this module
                    (dolist (suite-xml suite-xmls)
                      (push suite-xml all-testsuites))
                    ;; Update totals
                    (incf total-tests (count-module-tests result))
                    (incf total-failures (count-module-failures result))
                    (incf total-errors (count-module-errors result))
                    (incf total-time (or (get-module-time result) 0.0)))))))
          
          ;; Write the aggregated XML
          (funcall xml-emit
                   (funcall xml-element "testsuites"
                            :attributes (list "tests" total-tests
                                              "failures" total-failures
                                              "errors" total-errors
                                              "time" (format nil "~,3F" total-time))
                            :children (reverse all-testsuites))
                   stream)))
      
      ;; Fallback to simple summary format if detailed reporting unavailable
      (with-open-file (stream file
                              :direction :output 
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (format stream "<testsuites tests=\"~D\" failures=\"~D\" errors=\"0\" time=\"0.0\">~%"
                total-tested (- total-tested total-passed))
        
        ;; Create a testsuite for each module
        (dolist (module-result (reverse all-results))
          (let* ((module (car module-result))
                 (result (cdr module-result))
                 (success (if result
                             (zerop (+ (length (funcall (find-symbol "FAILURES" "EPSILON.TEST.SUITE") result))
                                      (length (funcall (find-symbol "ERRORS" "EPSILON.TEST.SUITE") result))))
                             nil)))
            (format stream "  <testsuite name=\"~A\" tests=\"1\" failures=\"~D\" errors=\"0\" time=\"0.0\">~%"
                    module (if success 0 1))
            (format stream "    <testcase name=\"~A\" classname=\"epsilon.release\" time=\"0.0\"" module)
            (if success
                (format stream "/>~%")
                (progn
                  (format stream ">~%")
                  (format stream "      <failure message=\"Module tests failed\">Module ~A tests failed</failure>~%" module)
                  (format stream "    </testcase>~%")))
            (format stream "  </testsuite>~%")))
        
        (format stream "</testsuites>~%"))))
  (log:info "JUnit report written to ~A" file))

(defun selftest (&key (environment (loader:environment)) (format :shell) (file nil))
  "Discover all modules and test them sequentially.
   FORMAT can be :shell (default) or :junit for XML output.
   FILE specifies the output file for junit format."
  (log:info "Starting Epsilon Release Self-Test")
  
  ;; Convert string format to keyword for compatibility
  (when (stringp format)
    (setf format (intern (string-upcase format) :keyword)))
  
  
  (let ((modules (get-modules environment))
        (total-tested 0)
        (total-passed 0)
        (failed-modules '())
        (all-results '()))
    
    (log:info "Found ~D modules to test" (length modules))
    
    ;; Ensure epsilon.test is loaded once
    (unless (find-package "EPSILON.TEST")
      (loader:load-module environment "epsilon.test"))
    
    (dolist (module modules)
      (log:info "Testing ~A..." module)
      (incf total-tested)
      
      (let ((result nil))
        (handler-case
            (progn
              ;; Load the module
              (loader:load-module environment module)
              
              ;; Run tests with format and file parameters
              (let* ((test-run-fn (find-symbol "RUN" (find-package "EPSILON.TEST")))
                     (success-p-fn (find-symbol "SUCCESS-P" (find-package "EPSILON.TEST")))
                     (clear-tests-fn (find-symbol "CLEAR-TESTS" (find-package "EPSILON.TEST.SUITE")))
                     ;; For individual modules in junit mode, suppress output
                     (module-format (if (eq format :junit) :none format)))
                (setf result (funcall test-run-fn environment module :format module-format))
                
                (if (funcall success-p-fn result)
                    (progn
                      (unless (eq format :junit)
                        (format t "  ✓ PASSED~%"))
                      (incf total-passed))
                    (progn
                      (unless (eq format :junit)
                        (format t "  ✗ FAILED~%"))
                      (push module failed-modules)))
                
                ;; Clear tests after each package to free memory and prevent accumulation
                (when (and clear-tests-fn (fboundp clear-tests-fn))
                  (funcall clear-tests-fn))))
          
          (error (e)
            (unless (eq format :junit)
              (format t "  ✗ ERROR: ~A~%" e))
            (push module failed-modules)))
        
        ;; Collect results for junit aggregation (always executed)
        (when (eq format :junit)
          (push (cons module result) all-results))))
    
    ;; Generate appropriate output based on format
    (case format
      (:junit
       ;; Generate aggregated JUnit XML report
       (when file
         (generate-junit-report all-results file total-tested total-passed failed-modules)))
      (otherwise
       ;; Summary for console output
       (format t "~%Test Summary:~%")
       (format t "=============~%")
       (format t "Total modules: ~D~%" total-tested)
       (format t "Passed: ~D~%" total-passed)
       (format t "Failed: ~D~%" (- total-tested total-passed))
       
       (when failed-modules
         (format t "~%Failed modules:~%")
         (dolist (module (reverse failed-modules))
           (format t "  - ~A~%" module)))
       
       (format t "~%")))
    
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
  
  (let ((src-dir (path:path-string (path:path-join (namestring (fs:current-directory)) "modules")))
        (target-src (path:path-string (path:path-join release-dir "modules"))))
        
    ;; Copy source tree using standard epsilon.sys.fs functions
    (fs:copy-directory src-dir target-src)
    (format t "  ✓ Source tree copied successfully~%")))

(defun copy-epsilon-script (release-dir)
  "Create a release-specific epsilon wrapper script."
  (format t "Copying epsilon script...~%")
  
  (let ((bin-dir (path:path-string (path:path-join release-dir "bin")))
        (target-script (path:path-string (path:path-join release-dir "bin" "epsilon"))))
    
    (fs:make-dirs bin-dir)
    
    ;; Create a release-specific wrapper script
    (fs:write-file-string target-script 
"#!/bin/sh
#
# Epsilon command-line interface (Release version)
#

set -eu

# The user's working directory
export EPSILON_USER=\"$PWD\"
# The directory where epsilon is installed (parent of bin)
export EPSILON_HOME=\"$(cd \"$(dirname \"$0\")/..\" && pwd)\"
EPSILON_BOOT=\"$EPSILON_HOME/modules/core/src/epsilon.lisp\"

# Detect boot script
if ! [ -f \"$EPSILON_BOOT\" ]; then
    echo \"Error: epsilon boot script not found in $EPSILON_HOME\" >&2
    exit 1
fi

# Use bundled SBCL if available, otherwise system SBCL
if [ -x \"$EPSILON_HOME/bin/sbcl\" ]; then
    SBCL=\"$EPSILON_HOME/bin/sbcl\"
else
    SBCL=\"sbcl\"
fi

# Set SBCL_HOME if using bundled SBCL
if [ -d \"$EPSILON_HOME/lib/sbcl-libs\" ]; then
    export SBCL_HOME=\"$EPSILON_HOME/lib/sbcl-libs\"
fi

# Change to EPSILON_HOME for relative path loading
cd \"$EPSILON_HOME\"

# Execute epsilon with all arguments
exec \"$SBCL\" --script \"$EPSILON_BOOT\" \"$@\"
")
    
    ;; Make executable (assuming Unix-like system)
    #+unix
    (handler-case
        (progn
          (format t "  Making script executable...~%")
          (process:run-sync "chmod" 
                            :args (list "+x" target-script)
                            :check-executable nil))
      (process:command-not-found ()
        (log:warn "chmod command not found - script may not be executable"))
      (error (e)
        (log:warn "Could not make epsilon script executable: ~A" e)))
    (format t "  ✓ Epsilon script created successfully~%")))

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
  (process:run-sync "zip" 
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

(defun create-release-archive (release-dir release-name platform-arch)
  "Create the final release archive."
  (let ((platform (seq:first (str:split #\- platform-arch)))
        (parent-dir (path:path-string (path:path-parent (path:make-path release-dir))))
        (old-cwd (namestring (fs:current-directory))))
    
    (log:info "Creating release archive for ~A in ~A" platform-arch release-dir)
    
    (unwind-protect
         (progn
           (fs:change-directory parent-dir)
           (if (string-equal platform "windows")
               (create-zip-archive release-dir release-name)
               (create-tar-archive-with-working-directory release-dir release-name parent-dir)))
      (fs:change-directory old-cwd))))

(defun create-tar-archive-with-working-directory (release-dir release-name working-dir)
  "Create a tar.gz archive for Unix releases with explicit working directory."
  (handler-case
      (progn
        (process:run-sync "tar" 
                          :args (list "czf" 
                                      (format nil "~a.tar.gz" release-name)
                                      release-name)
                          :working-directory working-dir
                          :check-executable nil)
        (format t "~%")
        ;; Create checksum file - use full path in working directory
        (create-checksum-file (path:path-string 
                               (path:path-join working-dir 
                                               (format nil "~A.tar.gz" release-name)))))
    (process:process-error-condition (e)
      (error "tar command failed with exit code ~A~%Output: ~A~%Error: ~A" 
             (epsilon.process::process-error-exit-code e)
             (epsilon.process::process-error-output e)
             (epsilon.process::process-error-error-output e)))
    (process:command-not-found ()
      (error "tar command not found - please install tar"))
    (process:process-timeout-error ()
      (error "tar command timed out - archive may be too large"))))

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
            (log:error "Could not remove directory: ~A" e)
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
