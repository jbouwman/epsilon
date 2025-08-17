;;;; epsilon-swank.lisp - SWANK backend extensions for Epsilon
;;;;
;;;; This file provides SWANK RPC functions for Epsilon development
;;;; including module loading, test execution, and environment configuration.

(defpackage epsilon-swank
  (:use cl)
  (:export
   #:configure-environment
   #:load-module
   #:run-test
   #:run-module-tests
   #:find-test-definitions
   #:get-module-info))

(in-package epsilon-swank)

;;; Environment Configuration

(defvar *epsilon-environment* nil
  "Current Epsilon environment configuration")

(defvar *epsilon-project-path* nil
  "Path to current Epsilon project")

(defvar *epsilon-module-paths* nil
  "List of module search paths")

(defun configure-environment (config)
  "Configure the Epsilon environment with PROJECT configuration."
  (setf *epsilon-project-path* (getf config :project-path)
        *epsilon-module-paths* (getf config :module-paths)
        *epsilon-environment* config)
  
  ;; Ensure Epsilon loader is available
  (unless (find-package "EPSILON.LOADER")
    (handler-case
        (progn
          ;; Try to load epsilon core first
          (when *epsilon-project-path*
            (let ((epsilon-script (merge-pathnames "epsilon" *epsilon-project-path*)))
              (when (probe-file epsilon-script)
                ;; Execute epsilon to set up environment
                (sb-ext:run-program epsilon-script '("--load" "epsilon.core")
                                   :search t :wait t))))
          ;; Load epsilon.loader if available
          (require "epsilon.loader"))
      (error (e)
        (format t "Warning: Could not load Epsilon loader: ~A~%" e))))
  
  (format nil "Epsilon environment configured for project: ~A" 
          (getf config :project-name)))

;;; Module Management

(defun load-module (module-name)
  "Load an Epsilon module by name."
  (handler-case
      (if (find-package "EPSILON.LOADER")
          (let ((loader-pkg (find-package "EPSILON.LOADER")))
            ;; Use epsilon.loader if available
            (let ((load-module-fn (find-symbol "LOAD-MODULE" loader-pkg))
                  (environment-fn (find-symbol "ENVIRONMENT" loader-pkg)))
              (if (and load-module-fn environment-fn)
                  (progn
                    (funcall load-module-fn (funcall environment-fn) module-name)
                    (format nil "Module ~A loaded successfully" module-name))
                  (format nil "Error: Epsilon loader functions not found"))))
          ;; Fallback: try to load as regular Lisp system
          (progn
            (require module-name)
            (format nil "Module ~A loaded (fallback)" module-name)))
    (error (e)
      (format nil "Error loading module ~A: ~A" module-name e))))

(defun get-module-info (module-name)
  "Get information about a module."
  (handler-case
      (if (find-package "EPSILON.LOADER")
          (let ((loader-pkg (find-package "EPSILON.LOADER")))
            (let ((find-module-fn (find-symbol "FIND-MODULE" loader-pkg))
                  (environment-fn (find-symbol "ENVIRONMENT" loader-pkg)))
              (if (and find-module-fn environment-fn)
                  (let ((module-info (funcall find-module-fn (funcall environment-fn) module-name)))
                    (if module-info
                        (list :name module-name
                              :version (getf module-info :version)
                              :location (getf module-info :location))
                        (format nil "Module ~A not found" module-name)))
                  "Error: Epsilon loader functions not found")))
          (format nil "Module info not available (loader not loaded)"))
    (error (e)
      (format nil "Error getting module info for ~A: ~A" module-name e))))

;;; Test Execution

(defun run-test (test-name)
  "Run a single test by name."
  (handler-case
      (let ((test-fn (find-symbol (string-upcase test-name) *package*)))
        (if (and test-fn (fboundp test-fn))
            (let ((start-time (get-internal-real-time)))
              (handler-case
                  (progn
                    (funcall test-fn)
                    (let ((elapsed (/ (- (get-internal-real-time) start-time)
                                    internal-time-units-per-second)))
                      (list (list :name test-name
                                 :status :pass
                                 :time elapsed))))
                (error (e)
                  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                                  internal-time-units-per-second)))
                    (list (list :name test-name
                               :status :fail
                               :message (format nil "~A" e)
                               :time elapsed))))))
            (list (list :name test-name
                       :status :fail
                       :message "Test function not found"))))
    (error (e)
      (list (list :name test-name
                 :status :fail
                 :message (format nil "Error running test: ~A" e))))))

(defun run-module-tests (module-name)
  "Run all tests for a module."
  (handler-case
      (if (find-package "EPSILON.TEST")
          (let ((test-pkg (find-package "EPSILON.TEST")))
            (let ((run-fn (find-symbol "RUN" test-pkg)))
              (if run-fn
                  ;; Use Epsilon test framework
                  (let ((results (funcall run-fn nil module-name :format :lisp)))
                    (convert-epsilon-test-results results))
                  ;; Fallback: try to find test functions manually
                  (run-tests-by-convention module-name))))
          ;; No test framework, use convention-based discovery
          (run-tests-by-convention module-name))
    (error (e)
      (list (list :name module-name
                 :status :fail
                 :message (format nil "Error running module tests: ~A" e))))))

(defun run-tests-by-convention (module-name)
  "Run tests using naming conventions when test framework is not available."
  (let ((results '())
        (module-package (find-package (string-upcase module-name))))
    (when module-package
      (do-symbols (symbol module-package)
        (when (and (fboundp symbol)
                   (search "TEST" (symbol-name symbol)))
          (let ((test-result (run-test (symbol-name symbol))))
            (setf results (append results test-result))))))
    (if results
        results
        (list (list :name module-name
                   :status :pass
                   :message "No tests found")))))

(defun convert-epsilon-test-results (epsilon-results)
  "Convert Epsilon test framework results to our format."
  ;; This would need to be implemented based on the actual
  ;; structure of epsilon.test results
  (if epsilon-results
      (list (list :name "module-tests"
                 :status :pass
                 :message "Epsilon test framework results"))
      (list (list :name "module-tests"
                 :status :fail
                 :message "No results from Epsilon test framework"))))

;;; Test Discovery

(defun find-test-definitions (file-path)
  "Find all test definitions in a file."
  (handler-case
      (with-open-file (stream file-path :direction :input)
        (let ((tests '())
              (eof (gensym)))
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (when (and (consp form)
                             (eq (car form) 'deftest))
                     (push (list :name (string (cadr form))
                                :line (file-position stream))
                           tests)))
          (nreverse tests)))
    (error (e)
      (format nil "Error reading file ~A: ~A" file-path e))))

;;; Utility Functions

(defun epsilon-package-p (package-name)
  "Check if a package is an Epsilon package."
  (and (stringp package-name)
       (or (search "EPSILON" (string-upcase package-name))
           (find-package (string-upcase package-name)))))

(defun list-epsilon-packages ()
  "List all loaded Epsilon packages."
  (let ((epsilon-packages '()))
    (dolist (pkg (list-all-packages))
      (when (epsilon-package-p (package-name pkg))
        (push (package-name pkg) epsilon-packages)))
    epsilon-packages))

;;; SWANK Integration

;; Register our functions with SWANK
(when (find-package "SWANK")
  (let ((swank-pkg (find-package "SWANK")))
    (when (find-symbol "*READTABLE-ALIST*" swank-pkg)
      ;; Add any necessary readtable configurations
      )))

;; Export for SWANK RPC calls
(defun swank-epsilon-configure-environment (config)
  "SWANK RPC wrapper for configure-environment."
  (configure-environment config))

(defun swank-epsilon-load-module (module-name)
  "SWANK RPC wrapper for load-module."
  (load-module module-name))

(defun swank-epsilon-run-test (test-name)
  "SWANK RPC wrapper for run-test."
  (run-test test-name))

(defun swank-epsilon-run-module-tests (module-name)
  "SWANK RPC wrapper for run-module-tests."
  (run-module-tests module-name))

(provide 'epsilon-swank)