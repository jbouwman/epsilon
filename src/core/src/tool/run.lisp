;;;; Run command implementation for executing local packages
;;;;
;;;; This module provides the 'epsilon run' command functionality,
;;;; allowing direct execution of packages from their source directories
;;;; without building executables.

(defpackage #:epsilon.tool.run
  (:use #:cl)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:build #:epsilon.tool.build))
  (:export
   #:run-package
   #:handle-run
   #:handle-run-help
   #:compile-local-package))

(in-package #:epsilon.tool.run)

(defparameter *default-source-dirs* '("src" "source" "lib")
  "Default source directories to search if not specified")

(defun find-package-file ()
  "Find package.lisp in current directory or search upward"
  ;; Use the original user directory if available (set by epsilon script)
  (let* ((user-dir (or (sb-ext:posix-getenv "EPSILON_USER_DIR")
                       (fs:current-directory)))
         (package-path (merge-pathnames "package.lisp" 
                                        (pathname (ensure-trailing-slash user-dir)))))
    (probe-file package-path)))

(defun ensure-trailing-slash (path-string)
  "Ensure path string ends with slash for directory merging"
  (if (and path-string (not (char= (char path-string (1- (length path-string))) #\/)))
      (concatenate 'string path-string "/")
      path-string))

(defun read-package-definition (package-file)
  "Read and parse package.lisp file"
  (handler-case
      (with-open-file (stream package-file)
        (let ((form (read stream nil nil)))
          (unless form
            (error "Empty package.lisp file"))
          (unless (and (listp form) (evenp (length form)))
            (error "Invalid package.lisp format - expected property list"))
          form))
    (error (e)
      (error "Failed to read package.lisp: ~A" e))))

(defun validate-package-definition (package-def package-name)
  "Validate package definition has required fields"
  (let ((name (getf package-def :name))
        (main (getf package-def :main)))
    
    ;; Check package name matches
    (unless name
      (error "Package definition missing :name field"))
    
    (unless (string-equal name package-name)
      (error "Package name mismatch: requested '~A' but found '~A'" 
             package-name name))
    
    ;; Check main entry point exists
    (unless main
      (error "Package definition missing :main entry point~%~
              Add :main \"package:function\" to package.lisp"))
    
    t))

(defun parse-main-spec (spec)
  "Parse main function specification like 'package:function' or just 'function'"
  (let* ((spec-string (string spec))
         (colon-pos (position #\: spec-string)))
    (if colon-pos
        ;; package:function format
        (let ((package-name (subseq spec-string 0 colon-pos))
              (function-name (subseq spec-string (1+ colon-pos))))
          (cons package-name function-name))
        ;; just function name - use current package
        (cons nil spec-string))))

(defun resolve-main-function (main-spec)
  "Resolve main function from specification"
  (destructuring-bind (package-spec . function-spec) 
      (parse-main-spec main-spec)
    (let* ((package (if package-spec
                        (find-package (string-upcase package-spec))
                        *package*))
           (symbol (when package
                     (find-symbol (string-upcase function-spec) package))))
      
      (unless package
        (error "Package ~A not found for main function ~A" 
               package-spec main-spec))
      
      (unless symbol
        (error "Function ~A not found in package ~A" 
               function-spec (package-name package)))
      
      (unless (fboundp symbol)
        (error "Symbol ~A is not a function" symbol))
      
      symbol)))


(defun load-package-dependencies (dependencies)
  "Load required dependencies"
  (dolist (dep dependencies)
    (unless (build:is-module-loaded dep)
      (format t ";;; Loading dependency: ~A~%" dep)
      (handler-case
          (build:build dep)
        (error (e)
          (error "Failed to load dependency ~A: ~A" dep e))))))

(defun run-package (package-name &rest args)
  "Run a package from the current directory"
  ;; Find package.lisp
  (let ((package-file (find-package-file)))
    (unless package-file
      (error "No package.lisp found in current directory~%~
              Create a package.lisp file with:~%~
              (:name \"~A\"~%~
               :sources (\"src\")~%~
               :main \"~A:main\")"
             package-name package-name))
    
    ;; Read and validate package definition
    (let* ((package-def (read-package-definition package-file))
           (dependencies (getf package-def :dependencies))
           (main-spec (getf package-def :main))
           (base-dir (or (sb-ext:posix-getenv "EPSILON_USER_DIR")
                         (directory-namestring package-file))))
      
      ;; Validate package
      (validate-package-definition package-def package-name)
      
      ;; Load dependencies
      (when dependencies
        (load-package-dependencies dependencies))
      
      ;; Load source files using build system
      (format t ";;; Loading sources using build system~%")
      (let ((project (funcall (find-symbol "LOAD-PROJECT" "EPSILON.TOOL.BUILD") base-dir)))
        (funcall (find-symbol "%BUILD" "EPSILON.TOOL.BUILD") project :force nil :include-tests nil))
      
      ;; Resolve and call main function
      (let ((main-fn (resolve-main-function main-spec)))
        (format t ";;; Starting ~A~%" package-name)
        (apply main-fn args)))))

(defun handle-run (parsed-args)
  "Handle the run command from epsilon CLI"
  (let* ((args (funcall (find-symbol "PARSED-ARGS-ARGUMENTS" "EPSILON.TOOL.DEV") 
                         parsed-args))
         (options (funcall (find-symbol "PARSED-ARGS-OPTIONS" "EPSILON.TOOL.DEV") 
                           parsed-args))
         (package-name (first args))
         (package-args (rest args)))
    
    ;; Check for help
    (when (or (map:get options "help") (null package-name))
      (handle-run-help parsed-args)
      (return-from handle-run))
    
    ;; Run the package
    (handler-case
        (apply #'run-package package-name package-args)
      (error (e)
        (format *error-output* "~%ERROR: ~A~%" e)
        (sb-ext:exit :code 1)))))

(defun handle-run-help (parsed-args)
  "Show help for run command"
  (declare (ignore parsed-args))
  (format t "run - Execute a package from the current directory~%~%")
  (format t "Usage: epsilon run <package-name> [arguments...]~%~%")
  (format t "Loads and executes a package defined in package.lisp in the current directory.~%~%")
  (format t "Package.lisp format:~%")
  (format t "  (:name \"package-name\"~%")
  (format t "   :version \"1.0.0\"~%")
  (format t "   :sources (\"src\")           ; Directories containing source files~%")
  (format t "   :dependencies (\"epsilon.core\") ; Required epsilon modules~%")
  (format t "   :main \"package:main\"       ; Entry point function~%")
  (format t "   :description \"Package description\")~%~%")
  (format t "Examples:~%")
  (format t "  epsilon run my-service                    # Run with defaults~%")
  (format t "  epsilon run my-service --port 8080        # Pass arguments~%")
  (format t "  epsilon run my-service --help             # Show package help~%~%")
  (format t "The main function will be called with command-line arguments as strings.~%"))

(defun compile-local-package (package-name base-path)
  "Compile all source files in a local package using build system"
  (declare (ignore package-name))  ; silence unused variable warning
  (let ((package-file (path:path-join base-path "package.lisp")))
    (when (fs:exists-p package-file)
      (let* ((project (funcall (find-symbol "LOAD-PROJECT" "EPSILON.TOOL.BUILD") base-path))
             (name (funcall (find-symbol "PROJECT-NAME" "EPSILON.TOOL.BUILD") project)))
        (format t "~%Compiling local package: ~A using build system~%" name)
        (funcall (find-symbol "%BUILD" "EPSILON.TOOL.BUILD") project :force t :include-tests nil)
        (format t "~%Local package compilation complete.~%")))))