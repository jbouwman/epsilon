;;;; Epsilon CLI Development Tools
;;;;
;;;; Provides project initialization, module scaffolding, and diagnostic tools.

(defpackage epsilon.cli
  (:use cl)
  (:local-nicknames
   (fs epsilon.sys.fs)
   (env epsilon.sys.env)
   (path epsilon.path)
   (str epsilon.string)
   (map epsilon.map)
   (log epsilon.log))
  (:export
   ;; Project initialization
   init

   ;; Module scaffolding
   new-module

   ;; Diagnostics
   doctor

   ;; Utilities
   list-templates))

(in-package epsilon.cli)

;;; ============================================================================
;;; Project Initialization (epsilon init)
;;; ============================================================================

(defparameter *project-template*
  '(("module.lisp" . "(:name \"~A\"
 :version \"0.1.0\"
 :description \"~A\")
")
    ("src/main.lisp" . ";;;; ~A - Main Module
;;;;
;;;; Description: ~A

(defpackage ~A
  (:use cl)
  (:export
   ;; Add your exports here
   main))

(in-package ~A)

(defun main (&rest args)
  \"Main entry point for ~A.\"
  (declare (ignore args))
  (format t \"Hello from ~A!~%\"))
")
    ("tests/main-tests.lisp" . ";;;; Tests for ~A

(defpackage ~A.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (main ~A)))

(in-package ~A.tests)

(defsuite ~A-tests
  :description \"Tests for ~A\")

(deftest example-test
  :description \"An example test\"
  :suite ~A-tests
  (is (= 1 1)))
")
    ("README.md" . "# ~A

~A

## Installation

```lisp
;; Load the module
(epsilon.loader:load-module env \"~A\")
```

## Usage

```lisp
(~A:main)
```

## License

MIT
"))
  "Template files for a new project.")

(defun sanitize-name (name)
  "Convert a name to a valid Lisp package name."
  (str:downcase (str:replace-all name "/" ".")))

(defun project-exists-p (directory)
  "Check if a project already exists in the directory."
  (let ((module-file (path:path-string (path:path-join directory "module.lisp"))))
    (probe-file module-file)))

(defun create-project-structure (directory name description)
  "Create the project directory structure and files."
  (let ((pkg-name (sanitize-name name)))
    ;; Create directories
    (fs:make-dirs (path:path-string (path:path-join directory "src")))
    (fs:make-dirs (path:path-string (path:path-join directory "tests")))

    ;; Create files from templates
    (dolist (template *project-template*)
      (let* ((filename (car template))
             (content-template (cdr template))
             (filepath (path:path-string (path:path-join directory filename)))
             (content (apply #'format nil content-template
                            (make-list 20 :initial-element
                                      (if (str:contains-p "README" filename)
                                          name
                                          pkg-name)))))
        ;; Handle description substitution
        (when (str:contains-p "~A" content-template)
          (setf content (format nil content-template
                               pkg-name description pkg-name pkg-name
                               pkg-name pkg-name pkg-name pkg-name
                               pkg-name pkg-name pkg-name pkg-name
                               pkg-name pkg-name pkg-name)))
        (fs:write-file-string filepath content)
        (format t "  Created ~A~%" filename)))))

(defun init (&key name description directory force)
  "Initialize a new Epsilon project.

   Options:
     :name        - Project name (default: directory name)
     :description - Project description
     :directory   - Target directory (default: current directory)
     :force       - Overwrite existing project

   Examples:
     epsilon --exec epsilon.cli:init
     epsilon --exec epsilon.cli:init -- --name my-project
     epsilon --exec epsilon.cli:init -- --name my-project --description \"My cool project\""

  (let* ((target-dir (or directory
                         (namestring (or (probe-file ".")
                                        (sb-posix:getcwd)))))
         (project-name (or name
                          (path:path-name (path:make-path target-dir))
                          "my-project"))
         (project-desc (or description
                          (format nil "An Epsilon project: ~A" project-name))))

    (format t "~%Epsilon Project Initialization~%")
    (format t "==============================~%~%")
    (format t "  Project:     ~A~%" project-name)
    (format t "  Directory:   ~A~%" target-dir)
    (format t "  Description: ~A~%~%" project-desc)

    ;; Check for existing project
    (when (and (project-exists-p target-dir) (not force))
      (format *error-output* "Error: Project already exists in ~A~%" target-dir)
      (format *error-output* "Use --force to overwrite.~%")
      (return-from init nil))

    ;; Create project structure
    (format t "Creating project structure...~%")
    (create-project-structure target-dir project-name project-desc)

    (format t "~%Project initialized successfully!~%~%")
    (format t "Next steps:~%")
    (format t "  cd ~A~%" target-dir)
    (format t "  epsilon --module ~A~%" (sanitize-name project-name))
    (format t "  epsilon --test ~A~%" (sanitize-name project-name))
    t))

;;; ============================================================================
;;; Module Scaffolding (epsilon new)
;;; ============================================================================

(defparameter *module-templates*
  '((:basic . (("module.lisp" . "(:name \"epsilon.~A\"
 :version \"1.0.0\"
 :description \"~A\")
")
               ("src/~A.lisp" . ";;;; ~A Module
;;;;
;;;; ~A

(defpackage epsilon.~A
  (:use cl)
  (:export
   ;; Add exports here
   ))

(in-package epsilon.~A)

;; Add your code here
")
               ("tests/~A-tests.lisp" . ";;;; Tests for epsilon.~A

(defpackage epsilon.~A.tests
  (:use cl epsilon.test))

(in-package epsilon.~A.tests)

(defsuite ~A-tests
  :description \"Tests for epsilon.~A\")

(deftest placeholder-test
  :suite ~A-tests
  :description \"Placeholder test\"
  (is t))
")))

    (:library . (("module.lisp" . "(:name \"epsilon.~A\"
 :version \"1.0.0\"
 :description \"~A\"
 :requires ())
")
                 ("src/~A.lisp" . ";;;; ~A Library Module
;;;;
;;;; ~A
;;;;
;;;; Usage:
;;;;   (epsilon.loader:load-module env \"epsilon.~A\")
;;;;   (epsilon.~A:function-name args)

(defpackage epsilon.~A
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   ;; Public API
   ))

(in-package epsilon.~A)

;;; ============================================================================
;;; Public API
;;; ============================================================================

;; Add your public functions here

;;; ============================================================================
;;; Internal Functions
;;; ============================================================================

;; Add your internal functions here
")
                 ("tests/~A-tests.lisp" . ";;;; Tests for epsilon.~A

(defpackage epsilon.~A.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (~A epsilon.~A)))

(in-package epsilon.~A.tests)

(defsuite ~A-tests
  :description \"Tests for epsilon.~A\")

;; Add your tests here
")
                 ("README.md" . "# epsilon.~A

~A

## Installation

```lisp
(epsilon.loader:load-module env \"epsilon.~A\")
```

## API

TODO: Document the public API

## Examples

```lisp
;; Example usage
```
")))

    (:cli . (("module.lisp" . "(:name \"epsilon.~A\"
 :version \"1.0.0\"
 :description \"~A\"
 :requires ())
")
             ("src/~A.lisp" . ";;;; ~A CLI Tool
;;;;
;;;; ~A
;;;;
;;;; Usage:
;;;;   epsilon --exec epsilon.~A:main -- [args]

(defpackage epsilon.~A
  (:use cl)
  (:local-nicknames
   (argparse epsilon.argparse)
   (map epsilon.map)
   (str epsilon.string)
   (log epsilon.log))
  (:export
   main))

(in-package epsilon.~A)

(defun make-parser ()
  \"Create the argument parser for this CLI tool.\"
  (let ((parser (argparse:make-parser
                 :command \"~A\"
                 :description \"~A\")))
    ;; Add your arguments here
    (argparse:add-argument parser \"--verbose\"
                           :action 'store-true
                           :help \"Enable verbose output\")
    parser))

(defun main (&rest args)
  \"Main entry point for ~A CLI.\"
  (let* ((parser (make-parser))
         (parsed (argparse:parse-args parser args))
         (options (argparse:parsed-options parsed)))
    (when (map:get options \"verbose\")
      (log:info \"Running in verbose mode\"))
    ;; Add your main logic here
    (format t \"Hello from ~A!~%\")))
")
             ("tests/~A-tests.lisp" . ";;;; Tests for epsilon.~A

(defpackage epsilon.~A.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (~A epsilon.~A)))

(in-package epsilon.~A.tests)

(defsuite ~A-tests
  :description \"Tests for epsilon.~A CLI tool\")

(deftest cli-loads
  :suite ~A-tests
  :description \"CLI module loads successfully\"
  (is (find-package :epsilon.~A)))
"))))
  "Templates for different types of modules.")

(defun get-template (template-name)
  "Get template by name."
  (cdr (assoc template-name *module-templates*)))

(defun list-templates ()
  "List available module templates."
  (format t "~%Available templates:~%")
  (format t "  basic   - Simple module with minimal structure~%")
  (format t "  library - Library module with full documentation~%")
  (format t "  cli     - Command-line tool with argument parsing~%"))

(defun create-module-from-template (directory name description template-files)
  "Create a module from template files."
  ;; Create directories
  (fs:make-dirs (path:path-string (path:path-join directory "src")))
  (fs:make-dirs (path:path-string (path:path-join directory "tests")))

  ;; Create files from templates
  (dolist (template template-files)
    (let* ((filename-template (car template))
           (content-template (cdr template))
           (filename (format nil filename-template name name name name name
                            name name name name name name name name name name))
           (filepath (path:path-string (path:path-join directory filename)))
           (content (format nil content-template
                           name description name name description
                           name name name name name name name
                           name name name name name name name
                           name name name name name name name)))
      (fs:write-file-string filepath content)
      (format t "  Created ~A~%" filename))))

(defun new-module (&key name description (template :basic) directory)
  "Create a new module from a template.

   Options:
     :name        - Module name (required)
     :description - Module description
     :template    - Template type (:basic, :library, :cli)
     :directory   - Target directory (default: modules/<name>)

   Examples:
     epsilon --exec epsilon.cli:new-module -- --name mymodule
     epsilon --exec epsilon.cli:new-module -- --name myutil --template library
     epsilon --exec epsilon.cli:new-module -- --name mytool --template cli"

  (unless name
    (format *error-output* "Error: Module name is required.~%")
    (format *error-output* "Usage: epsilon --exec epsilon.cli:new-module -- --name <name>~%")
    (return-from new-module nil))

  (let* ((template-key (if (keywordp template)
                           template
                           (intern (str:upcase template) :keyword)))
         (template-files (get-template template-key))
         (module-name (str:downcase name))
         (target-dir (or directory
                        (path:path-string
                         (path:path-join (namestring (fs:current-directory))
                                        "modules" module-name))))
         (module-desc (or description
                         (format nil "The ~A module" module-name))))

    (unless template-files
      (format *error-output* "Error: Unknown template '~A'~%" template)
      (list-templates)
      (return-from new-module nil))

    (format t "~%Creating Module~%")
    (format t "===============~%~%")
    (format t "  Name:        epsilon.~A~%" module-name)
    (format t "  Template:    ~A~%" template-key)
    (format t "  Directory:   ~A~%" target-dir)
    (format t "  Description: ~A~%~%" module-desc)

    ;; Check if directory exists
    (when (probe-file target-dir)
      (format *error-output* "Error: Directory already exists: ~A~%" target-dir)
      (return-from new-module nil))

    ;; Create module
    (format t "Creating module structure...~%")
    (create-module-from-template target-dir module-name module-desc template-files)

    (format t "~%Module created successfully!~%~%")
    (format t "Next steps:~%")
    (format t "  epsilon --module epsilon.~A~%" module-name)
    (format t "  epsilon --test epsilon.~A~%" module-name)
    t))

;;; ============================================================================
;;; Environment Diagnostics (epsilon doctor)
;;; ============================================================================

(defun check-sbcl ()
  "Check SBCL installation and version."
  (list :name "SBCL"
        :status :ok
        :version (lisp-implementation-version)
        :details (format nil "~A ~A"
                        (lisp-implementation-type)
                        (lisp-implementation-version))))

(defun check-epsilon-home ()
  "Check EPSILON_HOME environment variable."
  (let ((home (sb-ext:posix-getenv "EPSILON_HOME")))
    (if home
        (if (probe-file home)
            (list :name "EPSILON_HOME"
                  :status :ok
                  :details home)
            (list :name "EPSILON_HOME"
                  :status :warning
                  :details (format nil "Set but directory not found: ~A" home)))
        (list :name "EPSILON_HOME"
              :status :info
              :details "Not set (using current directory)"))))

(defun check-modules-directory ()
  "Check modules directory exists and contains modules."
  (let* ((epsilon-home (or (sb-ext:posix-getenv "EPSILON_HOME")
                          (namestring (fs:current-directory))))
         (modules-dir (path:path-string (path:path-join epsilon-home "modules"))))
    (if (probe-file modules-dir)
        (let ((module-count (length (directory (format nil "~A/*/module.lisp" modules-dir)))))
          (list :name "Modules Directory"
                :status :ok
                :details (format nil "~A (~D modules found)" modules-dir module-count)))
        (list :name "Modules Directory"
              :status :error
              :details (format nil "Not found: ~A" modules-dir)))))

(defun check-core-module ()
  "Check epsilon.core module is loadable."
  (let ((core-file (path:path-string
                    (path:path-join (or (sb-ext:posix-getenv "EPSILON_HOME")
                                       (namestring (fs:current-directory)))
                                   "modules" "core" "module.lisp"))))
    (if (probe-file core-file)
        (list :name "Core Module"
              :status :ok
              :details "epsilon.core is available")
        (list :name "Core Module"
              :status :error
              :details "epsilon.core not found"))))

(defun check-platform ()
  "Check platform compatibility."
  (let ((platform (env:platform))
        (arch (machine-type)))
    (list :name "Platform"
          :status :ok
          :details (format nil "~A ~A" platform arch))))

(defun check-memory ()
  "Check available memory."
  (let* ((dynamic-usage (sb-kernel:dynamic-usage))
         (dynamic-space-size (sb-ext:dynamic-space-size))
         (usage-percent (* 100.0 (/ dynamic-usage dynamic-space-size))))
    (list :name "Memory"
          :status (if (< usage-percent 80) :ok :warning)
          :details (format nil "~,1F MB used of ~,1F MB (~,1F%)"
                          (/ dynamic-usage 1048576.0)
                          (/ dynamic-space-size 1048576.0)
                          usage-percent))))

(defun check-path ()
  "Check if epsilon is in PATH."
  (let ((path (sb-ext:posix-getenv "PATH")))
    (if (and path (search ".epsilon" path))
        (list :name "PATH"
              :status :ok
              :details "epsilon appears to be in PATH")
        (list :name "PATH"
              :status :info
              :details "Consider adding ~/.local/bin to PATH"))))

(defun format-check-result (result)
  "Format a check result for display."
  (let* ((status (getf result :status))
         (status-str (case status
                       (:ok "[OK]")
                       (:warning "[WARN]")
                       (:error "[ERROR]")
                       (:info "[INFO]")))
         (name (getf result :name))
         (details (getf result :details)))
    (format t "  ~8A ~20A ~A~%" status-str name details)))

(defun doctor (&key verbose)
  "Run diagnostic checks on the Epsilon environment.

   Options:
     :verbose - Show additional diagnostic information

   Examples:
     epsilon --exec epsilon.cli:doctor
     epsilon --exec epsilon.cli:doctor -- --verbose"

  (format t "~%Epsilon Environment Diagnostics~%")
  (format t "================================~%~%")

  (let ((checks (list
                 (check-sbcl)
                 (check-platform)
                 (check-epsilon-home)
                 (check-modules-directory)
                 (check-core-module)
                 (check-memory)
                 (check-path)))
        (errors 0)
        (warnings 0))

    ;; Display results
    (dolist (check checks)
      (format-check-result check)
      (case (getf check :status)
        (:error (incf errors))
        (:warning (incf warnings))))

    ;; Verbose output
    (when verbose
      (format t "~%Additional Information:~%")
      (format t "  Lisp features: ~A~%" (length *features*))
      (format t "  Packages loaded: ~A~%" (length (list-all-packages)))
      (format t "  Current directory: ~A~%" (namestring (fs:current-directory))))

    ;; Summary
    (format t "~%")
    (cond
      ((plusp errors)
       (format t "Found ~D error(s) and ~D warning(s).~%" errors warnings)
       (format t "Please fix the errors before using Epsilon.~%"))
      ((plusp warnings)
       (format t "Found ~D warning(s). Epsilon should work, but check the warnings.~%" warnings))
      (t
       (format t "All checks passed! Epsilon is ready to use.~%")))

    (zerop errors)))
