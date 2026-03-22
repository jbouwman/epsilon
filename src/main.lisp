;;;; The CLI entry point for Epsilon using a subcommand-based interface similar
;;;; to git
;;;;
;;;; Usage:
;;;;   epsilon <command> [options] [-- args]

(defpackage epsilon.main
  (:use cl)
  (:shadow defpackage)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (loader epsilon.loader)
   (path epsilon.path)
   (fs epsilon.file)
   (log epsilon.log)
   (env epsilon.sys.env)
   (commands epsilon.commands)
   (discovery epsilon.file)
   (project epsilon.project))
  (:export
   cli-run
   defpackage
   package-name-to-module-name))

(in-package epsilon.main)

;;; Version Information

(defun get-version-info ()
  "Get version information as a map."
  (map:make-map
   "version" (env:version)
   "sbcl_version" (lisp-implementation-version)
   "os" (string-downcase (symbol-name (env:platform)))
   "architecture" (machine-type)
   "epsilon_home" (or (sb-ext:posix-getenv "EPSILON_HOME") "")
   "lisp_implementation" (lisp-implementation-type)))

(defun format-version-table ()
  "Format version information as a structured table string."
  (let ((info (get-version-info)))
    (format nil "~
+-----------------------------------------+
|                EPSILON                  |
+-----------------------------------------+
| Version:      ~24A |
| SBCL:         ~24A |
| OS:           ~24A |
| Architecture: ~24A |
+-----------------------------------------+"
            (map:get info "version")
            (map:get info "sbcl_version")
            (map:get info "os")
            (map:get info "architecture"))))

(defun format-version-json ()
  "Format version information as JSON string."
  (let ((info (get-version-info)))
    (format nil "{\"version\":\"~A\",\"sbcl_version\":\"~A\",\"os\":\"~A\",\"architecture\":\"~A\",\"epsilon_home\":\"~A\",\"lisp_implementation\":\"~A\"}"
            (map:get info "version")
            (map:get info "sbcl_version")
            (map:get info "os")
            (map:get info "architecture")
            (map:get info "epsilon_home")
            (map:get info "lisp_implementation"))))

;;; Help Display

(defun show-help ()
  "Display help message with commands dynamically populated from the registry."
  (format t "~%Epsilon - A Lisp Environment~%")
  (format t "============================~%~%")
  (format t "Usage: epsilon <command> [options] [-- args]~%~%")

  (let ((all-cmds (commands:list-commands)))
    (when all-cmds
      (format t "Commands:~%")
      (dolist (cmd all-cmds)
        (format t "  ~28A ~A~%"
                (or (commands:command-usage-hint cmd)
                    (commands:command-name cmd))
                (commands:command-description cmd)))))

  (format t "~%Preamble options (usable with any command, repeatable):~%")
  (format t "  --package, -p MODULE       Pre-load a module~%")
  (format t "  --load, -l FILE            Load a Lisp file~%")
  (format t "  --eval, -e EXPR            Evaluate a Lisp expression~%")

  (format t "~%Global options:~%")
  (format t "  --version                  Show version information~%")
  (format t "  --version-json             Show version as JSON~%")
  (format t "  --log SPEC                 Configure logging (repeatable)~%")
  (format t "  --quiet                    Shorthand for --log quiet~%")
  (format t "  --project PATH             Use project manifest at PATH~%")
  (format t "  --help                     Show this help message~%")
  (format t "~%Log spec syntax:~%")
  (format t "  LEVEL                      Set root level (trace/debug/info/warn/error/fatal)~%")
  (format t "  TARGET=LEVEL               Set level for specific logger~%")
  (format t "  +FORMAT                    Set format (compact/simple/detailed/json)~%")
  (format t "  @PATH                      Tee output to file~%")
  (format t "  verbose|quiet|silent       Presets for debug/warn/fatal~%")
  (format t "  Comma-separated:           --log info,hemidemi=debug,+json~%")

  (format t "~%Use 'epsilon <command> --help' for more information on a command.~%")
  (format t "~%Documentation: https://github.com/jbouwman/epsilon~%")
  (format t "Version: ~A~%" (env:version)))

;;; Argument Parsing Helpers

(defun split-args-at-delimiter (args &optional (delimiter "--"))
  "Split argument list at delimiter. Returns (values before-args after-args)."
  (let ((delimiter-pos (position delimiter args :test #'string=)))
    (if delimiter-pos
        (values (subseq args 0 delimiter-pos)
                (subseq args (1+ delimiter-pos)))
        (values args nil))))

(defun extract-global-options (args)
  "Extract global options from args.
   Returns (values remaining-args options-plist).
   Multiple --log flags are concatenated: --log info --log hemidemi=debug
   becomes \"info,hemidemi=debug\"."
  (let ((remaining '())
        (log-specs '())
        (quiet nil)
        (project-path nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; --quiet is retained for backwards compatibility in shell
               ;; scripts; it is equivalent to --log quiet.
               ((string= arg "--quiet")
                (push "quiet" log-specs)
                (setf quiet t)
                (incf i))
               ((string= arg "--log")
                (when (< (1+ i) (length args))
                  (push (nth (1+ i) args) log-specs)
                  (incf i 2)))
               ((string= arg "--project")
                (when (< (1+ i) (length args))
                  (setf project-path (nth (1+ i) args))
                  (incf i 2)))
               (t
                (push arg remaining)
                (incf i))))
    (let ((combined-log (when log-specs
                          (str:join #\, (seq:seq (nreverse log-specs))))))
      (values (nreverse remaining)
              (list :quiet quiet :log combined-log :project project-path)))))

;;; Environment Setup

(defun setup-environment ()
  "Create and configure a build environment."
  (let ((environment (loader:environment)))
    ;; Set global REPL environment
    (setf loader:*environment* environment)
    environment))

;;; Interactive REPL

(defun start-interactive-repl ()
  "Start the epsilon interactive REPL."
  (format t "Epsilon REPL~%")
  (format t "Type (quit) or Ctrl+D to exit~%~%")
  (sb-impl::toplevel-init))

(defun package-name-to-module-name (package-name)
  "Derive module name from a package name.
   Returns the epsilon module name for auto-loading, or nil if not an epsilon package."
  (let* ((name-lower (string-downcase (string package-name)))
         (parts (seq:realize (str:split #\. name-lower)))
         (num-parts (length parts)))
    (cond
      ((or (< num-parts 2)
           (not (string-equal "epsilon" (first parts))))
       nil)
      ((= num-parts 2) name-lower)
      (t (format nil "~A.~A" (first parts) (second parts))))))

;;; Extended defpackage Macro
;;;
;;; This macro extends CL:DEFPACKAGE with two new options:
;;;
;;;   :require - Load module dependencies and create local nicknames
;;;   :enter   - Emit (in-package name) after package definition
;;;
;;; All standard CL defpackage options pass through unchanged.
;;;
;;; Example:
;;;   (defpackage my-module
;;;     (:use :cl)
;;;     (:require (epsilon.http http)
;;;               (epsilon.crypto crypto))
;;;     (:export start stop)
;;;     (:enter t))

(defun generate-module-load-form (pkg-name)
  "Generate code to load the module for PKG-NAME if not already loaded."
  (let ((module-name (package-name-to-module-name pkg-name))
        (pkg-name-str (string-upcase (string pkg-name))))
    (when module-name
      `(unless (find-package ,pkg-name-str)
         (when (and (boundp 'loader:*environment*)
                    loader:*environment*)
           (let ((env loader:*environment*))
             (unless (loader:module-loaded-p
                      (or (loader:get-module env ,module-name)
                          (loader:get-module env "epsilon")))
               (loader:load-module env
                                   (if (loader:get-module env ,module-name)
                                       ,module-name
                                       "epsilon")))))))))

(defun extract-defpackage-require-specs (options)
  "Extract all :require specs from OPTIONS.
   Returns list of (package-name nickname) pairs.
   Handles multiple :require clauses by consolidating them."
  (let ((specs nil))
    (dolist (opt options)
      (when (and (consp opt)
                 (eq :require (first opt)))
        (dolist (spec (rest opt))
          (push (if (consp spec)
                    (list (first spec) (second spec))
                    (list spec nil))
                specs))))
    (nreverse specs)))

(defun extract-defpackage-enter (options)
  "Extract :enter option from OPTIONS. Returns t if (:enter t) present."
  (let ((enter-clause (find :enter options
                            :key (lambda (opt)
                                   (when (consp opt) (first opt))))))
    (and enter-clause (second enter-clause))))

(defun extract-defpackage-local-nicknames (options)
  "Extract existing :local-nicknames entries from OPTIONS."
  (let ((ln-clause (find :local-nicknames options
                         :key (lambda (opt)
                                (when (consp opt) (first opt))))))
    (when ln-clause
      (rest ln-clause))))

(defun filter-extended-options (options)
  "Remove :require and :enter options, leaving standard CL options."
  (remove-if (lambda (opt)
               (and (consp opt)
                    (member (first opt) '(:require :enter))))
             options))

(defun merge-local-nicknames (require-specs existing-nicknames)
  "Merge local nicknames from :require specs with existing :local-nicknames.
   Returns deduplicated list of (nickname package) pairs."
  (let ((from-require
          (mapcar (lambda (spec)
                    (let ((pkg-name (first spec))
                          (nickname (second spec)))
                      (when nickname
                        (list (if (keywordp nickname)
                                  nickname
                                  (intern (string-upcase (string nickname)) :keyword))
                              (if (keywordp pkg-name)
                                  pkg-name
                                  (intern (string-upcase (string pkg-name)) :keyword))))))
                  require-specs))
        (from-existing
          (mapcar (lambda (entry)
                    (list (first entry)
                          (if (keywordp (second entry))
                              (second entry)
                              (intern (string-upcase (string (second entry))) :keyword))))
                  existing-nicknames)))
    ;; Combine and deduplicate (require specs take precedence)
    (let ((result nil)
          (seen-nicknames nil))
      (dolist (entry (append (remove nil from-require) from-existing))
        (unless (member (first entry) seen-nicknames)
          (push (first entry) seen-nicknames)
          (push entry result)))
      (nreverse result))))

(defmacro defpackage (name &body options)
  "Extended defpackage with module auto-loading support.

   Accepts all standard CL defpackage options plus:
   - (:require (pkg nick) ...) - Load epsilon modules and create local nicknames
   - (:enter t) - Emit (in-package name) after package definition

   Example:
     (defpackage my-app
       (:use :cl)
       (:require (epsilon.http http)
                 (epsilon.json json))
       (:export main)
       (:enter t))"
  (let* ((require-specs (extract-defpackage-require-specs options))
         (enter-p (extract-defpackage-enter options))
         (standard-options (filter-extended-options options))
         ;; Generate module load forms for each require spec
         (module-loads
           (remove nil
                   (mapcar (lambda (spec)
                             (generate-module-load-form (first spec)))
                           require-specs))))
    ;; Only merge local nicknames if there are :require specs with nicknames
    ;; Otherwise pass through standard options unchanged to preserve original format
    (if (and require-specs (some #'second require-specs))
        ;; Has :require with nicknames - merge them with any existing :local-nicknames
        (let* ((existing-nicknames (extract-defpackage-local-nicknames options))
               (merged-nicknames (merge-local-nicknames require-specs existing-nicknames))
               (options-without-nicknames
                 (remove-if (lambda (opt)
                              (and (consp opt)
                                   (eq :local-nicknames (first opt))))
                            standard-options))
               (final-options
                 (if merged-nicknames
                     (append options-without-nicknames
                             (list (cons :local-nicknames merged-nicknames)))
                     options-without-nicknames)))
          `(progn
             ,@(when module-loads
                 `((eval-when (:compile-toplevel :load-toplevel :execute)
                     ,@module-loads)))
             (declaim (sb-ext:muffle-conditions sb-int:package-at-variance))
             (cl:defpackage ,name
               ,@final-options)
             (declaim (sb-ext:unmuffle-conditions sb-int:package-at-variance))
             ,@(when enter-p
                 `((in-package ,name)))))
        ;; No :require nicknames - pass through unchanged
        `(progn
           ,@(when module-loads
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   ,@module-loads)))
           (declaim (sb-ext:muffle-conditions sb-int:package-at-variance))
           (cl:defpackage ,name
             ,@standard-options)
           (declaim (sb-ext:unmuffle-conditions sb-int:package-at-variance))
           ,@(when enter-p
               `((in-package ,name)))))))

;;; Main Entry Point

(defun run (args)
  "Dispatch to the appropriate command handler."
  (let ((environment (setup-environment)))

    ;; Split args at "--" to separate epsilon args from passthrough args
    (multiple-value-bind (epsilon-args passthrough-args)
        (split-args-at-delimiter args)

      ;; Handle version flags early (no module discovery needed)
      (when (member "--version-json" epsilon-args :test #'string=)
        (format t "~A~%" (format-version-json))
        (sb-ext:exit :code 0))

      (when (member "--version" epsilon-args :test #'string=)
        (format t "~A~%" (format-version-table))
        (sb-ext:exit :code 0))

      ;; Extract global options
      (multiple-value-bind (remaining-args options)
          (extract-global-options epsilon-args)

        ;; --quiet also muffles SBCL compilation warnings
        (when (getf options :quiet)
          (map:assoc! (loader:environment-config environment) :warning-behavior :muffle))

        ;; Logging priority: CLI --log > EPSILON_LOG env var > defaults.
        ;; --quiet is folded into the log spec as "quiet" by extract-global-options.
        (let ((env-log (env:getenv "EPSILON_LOG")))
          (when env-log
            (log:configure-from-spec env-log)))

        (when (getf options :log)
          (log:configure-from-spec (getf options :log)))

        ;; Load project manifest if present
        (let ((project-dir (or (getf options :project)
                               (project:find-project-file (discovery:get-cwd)))))
          (when project-dir
            (handler-case
                (project:load-project environment project-dir)
              (error (e)
                (format *error-output* "Error loading project: ~A~%" e)
                (sb-ext:exit :code 1)))))

        ;; Discover commands declared by modules now in scope
        (commands:discover-module-commands environment)

        ;; Handle --help: global help if no subcommand, per-command help if subcommand
        (when (member "--help" remaining-args :test #'string=)
          (let ((first-arg (first remaining-args)))
            (if (and first-arg
                      (not (string= first-arg "--help"))
                      (commands:get-command first-arg))
                ;; Per-command help: "epsilon test --help"
                (commands:show-command-help first-arg)
                ;; Global help: "epsilon --help"
                (show-help)))
          (sb-ext:exit :code 0))

        ;; Check for subcommand
        (cond
          ;; Help for a specific command: "epsilon help <command>"
          ((and remaining-args
                (string= (first remaining-args) "help")
                (second remaining-args))
           (unless (commands:show-command-help (second remaining-args))
             (format *error-output* "Unknown command: ~A~%" (second remaining-args)))
           (sb-ext:exit :code 0))

          ;; Subcommand invocation
          ((and remaining-args
                (not (str:starts-with-p (first remaining-args) "-"))
                (commands:is-subcommand-invocation-p remaining-args))
           (let ((command-name (first remaining-args))
                 (command-args (rest remaining-args)))
             (commands:dispatch-command command-name environment command-args passthrough-args)
             (sb-ext:exit :code 0)))

          ;; No args or unrecognized - show help or start REPL
          ((null remaining-args)
           (start-interactive-repl))

          ;; Unknown command
          (t
           (format *error-output* "Unknown command: ~A~%" (first remaining-args))
           (format *error-output* "~%Run 'epsilon --help' for usage information.~%")
           (sb-ext:exit :code 1)))))))

(defun cli-run (&optional args)
  "Main entry point from epsilon script."
  (declare (ignore args))
  ;; Enable epsilon reader syntax at the top level
  (epsilon.reader:enable-epsilon-syntax)
  (let ((posix-args (rest sb-ext:*posix-argv*)))
    (when (string-equal "--" (car posix-args))
      (pop posix-args))
    (run posix-args)))
