;;;; The CLI entry point for Epsilon using a subcommand-based interface similar
;;;; to git
;;;;
;;;; Usage:
;;;;   epsilon <command> [options] [-- args]
(cl:defpackage epsilon.main
  (:use cl)
  (:shadow defpackage)
  (:local-nicknames (map epsilon.map)
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
  (:export cli-run defpackage package-name-to-module-name))

(in-package epsilon.main)

;;; Version Information
(defun get-version-info ()
  "Get version information as a map."
  (map:make-map "version"
                (env:version)
                "sbcl_version"
                (lisp-implementation-version)
                "os"
                (string-downcase (symbol-name (env:platform)))
                "architecture"
                (machine-type)
                "epsilon_home"
                (or (sb-ext:posix-getenv "EPSILON_HOME") "")
                "lisp_implementation"
                (lisp-implementation-type)))

(defun format-version-table ()
  "Format version information as a structured table string."
  (let ((info (get-version-info)))
    (format nil
            "~
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
    (format nil
            "{\"version\":\"~A\",\"sbcl_version\":\"~A\",\"os\":\"~A\",\"architecture\":\"~A\",\"epsilon_home\":\"~A\",\"lisp_implementation\":\"~A\"}"
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
        (format t
                "  ~28A ~A~%"
                (or (commands:command-usage-hint cmd) (commands:command-name cmd))
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
      (values (subseq args 0 delimiter-pos) (subseq args (1+ delimiter-pos)))
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
      (values (nreverse remaining) (list :quiet quiet :log combined-log :project project-path)))))

;;; Environment Setup
(defun setup-environment ()
  "Create and configure the build environment."
  (loader:environment))

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
      ((or (< num-parts 2) (not (string-equal "epsilon" (first parts))))
       nil)
      ((= num-parts 2)
       name-lower)
      (t
       (format nil "~A.~A" (first parts) (second parts))))))

;;; Extended defpackage Macro
;;;
;;; This macro extends CL:DEFPACKAGE with two new options:
;;;
;;;   :import - Load module dependencies, create local nicknames, and use packages
;;;   :enter  - Suppress implicit (in-package name) when set to nil
;;;
;;; All standard CL defpackage options pass through unchanged.
;;; The :use and :local-nicknames options are rejected; use :import instead.
;;; Enter defaults to T: the macro emits (in-package name) unless
;;; (:enter nil) is explicitly present.
;;;
;;; Import specs:
;;;   bare-symbol         - Use all exported symbols (like CL :use)
;;;   (package nickname)  - Create local nickname (like CL :local-nicknames)
;;;
;;; Example:
;;;   (defpackage my-module
;;;     (:import cl
;;;              (epsilon.http http)
;;;              (epsilon.crypto crypto))
;;;     (:export start stop))
(defun generate-module-load-form (pkg-name)
  "Generate code to load the module for PKG-NAME if not already loaded.
   Skips loading when the target module is already being built (intra-module
   imports during compilation)."
  (let ((module-name (package-name-to-module-name pkg-name))
        (pkg-name-str (string-upcase (string pkg-name))))
    (when module-name
      `(unless (or
                 (find-package ,pkg-name-str)
                 ;; Skip if we are currently building this module -- the
                 ;; package will be created when its file is compiled in
                 ;; the correct build order.
                 (and (boundp 'loader:*building-module*)
                      loader:*building-module*
                      (string-equal loader:*building-module* ,module-name)))
               (when (and (boundp 'loader:*environment*) loader:*environment*)
                     ;; Try exact module name, then :provides lookup, then "epsilon".
                     (let* ((by-name (loader:get-module ,module-name))
                            (by-provides (unless by-name
                                           (first (loader:query-modules :provides ,(string-downcase pkg-name-str)))))
                            (resolved (or by-name by-provides (loader:get-module "epsilon"))))
                       (when (and resolved (not (loader:module-loaded-p resolved)))
                         (loader:load-module (loader:module-name resolved)))))))))

(defun extract-defpackage-import-specs (options)
  "Extract all :import and :use specs from OPTIONS.
   Returns list of (package-name nickname) pairs.
   Bare symbols in :import and all :use packages get nickname NIL
   (meaning use all exports). Handles multiple clauses by consolidating."
  (let ((specs nil))
    (dolist (opt options)
      (when (consp opt)
        (case (first opt)
          (:import
           (dolist (spec (rest opt))
             (push (if (consp spec)
                     (list (first spec) (second spec))
                     (list spec nil))
                   specs)))
          (:use
           (dolist (pkg (rest opt)) (push (list pkg nil) specs))))))
    (nreverse specs)))

(defun extract-defpackage-enter (options)
  "Extract :enter option from OPTIONS.
   Returns T (default) unless (:enter nil) is explicitly present."
  (let ((enter-clause (find :enter options
                            :key (lambda (opt)
                              (when (consp opt)
                                (first opt))))))
    (if enter-clause
      (second enter-clause)
      t)))

(defun filter-extended-options (options)
  "Remove :import, :enter, and :use options, leaving standard CL options."
  (remove-if (lambda (opt)
               (and (consp opt) (member (first opt) '(:import :enter :use))))
             options))

(defmacro defpackage (name &body options)
  "Extended defpackage with module auto-loading support.

   Accepts all standard CL defpackage options plus:
   - (:import pkg (pkg nick) ...) - Load modules, use packages, create nicknames
   - (:enter nil) - Suppress implicit (in-package name) after definition

   Bare symbols in :import use all exported symbols from that package.
   Pairs (pkg nick) create local nicknames. The :use and :local-nicknames
   options are not accepted; use :import instead.

   Enter defaults to T: the macro emits (in-package name) unless (:enter nil)
   is explicitly present.

   Example:
     (defpackage my-app
       (:import cl
                (epsilon.http http)
                (epsilon.json json))
       (:export main))"
  ;; Reject :local-nicknames -- use :import instead
  (when (find :local-nicknames options
              :key (lambda (opt)
                (when (consp opt)
                  (first opt))))
    (error "Use (:import (package nickname) ...) instead of :local-nicknames ~
            in the extended defpackage for ~A"
           name))
  ;; :use is accepted for backwards compatibility but its packages are
  ;; folded into the generated :use clause alongside bare :import symbols.
  ;; Prefer bare symbols in (:import ...) for new code.
  (let* ((import-specs (extract-defpackage-import-specs options))
         (enter-p (extract-defpackage-enter options))
         (standard-options (filter-extended-options options))
         (module-loads (remove nil
                               (mapcar (lambda (spec)
                                         (generate-module-load-form (first spec)))
                                       import-specs)))
         ;; Bare symbols (no nickname) -> :use clause
         (use-packages (mapcar (lambda (spec)
                                 (let ((pkg-name (first spec)))
                                   (if (keywordp pkg-name)
                                     pkg-name
                                     (intern (string-upcase (string pkg-name)) :keyword))))
                               (remove-if #'second import-specs)))
         ;; Pairs with nicknames -> :local-nicknames clause
         (nicknames (remove nil
                            (mapcar (lambda (spec)
                                      (let ((pkg-name (first spec))
                                            (nickname (second spec)))
                                        (when nickname
                                          (list (if (keywordp nickname)
                                                  nickname
                                                  (intern (string-upcase (string nickname))
                                                          :keyword))
                                                (if (keywordp pkg-name)
                                                  pkg-name
                                                  (intern (string-upcase (string pkg-name))
                                                          :keyword))))))
                                    import-specs)))
         (final-options (append standard-options
                                (when use-packages
                                  (list (cons :use use-packages)))
                                (when nicknames
                                  (list (cons :local-nicknames nicknames))))))
    `(progn ,@(when module-loads
                    `((eval-when (:compile-toplevel :load-toplevel :execute) ,@module-loads)))
            (declaim (sb-ext:muffle-conditions sb-int:package-at-variance))
            (cl:defpackage ,name ,@final-options)
            (declaim (sb-ext:unmuffle-conditions sb-int:package-at-variance))
            ,@(when enter-p `((in-package ,name))))))

;;; Main Entry Point
(defun run (args)
  "Dispatch to the appropriate command handler."
  (handler-bind
      ;; Mirror the bootstrap muffle in epsilon.lisp. CLI arguments like
      ;; --module trigger load-module after the bootstrap's handler-bind
      ;; has unwound, so package-variance warnings (\"also exports\") and
      ;; redefinition-warnings would otherwise leak onto stderr at every
      ;; module load. Same predicate as the bootstrap so the suppression
      ;; rule lives in two visible places rather than diverging silently.
      ((sb-kernel:redefinition-warning #'muffle-warning)
       (warning (lambda (c)
                  (let ((msg (princ-to-string c)))
                    (when (or (search "also exports" msg)
                              (search "also shadows" msg))
                      (muffle-warning c))))))
    (%run-body args)))

(defun %run-body (args)
  (progn
    (setup-environment)
    ;; Split args at "--" to separate epsilon args from passthrough args
    (multiple-value-bind (epsilon-args passthrough-args) (split-args-at-delimiter args)
      ;; Handle version flags early (no module discovery needed)
      (when (member "--version-json" epsilon-args :test #'string=)
        (format t "~A~%" (format-version-json))
        (sb-ext:exit :code 0))
      (when (member "--version" epsilon-args :test #'string=)
        (format t "~A~%" (format-version-table))
        (sb-ext:exit :code 0))
      ;; Extract global options
      (multiple-value-bind (remaining-args options) (extract-global-options epsilon-args)
        ;; --quiet also muffles SBCL compilation warnings
        (when (getf options :quiet)
          (map:assoc! (loader:environment-config loader:*environment*) :warning-behavior :muffle))
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
            (handler-case (project:load-project project-dir)
              (error
               (e)
               (format *error-output* "Error loading project: ~A~%" e)
               (sb-ext:exit :code 1)))))
        ;; Pre-load ambient stdlib modules (epsilon.fs, epsilon.log, etc.) so
        ;; any module can import them without declaring them in module.sexp.
        ;; Must happen AFTER project discovery so contrib modules are registered.
        (loader:load-ambient-modules)
        ;; Discover commands declared by modules now in scope
        (commands:discover-module-commands)
        ;; Handle --help: global help if no subcommand, per-command help if subcommand
        (when (member "--help" remaining-args :test #'string=)
          (let ((first-arg (first remaining-args)))
            (if (and first-arg (not (string= first-arg "--help")) (commands:get-command first-arg))
              ;; Per-command help: "epsilon test --help"
              (commands:show-command-help first-arg)
              ;; Global help: "epsilon --help"
              (show-help)))
          (sb-ext:exit :code 0))
        ;; Check for subcommand
        (cond
          ;; Subcommand invocation (`help` itself is one of these and
          ;; routes through commands:dispatch-command -> the help core
          ;; command's handler, which knows how to drill into group
          ;; subcommands like `epsilon help score serve`).
          ((and remaining-args
                (not (str:starts-with-p (first remaining-args) "-"))
                (commands:is-subcommand-invocation-p remaining-args))
           (let ((command-name (first remaining-args))
                 (command-args (rest remaining-args)))
             (commands:dispatch-command command-name command-args passthrough-args)
             (sb-ext:exit :code 0)))
          ;; No args - show help (use 'epsilon repl' for interactive REPL)
          ((null remaining-args)
           (show-help)
           (sb-ext:exit :code 0))
          ;; Unknown command
          (t
           (format *error-output* "Unknown command: ~A~%" (first remaining-args))
           (format *error-output* "~%Run 'epsilon --help' for usage information.~%")
           (sb-ext:exit :code 1)))))))

(defun cli-run (&optional args)
  "Main entry point from epsilon script."
  (declare (ignore args))
  ;; Enable epsilon reader syntax at the top level
  (epsilon.readtable:enable-epsilon-syntax)
  (let ((posix-args (rest sb-ext:*posix-argv*)))
    (when (string-equal "--" (car posix-args))
      (pop posix-args))
    (run posix-args)))
