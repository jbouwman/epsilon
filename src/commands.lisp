;;;; Argument parsing, command registration and dispatch for
;;;; CLI. Declarative argument definitions, hierarchical subcommands,
;;;; automatic help generation, and module-contributed commands.
;;;;
;;;;   epsilon run module:function
;;;;   epsilon serve server --port 8080
;;;;   epsilon test json --verbose

(defpackage epsilon.commands
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (m epsilon.match)
   (loader epsilon.loader)
   (path epsilon.path)
   (fs epsilon.file)
   (log epsilon.log)
   (env epsilon.sys.env))
  (:export
   ;; Argument parsing
   make-parser
   add-argument
   add-command
   parse-args
   print-help
   print-usage
   parser-command
   parser-description
   parser-epilog
   parser-handler
   parser-commands
   argument
   make-argument
   argument-name
   argument-help
   argument-type
   argument-default
   argument-required
   argument-action
   argument-choices
   argument-metavar
   argument-nargs
   parsed-arguments
   parsed-command
   parsed-options
   parsed-positionals
   parsed-remaining
   parsed-subresult
   argument-error
   error-message
   error-parser
   unknown-argument-error
   missing-argument-error
   invalid-choice-error
   type-conversion-error

   ;; Command registration
   define-command
   register-command
   register-lazy-command
   get-command
   list-commands
   *commands*

   ;; Command struct accessors
   command-name
   command-description
   command-usage-hint
   command-usage
   command-options
   command-args
   command-examples
   command-core-p

   ;; Preamble pipeline
   parse-preamble-options
   execute-preamble

   ;; Command dispatch
   dispatch-command
   is-subcommand-invocation-p

   ;; Module command discovery
   discover-module-commands

   ;; Command handlers (referenced by module.plist :commands :handler)
   cmd-modules

   ;; Utilities for contributed commands
   resolve-module-name
   detect-changed-modules
   parse-keyword-args
   parse-declared-args
   ensure-location-string

   ;; Per-command help
   show-command-help))

(in-package epsilon.commands)

;;; ---------------------------------------------------------------------------
;;; Argument Parsing Conditions
;;; ---------------------------------------------------------------------------

(define-condition argument-error (error)
  ((message :initarg :message :reader error-message)
   (parser :initarg :parser :reader error-parser :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A" (error-message condition)))))

(define-condition unknown-argument-error (argument-error)
  ((argument :initarg :argument :reader error-argument)))

(define-condition missing-argument-error (argument-error)
  ((argument :initarg :argument :reader error-argument)))

(define-condition invalid-choice-error (argument-error)
  ((value :initarg :value :reader error-value)
   (choices :initarg :choices :reader error-choices)))

(define-condition type-conversion-error (argument-error)
  ((value :initarg :value :reader error-value)
   (target-type :initarg :target-type :reader error-target-type)))

;;; ---------------------------------------------------------------------------
;;; Argument Parsing Data Structures
;;; ---------------------------------------------------------------------------

(defclass argument ()
  ((name :initarg :name :reader argument-name
         :documentation "Argument name (e.g., '--verbose' or 'filename')")
   (help :initarg :help :reader argument-help :initform ""
         :documentation "Help text for this argument")
   (type :initarg :type :reader argument-type :initform 'string
         :documentation "Type to convert to: 'string, 'integer, 'boolean, or a function")
   (default :initarg :default :reader argument-default :initform nil
            :documentation "Default value if not provided")
   (required :initarg :required :reader argument-required :initform nil
             :documentation "Whether this argument is required")
   (action :initarg :action :reader argument-action :initform 'store
           :documentation "Action to take: 'store, 'store-true, 'store-false, 'append")
   (choices :initarg :choices :reader argument-choices :initform nil
            :documentation "List of valid choices")
   (metavar :initarg :metavar :reader argument-metavar :initform nil
            :documentation "Name to use in help messages")
   (nargs :initarg :nargs :reader argument-nargs :initform nil
          :documentation "Number of values: nil, '+, '*, '?, or an integer")
   (dest :initarg :dest :reader argument-dest :initform nil
         :documentation "Destination key for storing the value")))

(defun make-argument (name &rest args)
  "Create an argument object"
  (apply #'make-instance 'argument :name name args))

(defun argument-is-option-p (arg)
  "Check if argument is an option (starts with -)"
  (str:starts-with-p (argument-name arg) "-"))

(defun argument-key (arg)
  "Get the key for storing this argument's value"
  (or (argument-dest arg)
      (if (argument-is-option-p arg)
          (let* ((name (argument-name arg))
                 (trimmed (string-trim "-" name)))
            (with-output-to-string (s)
              (loop for char across trimmed
                    do (write-char (if (char= char #\-) #\_ char) s))))
          (argument-name arg))))

(defclass parser ()
  ((command :initarg :command :accessor parser-command :initform nil)
   (description :initarg :description :accessor parser-description :initform "")
   (epilog :initarg :epilog :accessor parser-epilog :initform "")
   (arguments :initform '() :accessor parser-arguments)
   (commands :initform (map:make-map) :accessor parser-commands)
   (parent :initarg :parent :accessor parser-parent :initform nil)
   (handler :initarg :handler :accessor parser-handler :initform nil)))

(defun make-parser (&key command description epilog handler)
  "Create a new argument parser"
  (make-instance 'parser :command command :description description :epilog epilog :handler handler))

(defun add-argument (parser name &rest args)
  "Add an argument to the parser"
  (let* ((action (getf args :action 'store))
         (action-keyword (intern (string action) :keyword))
         (args (if (and (member action-keyword '(:store-true :store-false))
                        (not (member :default args)))
                   (append args (list :default (if (eq action-keyword :store-true) nil t)))
                   args))
         (argument (apply #'make-argument name args)))
    (push argument (parser-arguments parser))
    argument))

(defun add-command (parser name &key description help handler)
  "Add a subcommand to the parser"
  (let ((subparser (make-parser :command name
                                :description (or description help)
                                :handler handler)))
    (setf (parser-parent subparser) parser)
    (setf (parser-commands parser)
          (map:assoc (parser-commands parser) name subparser))
    subparser))

;;; ---------------------------------------------------------------------------
;;; Parsing Results
;;; ---------------------------------------------------------------------------

(defclass parsed-arguments ()
  ((command :initarg :command :accessor parsed-command :initform nil)
   (options :initarg :options :accessor parsed-options :initform (map:make-map))
   (positionals :initarg :positionals :accessor parsed-positionals :initform '())
   (remaining :initarg :remaining :accessor parsed-remaining :initform '())
   (subresult :initarg :subresult :accessor parsed-subresult :initform nil)))

;;; ---------------------------------------------------------------------------
;;; Type Conversion
;;; ---------------------------------------------------------------------------

(defun convert-value (value type)
  "Convert a string value to the specified type"
  (handler-case
      (m:match type
        ('string value)
        ('integer (parse-integer value))
        ('boolean (if (member value '("true" "yes" "1" "on") :test #'string-equal) t nil))
        ((? functionp f) (funcall f value))
        (_ value))
    (error (e)
      (error 'type-conversion-error
             :message (format nil "Cannot convert '~A' to ~A: ~A" value type e)
             :value value
             :target-type type))))

;;; ---------------------------------------------------------------------------
;;; Argument Parsing Implementation
;;; ---------------------------------------------------------------------------

(defun parse-args (parser args)
  "Parse command line arguments"
  (let ((result (make-instance 'parsed-arguments))
        (remaining args))
    (multiple-value-bind (pre-command post-command command-name)
        (split-at-command parser remaining)
      (setf remaining (parse-arguments parser pre-command result))
      (when command-name
        (let ((subparser (map:get (parser-commands parser) command-name)))
          (if subparser
              (progn
                (setf (parsed-command result) command-name)
                (let ((sub-result (parse-args subparser post-command)))
                  (setf (parsed-subresult result) sub-result)
                  (setf (parsed-remaining result)
                        (parsed-remaining sub-result))))
              (push command-name (parsed-positionals result))))))
    (validate-required parser result)
    (validate-no-unknown-options parser result)
    (apply-defaults parser result)
    (setf (parsed-positionals result) (nreverse (parsed-positionals result)))
    result))

(defun split-at-command (parser args)
  "Split args at the first recognized command"
  (let ((commands (map:keys (parser-commands parser))))
    (loop for tail on args
          for i from 0
          when (member (first tail) commands :test #'string=)
          return (values (subseq args 0 i)
                        (rest tail)
                        (first tail))
          finally (return (values args nil nil)))))

(defun parse-arguments (parser args result)
  "Parse a list of arguments, updating result"
  (let ((remaining '())
        (positional-args (remove-if #'argument-is-option-p
                                   (parser-arguments parser)))
        (positional-index 0))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "--")
                (setf remaining (append remaining args))
                (return))
               ((str:starts-with-p arg "-")
                (multiple-value-bind (consumed-args option-handled-p)
                    (parse-option parser arg args result)
                  (if option-handled-p
                      (dotimes (i consumed-args)
                        (pop args))
                      (push arg remaining))))
               (t
                (if (< positional-index (length positional-args))
                    (let* ((pos-arg (nth positional-index positional-args))
                           (nargs (argument-nargs pos-arg)))
                      (cond
                        ((or (eq nargs '*) (eq nargs '+))
                         (let ((values (list arg)))
                           (loop while (and args
                                           (not (str:starts-with-p (first args) "-")))
                                 do (push (pop args) values))
                           (setf values (nreverse values))
                           (dolist (val values)
                             (push val (parsed-positionals result)))
                           (setf (parsed-options result)
                                 (map:assoc (parsed-options result)
                                           (argument-key pos-arg)
                                           values))
                           (incf positional-index)))
                        (t
                         (let ((converted (convert-value arg (argument-type pos-arg))))
                           (push arg (parsed-positionals result))
                           (setf (parsed-options result)
                                 (map:assoc (parsed-options result)
                                           (argument-key pos-arg)
                                           converted))
                           (incf positional-index)))))
                    (push arg remaining)))))
    (setf (parsed-remaining result) (nreverse remaining))
    remaining))

(defun parse-option (parser option args result)
  "Parse an option argument. Returns (values consumed-args handled-p)"
  (let* ((equals-pos (position #\= option))
         (option-name (if equals-pos
                         (subseq option 0 equals-pos)
                         option))
         (option-value (when equals-pos
                        (subseq option (1+ equals-pos))))
         (argument (find-argument parser option-name)))
    (unless argument
      (return-from parse-option (values 0 nil)))
    (let ((key (argument-key argument))
          (action (argument-action argument)))
      (case (intern (string action) :keyword)
        (:store-true
         (setf (parsed-options result) (map:assoc (parsed-options result) key t))
         (values 0 t))
        (:store-false
         (setf (parsed-options result) (map:assoc (parsed-options result) key nil))
         (values 0 t))
        (:store
         (let* ((nargs (argument-nargs argument))
                (has-next-arg (and args (not (str:starts-with-p (first args) "-"))))
                (value (or option-value
                          (when has-next-arg (first args)))))
           (cond
             ((eq (intern (string nargs) :keyword) :?)
              (let ((actual-value (or value "")))
                (let ((converted (convert-value actual-value (argument-type argument))))
                  (setf (parsed-options result)
                        (map:assoc (parsed-options result) key converted)))
                (values (if (and has-next-arg value) 1 0) t)))
             (t
              (unless value
                (error 'missing-argument-error
                       :message (format nil "Option ~A requires an argument" option-name)
                       :argument argument))
              (when (argument-choices argument)
                (unless (member value (argument-choices argument) :test #'string=)
                  (error 'invalid-choice-error
                         :message (format nil "Invalid choice '~A' for ~A (choose from ~{~A~^, ~})"
                                        value option-name (argument-choices argument))
                         :value value
                         :choices (argument-choices argument))))
              (let ((converted (convert-value value (argument-type argument))))
                (setf (parsed-options result)
                      (map:assoc (parsed-options result) key converted)))
              (values (if option-value 0 1) t)))))
        (:append
         (let ((value (or option-value
                         (when (and args (not (str:starts-with-p (first args) "-")))
                           (first args)))))
           (unless value
             (error 'missing-argument-error
                    :message (format nil "Option ~A requires an argument" option-name)
                    :argument argument))
           (let ((converted (convert-value value (argument-type argument)))
                 (current (map:get (parsed-options result) key '())))
             (setf (parsed-options result)
                   (map:assoc (parsed-options result) key
                             (append current (list converted)))))
           (values (if option-value 0 1) t)))
        (otherwise
         (values 0 nil))))))

(defun find-argument (parser name)
  "Find an argument by name"
  (find name (parser-arguments parser)
        :key #'argument-name
        :test #'string=))

(defun validate-required (parser result)
  "Validate that all required arguments are present"
  (dolist (arg (parser-arguments parser))
    (when (and (argument-required arg)
               (not (map:contains-p (parsed-options result) (argument-key arg))))
      (error 'missing-argument-error
             :message (format nil "Required argument ~A not provided"
                            (argument-name arg))
             :argument arg))))

(defun validate-no-unknown-options (parser result)
  "Check for unknown options in remaining arguments"
  (dolist (arg (parsed-remaining result))
    (when (str:starts-with-p arg "-")
      (error 'unknown-argument-error
             :message (format nil "Unknown option: ~A" arg)
             :argument arg
             :parser parser))))

(defun apply-defaults (parser result)
  "Apply default values for missing arguments"
  (dolist (arg (parser-arguments parser))
    (let ((key (argument-key arg)))
      (unless (map:contains-p (parsed-options result) key)
        (let ((default-value (argument-default arg))
              (action-keyword (intern (string (argument-action arg)) :keyword)))
          (when (and default-value
                     (not (and (eq action-keyword :store-true)
                               (null default-value))))
            (setf (parsed-options result)
                  (map:assoc (parsed-options result) key default-value))))))))

;;; ---------------------------------------------------------------------------
;;; Argument Help Generation
;;; ---------------------------------------------------------------------------

(defun print-help (parser &optional (stream *standard-output*))
  "Print help message for parser"
  (print-usage parser stream)
  (terpri stream)
  (when (parser-description parser)
    (format stream "~A~%~%" (parser-description parser)))
  (let ((positionals (remove-if #'argument-is-option-p (parser-arguments parser)))
        (options (remove-if-not #'argument-is-option-p (parser-arguments parser))))
    (when positionals
      (format stream "Arguments:~%")
      (dolist (arg (reverse positionals))
        (print-argument-help arg stream))
      (terpri stream))
    (when options
      (format stream "Options:~%")
      (dolist (arg (reverse options))
        (print-argument-help arg stream))
      (terpri stream))
    (when (plusp (map:count (parser-commands parser)))
      (format stream "Commands:~%")
      (map:each (parser-commands parser)
                (lambda (name subparser)
                  (format stream "  ~A~:[~;~:*~48T~A~]~%"
                          name (parser-description subparser))))
      (terpri stream)))
  (when (parser-epilog parser)
    (format stream "~A~%" (parser-epilog parser))))

(defun print-usage (parser &optional (stream *standard-output*))
  "Print usage line for parser"
  (let ((chain '()))
    (loop for p = parser then (parser-parent p)
          while p
          do (push (parser-command p) chain))
    (cond
      ((null chain)
       (format stream "Usage: command"))
      ((= (length chain) 1)
       (let ((cmd (first chain)))
         (if (string= cmd "epsilon")
             (format stream "Usage: epsilon")
             (format stream "Usage: epsilon ~A" cmd))))
      (t
       (let ((cmd-list (cons "epsilon" chain)))
         (format stream "Usage: ~{~A~^ ~}" cmd-list)))))
  (when (some #'argument-is-option-p (parser-arguments parser))
    (format stream " [options]"))
  (when (plusp (map:count (parser-commands parser)))
    (format stream " <command>"))
  (let ((positionals (remove-if #'argument-is-option-p (parser-arguments parser))))
    (dolist (arg (reverse positionals))
      (format stream " ~:[~A~;<~A>~]"
              (argument-required arg)
              (or (argument-metavar arg) (argument-name arg)))))
  (format stream " [arguments]"))

(defun print-argument-help (arg stream)
  "Print help for a single argument"
  (let ((names (argument-name arg))
        (metavar (or (argument-metavar arg)
                    (string-upcase (argument-key arg)))))
    (format stream "  ~A" names)
    (when (and (argument-is-option-p arg)
               (let ((action-keyword (intern (string (argument-action arg)) :keyword)))
                 (not (member action-keyword '(:store-true :store-false)))))
      (format stream " ~A" metavar))
    (when (argument-help arg)
      (format stream "~48T~A" (argument-help arg)))
    (let ((info '()))
      (when (argument-choices arg)
        (push (format nil "choices: ~{~A~^, ~}" (argument-choices arg)) info))
      (when (argument-default arg)
        (push (format nil "default: ~A" (argument-default arg)) info))
      (when info
        (format stream " (~{~A~^; ~})" (nreverse info))))
    (terpri stream)))

;;; ---------------------------------------------------------------------------
;;; Command Registry
;;; ---------------------------------------------------------------------------

(defvar *commands* (map:make-map)
  "Registry of available commands. Maps command name strings to command structs.")

(defstruct command
  "A CLI command definition."
  (name "" :type string)
  (description "" :type string)
  (handler nil :type (or null function))
  (parser nil)
  (aliases nil :type list)
  ;; Metadata for help generation
  (usage-hint nil :type (or null string))   ; e.g., "test [modules]" for help column
  (usage nil :type (or null string))        ; full usage string for per-command help
  (options nil :type list)                  ; list of option plists (legacy help-only)
  (args nil :type list)                     ; list of declared arg specs for parsing
  (examples nil :type list)                 ; list of (invocation description) pairs
  (module-name nil :type (or null string))  ; nil for core, module name for contributed
  (core-p nil :type boolean))               ; true for always-available commands

(defun register-command (name &key description handler parser aliases
                                   usage-hint usage options args examples
                                   module-name core-p)
  "Register a command in the global registry."
  (let ((cmd (make-command :name name
                           :description description
                           :handler handler
                           :parser parser
                           :aliases aliases
                           :usage-hint usage-hint
                           :usage usage
                           :options options
                           :args args
                           :examples examples
                           :module-name module-name
                           :core-p core-p)))
    (map:assoc! *commands* name cmd)
    ;; Also register aliases
    (dolist (alias aliases)
      (map:assoc! *commands* alias cmd))
    cmd))

(defun register-lazy-command (name module-name handler-spec
                              &key description usage-hint usage
                                   options args examples aliases)
  "Register a command whose handler is loaded on first invocation.
   HANDLER-SPEC is a string like \"epsilon.test.cli:run\" specifying
   the package:function to resolve when the command is invoked."
  (register-command name
    :description (or description "")
    :handler (make-lazy-handler name module-name handler-spec args)
    :aliases aliases
    :usage-hint usage-hint
    :usage usage
    :options options
    :args args
    :examples examples
    :module-name module-name
    :core-p nil))

(defun make-lazy-handler (command-name module-name handler-spec args-spec)
  "Return a closure that loads MODULE-NAME and resolves HANDLER-SPEC on first call.
   HANDLER-SPEC is \"package:function\".
   When ARGS-SPEC is non-nil, parse raw args against the declaration and call
   the handler with keyword arguments instead of (environment args passthrough-args)."
  (let ((resolved-fn nil))
    (lambda (environment args passthrough-args)
      (unless resolved-fn
        ;; Load the module
        (handler-case
            (loader:load-module environment module-name)
          (error (e)
            (format *error-output* "Error loading module ~A for command ~A: ~A~%"
                    module-name command-name e)
            (sb-ext:exit :code 1)))
        ;; Resolve the handler function
        (let* ((colon-pos (position #\: handler-spec))
               (pkg-name (subseq handler-spec 0 colon-pos))
               (fn-name (subseq handler-spec (1+ colon-pos)))
               (pkg (find-package (string-upcase pkg-name))))
          (unless pkg
            (format *error-output* "Error: Package ~A not found for command ~A~%"
                    pkg-name command-name)
            (sb-ext:exit :code 1))
          (let ((sym (find-symbol (string-upcase fn-name) pkg)))
            (unless (and sym (fboundp sym))
              (format *error-output* "Error: Function ~A:~A not found for command ~A~%"
                      pkg-name fn-name command-name)
              (sb-ext:exit :code 1))
            (setf resolved-fn (fdefinition sym)))))
      (if args-spec
          ;; Declarative arg parsing: merge args and passthrough-args
          (let* ((all-args (append args passthrough-args))
                 (parsed (parse-declared-args args-spec all-args)))
            (if (eq parsed :help)
                (show-command-help command-name)
                (apply resolved-fn parsed)))
          ;; Legacy 3-arg handler
          (funcall resolved-fn environment args passthrough-args)))))

(defun get-command (name)
  "Look up a command by name or alias."
  (map:get *commands* name))

(defun list-commands (&key core-only contributed-only)
  "Return a sorted list of command structs (excluding aliases).
   When CORE-ONLY, return only core commands.
   When CONTRIBUTED-ONLY, return only contributed commands."
  (let ((commands '()))
    (map:each *commands*
              (lambda (name cmd)
                (when (and (string= name (command-name cmd))
                           (or (not core-only) (command-core-p cmd))
                           (or (not contributed-only) (not (command-core-p cmd))))
                  (push cmd commands))))
    (sort commands #'string< :key #'command-name)))

(defmacro define-command (name (&rest args) &body body)
  "Define a command handler function and register it.

   Usage:
     (define-command run (environment args passthrough-args)
       \"Run a module function.\"
       (:metadata :core-p t :usage-hint \"run <module>[:<fn>]\")
       (body...))

   The optional (:metadata ...) form after the docstring provides
   help metadata: :usage-hint, :usage, :options, :examples.
  "
  (let* ((fn-name (intern (format nil "CMD-~A" (string-upcase name))))
         (description (if (stringp (first body)) (first body) ""))
         (after-doc (if (stringp (first body)) (rest body) body))
         ;; Check for (:metadata ...) form
         (metadata (when (and (consp (first after-doc))
                              (eq :metadata (caar after-doc)))
                     (cdar after-doc)))
         (real-body (if metadata (rest after-doc) after-doc)))
    `(progn
       (defun ,fn-name ,args
         ,description
         ,@real-body)
       (register-command ,(string-downcase (string name))
                         :description ,description
                         :handler #',fn-name
                         ,@(when metadata
                             (loop for (k v) on metadata by #'cddr
                                   collect k collect `',v))))))

;;; Command Detection

(defun is-subcommand-invocation-p (args)
  "Check if args use the new subcommand style rather than flag style.
   Returns T if the first argument matches a registered command."
  (and args
       (not (str:starts-with-p (first args) "-"))
       (get-command (first args))))

;;; Per-Command Help

(defun show-command-help (command-name)
  "Display detailed help for a specific command.
   Returns T if the command was found, NIL otherwise."
  (let ((cmd (get-command command-name)))
    (unless cmd (return-from show-command-help nil))
    (format t "~%~A~%" (command-description cmd))
    (format t "~%Usage: ~A~%"
            (or (command-usage cmd)
                (format nil "epsilon ~A" command-name)))
    ;; Prefer :args (declarative) over :options (legacy help-only)
    (let ((args-spec (command-args cmd)))
      (if args-spec
          (show-declared-args-help-section args-spec)
          (let ((options (command-options cmd)))
            (when options
              (format t "~%Options:~%")
              (dolist (opt options)
                (let ((name (getf opt :name))
                      (arg (getf opt :arg))
                      (desc (getf opt :description)))
                  (if arg
                      (format t "  ~A ~A~30T~A~%" name arg desc)
                      (format t "  ~A~30T~A~%" name desc))))))))
    (let ((examples (command-examples cmd)))
      (when examples
        (format t "~%Examples:~%")
        (dolist (ex examples)
          (format t "  ~A~%" (first ex))
          (when (second ex)
            (format t "      ~A~%" (second ex))))))
    t))

(defun show-declared-args-help-section (args-spec)
  "Print the Options section for declared :args specs."
  (when args-spec
    (format t "~%Options:~%")
    (dolist (spec args-spec)
      (let* ((flag (getf spec :flag))
             (type (or (getf spec :type) :string))
             (default (getf spec :default))
             (desc (or (getf spec :description) ""))
             (required (getf spec :required))
             (type-hint (unless (eq type :boolean)
                          (string-upcase (getf spec :name)))))
        (if type-hint
            (format t "  ~A ~A~30T~A" flag type-hint desc)
            (format t "  ~A~30T~A" flag desc))
        (when default
          (format t " (default: ~A)" default))
        (when required
          (format t " [required]"))
        (format t "~%")))
    (format t "  --help~30TShow this help~%")))

(defun show-declared-args-help (command-name args-spec)
  "Display help for a command using its declared :args spec."
  (format t "~%Usage: epsilon ~A [options]~%" command-name)
  (show-declared-args-help-section args-spec))

;;; Module Command Discovery

(defun discover-module-commands (environment)
  "Scan all registered modules for :commands declarations and register
   them as lazy commands. Called during environment setup."
  (dolist (mod (loader:query-modules environment))
    (let* ((metadata (loader:module-metadata mod))
           (commands (getf metadata :commands))
           (module-name (loader:module-name mod)))
      (when commands
        (dolist (cmd-spec commands)
          (let ((name (getf cmd-spec :name))
                (description (getf cmd-spec :description))
                (handler (getf cmd-spec :handler))
                (usage-hint (getf cmd-spec :usage-hint))
                (usage (getf cmd-spec :usage))
                (options (getf cmd-spec :options))
                (args (getf cmd-spec :args))
                (examples (getf cmd-spec :examples))
                (aliases (getf cmd-spec :aliases)))
            (when (and name handler)
              (let ((existing (get-command name)))
                (cond
                  (existing
                   ;; Already registered (bootstrap command or earlier module).
                   ;; Commands are disambiguated by package, so duplicates
                   ;; are harmless -- just keep the first registration.
                   nil)
                  (t
                   (register-lazy-command name module-name handler
                     :description description
                     :usage-hint usage-hint
                     :usage usage
                     :options options
                     :args args
                     :examples examples
                     :aliases aliases)))))))))))

;;; Module Name Resolution

(defun resolve-module-name (short-name environment)
  "Resolve a short module name to a full qualified name.

   Resolution rules:
   1. If name starts with 'epsilon.', use as-is
   2. Try exact name as-is (e.g., 'epsilon' for core module)
   3. Try 'epsilon.<name>' in loaded workspaces
   4. Return nil if not found

   Returns the full module name or nil."
  (cond
    ;; Already fully qualified
    ((str:starts-with-p short-name "epsilon.")
     (when (loader:get-module environment short-name)
       short-name))
    ;; Try exact name as-is (handles "epsilon" core module)
    ((loader:get-module environment short-name)
     short-name)
    ;; Try with epsilon. prefix
    (t
     (let ((full-name (format nil "epsilon.~A" short-name)))
       (when (loader:get-module environment full-name)
         full-name)))))

(defun module-default-entry-point (module-name environment)
  "Return the default entry point function name for MODULE-NAME.
   Checks the :main field in module.plist metadata first, then
   falls back to \"main\"."
  (let* ((mod (loader:get-module environment module-name))
         (metadata (when mod (loader:module-metadata mod)))
         (main-spec (when metadata (getf metadata :main))))
    (cond
      ;; :main "package:function" -> extract function name
      ((and main-spec (stringp main-spec) (position #\: main-spec))
       (subseq main-spec (1+ (position #\: main-spec))))
      ;; :main "function" -> use directly
      ((and main-spec (stringp main-spec))
       main-spec)
      ;; No :main -> default to "main"
      (t "main"))))

(defun parse-module-spec (spec)
  "Parse a module:function specification.
   Returns (values module-name function-name).

   Examples:
     'server' -> (values 'server' nil)
     'server:start' -> (values 'server' 'start')
     'http.client:get' -> (values 'http.client' 'get')"
  (let ((colon-pos (position #\: spec)))
    (if colon-pos
        (values (subseq spec 0 colon-pos)
                (subseq spec (1+ colon-pos)))
        (values spec nil))))

(defun suggest-modules (partial-name environment)
  "Return list of modules matching partial name for suggestions."
  (let ((pattern (format nil "epsilon.~A" partial-name))
        (matches '()))
    (dolist (module (loader:query-modules environment))
      (let ((name (loader:module-name module)))
        (when (or (str:starts-with-p name pattern)
                  (search partial-name name :test #'char-equal))
          (push name matches))))
    (sort matches #'string<)))

;;; Preamble Pipeline
;;;
;;; Composable preamble options (--package/-p, --load/-l, --eval/-e) that can
;;; precede any command invocation.  Directives are collected in order and
;;; executed left-to-right before the target function is called.

(defun parse-preamble-options (args)
  "Parse preamble options from ARGS.
   Recognizes --package/-p, --load/-l, --eval/-e with repeatable semantics.
   Returns (values preamble-directives remaining-args).
   Preamble directives are an ordered list of (:package name), (:load path),
   or (:eval expr)."
  (let ((preamble '())
        (remaining '()))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; --package / -p
               ((and (or (string= arg "--package") (string= arg "-p"))
                     (< (1+ i) (length args)))
                (push (list :package (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --load / -l
               ((and (or (string= arg "--load") (string= arg "-l"))
                     (< (1+ i) (length args)))
                (push (list :load (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --eval / -e
               ((and (or (string= arg "--eval") (string= arg "-e"))
                     (< (1+ i) (length args)))
                (push (list :eval (nth (1+ i) args)) preamble)
                (incf i 2))
               (t
                (push arg remaining)
                (incf i))))
    (values (nreverse preamble) (nreverse remaining))))

(defun execute-preamble (preamble environment)
  "Execute preamble directives in order.
   Each directive is one of:
     (:package module-name) - resolve and load a module
     (:load file-path)      - load a Lisp source file
     (:eval expression)     - evaluate a Lisp expression string
   Returns the result of the last directive, or NIL if PREAMBLE is empty."
  (let ((last-result nil))
    (dolist (directive preamble last-result)
      (ecase (first directive)
        (:package
         (let* ((name (second directive))
                (resolved (resolve-module-name name environment)))
           (unless resolved
             (format *error-output* "Error: Module '~A' not found~%" name)
             (let ((suggestions (suggest-modules name environment)))
               (when suggestions
                 (format *error-output* "Did you mean:~%")
                 (dolist (s (subseq suggestions 0 (min 5 (length suggestions))))
                   (format *error-output* "  ~A~%" s))))
             (sb-ext:exit :code 1))
           (log:info "Loading module: ~A" resolved)
           (loader:load-module environment resolved)
           (setf last-result t)))
        (:load
         (let* ((file (second directive))
                (user-cwd (or (env:getenv "EPSILON_CWD")
                              (namestring (truename "."))))
                (resolved-file (if (fs:is-absolute file)
                                   file
                                   (fs:join-paths user-cwd file))))
           (unless (probe-file resolved-file)
             (format *error-output* "Error: File not found: ~A~%" resolved-file)
             (sb-ext:exit :code 1))
           (log:info "Loading file: ~A" resolved-file)
           (handler-case
               (progn
                 (load resolved-file :verbose nil :print nil)
                 (setf last-result t))
             (error (e)
               (format *error-output* "Error loading file: ~A~%" e)
               (sb-ext:exit :code 1)))))
        (:eval
         (let ((expr (second directive)))
           (handler-case
               (let ((form (read-from-string expr)))
                 (setf last-result (eval form)))
             (error (e)
               (format *error-output* "Error evaluating expression: ~A~%" e)
               (sb-ext:exit :code 1)))))))))

;;; Command Dispatch

(defun dispatch-command (command-name environment args passthrough-args)
  "Dispatch to a registered command handler.
   For contributed (non-core) commands, preamble options are extracted and
   executed before the handler is called.  Core commands (run, eval, load)
   handle their own preamble parsing internally.
   Returns T if command was handled, NIL otherwise."
  (let ((cmd (get-command command-name)))
    (unless cmd (return-from dispatch-command nil))
    (if (command-core-p cmd)
        ;; Core commands handle their own preamble
        (funcall (command-handler cmd) environment args passthrough-args)
        ;; Contributed commands: extract and execute preamble, pass remainder
        (multiple-value-bind (preamble remaining-args)
            (parse-preamble-options args)
          (when preamble
            (execute-preamble preamble environment))
          (funcall (command-handler cmd) environment remaining-args passthrough-args)))
    t))

;;; Built-in Commands

(define-command run (environment args passthrough-args)
  "Run a module function."
  (:metadata
   :core-p t
   :usage-hint "run [options] [module][:<fn>]"
   :usage "epsilon run [PREAMBLE...] [module][:<function>] [-- args]"
   :options ((:name "--package, -p" :arg "MODULE" :description "Pre-load module (repeatable)")
             (:name "--load, -l" :arg "FILE" :description "Load a Lisp file (repeatable)")
             (:name "--eval, -e" :arg "EXPR" :description "Evaluate expression (repeatable)"))
   :examples (("epsilon run server" "Call epsilon.server:main")
              ("epsilon run server:start" "Call epsilon.server:start")
              ("epsilon run -p json -e '(json:parse \"{}\")'" "Evaluate with module loaded")
              ("epsilon run -p json -l setup.lisp mymod:process" "Load module, file, then run")))
  (multiple-value-bind (preamble remaining-args)
      (parse-preamble-options args)
    (let ((spec nil))
      ;; Extract target spec from remaining args
      (dolist (arg remaining-args)
        (when (not (str:starts-with-p arg "-"))
          (setf spec arg)
          (return)))

      ;; Execute preamble directives
      (when preamble
        (let ((result (execute-preamble preamble environment)))
          ;; If no target, print result and exit
          (when (null spec)
            (unless (null result)
              (format t "~A~%" result))
            (return-from cmd-run))))

      ;; If no preamble and no target, show usage
      (when (null spec)
        (show-command-help "run")
        (sb-ext:exit :code 1))

      (multiple-value-bind (module-short explicit-fn)
          (parse-module-spec spec)
        (let ((module-name (resolve-module-name module-short environment)))
          (unless module-name
            (let ((suggestions (suggest-modules module-short environment)))
              (format *error-output* "Error: Module '~A' not found.~%" module-short)
              (when suggestions
                (format *error-output* "Did you mean:~%")
                (dolist (s (subseq suggestions 0 (min 5 (length suggestions))))
                  (format *error-output* "  ~A~%" s))))
            (sb-ext:exit :code 1))

          ;; Look up :commands handler from module metadata if a command name
          ;; was given explicitly (e.g. epsilon run epsilon.score:serve).
          ;; The handler spec in module.plist may point to a different package
          ;; than the module's own package.
          (let ((handler-spec nil)
                (cmd-args-spec nil))
            (when explicit-fn
              (let* ((mod (loader:get-module environment module-name))
                     (metadata (when mod (loader:module-metadata mod)))
                     (commands (when metadata (getf metadata :commands))))
                (when commands
                  (let ((cmd-entry (find explicit-fn commands
                                        :key (lambda (c) (getf c :name))
                                        :test #'string-equal)))
                    (when cmd-entry
                      (setf handler-spec (getf cmd-entry :handler))
                      (setf cmd-args-spec (getf cmd-entry :args)))))))
            ;; Also check for :args on the default command when no explicit-fn
            (unless (or explicit-fn handler-spec)
              (let* ((mod (loader:get-module environment module-name))
                     (metadata (when mod (loader:module-metadata mod)))
                     (commands (when metadata (getf metadata :commands)))
                     (main-name (module-default-entry-point module-name environment)))
                (when commands
                  (let ((cmd-entry (find main-name commands
                                        :key (lambda (c) (getf c :name))
                                        :test #'string-equal)))
                    (when cmd-entry
                      (setf handler-spec (getf cmd-entry :handler))
                      (setf cmd-args-spec (getf cmd-entry :args)))))))

            ;; Load the module
            (log:info "Loading module: ~A" module-name)
            (loader:load-module environment module-name)

            (if handler-spec
                ;; Resolve from the handler spec (e.g. "epsilon.score.server:serve")
                (let* ((colon-pos (position #\: handler-spec))
                       (handler-pkg-name (if colon-pos
                                             (subseq handler-spec 0 colon-pos)
                                             module-name))
                       (handler-fn-name (if colon-pos
                                            (subseq handler-spec (1+ colon-pos))
                                            handler-spec))
                       (package (find-package (string-upcase handler-pkg-name))))
                  (unless package
                    (format *error-output* "Error: Package ~A not found after loading module~%"
                            handler-pkg-name)
                    (sb-ext:exit :code 1))
                  (let ((fn-symbol (find-symbol (string-upcase handler-fn-name) package)))
                    (unless (and fn-symbol (fboundp fn-symbol))
                      (format *error-output* "Error: Function ~A:~A not found~%"
                              handler-pkg-name handler-fn-name)
                      (sb-ext:exit :code 1))
                    (log:info "Calling ~A:~A" handler-pkg-name handler-fn-name)
                    (if cmd-args-spec
                        ;; Declarative arg parsing
                        (let ((parsed (parse-declared-args cmd-args-spec
                                        (append remaining-args passthrough-args))))
                          (if (eq parsed :help)
                              (show-declared-args-help (or explicit-fn "run") cmd-args-spec)
                              (apply fn-symbol parsed)))
                        ;; Legacy passthrough
                        (if passthrough-args
                            (apply fn-symbol passthrough-args)
                            (funcall fn-symbol)))))

                ;; No handler spec found — fall back to module package lookup
                (let* ((function-name (or explicit-fn
                                          (module-default-entry-point module-name environment)))
                       (pkg-name (string-upcase module-name))
                       (package (find-package pkg-name)))
                  (unless package
                    (format *error-output* "Error: Package ~A not found after loading module~%" pkg-name)
                    (sb-ext:exit :code 1))

                  (let ((fn-symbol (find-symbol (string-upcase function-name) package)))
                    (unless fn-symbol
                      (format *error-output* "Error: Function ~A not found in ~A~%" function-name pkg-name)
                      (format *error-output* "Available exports:~%")
                      (do-external-symbols (s package)
                        (when (fboundp s)
                          (format *error-output* "  ~A~%" (symbol-name s))))
                      (sb-ext:exit :code 1))

                    (unless (fboundp fn-symbol)
                      (format *error-output* "Error: ~A is not a function~%" fn-symbol)
                      (sb-ext:exit :code 1))

                    (log:info "Calling ~A:~A" module-name function-name)
                    (if cmd-args-spec
                        ;; Declarative arg parsing
                        (let ((parsed (parse-declared-args cmd-args-spec
                                        (append remaining-args passthrough-args))))
                          (if (eq parsed :help)
                              (show-declared-args-help function-name cmd-args-spec)
                              (apply fn-symbol parsed)))
                        ;; Legacy passthrough
                        (if passthrough-args
                            (apply fn-symbol passthrough-args)
                            (funcall fn-symbol))))))))))))


(defun parse-keyword-args (args)
  "Parse --key value pairs into keyword argument list."
  (let ((result '()))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((and (str:starts-with-p arg "--")
                     (< (1+ i) (length args)))
                (let ((key (intern (string-upcase (subseq arg 2)) :keyword))
                      (value (nth (1+ i) args)))
                  ;; Try to parse as number
                  (let ((parsed-value (handler-case
                                          (parse-integer value)
                                        (error () value))))
                    (push key result)
                    (push parsed-value result))
                  (incf i 2)))
               (t (incf i))))
    (nreverse result)))

(defun parse-declared-args (arg-specs raw-args)
  "Parse RAW-ARGS according to declared ARG-SPECS.
   Each spec in ARG-SPECS is a plist with :name, :flag, :type, :default,
   :description, and :required.
   Returns a keyword plist suitable for APPLY, or :help if --help was given."
  ;; Check for --help first
  (when (member "--help" raw-args :test #'string=)
    (return-from parse-declared-args :help))
  (let ((result '()))
    ;; Process each declared arg spec
    (dolist (spec arg-specs)
      (let* ((name (getf spec :name))
             (flag (getf spec :flag))
             (type (or (getf spec :type) :string))
             (default (getf spec :default))
             (required (getf spec :required))
             (keyword (intern (string-upcase name) :keyword))
             (pos (position flag raw-args :test #'string=)))
        (cond
          ;; Boolean flags: presence means T
          ((eq type :boolean)
           (push keyword result)
           (push (if pos t default) result))
          ;; Value flags: consume the next arg
          (pos
           (let ((value-pos (1+ pos)))
             (if (< value-pos (length raw-args))
                 (let ((raw-value (nth value-pos raw-args)))
                   (push keyword result)
                   (push (coerce-arg-value raw-value type flag) result))
                 (progn
                   (format *error-output* "Error: ~A requires a value~%" flag)
                   (sb-ext:exit :code 1)))))
          ;; Not provided
          (required
           (format *error-output* "Error: ~A is required~%" flag)
           (sb-ext:exit :code 1))
          (t
           (push keyword result)
           (push default result)))))
    (nreverse result)))

(defun coerce-arg-value (raw-value type flag)
  "Coerce RAW-VALUE to TYPE. FLAG is used for error messages."
  (ecase type
    (:string raw-value)
    (:integer
     (handler-case (parse-integer raw-value)
       (error ()
         (format *error-output* "Error: ~A expects an integer, got ~S~%" flag raw-value)
         (sb-ext:exit :code 1))))))

(define-command eval (environment args passthrough-args)
  "Evaluate a Lisp expression."
  (:metadata
   :core-p t
   :usage-hint "eval [options] <expr>"
   :usage "epsilon eval [--package MODULE]... <expression>"
   :options ((:name "--package, -p" :arg "MODULE" :description "Pre-load module (repeatable)")
             (:name "--module" :arg "MODULE" :description "Pre-load module (alias for --package)")
             (:name "--load, -l" :arg "FILE" :description "Load a Lisp file (repeatable)"))
   :examples (("epsilon eval \"(+ 1 2)\"" "Evaluate expression")
              ("epsilon eval -p json '(json:parse \"{}\")'" "Evaluate with module loaded")
              ("epsilon eval --module json t" "Build a module")))
  (declare (ignore passthrough-args))
  ;; Translate eval-specific args into preamble directives.
  ;; --module is accepted as an alias for --package.
  ;; The positional expression becomes the final :eval directive.
  (let ((preamble '())
        (expression nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; --module (legacy) / --package / -p
               ((and (or (string= arg "--module")
                         (string= arg "--package")
                         (string= arg "-p"))
                     (< (1+ i) (length args)))
                (push (list :package (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --load / -l
               ((and (or (string= arg "--load") (string= arg "-l"))
                     (< (1+ i) (length args)))
                (push (list :load (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --eval / -e (explicit preamble eval)
               ((and (or (string= arg "--eval") (string= arg "-e"))
                     (< (1+ i) (length args)))
                (push (list :eval (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; Positional = expression
               ((not (str:starts-with-p arg "-"))
                (setf expression arg)
                (incf i))
               (t (incf i))))

    ;; Add positional expression as the final :eval directive
    (when expression
      (push (list :eval expression) preamble))

    (when (null preamble)
      (show-command-help "eval")
      (sb-ext:exit :code 1))

    ;; Execute all directives via the shared preamble pipeline
    (let ((result (execute-preamble (nreverse preamble) environment)))
      (unless (null result)
        (format t "~A~%" result)))))

(defun cmd-modules (environment args passthrough-args)
  "List available modules."
  (declare (ignore args passthrough-args))
  (let* ((modules (loader:query-modules environment))
         (module-data '()))

    (dolist (module modules)
      (let* ((name (loader:module-name module))
             (metadata (loader:module-metadata module))
             (module-set (or (getf metadata :module-set) ""))
             (platform (getf metadata :platform))
             (commands (getf metadata :commands))
             (location (ensure-location-string (loader:module-location module))))

        (when (or (not platform)
                  (string-equal platform (string-downcase (symbol-name (env:platform)))))
          (push (list name module-set location commands) module-data))))

    (setf module-data (sort module-data #'string< :key #'first))

    (format t "~%Available Modules~%")
    (format t "=================~%~%")
    (dolist (mod module-data)
      (format t "~40A ~18A ~A~%" (first mod) (second mod) (third mod))
      (dolist (cmd (fourth mod))
        (format t "~42A-> ~A~%" "" (getf cmd :name))))
    (format t "~%~D module~:P~%" (length module-data))))


(define-command load (environment args passthrough-args)
  "Load and execute a Lisp file."
  (:metadata
   :core-p t
   :usage-hint "load [options] <file>"
   :usage "epsilon load [--package MODULE]... <file> [--eval EXPR]"
   :options ((:name "--package, -p" :arg "MODULE" :description "Pre-load module (repeatable)")
             (:name "--module" :arg "MODULE" :description "Pre-load module (alias for --package)")
             (:name "--load, -l" :arg "FILE" :description "Load additional file (repeatable)")
             (:name "--eval, -e" :arg "EXPR" :description "Evaluate expression after loading"))
   :examples (("epsilon load script.lisp" "Load and execute a file")
              ("epsilon load -p json script.lisp" "Load with module pre-loaded")
              ("epsilon load script.lisp --eval \"(run)\"" "Load then evaluate")))
  (declare (ignore passthrough-args))
  ;; Translate load-specific args into preamble directives.
  ;; --module is accepted as an alias for --package.
  ;; The positional file becomes a :load directive.
  ;; --eval is placed after the file :load to preserve post-load semantics.
  (let ((preamble '())
        (file nil)
        (eval-expr nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; --module (legacy) / --package / -p
               ((and (or (string= arg "--module")
                         (string= arg "--package")
                         (string= arg "-p"))
                     (< (1+ i) (length args)))
                (push (list :package (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --load / -l (additional files)
               ((and (or (string= arg "--load") (string= arg "-l"))
                     (< (1+ i) (length args)))
                (push (list :load (nth (1+ i) args)) preamble)
                (incf i 2))
               ;; --eval / -e (post-load evaluation)
               ((and (or (string= arg "--eval") (string= arg "-e"))
                     (< (1+ i) (length args)))
                (setf eval-expr (nth (1+ i) args))
                (incf i 2))
               ;; Positional = file
               ((not (str:starts-with-p arg "-"))
                (setf file arg)
                (incf i))
               (t (incf i))))

    (when (null file)
      (show-command-help "load")
      (sb-ext:exit :code 1))

    ;; Build directives: packages/extra loads first, then main file, then eval
    (let ((directives (nreverse preamble)))
      (setf directives (append directives (list (list :load file))))
      (when eval-expr
        (setf directives (append directives (list (list :eval eval-expr)))))

      (let ((result (execute-preamble directives environment)))
        (unless (null result)
          (format t "~A~%" result))))))

;;; NOTE: The fmt, test, and repl commands have been moved to their
;;; respective modules (epsilon.print, epsilon.test, epsilon.server).
;;; They are registered via :commands in module.plist and loaded lazily.

;;; ---------------------------------------------------------------------------
;;; Change Detection
;;;
;;; Used by contributed command handlers (fmt, test, lint --changed).
;;; Exported via the epsilon.commands package.
;;; ---------------------------------------------------------------------------

(defun git-merge-base ()
  "Find the merge base between HEAD and main branch.
   Returns the commit hash or NIL if not in a git repo."
  (handler-case
      (let* ((result (with-output-to-string (out)
                       (sb-ext:run-program "git" '("merge-base" "HEAD" "main")
                                           :output out
                                           :error nil
                                           :search t)))
             (trimmed (string-trim '(#\Newline #\Return #\Space) result)))
        (when (> (length trimmed) 0) trimmed))
    (error () nil)))

(defun git-changed-files (&optional base)
  "Get list of files changed since BASE (defaults to merge base with main).
   Returns a list of relative file paths."
  (let ((merge-base (or base (git-merge-base))))
    (unless merge-base
      (return-from git-changed-files nil))
    (handler-case
        (let* ((result (with-output-to-string (out)
                         (sb-ext:run-program "git"
                                             (list "diff" "--name-only"
                                                   (format nil "~A...HEAD" merge-base))
                                             :output out
                                             :error nil
                                             :search t)))
               (lines (remove-if (lambda (l) (= 0 (length l)))
                                 (mapcar (lambda (l)
                                           (string-trim '(#\Newline #\Return #\Space) l))
                                         (loop for start = 0 then (1+ pos)
                                               for pos = (position #\Newline result :start start)
                                               collect (subseq result start (or pos (length result)))
                                               while pos)))))
          lines)
      (error () nil))))

(defun file-to-module-name (file-path environment)
  "Map a file path to its owning module name, or NIL if no match."
  (let ((modules (loader:query-modules environment)))
    (dolist (mod modules)
      (let ((location (path:path-string (loader:module-location mod))))
        (when (str:starts-with-p file-path location)
          (return-from file-to-module-name (loader:module-name mod))))))
  nil)

(defun transitive-dependents (module-name environment)
  "Find all modules that transitively depend on MODULE-NAME."
  (let ((result nil)
        (modules (loader:query-modules environment)))
    (labels ((depends-on-p (mod target)
               (let* ((mod-info (loader:get-module environment (loader:module-name mod)))
                      (location (when mod-info (loader:module-location mod-info)))
                      (project (when location
                                 (handler-case
                                     (loader:load-module-project (ensure-location-string location))
                                   (error () nil)))))
                 (when project
                   (member target (loader:module-project-requires project) :test #'string=))))
             (add-dependents (target)
               (dolist (mod modules)
                 (let ((name (loader:module-name mod)))
                   (when (and (not (member name result :test #'string=))
                              (not (string= name target))
                              (depends-on-p mod target))
                     (push name result)
                     (add-dependents name))))))
      (add-dependents module-name))
    result))

(defun ensure-location-string (location)
  "Coerce a module location (path object or pathname) to a string."
  (cond
    ((stringp location) location)
    ((pathnamep location) (namestring location))
    (t (path:path-string location))))

(defun detect-changed-modules (environment &key include-deps)
  "Detect modules changed since the merge base with main.
   When INCLUDE-DEPS is true, also include transitive dependents."
  (let* ((changed-files (git-changed-files))
         (module-names (remove-duplicates
                        (remove nil
                                (mapcar (lambda (f) (file-to-module-name f environment))
                                        changed-files))
                        :test #'string=)))
    (when include-deps
      (let ((all-deps nil))
        (dolist (mod module-names)
          (setf all-deps (append all-deps (transitive-dependents mod environment))))
        (setf module-names
              (remove-duplicates (append module-names all-deps) :test #'string=))))
    module-names))

