;;;; Argument Parsing Library
;;;;
;;;; A declarative argument parsing library that supports:
;;;; - Global options before commands
;;;; - Hierarchical command structure
;;;; - Automatic help generation
;;;; - Type validation and conversion

(defpackage #:epsilon.argparse
  (:use #:cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:m #:epsilon.match))
  (:export
   ;; Main API
   #:make-parser
   #:add-argument
   #:add-command
   #:parse-args
   #:print-help
   #:print-usage
   
   ;; Parser accessors
   #:parser-command
   #:parser-description
   #:parser-epilog
   #:parser-handler
   #:parser-commands
   
   ;; Argument types
   #:argument
   #:make-argument
   #:argument-name
   #:argument-help
   #:argument-type
   #:argument-default
   #:argument-required
   #:argument-action
   #:argument-choices
   #:argument-metavar
   #:argument-nargs
   
   ;; Parsed results
   #:parsed-arguments
   #:parsed-command
   #:parsed-options
   #:parsed-positionals
   #:parsed-remaining
   #:parsed-subresult
   
   ;; Conditions
   #:argument-error
   #:error-message
   #:error-parser
   #:unknown-argument-error
   #:missing-argument-error
   #:invalid-choice-error
   #:type-conversion-error))

(in-package #:epsilon.argparse)

;;; Conditions

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

;;; Core Data Structures

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
          ;; Strip leading dashes and convert remaining dashes to underscores
          (let* ((name (argument-name arg))
                 (trimmed (string-trim "-" name)))
            ;; Simple dash to underscore conversion
            (with-output-to-string (s)
              (loop for char across trimmed
                    do (write-char (if (char= char #\-) #\_ char) s))))
          (argument-name arg))))

(defclass parser ()
  ((command :initarg :command :accessor parser-command :initform nil
         :documentation "Command name")
   (description :initarg :description :accessor parser-description :initform ""
                :documentation "Command description")
   (epilog :initarg :epilog :accessor parser-epilog :initform ""
           :documentation "Text to display after help")
   (arguments :initform '() :accessor parser-arguments
              :documentation "List of arguments")
   (commands :initform (map:make-map) :accessor parser-commands
             :documentation "Map of subcommand names to parsers")
   (parent :initarg :parent :accessor parser-parent :initform nil
           :documentation "Parent parser for subcommands")
   (handler :initarg :handler :accessor parser-handler :initform nil
            :documentation "Function to handle this command")))

(defun make-parser (&key command description epilog handler)
  "Create a new argument parser"
  (make-instance 'parser :command command :description description :epilog epilog :handler handler))

(defun add-argument (parser name &rest args)
  "Add an argument to the parser"
  (let* ((action (getf args :action 'store))
         ;; Normalize action to keyword
         (action-keyword (intern (string action) :keyword))
         ;; Set implicit defaults for store-true/store-false if not provided
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

;;; Parsing Results

(defclass parsed-arguments ()
  ((command :initarg :command :accessor parsed-command :initform nil
            :documentation "The subcommand that was invoked")
   (options :initarg :options :accessor parsed-options :initform (map:make-map)
            :documentation "Map of option names to values")
   (positionals :initarg :positionals :accessor parsed-positionals :initform '()
                :documentation "List of positional argument values")
   (remaining :initarg :remaining :accessor parsed-remaining :initform '()
              :documentation "Remaining unparsed arguments")
   (subresult :initarg :subresult :accessor parsed-subresult :initform nil
              :documentation "Isolated parsing result for subcommand")))

;;; Type Conversion

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

;;; Parsing Implementation

(defun parse-args (parser args)
  "Parse command line arguments"
  (let ((result (make-instance 'parsed-arguments))
        (remaining args))
    
    ;; First pass: collect all arguments up to a potential subcommand
    (multiple-value-bind (pre-command post-command command-name)
        (split-at-command parser remaining)
      
      ;; Parse pre-command arguments (global options and positionals)
      (setf remaining (parse-arguments parser pre-command result))
      
      ;; Handle subcommand if present
      (when command-name
        (let ((subparser (map:get (parser-commands parser) command-name)))
          (if subparser
              (progn
                (setf (parsed-command result) command-name)
                ;; Parse subcommand arguments in isolated context
                (let ((sub-result (parse-args subparser post-command)))
                  ;; Store subcommand result separately instead of merging
                  (setf (parsed-subresult result) sub-result)
                  ;; Only propagate remaining args from subcommand
                  (setf (parsed-remaining result)
                        (parsed-remaining sub-result))))
              ;; Unknown command - treat as positional
              (push command-name (parsed-positionals result))))))
    
    ;; Validate required arguments
    (validate-required parser result)
    
    ;; Validate no unknown options in remaining
    (validate-no-unknown-options parser result)
    
    ;; Apply defaults
    (apply-defaults parser result)
    
    ;; Reverse positionals since they were built with push
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
               ;; Double dash - rest are positional
               ((string= arg "--")
                (setf remaining (append remaining args))
                (return))
               
               ;; Option argument
               ((str:starts-with-p arg "-")
                (multiple-value-bind (consumed-args option-handled-p)
                    (parse-option parser arg args result)
                  (if option-handled-p
                      (dotimes (i consumed-args)
                        (pop args))
                      (push arg remaining))))
               
               ;; Positional argument
               (t
                (if (< positional-index (length positional-args))
                    (let* ((pos-arg (nth positional-index positional-args))
                           (nargs (argument-nargs pos-arg)))
                      (cond
                        ;; nargs = '*' or '+' - consume all remaining positional args
                        ((or (eq nargs '*) (eq nargs '+))
                         (let ((values (list arg)))
                           ;; Collect all remaining positional arguments
                           (loop while (and args 
                                           (not (str:starts-with-p (first args) "-")))
                                 do (push (pop args) values))
                           (setf values (nreverse values))
                           ;; Store all values in positionals
                           (dolist (val values)
                             (push val (parsed-positionals result)))
                           ;; Store collected values in options under the argument key
                           (setf (parsed-options result)
                                 (map:assoc (parsed-options result)
                                           (argument-key pos-arg)
                                           values))
                           (incf positional-index)))
                        ;; nargs = nil (single value) - existing behavior
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
      ;; Normalize action symbol to handle package differences
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
           
           ;; Handle nargs='? - optional argument
           (cond
             ((eq (intern (string nargs) :keyword) :?)
              ;; For optional arguments, if no value is provided, use empty string
              (let ((actual-value (or value "")))
                (let ((converted (convert-value actual-value (argument-type argument))))
                  (setf (parsed-options result) 
                        (map:assoc (parsed-options result) key converted)))
                (values (if (and has-next-arg value) 1 0) t)))
             
             ;; Regular store - value is required
             (t
              (unless value
                (error 'missing-argument-error
                       :message (format nil "Option ~A requires an argument" option-name)
                       :argument argument))
              
              ;; Validate choice if specified
              (when (argument-choices argument)
                (unless (member value (argument-choices argument) :test #'string=)
                  (error 'invalid-choice-error
                         :message (format nil "Invalid choice '~A' for ~A (choose from ~{~A~^, ~})"
                                        value option-name (argument-choices argument))
                         :value value
                         :choices (argument-choices argument))))
              
              ;; Convert type
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
         ;; This shouldn't happen - return nil to indicate not handled
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
        ;; Apply default value if explicitly provided
        ;; Don't auto-add nil defaults for store-true actions to keep options map clean
        (let ((default-value (argument-default arg))
              (action-keyword (intern (string (argument-action arg)) :keyword)))
          (when (and default-value 
                     (not (and (eq action-keyword :store-true) 
                               (null default-value))))
            (setf (parsed-options result)
                  (map:assoc (parsed-options result) key default-value))))))))

;;; Help Generation

(defun print-help (parser &optional (stream *standard-output*))
  "Print help message for parser"
  (print-usage parser stream)
  (terpri stream)
  
  ;; Description
  (when (parser-description parser)
    (format stream "~A~%~%" (parser-description parser)))
  
  ;; Arguments sections
  (let ((positionals (remove-if #'argument-is-option-p (parser-arguments parser)))
        (options (remove-if-not #'argument-is-option-p (parser-arguments parser))))
    
    ;; Positional arguments
    (when positionals
      (format stream "Arguments:~%")
      (dolist (arg (reverse positionals))
        (print-argument-help arg stream))
      (terpri stream))
    
    ;; Optional arguments
    (when options
      (format stream "Options:~%")
      (dolist (arg (reverse options))
        (print-argument-help arg stream))
      (terpri stream))
    
    ;; Subcommands
    (when (plusp (map:count (parser-commands parser)))
      (format stream "Commands:~%")
      (map:each (lambda (name subparser)
                  (format stream "  ~A~:[~;~:*~48T~A~]~%"
                          name (parser-description subparser)))
                (parser-commands parser))
      (terpri stream)))
  
  ;; Epilog
  (when (parser-epilog parser)
    (format stream "~A~%" (parser-epilog parser))))

(defun print-usage (parser &optional (stream *standard-output*))
  "Print usage line for parser"
  ;; Build command chain from current command up through parents
  (let ((chain '()))
    (loop for p = parser then (parser-parent p)
          while p
          do (push (parser-command p) chain))
    
    ;; Handle special cases for proper usage display
    (cond
      ;; No chain means standalone command
      ((null chain)
       (format stream "Usage: command"))
      ;; Single command - could be top-level epsilon command
      ((= (length chain) 1)
       (let ((cmd (first chain)))
         (if (string= cmd "epsilon")
             (format stream "Usage: epsilon")
             ;; Assume it's a subcommand under epsilon
             (format stream "Usage: epsilon ~A" cmd))))
      ;; Multiple commands - build proper hierarchy
      (t
       ;; Chain is built as: [parent, current] by pushing during traversal
       ;; So for "list" under "package": chain = (package list)
       ;; We want: epsilon package list
       (let ((cmd-list (cons "epsilon" chain)))
         (format stream "Usage: ~{~A~^ ~}" cmd-list)))))
  
  ;; Global options indicator
  (when (some #'argument-is-option-p (parser-arguments parser))
    (format stream " [options]"))
  
  ;; Subcommand indicator
  (when (plusp (map:count (parser-commands parser)))
    (format stream " <command>"))
  
  ;; Positional arguments
  (let ((positionals (remove-if #'argument-is-option-p (parser-arguments parser))))
    (dolist (arg (reverse positionals))
      (format stream " ~:[~A~;<~A>~]" 
              (argument-required arg)
              (or (argument-metavar arg) (argument-name arg)))))
  
  ;; Trailing arguments indicator
  (format stream " [arguments]"))

(defun print-argument-help (arg stream)
  "Print help for a single argument"
  (let ((names (argument-name arg))
        (metavar (or (argument-metavar arg)
                    (string-upcase (argument-key arg)))))
    
    ;; Argument name(s) and metavar
    (format stream "  ~A" names)
    (when (and (argument-is-option-p arg)
               (let ((action-keyword (intern (string (argument-action arg)) :keyword)))
                 (not (member action-keyword '(:store-true :store-false)))))
      (format stream " ~A" metavar))
    
    ;; Help text
    (when (argument-help arg)
      (format stream "~48T~A" (argument-help arg)))
    
    ;; Additional info
    (let ((info '()))
      (when (argument-choices arg)
        (push (format nil "choices: ~{~A~^, ~}" (argument-choices arg)) info))
      (when (argument-default arg)
        (push (format nil "default: ~A" (argument-default arg)) info))
      (when info
        (format stream " (~{~A~^; ~})" (nreverse info))))
    
    (terpri stream)))

;;; Convenience Functions

(defun get-option (parsed name &optional default)
  "Get an option value from parsed arguments"
  (map:get (parsed-options parsed) name default))

(defun get-positional (parsed index &optional default)
  "Get a positional argument by index"
  (if (< index (length (parsed-positionals parsed)))
      (nth index (parsed-positionals parsed))
      default))
