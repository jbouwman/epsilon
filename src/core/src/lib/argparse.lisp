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
   (#:seq #:epsilon.sequence))
  (:export
   ;; Main API
   #:make-parser
   #:add-argument
   #:add-command
   #:parse-args
   #:print-help
   #:print-usage
   
   ;; Parser accessors
   #:parser-prog
   #:parser-description
   #:parser-epilog
   
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
   
   ;; Conditions
   #:argument-error
   #:unknown-argument-error
   #:missing-argument-error
   #:invalid-choice-error
   #:type-conversion-error))

(in-package #:epsilon.argparse)

;;; Conditions

(define-condition argument-error (error)
  ((message :initarg :message :reader error-message))
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
  ((prog :initarg :prog :accessor parser-prog :initform nil
         :documentation "Program name")
   (description :initarg :description :accessor parser-description :initform ""
                :documentation "Program description")
   (epilog :initarg :epilog :accessor parser-epilog :initform ""
           :documentation "Text to display after help")
   (arguments :initform '() :accessor parser-arguments
              :documentation "List of arguments")
   (commands :initform (map:make-map) :accessor parser-commands
             :documentation "Map of subcommand names to parsers")
   (parent :initarg :parent :accessor parser-parent :initform nil
           :documentation "Parent parser for subcommands")))

(defun make-parser (&key prog description epilog)
  "Create a new argument parser"
  (make-instance 'parser :prog prog :description description :epilog epilog))

(defun add-argument (parser name &rest args)
  "Add an argument to the parser"
  (let* ((action (getf args :action 'store))
         ;; Set implicit defaults for store-true/store-false if not provided
         (args (if (and (member action '(store-true store-false))
                        (not (member :default args)))
                   (append args (list :default (if (eq action 'store-true) nil t)))
                   args))
         (argument (apply #'make-argument name args)))
    (push argument (parser-arguments parser))
    argument))

(defun add-command (parser name &key description help)
  "Add a subcommand to the parser"
  (let ((subparser (make-parser :prog name :description (or description help))))
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
              :documentation "Remaining unparsed arguments")))

;;; Type Conversion

(defun convert-value (value type)
  "Convert a string value to the specified type"
  (handler-case
      (cond
        ((eq type 'string) value)
        ((eq type 'integer) (parse-integer value))
        ((eq type 'boolean) 
         (if (member value '("true" "yes" "1" "on") :test #'string-equal) t nil))
        ((functionp type) (funcall type value))
        (t value))
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
                ;; Parse subcommand arguments
                (let ((sub-result (parse-args subparser post-command)))
                  ;; Merge subcommand results
                  (setf (parsed-options result) 
                        (map:merge (parsed-options result) 
                                  (parsed-options sub-result)))
                  (setf (parsed-positionals result)
                        (append (parsed-positionals result)
                               (parsed-positionals sub-result)))
                  (setf (parsed-remaining result)
                        (parsed-remaining sub-result))))
              ;; Unknown command - treat as positional
              (push command-name (parsed-positionals result))))))
    
    ;; Validate required arguments
    (validate-required parser result)
    
    ;; Apply defaults
    (apply-defaults parser result)
    
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
                           (converted (convert-value arg (argument-type pos-arg))))
                      (push arg (parsed-positionals result))
                      (setf (parsed-options result)
                            (map:assoc (parsed-options result)
                                      (argument-key pos-arg)
                                      converted))
                      (incf positional-index))
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
    
    (let ((key (argument-key argument)))
      (case (argument-action argument)
        (store-true
         (setf (parsed-options result) (map:assoc (parsed-options result) key t))
         (values 0 t))
        
        (store-false
         (setf (parsed-options result) (map:assoc (parsed-options result) key nil))
         (values 0 t))
        
        (store
         (let ((value (or option-value
                         (when (and args (not (str:starts-with-p (first args) "-")))
                           (first args)))))
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
           
           (values (if option-value 0 1) t)))
        
        (append
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
           
           (values (if option-value 0 1) t)))))))

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

(defun apply-defaults (parser result)
  "Apply default values for missing arguments"
  (dolist (arg (parser-arguments parser))
    (let ((key (argument-key arg)))
      (unless (map:contains-p (parsed-options result) key)
        (when (argument-default arg)
          (setf (parsed-options result)
                (map:assoc (parsed-options result) key 
                          (argument-default arg))))))))

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
  (format stream "Usage: ~A" (or (parser-prog parser) "prog"))
  
  ;; Add parent command chain
  (when (parser-parent parser)
    (let ((chain '()))
      (loop for p = parser then (parser-parent p)
            while (parser-parent p)
            do (push (parser-prog p) chain))
      (dolist (cmd chain)
        (format stream " ~A" cmd))))
  
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
               (not (member (argument-action arg) '(store-true store-false))))
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
