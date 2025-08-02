;;;; Package Command Base Class
;;;;
;;;; Provides a base class for commands that operate on packages,
;;;; with standard argument parsing and package resolution.

(defpackage epsilon.tool.package-command
  (:use cl)
  (:local-nicknames
   (main epsilon.main)
   (argparse epsilon.argparse)
   (proto epsilon.build.protocol)
   (build epsilon.tool.build)
   (map epsilon.map))
  (:export 
   #:package-command
   #:add-package-arguments
   #:setup-package-repos
   #:resolve-package
   #:get-local-package))

(in-package epsilon.tool.package-command)

;;; Package Command Base Class

(defclass package-command (main:command)
  ((package :accessor command-package :initform nil))
  (:documentation "Base class for commands that operate on packages"))

(defgeneric add-package-arguments (command parser)
  (:documentation "Add standard package selection arguments to parser"))

(defmethod add-package-arguments ((command package-command) parser)
  "Add standard package selection arguments"
  (argparse:add-argument parser "package"
                        :nargs '?
                        :help "Package name (defaults to local package)")
  (argparse:add-argument parser "--package-repo"
                        :action 'append
                        :metavar "PATH"
                        :help "Add additional package repository path (can be specified multiple times)")
  parser)

(defgeneric setup-package-repos (command parsed-args)
  (:documentation "Setup package repositories from command line arguments"))

(defmethod setup-package-repos ((command package-command) parsed-args)
  "Add any additional package repositories specified on command line"
  ;; This now happens in the command itself with proper environment
  nil)

(defgeneric resolve-package (command parsed-args)
  (:documentation "Resolve package from arguments or local context"))

(defmethod resolve-package ((command package-command) parsed-args)
  "Resolve package from arguments or local context"
  ;; Package repos should already be set up by the command
  (or (first (argparse:parsed-positionals parsed-args))
      (get-local-package)))

(defun get-local-package ()
  "Get the single local package or error if ambiguous/missing"
  (let ((packages (proto:list-packages (build:local-packages))))
    (case (length packages)
      (0 (error "No local package defined"))
      (1 (first packages))
      (t (error "Ambiguous local packages: ~{~A~^, ~}" packages)))))