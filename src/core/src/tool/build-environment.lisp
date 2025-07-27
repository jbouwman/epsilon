;;;; Build Environment
;;;;
;;;; This module defines the build environment object that encapsulates
;;;; all configuration and state for a build session.

(defpackage epsilon.tool.build-environment
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (path epsilon.path)
   (proto epsilon.build.protocol))
  (:export
   ;; Environment class
   #:build-environment
   #:make-build-environment
   
   ;; Accessors
   #:environment-package-source
   #:environment-package-repos
   #:environment-loaded-packages
   #:environment-policies
   #:environment-options
   
   ;; Policy class and accessors
   #:build-policies
   #:make-build-policies
   #:policy-error-behavior
   #:policy-warning-behavior
   #:policy-optimization
   
   ;; Options class and accessors
   #:build-options
   #:make-build-options
   #:option-parallel
   #:option-timeout
   #:option-force
   #:option-verbose
   
   ;; Operations
   #:ensure-package-source
   #:add-package-repo
   #:get-package
   #:register-package
   #:is-package-loaded
   #:mark-package-loaded
   #:with-build-environment
   
   ;; Package info
   #:package-info
   #:package-info-name
   #:package-location
   #:package-project
   #:package-loaded-p
   #:package-load-time))

(in-package epsilon.tool.build-environment)

;;; Build Policies

(defclass build-policies ()
  ((error-behavior :initarg :error-behavior
                   :initform :halt
                   :accessor policy-error-behavior
                   :documentation "How to handle compilation errors: :halt, :ignore, or :print")
   (warning-behavior :initarg :warning-behavior
                     :initform :ignore
                     :accessor policy-warning-behavior
                     :documentation "How to handle compilation warnings: :halt, :ignore, or :print")
   (optimization :initarg :optimization
                 :initform '(speed 1 safety 3 debug 2)
                 :accessor policy-optimization
                 :documentation "Compiler optimization settings"))
  (:documentation "Build policies that control compilation behavior"))

(defun make-build-policies (&key (error-behavior :halt)
                                 (warning-behavior :ignore)
                                 (optimization '(speed 1 safety 3 debug 2)))
  "Create a new build policies object"
  (make-instance 'build-policies
                 :error-behavior error-behavior
                 :warning-behavior warning-behavior
                 :optimization optimization))

;;; Build Options

(defclass build-options ()
  ((parallel :initarg :parallel
             :initform nil
             :accessor option-parallel
             :documentation "Whether to use parallel compilation")
   (timeout :initarg :timeout
            :initform 60
            :accessor option-timeout
            :documentation "Timeout for individual file compilation in seconds")
   (force :initarg :force
          :initform nil
          :accessor option-force
          :documentation "Force rebuild even if up-to-date")
   (verbose :initarg :verbose
            :initform nil
            :accessor option-verbose
            :documentation "Show verbose build output"))
  (:documentation "Build options that control build execution"))

(defun make-build-options (&key parallel (timeout 60) force verbose)
  "Create a new build options object"
  (make-instance 'build-options
                 :parallel parallel
                 :timeout timeout
                 :force force
                 :verbose verbose))

;;; Package Info

(defclass package-info ()
  ((name :initarg :name 
         :reader package-info-name 
         :type string
         :documentation "Package name")
   (location :initarg :location 
             :reader package-location
             :documentation "Package location (path or URI)")
   (project :initarg :project 
            :accessor package-project 
            :initform nil
            :documentation "Associated project object")
   (loaded-p :initarg :loaded-p 
             :accessor package-loaded-p 
             :initform nil 
             :type boolean
             :documentation "Whether package has been loaded")
   (load-time :initarg :load-time 
              :accessor package-load-time 
              :initform nil
              :documentation "When package was loaded"))
  (:documentation "Information about a loaded or registered package"))

;;; Build Environment

(defclass build-environment ()
  ((package-source :initform nil
                   :accessor environment-package-source
                   :documentation "Composite package source for this environment")
   (package-repos :initform '()
                  :accessor environment-package-repos
                  :documentation "List of additional package repository paths")
   (loaded-packages :initform (map:make-map)
                    :accessor environment-loaded-packages
                    :documentation "Registry of loaded packages (map of name -> package-info)")
   (policies :initarg :policies
             :accessor environment-policies
             :initform (make-build-policies)
             :documentation "Build policies (errors, warnings, etc.)")
   (options :initarg :options
            :accessor environment-options
            :initform (make-build-options)
            :documentation "Build options (parallel, timeout, etc.)"))
  (:documentation "Encapsulates all configuration and state for a build session"))

(defun make-build-environment (&key policies options)
  "Create a new build environment"
  (make-instance 'build-environment
                 :policies (or policies (make-build-policies))
                 :options (or options (make-build-options))))

;;; Package Source Management

(defgeneric ensure-package-source (environment)
  (:documentation "Ensure package source is initialized for the environment"))

(defmethod ensure-package-source ((env build-environment))
  "Ensure package source is initialized and return it"
  (or (environment-package-source env)
      (let* ((epsilon-home (or (sb-ext:posix-getenv "EPSILON_HOME") "."))
             (epsilon-src (path:path-join epsilon-home "src"))
             (main-source (proto:make-directory-source epsilon-src))
             (repo-sources (mapcar #'proto:make-directory-source 
                                   (environment-package-repos env))))
        (setf (environment-package-source env)
              (if repo-sources
                  (proto:make-composite-source 
                   (cons main-source repo-sources))
                  main-source)))))

(defgeneric add-package-repo (environment path)
  (:documentation "Add a package repository to the environment"))

(defmethod add-package-repo ((env build-environment) path)
  "Add an additional package repository to search"
  (pushnew (path:path-string (path:ensure-path path)) 
           (environment-package-repos env)
           :test #'string=)
  ;; Reset the package source to include the new repo
  (setf (environment-package-source env) nil)
  (ensure-package-source env))

;;; Module Registry

(defgeneric get-package (environment name &key error-p)
  (:documentation "Get package-info for a given package name"))

(defmethod get-package ((env build-environment) name &key (error-p nil))
  "Get package-info for a given package name, or NIL if not found"
  (or (map:get (environment-loaded-packages env) name)
      (and error-p
           (error "Package not found: ~A" name))))

(defgeneric register-package (environment name location &key project)
  (:documentation "Register a package in the environment"))

(defmethod register-package ((env build-environment) name location &key project)
  "Register a package in the environment registry"
  (let ((package-info (make-instance 'package-info
                                     :name name
                                     :location location
                                     :project project
                                     :loaded-p nil)))
    (map:assoc! (environment-loaded-packages env) name package-info)
    package-info))

(defgeneric is-package-loaded (environment name)
  (:documentation "Check if a package has been loaded"))

(defmethod is-package-loaded ((env build-environment) name)
  "Check if a package has been loaded"
  (let ((package-info (get-package env name)))
    (and package-info (package-loaded-p package-info))))

(defgeneric mark-package-loaded (environment name)
  (:documentation "Mark a package as loaded"))

(defmethod mark-package-loaded ((env build-environment) name)
  "Mark a package as loaded"
  (let ((package-info (get-package env name)))
    (when package-info
      (setf (package-loaded-p package-info) t
            (package-load-time package-info) (get-universal-time)))))

;;; Context Management

(defmacro with-build-environment ((var &optional environment) &body body)
  "Execute body with VAR bound to a build environment"
  `(let ((,var (or ,environment (make-build-environment))))
     ,@body))