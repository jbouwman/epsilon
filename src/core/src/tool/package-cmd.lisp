;;;; Package Command - Manage epsilon packages
;;;;
;;;; This module provides package management functionality including
;;;; listing, installing, searching, and showing information about packages.

(defpackage epsilon.tool.package-cmd
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (path epsilon.path)
   (argparse epsilon.argparse)
   (main epsilon.main)
   (package epsilon.tool.package)
   (table epsilon.table)
   (common epsilon.tool.common)))

(in-package :epsilon.tool.package-cmd)

(defclass pkg (main:command)
  ())

(defun handle-package-list (parsed)
  "Handle package list subcommand"
  (let* ((options (argparse:parsed-options parsed))
         (installed-only (map:get options "installed_only"))
         (available-only (map:get options "available_only"))
         (packages (package:list-packages 
                    :installed-only installed-only
                    :available-only available-only))
         (local-package (common:read-local-package-definition))
         (package-data '()))
    
    ;; Collect package information
    (when (and local-package (not available-only))
      (push (list (getf local-package :name)
                  (or (getf local-package :version) "unknown")
                  "LOCAL"
                  (getf local-package :path))
            package-data))
    
    (dolist (pkg packages)
      (let ((pkg-name (if (consp pkg) (car pkg) pkg))
            (pkg-status (if (consp pkg) (string (cdr pkg)) "AVAILABLE")))
        ;; Skip if this is the same as our local package
        (unless (and local-package 
                     (string= pkg-name (getf local-package :name)))
          (let ((pkg-info (ignore-errors (package:package-info pkg-name))))
            (if pkg-info
                (let ((uri (getf pkg-info :uri)))
                  (push (list pkg-name
                              (or (getf pkg-info :version) "unknown")
                              pkg-status
                              (if uri (ignore-errors (path:path-string uri)) "unknown"))
                        package-data))
                ;; Fallback if no info available
                (push (list pkg-name "unknown" pkg-status "unknown")
                      package-data))))))
    
    ;; Sort packages by name and format as table
    (setf package-data (sort (nreverse package-data) #'string< :key #'first))
    
    (let ((pkg-table (table:simple-table 
                      '("NAME" "VERSION" "STATUS" "LOCATION")
                      package-data)))
      (format t "~%")
      (table:print-table pkg-table)
      (format t "~%Found ~D package~:P~%" (length package-data)))))

(defun handle-package-info (parsed)
  "Handle package info subcommand"
  (let* ((positionals (argparse:parsed-positionals parsed))
         (remaining (argparse:parsed-remaining parsed))
         ;; Handle both proper subcommand parsing and workaround
         (package-name (or (first positionals) (second remaining))))
    (if package-name
        (let ((info (package:package-info package-name)))
          (if info
              (progn
                (format t "Package: ~A~%" (getf info :name))
                (format t "Version: ~A~%" (or (getf info :version) "unknown"))
                (when (getf info :author)
                  (format t "Author: ~A~%" (getf info :author)))
                (when (getf info :description)
                  (format t "Description: ~A~%" (getf info :description)))
                (when (getf info :location)
                  (format t "Location: ~A~%" (getf info :location)))
                (when (getf info :dependencies)
                  (format t "Dependencies: ~{~A~^, ~}~%" (getf info :dependencies)))
                (when (getf info :installed)
                  (format t "Status: INSTALLED~%")))
              (format t "Package not found: ~A~%" package-name)))
        (format t "Error: Package name required~%"))))

(defun handle-package-install (parsed)
  "Handle package install subcommand"
  (let* ((positionals (argparse:parsed-positionals parsed))
         (remaining (argparse:parsed-remaining parsed))
         (options (argparse:parsed-options parsed))
         ;; Handle both proper subcommand parsing and workaround
         (package-name (or (first positionals) (second remaining)))
         (force (map:get options "force")))
    (if package-name
        (handler-case
            (if (package:install-package package-name :force force :verbose t)
                (format t "Package ~A installed successfully~%" package-name)
                (format t "Failed to install package ~A~%" package-name))
          (error (e)
            (format *error-output* "Error: ~A~%" e)))
        (format t "Error: Package name required~%"))))

(defun handle-package-search (parsed)
  "Handle package search subcommand"
  (let* ((positionals (argparse:parsed-positionals parsed))
         (remaining (argparse:parsed-remaining parsed))
         (options (argparse:parsed-options parsed))
         ;; Handle both proper subcommand parsing and workaround
         (query (or (first positionals) (second remaining)))
         (name-only (map:get options "name_only"))
         (description-only (map:get options "description_only")))
    (if query
        (let ((results (package:search-packages query 
                                                :name-only name-only
                                                :description-only description-only)))
          (if results
              (dolist (result results)
                (format t "~A - ~A~%" 
                        (getf result :name)
                        (or (getf result :description) "No description")))
              (format t "No packages found matching '~A'~%" query)))
        (format t "Error: Search query required~%"))))

(defmethod main:run-command ((command pkg) parsed-args)
  (let* ((subcommand (argparse:parsed-command parsed-args))
         (remaining (argparse:parsed-remaining parsed-args))
         (actual-subcommand (or subcommand (first remaining))))
    (cond
      ((string= actual-subcommand "list")
       (handle-package-list parsed-args))
      
      ((string= actual-subcommand "info")
       (handle-package-info parsed-args))
      
      ((string= actual-subcommand "install")
       (handle-package-install parsed-args))
      
      ((string= actual-subcommand "search")
       (handle-package-search parsed-args))
      
      ((or (null actual-subcommand) (string= actual-subcommand "help"))
       (argparse:print-help (main:argument-parser command)))
      
      (t
       (format t "Unknown subcommand: ~A~%" actual-subcommand)))))

(defmethod main:argument-parser ((command pkg))
  (let ((parser (argparse:make-parser 
                 :command "package"
                 :description "Manage epsilon packages")))
    
    ;; Add subcommands
    (let ((list-cmd (argparse:add-command parser "list" 
                                          :description "List all packages"))
          (info-cmd (argparse:add-command parser "info" 
                                          :description "Show package information"))
          (install-cmd (argparse:add-command parser "install" 
                                             :description "Install a package"))
          (search-cmd (argparse:add-command parser "search" 
                                            :description "Search for packages")))
      
      ;; Add help command
      (argparse:add-command parser "help" 
                            :description "Show help for package command")
      
      ;; list command options
      (argparse:add-argument list-cmd "--installed-only" 
                             :action "store_true"
                             :help "Show only installed packages")
      (argparse:add-argument list-cmd "--available-only" 
                             :action "store_true"
                             :help "Show only available packages")
      
      ;; info command arguments
      (argparse:add-argument info-cmd "package" 
                             :help "Package name to show info for")
      
      ;; install command arguments
      (argparse:add-argument install-cmd "package" 
                             :help "Package name to install")
      (argparse:add-argument install-cmd "--force" 
                             :action "store_true"
                             :help "Force reinstall even if already installed")
      
      ;; search command arguments
      (argparse:add-argument search-cmd "query" 
                             :help "Search query")
      (argparse:add-argument search-cmd "--name-only" 
                             :action "store_true"
                             :help "Search only in package names")
      (argparse:add-argument search-cmd "--description-only" 
                             :action "store_true"
                             :help "Search only in descriptions"))
    
    parser))

(main:register-command 'pkg)