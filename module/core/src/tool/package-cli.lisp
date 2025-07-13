;;;; Package Management CLI - Moved to Registry Module
;;;;
;;;; Package management functionality has been moved to the epsilon.registry module
;;;; to avoid bootstrap circular dependencies.

(defpackage :epsilon.tool.package-cli
  (:use :cl)
  (:export
   ;; Stub functions
   #:search-packages
   #:add-package
   #:remove-package
   #:update-packages
   #:install-package
   #:publish-package))

(in-package :epsilon.tool.package-cli)

(defun search-packages (&rest args)
  "Package search functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))

(defun add-package (&rest args)
  "Package add functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))

(defun remove-package (&rest args)
  "Package remove functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))

(defun update-packages (&rest args)
  "Package update functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))

(defun install-package (&rest args)
  "Package install functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))

(defun publish-package (&rest args)
  "Package publish functionality has been moved to epsilon.registry module"
  (declare (ignore args))
  (error "Package management has been moved to epsilon.registry module. Please load that module first."))