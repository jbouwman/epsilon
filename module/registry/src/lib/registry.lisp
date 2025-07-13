;;;; Basic Module Registry and Package Cache
;;;;
;;;; This module provides basic package registry functionality.
;;;; This is a simplified version focused on core registry operations
;;;; without complex dependencies.

(defpackage :epsilon.lib.registry
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:path :epsilon.lib.path)
   (:package :epsilon.lib.package))
  (:export
   ;; Registry management
   #:add-registry
   #:remove-registry
   #:list-registries
   #:set-primary-registry
   
   ;; Basic operations
   #:get-package-info
   #:download-package
   
   ;; Configuration
   #:*offline-mode*
   #:*cache-dir*
   #:*registries*))

(in-package :epsilon.lib.registry)

;;; Configuration

(defparameter *cache-dir* 
  (path:path-merge (or (sb-posix:getenv "EPSILON_CACHE")
                       (path:path-merge (sb-posix:getenv "HOME") ".epsilon/cache")))
  "Local cache directory for packages")

(defparameter *registries*
  (list (map:make-map
         "name" "official"
         "url" "https://packages.epsilon-lang.org/api/v1"
         "primary" t))
  "List of configured registries")

(defparameter *offline-mode* nil
  "When true, only use cached packages")

;;; Registry data structures

(defstruct registry
  "Registry configuration"
  (name nil :type string)
  (url nil :type string)
  (primary nil :type boolean))

(defstruct package-info
  "Basic package metadata"
  (name nil :type string)
  (description nil :type (or null string))
  (versions nil :type list)
  (homepage nil :type (or null string)))

;;; Registry management

(defun find-registry (name)
  "Find registry by name"
  (find name *registries*
        :key (lambda (r) (map:get r "name"))
        :test #'string=))

(defun primary-registry ()
  "Get the primary registry"
  (or (find-if (lambda (r) (map:get r "primary")) *registries*)
      (first *registries*)))

(defun add-registry (name url &key primary)
  "Add a new registry"
  (when (find-registry name)
    (error "Registry ~A already exists" name))
  
  ;; If this is marked as primary, unmark others
  (when primary
    (setf *registries*
          (mapcar (lambda (r)
                    (map:assoc r "primary" nil))
                  *registries*)))
  
  (push (map:make-map
         "name" name
         "url" url
         "primary" primary)
        *registries*))

(defun remove-registry (name)
  "Remove a registry"
  (setf *registries*
        (remove name *registries*
                :key (lambda (r) (map:get r "name"))
                :test #'string=)))

(defun list-registries ()
  "List all configured registries"
  *registries*)

(defun set-primary-registry (name)
  "Set the primary registry"
  (unless (find-registry name)
    (error "Registry ~A not found" name))
  
  (setf *registries*
        (mapcar (lambda (r)
                  (map:assoc r "primary" 
                             (string= (map:get r "name") name)))
                *registries*)))

;;; Basic package operations

(defun get-package-info (name &key (registry (primary-registry)))
  "Get basic package information (placeholder implementation)"
  (declare (ignore registry))
  (when *offline-mode*
    (return-from get-package-info nil))
  
  ;; This is a placeholder - real implementation would make HTTP requests
  (make-package-info
   :name name
   :description (format nil "Package ~A" name)
   :versions '("1.0.0" "1.1.0" "2.0.0")
   :homepage (format nil "https://packages.epsilon-lang.org/~A" name)))

(defun download-package (name version &key (registry (primary-registry)))
  "Download a package version (placeholder implementation)"
  (declare (ignore registry))
  (when *offline-mode*
    (error "Package ~A@~A not in cache and offline mode is enabled" name version))
  
  ;; This is a placeholder - real implementation would download and cache
  (format nil "~A/packages/~A/~A" *cache-dir* name version))

;;; Placeholder functions for compatibility

(defun resolve-dependencies (package-def &key (registry (primary-registry)))
  "Resolve dependencies (placeholder implementation)"
  (declare (ignore package-def registry))
  map:+empty+)

(defun search-packages (query &key (registry (primary-registry)))
  "Search packages (placeholder implementation)"
  (declare (ignore query registry))
  '())

(defun publish-package (package-path &key (registry (primary-registry)))
  "Publish package (placeholder implementation)"
  (declare (ignore package-path registry))
  nil)