;;;; epsilon.registry - Secure package registry service
;;;;
;;;; This module provides package management functionality including:
;;;; - Package publishing and discovery
;;;; - Version management and dependency resolution
;;;; - Integrity verification via checksums
;;;; - Authentication and authorization

(defpackage epsilon.registry
  (:use cl)
  (:local-nicknames
   (http epsilon.http)
   (json epsilon.json)
   (crypto epsilon.crypto)
   (digest epsilon.digest)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (log epsilon.log))
  (:export
   ;; Configuration
   *registries*
   *offline-mode*
   *parallel-downloads*
   add-registry
   remove-registry
   list-registries

   ;; Package discovery
   search-packages
   get-package-info
   get-package-versions

   ;; Package operations
   download-package
   publish-package
   yank-version

   ;; Dependency resolution
   resolve-dependencies
   check-conflicts

   ;; Cache management
   is-cached-p
   cache-size
   clear-cache
   gc-cache

   ;; Integrity
   verify-checksum
   compute-checksum

   ;; Authentication
   authenticate
   set-auth-token

   ;; Admin mTLS authentication
   *admin-cert-fingerprints*
   *require-mtls-for-admin*
   add-admin-certificate
   remove-admin-certificate
   list-admin-certificates
   clear-admin-certificates

   ;; Server API
   register-api-routes))

(in-package epsilon.registry)

;;; Configuration

(defvar *registries* (map:make-map)
  "Map of registry name to registry configuration.")

(defvar *offline-mode* nil
  "When true, only use cached packages.")

(defvar *parallel-downloads* 4
  "Number of parallel package downloads.")

(defvar *cache-directory* nil
  "Directory for package cache. Defaults to ~/.epsilon/cache/")

;;; Security headers for all responses

(defparameter +security-headers+
  '(("Strict-Transport-Security" . "max-age=31536000; includeSubDomains")
    ("X-Content-Type-Options" . "nosniff")
    ("X-Frame-Options" . "DENY")
    ("Content-Security-Policy" . "default-src 'none'")
    ("X-XSS-Protection" . "1; mode=block"))
  "Security headers included in all registry responses.")

;;; Registry configuration

(defclass registry-config ()
  ((name :initarg :name :accessor registry-name
         :documentation "Registry identifier")
   (url :initarg :url :accessor registry-url
        :documentation "Base URL for registry API")
   (primary-p :initarg :primary :accessor registry-primary-p
              :initform nil
              :documentation "Whether this is the primary registry")
   (auth-token :initarg :auth-token :accessor registry-auth-token
               :initform nil
               :documentation "Authentication token")
   (mirrors :initarg :mirrors :accessor registry-mirrors
            :initform nil
            :documentation "Mirror URLs for fallback"))
  (:documentation "Configuration for a package registry."))

(defun add-registry (name url &key primary auth-token mirrors)
  "Add a new registry configuration.

   NAME - Unique identifier for the registry
   URL - Base URL for the registry API (e.g., https://packages.epsilon-lang.org/api/v1)
   PRIMARY - If true, this is the primary registry for publishing
   AUTH-TOKEN - Optional authentication token
   MIRRORS - List of mirror URLs for fallback"
  (let ((config (make-instance 'registry-config
                               :name name
                               :url url
                               :primary primary
                               :auth-token auth-token
                               :mirrors mirrors)))
    (setf *registries* (map:assoc *registries* name config))
    config))

(defun remove-registry (name)
  "Remove a registry configuration by name."
  (setf *registries* (map:dissoc *registries* name)))

(defun list-registries ()
  "List all configured registries."
  (map:vals *registries*))

(defun get-registry (name)
  "Get registry configuration by name."
  (map:get *registries* name))

(defun get-primary-registry ()
  "Get the primary registry configuration."
  (seq:find (lambda (reg) (registry-primary-p reg))
            (map:vals *registries*)))
