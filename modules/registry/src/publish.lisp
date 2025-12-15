;;;; epsilon.registry.publish - Package publishing
;;;;
;;;; Provides functionality for publishing packages to the registry.

(in-package epsilon.registry)

;;; Publishing

(defun publish-package (package-info &key registry auth-token)
  "Publish a package to the registry.

   PACKAGE-INFO - Map containing:
     - name: Package name
     - version: Package version
     - checksum: SHA-256 checksum
     - dependencies: Map of dependency constraints
     - description: Package description
     - authors: List of author strings
     - license: License identifier
     - archive-url: URL where archive was uploaded

   REGISTRY - Optional registry name (uses primary if not specified)
   AUTH-TOKEN - Authentication token

   Returns the publish result."
  (let* ((reg (or (and registry (get-registry registry))
                  (get-primary-registry)))
         (token (or auth-token (registry-auth-token reg))))
    (unless reg
      (error "No registry configured for publishing"))
    (unless token
      (error "Authentication required for publishing"))
    (validate-package-info package-info)
    (let ((response (make-registry-request reg "/packages"
                                           :method :post
                                           :body (json:encode package-info)
                                           :auth-token token)))
      (if (= (http:response-status response) 201)
          (json:decode (http:response-body response))
          (error 'publish-failed
                 :package (map:get package-info "name")
                 :version (map:get package-info "version")
                 :status (http:response-status response)
                 :message (http:response-body response))))))

(defun validate-package-info (info)
  "Validate package info before publishing."
  (let ((required-fields '("name" "version" "checksum")))
    (dolist (field required-fields)
      (unless (map:get info field)
        (error "Missing required field: ~A" field))))
  ;; Validate name format
  (let ((name (map:get info "name")))
    (unless (valid-package-name-p name)
      (error "Invalid package name: ~A" name)))
  ;; Validate version format
  (let ((version (map:get info "version")))
    (unless (parse-version version)
      (error "Invalid version format: ~A" version)))
  ;; Validate checksum format
  (let ((checksum (map:get info "checksum")))
    (unless (valid-checksum-format-p checksum)
      (error "Invalid checksum format: ~A" checksum)))
  t)

(defun valid-package-name-p (name)
  "Check if a package name is valid.
   Names must start with a letter and contain only letters, numbers, dots, and hyphens."
  (and (stringp name)
       (> (length name) 0)
       (alpha-char-p (char name 0))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\.)
                    (char= c #\-)))
              name)))

(defun valid-checksum-format-p (checksum)
  "Check if a checksum string has valid format."
  (and (stringp checksum)
       (str:starts-with-p checksum "sha256:")
       (= (length checksum) 71)))  ; sha256: + 64 hex chars

(define-condition publish-failed (error)
  ((package :initarg :package :reader publish-failed-package)
   (version :initarg :version :reader publish-failed-version)
   (status :initarg :status :reader publish-failed-status)
   (message :initarg :message :reader publish-failed-message))
  (:report (lambda (condition stream)
             (format stream "Failed to publish ~A@~A: HTTP ~A - ~A"
                     (publish-failed-package condition)
                     (publish-failed-version condition)
                     (publish-failed-status condition)
                     (publish-failed-message condition)))))

;;; Yanking (marking versions as deprecated)

(defun yank-version (name version &key registry auth-token)
  "Mark a package version as yanked (deprecated).

   NAME - Package name
   VERSION - Version to yank
   REGISTRY - Optional registry name
   AUTH-TOKEN - Authentication token

   Returns T if successful."
  (let* ((reg (or (and registry (get-registry registry))
                  (get-primary-registry)))
         (token (or auth-token (registry-auth-token reg))))
    (unless reg
      (error "No registry configured"))
    (unless token
      (error "Authentication required for yanking"))
    (let ((response (make-registry-request
                     reg (format nil "/packages/~A/~A/yank" name version)
                     :method :post
                     :auth-token token)))
      (= (http:response-status response) 200))))
