;;;; epsilon.registry.auth - Authentication
;;;;
;;;; Provides authentication for registry operations.

(in-package epsilon.registry)

;;; Authentication

(defun set-auth-token (registry-name token)
  "Set authentication token for a registry.

   REGISTRY-NAME - Name of the configured registry
   TOKEN - Bearer token for authentication"
  (let ((registry (get-registry registry-name)))
    (unless registry
      (error "Registry not found: ~A" registry-name))
    (setf (registry-auth-token registry) token)))

(defun authenticate (registry-name &optional token)
  "Authenticate with a registry.

   REGISTRY-NAME - Name of the registry to authenticate with
   TOKEN - Optional token (uses stored token if not provided)

   Returns T if authentication succeeds."
  (let* ((registry (get-registry registry-name))
         (auth-token (or token (registry-auth-token registry))))
    (unless registry
      (error "Registry not found: ~A" registry-name))
    (unless auth-token
      (error "No authentication token provided for registry: ~A" registry-name))
    ;; Verify token by making an authenticated request
    (let ((response (make-registry-request registry "/auth/verify"
                                           :auth-token auth-token)))
      (= (http:response-status response) 200))))

(defun make-auth-headers (auth-token)
  "Create HTTP headers for authenticated requests."
  (when auth-token
    (list (cons "Authorization" (format nil "Bearer ~A" auth-token)))))

(defun get-token-from-env (env-var)
  "Get authentication token from environment variable."
  (let ((token (epsilon.sys.env:getenv env-var)))
    (when (and token (> (length token) 0))
      token)))
