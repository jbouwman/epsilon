;;;; epsilon.registry.auth - Authentication
;;;;
;;;; Provides authentication for registry operations.
;;;; Supports both Bearer token auth (write ops) and mTLS (admin ops).

(in-package epsilon.registry)

;;; Configuration

(defvar *admin-cert-fingerprints* nil
  "List of allowed admin certificate SHA-256 fingerprints.
   Only certificates with fingerprints in this list can perform admin operations.")

(defvar *require-mtls-for-admin* t
  "If true, require mTLS client certificates for admin operations.")

;;; mTLS Client Certificate Authentication

(defun add-admin-certificate (fingerprint)
  "Add a certificate fingerprint to the allowed admin list.

   FINGERPRINT - SHA-256 fingerprint of the client certificate (hex string)"
  (unless (valid-fingerprint-p fingerprint)
    (error "Invalid certificate fingerprint format"))
  (pushnew (string-downcase fingerprint) *admin-cert-fingerprints* :test #'string=))

(defun remove-admin-certificate (fingerprint)
  "Remove a certificate fingerprint from the allowed admin list."
  (setf *admin-cert-fingerprints*
        (remove (string-downcase fingerprint) *admin-cert-fingerprints* :test #'string=)))

(defun list-admin-certificates ()
  "Return the list of allowed admin certificate fingerprints."
  (copy-list *admin-cert-fingerprints*))

(defun clear-admin-certificates ()
  "Clear all admin certificate fingerprints."
  (setf *admin-cert-fingerprints* nil))

(defun valid-fingerprint-p (fingerprint)
  "Check if a fingerprint string is valid (64 hex characters)."
  (and (stringp fingerprint)
       (= (length fingerprint) 64)
       (every (lambda (c)
                (or (digit-char-p c)
                    (find c "abcdefABCDEF")))
              fingerprint)))

(defun verify-admin-certificate (request)
  "Verify that the request has a valid admin client certificate.

   REQUEST - HTTP request object

   Returns the certificate fingerprint if valid, NIL otherwise."
  (let ((client-cert (http:request-client-certificate request)))
    (when client-cert
      (let ((fingerprint (crypto:certificate-fingerprint client-cert :sha256)))
        (when (member (string-downcase fingerprint) *admin-cert-fingerprints*
                      :test #'string=)
          fingerprint)))))

(defun require-admin-auth (request)
  "Check if request has valid admin authentication.

   REQUEST - HTTP request object

   Returns T if authorized, signals ADMIN-AUTH-REQUIRED otherwise."
  (unless *require-mtls-for-admin*
    ;; mTLS not required, fall back to bearer token
    (let ((token (extract-bearer-token request)))
      (when (verify-admin-token token)
        (return-from require-admin-auth t))))
  (let ((fingerprint (verify-admin-certificate request)))
    (unless fingerprint
      (error 'admin-auth-required))
    t))

(defun verify-admin-token (token)
  "Verify if a bearer token has admin privileges.

   TOKEN - Bearer token string

   Returns T if the token has admin privileges."
  ;; Tokens starting with 'admin:' are admin tokens
  ;; In production, this should verify against a proper token store
  (and token (str:starts-with-p "admin:" token)))

(define-condition admin-auth-required (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Admin authentication required (mTLS client certificate)"))))

;;; Bearer Token Authentication

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

(defun extract-bearer-token (request)
  "Extract Bearer token from Authorization header.

   REQUEST - HTTP request object

   Returns the token string or NIL if not present."
  (let ((auth-header (http:request-header request "Authorization")))
    (when (and auth-header (str:starts-with-p "Bearer " auth-header))
      (subseq auth-header 7))))
