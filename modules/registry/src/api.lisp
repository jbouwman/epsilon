;;;; epsilon.registry.api - REST API handlers
;;;;
;;;; Provides HTTP handlers for the registry API endpoints.

(in-package epsilon.registry)

;;; HTTP request helpers

(defun make-registry-request (registry path &key method body auth-token)
  "Make an HTTP request to a registry.

   REGISTRY - Registry configuration object
   PATH - API path (e.g., /packages/epsilon.core)
   METHOD - HTTP method (default :get)
   BODY - Request body (for POST/PUT)
   AUTH-TOKEN - Optional authentication token"
  (let* ((url (format nil "~A~A" (registry-url registry) path))
         (headers (append (make-auth-headers auth-token)
                          '(("Accept" . "application/json")
                            ("User-Agent" . "epsilon-registry/1.0")))))
    (http:request url
                  :method (or method :get)
                  :headers headers
                  :body body)))

(defun add-security-headers (response)
  "Add security headers to an HTTP response."
  (dolist (header +security-headers+)
    (http:add-header response (car header) (cdr header)))
  response)

;;; API endpoint handlers (for server mode)

(defun handle-list-packages (request)
  "Handle GET /api/v1/packages - List/search packages."
  (let* ((query (http:request-query-param request "q"))
         (limit (or (parse-integer (http:request-query-param request "limit")
                                   :junk-allowed t)
                    20))
         (packages (if query
                       (search-packages-internal query :limit limit)
                       (list-all-packages :limit limit))))
    (add-security-headers
     (http:json-response
      (map:make-map "packages" packages
                    "total" (length packages))))))

(defun handle-get-package (request)
  "Handle GET /api/v1/packages/:name - Get package metadata."
  (let* ((name (http:request-path-param request "name"))
         (package-info (get-package-info-internal name)))
    (if package-info
        (add-security-headers
         (http:json-response package-info))
        (add-security-headers
         (http:json-response
          (map:make-map "error" "Package not found"
                        "package" name)
          :status 404)))))

(defun handle-get-versions (request)
  "Handle GET /api/v1/packages/:name/versions - Get version list."
  (let* ((name (http:request-path-param request "name"))
         (versions (get-package-versions-internal name)))
    (if versions
        (add-security-headers
         (http:json-response
          (map:make-map "versions" versions)))
        (add-security-headers
         (http:json-response
          (map:make-map "error" "Package not found"
                        "package" name)
          :status 404)))))

(defun handle-get-version-details (request)
  "Handle GET /api/v1/packages/:name/:version - Get version details."
  (let* ((name (http:request-path-param request "name"))
         (version (http:request-path-param request "version"))
         (details (get-version-details-internal name version)))
    (if details
        (add-security-headers
         (http:json-response details))
        (add-security-headers
         (http:json-response
          (map:make-map "error" "Version not found"
                        "package" name
                        "version" version)
          :status 404)))))

(defun handle-download-archive (request)
  "Handle GET /api/v1/packages/:name/:version/archive - Download package."
  (let* ((name (http:request-path-param request "name"))
         (version (http:request-path-param request "version"))
         (archive-path (get-archive-path name version)))
    (if (and archive-path (probe-file archive-path))
        (add-security-headers
         (http:file-response archive-path
                             :content-type "application/gzip"))
        (add-security-headers
         (http:json-response
          (map:make-map "error" "Archive not found"
                        "package" name
                        "version" version)
          :status 404)))))

(defun handle-publish-package (request)
  "Handle POST /api/v1/packages - Publish new package."
  (let* ((auth-token (extract-bearer-token request))
         (body (http:request-json-body request)))
    (unless auth-token
      (return-from handle-publish-package
        (add-security-headers
         (http:json-response
          (map:make-map "error" "Authentication required")
          :status 401))))
    (handler-case
        (let ((result (publish-package-internal body auth-token)))
          (add-security-headers
           (http:json-response result :status 201)))
      (error (e)
        (add-security-headers
         (http:json-response
          (map:make-map "error" (format nil "~A" e))
          :status 400))))))

;;; Admin API endpoints (require mTLS)

(defun handle-delete-package (request)
  "Handle DELETE /api/v1/admin/packages/:name - Delete a package (admin only)."
  (handler-case
      (progn
        (require-admin-auth request)
        (let* ((name (http:request-path-param request "name")))
          (if (delete-package-internal name)
              (add-security-headers
               (http:json-response
                (map:make-map "status" "deleted"
                              "package" name)))
              (add-security-headers
               (http:json-response
                (map:make-map "error" "Package not found"
                              "package" name)
                :status 404)))))
    (admin-auth-required ()
      (add-security-headers
       (http:json-response
        (map:make-map "error" "Admin authentication required (mTLS)")
        :status 403)))))

(defun handle-delete-version (request)
  "Handle DELETE /api/v1/admin/packages/:name/:version - Delete a version (admin only)."
  (handler-case
      (progn
        (require-admin-auth request)
        (let* ((name (http:request-path-param request "name"))
               (version (http:request-path-param request "version")))
          (if (delete-version-internal name version)
              (add-security-headers
               (http:json-response
                (map:make-map "status" "deleted"
                              "package" name
                              "version" version)))
              (add-security-headers
               (http:json-response
                (map:make-map "error" "Version not found"
                              "package" name
                              "version" version)
                :status 404)))))
    (admin-auth-required ()
      (add-security-headers
       (http:json-response
        (map:make-map "error" "Admin authentication required (mTLS)")
        :status 403)))))

(defun handle-list-users (request)
  "Handle GET /api/v1/admin/users - List all users (admin only)."
  (handler-case
      (progn
        (require-admin-auth request)
        (add-security-headers
         (http:json-response
          (map:make-map "users" (list-users-internal)))))
    (admin-auth-required ()
      (add-security-headers
       (http:json-response
        (map:make-map "error" "Admin authentication required (mTLS)")
        :status 403)))))

(defun handle-revoke-token (request)
  "Handle POST /api/v1/admin/tokens/revoke - Revoke a token (admin only)."
  (handler-case
      (progn
        (require-admin-auth request)
        (let* ((body (http:request-json-body request))
               (token-id (map:get body "token_id")))
          (if token-id
              (progn
                (revoke-token-internal token-id)
                (add-security-headers
                 (http:json-response
                  (map:make-map "status" "revoked"
                                "token_id" token-id))))
              (add-security-headers
               (http:json-response
                (map:make-map "error" "token_id required")
                :status 400)))))
    (admin-auth-required ()
      (add-security-headers
       (http:json-response
        (map:make-map "error" "Admin authentication required (mTLS)")
        :status 403)))))

(defun handle-registry-stats (request)
  "Handle GET /api/v1/admin/stats - Get registry statistics (admin only)."
  (handler-case
      (progn
        (require-admin-auth request)
        (add-security-headers
         (http:json-response
          (get-registry-stats-internal))))
    (admin-auth-required ()
      (add-security-headers
       (http:json-response
        (map:make-map "error" "Admin authentication required (mTLS)")
        :status 403)))))

;;; Internal admin functions

(defun delete-package-internal (name)
  "Delete a package and all its versions from storage."
  (when *storage-directory*
    (let ((package-dir (merge-pathnames (format nil "~A/" name) *storage-directory*)))
      (when (probe-file package-dir)
        (delete-directory-recursive package-dir)
        t))))

(defun delete-version-internal (name version)
  "Delete a specific version from storage."
  (when *storage-directory*
    (let ((version-dir (merge-pathnames (format nil "~A/~A/" name version) *storage-directory*)))
      (when (probe-file version-dir)
        (delete-directory-recursive version-dir)
        t))))

(defun list-users-internal ()
  "List all users. Returns list of user info maps."
  ;; Placeholder - actual implementation depends on user storage
  nil)

(defun revoke-token-internal (token-id)
  "Revoke a specific token."
  ;; Placeholder - actual implementation depends on token storage
  (declare (ignore token-id))
  t)

(defun get-registry-stats-internal ()
  "Get registry statistics."
  (let ((package-count 0)
        (version-count 0)
        (total-size 0))
    (when *storage-directory*
      ;; Count packages and versions
      (dolist (package-dir (directory (merge-pathnames "*/" *storage-directory*)))
        (incf package-count)
        (dolist (version-dir (directory (merge-pathnames "*/" package-dir)))
          (incf version-count)
          (incf total-size (directory-size version-dir)))))
    (map:make-map "package_count" package-count
                  "version_count" version-count
                  "total_size_bytes" total-size
                  "storage_directory" (namestring *storage-directory*))))

;;; Route registration

(defun register-api-routes (server)
  "Register all API routes with the HTTP server."
  ;; Public read endpoints
  (http:route server :get "/api/v1/packages" #'handle-list-packages)
  (http:route server :get "/api/v1/packages/:name" #'handle-get-package)
  (http:route server :get "/api/v1/packages/:name/versions" #'handle-get-versions)
  (http:route server :get "/api/v1/packages/:name/:version" #'handle-get-version-details)
  (http:route server :get "/api/v1/packages/:name/:version/archive" #'handle-download-archive)
  ;; Write endpoints (require bearer token)
  (http:route server :post "/api/v1/packages" #'handle-publish-package)
  ;; Admin endpoints (require mTLS client certificate)
  (http:route server :delete "/api/v1/admin/packages/:name" #'handle-delete-package)
  (http:route server :delete "/api/v1/admin/packages/:name/:version" #'handle-delete-version)
  (http:route server :get "/api/v1/admin/users" #'handle-list-users)
  (http:route server :post "/api/v1/admin/tokens/revoke" #'handle-revoke-token)
  (http:route server :get "/api/v1/admin/stats" #'handle-registry-stats))
