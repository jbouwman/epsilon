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

(defun extract-bearer-token (request)
  "Extract Bearer token from Authorization header."
  (let ((auth-header (http:request-header request "Authorization")))
    (when (and auth-header (str:starts-with-p "Bearer " auth-header))
      (subseq auth-header 7))))

;;; Route registration

(defun register-api-routes (server)
  "Register all API routes with the HTTP server."
  (http:route server :get "/api/v1/packages" #'handle-list-packages)
  (http:route server :post "/api/v1/packages" #'handle-publish-package)
  (http:route server :get "/api/v1/packages/:name" #'handle-get-package)
  (http:route server :get "/api/v1/packages/:name/versions" #'handle-get-versions)
  (http:route server :get "/api/v1/packages/:name/:version" #'handle-get-version-details)
  (http:route server :get "/api/v1/packages/:name/:version/archive" #'handle-download-archive))
