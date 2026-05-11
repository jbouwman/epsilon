;;;; epsilon.http.client-builder - Reqwest-style HTTP Client Builder
;;;;
;;;; Provides a fluent API for building HTTP clients with configuration
;;;; for connection pooling, TLS, cookies, and middleware.

(defpackage :epsilon.http.client-builder
  (:use :cl)
  (:import
   (epsilon.map map)
   (epsilon.http.connection-pool pool)
   (epsilon.crypto crypto)
   (epsilon.sys.lock lock))
  (:export
   ;; Client builder
   #:client-builder
   #:make-client-builder
   #:client
   #:build

   ;; Builder configuration
   #:with-timeout
   #:with-connect-timeout
   #:with-pool-size
   #:with-max-idle-time
   #:with-default-headers
   #:with-user-agent
   #:with-cookie-store
   #:with-tls-config
   #:with-proxy
   #:with-middleware

   ;; Built client
   #:http-client
   #:http-client-p
   #:http-client-pool
   #:http-client-default-headers
   #:http-client-timeout
   #:http-client-middleware

   ;; Cookie store
   #:cookie-store
   #:make-cookie-store
   #:cookie-store-cookies
   #:cookie-store-get
   #:cookie-store-set
   #:cookie-store-clear
   #:cookie-store-for-request
   #:cookie-store-remove-expired
   #:parse-set-cookie
   #:cookie-entry
   #:make-cookie-entry
   #:cookie-entry-name
   #:cookie-entry-value
   #:cookie-entry-domain
   #:cookie-entry-path
   #:cookie-entry-expires
   #:cookie-entry-http-only
   #:cookie-entry-secure

   ;; TLS configuration
   #:tls-config
   #:make-tls-config
   #:tls-config-verify
   #:tls-config-cert-file
   #:tls-config-key-file
   #:tls-config-ca-file

   ;; Proxy configuration
   #:proxy-config
   #:make-proxy-config
   #:proxy-config-url
   #:proxy-config-username
   #:proxy-config-password))

;;;; Cookie Store (RFC 6265)

(defstruct cookie-entry
  "A stored cookie with all RFC 6265 attributes"
  (name "" :type string)
  (value "" :type string)
  (domain "" :type string)
  (path "/" :type string)
  (expires nil :type (or null integer))   ; Universal time, nil = session cookie
  (http-only nil :type boolean)
  (secure nil :type boolean)
  (creation-time 0 :type integer))

(defstruct cookie-store
  "HTTP cookie storage with domain-path indexing"
  (cookies (make-hash-table :test 'equal) :type hash-table)
  (lock (lock:make-lock "cookie-store") :type lock:lock))

(defun cookie-key (domain path name)
  "Generate unique key for cookie storage"
  (format nil "~A|~A|~A" (string-downcase domain) path name))

(defun cookie-store-get (store domain path name)
  "Get a cookie value from the store"
  (lock:with-lock ((cookie-store-lock store))
    (let ((entry (gethash (cookie-key domain path name) (cookie-store-cookies store))))
      (when entry
        (cookie-entry-value entry)))))

(defun cookie-store-set (store domain path name value &key expires http-only secure)
  "Store a cookie with all RFC 6265 attributes.
   EXPIRES is a universal time or nil for session cookies."
  (lock:with-lock ((cookie-store-lock store))
    (setf (gethash (cookie-key domain path name) (cookie-store-cookies store))
          (make-cookie-entry
           :name name
           :value value
           :domain (string-downcase domain)
           :path (or path "/")
           :expires expires
           :http-only http-only
           :secure secure
           :creation-time (get-universal-time)))))

(defun cookie-store-clear (store)
  "Clear all cookies from the store"
  (lock:with-lock ((cookie-store-lock store))
    (clrhash (cookie-store-cookies store))))

(defun cookie-store-remove-expired (store)
  "Remove all expired cookies from the store"
  (lock:with-lock ((cookie-store-lock store))
    (let ((now (get-universal-time))
          (keys-to-remove '()))
      (maphash (lambda (key entry)
                 (when (and (cookie-entry-expires entry)
                            (< (cookie-entry-expires entry) now))
                   (push key keys-to-remove)))
               (cookie-store-cookies store))
      (dolist (key keys-to-remove)
        (remhash key (cookie-store-cookies store)))
      (length keys-to-remove))))

(defun domain-matches-p (cookie-domain request-domain)
  "Check if COOKIE-DOMAIN matches REQUEST-DOMAIN per RFC 6265 Section 5.1.3.
   Domain matching: cookie domain '.example.com' matches 'www.example.com'."
  (let ((cd (string-downcase cookie-domain))
        (rd (string-downcase request-domain)))
    (cond
      ;; Exact match
      ((string= cd rd) t)
      ;; Cookie domain is a suffix of request domain
      ;; e.g., cookie for '.example.com' matches 'sub.example.com'
      ((and (> (length rd) (length cd))
            (let ((suffix-start (- (length rd) (length cd))))
              (and (string= cd (subseq rd suffix-start))
                   ;; Must be preceded by a dot in the request domain
                   (char= (char rd (1- suffix-start)) #\.))))
       t)
      ;; Cookie domain with leading dot stripped
      ((and (> (length cd) 0)
            (char= (char cd 0) #\.)
            (domain-matches-p (subseq cd 1) rd))
       t)
      (t nil))))

(defun path-matches-p (cookie-path request-path)
  "Check if COOKIE-PATH matches REQUEST-PATH per RFC 6265 Section 5.1.4."
  (cond
    ;; Exact match
    ((string= cookie-path request-path) t)
    ;; Cookie path is a prefix of request path
    ((and (> (length request-path) (length cookie-path))
          (string= cookie-path (subseq request-path 0 (length cookie-path)))
          ;; Cookie path ends with / or next char in request is /
          (or (char= (char cookie-path (1- (length cookie-path))) #\/)
              (char= (char request-path (length cookie-path)) #\/)))
     t)
    (t nil)))

(defun parse-url-components (url)
  "Extract scheme, domain, and path from URL for cookie matching."
  (let* ((scheme-end (search "://" url))
         (scheme (if scheme-end (subseq url 0 scheme-end) "http"))
         (authority-start (if scheme-end (+ scheme-end 3) 0))
         (path-start (position #\/ url :start authority-start))
         (host (subseq url authority-start (or path-start (length url))))
         ;; Strip port from host
         (colon-pos (position #\: host))
         (domain (if colon-pos (subseq host 0 colon-pos) host))
         (path (if path-start
                   (let ((query-pos (position #\? url :start path-start)))
                     (subseq url path-start (or query-pos (length url))))
                   "/")))
    (values scheme domain path)))

(defun cookie-store-for-request (store url)
  "Get all cookies applicable for URL per RFC 6265 domain/path matching.
   Returns an alist of (name . value) pairs."
  (multiple-value-bind (scheme domain path) (parse-url-components url)
    (let ((secure-p (string-equal scheme "https"))
          (now (get-universal-time))
          (matching-cookies '()))
      (lock:with-lock ((cookie-store-lock store))
        (maphash (lambda (key entry)
                   (declare (ignore key))
                   ;; Collect cookie if it passes all matching criteria
                   (when (and
                          ;; Not expired
                          (or (null (cookie-entry-expires entry))
                              (>= (cookie-entry-expires entry) now))
                          ;; Secure cookies only on HTTPS
                          (or (not (cookie-entry-secure entry)) secure-p)
                          ;; Domain match
                          (domain-matches-p (cookie-entry-domain entry) domain)
                          ;; Path match
                          (path-matches-p (cookie-entry-path entry) path))
                     (push (cons (cookie-entry-name entry)
                                 (cookie-entry-value entry))
                           matching-cookies)))
                 (cookie-store-cookies store)))
      ;; Sort by path length (longest first) then by creation time
      (sort matching-cookies
            (lambda (a b)
              (let ((ea (cookie-store-find-entry store (car a)))
                    (eb (cookie-store-find-entry store (car b))))
                (if (and ea eb)
                    (or (> (length (cookie-entry-path ea))
                            (length (cookie-entry-path eb)))
                        (and (= (length (cookie-entry-path ea))
                                (length (cookie-entry-path eb)))
                             (< (cookie-entry-creation-time ea)
                                (cookie-entry-creation-time eb))))
                    nil)))))))

(defun cookie-store-find-entry (store name)
  "Find first cookie entry by name. Internal helper."
  (lock:with-lock ((cookie-store-lock store))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (when (string= (cookie-entry-name entry) name)
                 (return-from cookie-store-find-entry entry)))
             (cookie-store-cookies store)))
  nil)

(defun parse-set-cookie (header-value)
  "Parse a Set-Cookie header value into components.
   Returns (values name value domain path expires http-only secure)."
  (let ((parts (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                       (loop with start = 0
                             for pos = (position #\; header-value :start start)
                             collect (subseq header-value start (or pos (length header-value)))
                             while pos
                             do (setf start (1+ pos)))))
        (name nil) (value nil) (domain nil) (path nil)
        (expires nil) (http-only nil) (secure nil))
    ;; First part is name=value
    (when parts
      (let* ((nv (first parts))
             (eq-pos (position #\= nv)))
        (when eq-pos
          (setf name (subseq nv 0 eq-pos))
          (setf value (subseq nv (1+ eq-pos)))))
      ;; Remaining parts are attributes
      (dolist (attr (rest parts))
        (let ((eq-pos (position #\= attr)))
          (if eq-pos
              (let ((attr-name (string-downcase (subseq attr 0 eq-pos)))
                    (attr-value (subseq attr (1+ eq-pos))))
                (cond
                  ((string= attr-name "domain") (setf domain attr-value))
                  ((string= attr-name "path") (setf path attr-value))
                  ((string= attr-name "max-age")
                   (let ((seconds (ignore-errors (parse-integer attr-value))))
                     (when seconds
                       (setf expires (+ (get-universal-time) seconds)))))
                  ;; expires header parsing omitted -- max-age takes priority
                  ))
              (let ((attr-name (string-downcase attr)))
                (cond
                  ((string= attr-name "httponly") (setf http-only t))
                  ((string= attr-name "secure") (setf secure t))))))))
    (values name value domain path expires http-only secure)))

;;;; TLS Configuration

(defstruct tls-config
  "TLS/SSL configuration"
  (verify t :type boolean)              ; Verify server certificate
  (cert-file nil :type (or null string)) ; Client certificate file
  (key-file nil :type (or null string))  ; Client private key file
  (ca-file nil :type (or null string))   ; CA certificate file
  (min-version nil :type (or null keyword)) ; Minimum TLS version (:tls1.2, :tls1.3)
  (ciphers nil :type (or null string)))  ; Cipher suites

(defun create-tls-context (config)
  "Create a TLS context from configuration"
  (let ((verify-mode (if (tls-config-verify config)
                         crypto:+verify-peer+
                         crypto:+verify-none+)))
    (if (and (tls-config-cert-file config)
             (tls-config-key-file config))
        ;; Mutual TLS
        (crypto:make-mtls-client-context
         :cert-file (tls-config-cert-file config)
         :key-file (tls-config-key-file config)
         :ca-file (tls-config-ca-file config))
        ;; Standard TLS
        (crypto:make-client-context
         :ca-file (tls-config-ca-file config)
         :verify-mode verify-mode))))

;;;; Proxy Configuration

(defstruct proxy-config
  "HTTP proxy configuration"
  (url nil :type (or null string))       ; Proxy URL (http://host:port)
  (username nil :type (or null string))  ; Proxy authentication
  (password nil :type (or null string))
  (no-proxy nil :type list))             ; Hosts to bypass proxy

;;;; Client Builder

(defstruct client-builder
  "Builder for HTTP client configuration"
  ;; Timeouts (in seconds)
  (timeout 30 :type integer)
  (connect-timeout 30 :type integer)
  (read-timeout 30 :type integer)

  ;; Connection pool
  (pool-size 10 :type integer)
  (max-idle-time 300 :type integer)
  (pool-per-host nil :type boolean)

  ;; Headers
  (default-headers nil :type list)       ; Alist of (name . value)
  (user-agent "epsilon-http/2.0" :type string)

  ;; Features
  (cookie-store nil :type (or null cookie-store))
  (tls-config nil :type (or null tls-config))
  (proxy nil :type (or null proxy-config))

  ;; Middleware
  (middleware nil :type list)

  ;; Behavior
  (follow-redirects t :type boolean)
  (max-redirects 10 :type integer))

;;;; Built HTTP Client

(defstruct http-client
  "Configured HTTP client instance"
  (pool nil :type (or null pool:connection-pool))
  (default-headers nil :type list)
  (user-agent "epsilon-http/2.0" :type string)
  (timeout 30 :type integer)
  (connect-timeout 30 :type integer)
  (cookie-store nil :type (or null cookie-store))
  (tls-config nil :type (or null tls-config))
  (proxy nil :type (or null proxy-config))
  (middleware nil :type list)
  (follow-redirects t :type boolean)
  (max-redirects 10 :type integer))

;;;; Builder Creation

(defun client ()
  "Create a new client builder with default settings"
  (make-client-builder))

;;;; Builder Methods (Fluent API)

(defun with-timeout (builder seconds)
  "Set request timeout"
  (setf (client-builder-timeout builder) seconds)
  builder)

(defun with-connect-timeout (builder seconds)
  "Set connection timeout"
  (setf (client-builder-connect-timeout builder) seconds)
  builder)

(defun with-pool-size (builder size)
  "Set connection pool size per host"
  (setf (client-builder-pool-size builder) size)
  builder)

(defun with-max-idle-time (builder seconds)
  "Set maximum idle time for pooled connections"
  (setf (client-builder-max-idle-time builder) seconds)
  builder)

(defun with-default-headers (builder headers)
  "Set default headers for all requests.
   HEADERS should be an alist of (name . value) pairs."
  (setf (client-builder-default-headers builder) headers)
  builder)

(defun with-user-agent (builder user-agent)
  "Set the User-Agent header"
  (setf (client-builder-user-agent builder) user-agent)
  builder)

(defun with-cookie-store (builder &optional store)
  "Enable cookie storage. If STORE is nil, creates a new store."
  (setf (client-builder-cookie-store builder)
        (or store (make-cookie-store)))
  builder)

(defun with-tls-config (builder config)
  "Set TLS configuration"
  (setf (client-builder-tls-config builder) config)
  builder)

(defun with-proxy (builder proxy-url &key username password no-proxy)
  "Configure HTTP proxy"
  (setf (client-builder-proxy builder)
        (make-proxy-config
         :url proxy-url
         :username username
         :password password
         :no-proxy no-proxy))
  builder)

(defun with-middleware (builder middleware)
  "Add middleware to the chain.
   MIDDLEWARE should be a function (request next) -> response"
  (push middleware (client-builder-middleware builder))
  builder)

;;;; Build Client

(defun build (builder)
  "Build the configured HTTP client"
  (let ((pool (pool:make-connection-pool
               :max-size (client-builder-pool-size builder)
               :max-idle-time (client-builder-max-idle-time builder)
               :connection-timeout (client-builder-connect-timeout builder))))
    (make-http-client
     :pool pool
     :default-headers (append
                       (list (cons "User-Agent" (client-builder-user-agent builder)))
                       (client-builder-default-headers builder))
     :user-agent (client-builder-user-agent builder)
     :timeout (client-builder-timeout builder)
     :connect-timeout (client-builder-connect-timeout builder)
     :cookie-store (client-builder-cookie-store builder)
     :tls-config (client-builder-tls-config builder)
     :proxy (client-builder-proxy builder)
     :middleware (nreverse (client-builder-middleware builder))
     :follow-redirects (client-builder-follow-redirects builder)
     :max-redirects (client-builder-max-redirects builder))))

;;;; Convenience Functions

(defun default-client ()
  "Create a default HTTP client with sensible defaults"
  (build (client)))
