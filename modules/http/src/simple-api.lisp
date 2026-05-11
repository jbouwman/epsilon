;;;; epsilon.http.simple - Simple High-Level HTTP API
;;;;
;;;; Provides a simplified interface for common HTTP operations

(defpackage epsilon.http.simple
  (:use :cl)
  (:shadow #:get #:delete)
  (:import (epsilon.http.client client)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.http.cache http-cache)
            (epsilon.map map)
            (epsilon.json json)
            (epsilon.string str)
            (epsilon.compression compress)))

;;;; Configuration Parameters

(defparameter *default-timeout* 30
  "Default timeout in seconds for HTTP requests")

(defparameter *default-user-agent* "epsilon-http/1.0"
  "Default User-Agent header value")

(defparameter *follow-redirects* t
  "Whether to automatically follow redirects")

(defparameter *verify-ssl* t
  "Whether to verify SSL certificates")

(defparameter *max-redirects* 5
  "Maximum number of redirects to follow")

(defparameter *accept-encoding* "gzip, deflate"
  "Default Accept-Encoding header value. Set to nil to disable.
   Brotli (br) and zstd are added automatically when their libraries are available.")

(defparameter *default-cache* nil
  "Default HTTP cache used by the request function when no :cache argument
   is provided. Set via enable-default-cache / disable-default-cache.")

(defparameter *session-headers* nil
  "Session-scoped default headers, bound by WITH-SESSION.
   These are merged (without overwriting) into every request's headers.")

(defparameter *session-cookies* nil
  "Session-scoped cookie jar, bound by WITH-SESSION.
   An alist of (name . value) pairs sent as Cookie headers.")

(defun enable-default-cache (&key (max-entries 1000) (max-bytes (* 50 1024 1024)))
  "Enable the default HTTP response cache.

   Arguments:
     MAX-ENTRIES - Maximum cached responses (default 1000)
     MAX-BYTES   - Maximum cache size in bytes (default 50 MB)

   Returns: The newly created cache instance"
  (setf *default-cache* (http-cache:make-http-cache
                         :max-entries max-entries
                         :max-bytes max-bytes)))

(defun disable-default-cache ()
  "Disable and discard the default HTTP response cache.
   Returns NIL."
  (setf *default-cache* nil))

;;;; Helper Functions

(defun effective-accept-encoding ()
  "Build the Accept-Encoding header value, including optional codecs.
   Returns nil if *accept-encoding* is nil (compression disabled)."
  (when *accept-encoding*
    (let ((encodings (list *accept-encoding*)))
      ;; Add brotli if available and not already listed
      (when (and (not (search "br" *accept-encoding*))
                 (ignore-errors (compress:brotli-available-p)))
        (push "br" encodings))
      ;; Add zstd if available and not already listed
      (when (and (not (search "zstd" *accept-encoding*))
                 (ignore-errors (compress:zstd-available-p)))
        (push "zstd" encodings))
      (format nil "~{~A~^, ~}" (nreverse encodings)))))

(defun prepare-headers (headers user-agent)
  "Prepare headers map with defaults, session headers, and cookies.
   Returns a plain epsilon.map. Lookups for the well-known headers below
   are done case-insensitively so the wrapper isn't required."
  (let ((h (or headers (map:make-map))))
    ;; Merge session headers (without overwriting explicit headers)
    (when *session-headers*
      (map:each (lambda (k v)
                  (unless (map:get h k)
                    (setf h (map:assoc h k v))))
                *session-headers*))
    ;; Add session cookies
    (when *session-cookies*
      (unless (or (map:get h "Cookie") (map:get h "cookie"))
        (let ((cookie-str (str:join "; "
                                    (mapcar (lambda (pair)
                                              (format nil "~A=~A" (car pair) (cdr pair)))
                                            *session-cookies*))))
          (setf h (map:assoc h "Cookie" cookie-str)))))
    ;; Add User-Agent if not present
    (unless (or (map:get h "User-Agent") (map:get h "user-agent"))
      (setf h (map:assoc h "User-Agent" user-agent)))
    ;; Add Accept-Encoding if not present and compression is enabled
    (unless (or (map:get h "Accept-Encoding") (map:get h "accept-encoding"))
      (let ((encoding (effective-accept-encoding)))
        (when encoding
          (setf h (map:assoc h "Accept-Encoding" encoding)))))
    h))

(defun prepare-json-body (data)
  "Convert Lisp data to JSON string"
  (cond
    ((stringp data) data)
    ((null data) nil)
    (t (json:encode-to-string data))))

(defun prepare-form-body (data)
  "Convert plist or map to URL-encoded form data"
  (cond
    ((stringp data) data)
    ((null data) nil)
    ((listp data)
     ;; Convert plist to URL-encoded string
     (str:join "&"
              (loop for (key value) on data by #'cddr
                    collect (format nil "~A=~A"
                                   (request:url-encode (string key))
                                   (request:url-encode (princ-to-string value))))))
    (t (error "Invalid form data: ~A" data))))

(defun handle-response (response)
  "Process HTTP response and handle errors"
  (when (null response)
    (error "No response received"))
  response)

(defun resolve-redirect-url (base-url location)
  "Resolve a redirect Location header against the base URL.
   Handles both absolute URLs and relative paths."
  (cond
    ;; Already absolute URL (has scheme)
    ((search "://" location)
     location)
    ;; Protocol-relative URL (//host/path)
    ((and (>= (length location) 2)
          (char= (char location 0) #\/)
          (char= (char location 1) #\/))
     (let ((scheme-end (search "://" base-url)))
       (if scheme-end
           (concatenate 'string (subseq base-url 0 scheme-end) ":" location)
           (concatenate 'string "http:" location))))
    ;; Absolute path (/path)
    ((and (> (length location) 0)
          (char= (char location 0) #\/))
     (let* ((scheme-end (search "://" base-url))
            (authority-start (if scheme-end (+ scheme-end 3) 0))
            (path-start (position #\/ base-url :start authority-start)))
       (if path-start
           (concatenate 'string (subseq base-url 0 path-start) location)
           (concatenate 'string base-url location))))
    ;; Relative path (path or ./path or ../path)
    (t
     (let* ((scheme-end (search "://" base-url))
            (authority-start (if scheme-end (+ scheme-end 3) 0))
            (path-start (position #\/ base-url :start authority-start))
            (last-slash (when path-start
                          (position #\/ base-url :from-end t :start path-start))))
       (if last-slash
           (concatenate 'string (subseq base-url 0 (1+ last-slash)) location)
           (concatenate 'string base-url "/" location))))))

(defun follow-redirect (base-url method headers body redirect-count verify-ssl)
  "Follow HTTP redirect"
  (when (>= redirect-count *max-redirects*)
    (error "Too many redirects (~D)" redirect-count))

  ;; verify-ssl: NIL disables, T uses system default, string is CA file path
  (let ((response (cond
                    ((null verify-ssl)
                     (client:request base-url :method method :headers headers :body body
                                     :ca-file nil))
                    ((stringp verify-ssl)
                     (client:request base-url :method method :headers headers :body body
                                     :ca-file verify-ssl))
                    (t
                     (client:request base-url :method method :headers headers :body body)))))
    (if (and *follow-redirects*
             (member (response:response-status response) '(301 302 303 307 308)))
        (let ((location (response:response-header response "Location")))
          (if location
              ;; Resolve relative URLs against base URL and follow redirect
              (let ((resolved-url (resolve-redirect-url base-url location)))
                (follow-redirect resolved-url
                                (if (= (response:response-status response) 303) "GET" method)
                                headers
                                (if (= (response:response-status response) 303) nil body)
                                (1+ redirect-count)
                                verify-ssl))
              response))
        response)))

;;;; Main Request Function

(defun request (url &key
                    (method :get)
                    headers
                    body
                    json
                    form
                    (timeout *default-timeout*)
                    (user-agent *default-user-agent*)
                    (follow-redirects *follow-redirects*)
                    (verify-ssl *verify-ssl*)
                    protocol
                    proxy
                    (cache *default-cache*))
  "Make an HTTP request with automatic handling of common patterns.

   Arguments:
   - url: Target URL
   - method: HTTP method (:get :post :put :patch :delete :head :options)
   - headers: Additional headers as plist or map
   - body: Raw body string
   - json: Data to send as JSON (will be encoded)
   - form: Data to send as form-encoded
   - timeout: Request timeout in seconds
   - user-agent: User-Agent header value
   - follow-redirects: Whether to follow redirects
   - verify-ssl: Whether to verify SSL certificates (ignored for H3)
   - protocol: Force specific protocol (:h3 for HTTP/3, nil for automatic)
   - proxy: Proxy URL string (e.g. \"http://proxy.example.com:8080\")
   - cache: HTTP cache instance (default *default-cache*). When non-nil,
     responses are cached and conditional requests are used for revalidation.

   Returns: Response object

   Examples:
   (request \"https://api.example.com/data\")
   (request \"https://api.example.com/users\" :method :post :json '(:name \"John\"))
   (request \"https://example.com/form\" :method :post :form '(:field1 \"value1\"))
   (request \"https://example.com\" :protocol :h3)  ; Force HTTP/3
   (request \"https://example.com\" :proxy \"http://proxy:8080\")
   (request \"https://example.com\" :cache (http-cache:make-http-cache))"
  (let* ((method-str (string-upcase (string method)))
         (cache-key (when cache (http-cache:make-cache-key method-str url)))
         (headers-prepared (prepare-headers headers user-agent))
         (body-prepared (cond
                          (json
                           (progn
                             (setf headers-prepared
                                   (map:assoc headers-prepared "Content-Type"
                                              "application/json"))
                             (prepare-json-body json)))
                          (form
                           (progn
                             (setf headers-prepared
                                   (map:assoc headers-prepared "Content-Type"
                                              "application/x-www-form-urlencoded"))
                             (prepare-form-body form)))
                          (t body))))

    ;; Check cache for a fresh hit before sending
    (when cache
      (multiple-value-bind (entry hit-p) (http-cache:cache-get cache cache-key)
        (when hit-p
          ;; Fresh cache hit -- reconstruct and return response immediately
          (return-from request
            (response:make-response
             :status (http-cache:cache-entry-status-code entry)
             :headers (http-cache:cache-entry-response-headers entry)
             :body (http-cache:cache-entry-response-body entry)))))

      ;; No fresh hit -- add conditional headers for revalidation
      (let ((cond-headers (http-cache:conditional-headers-for cache cache-key)))
        (dolist (pair cond-headers)
          (setf headers-prepared
                (map:assoc headers-prepared (car pair) (cdr pair))))))

    ;; Parse proxy URL if provided
    (multiple-value-bind (proxy-host proxy-port)
        (when proxy (parse-proxy-url proxy))

      ;; Route to appropriate protocol implementation
      (let ((response
              (cond
                ;; Explicit HTTP/3 requested
                ((eq protocol :h3)
                 (make-h3-request url method-str headers-prepared body-prepared timeout))

                ;; Default: use HTTP/1.1 or HTTP/2
                (t
                 (handle-response
                  (if follow-redirects
                      (follow-redirect url method-str headers-prepared body-prepared 0 verify-ssl)
                      (let ((proxy-args (when proxy-host
                                          (list :proxy-host proxy-host :proxy-port proxy-port)))
                            (ssl-args (cond
                                        ((null verify-ssl) (list :ca-file nil))
                                        ((stringp verify-ssl) (list :ca-file verify-ssl))
                                        (t nil))))
                        (apply #'client:request url
                               :method method-str
                               :headers headers-prepared
                               :body body-prepared
                               (append ssl-args proxy-args)))))))))

        ;; Post-response cache handling
        (when (and cache response)
          (let ((status (response:response-status response))
                (resp-headers (response:response-headers response)))
            (cond
              ;; 304 Not Modified -- refresh cached entry and return cached body
              ((= status 304)
               (let ((cached-body (http-cache:handle-304-response
                                   cache cache-key resp-headers)))
                 (when cached-body
                   (return-from request
                     (response:make-response
                      :status 200
                      :headers resp-headers
                      :body cached-body)))))

              ;; Cacheable response -- store it. Header keys are lowercase
              ;; per the parser's RFC 7230 normalization.
              ((http-cache:cacheable-response-p resp-headers status)
               (let* ((cc-header (map:get resp-headers "cache-control"))
                      (directives (when cc-header
                                    (http-cache:parse-cache-control cc-header)))
                      (max-age (cdr (assoc :max-age directives)))
                      (etag (map:get resp-headers "etag"))
                      (last-modified (map:get resp-headers "last-modified")))
                 (http-cache:cache-put cache cache-key
                                       (response:response-body response)
                                       resp-headers
                                       status
                                       :etag etag
                                       :last-modified last-modified
                                       :max-age max-age))))))

        response))))

(defun make-h3-request (url method headers body timeout)
  "Make HTTP/3 request and convert response to standard format.
   Requires epsilon.http3 module to be loaded."
  (let ((h3-pkg (find-package :epsilon.http3)))
    ;; Check if H3 is available
    (unless (and h3-pkg
                 (ignore-errors (funcall (find-symbol "HTTP3-AVAILABLE-P" h3-pkg))))
      (error "HTTP/3 not available. Load epsilon.http3 module and ensure ngtcp2/nghttp3 libraries are installed."))

    ;; Convert headers from map to alist for H3
    (let ((headers-alist (when headers
                           (map:to-alist headers))))
      ;; Make H3 request
      (let ((h3-response (funcall (find-symbol "HTTP3-REQUEST" h3-pkg) url
                                  :method method
                                  :headers headers-alist
                                  :body body
                                  :timeout timeout)))
        ;; Convert H3 response to standard response format
        (let ((status (getf h3-response :status))
              (resp-headers (getf h3-response :headers))
              (resp-body (getf h3-response :body)))
          ;; Keep body as bytes for consistency with HTTP/1.1 path
          (let ((body-bytes (typecase resp-body
                              ((array (unsigned-byte 8) (*)) resp-body)
                              (string (sb-ext:string-to-octets resp-body :external-format :utf-8))
                              (t (when resp-body
                                   (sb-ext:string-to-octets (princ-to-string resp-body)
                                                            :external-format :utf-8))))))
            ;; Create response object matching epsilon.http.response format
            (response:make-response
             :status status
             :headers (map:from-pairs resp-headers)
             :body body-bytes)))))))

(defun parse-proxy-url (proxy-url)
  "Parse a proxy URL like 'http://host:port' into (values host port).
   Returns nil if PROXY-URL is nil."
  (when proxy-url
    (multiple-value-bind (host port)
        (extract-host-port-from-url proxy-url)
      (values host (or port 8080)))))

(defun extract-host-port-from-url (url)
  "Extract host and port from URL. Returns (host port)."
  (let* ((scheme-end (search "://" url))
         (authority-start (if scheme-end (+ scheme-end 3) 0))
         (path-start (position #\/ url :start authority-start))
         (authority (if path-start
                        (subseq url authority-start path-start)
                        (subseq url authority-start)))
         (colon-pos (position #\: authority)))
    (if colon-pos
        (values (subseq authority 0 colon-pos)
                (parse-integer (subseq authority (1+ colon-pos)) :junk-allowed t))
        (values authority
                (if (and scheme-end (string-equal "https" (subseq url 0 scheme-end)))
                    443
                    80)))))

(defun record-alt-svc-from-response (url response)
  "Check response for Alt-Svc header and record if present."
  (let ((alt-svc (map:get (response:response-headers response) "alt-svc")))
    (when alt-svc
      (multiple-value-bind (host port) (extract-host-port-from-url url)
        (let ((h3-pkg (find-package :epsilon.http3)))
          (when h3-pkg
            (let ((record-fn (find-symbol "RECORD-ALT-SVC" h3-pkg)))
              (when record-fn
                (ignore-errors (funcall record-fn host port alt-svc))))))))))

;;;; Simple One-Liner Functions

(defun get (url &rest options)
  "Make a GET request.

   Examples:
   (get \"https://api.example.com/data\")
   (get \"https://api.example.com/data\" :headers '(\"Authorization\" \"Bearer token\"))"
  (apply #'request url :method :get options))

(defun post (url &rest options)
  "Make a POST request.

   Examples:
   (post \"https://api.example.com/users\" :json '(:name \"John\" :age 30))
   (post \"https://api.example.com/form\" :form '(:field1 \"value1\"))"
  (apply #'request url :method :post options))

(defun put (url &rest options)
  "Make a PUT request.

   Examples:
   (put \"https://api.example.com/users/1\" :json '(:name \"Jane\"))"
  (apply #'request url :method :put options))

(defun patch (url &rest options)
  "Make a PATCH request.

   Examples:
   (patch \"https://api.example.com/users/1\" :json '(:age 31))"
  (apply #'request url :method :patch options))

(defun delete (url &rest options)
  "Make a DELETE request.

   Examples:
   (delete \"https://api.example.com/users/1\")"
  (apply #'request url :method :delete options))

(defun head (url &rest options)
  "Make a HEAD request.

   Examples:
   (head \"https://example.com/large-file.pdf\")"
  (apply #'request url :method :head options))

(defun options (url &rest options)
  "Make an OPTIONS request.

   Examples:
   (options \"https://api.example.com/users\")"
  (apply #'request url :method :options options))

;;;; Response Helper Functions

(defun response-ok-p (response)
  "Check if response status is successful (2xx)"
  (let ((status (response:response-status response)))
    (and (>= status 200) (< status 300))))

(defun response-json (response)
  "Parse response body as JSON"
  (let ((body (response:response-body-string response)))
    (when (and body (not (str:empty-p body)))
      (json:parse body))))

(defun response-text (response)
  "Get response body as text"
  (response:response-body-string response))

(defun response-headers (response)
  "Get response headers as map"
  (response:response-headers response))

(defun response-status (response)
  "Get response status code"
  (response:response-status response))

(defun response-header (response header-name)
  "Get specific response header value (case-insensitive on string keys)."
  (let ((hs (response:response-headers response)))
    (or (map:get hs header-name)
        (and (stringp header-name)
             (map:get hs (string-downcase header-name))))))

;;;; Session Management

(defmacro with-session ((session-var &key cookies headers) &body body)
  "Execute requests with shared session state (cookies, headers).
   All requests within BODY automatically include the session headers
   and cookies. Explicit per-request headers take precedence.

   Arguments:
     SESSION-VAR - Bound to a session plist for introspection (unused currently)
     COOKIES - Alist of (name . value) cookie pairs
     HEADERS - A map of default headers for all requests in this session

   Example:
   (with-session (session :headers (map:make-map \"Authorization\" \"Bearer token\")
                          :cookies '((\"sid\" . \"abc123\")))
     (get \"https://api.example.com/profile\")
     (post \"https://api.example.com/data\" :json data))"
  `(let ((*session-headers* ,headers)
         (*session-cookies* ,cookies)
         (,session-var (list :headers ,headers :cookies ,cookies)))
     (declare (ignorable ,session-var))
     ,@body))

;;;; File Operations

(defun download-file (url filepath &key headers (timeout 300))
  "Download file from URL to local filesystem.

   Example:
   (download-file \"https://example.com/file.pdf\" \"/tmp/file.pdf\")"
  (let ((response (request url :headers headers :timeout timeout)))
    (when (response-ok-p response)
      (let ((bytes (response:response-body-bytes response)))
        (when bytes
          (with-open-file (stream filepath
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
            (write-sequence bytes stream))))
      filepath)))

(defun upload-file (url filepath &key
                                  (field-name "file")
                                  additional-fields
                                  headers)
  "Upload file to URL using multipart/form-data.

   Example:
   (upload-file \"https://api.example.com/upload\" \"/tmp/document.pdf\"
                :field-name \"document\"
                :additional-fields '(:description \"Important document\"))"

  ;; Read file
  (let* ((filename (file-namestring filepath))
         (file-data (with-open-file (stream filepath
                                           :element-type '(unsigned-byte 8))
                     (let ((bytes (make-array (file-length stream)
                                            :element-type '(unsigned-byte 8))))
                       (read-sequence bytes stream)
                       (map 'string #'code-char bytes))))
         (boundary (format nil "----epsilon~D" (get-universal-time)))
         (body (with-output-to-string (s)
                 ;; Add file field
                 (format s "--~A~%" boundary)
                 (format s "Content-Disposition: form-data; name=\"~A\"; filename=\"~A\"~%"
                        field-name filename)
                 (format s "Content-Type: application/octet-stream~%~%")
                 (format s "~A~%" file-data)

                 ;; Add additional fields
                 (when additional-fields
                   (loop for (key value) on additional-fields by #'cddr
                         do (format s "--~A~%" boundary)
                            (format s "Content-Disposition: form-data; name=\"~A\"~%~%"
                                   (string-downcase (string key)))
                            (format s "~A~%" value)))

                 ;; End boundary
                 (format s "--~A--~%" boundary))))

    (request url
            :method :post
            :body body
            :headers (map:assoc (or headers (map:make-map))
                                "Content-Type"
                                (format nil "multipart/form-data; boundary=~A" boundary)))))
