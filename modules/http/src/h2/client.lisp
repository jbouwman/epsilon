;;;; HTTP/2 Client Implementation with mTLS Support
;;;;
;;;; HTTP/2 client with mutual TLS authentication
;;;;
;;;; This file is part of the unified HTTP module (epsilon.http.h2)

(defpackage :epsilon.http.h2.client
  (:use :cl)
  (:import
   (epsilon.net net)
   (epsilon.crypto crypto)
   (epsilon.http.h2 h2)
   (epsilon.log log))
  (:export
   ;; Client connection
   #:http2-connect
   #:http2-disconnect
   #:with-http2-connection

   ;; Request functions
   #:http2-request
   #:http2-get
   #:http2-post
   #:http2-put
   #:http2-delete

   ;; mTLS configuration
   #:make-http2-client-config
   #:http2-client-config-p
   #:http2-client-config-cert-file
   #:http2-client-config-key-file
   #:http2-client-config-ca-file
   #:http2-client-config-verify-hostname
   #:http2-client-config-session-cache-p))

;;;; Client Configuration

(defstruct http2-client-config
  "HTTP/2 client configuration with mTLS support"
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (ca-file nil :type (or null string))
  (verify-hostname t :type boolean)
  (verify-depth 4 :type integer)
  (session-cache-p t :type boolean)
  (alpn-protocols '("h2") :type list)
  (timeout 30 :type integer))

;;;; Forward references (resolved at load time)
(declaim (ftype (function (t &key (:tls-connection t) (:client-p t)) t) h2-make-connection))
(declaim (ftype (function (t) t) h2-create-stream))
(declaim (ftype (function (t t &key (:end-stream t)) t) h2-stream-send-headers))
(declaim (ftype (function (t t &key (:end-stream t)) t) h2-stream-send-data))
(declaim (ftype (function (t t) t) h2-read-response))
(declaim (ftype (function (t) t) h2-connection-close))

;;;; Connection Management

(defun http2-connect (host port &key config cert-file key-file ca-file
                                 verify-hostname session-cache-p)
  "Connect to HTTP/2 server with optional mTLS.

   Parameters:
     host: Server hostname
     port: Server port
     config: http2-client-config structure (overrides individual params)
     cert-file: Client certificate for mTLS
     key-file: Client private key for mTLS
     ca-file: CA certificates for server verification
     verify-hostname: Verify server hostname against certificate
     session-cache-p: Enable TLS session resumption

   Returns:
     HTTP/2 connection object

   Example:
     (http2-connect \"api.example.com\" 443
                    :cert-file \"client.pem\"
                    :key-file \"client-key.pem\"
                    :ca-file \"ca.pem\")"

  (log:info "Connecting to HTTP/2 server ~A:~D" host port)

  ;; Use config if provided, otherwise use individual parameters
  (let* ((effective-config (or config
                               (make-http2-client-config
                                :cert-file cert-file
                                :key-file key-file
                                :ca-file ca-file
                                :verify-hostname verify-hostname
                                :session-cache-p session-cache-p)))

         ;; Create TLS context for mTLS or regular TLS
         (tls-context (if (or (http2-client-config-cert-file effective-config)
                             (http2-client-config-key-file effective-config))
                         ;; mTLS with client certificate. ALPN is configured below
                         ;; via context-set-alpn-protocols; verify-depth has no
                         ;; equivalent in the TLS 1.3 stack and is currently a
                         ;; no-op carried in the config struct.
                         (crypto:make-mtls-client-context
                          :cert-file (http2-client-config-cert-file effective-config)
                          :key-file (http2-client-config-key-file effective-config)
                          :ca-file (http2-client-config-ca-file effective-config))
                       ;; Regular TLS without client cert
                       (crypto:make-client-context
                        :ca-file (http2-client-config-ca-file effective-config))))

         ;; Connect to server
         (address (net:make-socket-address host port))
         (socket (net:tcp-connect address)))

    ;; Set ALPN protocols on context
    (crypto:context-set-alpn-protocols tls-context
                                       (http2-client-config-alpn-protocols effective-config))

    ;; Establish TLS connection
    (let ((tls-conn (crypto:tls-connect socket tls-context
                                        :hostname host
                                        :alpn-protocols '("h2"))))

      ;; Verify ALPN negotiation
      (let ((selected-protocol (crypto:connection-alpn-protocol tls-conn)))
        (unless (string= selected-protocol "h2")
          (crypto:tls-close tls-conn)
          (error "Failed to negotiate HTTP/2 via ALPN. Server selected: ~A" selected-protocol)))

      ;; Verify server certificate if requested
      (when (and (http2-client-config-verify-hostname effective-config)
                 (http2-client-config-ca-file effective-config))
        (let ((server-cert (crypto:connection-peer-certificate tls-conn)))
          (unless server-cert
            (crypto:tls-close tls-conn)
            (error "No server certificate provided"))

          (unless (crypto:match-certificate-hostname server-cert host)
            (crypto:tls-close tls-conn)
            (error "Server certificate hostname mismatch: ~A" host))))

      ;; Create HTTP/2 connection
      (let ((http2-conn (h2:make-http2-connection socket
                                                  :tls-connection tls-conn
                                                  :client-p t)))
        (log:info "HTTP/2 connection established with ~A" host)
        http2-conn))))

(defun http2-disconnect (connection)
  "Disconnect HTTP/2 connection."
  (when connection
    (h2:connection-close connection)))

(defmacro with-http2-connection ((conn-var host port &rest options) &body body)
  "Execute body with HTTP/2 connection.

   Example:
     (with-http2-connection (conn \"api.example.com\" 443
                                  :cert-file \"client.pem\"
                                  :key-file \"client-key.pem\")
       (http2-get conn \"/api/v1/data\"))"

  `(let ((,conn-var (http2-connect ,host ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (http2-disconnect ,conn-var))))

;;;; Request Functions

(defun http2-request (connection method path &key headers body authority (scheme "https"))
  "Send HTTP/2 request over connection.

   Parameters:
     connection: HTTP/2 connection
     method: HTTP method (e.g., \"GET\", \"POST\")
     path: Request path
     headers: Request headers as alist (lowercase names; pseudo-headers
              :method/:path/:scheme/:authority are added by this fn)
     body: Request body (string or bytes)
     authority: value for the :authority pseudo-header (the original Host).
                Defaults to \"localhost\" for backward compatibility, but
                callers proxying real traffic must pass the upstream host.
     scheme: :scheme pseudo-header value (defaults to \"https\").

   Returns:
     Response object with status, headers, and body

   Example:
     (http2-request conn \"POST\" \"/api/data\"
                    :authority \"api.example.com\"
                    :headers '((\"content-type\" . \"application/json\"))
                    :body \"{\\\"key\\\": \\\"value\\\"}\")"

  (log:debug "Sending HTTP/2 ~A request to ~A" method path)
  (let* ((stream (h2:create-stream connection))
         (request-headers (append
                           `((":method" . ,method)
                             (":path" . ,path)
                             (":scheme" . ,scheme)
                             (":authority" . ,(or authority "localhost")))
                           headers)))
    (h2:stream-send-headers stream request-headers :end-stream (null body))
    (when body
      (h2:stream-send-data stream body :end-stream t))
    (read-stream-response connection stream)))

(defun http2-get (connection path &key headers)
  "Send HTTP/2 GET request.

   Example:
     (http2-get conn \"/api/users\"
                :headers '((\"authorization\" . \"Bearer token\")))"
  (http2-request connection "GET" path :headers headers))

(defun http2-post (connection path &key headers body)
  "Send HTTP/2 POST request.

   Example:
     (http2-post conn \"/api/users\"
                 :headers '((\"content-type\" . \"application/json\"))
                 :body \"{\\\"name\\\": \\\"John\\\"}\")"
  (http2-request connection "POST" path :headers headers :body body))

(defun http2-put (connection path &key headers body)
  "Send HTTP/2 PUT request."
  (http2-request connection "PUT" path :headers headers :body body))

(defun http2-delete (connection path &key headers)
  "Send HTTP/2 DELETE request."
  (http2-request connection "DELETE" path :headers headers))

;;;; Response Reading

(defun read-stream-response (connection stream)
  "Read HTTP/2 response from connection for a specific stream.
   Returns plist with :status, :headers, and :body."
  (let ((response (h2:read-http2-response connection stream)))
    (let* ((headers (getf response :headers))
           (body (getf response :body))
           (status-header (assoc ":status" headers :test #'string=))
           (status (if status-header
                       (parse-integer (cdr status-header) :junk-allowed t)
                       200)))
      (list :status status
            :headers headers
            :body body))))
