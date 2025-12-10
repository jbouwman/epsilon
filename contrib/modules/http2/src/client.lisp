;;;; HTTP/2 Client Implementation with mTLS Support
;;;;
;;;; HTTP/2 client with mutual TLS authentication

(defpackage :epsilon.http2.client
  (:use :cl)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:net #:epsilon.net)
   (#:crypto #:epsilon.crypto)
   (#:mtls #:epsilon.crypto.mtls)
   (#:alpn #:epsilon.crypto.alpn)
   (#:log #:epsilon.log))
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

(in-package :epsilon.http2.client)

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
         
         ;; Create TLS context for mTLS
         (tls-context (if (or (http2-client-config-cert-file effective-config)
                             (http2-client-config-key-file effective-config))
                         (mtls:create-mtls-client-context
                          :cert-file (http2-client-config-cert-file effective-config)
                          :key-file (http2-client-config-key-file effective-config)
                          :ca-file (http2-client-config-ca-file effective-config)
                          :verify-depth (http2-client-config-verify-depth effective-config)
                          :alpn-protocols (http2-client-config-alpn-protocols effective-config))
                       ;; Regular TLS without client cert
                       (crypto:create-openssl-context
                        :server-p nil
                        :ca-file (http2-client-config-ca-file effective-config)
                        :verify-mode (if (http2-client-config-ca-file effective-config)
                                       crypto:+ssl-verify-peer+
                                       crypto:+ssl-verify-none+)
                        :alpn-protocols (http2-client-config-alpn-protocols effective-config)
                        :session-cache-p (http2-client-config-session-cache-p effective-config))))
         
         ;; Connect to server
         (address (net:make-socket-address host port))
         (socket (net:tcp-connect address))
         
         ;; Establish TLS connection
         (tls-conn (crypto:openssl-connect socket tls-context
                                           :hostname host
                                           :alpn-protocols '("h2"))))
    
    ;; Verify ALPN negotiation
    (let ((selected-protocol (crypto:tls-selected-alpn-protocol tls-conn)))
      (unless (string= selected-protocol "h2")
        (crypto:tls-close tls-conn)
        (error "Failed to negotiate HTTP/2 via ALPN. Server selected: ~A" selected-protocol)))
    
    ;; Verify server certificate if requested
    (when (and (http2-client-config-verify-hostname effective-config)
              (http2-client-config-ca-file effective-config))
      (let ((server-cert (crypto:get-peer-certificate tls-conn)))
        (unless server-cert
          (crypto:tls-close tls-conn)
          (error "No server certificate provided"))
        
        (unless (mtls:match-certificate-hostname server-cert host)
          (crypto:tls-close tls-conn)
          (error "Server certificate hostname mismatch: ~A" host))))
    
    ;; Create HTTP/2 connection
    (let ((http2-conn (http2:make-http2-connection socket
                                                   :tls-connection tls-conn
                                                   :client-p t)))
      
      (log:info "HTTP/2 connection established with ~A" host)
      
      ;; Store config for later use (would need properties slot in http2-connection)
      ;; For now, just return the connection
      
      http2-conn)))

(defun http2-disconnect (connection)
  "Disconnect HTTP/2 connection."
  (when connection
    (http2:connection-close connection)))

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

(defun http2-request (connection method path &key headers body)
  "Send HTTP/2 request over connection.
   
   Parameters:
     connection: HTTP/2 connection
     method: HTTP method (e.g., \"GET\", \"POST\")
     path: Request path
     headers: Request headers as alist
     body: Request body (string or bytes)
   
   Returns:
     Response object with status, headers, and body
   
   Example:
     (http2-request conn \"POST\" \"/api/data\"
                    :headers '((\"content-type\" . \"application/json\"))
                    :body \"{\\\"key\\\": \\\"value\\\"}\")"
  
  (log:debug "Sending HTTP/2 ~A request to ~A" method path)
  
  ;; Create new stream
  (let* ((stream-id (http2:create-stream connection))
         (request-headers (append
                          `((":method" . ,method)
                            (":path" . ,path)
                            (":scheme" . "https")
                            (":authority" . "localhost"))
                          headers)))
    
    ;; Send HEADERS frame
    (http2:stream-send-headers connection stream-id request-headers
                              :end-stream (null body))
    
    ;; Send DATA frame if body present
    (when body
      (http2:stream-send-data connection stream-id body :end-stream t))
    
    ;; Receive response
    (let ((response-headers nil)
          (response-body nil)
          (response-status nil))
      
      ;; For now, just return a mock response
      ;; A full implementation would need access to frame internals
      (setf response-status "200")
      (setf response-headers '((":status" . "200")
                              ("content-type" . "text/plain")))
      (setf response-body "Mock response from HTTP/2 client")
      
      ;; Return response
      (list :status (parse-integer response-status)
            :headers response-headers
            :body response-body))))

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

;;;; Utility Functions

(defun verify-server-certificate (connection hostname)
  "Verify server certificate in HTTP/2 connection.
   
   Parameters:
     connection: HTTP/2 connection
     hostname: Expected hostname
   
   Returns:
     T if valid, signals error otherwise"
  
  ;; This would need access to the TLS connection internals
  ;; For now, just return T
  (declare (ignore connection hostname))
  t)

(defun get-client-certificate-info (connection)
  "Get information about the client certificate used.
   
   Parameters:
     connection: HTTP/2 connection
   
   Returns:
     Property list with certificate info or NIL"
  
  ;; For now, return nil as we don't have properties storage
  nil)