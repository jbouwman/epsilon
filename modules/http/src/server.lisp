(defpackage epsilon.http.server
  (:use cl)
  (:local-nicknames
   (net epsilon.net)
   (str epsilon.string)
   (map epsilon.map)
   (time epsilon.time)
   (request epsilon.http.request)
   (response epsilon.http.response)
   (tls epsilon.crypto)
   (log epsilon.log))
  (:export
   start-server
   stop-server
   with-server
   *default-port*
   wrap-middleware))

(in-package :epsilon.http.server)

(defparameter *default-port* 8080)

(defvar *servers* map:+empty+)

(defclass http-server ()
  ((port :initarg :port :accessor server-port)
   (socket :initarg :socket :accessor server-socket)
   (thread :initarg :thread :accessor server-thread)
   (running-p :initform t :accessor server-running-p)
   (tls-context :initarg :tls-context :accessor server-tls-context :initform nil)
   (ssl-p :initarg :ssl-p :accessor server-ssl-p :initform nil)
   (require-client-cert :initarg :require-client-cert :accessor server-require-client-cert :initform nil)
   (alpn-protocols :initarg :alpn-protocols :accessor server-alpn-protocols :initform nil)
   (application :initarg :application :accessor server-application
                :documentation "Middleware pipeline function to handle requests")))

(defun read-http-headers (stream)
  "Read HTTP headers from stream until double CRLF is found"
  (log:debug "read-http-headers called")
  (let ((request-text "")
        (char-count 0)
        (max-chars 8192))  ; Limit request size
    (loop for char = (handler-case 
                         (progn
                           (log:trace "Reading char from stream...")
                           (read-char stream nil nil))
                       (error (e) 
                         (log:error "Error reading char: ~A" e)
                         nil))
          while (and char (< char-count max-chars))
          do (progn
               (incf char-count)
               (when (= (mod char-count 100) 0)
                 (log:trace "Read ~A chars so far" char-count))
               (setf request-text (concatenate 'string request-text (string char)))
               ;; Check for double CRLF (end of headers)
               (when (and (>= (length request-text) 4)
                          (string= (subseq request-text (- (length request-text) 4))
                                   (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline)))
                 (log:debug "Found end of headers after ~A chars" char-count)
                 (return request-text))))
    (log:debug "Finished reading headers, total chars: ~A" (length request-text))
    (if (> (length request-text) 0)
        request-text
      nil)))

(defun read-http-request (connection ssl-p)
  "Read HTTP request from connection and return request object"
  (log:debug "read-http-request called, ssl-p: ~A" ssl-p)
  (let ((stream (if ssl-p
                    (tls:tls-stream connection)
                  (make-two-way-stream (net:tcp-stream-reader connection)
                                       (net:tcp-stream-writer connection)))))
    (log:debug "Got stream: ~A" stream)
    (when stream
      (handler-case
          (progn
            (log:debug "Reading HTTP headers...")
            (let ((request-text (read-http-headers stream)))
              (log:debug "Request text: ~A" request-text)
              ;; Parse the complete request
              (when request-text
                ;; First parse headers to get content-length
                (let ((parsed-request (request:parse-http-request request-text)))
                  (when parsed-request
                    ;; Read body if content-length is specified
                    (let ((content-length-str (map:get (request:request-headers parsed-request) "content-length")))
                      (when content-length-str
                        (let ((content-length (ignore-errors (parse-integer content-length-str))))
                          (when (and content-length (> content-length 0))
                            (log:debug "Reading body of ~A bytes" content-length)
                            (let ((body-chars (make-string content-length)))
                              (read-sequence body-chars stream)
                              (log:debug "Read body: ~S" body-chars)
                              ;; Create new request with body
                              (setf parsed-request
                                    (request:make-request 
                                     (request:request-method parsed-request)
                                     (request:request-path parsed-request)
                                     :headers (request:request-headers parsed-request)
                                     :body body-chars
                                     :params (request:request-params parsed-request))))))))
                    parsed-request)))))
        (error (e)
               (log:error "Error reading HTTP request: ~A" e)
               nil)))))  ; Return nil on any error

(defun send-http-response (connection response-obj ssl-p)
  "Send HTTP response object to connection"
  (let ((stream (if ssl-p
                    (tls:tls-stream connection)
                  (make-two-way-stream (net:tcp-stream-reader connection)
                                       (net:tcp-stream-writer connection)))))
    (when stream
      (let ((response-text (response:response-to-string response-obj)))
        (write-string response-text stream)
        (force-output stream)))))

(defun handle-client (connection ssl-p application &key require-client-cert)
  "Handle a client connection using application pipeline with mTLS support"
  (log:info "Handle-client called with connection: ~A, ssl-p: ~A" connection ssl-p)
  (handler-case
      (progn
        ;; For mTLS, verify client certificate was provided if required
        (when (and ssl-p require-client-cert)
          (let ((peer-cert (tls:get-peer-certificate connection)))
            (unless peer-cert
              (error "Client certificate required but not provided"))
            ;; Verify the certificate
            (multiple-value-bind (valid-p error-msg)
                (tls:verify-peer-certificate connection)
              (unless valid-p
                (error "Client certificate verification failed: ~A" error-msg)))))
        
        (log:debug "Reading HTTP request...")
        (let ((req (read-http-request connection ssl-p)))
          (log:info "Request read: ~A" req)
          (if req
              (progn
                ;; Add client certificate info to request if available
                (when ssl-p
                  (let ((peer-cert (tls:get-peer-certificate connection)))
                    (when peer-cert
                      ;; Add certificate info to request headers for application use
                      (let ((cert-subject (tls:get-certificate-subject peer-cert))
                            (cert-issuer (tls:get-certificate-issuer peer-cert)))
                        (setf req (request:add-header req "X-Client-Cert-Subject" cert-subject))
                        (setf req (request:add-header req "X-Client-Cert-Issuer" cert-issuer))))))
                
                ;; Add negotiated protocol to request
                (when ssl-p
                  (let ((protocol (tls:tls-selected-alpn-protocol connection)))
                    (when protocol
                      (setf req (request:add-header req "X-Negotiated-Protocol" protocol)))))
                
                (log:debug "Calling application with request...")
                (let ((response (funcall application req)))
                  (log:info "Application returned response: ~A" response)
	          (send-http-response connection response ssl-p)))
            (progn
              (log:warn "No request read, sending 400")
              (send-http-response connection 
                                  (response:html-response 
                                   "<h1>400 Bad Request</h1>"
                                   :status 400)
                                  ssl-p)))))
    (error (e)
           (log:error "Error handling client: ~A" e)
	   (ignore-errors
             (send-http-response connection 
				 (response:html-response 
				  (format nil "<h1>500 Internal Server Error</h1><p>~A</p>" e)
				  :status 500)
				 ssl-p))))
  
  ;; Close connection properly
  (log:debug "Closing connection...")
  (if ssl-p
      (tls:tls-close connection)
    (net:tcp-close connection))
  (log:debug "Connection closed"))

(defun server-loop (server)
  "Main server loop"
  (log:info "Server loop started for port ~A" (server-port server))
  (loop while (server-running-p server)
        do (handler-case
	       (progn
                 (log:debug "Waiting for connection on port ~A..." (server-port server))
                 (let ((raw-connection (net:tcp-accept (server-socket server))))
                   ;; Check if accept returned nil (timeout or socket closed)
                   (unless raw-connection
                     ;; If server is still running, this was just a timeout - continue
                     (if (server-running-p server)
                         (progn
                           (log:trace "Accept timeout, continuing...")
                           nil)  ; Continue the loop
                         (progn
                           (log:debug "Server stopped, exiting server loop")
                           (return))))
                   ;; Only process if we got a connection
                   (when raw-connection
                     (log:info "Accepted connection: ~A" raw-connection)
                     (let ((connection (if (server-ssl-p server)
                                           (tls:tls-accept raw-connection
                                                           :context (server-tls-context server))
                                         raw-connection))
                           (app (server-application server)))
                       (log:debug "Creating client handler thread...")
                       (sb-thread:make-thread
                        (lambda ()
                          (log:debug "Client handler thread started")
                          (handle-client connection (server-ssl-p server) app
                                         :require-client-cert (server-require-client-cert server))
                          (log:debug "Client handler thread finished"))
                        :name "HTTP client handler")))))
             (error (e)
                    (log:error "Error in server loop: ~A" e)
                    (unless (server-running-p server)
                      (return))))))

(defun start-server (application &key (port *default-port*) (address "0.0.0.0") 
                                 tls-context ssl-p cert-file key-file ca-file
                                 require-client-cert alpn-protocols verify-depth
                                 session-cache-p)
  "Start HTTP/HTTPS server with full mTLS and HTTP/2 support.
   Parameters:
   - application: Request handler function
   - port: Server port
   - address: Bind address
   - tls-context: Pre-configured TLS context
   - ssl-p: Enable SSL/TLS
   - cert-file: Server certificate file
   - key-file: Server private key file
   - ca-file: CA certificates for client verification (mTLS)
   - require-client-cert: Require client certificates (mTLS)
   - alpn-protocols: ALPN protocols to support (default: '(\"h2\" \"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption"
  (log:info "Starting server on ~A:~A" address port)
  (when (map:get *servers* port)
    (error "Server already running on port ~D" port))
  
  ;; Create TLS context if SSL is requested
  (when (or ssl-p cert-file key-file)
    (unless tls-context
      (let ((effective-alpn-protocols (or alpn-protocols '("h2" "http/1.1"))))
        (setf tls-context (tls:create-openssl-context 
                          :server-p t
                          :cert-file cert-file
                          :key-file key-file
                          :ca-file ca-file
                          :verify-mode (if require-client-cert
                                          (logior tls:+ssl-verify-peer+
                                                 tls:+ssl-verify-fail-if-no-peer-cert+)
                                        tls:+ssl-verify-none+)
                          :require-client-cert require-client-cert
                          :verify-depth verify-depth
                          :alpn-protocols effective-alpn-protocols
                          :session-cache-p session-cache-p)))))
  
  (let* ((addr (net:make-socket-address address port))
         (listener (net:tcp-bind addr :reuse-addr t)))
    (let ((server (make-instance 'http-server
                                 :port port
                                 :socket listener
                                 :tls-context tls-context
                                 :ssl-p (or ssl-p (not (null tls-context)))
                                 :require-client-cert require-client-cert
                                 :alpn-protocols alpn-protocols
                                :application application)))
      (setf (server-thread server)
            (sb-thread:make-thread 
             (lambda () 
               (server-loop server))
             :name (format nil "HTTP server on port ~D" port)))
      
      (setf *servers* (map:assoc *servers* port server))
      server)))

(defun stop-server (port-or-server)
  "Stop HTTP server"
  (let ((server (if (typep port-or-server 'http-server)
                    port-or-server
                  (map:get *servers* port-or-server))))
    (when server
      (setf (server-running-p server) nil)
      (net:tcp-close (server-socket server))
      (sb-thread:join-thread (server-thread server))
      (setf *servers* (map:dissoc *servers* (server-port server)))
      t)))

(defmacro with-server ((server application &key (port *default-port*)) &body body)
  "Execute body with HTTP server running"
  `(let ((,server (start-server ,application :port ,port)))
     (unwind-protect
         (progn ,@body)
       (stop-server ,server))))

(defun wrap-middleware (handler &rest middlewares)
  "Wrap handler with multiple middleware functions"
  (if middlewares
      ;; Apply middleware functions in reverse order (innermost first)
      (reduce (lambda (h mw) (funcall mw h))
	      (reverse middlewares)
	      :initial-value handler)
    handler))
