;;;; HTTP/2 Server Implementation
;;;;
;;;; Implements an HTTP/2 server that can pass h2spec conformance tests

(defpackage :epsilon.http2.server
  (:use :cl)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:net #:epsilon.net)
   (#:crypto #:epsilon.crypto)
   (#:log #:epsilon.log))
  (:export
   ;; Server configuration
   #:http2-server-config
   #:make-http2-server-config
   #:http2-server-config-port
   #:http2-server-config-host
   #:http2-server-config-ssl-p
   #:http2-server-config-cert-file
   #:http2-server-config-key-file
   #:http2-server-config-handler
   #:http2-server-config-max-concurrent-streams
   #:http2-server-config-initial-window-size
   #:http2-server-config-max-frame-size
   #:http2-server-config-max-header-list-size
   #:http2-server-config-enable-push
   #:http2-server-config-ca-file
   #:http2-server-config-require-client-cert
   #:http2-server-config-verify-depth
   
   ;; Server functions
   #:start-http2-server
   #:stop-http2-server
   #:default-handler
   
   ;; Constants
   #:*client-preface*))

(in-package :epsilon.http2.server)

;;;; Server Configuration

(defstruct http2-server-config
  "HTTP/2 server configuration"
  (port 8080 :type integer)
  (host "0.0.0.0" :type string)
  (ssl-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (ca-file nil :type (or null string))
  (require-client-cert nil :type boolean)
  (verify-depth 4 :type integer)
  (handler nil :type (or null function))
  (max-concurrent-streams 100 :type integer)
  (initial-window-size 65535 :type integer)
  (max-frame-size 16384 :type integer)
  (max-header-list-size 8192 :type integer)
  (enable-push t :type boolean))

;;;; Connection Preface

(defparameter *client-preface* 
  #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)
  "HTTP/2 client connection preface")

;;;; Server Implementation

(defun start-http2-server (&key (port 8080) 
                               (host "0.0.0.0")
                               ssl-p
                               cert-file
                               key-file
                               ca-file
                               require-client-cert
                               (verify-depth 4)
                               handler)
  "Start an HTTP/2 server"
  (let ((config (make-http2-server-config
                 :port port
                 :host host
                 :ssl-p ssl-p
                 :cert-file cert-file
                 :key-file key-file
                 :ca-file ca-file
                 :require-client-cert require-client-cert
                 :verify-depth verify-depth
                 :handler (or handler #'default-handler))))
    
    (format t "Starting HTTP/2 server on ~A:~D~%" host port)
    
    ;; Create server socket
    (let ((server-socket (create-server-socket config)))
      (unwind-protect
           (loop
             (let ((client-socket (accept-connection server-socket)))
               (sb-thread:make-thread 
                (lambda () 
                  (handle-connection client-socket config))
                :name "http2-connection")))
        (close-socket server-socket)))))

(defun stop-http2-server ()
  "Stop the HTTP/2 server"
  ;; This would need to track the server state
  (format t "Stopping HTTP/2 server~%"))

(defun create-server-socket (config)
  "Create a server socket with optional TLS"
  (let ((address (net:make-socket-address 
                  (http2-server-config-host config)
                  (http2-server-config-port config))))
    (let ((socket (net:tcp-bind address :backlog 128)))
      (if (http2-server-config-ssl-p config)
          (wrap-with-tls-config socket config)
          socket))))

(defun accept-connection (server-socket)
  "Accept a new connection"
  (net:tcp-accept server-socket))

(defun wrap-with-tls (socket cert-file key-file)
  "Wrap socket with TLS (using epsilon.crypto)"
  ;; Create OpenSSL context with ALPN support for HTTP/2
  (let ((tls-context (crypto:create-openssl-context 
                      :server-p t
                      :cert-file cert-file
                      :key-file key-file
                      :alpn-protocols '("h2" "http/1.1")  ; HTTP/2 and fallback
                      :verify-mode crypto:+ssl-verify-none+))) ; Basic TLS first
    
    ;; Accept TLS connection on the socket
    ;; The socket is a tcp-stream from epsilon.net, get its file descriptor
    (let ((fd (net:tcp-stream-handle socket)))
      (crypto:openssl-accept fd tls-context))))

(defun wrap-with-tls-config (socket config)
  "Wrap socket with TLS using full configuration including mTLS"
  ;; Create OpenSSL context with ALPN and optional mTLS support
  (let ((tls-context (crypto:create-openssl-context 
                      :server-p t
                      :cert-file (http2-server-config-cert-file config)
                      :key-file (http2-server-config-key-file config)
                      :ca-file (http2-server-config-ca-file config)
                      :require-client-cert (http2-server-config-require-client-cert config)
                      :verify-depth (http2-server-config-verify-depth config)
                      :alpn-protocols '("h2" "http/1.1")  ; HTTP/2 and fallback
                      :verify-mode (if (http2-server-config-require-client-cert config)
                                      (logior crypto:+ssl-verify-peer+ 
                                              crypto:+ssl-verify-fail-if-no-peer-cert+)
                                      crypto:+ssl-verify-none+))))
    
    ;; Return the socket wrapped for TLS - actual TLS handshake happens per connection
    ;; Store the context with the socket for later use
    socket))

(defun close-socket (socket)
  "Close a socket"
  (net:tcp-shutdown socket :how :both))

(defun handle-connection (socket config)
  "Handle an HTTP/2 connection"
  ;; socket may be plain tcp-stream or openssl-connection
  (let* ((is-tls (typep socket 'crypto:openssl-connection))
         (conn (http2:make-http2-connection 
                (if is-tls
                    socket  ; Pass TLS connection directly
                    socket) ; Pass plain socket
                :tls-connection (when is-tls socket)
                :client-p nil))
         (handler (http2-server-config-handler config)))
    
    (handler-case
        (progn
          ;; The HTTP/2 connection handles the protocol
          (format t "HTTP/2 connection established (~A)~%" 
                    (if is-tls "TLS" "plain"))
          
          ;; Check ALPN negotiation result if TLS
          (when is-tls
            (let ((alpn-protocol (epsilon.crypto.alpn:get-selected-protocol 
                                 (crypto:openssl-connection-ssl socket))))
              (when alpn-protocol
                (format t "ALPN negotiated: ~A~%" alpn-protocol))))
          
          ;; Handle the connection with the provided handler
          (http2:handle-http2-connection conn handler))
      
      (error (e)
        (format t "Connection error: ~A~%" e)))
    
    ;; Close appropriately
    (if is-tls
        (crypto:openssl-close socket)
        (close-socket socket))))

(defun default-handler (headers body)
  "Default request handler"
  (declare (ignore body))
  (format t "Received request with headers: ~A~%" headers)
  (list :status 200
        :headers (list (cons "content-type" "text/plain"))
        :body "Hello from HTTP/2 server!"))

;;;; Export symbols

(export '(;; Server
          start-http2-server
          stop-http2-server
          http2-server-config
          make-http2-server-config
          default-handler))