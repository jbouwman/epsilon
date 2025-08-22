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
                               handler)
  "Start an HTTP/2 server"
  (let ((config (make-http2-server-config
                 :port port
                 :host host
                 :ssl-p ssl-p
                 :cert-file cert-file
                 :key-file key-file
                 :handler (or handler #'default-handler))))
    
    (format t "Starting HTTP/2 server on ~A:~D~%" host port)
    
    ;; Create server socket
    (let ((server-socket (create-server-socket host port ssl-p cert-file key-file)))
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

(defun create-server-socket (host port ssl-p cert-file key-file)
  "Create a server socket with optional TLS"
  (let ((address (net:make-socket-address host port)))
    (let ((socket (net:tcp-bind address :backlog 128)))
      (if ssl-p
          (wrap-with-tls socket cert-file key-file)
          socket))))

(defun accept-connection (server-socket)
  "Accept a new connection"
  (net:tcp-accept server-socket))

(defun wrap-with-tls (socket cert-file key-file)
  "Wrap socket with TLS (using epsilon.crypto)"
  ;; TODO: Implement TLS wrapping with cert-file and key-file
  (declare (ignore cert-file key-file))
  socket)

(defun close-socket (socket)
  "Close a socket"
  (net:tcp-shutdown socket :how :both))

(defun handle-connection (socket config)
  "Handle an HTTP/2 connection"
  ;; Create HTTP/2 connection using public API
  (let ((conn (http2:make-http2-connection socket
                                           :tls-connection nil
                                           :client-p nil))
        (handler (http2-server-config-handler config)))
    
    (handler-case
        (progn
          ;; The HTTP/2 connection handles the protocol
          (format t "HTTP/2 connection established~%")
          
          ;; Handle the connection with the provided handler
          (http2:handle-http2-connection conn handler))
      
      (error (e)
        (format t "Connection error: ~A~%" e)))
    
    (close-socket socket)))

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