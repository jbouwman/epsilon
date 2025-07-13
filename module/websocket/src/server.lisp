;;;; WebSocket Server Implementation
;;;;
;;;; This module provides a WebSocket server that can handle WebSocket connections
;;;; and integrate with existing HTTP servers.

(defpackage epsilon.websocket.server
  (:use
   cl
   epsilon.lib.syntax)
  (:local-nicknames
   (net epsilon.net)
   (str epsilon.lib.string)
   (thread epsilon.sys.thread)
   (map epsilon.lib.map)
   (http epsilon.http.server)
   (req epsilon.http.request)
   (resp epsilon.http.response)
   (handshake epsilon.websocket.handshake)
   (conn epsilon.websocket.connection)
   (frame epsilon.websocket.frame))
  (:export
   ;; Server creation
   websocket-server
   make-server
   start-server
   stop-server
   
   ;; Server configuration
   websocket-server-options
   make-server-options
   
   ;; Handler registration
   register-handler
   unregister-handler
   
   ;; Connection management
   get-connections
   broadcast-text
   broadcast-binary
   close-all-connections
   
   ;; HTTP integration
   websocket-upgrade-handler
   add-websocket-to-http-server))

(in-package epsilon.websocket.server)

;;; Server structure

(defstruct websocket-server
  "WebSocket server state"
  (socket nil :type (or null t))              ; Server socket
  (handlers (map:make-map) :type map:map)     ; Path -> handler mapping
  (connections (map:make-map) :type map:map)  ; Connection ID -> connection
  (options nil :type websocket-server-options) ; Server options
  (running nil :type boolean)                 ; Server running flag
  (accept-thread nil :type (or null thread:thread))) ; Accept thread

(defstruct websocket-server-options
  "WebSocket server configuration"
  (host "localhost" :type string)             ; Bind host
  (port 8080 :type integer)                  ; Bind port
  (subprotocols nil :type list)              ; Supported subprotocols
  (extensions nil :type list)                ; Supported extensions
  (max-connections 1000 :type integer)       ; Maximum concurrent connections
  (ping-interval 30 :type number)            ; Ping interval in seconds
  (ping-timeout 5 :type number)              ; Ping response timeout
  (max-frame-size (* 1024 1024) :type integer) ; Maximum frame size
  (compression nil :type boolean)             ; Enable compression
  
  ;; Default event handlers
  (on-connect nil :type (or null function))   ; New connection
  (on-disconnect nil :type (or null function)) ; Connection closed
  (on-error nil :type (or null function)))    ; Error handler

(defun make-server-options (&rest args)
  "Create server options structure"
  (apply #'make-websocket-server-options args))

;;; Handler management

(defstruct websocket-handler
  "WebSocket request handler"
  (path "/" :type string)                     ; URL path pattern
  (subprotocols nil :type list)              ; Supported subprotocols
  (on-connect nil :type (or null function))   ; Connection established
  (on-message nil :type (or null function))   ; Message received
  (on-close nil :type (or null function))     ; Connection closed
  (on-error nil :type (or null function))     ; Error occurred
  (on-ping nil :type (or null function))      ; Ping received
  (on-pong nil :type (or null function)))     ; Pong received

(defun register-handler (server path handler)
  "Register WebSocket handler for path"
  (map:assoc! (websocket-server-handlers server) path handler))

(defun unregister-handler (server path)
  "Unregister WebSocket handler for path"
  (map:dissoc! (websocket-server-handlers server) path))

(defun find-handler (server path)
  "Find handler for given path"
  (or (map:get (websocket-server-handlers server) path)
      (map:get (websocket-server-handlers server) "/"))) ; Default handler

;;; Connection management

(defun add-connection (server connection)
  "Add connection to server"
  (let ((id (generate-connection-id)))
    (map:assoc! (websocket-server-connections server) id connection)
    id))

(defun remove-connection (server connection-id)
  "Remove connection from server"
  (map:dissoc! (websocket-server-connections server) connection-id))

(defun get-connections (server)
  "Get all active connections"
  (map:vals (websocket-server-connections server)))

(defun generate-connection-id ()
  "Generate unique connection ID"
  (format nil "~A-~A" (get-universal-time) (random 1000000)))

;;; Server lifecycle

(defun start-server (server)
  "Start WebSocket server"
  (when (websocket-server-running server)
    (error "Server is already running"))
  
  (let* ((options (websocket-server-options server))
         (host (websocket-server-options-host options))
         (port (websocket-server-options-port options)))
    
    ;; Create server socket
    (let ((socket (net:socket-listen host port)))
      (setf (websocket-server-socket server) socket
            (websocket-server-running server) t)
      
      ;; Start accept thread
      (setf (websocket-server-accept-thread server)
            (thread:make-thread
             (lambda () (accept-loop server))
             :name (format nil "websocket-server-~A:~A" host port)))
      
      (format t "WebSocket server started on ~A:~A~%" host port)
      server)))

(defun stop-server (server)
  "Stop WebSocket server"
  (unless (websocket-server-running server)
    (return-from stop-server server))
  
  (setf (websocket-server-running server) nil)
  
  ;; Close all connections
  (close-all-connections server)
  
  ;; Close server socket
  (when (websocket-server-socket server)
    (net:socket-close (websocket-server-socket server))
    (setf (websocket-server-socket server) nil))
  
  ;; Wait for accept thread to finish
  (when (websocket-server-accept-thread server)
    (thread:join-thread (websocket-server-accept-thread server))
    (setf (websocket-server-accept-thread server) nil))
  
  (format t "WebSocket server stopped~%")
  server)

;;; Connection acceptance

(defun accept-loop (server)
  "Main accept loop for server"
  (loop while (websocket-server-running server)
        do (handler-case
               (accept-connection server)
             (error (e)
               (when (websocket-server-options-on-error (websocket-server-options server))
                 (funcall (websocket-server-options-on-error (websocket-server-options server))
                          e))
               (format t "Error accepting connection: ~A~%" e)))))

(defun accept-connection (server)
  "Accept and handle single connection"
  (let ((client-socket (net:socket-accept (websocket-server-socket server))))
    (when client-socket
      ;; Handle connection in separate thread
      (thread:make-thread
       (lambda () (handle-client-connection server client-socket))
       :name (format nil "websocket-client-~A" (generate-connection-id))))))

(defun handle-client-connection (server client-socket)
  "Handle individual client connection"
  (let ((stream (net:socket-stream client-socket :binary t)))
    (unwind-protect
         (progn
           ;; Parse HTTP request
           (let ((request (req:parse-request stream)))
             (if (handshake:websocket-request-p request)
                 (handle-websocket-upgrade server stream request)
                 (send-bad-request stream))))
      
      ;; Cleanup
      (when client-socket
        (net:socket-close client-socket)))))

(defun handle-websocket-upgrade (server stream request)
  "Handle WebSocket upgrade request"
  (let* ((path (req:request-path request))
         (handler (find-handler server path)))
    
    (unless handler
      (send-not-found stream)
      (return-from handle-websocket-upgrade))
    
    ;; Negotiate subprotocol
    (let* ((client-protocols (handshake:extract-subprotocols request))
           (server-protocols (websocket-handler-subprotocols handler))
           (subprotocol (handshake:negotiate-subprotocol client-protocols server-protocols)))
      
      ;; Create handshake response
      (let ((response (handshake:create-handshake-response 
                      request 
                      :subprotocol subprotocol)))
        
        ;; Send response
        (let ((response-data (resp:serialize-response response)))
          (write-sequence response-data stream)
          (force-output stream))
        
        ;; Create WebSocket connection
        (let* ((connection (conn:make-connection 
                           stream
                           :client-side nil
                           :subprotocol subprotocol
                           :on-message (websocket-handler-on-message handler)
                           :on-close (lambda (conn code reason)
                                      (handle-connection-close server conn code reason handler))
                           :on-error (websocket-handler-on-error handler)
                           :on-ping (websocket-handler-on-ping handler)
                           :on-pong (websocket-handler-on-pong handler)))
               (connection-id (add-connection server connection)))
          
          ;; Call connection handler
          (when (websocket-handler-on-connect handler)
            (funcall (websocket-handler-on-connect handler) connection))
          
          ;; Start message loop
          (message-loop server connection connection-id))))))

(defun handle-connection-close (server connection code reason handler)
  "Handle connection close event"
  (declare (ignore code reason))
  
  ;; Remove from server connections
  (map:each (lambda (id conn)
              (when (eq conn connection)
                (remove-connection server id)))
            (websocket-server-connections server))
  
  ;; Call handler
  (when (websocket-handler-on-close handler)
    (funcall (websocket-handler-on-close handler) connection code reason)))

(defun message-loop (server connection connection-id)
  "Main message processing loop for connection"
  (loop while (and (websocket-server-running server)
                   (conn:connection-open-p connection))
        do (handler-case
               (conn:read-message connection)
             (error (e)
               (when (conn:websocket-connection-on-error connection)
                 (funcall (conn:websocket-connection-on-error connection)
                          connection e))
               (conn:close-connection connection frame:+close-internal-error+)
               (return))))
  
  ;; Remove connection
  (remove-connection server connection-id))

;;; HTTP responses

(defun send-bad-request (stream)
  "Send 400 Bad Request response"
  (let ((response (resp:make-response 
                  :status 400
                  :reason "Bad Request"
                  :headers '(("Content-Type" . "text/plain"))
                  :body "Bad Request")))
    (write-sequence (resp:serialize-response response) stream)
    (force-output stream)))

(defun send-not-found (stream)
  "Send 404 Not Found response"
  (let ((response (resp:make-response
                  :status 404
                  :reason "Not Found"
                  :headers '(("Content-Type" . "text/plain"))
                  :body "Not Found")))
    (write-sequence (resp:serialize-response response) stream)
    (force-output stream)))

;;; Broadcasting

(defun broadcast-text (server text &key path)
  "Broadcast text message to all connections (optionally filtered by path)"
  (dolist (connection (get-connections server))
    (when (and (conn:connection-open-p connection)
               (or (null path) 
                   (string= path (conn:websocket-connection-path connection))))
      (handler-case
          (conn:send-text connection text)
        (error (e)
          (format t "Error broadcasting to connection: ~A~%" e))))))

(defun broadcast-binary (server data &key path)
  "Broadcast binary message to all connections (optionally filtered by path)"
  (dolist (connection (get-connections server))
    (when (and (conn:connection-open-p connection)
               (or (null path)
                   (string= path (conn:websocket-connection-path connection))))
      (handler-case
          (conn:send-binary connection data)
        (error (e)
          (format t "Error broadcasting to connection: ~A~%" e))))))

(defun close-all-connections (server)
  "Close all active connections"
  (dolist (connection (get-connections server))
    (when (conn:connection-open-p connection)
      (conn:close-connection connection frame:+close-going-away+ "Server shutting down"))))

;;; HTTP server integration

(defun websocket-upgrade-handler (server)
  "Create HTTP handler for WebSocket upgrades"
  (lambda (request response stream)
    (declare (ignore response))
    (if (handshake:websocket-request-p request)
        (handle-websocket-upgrade server stream request)
        (send-bad-request stream))))

(defun add-websocket-to-http-server (http-server websocket-server &key (path "/ws"))
  "Add WebSocket support to existing HTTP server"
  (http:add-handler http-server path (websocket-upgrade-handler websocket-server)))

;;; Convenience functions

(defun make-server (&key (host "localhost") (port 8080) (options (make-server-options)))
  "Create new WebSocket server"
  (setf (websocket-server-options-host options) host
        (websocket-server-options-port options) port)
  (make-websocket-server :options options))

(defmacro with-websocket-server ((server &rest options) &body body)
  "Create and run WebSocket server for duration of body"
  `(let ((,server (make-server ,@options)))
     (start-server ,server)
     (unwind-protect
          (progn ,@body)
       (stop-server ,server))))

;;; Simple echo server example

(defun make-echo-handler ()
  "Create simple echo handler"
  (make-websocket-handler
   :on-connect (lambda (connection)
                 (format t "Client connected~%"))
   :on-message (lambda (connection message is-text)
                 (if is-text
                     (conn:send-text connection (format nil "Echo: ~A" message))
                     (conn:send-binary connection message)))
   :on-close (lambda (connection code reason)
               (format t "Client disconnected: ~A ~A~%" code reason))))

(defun run-echo-server (&key (host "localhost") (port 8080))
  "Run simple echo server"
  (let ((server (make-server :host host :port port)))
    (register-handler server "/" (make-echo-handler))
    (start-server server)
    (format t "Press Enter to stop server...~%")
    (read-line)
    (stop-server server)))