;;;; WebSocket Public API
;;;;
;;;; This module provides the main public API for the WebSocket implementation.
;;;; It re-exports the most commonly used functions and structures from
;;;; the various WebSocket modules.

(defpackage epsilon.websocket
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (frame epsilon.websocket.frame)
   (handshake epsilon.websocket.handshake)
   (conn epsilon.websocket.connection)
   (client epsilon.websocket.client)
   (server epsilon.websocket.server))
  (:export
   ;; Frame operations (from frame module)
   websocket-frame
   make-frame
   frame-fin
   frame-opcode
   frame-masked
   frame-mask
   frame-payload
   mask-payload
   unmask-payload
   parse-frame
   serialize-frame
   encode-frame
   decode-frame
   make-text-frame
   make-binary-frame
   make-close-frame
   make-ping-frame
   make-pong-frame
   frame-text
   frame-rsv1
   frame-rsv2
   frame-rsv3
   
   ;; Frame opcodes
   +opcode-continuation+
   +opcode-text+
   +opcode-binary+
   +opcode-close+
   +opcode-ping+
   +opcode-pong+
   
   ;; Close frame payload
   encode-close-payload
   
   ;; Close codes
   +close-normal+
   +close-going-away+
   +close-protocol-error+
   +close-unsupported-data+
   +close-invalid-frame-payload+
   +close-policy-violation+
   +close-message-too-big+
   +close-mandatory-extension+
   +close-internal-error+
   
   ;; Connection management (from connection module)
   websocket-connection
   make-connection
   connection-stream
   connection-state
   connection-subprotocol
   connection-extensions
   connection-client-side
   send-text
   send-binary
   send-ping
   send-pong
   close-connection
   connection-open-p
   connection-closed-p
   wait-for-close
   
   ;; Connection states
   +state-connecting+
   +state-open+
   +state-closing+
   +state-closed+
   
   ;; Client API (from client module)
   connect
   connect-async
   websocket-client-options
   make-client-options
   parse-websocket-uri
   with-websocket-connection
   send-text-message
   send-binary-message
   echo-client
   
   ;; Server API (from server module)
   websocket-server
   make-server
   start-server
   stop-server
   websocket-server-options
   make-server-options
   websocket-handler
   register-handler
   unregister-handler
   get-connections
   broadcast-text
   broadcast-binary
   close-all-connections
   websocket-upgrade-handler
   add-websocket-to-http-server
   with-websocket-server
   make-echo-handler
   run-echo-server
   
   ;; Handshake operations (from handshake module)
   validate-websocket-request
   generate-accept-key
   valid-handshake-response-p
   create-handshake-response
   generate-websocket-key
   generate-handshake-headers
   create-upgrade-request
   validate-handshake-response
   negotiate-subprotocol
   negotiate-extensions
   websocket-handshake-error
   websocket-request-p
   extract-websocket-key
   extract-subprotocols
   extract-extensions
   
   ;; Utility functions
   valid-utf8-p))

(in-package epsilon.websocket)

;;; Re-export frame constants and functions
(defconstant +opcode-continuation+ frame:+opcode-continuation+)
(defconstant +opcode-text+ frame:+opcode-text+)
(defconstant +opcode-binary+ frame:+opcode-binary+)
(defconstant +opcode-close+ frame:+opcode-close+)
(defconstant +opcode-ping+ frame:+opcode-ping+)
(defconstant +opcode-pong+ frame:+opcode-pong+)

(defconstant +close-normal+ frame:+close-normal+)
(defconstant +close-going-away+ frame:+close-going-away+)
(defconstant +close-protocol-error+ frame:+close-protocol-error+)
(defconstant +close-unsupported-data+ frame:+close-unsupported-data+)
(defconstant +close-invalid-frame-payload+ frame:+close-invalid-frame-payload+)
(defconstant +close-policy-violation+ frame:+close-policy-violation+)
(defconstant +close-message-too-big+ frame:+close-message-too-big+)
(defconstant +close-mandatory-extension+ frame:+close-mandatory-extension+)
(defconstant +close-internal-error+ frame:+close-internal-error+)

;;; Re-export connection states
(defconstant +state-connecting+ conn:+state-connecting+)
(defconstant +state-open+ conn:+state-open+)
(defconstant +state-closing+ conn:+state-closing+)
(defconstant +state-closed+ conn:+state-closed+)

;;; Frame operations
(defun websocket-frame (&rest args) (apply #'frame:websocket-frame args))
(defun make-frame (&rest args) (apply #'frame:make-frame args))
(defun frame-fin (frame) (frame:websocket-frame-fin frame))
(defun frame-opcode (frame) (frame:websocket-frame-opcode frame))
(defun frame-masked (frame) (frame:websocket-frame-masked frame))
(defun frame-mask (frame) (frame:websocket-frame-mask frame))
(defun frame-payload (frame) (frame:websocket-frame-payload frame))
(defun mask-payload (payload mask) (frame:mask-payload payload mask))
(defun unmask-payload (payload mask) (frame:unmask-payload payload mask))
(defun parse-frame (stream) (frame:parse-frame stream))
(defun serialize-frame (frame &key client-side) 
  (frame:serialize-frame frame :client-side client-side))
(defun encode-frame (frame) (frame:serialize-frame frame))
(defun decode-frame (octets) (frame:decode-frame octets))
(defun encode-close-payload (code &optional reason) (frame:encode-close-payload code reason))
(defun make-text-frame (text &key fin) (frame:make-text-frame text :fin fin))
(defun make-binary-frame (data &key fin) (frame:make-binary-frame data :fin fin))
(defun make-close-frame (code &optional reason) (frame:make-close-frame code reason))
(defun make-ping-frame (&optional payload) (frame:make-ping-frame payload))
(defun make-pong-frame (&optional payload) (frame:make-pong-frame payload))
(defun frame-text (frame) (frame:frame-text frame))
(defun frame-rsv1 (frame) (frame:websocket-frame-rsv1 frame))
(defun frame-rsv2 (frame) (frame:websocket-frame-rsv2 frame))
(defun frame-rsv3 (frame) (frame:websocket-frame-rsv3 frame))

;;; Utility functions
(defun valid-utf8-p (octets) (frame:valid-utf8-p octets))

;;; Connection operations
(defun websocket-connection (&rest args) (apply #'conn:make-websocket-connection args))
(defun make-connection (stream &rest args) (apply #'conn:make-connection stream args))
(defun connection-stream (conn) (conn:websocket-connection-stream conn))
(defun connection-state (conn) (conn:websocket-connection-state conn))
(defun (setf connection-state) (new-state conn) 
  (setf (conn:websocket-connection-state conn) new-state))
(defun connection-subprotocol (conn) (conn:websocket-connection-subprotocol conn))
(defun connection-extensions (conn) (conn:websocket-connection-extensions conn))
(defun connection-client-side (conn) (conn:websocket-connection-client-side conn))
(defun send-text (conn text) (conn:send-text conn text))
(defun send-binary (conn data) (conn:send-binary conn data))
(defun send-ping (conn &optional payload) (conn:send-ping conn payload))
(defun send-pong (conn &optional payload) (conn:send-pong conn payload))
(defun close-connection (conn &optional code reason) 
  (conn:close-connection conn code reason))
(defun connection-open-p (conn) (conn:connection-open-p conn))
(defun connection-closed-p (conn) (conn:connection-closed-p conn))
(defun wait-for-close (conn &key timeout) (conn:wait-for-close conn :timeout timeout))

;;; Client operations
(defun connect (uri &rest args) (apply #'client:connect uri args))
(defun connect-async (uri &rest args) (apply #'client:connect-async uri args))
(defun websocket-client-options (&rest args) (apply #'client:websocket-client-options args))
(defun make-client-options (&rest args) (apply #'client:make-client-options args))
(defun parse-websocket-uri (uri) (client:parse-websocket-uri uri))
(defmacro with-websocket-connection ((connection uri &rest options) &body body)
  `(client:with-websocket-connection (,connection ,uri ,@options) ,@body))
(defun send-text-message (uri message &rest args) 
  (apply #'client:send-text-message uri message args))
(defun send-binary-message (uri data &rest args) 
  (apply #'client:send-binary-message uri data args))
(defun echo-client (uri message &rest args) 
  (apply #'client:echo-client uri message args))

;;; Server operations
(defun websocket-server (&rest args) (apply #'server:websocket-server args))
(defun make-server (&rest args) (apply #'server:make-server args))
(defun start-server (server) (server:start-server server))
(defun stop-server (server) (server:stop-server server))
(defun websocket-server-options (&rest args) (apply #'server:websocket-server-options args))
(defun make-server-options (&rest args) (apply #'server:make-server-options args))
(defun websocket-handler (&rest args) (apply #'server:websocket-handler args))
(defun register-handler (server path handler) 
  (server:register-handler server path handler))
(defun unregister-handler (server path) (server:unregister-handler server path))
(defun get-connections (server) (server:get-connections server))
(defun broadcast-text (server text) 
  (server:broadcast-text server text))
(defun broadcast-binary (server data) 
  (server:broadcast-binary server data))
(defun close-all-connections (server) (server:close-all-connections server))
(defun websocket-upgrade-handler (server) (server:websocket-upgrade-handler server))
(defun add-websocket-to-http-server (http-server ws-server &key path) 
  (server:add-websocket-to-http-server http-server ws-server :path path))
(defmacro with-websocket-server ((server &rest options) &body body)
  `(server:with-websocket-server (,server ,@options) ,@body))
(defun make-echo-handler () (server:make-echo-handler))
(defun run-echo-server (&rest args) (apply #'server:run-echo-server args))

;;; Handshake operations
(defun validate-websocket-request (request) (handshake:validate-websocket-request request))
(defun generate-accept-key (key) (handshake:generate-accept-key key))
(defun valid-handshake-response-p (accept-key request-key) 
  (handshake:valid-handshake-response-p accept-key request-key))
(defun create-handshake-response (request &rest args) 
  (apply #'handshake:create-handshake-response request args))
(defun generate-websocket-key () (handshake:generate-websocket-key))
(defun generate-handshake-headers (host path) (handshake:generate-handshake-headers host path))
(defun create-upgrade-request (uri &rest args) 
  (apply #'handshake:create-upgrade-request uri args))
(defun validate-handshake-response (response key) 
  (handshake:validate-handshake-response response key))
(defun negotiate-subprotocol (client server) 
  (handshake:negotiate-subprotocol client server))
(defun negotiate-extensions (client server) 
  (handshake:negotiate-extensions client server))
(defun websocket-request-p (request) (handshake:websocket-request-p request))
(defun extract-websocket-key (request) (handshake:extract-websocket-key request))
(defun extract-subprotocols (request) (handshake:extract-subprotocols request))
(defun extract-extensions (request) (handshake:extract-extensions request))

;;; Utility functions

(defun websocket-version ()
  "Return WebSocket implementation version"
  "RFC 6455 compliant WebSocket implementation v0.1.0")

(defun supported-protocols ()
  "Return list of supported WebSocket subprotocols"
  '())

(defun supported-extensions ()
  "Return list of supported WebSocket extensions" 
  '())

;;; Example usage functions

(defun simple-echo-test (&key (host "localhost") (port 8080))
  "Run a simple echo test between client and server"
  (let ((server (make-server :host host :port port)))
    (register-handler server "/" (make-echo-handler))
    (start-server server)
    
    (unwind-protect
         (let ((uri (format nil "ws://~A:~A/" host port)))
           (echo-client uri "Hello, WebSocket!"))
      (stop-server server))))

(defun benchmark-throughput (&key (host "localhost") (port 8080) 
                                 (message-count 1000) (message-size 1024))
  "Benchmark WebSocket throughput"
  (let ((server (make-server :host host :port port))
        (message (make-string message-size :initial-element #\A))
        (received-count 0))
    
    ;; Set up echo handler
    (register-handler server "/" 
                     (make-websocket-handler
                      :on-message (lambda (conn msg is-text)
                                   (declare (ignore is-text))
                                   (send-text conn msg)
                                   (incf received-count))))
    
    (start-server server)
    
    (unwind-protect
         (let ((start-time (get-internal-real-time))
               (uri (format nil "ws://~A:~A/" host port)))
           
           (with-websocket-connection (conn uri)
             (dotimes (i message-count)
               (send-text conn message))
             
             ;; Wait for all responses
             (loop while (< received-count message-count)
                   do (sleep 0.001))
             
             (let* ((end-time (get-internal-real-time))
                    (elapsed (/ (- end-time start-time) internal-time-units-per-second))
                    (throughput (/ message-count elapsed))
                    (bytes-per-sec (/ (* message-count message-size) elapsed)))
               
               (format t "Benchmark Results:~%")
               (format t "  Messages: ~D~%" message-count)
               (format t "  Message size: ~D bytes~%" message-size)
               (format t "  Time: ~,3F seconds~%" elapsed)
               (format t "  Throughput: ~,1F messages/sec~%" throughput)
               (format t "  Bandwidth: ~,1F KB/sec~%" (/ bytes-per-sec 1024)))))
      
      (stop-server server))))
