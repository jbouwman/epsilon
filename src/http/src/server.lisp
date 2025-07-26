(defpackage :epsilon.http.server
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.http.net)
   (#:str #:epsilon.string)
   (#:map #:epsilon.map)
   (#:time #:epsilon.time)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:tls #:epsilon.tls))
  (:export
   #:start-server
   #:stop-server
   #:define-handler
   #:with-server
   #:*default-port*))

(in-package :epsilon.http.server)

(defparameter *default-port* 8080)
(defvar *handlers* map:+empty+)
(defvar *servers* map:+empty+)

(defclass http-server ()
  ((port :initarg :port :accessor server-port)
   (socket :initarg :socket :accessor server-socket)
   (thread :initarg :thread :accessor server-thread)
   (running-p :initform t :accessor server-running-p)
   (tls-context :initarg :tls-context :accessor server-tls-context :initform nil)
   (ssl-p :initarg :ssl-p :accessor server-ssl-p :initform nil)))

(defun read-http-request (connection ssl-p)
  "Read HTTP request from connection and return request object"
  (let ((stream (if ssl-p
                    (tls:tls-stream connection)
                    (net:socket-stream connection))))
    (when stream
      (let ((request-text ""))
        ;; Read request line and headers
        (loop for line = (read-line stream nil nil)
              while (and line (> (length line) 0))
              do (setf request-text (concatenate 'string request-text line 
                                                 (format nil "~C~C" #\Return #\Newline))))
        
        ;; Add final CRLF to mark end of headers
        (setf request-text (concatenate 'string request-text 
                                        (format nil "~C~C" #\Return #\Newline)))
        
        ;; Parse the complete request
        (when (> (length request-text) 0)
          (request:parse-http-request request-text))))))

(defun send-http-response (connection response-obj ssl-p)
  "Send HTTP response object to connection"
  (let ((stream (if ssl-p
                    (tls:tls-stream connection)
                    (net:socket-stream connection))))
    (when stream
      (let ((response-text (response:response-to-string response-obj)))
        (write-string response-text stream)
        (force-output stream)))))

(defun find-handler (method path)
  "Find handler for method and path"
  (map:get *handlers* (format nil "~A ~A" method path)))

(defun handle-client (connection ssl-p)
  "Handle a client connection"
  (handler-case
      (let ((req (read-http-request connection ssl-p)))
        (if req
            (let ((handler (find-handler (request:request-method req) 
                                         (request:request-path req))))
              (if handler
                  (let ((response (funcall handler req)))
                    (send-http-response connection response ssl-p))
                  (send-http-response connection 
                                      (response:html-response 
                                       "<h1>404 Not Found</h1>"
                                       :status 404)
                                      ssl-p)))
            (send-http-response connection 
                                (response:html-response 
                                 "<h1>400 Bad Request</h1>"
                                 :status 400)
                                ssl-p)))
    (error (e)
      (ignore-errors
        (send-http-response connection 
                            (response:html-response 
                             (format nil "<h1>500 Internal Server Error</h1><p>~A</p>" e)
                             :status 500)
                            ssl-p))))
  
  ;; Close connection properly
  (if ssl-p
      (tls:tls-close connection)
      (net:socket-close connection)))

(defun server-loop (server)
  "Main server loop"
  (loop while (server-running-p server)
        do (handler-case
               (let ((raw-connection (net:socket-accept (server-socket server))))
                 (when raw-connection
                   (let ((connection (if (server-ssl-p server)
                                         (tls:tls-accept raw-connection 
                                                         (server-tls-context server))
                                         raw-connection)))
                     (sb-thread:make-thread 
                      (lambda () 
                        (handle-client connection (server-ssl-p server)))
                      :name "HTTP client handler"))))
             (error (e)
               (declare (ignore e))  ; Log errors in production
               (unless (server-running-p server)
                 (return))))))

(defun start-server (&key (port *default-port*) (address "0.0.0.0") 
                           tls-context ssl-p cert-file key-file)
  "Start HTTP server on specified port"
  (when (map:get *servers* port)
    (error "Server already running on port ~D" port))
  
  ;; Create TLS context if SSL is requested
  (when (or ssl-p cert-file key-file)
    (unless tls-context
      (setf tls-context (tls:create-tls-context :server-p t))
      (when cert-file
        (tls:load-cert-file tls-context cert-file))
      (when key-file  
        (tls:load-key-file tls-context key-file))
      (tls:set-verify-mode tls-context tls:+tls-verify-none+)))
  
  (let* ((listener (net:socket-listen address port))
         (server (make-instance 'http-server
                                :port port
                                :socket listener
                                :tls-context tls-context
                                :ssl-p (or ssl-p (not (null tls-context))))))
    (setf (server-thread server)
          (sb-thread:make-thread 
           (lambda () 
             (server-loop server))
           :name (format nil "HTTP server on port ~D" port)))
    
    (setf *servers* (map:assoc *servers* port server))
    server))

(defun stop-server (port-or-server)
  "Stop HTTP server"
  (let ((server (if (typep port-or-server 'http-server)
                    port-or-server
                    (map:get *servers* port-or-server))))
    (when server
      (setf (server-running-p server) nil)
      (net:socket-close (server-socket server))
      (sb-thread:join-thread (server-thread server))
      (setf *servers* (map:dissoc *servers* (server-port server)))
      t)))

(defmacro with-server ((server &key (port *default-port*)) &body body)
  "Execute body with HTTP server running"
  `(let ((,server (start-server :port ,port)))
     (unwind-protect
          (progn ,@body)
       (stop-server ,server))))

(defmacro define-handler ((method path) (request-var) &body handler-body)
  "Define an HTTP request handler"
  `(setf *handlers* (map:assoc *handlers* (format nil "~A ~A" ',method ,path)
                               (lambda (,request-var)
                                 ,@handler-body))))
