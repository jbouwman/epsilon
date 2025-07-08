(defpackage :epsilon.http.server
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:str #:epsilon.lib.string)
   (#:map #:epsilon.lib.map)
   (#:time #:epsilon.lib.time)
   (#:thread #:epsilon.sys.thread)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response))
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
   (running-p :initform t :accessor server-running-p)))

(defun read-http-request (connection)
  "Read HTTP request from connection and return request object"
  (let ((stream (net:socket-stream connection)))
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

(defun send-http-response (connection response-obj)
  "Send HTTP response object to connection"
  (let ((stream (net:socket-stream connection)))
    (when stream
      (let ((response-text (response:response-to-string response-obj)))
        (write-string response-text stream)
        (force-output stream)))))

(defun find-handler (method path)
  "Find handler for method and path"
  (map:get *handlers* (format nil "~A ~A" method path)))

(defun handle-client (connection)
  "Handle a client connection"
  (handler-case
      (let ((req (read-http-request connection)))
        (if req
            (let ((handler (find-handler (request:request-method req) 
                                         (request:request-path req))))
              (if handler
                  (let ((response (funcall handler req)))
                    (send-http-response connection response))
                  (send-http-response connection 
                                      (response:html-response 
                                       "<h1>404 Not Found</h1>"
                                       :status 404))))
            (send-http-response connection 
                                (response:html-response 
                                 "<h1>400 Bad Request</h1>"
                                 :status 400))))
    (error (e)
      (ignore-errors
        (send-http-response connection 
                            (response:html-response 
                             (format nil "<h1>500 Internal Server Error</h1><p>~A</p>" e)
                             :status 500)))))
  
  (net:socket-close connection))

(defun server-loop (server)
  "Main server loop"
  (loop while (server-running-p server)
        do (handler-case
               (let ((connection (net:socket-accept (server-socket server))))
                 (when connection
                   (thread:spawn 
                    (lambda () 
                      (handle-client connection))
                    :name "HTTP client handler")))
             (error (e)
               (unless (server-running-p server)
                 (return))))))

(defun start-server (&key (port *default-port*) (address "0.0.0.0"))
  "Start HTTP server on specified port"
  (when (map:get *servers* port)
    (error "Server already running on port ~D" port))
  
  (let* ((listener (net:socket-listen address port))
         (server (make-instance 'http-server
                                :port port
                                :socket listener)))
    (setf (server-thread server)
          (thread:spawn 
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
      (thread:join (server-thread server))
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