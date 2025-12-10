;;;; Simple HTTP/2 Server
;;;;
;;;; A minimal HTTP/2 server for testing with curl --http2

(defpackage :http2-simple-server
  (:use :cl)
  (:local-nicknames
   (#:h2 #:epsilon.http2)
   (#:crypto #:epsilon.crypto)
   (#:net #:epsilon.net)))

(in-package :http2-simple-server)

(defparameter *server* nil "The running server instance")
(defparameter *port* 8443 "Default server port")

(defun handle-request (headers body)
  "Simple request handler"
  (declare (ignore body))
  (let ((path (cdr (assoc ":path" headers :test #'string=)))
        (method (cdr (assoc ":method" headers :test #'string=))))
    
    (format t "~%Request: ~A ~A~%" method path)
    (format t "Headers:~%")
    (dolist (header headers)
      (unless (char= (char (car header) 0) #\:)
        (format t "  ~A: ~A~%" (car header) (cdr header))))
    
    ;; Generate response based on path
    (cond
      ((string= path "/")
       (list :status 200
             :headers '(("content-type" . "text/html"))
             :body "<html><body><h1>HTTP/2 Server Running!</h1><p>Epsilon HTTP/2 implementation</p></body></html>"))
      
      ((string= path "/api/status")
       (list :status 200
             :headers '(("content-type" . "application/json"))
             :body "{\"status\":\"ok\",\"protocol\":\"HTTP/2\",\"server\":\"Epsilon\"}"))
      
      ((string= path "/test")
       (list :status 200
             :headers '(("content-type" . "text/plain"))
             :body "This is a test response from the HTTP/2 server"))
      
      (t
       (list :status 404
             :headers '(("content-type" . "text/plain"))
             :body "Not Found")))))

(defun start-server (&key (port *port*) cert-file key-file)
  "Start the HTTP/2 server"
  (when *server*
    (stop-server))
  
  (format t "Starting HTTP/2 server on port ~D~%" port)
  
  ;; Generate self-signed cert if not provided
  (unless (and cert-file key-file)
    (format t "Generating self-signed certificate...~%")
    (multiple-value-bind (cert key)
        (epsilon.crypto.certificates:generate-self-signed-certificate
         "localhost"
         :days 365
         :key-bits 2048)
      
      ;; Save to temp files
      (setf cert-file "/tmp/http2-cert.pem"
            key-file "/tmp/http2-key.pem")
      
      (epsilon.crypto.certificates:save-certificate cert cert-file)
      (epsilon.crypto.certificates:save-private-key key key-file)
      
      (format t "Certificate saved to ~A~%" cert-file)
      (format t "Private key saved to ~A~%" key-file)))
  
  ;; Start server with TLS and ALPN
  (setf *server*
        (h2:http2-server
         :port port
         :ssl-p t
         :cert-file cert-file
         :key-file key-file
         :handler #'handle-request))
  
  (format t "~%Server started successfully!~%")
  (format t "Test with: curl --http2 -k https://localhost:~D/~%" port)
  (format t "~%Press Ctrl-C to stop~%~%"))

(defun stop-server ()
  "Stop the HTTP/2 server"
  (when *server*
    (format t "Stopping server...~%")
    ;; Server stop logic here
    (setf *server* nil)))

;; Simple test client
(defun test-client (&key (host "localhost") (port *port*))
  "Test the server with a simple HTTP/2 client"
  (handler-case
      (let ((response (h2:http2-get 
                      (format nil "https://~A:~D/" host port)
                      :headers '(("accept" . "text/html")))))
        (format t "Response received:~%")
        (format t "Headers: ~A~%" (getf response :headers))
        (format t "Body: ~A~%" (getf response :body)))
    (error (e)
      (format t "Error: ~A~%" e))))

;; Export functions
(export '(start-server stop-server test-client))