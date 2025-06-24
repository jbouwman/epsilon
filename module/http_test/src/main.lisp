(defpackage :epsilon.http-test
  (:use :cl)
  (:local-nicknames
   (#:server #:epsilon.http.server)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response))
  (:export
   #:main
   #:start-test-server
   #:stop-test-server))

(in-package :epsilon.http-test)

(defvar *test-server* nil)

(defun hello-world-handler (req)
  "Handle requests to / with hello world HTML"
  (declare (ignore req))
  (response:html-response 
   "<!DOCTYPE html>
<html>
<head>
    <title>Hello World</title>
</head>
<body>
    <h1>hello, world!</h1>
    <p>This is epsilon HTTP test server running on Darwin.</p>
</body>
</html>"))

(defun start-test-server (&key (port 8080))
  "Start the test HTTP server"
  (when *test-server*
    (error "Test server is already running"))
  
  ;; Define the hello world handler for GET /
  (server:define-handler (get "/") (req)
    (hello-world-handler req))
  
  ;; Start the server
  (setf *test-server* (server:start-server :port port))
  (format t "HTTP test server started on port ~A~%" port)
  (format t "Visit http://localhost:~A/ to see hello world~%" port)
  *test-server*)

(defun stop-test-server ()
  "Stop the test HTTP server"
  (when *test-server*
    (server:stop-server *test-server*)
    (setf *test-server* nil)
    (format t "HTTP test server stopped~%")))

(defun main ()
  "Main entry point for the HTTP test application"
  (format t "Starting epsilon HTTP test application...~%")
  
  ;; Start server
  (start-test-server :port 8080)
  
  ;; Keep running until interrupted
  (handler-case
      (loop 
        (sleep 1))
    (sb-sys:interactive-interrupt ()
      (format t "~%Shutting down...~%")
      (stop-test-server))))