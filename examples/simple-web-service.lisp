;;;; Simple web service example for Epsilon
;;;; This demonstrates basic HTTP server functionality

;; First, we need to ensure required modules are loaded
;; In a real epsilon distribution, these would be loaded from the bundled repository

(defpackage :simple-web-service
  (:use :cl)
  (:export #:start-server #:main))

(in-package :simple-web-service)

;; Simple HTTP response formatting
(defun http-response (status content-type body)
  "Format an HTTP response"
  (format nil "HTTP/1.1 ~A~%Content-Type: ~A~%Content-Length: ~D~%~%~A"
          status content-type (length body) body))

;; Basic request parsing
(defun split-string (string &optional (separator #\Space))
  "Simple string splitting function"
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) separator)
          do (push (subseq string start i) result)
             (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun parse-request-line (line)
  "Parse HTTP request line"
  (let ((parts (split-string line)))
    (when (>= (length parts) 3)
      (list :method (first parts)
            :path (second parts)
            :version (third parts)))))

;; Simple TCP server using SBCL built-ins
(defun handle-client (client-stream)
  "Handle a single client connection"
  (handler-case
      (let* ((request-line (read-line client-stream nil))
             (request (when request-line (parse-request-line request-line))))
        
        ;; Skip headers for now
        (loop for line = (read-line client-stream nil)
              while (and line (> (length line) 0)))
        
        (when request
          (let ((response
                 (cond
                   ;; Home page
                   ((string= (getf request :path) "/")
                    (http-response "200 OK" "text/html"
                                  "<h1>Epsilon Web Service</h1>
                                   <p>A simple demonstration server</p>
                                   <ul>
                                     <li><a href=\"/health\">Health Check</a></li>
                                     <li><a href=\"/time\">Current Time</a></li>
                                   </ul>"))
                   
                   ;; Health check
                   ((string= (getf request :path) "/health")
                    (http-response "200 OK" "application/json"
                                  "{\"status\": \"healthy\"}"))
                   
                   ;; Current time
                   ((string= (getf request :path) "/time")
                    (http-response "200 OK" "application/json"
                                  (format nil "{\"time\": ~D}" (get-universal-time))))
                   
                   ;; 404 for everything else
                   (t
                    (http-response "404 Not Found" "text/plain"
                                  "Not Found")))))
            
            (write-string response client-stream)
            (force-output client-stream))))
    
    (error (e)
      (format *error-output* "Error handling client: ~A~%" e)))
  
  (close client-stream))

(defun start-server (&key (port 8080))
  "Start the HTTP server"
  (let ((server-socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream
                                     :protocol :tcp)))
    
    ;; Allow socket reuse
    (setf (sb-bsd-sockets:sockopt-reuse-address server-socket) t)
    
    ;; Bind and listen
    (sb-bsd-sockets:socket-bind server-socket 
                               (sb-bsd-sockets:make-inet-address "0.0.0.0")
                               port)
    (sb-bsd-sockets:socket-listen server-socket 5)
    
    (format t "Server listening on http://localhost:~D~%" port)
    (format t "Press Ctrl+C to stop~%~%")
    
    ;; Accept connections
    (unwind-protect
         (loop
           (let ((client-socket (sb-bsd-sockets:socket-accept server-socket)))
             (let ((client-stream (sb-bsd-sockets:socket-make-stream 
                                  client-socket
                                  :input t
                                  :output t
                                  :element-type 'character)))
               ;; Handle in same thread for simplicity
               (handle-client client-stream))))
      
      ;; Cleanup
      (sb-bsd-sockets:socket-close server-socket))))

(defun main ()
  "Main entry point"
  (handler-case
      (start-server)
    (sb-sys:interactive-interrupt ()
      (format t "~%Shutting down...~%"))
    (error (e)
      (format *error-output* "Server error: ~A~%" e)
      (sb-ext:exit :code 1))))

;; Instructions for use:
;; 
;; 1. With Epsilon REPL:
;;    epsilon
;;    (load "simple-web-service.lisp")
;;    (simple-web-service:start-server)
;;
;; 2. Direct execution:
;;    epsilon --load simple-web-service.lisp --eval "(simple-web-service:main)"
;;
;; 3. Test with curl:
;;    curl http://localhost:8080/
;;    curl http://localhost:8080/health
;;    curl http://localhost:8080/time