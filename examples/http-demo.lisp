;;;; epsilon.http Demo - HTTP Client and Server Example
;;;;
;;;; This example demonstrates a complete HTTP client and server application
;;;; using epsilon.http with JSON API endpoints.

(load "scripts/epsilon.lisp")

;; Build dependencies
(epsilon.tool.build:build "epsilon.http")

(defpackage :http-demo
  (:use :cl)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:server #:epsilon.http.server)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:json #:epsilon.json)))

(in-package :http-demo)

;;; Sample Data
(defparameter *users* 
  (list (map:make-map "id" 1 "name" "Alice" "email" "alice@example.com")
        (map:make-map "id" 2 "name" "Bob" "email" "bob@example.com")))

(defparameter *next-user-id* 3)

;;; Helper Functions
(defun find-user-by-id (id)
  "Find user by ID"
  (find-if (lambda (user) (= (map:get user "id") id)) *users*))

(defun create-user (name email)
  "Create a new user"
  (let ((user (map:make-map "id" *next-user-id* 
                           "name" name 
                           "email" email)))
    (push user *users*)
    (incf *next-user-id*)
    user))

;;; API Handlers

;; GET /api/users - List all users
(server:define-handler (:get "/api/users") (req)
  (response:json-response (map:make-map "users" *users*)))

;; GET /api/users/:id - Get user by ID (simplified - just parse from query)
(server:define-handler (:get "/api/user") (req)
  (let* ((params (request:request-params req))
         (id-str (map:get params "id"))
         (id (when id-str (parse-integer id-str :junk-allowed t))))
    (if (and id (find-user-by-id id))
        (response:json-response (find-user-by-id id))
        (response:make-response :status 404
                               :headers (map:make-map "Content-Type" "application/json")
                               :body "{\"error\": \"User not found\"}"))))

;; POST /api/users - Create new user  
(server:define-handler (:post "/api/users") (req)
  (handler-case
      (let* ((body (request:request-body req))
             (data (when body (json:parse body)))
             (name (map:get data "name"))
             (email (map:get data "email")))
        (if (and name email)
            (let ((user (create-user name email)))
              (response:json-response user :status 201))
            (response:make-response 
             :status 400
             :headers (map:make-map "Content-Type" "application/json")
             :body "{\"error\": \"Name and email required\"}")))
    (error (e)
      (response:make-response 
       :status 400
       :headers (map:make-map "Content-Type" "application/json")
       :body (format nil "{\"error\": \"Invalid JSON: ~A\"}" e)))))

;; GET / - Home page
(server:define-handler (:get "/") (req)
  (response:html-response 
   "<html>
<head><title>epsilon.http Demo</title></head>
<body>
<h1>epsilon.http Demo Server</h1>
<p>Welcome to the epsilon.http demonstration server!</p>

<h2>Available API Endpoints:</h2>
<ul>
  <li><strong>GET /api/users</strong> - List all users</li>
  <li><strong>GET /api/user?id=N</strong> - Get user by ID</li>
  <li><strong>POST /api/users</strong> - Create new user (JSON: {\"name\": \"...\", \"email\": \"...\"})</li>
</ul>

<h2>Try the API:</h2>
<pre>
# List users
curl http://localhost:8080/api/users

# Get specific user
curl http://localhost:8080/api/user?id=1

# Create new user
curl -X POST http://localhost:8080/api/users \\
     -H \"Content-Type: application/json\" \\
     -d '{\"name\": \"Charlie\", \"email\": \"charlie@example.com\"}'
</pre>
</body>
</html>"))

;;; Demo Functions

(defun demo-server ()
  "Start the demo HTTP server"
  (format t "~%Starting epsilon.http demo server on port 8080...~%")
  (format t "Visit http://localhost:8080 in your browser~%")
  (format t "Press Ctrl+C to stop the server~%~%")
  
  (let ((server (server:start-server :port 8080)))
    (handler-case
        (loop (sleep 1))  ; Keep running
      (sb-sys:interactive-interrupt ()
        (format t "~%Shutting down server...~%")
        (server:stop-server server)
        (format t "Server stopped.~%")))))

(defun demo-client ()
  "Demonstrate HTTP client functionality"
  (format t "~%=== epsilon.http Client Demo ===~%~%")
  
  ;; Test basic GET request
  (format t "1. Testing GET request to httpbin.org...~%")
  (handler-case
      (multiple-value-bind (status headers body)
          (client:http-get "http://httpbin.org/get")
        (format t "   Status: ~A~%" status)
        (format t "   Response received (~A bytes)~%" (length body)))
    (error (e)
      (format t "   Error: ~A~%" e)))
  
  ;; Test POST request
  (format t "~%2. Testing POST request with JSON data...~%")
  (handler-case
      (let ((json-data (with-output-to-string (s)
                         (json:encode 
                          (map:make-map "name" "test-user" "data" "demo") s))))
        (multiple-value-bind (status headers body)
            (client:http-post "http://httpbin.org/post"
                             :body json-data
                             :headers (map:make-map 
                                      "Content-Type" "application/json"))
          (format t "   Status: ~A~%" status)
          (format t "   Response received (~A bytes)~%" (length body))))
    (error (e)
      (format t "   Error: ~A~%" e)))
  
  ;; Test URL parsing
  (format t "~%3. Testing URL parsing...~%")
  (let ((test-urls '("http://example.com/path?query=value"
                     "https://api.example.com:8080/users/123"
                     "http://localhost:3000/")))
    (dolist (url test-urls)
      (multiple-value-bind (scheme host port path query)
          (client::parse-url url)
        (format t "   URL: ~A~%" url)
        (format t "     Scheme: ~A, Host: ~A, Port: ~A~%" scheme host port)
        (format t "     Path: ~A, Query: ~A~%~%" path query))))
  
  (format t "Client demo complete!~%"))

;;; Main Demo Function

(defun main ()
  "Run the epsilon.http demonstration"
  (format t "=== epsilon.http Package Demonstration ===~%")
  (format t "This demo shows HTTP client and server functionality.~%~%")
  
  (format t "Choose a demo:~%")
  (format t "1. HTTP Client Demo~%")
  (format t "2. HTTP Server Demo~%")
  (format t "Choice (1 or 2): ")
  (force-output)
  
  (let ((choice (read-line)))
    (cond
      ((string= choice "1")
       (demo-client))
      ((string= choice "2")
       (demo-server))
      (t
       (format t "Invalid choice. Running client demo...~%")
       (demo-client)))))

;; Run the demo if this file is executed directly
(when (member "--demo" sb-ext:*posix-argv* :test #'string=)
  (main))