(defpackage :epsilon.http.response
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:map #:epsilon.map))
  (:export
   #:make-response
   #:response-status
   #:response-headers
   #:response-body
   #:set-header
   #:set-cookie
   #:redirect
   #:json-response
   #:html-response
   #:text-response
   #:response-to-string))

(in-package :epsilon.http.response)

(defclass http-response ()
  ((status :initarg :status :accessor response-status :initform 200)
   (headers :initarg :headers :accessor response-headers :initform map:+empty+)
   (body :initarg :body :accessor response-body :initform nil)))

(defun make-response (&key (status 200) headers body)
  "Create an HTTP response object"
  (make-instance 'http-response
                 :status status
                 :headers (or headers map:+empty+)
                 :body body))

(defun set-header (response header-name header-value)
  "Set a response header"
  (setf (response-headers response)
        (map:assoc (response-headers response) 
                   header-name 
                   header-value))
  response)

(defun set-cookie (response name value &key path domain expires secure http-only)
  "Set a cookie in the response"
  (let ((cookie-parts (list (format nil "~A=~A" name value))))
    (when path
      (push (format nil "Path=~A" path) cookie-parts))
    (when domain
      (push (format nil "Domain=~A" domain) cookie-parts))
    (when expires
      (push (format nil "Expires=~A" expires) cookie-parts))
    (when secure
      (push "Secure" cookie-parts))
    (when http-only
      (push "HttpOnly" cookie-parts))
    
    (set-header response "Set-Cookie" 
                (str:join (reverse cookie-parts) "; "))))

(defun redirect (url &key (status 302))
  "Create a redirect response"
  (make-response :status status
                 :headers (map:make-map "Location" url)))

(defun json-response (data &key (status 200))
  "Create a JSON response"
  (make-response :status status
                 :headers (map:make-map "Content-Type" "application/json")
                 :body (epsilon.json:to-json data)))

(defun html-response (html &key (status 200))
  "Create an HTML response"
  (make-response :status status
                 :headers (map:make-map "Content-Type" "text/html; charset=utf-8")
                 :body html))

(defun text-response (text &key (status 200))
  "Create a plain text response"
  (make-response :status status
                 :headers (map:make-map "Content-Type" "text/plain; charset=utf-8")
                 :body text))

(defun status-text (status-code)
  "Get status text for HTTP status code"
  (case status-code
    (200 "OK")
    (201 "Created")
    (204 "No Content")
    (301 "Moved Permanently")
    (302 "Found")
    (304 "Not Modified")
    (400 "Bad Request")
    (401 "Unauthorized")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (t "Unknown")))

(defun response-to-string (response)
  "Convert HTTP response to string format for transmission"
  (let ((body (response-body response))
        (headers (response-headers response))
        (status (response-status response)))
    
    ;; Add Content-Length header if body exists and header not set
    (when (and body (not (map:get headers "content-length")))
      (setf headers (map:assoc headers "content-length" 
                               (format nil "~A" (length body)))))
    
    ;; Build response string
    (with-output-to-string (out)
      ;; Status line
      (format out "HTTP/1.1 ~A ~A~C~C" 
              status 
              (status-text status)
              #\Return #\Newline)
      
      ;; Headers
      (map:each (lambda (key value)
                  (format out "~A: ~A~C~C" key value #\Return #\Newline))
                headers)
      
      ;; Empty line between headers and body
      (format out "~C~C" #\Return #\Newline)
      
      ;; Body
      (when body
        (write-string body out)))))
