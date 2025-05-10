(defpackage #:epsilon.net.msgpack-http
  (:use
   #:cl
   #:epsilon.lib.binding)
  (:local-nicknames
   (#:http #:epsilon.net.http)
   (#:server #:epsilon.net.http.server)
   (#:msgpack #:epsilon.lib.msgpack)
   (#:map #:epsilon.lib.map)
   (#:time #:epsilon.lib.time))
  (:export
   ;; Server
   #:make-server
   #:start-server
   #:stop-server
   #:define-handler
   #:with-msgpack-response
   #:msgpack-handler
   
   ;; Client
   #:make-client
   #:request
   #:get
   #:post
   #:put
   #:delete))

(in-package #:epsilon.net.msgpack-http)

;;;; MessagePack HTTP Client/Server Implementation
;;;;
;;;; This module provides HTTP client and server implementations
;;;; with MessagePack as the serialization format for request/response bodies.

;; Constants
(defparameter *msgpack-content-type* "application/msgpack"
  "MIME type for MessagePack content")

;;;; Server Implementation

(defun msgpack-handler (handler-fn)
  "Wraps a handler function to automatically decode/encode MessagePack data."
  (lambda (request response)
    (let* ((content-type (gethash "content-type" (server:request-headers request)))
           (body (server:request-body request))
           (decoded-body (when (and body 
                                   (string= content-type *msgpack-content-type*))
                          (msgpack:decode body))))
      
      ;; Call the handler function with decoded body
      (let ((result (funcall handler-fn request decoded-body)))
        ;; Set appropriate headers and encode result
        (server:set-header response "Content-Type" *msgpack-content-type*)
        
        ;; Encode response
        (let ((encoded-result (msgpack:encode result)))
          (server:set-header response "Content-Length" (length encoded-result))
          (setf (server:response-body response) encoded-result))))))

(defmacro with-msgpack-response ((response result) &body body)
  "Set up a response for returning MessagePack data"
  `(progn
     ;; Execute the body which should set the result
     (let ((,result ,@body))
       ;; Set appropriate headers and encode result
       (server:set-header ,response "Content-Type" *msgpack-content-type*)
       (let ((encoded-result (msgpack:encode ,result)))
         (server:set-header ,response "Content-Length" (length encoded-result))
         (setf (server:response-body ,response) encoded-result)))))

(defun make-server (&key (port 8080) (address "127.0.0.1"))
  "Create a new HTTP server for MessagePack-valued communication"
  (let ((router (server:make-router)))
    ;; Create the server with the router as the handler
    (server:make-server 
     (lambda (request response)
       (server:route router request response))
     :port port
     :address address)))

(defun start-server (server)
  "Start the server"
  (server:start server))

(defun stop-server (server)
  "Stop the server"
  (server:stop server))

(defun register-route (server path handler &key (method :any))
  "Register a route on the server"
  (server:connect-route 
   (server:server-handler server) 
   path 
   (msgpack-handler handler)
   :method method))

(defmacro define-handler (server path args &body body)
  "Define a MessagePack handler and register it with the server"
  (let ((request-var (first args))
        (body-var (second args)))
    `(register-route ,server ,path
                    (lambda (,request-var ,body-var)
                      ,@body))))

;;;; Client Implementation

(defclass msgpack-client ()
  ((base-url :initarg :base-url
             :reader client-base-url
             :documentation "Base URL for the client")
   (headers :initarg :headers
            :initform (map:+empty+)
            :accessor client-headers
            :documentation "Default headers for requests")))

(defun make-client (base-url &key headers)
  "Create a new MessagePack HTTP client"
  (make-instance 'msgpack-client
                :base-url base-url
                :headers (if headers
                            (reduce (lambda (m k v) 
                                     (map:assoc m k v))
                                    headers
                                    :initial-value map:+empty+)
                            (map:assoc map:+empty+
                                    "Content-Type" *msgpack-content-type*
                                    "Accept" *msgpack-content-type*))))

(defun request (client method path &key body headers)
  "Make a request to the server with optional MessagePack body"
  (let* ((url (concatenate 'string (client-base-url client) path))
         (request-headers (if headers
                             (reduce (lambda (m k v) 
                                      (map:assoc m k v))
                                    headers
                                    :initial-value (client-headers client))
                             (client-headers client)))
         ;; Encode body if present
         (encoded-body (when body (msgpack:encode body)))
         ;; Make the request
         (response (http:request method url
                               :headers request-headers
                               :body encoded-body)))
    
    ;; Decode the response body if it's MessagePack
    (let* ((content-type (map:get (getf response :headers) "content-type"))
           (body (getf response :body)))
      (if (and body (string= content-type *msgpack-content-type*))
          ;; Return decoded response
          (list :status (getf response :status)
                :headers (getf response :headers)
                :body (msgpack:decode body))
          ;; Return raw response
          response))))

(defun get (client path &key headers)
  "Make a GET request"
  (request client :get path :headers headers))

(defun post (client path body &key headers)
  "Make a POST request with MessagePack body"
  (request client :post path :body body :headers headers))

(defun put (client path body &key headers)
  "Make a PUT request with MessagePack body"
  (request client :put path :body body :headers headers))

(defun delete (client path &key headers)
  "Make a DELETE request"
  (request client :delete path :headers headers))

;;;; Example Echo and Message Routes

(defun add-echo-route (server)
  "Add an echo route that decodes and re-encodes MessagePack"
  (define-handler server "/echo" (request body)
    ;; Simply return the decoded body
    body))

(defun add-message-route (server)
  "Add a message route that returns 'OK'"
  (define-handler server "/message" (request body)
    ;; Return a simple OK message
    (map:make-map "status" "OK"
                "timestamp" (time:now))))