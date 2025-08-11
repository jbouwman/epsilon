;;;; TODO
;;;;
;;;; - Add a test for wrap-middleware
;;;; - middleware is an annoying term

(defpackage epsilon.web
  (:use cl)
  (:local-nicknames
   (str epsilon.string)
   (seq epsilon.sequence)
   (map epsilon.map)
   (json epsilon.json)
   (log epsilon.log)
   (request epsilon.http.request)
   (response epsilon.http.response)
   (server epsilon.http.server)
   (client epsilon.http.client))
  (:export
   ;; Request helpers
   json-request-p
   parse-query-string
   request-param
   request-header
   with-json-body
   
   ;; Response builders
   json
   html
   text
   redirect
   not-found
   bad-request
   internal-error
   
   ;; Route definition
   defroutes
   defhandler
   handle-routes
   route
   make-route
   route-method
   route-path
   route-handler
   
   ;; Server control
   start-server
   stop-server
   with-server
   
   ;; Middleware
   defmiddleware
   wrap-middleware
   logging-middleware
   json-errors-middleware))

(in-package epsilon.web)

;;;; Request Helpers

(defun json-request-p (request)
  "Check if request has JSON content type"
  (let ((content-type (or (map:get (request:request-headers request) "content-type")
                          (map:get (request:request-headers request) "Content-Type")
                          "")))
    (search "application/json" content-type)))

(defun parse-query-string (query-string)
  "Parse URL query string into a map"
  (if (or (null query-string) (zerop (length query-string)))
      map:+empty+
      (let ((pairs (str:split #\& query-string)))
        (seq:reduce (lambda (m pair)
                      (let ((kv (seq:realize (str:split #\= pair))))
                        (if (= (length kv) 2)
                            (map:assoc m (first kv) (second kv))
                            m)))
                    pairs
                    :initial-value map:+empty+))))

(defun request-param (request name &optional default)
  "Get parameter from query string or POST body"
  (or 
   ;; Check query parameters
   (map:get (request:request-params request) name)
   ;; Check JSON body if present
   (when (and (json-request-p request)
              (request:request-body request))
     (handler-case
         (let ((parsed-json (json:parse (request:request-body request))))
           (if (listp parsed-json)
               ;; Convert alist to map
               (map:get (map:from-pairs parsed-json) name)
               ;; Already a map or other object
               (map:get parsed-json name)))
       (error () nil)))
   ;; Return default
   default))

(defun request-header (request name &optional default)
  "Get request header value"
  (map:get (request:request-headers request) name default))

(defmacro with-json-body ((var request) &body body)
  "Parse JSON body and bind to variable"
  (let ((req-var (gensym)))
    `(let ((,req-var ,request))
       (if (json-request-p ,req-var)
           (handler-case
               (let* ((parsed-json (json:parse (request:request-body ,req-var)))
                      (,var (if (listp parsed-json)
                                (map:from-pairs parsed-json)
                                parsed-json)))
                 ,@body)
             (error (e)
               (bad-request (format nil "Invalid JSON: ~A" e))))
           (bad-request "Content-Type must be application/json")))))

;;;; Response Builders

(defun json (data &key (status 200) headers)
  "Create JSON response"
  (declare (ignore headers))  ; TODO: implement custom headers
  (response:json-response data :status status))

(defun html (content &key (status 200) headers)
  "Create HTML response"
  (declare (ignore headers))  ; TODO: implement custom headers
  (response:html-response content :status status))

(defun text (content &key (status 200) headers)
  "Create plain text response"
  (declare (ignore headers))  ; TODO: implement custom headers
  (response:text-response content :status status))

(defun redirect (location &key (status 302))
  "Create redirect response"
  (response:redirect location :status status))

(defun not-found (&optional message)
  "Create 404 response"
  (html (or message "<h1>404 Not Found</h1>") :status 404))

(defun bad-request (&optional message)
  "Create 400 response"
  (json (map:make-map "error" (or message "Bad Request")) :status 400))

(defun internal-error (&optional message)
  "Create 500 response"
  (json (map:make-map "error" (or message "Internal Server Error")) :status 500))

;;;; Route Definition

(defstruct route
  method
  path
  handler)

(defmacro defroutes (name &body routes)
  "Define routes more elegantly"
  `(defparameter ,name
     (list ,@(loop for (method path handler) in routes
                   collect `(make-route :method ,(string-upcase (string method))
                                       :path ,path
                                       :handler ,handler)))))

(defmacro defhandler (name (request-var) &body body)
  "Define a handler with automatic error handling"
  `(defun ,name (,request-var)
     (declare (ignorable ,request-var))
     (handler-case
         (progn ,@body)
       (error (e)
         (internal-error (format nil "~A" e))))))

;;;; Route Handling

(defun find-route-handler (routes method path)
  "Find handler for given method and path"
  (loop for route in routes
        when (and (string= (route-method route) method)
                  (string= (route-path route) path))
          return (route-handler route)))

(defun handle-routes (routes)
  "Create a handler function that dispatches based on routes"
  (lambda (request)
    (let* ((method (request:request-method request))
           (path (request:request-path request))
           (handler (find-route-handler routes method path)))
      (if handler
          (funcall handler request)
          (not-found)))))

;;;; Server Control

(defun start-server (routes &key (port 8080) (address "0.0.0.0") middleware)
  "Start web server with routes and optional middleware"
  (let* ((route-handler (handle-routes routes))
         (app (if middleware
                  (apply #'wrap-middleware route-handler middleware)
                  route-handler)))
    (server:start-server app :port port :address address)))

(defun stop-server (server-or-port)
  "Stop web server"
  (server:stop-server server-or-port))

(defmacro with-server ((server routes &key (port 8080) middleware) &body body)
  "Execute body with a temporary web server"
  `(let ((,server (start-server ,routes :port ,port :middleware ,middleware)))
     (unwind-protect
          (progn ,@body)
       (stop-server ,server))))

;;;; Basic Middleware Support

(defmacro defmiddleware (name doc-string lambda-form)
  "Define middleware function"
  `(progn
     (defparameter ,name ,lambda-form)
     (setf (documentation ',name 'variable) ,doc-string)
     ,name))

(defun wrap-middleware (handler &rest middlewares)
  "Wrap handler with multiple middleware functions"
  (seq:reduce (lambda (h mw) (funcall mw h))
              (seq:from-list middlewares)
              :initial-value handler))

;;;; Built-in Middleware

(defmiddleware logging-middleware
  "Log all requests with timing"
  (lambda (handler)
    (lambda (req)
      (let ((start (get-internal-real-time)))
        (let ((res (funcall handler req)))
          (log:info "~A ~A -> ~A (~Dms)"
                    (request:request-method req)
                    (request:request-path req)
                    (response:response-status res)
                    (round (/ (- (get-internal-real-time) start) 
                             internal-time-units-per-second)
                           1000))
          res)))))

(defmiddleware json-errors-middleware
  "Convert errors to JSON responses"
  (lambda (handler)
    (lambda (req)
      (handler-case
          (funcall handler req)
        (error (e)
          (json (map:make-map "error" (format nil "~A" e)
                             "type" (string (type-of e)))
                :status 500))))))
