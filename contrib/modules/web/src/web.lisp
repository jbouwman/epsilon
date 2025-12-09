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
   (client epsilon.http.client)
   (routing epsilon.web.routing)
   (validation epsilon.web.validation)
   (session epsilon.web.session)
   (ws epsilon.websocket))
  (:export
   ;; Routing
   compile-route-pattern
   route-matches-p
   extract-route-params
   make-route
   find-handler
   defroutes
   handle-routes
   route-method
   
   ;; Request helpers
   request-value
   request-values
   with-request-values
   
   ;; Response builders
   respond
   redirect
   not-found
   bad-request
   unauthorized
   forbidden
   internal-error
   
   ;; Server control
   start-server
   stop-server
   with-server
   
   ;; Middleware
   make-middleware-chain
   add-middleware
   execute
   wrap-middleware
   logging-middleware
   json-errors-middleware
   static-files-middleware
   validation-middleware
   session-middleware
   
   ;; Content negotiation
   accepts-p
   preferred-type
   negotiate-content
   
   ;; WebSocket support
   websocket-upgrade-request-p
   make-websocket-handler
   websocket-handler-on-open
   websocket-handler-on-message
   websocket-handler-on-close
   websocket-broadcast
   route-raw-path))

(in-package epsilon.web)

;;;; Request Value Extraction

(defun request-value (request key &key (from :any) default)
  "Get value from request - :path, :query, :form, :json, :header, or :any"
  (flet ((get-from-source (source)
           (case source
             (:path (map:get (request:request-path-params request) key))
             (:query (map:get (request:request-params request) key))
             (:header (map:get (request:request-headers request) key))
             (:json (when (request:request-body request)
                     (handler-case
                         (let ((data (json:parse (request:request-body request))))
                           (if (listp data)
                               (map:get (map:from-pairs data) key)
                               (map:get data key)))
                       (error () nil))))
             (:form (parse-form-value request key)))))
    
    (if (eq from :any)
        (or (get-from-source :path)
            (get-from-source :query)
            (get-from-source :json)
            (get-from-source :form)
            (get-from-source :header)
            default)
        (or (get-from-source from) default))))

(defun request-values (request &rest keys)
  "Get multiple values from request"
  (map:from-pairs
   (loop for key in keys
         collect (cons key (request-value request key)))))

(defmacro with-request-values (bindings request &body body)
  "Bind multiple request values"
  (let ((req-var (gensym "REQ")))
    `(let ((,req-var ,request))
       (let ,(loop for binding in bindings
                   collect (if (listp binding)
                             `(,(first binding) (request-value ,req-var 
                                                              ,(second binding)
                                                              ,@(cddr binding)))
                             `(,binding (request-value ,req-var ',binding))))
         ,@body))))

(defun parse-form-value (request key)
  "Parse form-encoded body"
  (when (search "application/x-www-form-urlencoded" 
                (or (map:get (request:request-headers request) "Content-Type") ""))
    (let ((body (request:request-body request)))
      (when body
        (let ((pairs (str:split #\& body)))
          (loop for pair in pairs
                for kv = (str:split #\= pair)
                when (and (= (length kv) 2)
                         (string= (first kv) key))
                return (url-decode (second kv))))))))

(defun url-decode (string)
  "Basic URL decoding"
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length string))
          do (let ((char (char string i)))
               (cond
                 ((char= char #\%)
                  (when (< (+ i 2) (length string))
                    (write-char (code-char (parse-integer string 
                                                         :start (1+ i) 
                                                         :end (+ i 3)
                                                         :radix 16))
                               out)
                    (incf i 2)))
                 ((char= char #\+)
                  (write-char #\Space out))
                 (t (write-char char out)))
               (incf i)))))

;;;; Content Negotiation

(defun parse-accept-header (accept-header)
  "Parse Accept header into list of (type . quality) pairs"
  (when accept-header
    (let ((types nil))
      (dolist (type-spec (seq:realize (str:split #\, accept-header)))
        (let* ((parts-seq (str:split #\; (str:trim type-spec)))
               (parts (seq:realize parts-seq))
               (mime-type (first parts))
               (quality 1.0))
          (dolist (param (rest parts))
            (let ((trimmed-param (str:trim param)))
              (when (and (>= (length trimmed-param) 2)
                        (string= "q=" (subseq trimmed-param 0 2)))
                (setf quality (read-from-string (subseq trimmed-param 2))))))
          (push (cons mime-type quality) types)))
      (sort types #'> :key #'cdr))))

(defun accepts-p (request mime-type)
  "Check if request accepts given mime type"
  (let ((accept (map:get (request:request-headers request) "Accept")))
    (or (null accept)
        (search "*/*" accept)
        (search mime-type accept))))

(defun preferred-type (request &rest types)
  "Return the preferred content type from options"
  (let ((accept-types (parse-accept-header 
                       (map:get (request:request-headers request) "Accept"))))
    (if accept-types
        ;; Find the highest quality match from accept-types that's in our types list
        (loop for (accepted . quality) in accept-types
              for match = (find-if (lambda (type)
                                   (or (string= accepted type)
                                       (string= accepted "*/*")))
                                 types)
              when match return match)
        (first types))))

(defun negotiate-content (request data)
  "Automatically select response format based on Accept header"
  (let ((preferred (preferred-type request 
                                  "application/json"
                                  "text/html"
                                  "text/plain")))
    (cond
      ((string= preferred "application/json")
       (response:json-response data))
      ((string= preferred "text/html")
       (response:html-response (format nil "<pre>~A</pre>" data)))
      (t
       (response:text-response (format nil "~A" data))))))

;;;; Response Builders

(defun respond (data &key (status 200) headers)
  "Create response with automatic content negotiation"
  (let ((response (response:make-response :status status
                                         :body (if (stringp data)
                                                 data
                                                 (with-output-to-string (s)
                                                   (json:encode data s)))
                                         :headers (or headers map:+empty+))))
    (when headers
      (loop for (key . value) in (map:seq headers)
            do (setf (response:response-headers response)
                    (map:assoc (response:response-headers response) key value))))
    response))

(defun redirect (location &key (status 302))
  "Create redirect response"
  (response:redirect location :status status))

(defun not-found (&optional message)
  "Create 404 response"
  (respond (or message "Not Found") :status 404))

(defun bad-request (&optional message)
  "Create 400 response"
  (respond (map:make-map "error" (or message "Bad Request")) :status 400))

(defun unauthorized (&optional message)
  "Create 401 response"
  (respond (map:make-map "error" (or message "Unauthorized")) :status 401))

(defun forbidden (&optional message)
  "Create 403 response"
  (respond (map:make-map "error" (or message "Forbidden")) :status 403))

(defun internal-error (&optional message)
  "Create 500 response"
  (respond (map:make-map "error" (or message "Internal Server Error")) :status 500))

;;;; Route Definition

(defmacro defroutes (name &body route-specs)
  "Define routes with support for groups and patterns"
  `(defparameter ,name
     ,(compile-route-specs route-specs)))

(defun compile-route-specs (specs &optional prefix)
  "Compile route specifications into route objects"
  (let ((routes nil))
    (dolist (spec specs)
      (destructuring-bind (method-or-group path &rest args) spec
        (if (eq method-or-group :group)
            (let* ((new-prefix (concatenate 'string (or prefix "") path))
                   (group-routes (compile-route-specs args new-prefix)))
              ;; Extract the actual route forms from the nested list
              (if (and (listp group-routes) (eq (car group-routes) 'list))
                  (dolist (route-form (cdr group-routes))
                    (push route-form routes))
                  (push group-routes routes)))
            (let ((full-path (concatenate 'string (or prefix "") path))
                  (handler (first args))
                  (options (rest args)))
              (push `(routing:make-route ',method-or-group ,full-path ',handler ,@options)
                   routes)))))
    `(list ,@(nreverse routes))))

;;;; Route Handling

(defun handle-routes (routes)
  "Create a handler function that dispatches based on routes"
  (lambda (request)
    (multiple-value-bind (handler params route)
        (routing:find-handler routes 
                            (request:request-method request)
                            (request:request-path request))
      (if handler
          (let ((request-with-params (add-path-params request params)))
            (if (routing:route-middleware route)
                (funcall (apply-route-middleware 
                         handler 
                         (routing:route-middleware route))
                        request-with-params)
                (funcall handler request-with-params)))
          (not-found)))))

(defun add-path-params (request params)
  "Add path parameters to request object"
  (setf (request:request-path-params request) params)
  request)

(defun apply-route-middleware (handler middleware)
  "Apply route-specific middleware"
  (seq:reduce (lambda (h mw) (funcall mw h))
             middleware
             :initial-value handler))

;;;; Server Control

(defun start-server (routes &key (port 8080) (address "0.0.0.0") middleware)
  "Start web server with routes and optional middleware"
  (let* ((route-handler (handle-routes routes))
         (app (if middleware
                 (wrap-middleware route-handler middleware)
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

;;;; Middleware

(defclass middleware-chain ()
  ((middlewares :initform nil :accessor chain-middlewares)))

(defun make-middleware-chain (&rest middlewares)
  "Create a new middleware chain"
  (let ((chain (make-instance 'middleware-chain)))
    (setf (chain-middlewares chain) middlewares)
    chain))

(defmethod add-middleware ((chain middleware-chain) middleware)
  "Add middleware to chain"
  (push middleware (chain-middlewares chain))
  chain)

(defmethod execute ((chain middleware-chain) handler request)
  "Execute middleware chain"
  (let ((wrapped-handler
         (reduce (lambda (h mw) (funcall mw h))
                 (reverse (chain-middlewares chain))
                 :initial-value handler)))
    (funcall wrapped-handler request)))

(defun wrap-middleware (handler middleware)
  "Wrap handler with middleware (single or chain)"
  (if (typep middleware 'middleware-chain)
      (lambda (request) (execute middleware handler request))
      (funcall middleware handler)))

;;;; Built-in Middleware

(defparameter logging-middleware
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

(defparameter json-errors-middleware
  (lambda (handler)
    (lambda (req)
      (handler-case
          (funcall handler req)
        (error (e)
          (respond (map:make-map "error" (format nil "~A" e)
                               "type" (string (type-of e)))
                  :status 500))))))

(defun static-files-middleware (root &key (cache-control "public, max-age=3600"))
  "Serve static files from directory"
  (lambda (handler)
    (lambda (req)
      (let ((path (request:request-path req)))
        (if (and (string= (request:request-method req) "GET")
                (str:starts-with-p "/static/" path))
            (let ((file-path (merge-pathnames 
                            (subseq path 8)
                            (pathname root))))
              (if (probe-file file-path)
                  (respond (read-file-string file-path)
                          :headers (map:make-map 
                                  "Content-Type" (mime-type file-path)
                                  "Cache-Control" cache-control))
                  (not-found)))
            (funcall handler req))))))

(defun read-file-string (path)
  "Read file as string"
  (with-open-file (stream path :element-type 'character)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun mime-type (path)
  "Guess MIME type from file extension"
  (let ((ext (pathname-type path)))
    (cond
      ((string= ext "html") "text/html")
      ((string= ext "css") "text/css")
      ((string= ext "js") "application/javascript")
      ((string= ext "json") "application/json")
      ((string= ext "png") "image/png")
      ((string= ext "jpg") "image/jpeg")
      ((string= ext "jpeg") "image/jpeg")
      ((string= ext "gif") "image/gif")
      ((string= ext "svg") "image/svg+xml")
      (t "application/octet-stream"))))

(defun validation-middleware (schema &key (source :json))
  "Validate request data against schema"
  (lambda (handler)
    (lambda (req)
      (let ((data (case source
                    (:json (when (request:request-body req)
                            (handler-case
                                (json:parse (request:request-body req))
                              (error () nil))))
                    (:params (request:request-params req))
                    (:all (map:merge (request:request-params req)
                                   (when (request:request-body req)
                                     (handler-case
                                         (json:parse (request:request-body req))
                                       (error () map:+empty+))))))))
        (if data
            (multiple-value-bind (valid-p errors)
                (validation:validate schema data)
              (if valid-p
                  (funcall handler req)
                  (bad-request (map:make-map "errors" errors))))
            (bad-request "No data to validate"))))))

(defun session-middleware (store &key 
                                 (cookie-name "session-id")
                                 (path "/")
                                 (max-age (* 60 60 24))
                                 secure
                                 http-only
                                 same-site)
  "Add session support via cookies"
  (lambda (handler)
    (lambda (req)
      ;; Load session from cookie
      (let* ((cookies (session:parse-cookie-header 
                      (map:get (request:request-headers req) "Cookie")))
             (session-id (gethash cookie-name cookies))
             (session (if session-id
                         (session:load-session store session-id)
                         (session:create-session))))
        
        ;; Create new session if needed
        (unless session
          (setf session (session:create-session)))
        
        ;; Attach session to request
        (setf (request:request-session req) session)
        
        ;; Handle request
        (let ((response (funcall handler req)))
          
          ;; Save session
          (session:save-session store session)
          
          ;; Set session cookie
          (let ((cookie (session:make-cookie-string 
                        cookie-name
                        (session:session-id session)
                        :path path
                        :max-age max-age
                        :secure secure
                        :http-only http-only
                        :same-site same-site)))
            (setf (response:response-headers response)
                  (map:assoc (response:response-headers response)
                           "Set-Cookie" cookie)))
          
          response)))))

;;;; WebSocket Support

(defun websocket-upgrade-request-p (request)
  "Check if request is a WebSocket upgrade request"
  (and (string-equal "websocket" 
                    (map:get (request:request-headers request) "Upgrade"))
       (search "Upgrade" 
              (map:get (request:request-headers request) "Connection"))))

(defstruct (websocket-handler (:constructor make-websocket-handler-internal))
  on-open
  on-message
  on-close
  on-error)

(defun make-websocket-handler (&key on-open on-message on-close on-error)
  "Create a WebSocket handler"
  (make-websocket-handler-internal
   :on-open (or on-open (lambda (conn) nil))
   :on-message (or on-message (lambda (conn msg) nil))
   :on-close (or on-close (lambda (conn) nil))
   :on-error (or on-error (lambda (conn error) nil))))

(defun websocket-broadcast (connections message)
  "Broadcast message to all connections"
  (dolist (conn connections)
    (handler-case
        (ws:send-text conn message)
      (error () 
        ;; Remove failed connection
        nil))))

(defun handle-websocket-upgrade (request handler)
  "Handle WebSocket upgrade request"
  (let ((key (map:get (request:request-headers request) "Sec-WebSocket-Key")))
    (if key
        ;; Return upgrade response
        (response:make-response
         :status 101
         :headers (map:make-map
                  "Upgrade" "websocket"
                  "Connection" "Upgrade"
                  "Sec-WebSocket-Accept" (ws:generate-accept-key key)))
        (bad-request "Missing WebSocket key"))))

;; Export helpers
(defun route-raw-path (route)
  "Get raw path from route"
  (routing:route-raw-path route))

(defun route-method (route)
  "Get method from route"
  (routing:route-method route))

;; Extend route handling for WebSocket
(defun handle-routes-with-websocket (routes)
  "Create handler that supports WebSocket routes"
  (lambda (request)
    (multiple-value-bind (handler params route)
        (routing:find-handler routes 
                            (if (websocket-upgrade-request-p request)
                                "WEBSOCKET"
                                (request:request-method request))
                            (request:request-path request))
      (cond
        ;; WebSocket upgrade
        ((and handler (string= (routing:route-method route) "WEBSOCKET"))
         (handle-websocket-upgrade request handler))
        
        ;; Normal HTTP
        (handler
         (let ((request-with-params (add-path-params request params)))
           (if (routing:route-middleware route)
               (funcall (apply-route-middleware 
                        handler 
                        (routing:route-middleware route))
                       request-with-params)
               (funcall handler request-with-params))))
        
        ;; Not found
        (t (not-found))))))

;; Override handle-routes to support WebSocket
(setf (symbol-function 'handle-routes) #'handle-routes-with-websocket)