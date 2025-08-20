;;;; HTTP Request Builder
;;;;
;;;; Fluent API for building complex HTTP requests

(defpackage :epsilon.http.builder
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:json #:epsilon.json)
   (#:base64 #:epsilon.base64)
   (#:uuid #:epsilon.uuid)
   (#:client #:epsilon.http.client)
   (#:errors #:epsilon.http.errors))
  (:export
   ;; Builder creation
   #:request
   #:http-get
   #:http-post
   #:http-put
   #:http-delete
   #:http-patch
   #:http-head
   #:http-options
   
   ;; Builder methods
   #:with-url
   #:with-method
   #:with-header
   #:with-headers
   #:with-param
   #:with-params
   #:with-body
   #:with-json
   #:with-form
   #:with-multipart
   #:with-timeout
   #:with-follow-redirects
   #:with-max-redirects
   #:with-auth
   #:with-basic-auth
   #:with-bearer-auth
   #:with-cookie
   #:with-cookies
   #:with-user-agent
   #:with-accept
   #:with-content-type
   
   ;; Execution
   #:send
   #:send-async
   
   ;; Response handling
   #:on-success
   #:on-error
   #:on-complete
   
   ;; Builder state
   #:request-builder
   #:request-builder-p))

(in-package :epsilon.http.builder)

;;;; Request Builder Structure

(defstruct request-builder
  "HTTP request builder state"
  (method "GET" :type string)
  (url nil :type (or null string))
  (headers (map:make-map))
  (params (map:make-map))
  (body nil)
  (timeout 30 :type integer)
  (follow-redirects t :type boolean)
  (max-redirects 5 :type integer)
  (auth nil)
  (cookies nil)
  (on-success nil :type (or null function))
  (on-error nil :type (or null function))
  (on-complete nil :type (or null function)))

;;;; Builder Creation

(defun request (&optional method url-string)
  "Create a new request builder"
  (make-request-builder 
   :method (or method "GET")
   :url url-string))

(defun http-get (&optional url-string)
  "Create a GET request builder"
  (request "GET" url-string))

(defun http-post (&optional url-string)
  "Create a POST request builder"
  (request "POST" url-string))

(defun http-put (&optional url-string)
  "Create a PUT request builder"
  (request "PUT" url-string))

(defun http-delete (&optional url-string)
  "Create a DELETE request builder"
  (request "DELETE" url-string))

(defun http-patch (&optional url-string)
  "Create a PATCH request builder"
  (request "PATCH" url-string))

(defun http-head (&optional url-string)
  "Create a HEAD request builder"
  (request "HEAD" url-string))

(defun http-options (&optional url-string)
  "Create an OPTIONS request builder"
  (request "OPTIONS" url-string))

;;;; Builder Methods

(defun with-url (builder url-string)
  "Set the URL for the request"
  (setf (request-builder-url builder) url-string)
  builder)

(defun with-method (builder method-string)
  "Set the HTTP method"
  (setf (request-builder-method builder) (string-upcase method-string))
  builder)

(defun with-header (builder name value)
  "Add a single header"
  (setf (request-builder-headers builder)
        (map:assoc (request-builder-headers builder) name (format nil "~A" value)))
  builder)

(defun with-headers (builder header-map)
  "Set multiple headers at once"
  (setf (request-builder-headers builder)
        (map:merge (request-builder-headers builder) header-map))
  builder)

(defun with-param (builder name value)
  "Add a query parameter"
  (setf (request-builder-params builder)
        (map:assoc (request-builder-params builder) name (format nil "~A" value)))
  builder)

(defun with-params (builder param-map)
  "Set multiple query parameters at once"
  (setf (request-builder-params builder)
        (map:merge (request-builder-params builder) param-map))
  builder)

(defun with-body (builder body-content)
  "Set the request body"
  (setf (request-builder-body builder) body-content)
  builder)

(defun with-json (builder data)
  "Set JSON body and appropriate content-type"
  (with-header builder "Content-Type" "application/json")
  (with-body builder (if (stringp data)
                        data
                        (with-output-to-string (s)
                          (json:encode data s)))))

(defun with-form (builder form-data)
  "Set form-encoded body"
  (with-header builder "Content-Type" "application/x-www-form-urlencoded")
  (with-body builder (encode-form-data form-data)))

(defun with-multipart (builder parts)
  "Set multipart form data"
  (let ((boundary (generate-boundary)))
    (with-header builder "Content-Type" (format nil "multipart/form-data; boundary=~A" boundary))
    (with-body builder (encode-multipart-data parts boundary))))

(defun with-timeout (builder seconds)
  "Set request timeout in seconds"
  (setf (request-builder-timeout builder) seconds)
  builder)

(defun with-follow-redirects (builder &optional (follow t))
  "Set whether to follow redirects"
  (setf (request-builder-follow-redirects builder) follow)
  builder)

(defun with-max-redirects (builder count)
  "Set maximum number of redirects to follow"
  (setf (request-builder-max-redirects builder) count)
  builder)

(defun with-basic-auth (builder username password)
  "Set basic authentication"
  (let* ((credentials (format nil "~A:~A" username password))
         (encoded (base64:string-to-base64-string credentials)))
    (with-header builder "Authorization" (format nil "Basic ~A" encoded))))

(defun with-bearer-auth (builder token)
  "Set bearer token authentication"
  (with-header builder "Authorization" (format nil "Bearer ~A" token)))

(defun with-auth (builder auth-type &rest args)
  "Generic authentication setup"
  (case auth-type
    (:basic (apply #'with-basic-auth builder args))
    (:bearer (apply #'with-bearer-auth builder args))
    (otherwise (error "Unknown auth type: ~A" auth-type))))

(defun with-cookie (builder name value)
  "Add a cookie"
  (let ((cookies (or (request-builder-cookies builder) 
                     (map:make-map))))
    (setf (request-builder-cookies builder)
          (map:assoc cookies name value)))
  builder)

(defun with-cookies (builder cookie-map)
  "Set multiple cookies"
  (setf (request-builder-cookies builder)
        (if (request-builder-cookies builder)
            (map:merge (request-builder-cookies builder) cookie-map)
            cookie-map))
  builder)

(defun with-user-agent (builder agent-string)
  "Set User-Agent header"
  (with-header builder "User-Agent" agent-string))

(defun with-accept (builder media-type)
  "Set Accept header"
  (with-header builder "Accept" media-type))

(defun with-content-type (builder media-type)
  "Set Content-Type header"
  (with-header builder "Content-Type" media-type))

;;;; Response Handling

(defun on-success (builder success-fn)
  "Set success callback"
  (setf (request-builder-on-success builder) success-fn)
  builder)

(defun on-error (builder error-fn)
  "Set error callback"
  (setf (request-builder-on-error builder) error-fn)
  builder)

(defun on-complete (builder complete-fn)
  "Set completion callback"
  (setf (request-builder-on-complete builder) complete-fn)
  builder)

;;;; Execution

(defun build-url-with-params (base-url params)
  "Build URL with query parameters"
  (if (zerop (map:count params))
      base-url
      (let ((query-string (encode-query-params params)))
        (if (find #\? base-url)
            (format nil "~A&~A" base-url query-string)
            (format nil "~A?~A" base-url query-string)))))

(defun encode-query-params (params)
  "Encode parameters as URL query string"
  (let ((parts '()))
    (map:each (lambda (k v)
                (push (format nil "~A=~A" 
                             (url-encode (format nil "~A" k))
                             (url-encode (format nil "~A" v)))
                      parts))
              params)
    (str:join "&" (seq:from-list (reverse parts)))))

(defun encode-form-data (data)
  "Encode data as application/x-www-form-urlencoded"
  (encode-query-params data))

(defun encode-multipart-data (parts boundary)
  "Encode multipart form data"
  (with-output-to-string (s)
    (map:each (lambda (name content)
                (format s "--~A~%" boundary)
                (if (and (listp content) (eq (first content) :file))
                    (let ((filename (second content))
                          (data (third content))
                          (content-type (or (fourth content) "application/octet-stream")))
                      (format s "Content-Disposition: form-data; name=\"~A\"; filename=\"~A\"~%" 
                              name filename)
                      (format s "Content-Type: ~A~%~%" content-type)
                      (write-string data s))
                    (progn
                      (format s "Content-Disposition: form-data; name=\"~A\"~%~%" name)
                      (write-string (format nil "~A" content) s)))
                (format s "~%"))
              parts)
    (format s "--~A--~%" boundary)))

(defun generate-boundary ()
  "Generate a unique multipart boundary"
  (format nil "----FormBoundary~A" (uuid:make-v4)))

(defun url-encode (string)
  "URL encode a string"
  ;; Simple URL encoding - should be improved
  (with-output-to-string (s)
    (loop for char across string
          do (cond
               ((or (alphanumericp char) 
                    (member char '(#\- #\_ #\. #\~)))
                (write-char char s))
               ((char= char #\Space)
                (write-char #\+ s))
               (t 
                (format s "%~2,'0X" (char-code char)))))))

(defun build-cookie-header (cookies)
  "Build Cookie header from cookie map"
  (when cookies
    (let ((parts '()))
      (map:each (lambda (k v)
                  (push (format nil "~A=~A" k v) parts))
                cookies)
      (str:join "; " (seq:from-list (reverse parts))))))

(defun send (builder)
  "Execute the request and return the response"
  (unless (request-builder-url builder)
    (error 'errors:invalid-url-error :message "URL is required"))
  
  (let* ((final-url (build-url-with-params 
                     (request-builder-url builder)
                     (request-builder-params builder)))
         (final-headers (request-builder-headers builder))
         (cookie-header (build-cookie-header (request-builder-cookies builder))))
    
    ;; Add cookie header if we have cookies
    (when cookie-header
      (setf final-headers (map:assoc final-headers "Cookie" cookie-header)))
    
    (handler-case
        (let ((response (client:request final-url
                                       :method (request-builder-method builder)
                                       :headers final-headers
                                       :body (request-builder-body builder))))
          ;; Handle redirects
          (when (and (request-builder-follow-redirects builder)
                     (member (getf response :status) '(301 302 303 307 308)))
            (let ((location (map:get (getf response :headers) "Location")))
              (when location
                (setf response (handle-redirect builder response location)))))
          
          ;; Call success callback if set
          (when (request-builder-on-success builder)
            (funcall (request-builder-on-success builder) response))
          
          ;; Call completion callback
          (when (request-builder-on-complete builder)
            (funcall (request-builder-on-complete builder) response nil))
          
          response)
      (error (e)
        ;; Call error callback if set
        (when (request-builder-on-error builder)
          (funcall (request-builder-on-error builder) e))
        
        ;; Call completion callback
        (when (request-builder-on-complete builder)
          (funcall (request-builder-on-complete builder) nil e))
        
        ;; Re-signal the error
        (error e)))))

(defun handle-redirect (builder response location &optional (redirect-count 0))
  "Handle HTTP redirect"
  (when (>= redirect-count (request-builder-max-redirects builder))
    (error 'errors:http-error 
           :message "Too many redirects"
           :status-code (getf response :status)))
  
  ;; For 303, always use GET
  (when (= (getf response :status) 303)
    (setf (request-builder-method builder) "GET")
    (setf (request-builder-body builder) nil))
  
  ;; Update URL and resend
  (setf (request-builder-url builder) location)
  (send builder))

(defun send-async (builder &key callback)
  "Execute the request asynchronously (placeholder for future implementation)"
  ;; This would require thread support or async I/O
  ;; For now, just execute synchronously in a thread
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let ((response (send builder)))
           (when callback
             (funcall callback response nil)))
       (error (e)
         (when callback
           (funcall callback nil e)))))))