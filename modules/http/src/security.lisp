;;;; HTTP Security Middleware
;;;;
;;;; Security headers, input validation, and protection mechanisms

(defpackage :epsilon.http.security
  (:use :cl)
  (:local-nicknames
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:base64 #:epsilon.base64))
  (:export
   ;; Security Headers Middleware
   #:security-headers-middleware
   #:make-security-headers-middleware
   
   ;; CORS Middleware
   #:cors-middleware
   #:make-cors-middleware
   
   ;; Rate Limiting
   #:rate-limit-middleware
   #:create-rate-limiter
   
   ;; Input Validation
   #:validate-request-size
   #:validate-content-type
   #:sanitize-path
   #:validate-headers
   
   ;; CSRF Protection
   #:csrf-middleware
   #:generate-csrf-token
   #:validate-csrf-token
   
   ;; Authentication helpers
   #:basic-auth-middleware
   #:bearer-auth-middleware
   
   ;; Utility functions
   #:parse-authorization-header
   #:constant-time-compare))

(in-package :epsilon.http.security)

;;;; Security Headers

(defparameter *default-security-headers*
  (map:make-map
   "X-Content-Type-Options" "nosniff"
   "X-Frame-Options" "DENY"
   "X-XSS-Protection" "1; mode=block"
   "Referrer-Policy" "no-referrer-when-downgrade"
   "Permissions-Policy" "geolocation=(), microphone=(), camera=()"
   "Content-Security-Policy" "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'"))

(defun security-headers-middleware (handler &key (headers *default-security-headers*))
  "Middleware that adds security headers to all responses"
  (lambda (request)
    (let ((response (funcall handler request)))
      ;; Add security headers to response
      (map:each (lambda (k v)
                  (response:set-header response k v))
                headers)
      response)))

(defun make-security-headers-middleware (&key 
                                         (x-content-type-options "nosniff")
                                         (x-frame-options "DENY")
                                         (x-xss-protection "1; mode=block")
                                         (referrer-policy "no-referrer-when-downgrade")
                                         (permissions-policy "geolocation=(), microphone=(), camera=()")
                                         (content-security-policy nil)
                                         (strict-transport-security nil)
                                         custom-headers)
  "Create security headers middleware with custom configuration"
  (let ((headers (map:make-map)))
    ;; Add standard headers
    (when x-content-type-options
      (setf headers (map:assoc headers "X-Content-Type-Options" x-content-type-options)))
    (when x-frame-options
      (setf headers (map:assoc headers "X-Frame-Options" x-frame-options)))
    (when x-xss-protection
      (setf headers (map:assoc headers "X-XSS-Protection" x-xss-protection)))
    (when referrer-policy
      (setf headers (map:assoc headers "Referrer-Policy" referrer-policy)))
    (when permissions-policy
      (setf headers (map:assoc headers "Permissions-Policy" permissions-policy)))
    (when content-security-policy
      (setf headers (map:assoc headers "Content-Security-Policy" content-security-policy)))
    (when strict-transport-security
      (setf headers (map:assoc headers "Strict-Transport-Security" strict-transport-security)))
    
    ;; Add custom headers
    (when custom-headers
      (setf headers (map:merge headers custom-headers)))
    
    (lambda (handler)
      (security-headers-middleware handler :headers headers))))

;;;; CORS Middleware

(defun cors-middleware (handler &key 
                               (allowed-origins '("*"))
                               (allowed-methods '("GET" "POST" "PUT" "DELETE" "OPTIONS"))
                               (allowed-headers '("Content-Type" "Authorization"))
                               (expose-headers nil)
                               (max-age 86400)
                               (credentials nil))
  "CORS (Cross-Origin Resource Sharing) middleware"
  (lambda (request)
    (let* ((origin (map:get (request:request-headers request) "origin"))
           (method (request:request-method request)))
      
      ;; Handle preflight requests
      (if (string= method "OPTIONS")
          (let ((resp (response:make-response :status 204)))
            ;; Set CORS headers
            (response:set-header resp "Access-Control-Allow-Origin" 
                                 (if (member origin allowed-origins :test #'string=)
                                     origin
                                     (first allowed-origins)))
            (response:set-header resp "Access-Control-Allow-Methods" 
                                 (str:join ", " allowed-methods))
            (response:set-header resp "Access-Control-Allow-Headers" 
                                 (str:join ", " allowed-headers))
            (when expose-headers
              (response:set-header resp "Access-Control-Expose-Headers" 
                                   (str:join ", " expose-headers)))
            (when max-age
              (response:set-header resp "Access-Control-Max-Age" 
                                   (format nil "~D" max-age)))
            (when credentials
              (response:set-header resp "Access-Control-Allow-Credentials" "true"))
            resp)
          
          ;; Handle actual requests
          (let ((response (funcall handler request)))
            (response:set-header response "Access-Control-Allow-Origin" 
                                 (if (or (member "*" allowed-origins :test #'string=)
                                         (member origin allowed-origins :test #'string=))
                                     (or origin "*")
                                     ""))
            (when credentials
              (response:set-header response "Access-Control-Allow-Credentials" "true"))
            (when expose-headers
              (response:set-header response "Access-Control-Expose-Headers" 
                                   (str:join ", " expose-headers)))
            response)))))

(defun make-cors-middleware (&rest args &key &allow-other-keys)
  "Create CORS middleware with configuration"
  (lambda (handler)
    (apply #'cors-middleware handler args)))

;;;; Rate Limiting

;; Define the struct with a completely unique name
(defstruct (http-security-rate-limiter (:constructor %make-security-rate-limiter
                                        (&key (requests (map:make-map))
                                              (window-seconds 60)
                                              (max-requests 60)
                                              (identifier-fn #'get-client-ip))))
  "Rate limiter state"
  (requests (map:make-map))  ; No type declaration as map:map type doesn't exist
  (window-seconds 60 :type integer)
  (max-requests 60 :type integer)
  (identifier-fn #'get-client-ip :type function))

(defun get-client-ip (request)
  "Extract client IP from request"
  (or (map:get (request:request-headers request) "x-forwarded-for")
      (map:get (request:request-headers request) "x-real-ip")
      "unknown"))

(defun clean-old-requests (limiter current-time)
  "Remove requests older than the time window"
  (let ((cutoff (- current-time (http-security-rate-limiter-window-seconds limiter)))
        (new-requests (map:make-map)))
    (map:each (lambda (ip timestamps)
                (let ((recent (remove-if (lambda (ts) (< ts cutoff)) timestamps)))
                  (when recent
                    (setf new-requests (map:assoc new-requests ip recent)))))
              (http-security-rate-limiter-requests limiter))
    (setf (http-security-rate-limiter-requests limiter) new-requests)))

(defun rate-limit-middleware (handler limiter)
  "Rate limiting middleware"
  (lambda (request)
    (let* ((current-time (get-universal-time))
           (identifier (funcall (http-security-rate-limiter-identifier-fn limiter) request))
           (requests (http-security-rate-limiter-requests limiter)))
      
      ;; Clean old requests periodically
      (when (zerop (mod current-time 60))
        (clean-old-requests limiter current-time))
      
      ;; Get request timestamps for this identifier
      (let* ((timestamps (or (map:get requests identifier) '()))
             (recent-count (count-if (lambda (ts) 
                                       (>= ts (- current-time 
                                                 (http-security-rate-limiter-window-seconds limiter))))
                                     timestamps)))
        
        (if (>= recent-count (http-security-rate-limiter-max-requests limiter))
            ;; Rate limit exceeded
            (let ((resp (response:text-response "Rate limit exceeded" :status 429)))
              (response:set-header resp "Retry-After" 
                                   (format nil "~D" (http-security-rate-limiter-window-seconds limiter)))
              resp)
            ;; Allow request and record timestamp
            (progn
              (setf (http-security-rate-limiter-requests limiter)
                    (map:assoc requests identifier (cons current-time timestamps)))
              (funcall handler request)))))))

(defun create-rate-limiter (&key (window-seconds 60) (max-requests 60) 
                                (identifier-fn #'get-client-ip))
  "Create a new rate limiter"
  (%make-security-rate-limiter :window-seconds window-seconds
                               :max-requests max-requests
                               :identifier-fn identifier-fn))

;;;; Input Validation

(defun validate-request-size (max-size)
  "Middleware to validate request body size"
  (lambda (handler)
    (lambda (request)
      (let ((content-length (map:get (request:request-headers request) "content-length")))
        (if (and content-length
                 (> (parse-integer content-length :junk-allowed t) max-size))
            (response:text-response "Request body too large" :status 413)
            (funcall handler request))))))

(defun validate-content-type (allowed-types)
  "Middleware to validate Content-Type header"
  (lambda (handler)
    (lambda (request)
      (let ((content-type (map:get (request:request-headers request) "content-type")))
        (if (and content-type
                 (not (some (lambda (allowed)
                              (str:starts-with-p content-type allowed))
                            allowed-types)))
            (response:text-response "Unsupported media type" :status 415)
            (funcall handler request))))))

(defun sanitize-path (path)
  "Sanitize request path to prevent directory traversal"
  (let ((segments (str:split #\/ path)))
    ;; Remove empty, ".", and ".." segments
    (str:join "/" 
              (remove-if (lambda (seg)
                           (or (string= seg "")
                               (string= seg ".")
                               (string= seg "..")))
                         segments))))

(defun validate-headers (required-headers)
  "Middleware to validate required headers are present"
  (lambda (handler)
    (lambda (request)
      (let ((headers (request:request-headers request))
            (missing '()))
        (dolist (required required-headers)
          (unless (map:get headers required)
            (push required missing)))
        (if missing
            (response:text-response 
             (format nil "Missing required headers: ~{~A~^, ~}" missing)
             :status 400)
            (funcall handler request))))))

;;;; CSRF Protection

(defvar *csrf-tokens* (map:make-map)
  "Active CSRF tokens")

(defun generate-csrf-token ()
  "Generate a new CSRF token"
  (let ((token (format nil "~36R" (random (expt 36 20)))))
    ;; Store token with timestamp
    (setf *csrf-tokens* (map:assoc *csrf-tokens* token (get-universal-time)))
    token))

(defun validate-csrf-token (token)
  "Validate CSRF token"
  (when token
    (let ((timestamp (map:get *csrf-tokens* token)))
      (and timestamp
           ;; Token valid for 1 hour
           (< (- (get-universal-time) timestamp) 3600)))))

(defun csrf-middleware (handler &key (header-name "X-CSRF-Token") 
                                    (cookie-name "csrf_token")
                                    (safe-methods '("GET" "HEAD" "OPTIONS")))
  "CSRF protection middleware"
  (declare (ignore cookie-name)) ;; TODO: Implement cookie-based CSRF token lookup
  (lambda (request)
    (let ((method (request:request-method request)))
      (if (member method safe-methods :test #'string=)
          ;; Safe methods don't need CSRF protection
          (funcall handler request)
          ;; Check CSRF token
          (let ((token (or (map:get (request:request-headers request) header-name)
                           ;; Could also check cookies here
                           nil)))
            (if (validate-csrf-token token)
                (funcall handler request)
                (response:text-response "Invalid CSRF token" :status 403)))))))

;;;; Authentication Helpers

(defun parse-authorization-header (auth-header)
  "Parse Authorization header into scheme and credentials"
  (when auth-header
    (let ((space-pos (position #\Space auth-header)))
      (when space-pos
        (values (subseq auth-header 0 space-pos)
                (subseq auth-header (1+ space-pos)))))))

(defun decode-basic-auth (credentials)
  "Decode Basic auth credentials"
  (let* ((decoded (base64:base64-decode-string credentials))
         (colon-pos (position #\: decoded)))
    (when colon-pos
      (values (subseq decoded 0 colon-pos)
              (subseq decoded (1+ colon-pos))))))

(defun basic-auth-middleware (handler authenticate-fn &key realm)
  "Basic authentication middleware"
  (lambda (request)
    (let ((auth-header (map:get (request:request-headers request) "authorization")))
      (multiple-value-bind (scheme credentials)
          (parse-authorization-header auth-header)
        (if (and (string-equal scheme "Basic")
                 credentials)
            (multiple-value-bind (username password)
                (decode-basic-auth credentials)
              (if (and username password
                       (funcall authenticate-fn username password))
                  (funcall handler request)
                  (let ((resp (response:text-response "Unauthorized" :status 401)))
                    (response:set-header resp "WWW-Authenticate" 
                                         (format nil "Basic~@[ realm=\"~A\"~]" realm))
                    resp)))
            (let ((resp (response:text-response "Unauthorized" :status 401)))
              (response:set-header resp "WWW-Authenticate" 
                                   (format nil "Basic~@[ realm=\"~A\"~]" realm))
              resp))))))

(defun bearer-auth-middleware (handler validate-token-fn)
  "Bearer token authentication middleware"
  (lambda (request)
    (let ((auth-header (map:get (request:request-headers request) "authorization")))
      (multiple-value-bind (scheme token)
          (parse-authorization-header auth-header)
        (if (and (string-equal scheme "Bearer")
                 token
                 (funcall validate-token-fn token))
            (funcall handler request)
            (response:text-response "Unauthorized" :status 401))))))

;;;; Utility Functions

(defun constant-time-compare (a b)
  "Constant-time string comparison to prevent timing attacks"
  (when (= (length a) (length b))
    (let ((result 0))
      (dotimes (i (length a))
        (setf result (logior result (logxor (char-code (char a i))
                                             (char-code (char b i))))))
      (zerop result))))