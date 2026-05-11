;;;; epsilon.http.response - HTTP Response Handling

(defpackage epsilon.http.response
  (:use :cl)
  (:import (epsilon.string str)
            (epsilon.map map)
            (epsilon.json json)
            (epsilon.http.headers headers)))

(defun coerce-headers (headers-arg)
  "Normalize whatever the caller supplied to a plain epsilon.map.
   Accepts a header-map (projected to a plain map), a plain epsilon.map
   (passed through), or NIL.

   The slot type is a plain epsilon.map for backwards compatibility.
   Case-insensitive lookup is available via response-header or via the
   epsilon.http.headers wrapper."
  (cond
    ((null headers-arg) map:+empty+)
    ((headers:headers-p headers-arg)
     (let ((m map:+empty+))
       (headers:headers-each
        (lambda (k v)
          (setf m (map:assoc m (if (stringp k) (string-downcase k) k) v)))
        headers-arg)
       m))
    (t headers-arg)))

(defclass http-response ()
  ((status :initarg :status :accessor response-status :initform 200)
   (headers :initarg :headers :accessor response-headers :initform map:+empty+)
   (body :initarg :body :accessor response-body :initform nil)
   (chunked :initarg :chunked
            :accessor response-chunked-p
            :initform nil
            :documentation "When non-nil, the server emits this response with
Transfer-Encoding: chunked. The body may be a list/vector of strings or byte
vectors (each one becomes a chunk) or a function of zero arguments that returns
the next chunk and returns nil when done.")
   (websocket-handler :initarg :websocket-handler
                      :accessor response-websocket-handler
                      :initform nil
                      :documentation "When set on a 101 response, a function called with
(connection ssl-p) to take over the connection for WebSocket framing.")))

(defmethod initialize-instance :after ((resp http-response) &key)
  (setf (response-headers resp) (coerce-headers (response-headers resp))))

(defun make-response (&key (status 200) headers body chunked)
  "Create an HTTP response object. HEADERS may be a header-map or a plain
   epsilon.map; both are normalized to a plain epsilon.map. When CHUNKED
   is non-nil, the server emits this response with Transfer-Encoding:
   chunked instead of Content-Length."
  (make-instance 'http-response
                 :status status
                 :headers (coerce-headers headers)
                 :body body
                 :chunked chunked))

(defun set-header (response header-name header-value)
  "Set a response header. The name is preserved as supplied so that
   downstream code that does case-sensitive lookups (e.g. for headers
   set via this function) keeps working."
  (setf (response-headers response)
        (map:assoc (response-headers response) header-name header-value))
  response)

(defun response-header (response header-name &optional default)
  "Look up HEADER-NAME on RESPONSE. Case-insensitive on string keys: tries
   the supplied casing first, then walks the headers comparing lowercase
   forms so any wire-supplied or handler-supplied casing is found."
  (let ((hs (response-headers response)))
    (or (map:get hs header-name)
        (when (stringp header-name)
          (let ((target (string-downcase header-name))
                (found nil))
            (block search
              (map:each
               (lambda (k v)
                 (when (and (stringp k)
                            (string= (string-downcase k) target))
                   (setf found v)
                   (return-from search)))
               hs))
            found))
        default)))

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

(defun response-body-bytes (response)
  "Get response body as byte vector."
  (let ((body (response-body response)))
    (etypecase body
      (null nil)
      (string (sb-ext:string-to-octets body :external-format :utf-8))
      ((vector (unsigned-byte 8)) body))))

(defun response-body-string (response &key (encoding :utf-8))
  "Get response body as string, decoding from bytes if necessary."
  (let ((body (response-body response)))
    (etypecase body
      (null nil)
      (string body)
      ((vector (unsigned-byte 8))
       (sb-ext:octets-to-string body :external-format encoding)))))

(defun redirect (url &key (status 302))
  "Create a redirect response"
  (make-response :status status
                 :headers (map:make-map "Location" url)))

(defun json-response (data &key (status 200))
  "Create a JSON response"
  (make-response :status status
                 :headers (map:make-map "Content-Type" "application/json")
                 :body (with-output-to-string (s)
                         (json:encode data s))))

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
  "Convert HTTP response to string format for transmission.
   For responses with byte bodies, converts to string representation."
  (let ((body (response-body response))
        (resp-headers (response-headers response))
        (status (response-status response)))

    ;; Compute content length from body if not already set (case-insensitive)
    (when (and body
               (not (or (map:get resp-headers "content-length")
                        (map:get resp-headers "Content-Length"))))
      (let ((len (etypecase body
                   (string (length (sb-ext:string-to-octets body :external-format :utf-8)))
                   ((vector (unsigned-byte 8)) (length body)))))
        (setf resp-headers (map:assoc resp-headers "Content-Length"
                                      (format nil "~A" len)))))

    ;; Build response string
    (with-output-to-string (out)
      ;; Status line
      (format out "HTTP/1.1 ~A ~A~C~C"
              status
              (status-text status)
              #\Return #\Newline)

      ;; Headers (names canonicalized for wire presentation)
      (map:each
       (lambda (key value)
         (format out "~A: ~A~C~C"
                 (headers:canonicalize-name key)
                 value #\Return #\Newline))
       resp-headers)

      ;; Empty line between headers and body
      (format out "~C~C" #\Return #\Newline)

      ;; Body
      (when body
        (etypecase body
          (string (write-string body out))
          ((vector (unsigned-byte 8))
           (write-string (sb-ext:octets-to-string body :external-format :iso-8859-1) out)))))))
