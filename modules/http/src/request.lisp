;;;; epsilon.http.request - HTTP Request Handling

(defpackage epsilon.http.request
  (:use :cl)
  (:import (epsilon.string str)
            (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.http.headers headers)))

(defun coerce-headers (headers-arg)
  "Normalize whatever the caller supplied to a plain epsilon.map.
   Accepts a header-map (projected back to a plain map keyed by lowercase
   canonical names), a plain epsilon.map (passed through), or NIL.

   The slot type is a plain epsilon.map for backwards compatibility with
   downstream consumers; case-insensitive access is available via the
   request-header / response-header convenience accessors and through
   the epsilon.http.headers wrapper."
  (cond
    ((null headers-arg) map:+empty+)
    ((headers:headers-p headers-arg)
     ;; Project to a plain map keyed by lowercase canonical names so
     ;; downstream code that uses (map:get headers "lowercase-name") works.
     (let ((m map:+empty+))
       (headers:headers-each
        (lambda (k v)
          (setf m (map:assoc m (if (stringp k) (string-downcase k) k) v)))
        headers-arg)
       m))
    (t headers-arg)))

(defclass http-request ()
  ((method :initarg :method :accessor request-method)
   (path :initarg :path :accessor request-path)
   (headers :initarg :headers :accessor request-headers :initform map:+empty+)
   (body :initarg :body :accessor request-body :initform nil)
   ;; Streaming body delivery for requests whose declared Content-Length
   ;; exceeds the slurp threshold (see EPSILON.HTTP.SERVER:*BODY-SLURP-
   ;; THRESHOLD*).  When set, BODY is NIL and the handler must call this
   ;; thunk repeatedly until it returns NIL to drain the request body
   ;; off the connection.  Each call returns either a (SIMPLE-ARRAY
   ;; (UNSIGNED-BYTE 8) (*)) chunk or NIL on EOF.  Fully draining is
   ;; required for HTTP/1.1 keepalive: the server forces the connection
   ;; closed after the handler returns if BODY-SOURCE-DRAINED-P is NIL.
   (body-source :initarg :body-source :accessor request-body-source
                :initform nil)
   (body-source-drained-p :initarg :body-source-drained-p
                          :accessor request-body-source-drained-p
                          :initform nil)
   (content-length :initarg :content-length :accessor request-content-length
                   :initform nil)
   (params :initarg :params :accessor request-params :initform map:+empty+)
   (path-params :initarg :path-params :accessor request-path-params :initform map:+empty+)
   (session :initarg :session :accessor request-session :initform nil)))

(defmethod initialize-instance :after ((req http-request) &key)
  ;; Coerce whatever was supplied to a plain epsilon.map so the slot type
  ;; is uniform across all construction paths.
  (setf (request-headers req) (coerce-headers (request-headers req))))

(defun make-request (method path &key headers body params)
  "Create an HTTP request object. HEADERS may be a header-map or a plain
   epsilon.map; both are normalized to a plain epsilon.map keyed by
   lowercase canonical names."
  (make-instance 'http-request
                 :method method
                 :path path
                 :headers (coerce-headers headers)
                 :body body
                 :params (or params map:+empty+)))

(defun add-header (request header-name header-value)
  "Add or update a header in the request object. The header name is
   preserved as supplied; use REQUEST-HEADER for case-insensitive lookup."
  (setf (request-headers request)
        (map:assoc (request-headers request) header-name header-value))
  request)

(defun request-header (request header-name &optional default)
  "Look up HEADER-NAME on REQUEST. Case-insensitive on string keys: tries
   the supplied casing first (cheap path for code that already uses the
   canonical name), then walks the headers comparing lowercase forms."
  (let ((hs (request-headers request)))
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

(defun parse-query-string (query-string)
  "Parse URL query string into parameters map"
  (if (and query-string (> (length query-string) 0))
      (let ((params map:+empty+))
        (dolist (pair (seq:realize (str:split #\& query-string)))
          (let ((eq-pos (position #\= pair)))
            (if eq-pos
                (let ((key (subseq pair 0 eq-pos))
                      (value (subseq pair (1+ eq-pos))))
                  (setf params (map:assoc params
                                          (url-decode key)
                                          (url-decode value))))
                (setf params (map:assoc params
                                        (url-decode pair)
                                        "")))))
        params)
      map:+empty+))

(defun parse-form-data (form-string)
  "Parse application/x-www-form-urlencoded data"
  (parse-query-string form-string))

(defun url-encode (string)
  "Encode string for use in URL"
  (with-output-to-string (out)
    (loop for char across string
          do (cond
               ((or (alphanumericp char)
                    (member char '(#\- #\_ #\. #\~)))
                (write-char char out))
               ((char= char #\Space)
                (write-char #\+ out))
               (t
                (format out "%~2,'0X" (char-code char)))))))

(defun url-decode (string)
  "Decode URL-encoded string"
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          do (cond
               ((char= char #\+)
                (write-char #\Space out))
               ((char= char #\%)
                (when (< (+ i 2) (length string))
                  (let ((hex (subseq string (1+ i) (+ i 3))))
                    (write-char (code-char (parse-integer hex :radix 16)) out)
                    (incf i 2))))
               (t
                (write-char char out))))))

(defun parse-http-request (request-string)
  "Parse raw HTTP request string into http-request object"
  (let ((lines (mapcar (lambda (line) (string-right-trim '(#\Return) line))
                       (seq:realize (str:split #\Newline request-string)))))
    (when (< (length lines) 1)
      (error "Invalid HTTP request: no request line"))

    ;; Parse request line (GET /path HTTP/1.1)
    (let* ((request-line (first lines))
           (request-parts (seq:realize (str:split #\Space request-line))))
      (when (< (length request-parts) 3)
        (error "Invalid HTTP request line: ~A" request-line))

      (let ((method (string-upcase (first request-parts)))
            (path-with-query (second request-parts))
            (req-headers map:+empty+)
            (body nil))

        ;; Split path and query string
        (let* ((query-pos (position #\? path-with-query))
               (path (if query-pos
                         (subseq path-with-query 0 query-pos)
                         path-with-query))
               (query-string (when query-pos
                               (subseq path-with-query (1+ query-pos))))
               (params (if query-string
                           (parse-query-string query-string)
                           map:+empty+)))

          ;; Parse headers. Names are stored in lowercase canonical form
          ;; (RFC 7230 says HTTP header field names are case-insensitive).
          ;; This is the long-standing convention for parsed request headers
          ;; in this codebase; downstream consumers look up with lowercase
          ;; keys.
          (loop for i from 1 below (length lines)
                for line = (nth i lines)
                while (and line (> (length line) 0))
                do (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (let ((header-name (string-downcase
                                           (str:trim (subseq line 0 colon-pos))))
                             (header-value (str:trim (subseq line (1+ colon-pos)))))
                         (setf req-headers (map:assoc req-headers
                                                      header-name header-value))))))

          ;; For POST requests, parse body (simplified)
          (when (string= method "POST")
            (let ((content-length (map:get req-headers "content-length")))
              (when content-length
                ;; Find body start (after double CRLF)
                (let ((body-start (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) request-string)))
                  (when body-start
                    (setf body (subseq request-string (+ body-start 4))))))))

          (make-request method path :headers req-headers :body body :params params))))))
