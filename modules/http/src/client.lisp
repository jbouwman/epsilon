;;;; HTTP Client Implementation
;;;;
;;;; - HTTP/1.1 client with connection pooling and keep-alive

(defpackage :epsilon.http.client
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:map #:epsilon.map)
   (#:tls #:epsilon.cryptography))
  (:export
   #:request
   #:http-get
   #:http-post
   #:http-put
   #:http-delete
   #:http-head
   #:http-options
   #:with-connection))

(in-package :epsilon.http.client)

(defclass http-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (ssl-p :initarg :ssl-p :accessor connection-ssl-p :initform nil)
   (tls-connection :initarg :tls-connection :accessor connection-tls-connection :initform nil)))

(defun make-http-connection (host port &key ssl-p)
  "Create an HTTP connection to HOST:PORT, optionally with SSL.
   Example: (make-http-connection \"example.com\" 443 :ssl-p t)"
  (let* ((address (net:make-socket-address host port))
         (socket (net:tcp-connect address))
         (tls-conn nil))
    (when ssl-p
      (let ((tls-context (tls:create-tls-context :server-p nil)))
        (setf tls-conn (tls:tls-connect socket tls-context))))
    (make-instance 'http-connection
                   :socket socket
                   :host host
                   :port port
                   :ssl-p ssl-p
                   :tls-connection tls-conn)))

(defmacro with-connection ((conn host port &key ssl-p) &body body)
  "Execute body with an HTTP connection"
  `(let ((,conn (make-http-connection ,host ,port :ssl-p ,ssl-p)))
     (unwind-protect
          (progn ,@body)
       (progn
         (when (connection-tls-connection ,conn)
           ;; TODO: Fix TLS load order
           ;; (tls:tls-close (connection-tls-connection ,conn))
           )
         (when (connection-socket ,conn)
           (net:tcp-shutdown (connection-socket ,conn) :both))))))

(defun parse-url (url-string)
  "Parse URL-STRING into scheme, host, port, path, and query components.
   Example: (parse-url \"https://api.example.com:8080/data?id=123\")"
  (let* ((scheme-end (search "://" url-string))
         (scheme (if scheme-end
                     (subseq url-string 0 scheme-end)
                     "http"))
         (rest-url (if scheme-end
                       (subseq url-string (+ scheme-end 3))
                       url-string))
         (slash-pos (position #\/ rest-url))
         (question-pos (position #\? rest-url))
         (colon-pos (position #\: rest-url))
         ;; Find the end of the authority section (host:port)
         (authority-end (cond
                          (slash-pos slash-pos)
                          (question-pos question-pos)
                          (t (length rest-url))))
         ;; Only consider colon as port separator if it's before path/query
         (port-colon-pos (when (and colon-pos (< colon-pos authority-end))
                           colon-pos))
         (host-end (or port-colon-pos authority-end))
         (host (subseq rest-url 0 host-end))
         (port (cond
                 (port-colon-pos
                  (parse-integer (subseq rest-url (1+ port-colon-pos) authority-end)))
                 ((string= scheme "https") 443)
                 (t 80)))
         (path-start (or slash-pos (length rest-url)))
         (path-end (or question-pos (length rest-url)))
         (path (if (< path-start (length rest-url))
                   (subseq rest-url path-start path-end)
                   "/"))
         (query (when (and question-pos (< question-pos (length rest-url)))
                  (subseq rest-url (1+ question-pos)))))
    (values scheme host port path query)))

(defun format-request-line (method path query)
  "Format HTTP request line with METHOD, PATH and optional QUERY.
   Example: (format-request-line \"GET\" \"/api/users\" \"page=1\")"
  (format nil "~A ~A~@[?~A~] HTTP/1.1"
          method
          path
          query))

(defun format-headers (headers)
  "Format HTTP headers"
  (with-output-to-string (s)
    (map:each (lambda (k v)
                (format s "~A: ~A~C~C" k v #\Return #\Linefeed))
              headers)))

(defun extract-content-length (headers-text)
  "Extract Content-Length from headers text"
  (let ((start (search "Content-Length:" headers-text :test #'char-equal)))
    (when start
      (let* ((line-start (+ start 15)) ; length of "Content-Length:"
             (line-end (or (position #\Linefeed headers-text :start line-start)
                           (length headers-text)))
             (length-str (str:trim (subseq headers-text line-start line-end))))
        (ignore-errors (parse-integer length-str))))))

(defun send-request (connection method path &key headers body query)
  "Send HTTP request over connection"
  (let* ((request-line (format-request-line method path query))
         (default-headers (map:make-map 
                           "Host" (connection-host connection)
                           "User-Agent" "epsilon.http/1.0"
                           "Connection" "close"))
         (all-headers (map:merge default-headers (or headers map:+empty+)))
         (final-headers (if body
                            (map:assoc all-headers 
                                       "Content-Length" 
                                       (length body))
                            all-headers))
         (request (format nil "~A~C~C~A~C~C~@[~A~]"
                          request-line #\Return #\Linefeed
                          (format-headers final-headers) #\Return #\Linefeed
                          body)))
    (let ((stream (if (connection-ssl-p connection)
                      ;; TODO: Fix TLS load order
                      (error "TLS streams not yet implemented")
                      (make-two-way-stream (net:tcp-stream-reader (connection-socket connection))
                                           (net:tcp-stream-writer (connection-socket connection))))))
      (write-string request stream)
      (force-output stream))))

(defun read-response (connection)
  "Read HTTP response from connection"
  (let* ((stream (if (connection-ssl-p connection)
                     ;; TODO: Fix TLS load order
                     (error "TLS streams not yet implemented")
                     (make-two-way-stream (net:tcp-stream-reader (connection-socket connection))
                                          (net:tcp-stream-writer (connection-socket connection)))))
         (response-lines '())
         (line nil))
    ;; Read status line and headers
    (loop while (setf line (read-line stream nil nil))
          do (push line response-lines)
          when (string= line "") do (return))
    ;; Try to read body based on Content-Length header
    (let* ((headers-text (str:join #\Linefeed (seq:from-list (reverse response-lines))))
           (content-length (extract-content-length headers-text))
           (body (when (and content-length (> content-length 0))
                   (let ((buffer (make-string content-length)))
                     (read-sequence buffer stream)
                     buffer)))
           (response-text (format nil "~A~@[~A~]" headers-text body)))
      (parse-response response-text))))

(defun parse-response (response-string)
  "Parse HTTP response into status, headers, and body"
  (let* ((lines (str:split #\Linefeed response-string))
         (status-line (seq:first lines))
         (status-parts (seq:realize (str:split #\Space status-line)))
         (status-code (parse-integer (second status-parts)))
         (headers map:+empty+)
         (body-start nil))
    
    ;; Parse headers
    (let ((lines-list (seq:realize lines)))
      (loop for i from 1 below (length lines-list)
            for line = (nth i lines-list)
          do (cond
               ((or (string= line "") (string= line (string #\Return)))
                (setf body-start (1+ i))
                (return))
               (t
                (let ((colon-pos (position #\: line)))
                  (when colon-pos
                    (let ((key (subseq line 0 colon-pos))
                          (value (str:trim (subseq line (1+ colon-pos)))))
                      (setf headers (map:assoc headers key value))))))))
    
      ;; Extract body
      (let ((body (when body-start
                    (str:join #\Linefeed (seq:from-list (subseq lines-list body-start))))))
        (values status-code headers body)))))

(defun request (url &key (method "GET") headers body)
  "Make an HTTP request to URL"
  (multiple-value-bind (scheme host port path query)
      (parse-url url)
    (let ((ssl-p (string= scheme "https")))
      (with-connection (conn host port :ssl-p ssl-p)
        (send-request conn method path 
                      :headers headers 
                      :body body
                      :query query)
        (read-response conn)))))

(defun http-get (url &key headers)
  "Make GET request"
  (request url :method "GET" :headers headers))

(defun http-post (url &key headers body)
  "Make POST request"
  (request url :method "POST" :headers headers :body body))

(defun http-put (url &key headers body)
  "Make PUT request"
  (request url :method "PUT" :headers headers :body body))

(defun http-delete (url &key headers)
  "Make DELETE request"
  (request url :method "DELETE" :headers headers))

(defun http-head (url &key headers)
  "Make HEAD request"
  (request url :method "HEAD" :headers headers))

(defun http-options (url &key headers)
  "Make OPTIONS request"
  (request url :method "OPTIONS" :headers headers))
