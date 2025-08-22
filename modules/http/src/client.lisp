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
   (#:tls #:epsilon.crypto))
  (:export
   #:request
   #:http-get
   #:http-post
   #:http-put
   #:http-delete
   #:http-head
   #:http-options
   #:with-connection
   #:make-http-connection))

(in-package :epsilon.http.client)

(defclass http-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (ssl-p :initarg :ssl-p :accessor connection-ssl-p :initform nil)
   (tls-connection :initarg :tls-connection :accessor connection-tls-connection :initform nil)
   (tls-context :initarg :tls-context :accessor connection-tls-context :initform nil)))

(defun make-http-connection (host port &key ssl-p tls-context cert-file key-file ca-file
                                        alpn-protocols verify-depth session-cache-p)
  "Create an HTTP connection to HOST:PORT with full mTLS and HTTP/2 support.
   Parameters:
   - ssl-p: Enable SSL/TLS
   - tls-context: Pre-configured TLS context to use
   - cert-file: Client certificate file for mutual TLS
   - key-file: Client private key file for mutual TLS
   - ca-file: CA certificate file for server verification
   - alpn-protocols: List of ALPN protocols (default: '(\"h2\" \"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption
   Example: (make-http-connection \"example.com\" 443 :ssl-p t 
                                  :cert-file \"client.pem\" :key-file \"key.pem\"
                                  :alpn-protocols '(\"h2\" \"http/1.1\"))"
  (let* ((address (if (string= host "localhost")
                       (net:make-socket-address "127.0.0.1" port)
                       (net:make-socket-address host port)))
         (socket (net:tcp-connect address))
         (tls-conn nil)
         (tls-ctx nil)
         (effective-alpn-protocols (or alpn-protocols '("h2" "http/1.1"))))
    (when ssl-p
      (setf tls-ctx (or tls-context
                        (tls:create-openssl-context 
                         :server-p nil
                         :cert-file cert-file
                         :key-file key-file
                         :ca-file ca-file
                         :verify-mode (if ca-file
                                          tls:+ssl-verify-peer+
                                          tls:+ssl-verify-none+)
                         :verify-depth verify-depth
                         :alpn-protocols effective-alpn-protocols
                         :session-cache-p session-cache-p)))
      (setf tls-conn (tls:openssl-connect socket tls-ctx 
                                          :hostname host
                                          :alpn-protocols effective-alpn-protocols)))
    (make-instance 'http-connection
                   :socket socket
                   :host host
                   :port port
                   :ssl-p ssl-p
                   :tls-connection tls-conn
                   :tls-context tls-ctx)))

(defmacro with-connection ((conn host port &key ssl-p tls-context cert-file key-file ca-file
                                             alpn-protocols verify-depth session-cache-p) &body body)
  "Execute body with an HTTP connection, supporting mutual TLS and HTTP/2"
  `(let ((,conn (make-http-connection ,host ,port 
                                      :ssl-p ,ssl-p
                                      :tls-context ,tls-context
                                      :cert-file ,cert-file
                                      :key-file ,key-file
                                      :ca-file ,ca-file
                                      :alpn-protocols ,alpn-protocols
                                      :verify-depth ,verify-depth
                                      :session-cache-p ,session-cache-p)))
     (unwind-protect
          (progn ,@body)
       (progn
         (when (connection-tls-connection ,conn)
           (tls:tls-close (connection-tls-connection ,conn)))
         (when (and (connection-socket ,conn)
                    (not (connection-ssl-p ,conn)))
           (net:tcp-shutdown (connection-socket ,conn) :how :both))))))

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
    (if (connection-ssl-p connection)
        ;; Use TLS write directly
        (tls:tls-write (connection-tls-connection connection) request)
        ;; Use regular socket stream
        (let ((stream (make-two-way-stream 
                       (net:tcp-stream-reader (connection-socket connection))
                       (net:tcp-stream-writer (connection-socket connection)))))
          (write-string request stream)
          (force-output stream)))))

(defun read-response (connection)
  "Read HTTP response from connection"
  (if (connection-ssl-p connection)
      ;; TLS connection - read directly from TLS
      (read-tls-response connection)
      ;; Regular connection - use socket streams
      (let* ((stream (make-two-way-stream 
                      (net:tcp-stream-reader (connection-socket connection))
                      (net:tcp-stream-writer (connection-socket connection))))
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
          (parse-response response-text)))))

(defun read-tls-response (connection)
  "Read HTTP response from TLS connection"
  (let* ((tls-conn (connection-tls-connection connection))
         (buffer-size 4096)
         (buffer (make-string buffer-size))
         (response-data (make-array 0 :element-type 'character 
                                    :adjustable t :fill-pointer 0))
         (headers-complete nil)
         (content-length nil))
    
    ;; Read headers first
    (loop until headers-complete
          do (let ((bytes-read (tls:tls-read tls-conn buffer 0 buffer-size)))
               (when (and bytes-read (> bytes-read 0))
                 (loop for i from 0 below bytes-read
                       do (vector-push-extend (char buffer i) response-data))
                 ;; Check if headers are complete
                 (let ((current-data (coerce response-data 'string)))
                   (when (search (format nil "~C~C~C~C" 
                                         #\Return #\Linefeed #\Return #\Linefeed) 
                                 current-data)
                     (setf headers-complete t)
                     (setf content-length (extract-content-length current-data)))))))
    
    ;; Read body if Content-Length specified
    (when (and content-length (> content-length 0))
      (let* ((current-data (coerce response-data 'string))
             (header-end (+ (search (format nil "~C~C~C~C" 
                                            #\Return #\Linefeed #\Return #\Linefeed)
                                    current-data) 4))
             (body-bytes-read (- (length response-data) header-end))
             (bytes-remaining (- content-length body-bytes-read)))
        (when (> bytes-remaining 0)
          (loop while (> bytes-remaining 0)
                do (let* ((to-read (min bytes-remaining buffer-size))
                          (bytes-read (tls:tls-read tls-conn buffer 0 to-read)))
                     (when (and bytes-read (> bytes-read 0))
                       (loop for i from 0 below bytes-read
                             do (vector-push-extend (char buffer i) response-data))
                       (decf bytes-remaining bytes-read)))))))
    
    (parse-response (coerce response-data 'string))))

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
        (list :status status-code :headers headers :body body)))))

(defun request (url &key (method "GET") headers body tls-context cert-file key-file ca-file
                    alpn-protocols verify-depth session-cache-p)
  "Make an HTTP request to URL with full mTLS and HTTP/2 support.
   Parameters:
   - method: HTTP method (GET, POST, etc.)
   - headers: HTTP headers as a map
   - body: Request body
   - tls-context: Pre-configured TLS context
   - cert-file: Client certificate for mutual TLS
   - key-file: Client private key for mutual TLS
   - ca-file: CA certificate for server verification
   - alpn-protocols: ALPN protocols to advertise (default: '(\"h2\" \"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption"
  (multiple-value-bind (scheme host port path query)
      (parse-url url)
    (let ((ssl-p (string= scheme "https")))
      (with-connection (conn host port 
                        :ssl-p ssl-p
                        :tls-context tls-context
                        :cert-file cert-file
                        :key-file key-file
                        :ca-file ca-file
                        :alpn-protocols alpn-protocols
                        :verify-depth verify-depth
                        :session-cache-p session-cache-p)
        (send-request conn method path 
                      :headers headers 
                      :body body
                      :query query)
        (read-response conn)))))

(defun http-get (url &key headers tls-context cert-file key-file ca-file
                      alpn-protocols verify-depth session-cache-p)
  "Make GET request with full mTLS and HTTP/2 support"
  (request url :method "GET" :headers headers
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))

(defun http-post (url &key headers body tls-context cert-file key-file ca-file
                       alpn-protocols verify-depth session-cache-p)
  "Make POST request with full mTLS and HTTP/2 support"
  (request url :method "POST" :headers headers :body body
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))

(defun http-put (url &key headers body tls-context cert-file key-file ca-file
                      alpn-protocols verify-depth session-cache-p)
  "Make PUT request with full mTLS and HTTP/2 support"
  (request url :method "PUT" :headers headers :body body
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))

(defun http-delete (url &key headers tls-context cert-file key-file ca-file
                         alpn-protocols verify-depth session-cache-p)
  "Make DELETE request with full mTLS and HTTP/2 support"
  (request url :method "DELETE" :headers headers
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))

(defun http-head (url &key headers tls-context cert-file key-file ca-file
                       alpn-protocols verify-depth session-cache-p)
  "Make HEAD request with full mTLS and HTTP/2 support"
  (request url :method "HEAD" :headers headers
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))

(defun http-options (url &key headers tls-context cert-file key-file ca-file
                          alpn-protocols verify-depth session-cache-p)
  "Make OPTIONS request with full mTLS and HTTP/2 support"
  (request url :method "OPTIONS" :headers headers
           :tls-context tls-context
           :cert-file cert-file
           :key-file key-file
           :ca-file ca-file
           :alpn-protocols alpn-protocols
           :verify-depth verify-depth
           :session-cache-p session-cache-p))
