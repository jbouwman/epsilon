(defpackage :epsilon.http.client
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:uri #:epsilon.lib.uri)
   (#:str #:epsilon.lib.string)
   (#:map #:epsilon.lib.map)
   (#:base64 #:epsilon.lib.base64))
  (:export
   #:request
   #:get
   #:post
   #:put
   #:delete
   #:head
   #:options
   #:with-connection))

(in-package :epsilon.http.client)

(defclass http-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (ssl-p :initarg :ssl-p :accessor connection-ssl-p :initform nil)))

(defun make-http-connection (host port &key ssl-p)
  "Create an HTTP connection using platform-specific epsilon.net"
  (let ((socket (net:socket-connect host port)))
    (make-instance 'http-connection
                   :socket socket
                   :host host
                   :port port
                   :ssl-p ssl-p)))

(defmacro with-connection ((conn host port &key ssl-p) &body body)
  "Execute body with an HTTP connection"
  `(let ((,conn (make-http-connection ,host ,port :ssl-p ,ssl-p)))
     (unwind-protect
          (progn ,@body)
       (when (connection-socket ,conn)
         (net:socket-close (connection-socket ,conn))))))

(defun parse-url (url-string)
  "Parse URL into components"
  (let ((uri-obj (uri:parse url-string)))
    (values (uri:scheme uri-obj)
            (uri:host uri-obj)
            (or (uri:port uri-obj)
                (if (string= (uri:scheme uri-obj) "https") 443 80))
            (or (uri:path uri-obj) "/")
            (uri:query uri-obj))))

(defun format-request-line (method path query)
  "Format HTTP request line"
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
    (let ((stream (net:socket-stream (connection-socket connection))))
      (write-string request stream)
      (force-output stream))))

(defun read-response (connection)
  "Read HTTP response from connection"
  (let* ((stream (net:socket-stream (connection-socket connection)))
         (response-lines '())
         (line nil))
    ;; Read status line and headers
    (loop while (setf line (read-line stream nil nil))
          do (push line response-lines)
          when (string= line "") do (return))
    ;; Try to read body based on Content-Length header
    (let* ((headers-text (str:join (reverse response-lines) (string #\Linefeed)))
           (content-length (extract-content-length headers-text))
           (body (when (and content-length (> content-length 0))
                   (let ((buffer (make-string content-length)))
                     (read-sequence buffer stream)
                     buffer)))
           (response-text (format nil "~A~@[~A~]" headers-text body)))
      (parse-response response-text))))

(defun parse-response (response-string)
  "Parse HTTP response into status, headers, and body"
  (let* ((lines (str:split response-string #\Linefeed))
         (status-line (first lines))
         (status-parts (str:split status-line #\Space))
         (status-code (parse-integer (second status-parts)))
         (headers map:+empty+)
         (body-start nil))
    
    ;; Parse headers
    (loop for i from 1 below (length lines)
          for line = (nth i lines)
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
                  (str:join (subseq lines body-start) 
                            (string #\Linefeed)))))
      (values status-code headers body))))

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

(defun get (url &key headers)
  "Make GET request"
  (request url :method "GET" :headers headers))

(defun post (url &key headers body)
  "Make POST request"
  (request url :method "POST" :headers headers :body body))

(defun put (url &key headers body)
  "Make PUT request"
  (request url :method "PUT" :headers headers :body body))

(defun delete (url &key headers)
  "Make DELETE request"
  (request url :method "DELETE" :headers headers))

(defun head (url &key headers)
  "Make HEAD request"
  (request url :method "HEAD" :headers headers))

(defun options (url &key headers)
  "Make OPTIONS request"
  (request url :method "OPTIONS" :headers headers))