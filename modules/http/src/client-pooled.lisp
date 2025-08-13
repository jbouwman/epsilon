;;;; HTTP Client with Connection Pooling
;;;;
;;;; Enhanced HTTP client that uses connection pooling for better performance

(defpackage :epsilon.http.client.pooled
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:map #:epsilon.map)
   (#:tls #:epsilon.cryptography)
   (#:pool #:epsilon.http.connection-pool))
  (:export
   #:pooled-request
   #:pooled-http-get
   #:pooled-http-post
   #:pooled-http-put
   #:pooled-http-delete
   #:pooled-http-head
   #:pooled-http-options
   #:with-connection-pool
   
   ;; Pool management
   #:set-default-pool-config
   #:clear-connections
   #:pool-statistics))

(in-package :epsilon.http.client.pooled)

;;;; Configuration

(defparameter *use-connection-pooling* t
  "Whether to use connection pooling by default")

(defparameter *pool-config* 
  (list :max-size 10 :max-idle-time 300 :connection-timeout 30)
  "Default pool configuration")

;;;; URL Parsing (reused from original client)

(defun parse-url (url-string)
  "Parse URL into components"
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
         (authority-end (cond
                          (slash-pos slash-pos)
                          (question-pos question-pos)
                          (t (length rest-url))))
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

(defun parse-response (response-string)
  "Parse HTTP response into status, headers, and body"
  (let* ((lines (str:split #\Linefeed response-string))
         (status-line (seq:first lines))
         (status-parts (seq:realize (str:split #\Space status-line)))
         (status-code (parse-integer (second status-parts)))
         (headers (map:make-map))
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
                      (let ((key (str:trim (subseq line 0 colon-pos)))
                            (value (str:trim (subseq line (1+ colon-pos)))))
                        (setf headers (map:assoc headers (string-downcase key) value))))))))
      
      ;; Extract body
      (let ((body (when body-start
                    (str:join #\Linefeed (seq:from-list (subseq lines-list body-start))))))
        (values status-code headers body)))))

;;;; Pooled HTTP Client

(defun pooled-request (url &key (method "GET") headers body (use-pool *use-connection-pooling*) pool)
  "Make HTTP request using connection pooling"
  (multiple-value-bind (scheme host port path query)
      (parse-url url)
    (let ((ssl-p (string= scheme "https")))
      
      (if use-pool
          ;; Use connection pooling
          (pool:with-pooled-connection (conn host port :ssl-p ssl-p :pool pool)
            (pool:send-http-request-pooled conn method path headers body query)
            (multiple-value-bind (response-headers response-body)
                (pool:read-http-response-pooled conn)
              (parse-response (if response-body 
                                  (concatenate 'string response-headers response-body)
                                  response-headers))))
          
          ;; Fall back to non-pooled request
          (error "Non-pooled requests not implemented yet")))))

(defun pooled-http-get (url &key headers pool)
  "Make GET request using connection pooling"
  (pooled-request url :method "GET" :headers headers :pool pool))

(defun pooled-http-post (url &key headers body pool)
  "Make POST request using connection pooling"
  (pooled-request url :method "POST" :headers headers :body body :pool pool))

(defun pooled-http-put (url &key headers body pool)
  "Make PUT request using connection pooling"
  (pooled-request url :method "PUT" :headers headers :body body :pool pool))

(defun pooled-http-delete (url &key headers pool)
  "Make DELETE request using connection pooling"
  (pooled-request url :method "DELETE" :headers headers :pool pool))

(defun pooled-http-head (url &key headers pool)
  "Make HEAD request using connection pooling"
  (pooled-request url :method "HEAD" :headers headers :pool pool))

(defun pooled-http-options (url &key headers pool)
  "Make OPTIONS request using connection pooling"
  (pooled-request url :method "OPTIONS" :headers headers :pool pool))

;;;; Pool Management

(defmacro with-connection-pool ((pool-var &key max-size max-idle-time connection-timeout) &body body)
  "Execute body with a custom connection pool"
  `(let ((,pool-var (pool:create-connection-pool 
                     :max-size (or ,max-size 10)
                     :max-idle-time (or ,max-idle-time 300)
                     :connection-timeout (or ,connection-timeout 30))))
     (unwind-protect
          (progn ,@body)
       (pool:shutdown-connection-pool ,pool-var))))

(defun set-default-pool-config (&key max-size max-idle-time connection-timeout)
  "Set default connection pool configuration"
  (when max-size
    (setf (getf *pool-config* :max-size) max-size))
  (when max-idle-time
    (setf (getf *pool-config* :max-idle-time) max-idle-time))
  (when connection-timeout
    (setf (getf *pool-config* :connection-timeout) connection-timeout)))

(defun clear-connections (&optional pool)
  "Clear all connections from pool"
  (pool:clear-connection-pool pool))

(defun pool-statistics (&optional pool)
  "Get connection pool statistics"
  (let ((stats (pool:pool-stats (or pool pool:*global-connection-pool*))))
    (list :connections-created (pool:pool-stats-connections-created stats)
          :connections-reused (pool:pool-stats-connections-reused stats)
          :connections-closed (pool:pool-stats-connections-closed stats)
          :pool-hits (pool:pool-stats-pool-hits stats)
          :pool-misses (pool:pool-stats-pool-misses stats)
          :active-connections (pool:pool-active-count (or pool pool:*global-connection-pool*))
          :idle-connections (pool:pool-idle-count (or pool pool:*global-connection-pool*)))))