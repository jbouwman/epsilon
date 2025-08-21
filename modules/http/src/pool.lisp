;;;; Modern HTTP Connection Pool
;;;;
;;;; High-performance HTTP connection pooling using epsilon.pool abstraction.
;;;; Inspired by Rust's reqwest and Go's http.Transport connection pooling.

(defpackage :epsilon.http.pool
  (:use :cl)
  (:local-nicknames
   (#:pool #:epsilon.pool)
   (#:client #:epsilon.http.client)
   (#:net #:epsilon.net)
   (#:crypto #:epsilon.crypto)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:time #:epsilon.time))
  (:export
   ;; Connection pool management
   #:http-connection-pool
   #:create-http-connection-pool
   #:destroy-http-connection-pool
   #:pool-stats
   
   ;; Connection operations
   #:get-connection
   #:return-connection
   #:destroy-http-connection
   #:with-http-connection
   
   ;; High-level HTTP operations
   #:pooled-request
   #:pooled-get
   #:pooled-post
   #:pooled-put
   #:pooled-delete
   
   ;; Configuration
   #:pool-config
   #:make-pool-config
   #:*default-pool-config*
   
   ;; Connection key for pooling
   #:connection-key
   #:make-connection-key
   
   ;; HTTP connection accessors
   #:http-connection
   #:http-connection-stream
   #:http-connection-socket
   #:http-connection-key
   #:http-connection-tls-connection
   #:http-connection-protocol
   #:http-connection-created-at
   #:http-connection-last-used-at
   #:http-connection-keep-alive-p
   
   ;; Pool health and maintenance
   #:pool-health-check
   #:warm-up-pool))

(in-package :epsilon.http.pool)

;;;; Configuration

(defstruct pool-config
  "Configuration for HTTP connection pooling"
  (max-connections-per-host 10 :type integer)
  (max-idle-connections 100 :type integer)
  (idle-timeout 90 :type integer) ; seconds
  (connection-timeout 30 :type integer) ; seconds
  (keep-alive-timeout 30 :type integer) ; seconds
  (enable-http2 nil :type boolean)
  (enable-compression t :type boolean)
  (validate-connections t :type boolean)
  (warm-up-hosts '() :type list)) ; List of (host port ssl-p) to warm up

(defparameter *default-pool-config* (make-pool-config)
  "Default HTTP connection pool configuration")

;;;; Connection Key for Pool Partitioning

(defstruct connection-key
  "Key for partitioning connections by host/port/protocol"
  (host "" :type string)
  (port 80 :type integer)
  (ssl-p nil :type boolean)
  (protocol :http/1.1 :type keyword)) ; :http/1.1 or :http/2

(defmethod print-object ((key connection-key) stream)
  (format stream "#<CONNECTION-KEY ~A://~A:~D~@[ (~A)~]>"
          (if (connection-key-ssl-p key) "https" "http")
          (connection-key-host key)
          (connection-key-port key)
          (unless (eq (connection-key-protocol key) :http/1.1)
            (connection-key-protocol key))))

(defun connection-key-equal (key1 key2)
  "Test if two connection keys are equal"
  (and (string= (connection-key-host key1) (connection-key-host key2))
       (= (connection-key-port key1) (connection-key-port key2))
       (eq (connection-key-ssl-p key1) (connection-key-ssl-p key2))
       (eq (connection-key-protocol key1) (connection-key-protocol key2))))

(defun connection-key-hash (key)
  "Generate hash for connection key"
  (sxhash (format nil "~A:~D:~A:~A"
                  (connection-key-host key)
                  (connection-key-port key)
                  (connection-key-ssl-p key)
                  (connection-key-protocol key))))

;;;; HTTP Connection Wrapper

(defstruct http-connection
  "HTTP connection with metadata for pooling"
  (key nil :type (or null connection-key))
  (socket nil)
  (tls-connection nil)
  (stream nil)
  (created-at 0 :type integer)
  (last-used-at 0 :type integer)
  (request-count 0 :type integer)
  (protocol :http/1.1 :type keyword)
  (alive-p t :type boolean)
  (keep-alive-p t :type boolean))

;;;; HTTP Connection Pool

(defstruct http-connection-pool
  "HTTP connection pool using per-host sub-pools"
  (pools (make-hash-table :test 'equal) :type hash-table) ; key -> pool
  (config (make-pool-config) :type pool-config)
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (total-stats (pool:make-pool-stats) :type pool:pool-stats))

(defun create-http-connection-pool (&key (config *default-pool-config*))
  "Create a new HTTP connection pool"
  (let ((pool (make-http-connection-pool :config config)))
    ;; Warm up specified hosts if specified
    (when (and config (fboundp 'pool-config-warm-up-hosts))
      (dolist (host-spec (pool-config-warm-up-hosts config))
        (destructuring-bind (host port ssl-p) host-spec
          (warm-up-host pool host port ssl-p))))
    pool))

(defun destroy-http-connection-pool (pool)
  "Shutdown HTTP connection pool and all sub-pools"
  (sb-thread:with-mutex ((http-connection-pool-lock pool))
    (maphash (lambda (key sub-pool)
               (declare (ignore key))
               (pool:destroy-pool sub-pool))
             (http-connection-pool-pools pool))
    (clrhash (http-connection-pool-pools pool))))

;;;; Connection Factory Functions

(defun create-http-connection (key config)
  "Factory function to create new HTTP connections"
  (let* ((host (connection-key-host key))
         (port (connection-key-port key))
         (ssl-p (connection-key-ssl-p key))
         (protocol (connection-key-protocol key))
         (current-time (get-universal-time)))
    
    (handler-case
        (let* ((address (net:resolve-address host port))
               (socket (net:tcp-connect address 
                                       :timeout (pool-config-connection-timeout config))))
          
          ;; Configure socket
          (net:set-socket-option socket :keep-alive t)
          (net:set-socket-option socket :no-delay t)
          (when (pool-config-keep-alive-timeout config)
            (net:set-socket-option socket :recv-timeout 
                                  (pool-config-keep-alive-timeout config)))
          
          ;; Setup TLS if needed
          (let ((tls-conn nil)
                (stream nil))
            (if ssl-p
                (progn
                  (let ((tls-context (crypto:create-tls-context :server-p nil)))
                    ;; Configure TLS context for HTTP
                    ;; TODO: Implement ALPN protocol support
                    ;; (when (pool-config-enable-http2 config)
                    ;;   (crypto:set-alpn-protocols tls-context '("h2" "http/1.1")))
                    
                    (setf tls-conn (crypto:tls-connect socket tls-context))
                    (setf stream (crypto:tls-stream tls-conn))
                    
                    ;; Check negotiated protocol
                    ;; TODO: Implement ALPN protocol negotiation checking
                    ;; (when (pool-config-enable-http2 config)
                    ;;   (let ((negotiated (crypto:get-alpn-selected tls-conn)))
                    ;;     (when (string= negotiated "h2")
                    ;;       (setf protocol :http/2))))
                    ))
                
                ;; Plain HTTP
                (setf stream (net:tcp-stream socket)))
            
            (make-http-connection
             :key key
             :socket socket
             :tls-connection tls-conn
             :stream stream
             :created-at current-time
             :last-used-at current-time
             :protocol protocol
             :keep-alive-p t)))
      
      (error (e)
        (error "Failed to create HTTP connection to ~A:~D: ~A" host port e)))))

(defun validate-http-connection (connection)
  "Validate that HTTP connection is still alive and usable"
  (handler-case
      (and (http-connection-alive-p connection)
           (http-connection-socket connection)
           (net:tcp-connected-p (http-connection-socket connection))
           ;; Could add ping/probe here for more robust validation
           t)
    (error () nil)))

(defun destroy-http-connection (connection)
  "Cleanup HTTP connection resources"
  (setf (http-connection-alive-p connection) nil)
  (ignore-errors
    (when (http-connection-tls-connection connection)
      (crypto:tls-close (http-connection-tls-connection connection)))
    (when (http-connection-socket connection)
      (net:tcp-shutdown (http-connection-socket connection) :both))))

;;;; Pool Operations

(defun get-connection-pool (http-pool key)
  "Get or create sub-pool for connection key"
  (or (gethash key (http-connection-pool-pools http-pool))
      (let ((config (http-connection-pool-config http-pool)))
        (setf (gethash key (http-connection-pool-pools http-pool))
              (pool:create-pool
               :factory (lambda () (create-http-connection key config))
               :destroyer #'destroy-http-connection
               :validator #'validate-http-connection
               :max-size (pool-config-max-connections-per-host config)
               :idle-timeout (pool-config-idle-timeout config)
               :acquire-timeout (pool-config-connection-timeout config)
               :validation-on-acquire (pool-config-validate-connections config)
               :validation-on-release (pool-config-validate-connections config))))))

(defun get-connection (http-pool host port &key ssl-p protocol timeout)
  "Get an HTTP connection from the pool"
  (let* ((key (make-connection-key 
               :host host 
               :port port 
               :ssl-p ssl-p
               :protocol (or protocol :http/1.1)))
         (sub-pool (sb-thread:with-mutex ((http-connection-pool-lock http-pool))
                     (get-connection-pool http-pool key))))
    
    (let ((connection (pool:acquire sub-pool :timeout timeout)))
      ;; Update connection metadata
      (setf (http-connection-last-used-at connection) (get-universal-time))
      (incf (http-connection-request-count connection))
      connection)))

(defun return-connection (http-pool connection &key force-close)
  "Return an HTTP connection to the pool"
  (let* ((key (http-connection-key connection))
         (sub-pool (sb-thread:with-mutex ((http-connection-pool-lock http-pool))
                     (gethash key (http-connection-pool-pools http-pool)))))
    
    (when sub-pool
      (if (or force-close 
              (not (http-connection-keep-alive-p connection))
              (not (validate-http-connection connection)))
          ;; Force close or connection not reusable
          (pool:release sub-pool connection)
          ;; Return to pool for reuse
          (progn
            (setf (http-connection-last-used-at connection) (get-universal-time))
            (pool:release sub-pool connection))))))

(defmacro with-http-connection ((conn-var http-pool host port 
                                &key ssl-p protocol timeout) 
                               &body body)
  "Execute body with an HTTP connection from the pool"
  `(let ((,conn-var (get-connection ,http-pool ,host ,port 
                                   :ssl-p ,ssl-p 
                                   :protocol ,protocol
                                   :timeout ,timeout)))
     (unwind-protect
          (progn ,@body)
       (when ,conn-var
         (return-connection ,http-pool ,conn-var)))))

;;;; High-Level HTTP Operations

(defun pooled-request (http-pool method url &key headers body timeout)
  "Make HTTP request using connection pool"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url url)
    
    (let ((ssl-p (string= scheme "https")))
      (with-http-connection (conn http-pool host port 
                                 :ssl-p ssl-p 
                                 :timeout timeout)
        
        ;; Send request using the pooled connection
        (client::send-request-on-connection conn method path headers body query)
        
        ;; Read response
        (client::read-response-from-connection conn)))))

(defun pooled-get (http-pool url &key headers timeout)
  "Make HTTP GET request using connection pool"
  (pooled-request http-pool "GET" url :headers headers :timeout timeout))

(defun pooled-post (http-pool url &key headers body timeout)
  "Make HTTP POST request using connection pool"
  (pooled-request http-pool "POST" url :headers headers :body body :timeout timeout))

(defun pooled-put (http-pool url &key headers body timeout)
  "Make HTTP PUT request using connection pool"
  (pooled-request http-pool "PUT" url :headers headers :body body :timeout timeout))

(defun pooled-delete (http-pool url &key headers timeout)
  "Make HTTP DELETE request using connection pool"
  (pooled-request http-pool "DELETE" url :headers headers :timeout timeout))

;;;; Pool Health and Statistics

(defun pool-stats (http-pool)
  "Get aggregate statistics for all sub-pools"
  (sb-thread:with-mutex ((http-connection-pool-lock http-pool))
    (let ((total-stats (pool:make-pool-stats)))
      (maphash (lambda (key sub-pool)
                 (declare (ignore key))
                 (let ((stats (pool:pool-stats sub-pool)))
                   (incf (pool:pool-stats-created total-stats)
                         (pool:pool-stats-created stats))
                   (incf (pool:pool-stats-destroyed total-stats)
                         (pool:pool-stats-destroyed stats))
                   (incf (pool:pool-stats-acquired total-stats)
                         (pool:pool-stats-acquired stats))
                   (incf (pool:pool-stats-released total-stats)
                         (pool:pool-stats-released stats))
                   (incf (pool:pool-stats-timeouts total-stats)
                         (pool:pool-stats-timeouts stats))
                   (incf (pool:pool-stats-validation-failures total-stats)
                         (pool:pool-stats-validation-failures stats))))
               (http-connection-pool-pools http-pool))
      total-stats)))

(defun pool-health-check (http-pool)
  "Perform health check on all sub-pools"
  (sb-thread:with-mutex ((http-connection-pool-lock http-pool))
    (maphash (lambda (key sub-pool)
               (declare (ignore key))
               (pool:pool-health-check sub-pool))
             (http-connection-pool-pools http-pool))))

(defun warm-up-pool (http-pool host-specs)
  "Warm up connection pools for specified hosts"
  (dolist (spec host-specs)
    (destructuring-bind (host port ssl-p &optional (count 1)) spec
      (warm-up-host http-pool host port ssl-p count))))

(defun warm-up-host (http-pool host port ssl-p &optional (count 1))
  "Warm up connections for a specific host"
  (let* ((key (make-connection-key :host host :port port :ssl-p ssl-p))
         (sub-pool (sb-thread:with-mutex ((http-connection-pool-lock http-pool))
                     (get-connection-pool http-pool key))))
    
    ;; Create and immediately return connections to warm up the pool
    (dotimes (i count)
      (ignore-errors
        (let ((conn (pool:acquire sub-pool :timeout 5)))
          (when conn
            (pool:release sub-pool conn)))))))

;;;; Integration with HTTP Client

;; Update the main HTTP client to use pooling by default
(defvar *global-http-pool* nil
  "Global HTTP connection pool")

(defun ensure-global-http-pool ()
  "Ensure global HTTP pool exists"
  (unless *global-http-pool*
    (setf *global-http-pool* (make-http-connection-pool))))

(defun use-pooled-client (&optional (pool-config *default-pool-config*))
  "Configure HTTP client to use connection pooling"
  (ensure-global-http-pool)
  (setf *global-http-pool* (make-http-connection-pool :config pool-config)))

;; Extend client functions to support pooling
(defun client::send-request-on-connection (connection method path headers body query)
  "Send HTTP request on existing connection"
  (let ((stream (http-connection-stream connection))
        (host (connection-key-host (http-connection-key connection))))
    
    ;; Build request line
    (let ((request-line (if query
                            (format nil "~A ~A?~A HTTP/1.1" method path query)
                            (format nil "~A ~A HTTP/1.1" method path))))
      
      ;; Send request
      (format stream "~A~C~C" request-line #\Return #\Linefeed)
      
      ;; Send headers
      (let ((default-headers (map:make-map "Host" host
                                          "User-Agent" "epsilon.http/2.0"
                                          "Connection" "keep-alive")))
        (when headers
          (setf default-headers (map:merge default-headers headers)))
        
        (when body
          (setf default-headers 
                (map:assoc default-headers "Content-Length" 
                          (format nil "~D" (length body)))))
        
        (map:each (lambda (name value)
                    (format stream "~A: ~A~C~C" name value #\Return #\Linefeed))
                  default-headers))
      
      ;; End headers
      (format stream "~C~C" #\Return #\Linefeed)
      
      ;; Send body
      (when body
        (write-string body stream))
      
      (force-output stream))))

(defun client::read-response-from-connection (connection)
  "Read HTTP response from existing connection"
  (let ((stream (http-connection-stream connection)))
    ;; Reuse existing response parsing logic
    (client::read-response stream)))