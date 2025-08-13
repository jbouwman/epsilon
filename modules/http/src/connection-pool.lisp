;;;; HTTP Connection Pooling
;;;;
;;;; Connection pooling for improved HTTP client performance

(defpackage :epsilon.http.connection-pool
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:tls #:epsilon.crypto)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string))
  (:export
   ;; Connection pool management
   #:create-connection-pool
   #:connection-pool-p
   #:shutdown-connection-pool
   #:clear-connection-pool
   #:pool-stats
   
   ;; Connection management
   #:get-connection
   #:return-connection
   #:with-pooled-connection
   
   ;; HTTP integration
   #:send-http-request-pooled
   #:read-http-response-pooled
   
   ;; Configuration
   #:*default-pool-size*
   #:*default-max-idle-time*
   #:*default-connection-timeout*
   #:*global-connection-pool*
   
   ;; Pool information
   #:pool-active-count
   #:pool-idle-count
   #:pool-total-count
   
   ;; Pool statistics
   #:pool-stats
   #:pool-stats-connections-created
   #:pool-stats-connections-reused
   #:pool-stats-connections-closed
   #:pool-stats-pool-hits
   #:pool-stats-pool-misses))

(in-package :epsilon.http.connection-pool)

;;;; Configuration

(defparameter *default-pool-size* 10
  "Default maximum number of connections per host")

(defparameter *default-max-idle-time* 300
  "Default maximum idle time in seconds before closing connection")

(defparameter *default-connection-timeout* 30
  "Default connection timeout in seconds")

;;;; Connection Pool Data Structures

(defstruct pooled-connection
  "A pooled HTTP connection"
  (socket nil)
  (tls-connection nil)
  (host "" :type string)
  (port 80 :type integer)
  (ssl-p nil :type boolean)
  (created-time 0 :type integer)
  (last-used-time 0 :type integer)
  (request-count 0 :type integer)
  (alive-p t :type boolean))

(defstruct connection-pool
  "HTTP connection pool"
  (pools (map:make-map)) ; host:port -> list of connections
  (max-size *default-pool-size* :type integer)
  (max-idle-time *default-max-idle-time* :type integer)
  (connection-timeout *default-connection-timeout* :type integer)
  (lock (sb-thread:make-mutex :name "connection-pool") :type sb-thread:mutex)
  (stats (make-pool-stats) :type pool-stats))

(defstruct pool-stats
  "Connection pool statistics"
  (connections-created 0 :type integer)
  (connections-reused 0 :type integer)
  (connections-closed 0 :type integer)
  (pool-hits 0 :type integer)
  (pool-misses 0 :type integer))

;;;; Global connection pool

(defvar *global-connection-pool* nil
  "Global HTTP connection pool")

(defun ensure-global-pool ()
  "Ensure global connection pool exists"
  (unless *global-connection-pool*
    (setf *global-connection-pool* (create-connection-pool))))

;;;; Pool Management

(defun create-connection-pool (&key (max-size *default-pool-size*)
                                 (max-idle-time *default-max-idle-time*)
                                 (connection-timeout *default-connection-timeout*))
  "Create a new connection pool"
  (make-connection-pool :max-size max-size
                        :max-idle-time max-idle-time
                        :connection-timeout connection-timeout))

(defun pool-key (host port)
  "Generate pool key for host:port combination"
  (format nil "~A:~D" host port))

(defun cleanup-expired-connections (pool)
  "Remove expired connections from pool"
  (let ((current-time (get-universal-time))
        (max-idle (connection-pool-max-idle-time pool)))
    (map:each 
     (lambda (key connections)
       (let ((active-connections
               (remove-if 
                (lambda (conn)
                  (let ((idle-time (- current-time (pooled-connection-last-used-time conn))))
                    (when (or (> idle-time max-idle)
                              (not (pooled-connection-alive-p conn)))
                      ;; Close expired/dead connection
                      (ignore-errors
                       (when (pooled-connection-tls-connection conn)
                         (tls:tls-close (pooled-connection-tls-connection conn)))
                       (when (pooled-connection-socket conn)
                         (net:tcp-shutdown (pooled-connection-socket conn) :both))))
                    (incf (pool-stats-connections-closed (connection-pool-stats pool)))
                    t))
                connections))
             (setf (connection-pool-pools pool)
                   (if active-connections
                       (map:assoc (connection-pool-pools pool) key active-connections)
                       (map:dissoc (connection-pool-pools pool) key)))))
       (connection-pool-pools pool)))))

(defun create-new-connection (host port ssl-p timeout)
  "Create a new HTTP connection"
  (let* ((address (net:make-socket-address host port))
         (current-time (get-universal-time)))
    
    ;; Connect to host 
    (let ((socket (handler-case
                      (net:tcp-connect address)
                    (error (e)
                      (error "Failed to connect to ~A:~D: ~A" host port e)))))
      
      ;; Set socket timeout if needed
      (when timeout
        (net:set-socket-option socket :recv-timeout timeout)
        (net:set-socket-option socket :send-timeout timeout))
      
      ;; Create TLS connection if needed
      (let ((tls-conn nil))
        (when ssl-p
          (handler-case
              (let ((tls-context (tls:create-tls-context :server-p nil)))
                (setf tls-conn (tls:tls-connect socket tls-context)))
            (error (e)
              (net:tcp-shutdown socket :both)
              (error "TLS handshake failed for ~A:~D: ~A" host port e))))
        
        (make-pooled-connection :socket socket
                                :tls-connection tls-conn
                                :host host
                                :port port
                                :ssl-p ssl-p
                                :created-time current-time
                                :last-used-time current-time)))))

(defun get-connection (host port &key ssl-p (pool *global-connection-pool*))
  "Get a connection from the pool"
  (ensure-global-pool)
  (unless pool (setf pool *global-connection-pool*))
  
  (sb-thread:with-mutex ((connection-pool-lock pool))
    ;; Cleanup expired connections
    (cleanup-expired-connections pool)
    
    (let* ((key (pool-key host port))
           (connections (map:get (connection-pool-pools pool) key)))
      
      (cond
        ;; Reuse existing connection
        (connections
         (let ((conn (pop connections)))
           ;; Update pool
           (setf (connection-pool-pools pool)
                 (if connections
                     (map:assoc (connection-pool-pools pool) key connections)
                     (map:dissoc (connection-pool-pools pool) key)))
           
           ;; Update stats and connection
           (incf (pool-stats-connections-reused (connection-pool-stats pool)))
           (incf (pool-stats-pool-hits (connection-pool-stats pool)))
           (setf (pooled-connection-last-used-time conn) (get-universal-time))
           (incf (pooled-connection-request-count conn))
           
           conn))
        
        ;; Create new connection
        (t
         (incf (pool-stats-pool-misses (connection-pool-stats pool)))
         (incf (pool-stats-connections-created (connection-pool-stats pool)))
         
         (create-new-connection host port ssl-p 
                                (connection-pool-connection-timeout pool)))))))

(defun connection-alive-p (conn)
  "Check if connection is still alive"
  (handler-case
      (and (pooled-connection-socket conn)
           (net:tcp-connected-p (pooled-connection-socket conn))
           ;; TODO: Send a simple probe to verify connection
           t)
    (error () nil)))

(defun return-connection (conn &key (pool *global-connection-pool*) force-close)
  "Return a connection to the pool"
  (ensure-global-pool)
  (unless pool (setf pool *global-connection-pool*))
  
  (if (or force-close
          (not (connection-alive-p conn))
          (not (pooled-connection-alive-p conn)))
      ;; Close connection
      (progn
        (ignore-errors
         (when (pooled-connection-tls-connection conn)
           (tls:tls-close (pooled-connection-tls-connection conn)))
         (when (pooled-connection-socket conn)
           (net:tcp-shutdown (pooled-connection-socket conn) :both)))
        (sb-thread:with-mutex ((connection-pool-lock pool))
          (incf (pool-stats-connections-closed (connection-pool-stats pool)))))
      
      ;; Return to pool
      (sb-thread:with-mutex ((connection-pool-lock pool))
        (let* ((key (pool-key (pooled-connection-host conn)
                              (pooled-connection-port conn)))
               (connections (map:get (connection-pool-pools pool) key)))
          
          ;; Check pool size limit
          (if (< (length connections) (connection-pool-max-size pool))
              ;; Add to pool
              (progn
                (setf (pooled-connection-last-used-time conn) (get-universal-time))
                (setf (connection-pool-pools pool)
                      (map:assoc (connection-pool-pools pool) key (cons conn connections))))
              ;; Pool full, close connection
              (progn
                (ignore-errors
                 (when (pooled-connection-tls-connection conn)
                   (tls:tls-close (pooled-connection-tls-connection conn)))
                 (when (pooled-connection-socket conn)
                   (net:tcp-shutdown (pooled-connection-socket conn) :both)))
                (incf (pool-stats-connections-closed (connection-pool-stats pool)))))))))

(defmacro with-pooled-connection ((conn-var host port &key ssl-p pool) &body body)
  "Execute body with a pooled connection"
  `(let ((,conn-var (get-connection ,host ,port :ssl-p ,ssl-p :pool ,pool)))
     (unwind-protect
          (progn ,@body)
       (when ,conn-var
         (return-connection ,conn-var :pool ,pool)))))

;;;; Pool Information and Management

(defun pool-active-count (pool)
  "Get number of active connections in pool"
  (sb-thread:with-mutex ((connection-pool-lock pool))
    (reduce #'+ (map:vals (connection-pool-pools pool))
            :key #'length :initial-value 0)))

(defun pool-idle-count (pool)
  "Get number of idle connections in pool"
  ;; In this implementation, all pooled connections are idle
  (pool-active-count pool))

(defun pool-total-count (pool)
  "Get total number of connections managed by pool"
  (pool-active-count pool))

(defun pool-stats (pool)
  "Get pool statistics"
  (sb-thread:with-mutex ((connection-pool-lock pool))
    (copy-structure (connection-pool-stats pool))))

(defun clear-connection-pool (&optional (pool *global-connection-pool*))
  "Clear all connections from pool"
  (ensure-global-pool)
  (unless pool (setf pool *global-connection-pool*))
  
  (sb-thread:with-mutex ((connection-pool-lock pool))
    (map:each
     (lambda (key connections)
       (declare (ignore key))
       (dolist (conn connections)
         (ignore-errors
          (when (pooled-connection-tls-connection conn)
            (tls:tls-close (pooled-connection-tls-connection conn)))
          (when (pooled-connection-socket conn)
            (net:tcp-shutdown (pooled-connection-socket conn) :both)))
         (incf (pool-stats-connections-closed (connection-pool-stats pool)))))
     (connection-pool-pools pool))
    (setf (connection-pool-pools pool) (map:make-map))))

(defun shutdown-connection-pool (&optional (pool *global-connection-pool*))
  "Shutdown connection pool and close all connections"
  (when pool
    (clear-connection-pool pool)
    (when (eq pool *global-connection-pool*)
      (setf *global-connection-pool* nil))))

;;;; Integration with HTTP client

(defun pooled-socket-stream (conn)
  "Get stream for pooled connection"
  (if (pooled-connection-ssl-p conn)
      (tls:tls-stream (pooled-connection-tls-connection conn))
      ;; Return a bidirectional stream
      (let ((socket (pooled-connection-socket conn)))
        (make-two-way-stream (net:tcp-stream-reader socket)
                             (net:tcp-stream-writer socket)))))

(defun send-http-request-pooled (conn method path headers body query)
  "Send HTTP request using pooled connection"
  (let* ((stream (pooled-socket-stream conn))
         (request-line (format nil "~A ~A~@[?~A~] HTTP/1.1" method path query))
         (default-headers (map:make-map 
                           "Host" (pooled-connection-host conn)
                           "User-Agent" "epsilon.http/1.0"
                           "Connection" "keep-alive"))
         (all-headers (map:merge default-headers (or headers (map:make-map))))
         (final-headers (if body
                            (map:assoc all-headers 
                                       "Content-Length" 
                                       (format nil "~D" (length body)))
                            all-headers)))
    
    ;; Send request
    (format stream "~A~C~C" request-line #\Return #\Linefeed)
    (map:each (lambda (k v)
                (format stream "~A: ~A~C~C" k v #\Return #\Linefeed))
              final-headers)
    (format stream "~C~C" #\Return #\Linefeed)
    (when body
      (write-string body stream))
    (force-output stream)))

(defun read-http-response-pooled (conn)
  "Read HTTP response from pooled connection"
  (let* ((stream (pooled-socket-stream conn))
         (response-lines '())
         (line nil))
    ;; Read status line and headers
    (loop while (setf line (read-line stream nil nil))
          do (push line response-lines)
          when (string= line "") do (return))
    
    ;; Parse headers to get content length
    (let* ((headers-text (format nil "~{~A~^~%~}" (reverse response-lines)))
           (content-length-start (search "Content-Length:" headers-text :test #'char-equal)))
      
      (if content-length-start
          ;; Read body based on Content-Length
          (let* ((line-end (position #\Newline headers-text :start content-length-start))
                 (length-str (subseq headers-text 
                                     (+ content-length-start 15) ; "Content-Length:" length
                                     line-end))
                 (content-length (parse-integer (str:trim length-str) :junk-allowed t))
                 (body (when (and content-length (> content-length 0))
                         (let ((buffer (make-string content-length)))
                           (read-sequence buffer stream)
                           buffer))))
            (values headers-text body))
          ;; No content length, read until connection closes or timeout
          (values headers-text nil)))))

;;;; Cleanup thread

(defvar *cleanup-thread* nil)
(defvar *cleanup-interval* 60) ; seconds

(defun start-cleanup-thread ()
  "Start background thread to cleanup expired connections"
  (unless *cleanup-thread*
    (setf *cleanup-thread*
          (sb-thread:make-thread
           (lambda ()
             (loop
              (sleep *cleanup-interval*)
              (when *global-connection-pool*
                (handler-case
                    (cleanup-expired-connections *global-connection-pool*)
                  (error (e)
                    (warn "Connection pool cleanup error: ~A" e))))))
           :name "HTTP connection pool cleanup"))))

(defun stop-cleanup-thread ()
  "Stop background cleanup thread"
  (when *cleanup-thread*
    (sb-thread:terminate-thread *cleanup-thread*)
    (setf *cleanup-thread* nil)))

;; Start cleanup thread when module loads
(eval-when (:load-toplevel :execute)
  (start-cleanup-thread))
