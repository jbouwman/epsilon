;;;; epsilon.http.connection-pool - HTTP Connection Pooling
;;;;
;;;; Connection pooling for improved HTTP client performance

(defpackage epsilon.http.connection-pool
  (:use :cl)
  (:import (epsilon.net net)
            (epsilon.crypto crypto)
            (epsilon.map map)
            (epsilon.string str)
            (epsilon.sys.thread thread)
            (epsilon.sys.lock lock)))

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
  (lock (lock:make-lock "connection-pool") :type lock:lock)
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
  "Create a new connection pool. The first call also lazily starts the
   background cleanup thread; subsequent calls reuse it."
  (start-cleanup-thread)
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
                      ;; Close expired/dead connection (shutdown + close to avoid fd leak)
                      (ignore-errors
                       (when (pooled-connection-tls-connection conn)
                         (crypto:tls-close (pooled-connection-tls-connection conn)))
                       (when (pooled-connection-socket conn)
                         (handler-case (net:tcp-shutdown (pooled-connection-socket conn))
                           (error () nil))
                         (net:tcp-close (pooled-connection-socket conn))))
                      (incf (pool-stats-connections-closed (connection-pool-stats pool)))
                      t)))
                connections)))
         (setf (connection-pool-pools pool)
               (if active-connections
                   (map:assoc (connection-pool-pools pool) key active-connections)
                   (map:dissoc (connection-pool-pools pool) key)))))
     (connection-pool-pools pool))))

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
              (let ((tls-context (crypto:make-client-context)))
                (setf tls-conn (crypto:tls-connect socket tls-context)))
            (error (e)
              (net:tcp-shutdown socket)
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

  (lock:with-lock ((connection-pool-lock pool))
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
           ;; Send a simple probe to verify connection
           (probe-connection conn))
    (error () nil)))

(defun probe-connection (conn)
  "Send a minimal probe to verify connection is responsive"
  (handler-case
      (let ((socket (pooled-connection-socket conn)))
        ;; Try to peek at the socket without consuming data
        ;; If connection is closed, this will error
        (when socket
          ;; For HTTP, we can't send arbitrary data, so just check socket state
          ;; More sophisticated probing would require protocol-specific handling
          t))
    (error () nil)))

(defun return-connection (conn &key (pool *global-connection-pool*) force-close)
  "Return a connection to the pool"
  (ensure-global-pool)
  (unless pool (setf pool *global-connection-pool*))

  (if (or force-close
          (not (connection-alive-p conn))
          (not (pooled-connection-alive-p conn)))
      ;; Close connection (shutdown + close to avoid fd leak)
      (progn
        (ignore-errors
         (when (pooled-connection-tls-connection conn)
           (crypto:tls-close (pooled-connection-tls-connection conn)))
         (when (pooled-connection-socket conn)
           (handler-case (net:tcp-shutdown (pooled-connection-socket conn))
             (error () nil))
           (net:tcp-close (pooled-connection-socket conn))))
        (lock:with-lock ((connection-pool-lock pool))
          (incf (pool-stats-connections-closed (connection-pool-stats pool)))))

      ;; Return to pool
      (lock:with-lock ((connection-pool-lock pool))
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
              ;; Pool full, close connection (shutdown + close to avoid fd leak)
              (progn
                (ignore-errors
                 (when (pooled-connection-tls-connection conn)
                   (crypto:tls-close (pooled-connection-tls-connection conn)))
                 (when (pooled-connection-socket conn)
                   (handler-case (net:tcp-shutdown (pooled-connection-socket conn))
                     (error () nil))
                   (net:tcp-close (pooled-connection-socket conn))))
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
  (lock:with-lock ((connection-pool-lock pool))
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
  (lock:with-lock ((connection-pool-lock pool))
    (copy-structure (connection-pool-stats pool))))

(defun pool-metrics (pool &key (prefix "epsilon_http_pool"))
  "Return POOL's statistics as a list of (name . value) metric pairs.

   The format is intentionally simple so any monitoring backend can
   consume it: each entry is a cons whose car is a metric name string
   (PREFIX_<field>) and whose cdr is the integer value. Metric names
   follow Prometheus conventions (lowercase, underscore-separated)
   so a Prometheus exporter can render them directly.

   Returned metrics:
     <prefix>_connections_created      counter
     <prefix>_connections_reused       counter
     <prefix>_connections_closed       counter
     <prefix>_pool_hits                counter
     <prefix>_pool_misses              counter
     <prefix>_active_connections       gauge (current count of pooled
                                              idle connections)
     <prefix>_max_size                 gauge (configured max-size)"
  (let ((stats (pool-stats pool)))
    (list (cons (concatenate 'string prefix "_connections_created")
                (pool-stats-connections-created stats))
          (cons (concatenate 'string prefix "_connections_reused")
                (pool-stats-connections-reused stats))
          (cons (concatenate 'string prefix "_connections_closed")
                (pool-stats-connections-closed stats))
          (cons (concatenate 'string prefix "_pool_hits")
                (pool-stats-pool-hits stats))
          (cons (concatenate 'string prefix "_pool_misses")
                (pool-stats-pool-misses stats))
          (cons (concatenate 'string prefix "_active_connections")
                (pool-active-count pool))
          (cons (concatenate 'string prefix "_max_size")
                (connection-pool-max-size pool)))))

(defun format-prometheus-metrics (metrics &optional (stream *standard-output*))
  "Render METRICS (as returned by pool-metrics) in the Prometheus text
   exposition format. Each metric becomes a single line:
     <name> <value>
   No HELP or TYPE comments are emitted; the caller is expected to wrap
   this with whatever metadata its monitoring layer needs."
  (dolist (metric metrics)
    (format stream "~A ~D~%" (car metric) (cdr metric))))

(defun clear-connection-pool (&optional (pool *global-connection-pool*))
  "Clear all connections from pool"
  (ensure-global-pool)
  (unless pool (setf pool *global-connection-pool*))

  (lock:with-lock ((connection-pool-lock pool))
    (map:each
     (lambda (key connections)
       (declare (ignore key))
       (dolist (conn connections)
         (ignore-errors
          (when (pooled-connection-tls-connection conn)
            (crypto:tls-close (pooled-connection-tls-connection conn)))
          (when (pooled-connection-socket conn)
            (handler-case (net:tcp-shutdown (pooled-connection-socket conn))
              (error () nil))
            (net:tcp-close (pooled-connection-socket conn))))
         (incf (pool-stats-connections-closed (connection-pool-stats pool)))))
     (connection-pool-pools pool))
    (setf (connection-pool-pools pool) (map:make-map))))

(defun shutdown-connection-pool (&optional (pool *global-connection-pool*))
  "Shutdown connection pool and close all connections"
  (when pool
    (clear-connection-pool pool)
    (when (eq pool *global-connection-pool*)
      (setf *global-connection-pool* nil))))

;;;; Cleanup thread

(defvar *cleanup-thread* nil)
(defvar *cleanup-interval* 60) ; seconds

(defun start-cleanup-thread ()
  "Start background thread to cleanup expired connections"
  (unless *cleanup-thread*
    (setf *cleanup-thread*
          (thread:make-thread
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
    (thread:destroy-thread *cleanup-thread*)
    (setf *cleanup-thread* nil)))
