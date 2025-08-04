;;;; Redis Connection Pool
;;;;
;;;; Provides connection pooling for Redis clients with automatic
;;;; connection management and health checking.

(defpackage :epsilon.redis.pool
  (:use :cl)
  (:local-nicknames
   (#:conn #:epsilon.redis.connection)
   (#:thread #:epsilon.sys.thread)
   (#:lock #:epsilon.sys.lock))
  (:export
   #:create-redis-pool
   #:with-pooled-redis
   #:*default-redis-pool*
   #:pool-stats))

(in-package :epsilon.redis.pool)

;;; Pool configuration

(defparameter *default-pool-size* 10
  "Default maximum number of connections per pool")

(defparameter *default-pool-timeout* 5
  "Default timeout in seconds for acquiring a connection")

(defparameter *default-idle-timeout* 300
  "Default idle timeout in seconds before closing a connection")

;;; Pool structures

(defstruct redis-pool
  "Redis connection pool"
  (host "localhost" :type string)
  (port 6379 :type integer)
  (password nil)
  (database 0 :type integer)
  (max-size *default-pool-size* :type integer)
  (timeout *default-pool-timeout* :type integer)
  (idle-timeout *default-idle-timeout* :type integer)
  (connections nil :type list)
  (active-count 0 :type integer)
  (lock (lock:make-lock "redis-pool"))
  (stats (make-pool-stats) :type pool-stats))

(defstruct pool-stats
  "Connection pool statistics"
  (connections-created 0 :type integer)
  (connections-reused 0 :type integer)
  (connections-closed 0 :type integer)
  (pool-hits 0 :type integer)
  (pool-misses 0 :type integer)
  (wait-timeouts 0 :type integer))

(defstruct pooled-redis-connection
  "Wrapper for pooled Redis connections"
  (connection nil :type (or null conn:redis-connection))
  (pool nil :type (or null redis-pool))
  (last-used-time 0 :type integer)
  (request-count 0 :type integer))

;;; Global pool

(defvar *default-redis-pool* nil
  "Default global Redis connection pool")

(defun ensure-default-pool ()
  "Ensure default pool exists"
  (unless *default-redis-pool*
    (setf *default-redis-pool* (create-redis-pool))))

;;; Pool management

(defun create-redis-pool (&key (host "localhost") (port 6379) 
                               password (database 0)
                               (max-size *default-pool-size*)
                               (timeout *default-pool-timeout*)
                               (idle-timeout *default-idle-timeout*))
  "Create a new Redis connection pool"
  (make-redis-pool :host host
                   :port port
                   :password password
                   :database database
                   :max-size max-size
                   :timeout timeout
                   :idle-timeout idle-timeout))

(defun cleanup-idle-connections (pool)
  "Remove idle connections from pool"
  (let ((current-time (get-universal-time))
        (idle-timeout (redis-pool-idle-timeout pool))
        (active-connections nil))
    (dolist (pooled-conn (redis-pool-connections pool))
      (let ((idle-time (- current-time 
                          (pooled-redis-connection-last-used-time pooled-conn))))
        (if (or (> idle-time idle-timeout)
                (not (conn:connection-alive-p 
                      (pooled-redis-connection-connection pooled-conn))))
            ;; Close idle/dead connection
            (progn
              (ignore-errors
                (conn:disconnect (pooled-redis-connection-connection pooled-conn)))
              (incf (pool-stats-connections-closed 
                     (redis-pool-stats pool))))
            ;; Keep active connection
            (push pooled-conn active-connections))))
    (setf (redis-pool-connections pool) (nreverse active-connections))))

(defun acquire-connection (pool)
  "Acquire a connection from the pool"
  (let ((deadline (+ (get-universal-time) (redis-pool-timeout pool))))
    (loop
      (lock:with-lock ((redis-pool-lock pool))
        ;; Cleanup idle connections first
        (cleanup-idle-connections pool)
        
        ;; Try to get existing connection
        (when (redis-pool-connections pool)
          (let ((pooled-conn (pop (redis-pool-connections pool))))
            (incf (redis-pool-active-count pool))
            (incf (pool-stats-pool-hits (redis-pool-stats pool)))
            (incf (pool-stats-connections-reused (redis-pool-stats pool)))
            (incf (pooled-redis-connection-request-count pooled-conn))
            (setf (pooled-redis-connection-last-used-time pooled-conn)
                  (get-universal-time))
            (return-from acquire-connection pooled-conn)))
        
        ;; Create new connection if under limit
        (when (< (redis-pool-active-count pool) (redis-pool-max-size pool))
          (incf (redis-pool-active-count pool))
          (incf (pool-stats-pool-misses (redis-pool-stats pool)))
          (incf (pool-stats-connections-created (redis-pool-stats pool)))
          
          ;; Create connection outside of lock
          (let ((conn nil))
            (handler-case
                (setf conn (conn:connect (redis-pool-host pool)
                                         :port (redis-pool-port pool)
                                         :password (redis-pool-password pool)
                                         :database (redis-pool-database pool)
                                         :timeout (redis-pool-timeout pool)))
              (error (e)
                (lock:with-lock ((redis-pool-lock pool))
                  (decf (redis-pool-active-count pool)))
                (error e)))
            (return-from acquire-connection
              (make-pooled-redis-connection
               :connection conn
               :pool pool
               :last-used-time (get-universal-time)
               :request-count 1)))))
      
      ;; Check timeout
      (when (> (get-universal-time) deadline)
        (incf (pool-stats-wait-timeouts (redis-pool-stats pool)))
        (error 'conn:redis-connection-error
               :message "Timeout waiting for available connection"))
      
      ;; Brief sleep before retry
      (sleep 0.1))))

(defun release-connection (pooled-conn)
  "Release a connection back to the pool"
  (let ((pool (pooled-redis-connection-pool pooled-conn))
        (conn (pooled-redis-connection-connection pooled-conn)))
    (lock:with-lock ((redis-pool-lock pool))
      (decf (redis-pool-active-count pool))
      
      ;; Check if connection is still good
      (if (and conn (conn:connection-alive-p conn))
          ;; Return to pool
          (push pooled-conn (redis-pool-connections pool))
          ;; Close bad connection
          (progn
            (ignore-errors (when conn (conn:disconnect conn)))
            (incf (pool-stats-connections-closed 
                   (redis-pool-stats pool))))))))

;;; Public API

(defmacro with-pooled-redis ((var &optional pool) &body body)
  "Execute body with a pooled Redis connection"
  (let ((pooled-conn (gensym "POOLED-CONN"))
        (pool-var (gensym "POOL")))
    `(let ((,pool-var (or ,pool (progn
                                   (ensure-default-pool)
                                   *default-redis-pool*))))
       (let ((,pooled-conn (acquire-connection ,pool-var)))
         (unwind-protect
              (let ((,var (pooled-redis-connection-connection ,pooled-conn)))
                ,@body)
           (release-connection ,pooled-conn))))))

(defun pool-stats (pool)
  "Get pool statistics"
  (lock:with-lock ((redis-pool-lock pool))
    (let ((stats (redis-pool-stats pool)))
      (format nil "Pool Stats:
  Connections: ~D/~D active
  Created: ~D, Reused: ~D, Closed: ~D
  Hits: ~D, Misses: ~D, Timeouts: ~D"
              (redis-pool-active-count pool)
              (redis-pool-max-size pool)
              (pool-stats-connections-created stats)
              (pool-stats-connections-reused stats)
              (pool-stats-connections-closed stats)
              (pool-stats-pool-hits stats)
              (pool-stats-pool-misses stats)
              (pool-stats-wait-timeouts stats)))))