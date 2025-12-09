;;;; WebSocket Connection Pool
;;;;
;;;; This module provides connection pooling for WebSocket clients to improve
;;;; performance by reusing connections.

(defpackage epsilon.websocket.pool
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (conn epsilon.websocket.connection)
   (client epsilon.websocket.client)
   (lock epsilon.sys.lock)
   (thread epsilon.sys.thread)
   (map epsilon.map))
  (:export
   ;; Pool structure
   connection-pool
   create-connection-pool
   connection-pool-max-connections
   connection-pool-max-idle-time
   connection-pool-size
   connection-pool-active-count
   
   ;; Pool operations
   acquire-connection
   release-connection
   close-pool
   
   ;; Pool errors
   pool-exhausted-error))

(in-package epsilon.websocket.pool)

;;; Error conditions

(define-condition pool-exhausted-error (error)
  ((message :initarg :message :reader pool-error-message))
  (:report (lambda (condition stream)
             (format stream "Connection pool exhausted: ~A"
                     (pool-error-message condition)))))

;;; Pool entry structure

(defstruct pool-entry
  "Entry in connection pool"
  (connection nil :type (or null conn:websocket-connection))
  (uri nil :type (or null string))
  (in-use nil :type boolean)
  (last-used 0 :type integer)
  (created 0 :type integer))

;;; Connection pool structure

(defstruct connection-pool
  "WebSocket connection pool"
  (max-connections 10 :type integer)
  (max-idle-time 300 :type number) ; seconds
  (entries nil :type list)
  (lock (lock:make-lock) :type lock:lock)
  (cleanup-thread nil :type (or null thread:thread))
  (running t :type boolean))

;; Rename the constructor to avoid conflict
(defun create-connection-pool (&key (max-connections 10) (max-idle-time 300))
  "Create a new connection pool"
  (let ((pool (make-connection-pool
               :max-connections max-connections
               :max-idle-time max-idle-time
               :entries '()
               :running t)))
    ;; Start cleanup thread
    (setf (connection-pool-cleanup-thread pool)
          (thread:make-thread
           (lambda () (cleanup-idle-connections pool))
           :name "websocket-pool-cleanup"))
    pool))

;;; Pool statistics

(defun connection-pool-size (pool)
  "Get total number of connections in pool"
  (lock:with-lock ((connection-pool-lock pool))
    (length (connection-pool-entries pool))))

(defun connection-pool-active-count (pool)
  "Get number of active connections"
  (lock:with-lock ((connection-pool-lock pool))
    (count-if #'pool-entry-in-use (connection-pool-entries pool))))

;;; Connection acquisition

(defun acquire-connection (pool uri &key (timeout 30) options)
  "Acquire a connection from the pool or create a new one"
  (let ((deadline (when timeout
                    (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))))
    
    (loop
      ;; Check timeout
      (when (and deadline (> (get-internal-real-time) deadline))
        (error 'pool-exhausted-error
               :message "Timeout waiting for available connection"))
      
      (lock:with-lock ((connection-pool-lock pool))
        ;; Try to find an existing idle connection for this URI
        (let ((entry (find-idle-connection pool uri)))
          (when entry
            (setf (pool-entry-in-use entry) t
                  (pool-entry-last-used entry) (get-universal-time))
            (return-from acquire-connection (pool-entry-connection entry))))
        
        ;; Try to create a new connection if under limit
        (when (< (length (connection-pool-entries pool))
                 (connection-pool-max-connections pool))
          (let* ((connection (client:connect uri :options (or options
                                                              (client:make-client-options))))
                 (entry (make-pool-entry
                        :connection connection
                        :uri uri
                        :in-use t
                        :last-used (get-universal-time)
                        :created (get-universal-time))))
            (push entry (connection-pool-entries pool))
            (return-from acquire-connection connection))))
      
      ;; Wait briefly before retrying
      (sleep 0.1))))

(defun find-idle-connection (pool uri)
  "Find an idle connection for the given URI"
  (find-if (lambda (entry)
             (and (not (pool-entry-in-use entry))
                  (string= (pool-entry-uri entry) uri)
                  (conn:connection-open-p (pool-entry-connection entry))))
           (connection-pool-entries pool)))

;;; Connection release

(defun release-connection (pool connection)
  "Release a connection back to the pool"
  (lock:with-lock ((connection-pool-lock pool))
    (let ((entry (find connection (connection-pool-entries pool)
                      :key #'pool-entry-connection)))
      (when entry
        (setf (pool-entry-in-use entry) nil
              (pool-entry-last-used entry) (get-universal-time))))))

;;; Cleanup

(defun cleanup-idle-connections (pool)
  "Background thread to clean up idle connections"
  (loop while (connection-pool-running pool)
        do (sleep 10) ; Check every 10 seconds
           (lock:with-lock ((connection-pool-lock pool))
             (let ((now (get-universal-time))
                   (max-idle (connection-pool-max-idle-time pool)))
               ;; Remove idle connections that have exceeded max idle time
               (setf (connection-pool-entries pool)
                     (remove-if (lambda (entry)
                                  (and (not (pool-entry-in-use entry))
                                       (> (- now (pool-entry-last-used entry))
                                          max-idle)))
                               (connection-pool-entries pool)
                               :key (lambda (entry)
                                     ;; Close connection before removing
                                     (when (and (not (pool-entry-in-use entry))
                                              (> (- now (pool-entry-last-used entry))
                                                 max-idle))
                                       (ignore-errors
                                         (conn:close-connection
                                          (pool-entry-connection entry))))
                                     entry)))))))

(defun close-pool (pool)
  "Close all connections and shutdown pool"
  (setf (connection-pool-running pool) nil)
  
  ;; Wait for cleanup thread to finish
  (when (connection-pool-cleanup-thread pool)
    (thread:join-thread (connection-pool-cleanup-thread pool)))
  
  ;; Close all connections
  (lock:with-lock ((connection-pool-lock pool))
    (dolist (entry (connection-pool-entries pool))
      (ignore-errors
        (conn:close-connection (pool-entry-connection entry))))
    (setf (connection-pool-entries pool) '())))