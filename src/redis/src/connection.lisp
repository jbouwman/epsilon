;;;; Redis Connection Management
;;;;
;;;; Provides connection handling, authentication, and connection pooling
;;;; for Redis clients.

(defpackage :epsilon.redis.connection
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:str #:epsilon.string)
   (#:protocol #:epsilon.redis.protocol))
  (:export
   #:redis-connection
   #:connect
   #:disconnect
   #:connection-alive-p
   #:with-redis
   #:execute-command
   #:send-command
   #:receive-response
   #:connection-stream
   #:connection-in-transaction-p
   #:connection-in-pipeline-p  
   #:connection-pipeline-commands
   #:connection-pubsub-mode-p
   #:parse-connection-string
   #:with-transaction
   #:with-pipeline
   #:pipeline-execute
   #:redis-error
   #:redis-connection-error
   #:redis-command-error))

(in-package :epsilon.redis.connection)

;;; Conditions

(define-condition redis-error (error)
  ()
  (:documentation "Base condition for all Redis errors"))

(define-condition redis-connection-error (redis-error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "Redis connection error: ~A" 
                     (error-message condition)))))

(define-condition redis-command-error (redis-error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "Redis command error: ~A" 
                     (error-message condition)))))

;;; Connection structure

(defclass redis-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (stream :initarg :stream :accessor connection-stream)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (password :initarg :password :accessor connection-password :initform nil)
   (database :initarg :database :accessor connection-database :initform 0)
   (timeout :initarg :timeout :accessor connection-timeout :initform 5)
   (connected-p :initform nil :accessor connection-connected-p)
   (in-transaction-p :initform nil :accessor connection-in-transaction-p)
   (in-pipeline-p :initform nil :accessor connection-in-pipeline-p)
   (pipeline-commands :initform nil :accessor connection-pipeline-commands)
   (pubsub-mode-p :initform nil :accessor connection-pubsub-mode-p)
   (subscriptions :initform (make-hash-table :test 'equal) 
                  :accessor connection-subscriptions)))

(defmethod print-object ((conn redis-connection) stream)
  (print-unreadable-object (conn stream :type t :identity t)
    (format stream "~A:~D~@[ db:~D~]~:[~; CONNECTED~]"
            (connection-host conn)
            (connection-port conn)
            (when (> (connection-database conn) 0)
              (connection-database conn))
            (connection-connected-p conn))))

;;; Connection management

(defun parse-connection-string (connection-string)
  "Parse Redis connection string: redis://[:password@]host[:port][/database]"
  (let* ((url (str:trim connection-string))
         (redis-prefix "redis://")
         (url-without-prefix (if (str:starts-with-p url redis-prefix)
                                 (subseq url (length redis-prefix))
                                 url))
         ;; Check for password
         (at-pos (position #\@ url-without-prefix))
         (password (when at-pos
                     (subseq url-without-prefix 0 at-pos)))
         (host-part (if at-pos
                        (subseq url-without-prefix (1+ at-pos))
                        url-without-prefix))
         ;; Check for database
         (slash-pos (position #\/ host-part))
         (database (when slash-pos
                     (parse-integer (subseq host-part (1+ slash-pos)) 
                                    :junk-allowed t)))
         (host-port-part (if slash-pos
                             (subseq host-part 0 slash-pos)
                             host-part))
         ;; Check for port
         (colon-pos (position #\: host-port-part :from-end t))
         (host (if colon-pos
                   (subseq host-port-part 0 colon-pos)
                   host-port-part))
         (port (if colon-pos
                   (parse-integer (subseq host-port-part (1+ colon-pos)))
                   6379)))
    (values host port password (or database 0))))

(defun connect (host &key (port 6379) password (database 0) (timeout 5))
  "Connect to Redis server"
  (let* ((address (net:make-socket-address host port))
         (connection (make-instance 'redis-connection
                                    :socket nil ; Will be set after connect
                                    :host host
                                    :port port
                                    :password password
                                    :database database
                                    :timeout timeout)))
    (handler-case
        (progn
          ;; Connect using high-level TCP API
          (let ((stream (net:tcp-connect address)))
            (setf (connection-stream connection) stream)
            (setf (connection-socket connection) stream) ; Store stream as socket
            (setf (connection-connected-p connection) t)
            
            ;; Authenticate if password provided
            (when password
              (send-command connection "AUTH" password)
              (let ((response (receive-response connection)))
                (unless (string= response "OK")
                  (error 'redis-connection-error 
                         :message "Authentication failed"))))
            
            ;; Select database if not default
            (when (> database 0)
              (send-command connection "SELECT" (princ-to-string database))
              (let ((response (receive-response connection)))
                (unless (string= response "OK")
                  (error 'redis-connection-error 
                         :message (format nil "Failed to select database ~D" 
                                          database)))))
            
            connection))
      (error (e)
        (when (connection-stream connection)
          (ignore-errors (net:tcp-shutdown (connection-stream connection))))
        (error 'redis-connection-error 
               :message (format nil "Failed to connect to ~A:~D: ~A" 
                                host port e))))))

(defun disconnect (connection)
  "Disconnect from Redis server"
  (when (connection-connected-p connection)
    (ignore-errors
      ;; Send QUIT command
      (send-command connection "QUIT")
      (force-output (connection-stream connection)))
    
    ;; Close TCP stream
    (ignore-errors
      (net:tcp-shutdown (connection-stream connection)))
    
    (setf (connection-connected-p connection) nil)))

(defun connection-alive-p (connection)
  "Check if connection is still alive"
  (and (connection-connected-p connection)
       (handler-case
           (progn
             (send-command connection "PING")
             (string= (receive-response connection) "PONG"))
         (error () nil))))

(defun reconnect (connection)
  "Reconnect to Redis server"
  (disconnect connection)
  (let ((new-conn (connect (connection-host connection)
                           :port (connection-port connection)
                           :password (connection-password connection)
                           :database (connection-database connection)
                           :timeout (connection-timeout connection))))
    ;; Copy new connection details
    (setf (connection-socket connection) (connection-socket new-conn))
    (setf (connection-stream connection) (connection-stream new-conn))
    (setf (connection-connected-p connection) t)
    connection))

;;; Command execution

(defun send-command (connection command &rest args)
  "Send a command to Redis"
  (let ((stream (connection-stream connection)))
    (write-string (apply #'protocol:encode-command command args) stream)
    (force-output stream)))

(defun receive-response (connection)
  "Receive a response from Redis"
  (protocol:decode-response (connection-stream connection)))

(defun execute-command (connection command &rest args)
  "Execute a Redis command and return the response"
  (cond
    ;; Pipeline mode - queue command
    ((connection-in-pipeline-p connection)
     (push (cons command args) (connection-pipeline-commands connection))
     :queued)
    
    ;; Transaction mode - send command, expect QUEUED
    ((connection-in-transaction-p connection)
     (apply #'send-command connection command args)
     (let ((response (receive-response connection)))
       (unless (equal response "QUEUED")
         (error 'redis-command-error 
                :message (format nil "Expected QUEUED, got ~S" response)))
       :queued))
    
    ;; Normal mode - send and receive
    (t
     (apply #'send-command connection command args)
     (receive-response connection))))

;;; Connection macros

(defmacro with-redis ((var connection-spec) &body body)
  "Execute body with a Redis connection"
  (let ((conn-sym (gensym "CONN")))
    `(let ((,conn-sym ,(if (stringp connection-spec)
                           `(multiple-value-bind (host port password database)
                                (parse-connection-string ,connection-spec)
                              (connect host :port port 
                                           :password password 
                                           :database database))
                           connection-spec)))
       (let ((,var ,conn-sym))
         (unwind-protect
              (progn ,@body)
           (when (and ,conn-sym 
                      (not (eq ,conn-sym ,connection-spec)))
             (disconnect ,conn-sym)))))))

;;; Pipeline support

(defmacro with-pipeline ((connection) &body body)
  "Execute commands in pipeline mode"
  `(let ((old-pipeline-p (connection-in-pipeline-p ,connection))
         (old-commands (connection-pipeline-commands ,connection)))
     (unwind-protect
          (progn
            (setf (connection-in-pipeline-p ,connection) t)
            (setf (connection-pipeline-commands ,connection) nil)
            ,@body
            (pipeline-execute ,connection))
       (setf (connection-in-pipeline-p ,connection) old-pipeline-p)
       (setf (connection-pipeline-commands ,connection) old-commands))))

(defun pipeline-execute (connection)
  "Execute all queued pipeline commands"
  (let ((commands (reverse (connection-pipeline-commands connection))))
    ;; Send all commands
    (dolist (cmd-args commands)
      (apply #'send-command connection (car cmd-args) (cdr cmd-args)))
    (force-output (connection-stream connection))
    
    ;; Receive all responses
    (loop for cmd-args in commands
          collect (receive-response connection))))

;;; Transaction support

(defmacro with-transaction ((connection) &body body)
  "Execute commands in a transaction"
  `(let ((old-transaction-p (connection-in-transaction-p ,connection)))
     (unwind-protect
          (progn
            (multi ,connection)
            (setf (connection-in-transaction-p ,connection) t)
            ,@body
            (exec ,connection))
       (setf (connection-in-transaction-p ,connection) old-transaction-p))))

;;; Pub/Sub support

(defmacro with-pubsub ((connection) &body body)
  "Execute commands in pub/sub mode"
  `(let ((old-pubsub-p (connection-pubsub-mode-p ,connection)))
     (unwind-protect
          (progn
            (setf (connection-pubsub-mode-p ,connection) t)
            ,@body)
       (setf (connection-pubsub-mode-p ,connection) old-pubsub-p))))