;;;; PostgreSQL Transaction Support
;;;;
;;;; Implements transaction management including savepoints and nested transactions

(defpackage :epsilon.postgres.transactions
  (:use :cl)
  (:local-nicknames
   (#:conn #:epsilon.postgres.connection)
   (#:query #:epsilon.postgres.query))
  (:export
   #:begin-transaction
   #:commit
   #:rollback
   #:with-transaction
   #:savepoint
   #:release-savepoint
   #:rollback-to-savepoint
   #:with-savepoint
   #:with-nested-transaction
   #:with-deadlock-retry
   #:advisory-lock
   #:with-advisory-lock))

(in-package :epsilon.postgres.transactions)

;;; Transaction status tracking

(defun in-transaction-p (connection)
  "Check if connection is in a transaction"
  (not (char= (connection-transaction-status connection) #\I)))

(defun transaction-failed-p (connection)
  "Check if current transaction has failed"
  (char= (connection-transaction-status connection) #\E))

;;; Basic transaction commands

(defun begin-transaction (connection &key isolation-level read-only deferrable)
  "Begin a transaction with optional parameters"
  (let ((sql "BEGIN"))
    ;; Add transaction parameters
    (when (or isolation-level read-only deferrable)
      (setf sql (concatenate 'string sql " ISOLATION LEVEL"))
      (when isolation-level
        (setf sql (concatenate 'string sql " " 
                               (string-upcase (string isolation-level)))))
      (when read-only
        (setf sql (concatenate 'string sql " READ ONLY")))
      (when deferrable
        (setf sql (concatenate 'string sql " DEFERRABLE"))))
    
    (execute-simple-query connection sql)
    
    ;; Check transaction status
    (unless (in-transaction-p connection)
      (error 'postgres-query-error
             :message "Failed to begin transaction"))))

(defun commit (connection)
  "Commit the current transaction"
  (unless (in-transaction-p connection)
    (error 'postgres-query-error
           :message "Not in a transaction"))
  
  (when (transaction-failed-p connection)
    (error 'postgres-query-error
           :message "Cannot commit failed transaction"))
  
  (execute-simple-query connection "COMMIT"))

(defun rollback (connection)
  "Rollback the current transaction"
  (unless (in-transaction-p connection)
    (error 'postgres-query-error
           :message "Not in a transaction"))
  
  (execute-simple-query connection "ROLLBACK"))

;;; Savepoints

(defun savepoint (connection name)
  "Create a savepoint within the current transaction"
  (unless (in-transaction-p connection)
    (error 'postgres-query-error
           :message "Savepoints can only be used within transactions"))
  
  (execute-simple-query connection 
                        (format nil "SAVEPOINT ~A" (escape-identifier name))))

(defun release-savepoint (connection name)
  "Release a savepoint"
  (unless (in-transaction-p connection)
    (error 'postgres-query-error
           :message "Not in a transaction"))
  
  (execute-simple-query connection 
                        (format nil "RELEASE SAVEPOINT ~A" (escape-identifier name))))

(defun rollback-to-savepoint (connection name)
  "Rollback to a savepoint"
  (unless (in-transaction-p connection)
    (error 'postgres-query-error
           :message "Not in a transaction"))
  
  (execute-simple-query connection 
                        (format nil "ROLLBACK TO SAVEPOINT ~A" (escape-identifier name))))

;;; Transaction macros

(defmacro with-transaction ((connection &key isolation-level read-only deferrable) 
                            &body body)
  "Execute body within a transaction, with automatic rollback on error"
  (let ((committed-sym (gensym "COMMITTED"))
        (condition-sym (gensym "CONDITION")))
    `(let ((,committed-sym nil))
       (begin-transaction ,connection 
                          :isolation-level ,isolation-level
                          :read-only ,read-only
                          :deferrable ,deferrable)
       (unwind-protect
            (handler-case
                (prog1
                    (progn ,@body)
                  (commit ,connection)
                  (setf ,committed-sym t))
              (postgres-query-error (,condition-sym)
                ;; PostgreSQL query errors during transaction
                (unless ,committed-sym
                  (rollback ,connection))
                (signal ,condition-sym))
              (error (,condition-sym)
                ;; Other errors - rollback and re-signal
                (unless ,committed-sym
                  (ignore-errors (rollback ,connection)))
                (error ,condition-sym)))
         ;; Cleanup: ensure transaction is closed
         (unless ,committed-sym
           (ignore-errors (rollback ,connection)))))))

(defmacro with-savepoint ((connection name) &body body)
  "Execute body within a savepoint, with automatic rollback on error"
  (let ((released-sym (gensym "RELEASED"))
        (condition-sym (gensym "CONDITION")))
    `(let ((,released-sym nil))
       (savepoint ,connection ,name)
       (unwind-protect
            (handler-case
                (prog1
                    (progn ,@body)
                  (release-savepoint ,connection ,name)
                  (setf ,released-sym t))
              (postgres-query-error (,condition-sym)
                ;; PostgreSQL errors - rollback to savepoint
                (unless ,released-sym
                  (rollback-to-savepoint ,connection ,name))
                (signal ,condition-sym))
              (error (,condition-sym)
                ;; Other errors - rollback to savepoint
                (unless ,released-sym
                  (ignore-errors (rollback-to-savepoint ,connection ,name)))
                (error ,condition-sym)))
         ;; Cleanup: rollback to savepoint if not released
         (unless ,released-sym
           (ignore-errors (rollback-to-savepoint ,connection ,name)))))))

;;; Nested transaction support

(defvar *transaction-depth* 0
  "Current transaction nesting depth")

(defvar *savepoint-counter* 0
  "Counter for generating unique savepoint names")

(defmacro with-nested-transaction ((connection) &body body)
  "Execute body within a transaction, using savepoints for nesting"
  (let ((depth-sym (gensym "DEPTH"))
        (savepoint-name-sym (gensym "SAVEPOINT-NAME")))
    `(let* ((,depth-sym *transaction-depth*)
            (,savepoint-name-sym (when (> ,depth-sym 0)
                                   (format nil "sp_~D_~D" 
                                           ,depth-sym 
                                           (incf *savepoint-counter*)))))
       (let ((*transaction-depth* (1+ ,depth-sym)))
         (if (zerop ,depth-sym)
             ;; Top-level transaction
             (with-transaction (,connection)
               ,@body)
             ;; Nested transaction using savepoint
             (with-savepoint (,connection ,savepoint-name-sym)
               ,@body))))))

;;; Transaction isolation levels

(defun set-transaction-isolation (connection level)
  "Set transaction isolation level for current transaction"
  (let ((level-str (case level
                     (:read-uncommitted "READ UNCOMMITTED")
                     (:read-committed "READ COMMITTED")
                     (:repeatable-read "REPEATABLE READ")
                     (:serializable "SERIALIZABLE")
                     (otherwise (string-upcase (string level))))))
    (execute-simple-query connection 
                          (format nil "SET TRANSACTION ISOLATION LEVEL ~A" 
                                  level-str))))

(defun set-session-isolation (connection level)
  "Set default isolation level for the session"
  (let ((level-str (case level
                     (:read-uncommitted "read uncommitted")
                     (:read-committed "read committed")
                     (:repeatable-read "repeatable read")
                     (:serializable "serializable")
                     (otherwise (string-downcase (string level))))))
    (execute-simple-query connection 
                          (format nil "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL ~A" 
                                  level-str))))

;;; Transaction info and utilities

(defun current-transaction-isolation (connection)
  "Get current transaction isolation level"
  (let ((result (query-value connection "SHOW transaction_isolation")))
    (cond
      ((string= result "read uncommitted") :read-uncommitted)
      ((string= result "read committed") :read-committed)
      ((string= result "repeatable read") :repeatable-read)
      ((string= result "serializable") :serializable)
      (t (intern (string-upcase result) :keyword)))))

(defun transaction-timestamp (connection)
  "Get the timestamp of the current transaction"
  (query-value connection "SELECT transaction_timestamp()"))

(defun statement-timestamp (connection)
  "Get the timestamp of the current statement"
  (query-value connection "SELECT statement_timestamp()"))

;;; Deadlock detection and retry

(defun deadlock-error-p (condition)
  "Check if condition represents a deadlock error"
  (and (typep condition 'postgres-query-error)
       (let ((code (error-code condition)))
         (and code (string= code "40P01")))))  ; deadlock_detected

(defmacro with-deadlock-retry ((connection &key (max-retries 3) (delay 0.1)) 
                               &body body)
  "Execute body within transaction, retrying on deadlock"
  (let ((retries-sym (gensym "RETRIES"))
        (condition-sym (gensym "CONDITION")))
    `(loop for ,retries-sym from 0 to ,max-retries
           do (handler-case
                  (return (with-transaction (,connection)
                            ,@body))
                (postgres-query-error (,condition-sym)
                  (if (and (deadlock-error-p ,condition-sym)
                           (< ,retries-sym ,max-retries))
                      (progn
                        (warn "Deadlock detected, retrying (~D/~D)" 
                              (1+ ,retries-sym) ,max-retries)
                        (sleep ,delay))
                      (error ,condition-sym)))))))

;;; Advisory locks

(defun advisory-lock (connection key &key shared nowait session)
  "Acquire an advisory lock"
  (let ((function-name 
         (cond
           ((and shared nowait session) "pg_try_advisory_lock_shared")
           ((and shared session) "pg_advisory_lock_shared")
           ((and nowait session) "pg_try_advisory_lock")
           (session "pg_advisory_lock")
           ((and shared nowait) "pg_try_advisory_xact_lock_shared")
           (shared "pg_advisory_xact_lock_shared")
           (nowait "pg_try_advisory_xact_lock")
           (t "pg_advisory_xact_lock"))))
    (if nowait
        ;; Returns boolean for try variants
        (query-value connection 
                     (format nil "SELECT ~A(~D)" function-name key))
        ;; Void return for blocking variants
        (progn
          (execute connection 
                   (format nil "SELECT ~A(~D)" function-name key))
          t))))

(defun advisory-unlock (connection key &key shared session)
  "Release an advisory lock"
  (let ((function-name 
         (cond
           ((and shared session) "pg_advisory_unlock_shared")
           (session "pg_advisory_unlock")
           (shared "pg_advisory_unlock_shared")
           (t "pg_advisory_unlock"))))
    (query-value connection 
                 (format nil "SELECT ~A(~D)" function-name key))))

(defun advisory-unlock-all (connection &key session)
  "Release all advisory locks held by this session"
  (let ((function-name (if session
                           "pg_advisory_unlock_all"
                           "pg_advisory_unlock_all")))
    (execute connection (format nil "SELECT ~A()" function-name))))

(defmacro with-advisory-lock ((connection key &key shared nowait session timeout) 
                              &body body)
  "Execute body while holding an advisory lock"
  (let ((acquired-sym (gensym "ACQUIRED"))
        (timeout-sym (gensym "TIMEOUT")))
    `(let ((,acquired-sym nil)
           (,timeout-sym ,timeout))
       (unwind-protect
            (progn
              ;; Acquire lock with optional timeout
              (if ,timeout-sym
                  (let ((start-time (get-universal-time)))
                    (loop
                      (when (advisory-lock ,connection ,key 
                                           :shared ,shared 
                                           :nowait t 
                                           :session ,session)
                        (setf ,acquired-sym t)
                        (return))
                      (when (> (- (get-universal-time) start-time) ,timeout-sym)
                        (error 'postgres-query-error
                               :message "Timeout waiting for advisory lock"))
                      (sleep 0.1)))
                  (progn
                    (setf ,acquired-sym 
                          (advisory-lock ,connection ,key 
                                         :shared ,shared 
                                         :nowait ,nowait 
                                         :session ,session))
                    (unless ,acquired-sym
                      (error 'postgres-query-error
                             :message "Could not acquire advisory lock"))))
              
              ;; Execute body
              ,@body)
         
         ;; Release lock
         (when ,acquired-sym
           (ignore-errors 
             (advisory-unlock ,connection ,key 
                              :shared ,shared 
                              :session ,session)))))))

;;; Connection-level transaction utilities

(defun reset-connection-transaction-state (connection)
  "Reset connection to a clean transaction state"
  (when (in-transaction-p connection)
    (rollback connection))
  
  ;; Clear any prepared statements from failed transactions
  (clrhash (connection-prepared-statements connection))
  
  ;; Verify we're in idle state
  (unless (char= (connection-transaction-status connection) #\I)
    (error 'postgres-connection-error
           :message "Failed to reset connection transaction state")))