;;;; Common SQL Database Interface
;;;;
;;;; This module provides a unified interface for SQL database access
;;;; with pluggable backend implementations.

(defpackage epsilon.sql
  (:use cl epsilon.syntax)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (log epsilon.log))
  (:export
   ;; Configuration
   #:*default-backend*
   
   ;; Connection management
   #:connect
   #:disconnect
   #:with-connection
   #:connection
   #:connection-p
   #:connection-backend
   #:connection-handle
   
   ;; Transaction management
   #:begin-transaction
   #:commit
   #:rollback
   #:with-transaction
   
   ;; Query execution
   #:execute
   #:query
   #:query-one
   #:prepare
   #:bind
   #:fetch
   #:fetch-all
   
   ;; Prepared statements
   #:statement
   #:statement-p
   #:statement-handle
   #:statement-connection
   #:finalize-statement
   #:with-statement
   
   ;; Results
   #:result-set
   #:row-count
   #:column-count
   #:column-names
   #:column-types
   
   ;; Data types
   #:sql-null
   #:sql-null-p
   #:+sql-null+
   
   ;; Errors
   #:sql-error
   #:sql-constraint-error
   #:sql-syntax-error
   #:sql-connection-error
   
   ;; Backend registration and protocol
   #:backend
   #:backend-name
   #:backend-version
   #:backend-connect
   #:backend-disconnect
   #:backend-execute
   #:backend-query
   #:backend-prepare
   #:backend-bind
   #:backend-fetch
   #:backend-finalize
   #:backend-begin-transaction
   #:backend-commit
   #:backend-rollback
   #:register-backend
   #:list-backends
   #:find-backend
   
   ;; Utilities
   #:escape-identifier
   #:escape-string
   #:parameterize-query))

(in-package epsilon.sql)

;;; Backend Registry

(defvar *backends* map:+empty+
  "Registry of available SQL backends")

(defvar *default-backend* :sqlite
  "Default backend to use when none is specified")

;;; Conditions

(define-condition sql-error (error)
  ((message :initarg :message :reader sql-error-message)
   (code :initarg :code :reader sql-error-code :initform nil)
   (backend :initarg :backend :reader sql-error-backend))
  (:report (lambda (condition stream)
             (format stream "SQL Error (~A): ~A"
                     (sql-error-backend condition)
                     (sql-error-message condition)))))

(define-condition sql-constraint-error (sql-error) ())
(define-condition sql-syntax-error (sql-error) ())
(define-condition sql-connection-error (sql-error) ())

;;; Data Types

(defstruct sql-null
  "Represents SQL NULL value")

(defparameter +sql-null+ (make-sql-null)
  "Singleton SQL NULL value")

(defun sql-null-p (value)
  "Check if value is SQL NULL"
  (eq value +sql-null+))

;;; Backend Protocol

(defclass backend ()
  ((name :initarg :name :reader backend-name)
   (version :initarg :version :reader backend-version :initform "unknown")))

(defgeneric backend-connect (backend &key &allow-other-keys)
  (:documentation "Create a new connection using the backend"))

(defgeneric backend-disconnect (backend connection)
  (:documentation "Close a connection"))

(defgeneric backend-execute (backend connection sql &optional params)
  (:documentation "Execute a SQL statement"))

(defgeneric backend-query (backend connection sql &optional params)
  (:documentation "Execute a query and return results"))

(defgeneric backend-prepare (backend connection sql)
  (:documentation "Prepare a SQL statement"))

(defgeneric backend-bind (backend statement params)
  (:documentation "Bind parameters to a prepared statement"))

(defgeneric backend-fetch (backend statement)
  (:documentation "Fetch next row from statement results"))

(defgeneric backend-finalize (backend statement)
  (:documentation "Finalize a prepared statement"))

(defgeneric backend-begin-transaction (backend connection)
  (:documentation "Begin a transaction"))

(defgeneric backend-commit (backend connection)
  (:documentation "Commit current transaction"))

(defgeneric backend-rollback (backend connection)
  (:documentation "Rollback current transaction"))

;;; Connection Class

(defclass connection ()
  ((backend :initarg :backend :reader connection-backend)
   (handle :initarg :handle :reader connection-handle)
   (in-transaction :initform nil :accessor connection-in-transaction-p)))

(defun connection-p (obj)
  "Check if object is a connection"
  (typep obj 'connection))

;;; Statement Class

(defclass statement ()
  ((connection :initarg :connection :reader statement-connection)
   (handle :initarg :handle :reader statement-handle)
   (sql :initarg :sql :reader statement-sql)))

(defun statement-p (obj)
  "Check if object is a statement"
  (typep obj 'statement))

;;; Backend Registration

(defun register-backend (name backend-class &rest initargs)
  "Register a new backend"
  (let ((backend (apply #'make-instance backend-class
                        :name name
                        initargs)))
    (setf *backends* (map:assoc *backends* name backend))
    backend))

(defun find-backend (name)
  "Find a backend by name"
  (or (map:get *backends* name)
      (error 'sql-error
             :backend name
             :message (format nil "Backend ~A not found" name))))

(defun list-backends ()
  "List all registered backends"
  (map:keys *backends*))

;;; Connection Management

(defun connect (&rest args &key (backend *default-backend*) &allow-other-keys)
  "Connect to a database"
  (let ((backend-obj (find-backend backend))
        (clean-args (loop for (k v) on args by #'cddr
                           unless (eq k :backend)
                           collect k and collect v)))
    (let ((handle (apply #'backend-connect backend-obj clean-args)))
      (make-instance 'connection
                     :backend backend-obj
                     :handle handle))))

(defun disconnect (connection)
  "Disconnect from database"
  (backend-disconnect (connection-backend connection)
                      connection))

(defmacro with-connection ((var &rest connect-args) &body body)
  "Execute body with database connection"
  `(let ((,var (connect ,@connect-args)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,var))))

;;; Transaction Management

(defun begin-transaction (connection)
  "Begin a transaction"
  (when (connection-in-transaction-p connection)
    (error 'sql-error
           :backend (backend-name (connection-backend connection))
           :message "Already in transaction"))
  (backend-begin-transaction (connection-backend connection) connection)
  (setf (connection-in-transaction-p connection) t))

(defun commit (connection)
  "Commit current transaction"
  (unless (connection-in-transaction-p connection)
    (error 'sql-error
           :backend (backend-name (connection-backend connection))
           :message "Not in transaction"))
  (backend-commit (connection-backend connection) connection)
  (setf (connection-in-transaction-p connection) nil))

(defun rollback (connection)
  "Rollback current transaction"
  (unless (connection-in-transaction-p connection)
    (error 'sql-error
           :backend (backend-name (connection-backend connection))
           :message "Not in transaction"))
  (backend-rollback (connection-backend connection) connection)
  (setf (connection-in-transaction-p connection) nil))

(defmacro with-transaction ((connection) &body body)
  "Execute body within a transaction"
  (let ((conn (gensym "CONN"))
        (success (gensym "SUCCESS")))
    `(let ((,conn ,connection)
           (,success nil))
       (begin-transaction ,conn)
       (unwind-protect
            (prog1 (progn ,@body)
              (setf ,success t))
         (if ,success
             (commit ,conn)
             (rollback ,conn))))))

;;; Query Execution

(defun execute (connection sql &optional params)
  "Execute a SQL statement"
  (backend-execute (connection-backend connection)
                   connection sql params))

(defun query (connection sql &optional params)
  "Execute a query and return all results"
  (backend-query (connection-backend connection)
                 connection sql params))

(defun query-one (connection sql &optional params)
  "Execute a query and return first result"
  (let ((results (query connection sql params)))
    (when results
      (first results))))

;;; Prepared Statements

(defun prepare (connection sql)
  "Prepare a SQL statement"
  (let ((handle (backend-prepare (connection-backend connection)
                                  connection sql)))
    (make-instance 'statement
                   :connection connection
                   :handle handle
                   :sql sql)))

(defun bind (statement &rest params)
  "Bind parameters to a prepared statement"
  (backend-bind (connection-backend (statement-connection statement))
                statement params))

(defun fetch (statement)
  "Fetch next row from statement results"
  (backend-fetch (connection-backend (statement-connection statement))
                 statement))

(defun fetch-all (statement)
  "Fetch all remaining rows from statement"
  (loop for row = (fetch statement)
        while row
        collect row))

(defun finalize-statement (statement)
  "Finalize a prepared statement"
  (backend-finalize (connection-backend (statement-connection statement))
                    statement))

(defmacro with-statement ((var connection sql) &body body)
  "Execute body with prepared statement"
  `(let ((,var (prepare ,connection ,sql)))
     (unwind-protect
          (progn ,@body)
       (finalize-statement ,var))))

;;; Utilities

(defun escape-identifier (identifier)
  "Escape an identifier for use in SQL"
  (format nil "\"~A\"" 
          (str:replace-all identifier "\"" "\"\"")))

(defun escape-string (string)
  "Escape a string for use in SQL"
  (format nil "'~A'"
          (str:replace-all string "'" "''")))

(defun parameterize-query (sql params)
  "Replace ? placeholders with actual parameters"
  (let ((parts (str:split #\? sql))
        (param-list (if (listp params) params (list params))))
    (when (/= (length parts) (1+ (length param-list)))
      (error 'sql-error
             :backend :generic
             :message "Parameter count mismatch"))
    (with-output-to-string (s)
      (loop for part in parts
            for param in (cons nil param-list)
            do (write-string part s)
            when param
            do (write-string (format-sql-value param) s)))))

(defun format-sql-value (value)
  "Format a value for SQL"
  (cond
    ((sql-null-p value) "NULL")
    ((null value) "NULL")
    ((stringp value) (escape-string value))
    ((numberp value) (prin1-to-string value))
    ((eq value t) "1")
    ((eq value nil) "0")
    (t (escape-string (prin1-to-string value)))))