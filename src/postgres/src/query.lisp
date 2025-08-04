;;;; PostgreSQL Query Execution
;;;;
;;;; Implements query execution, prepared statements, and result processing

(defpackage :epsilon.postgres.query
  (:use :cl)
  (:local-nicknames
   (#:conn #:epsilon.postgres.connection)
   (#:protocol #:epsilon.postgres.protocol)
   (#:types #:epsilon.postgres.types)
   (#:str #:epsilon.string)
   (#:map #:epsilon.map))
  (:export
   #:query-result
   #:query
   #:execute
   #:query-one
   #:query-list
   #:query-alist
   #:query-plist
   #:query-value
   #:prepare
   #:execute-prepared
   #:deallocate
   #:with-prepared-statement
   #:escape-string
   #:escape-identifier
   #:format-query))

(in-package :epsilon.postgres.query)

;;; Query result structures

(defstruct query-result
  "Query execution result"
  (rows '() :type list)
  (columns '() :type list)
  (affected-rows 0 :type integer)
  (oid nil)
  (command-tag "" :type string))

(defstruct column-info
  "Column metadata"
  (name "" :type string)
  (table-oid 0 :type integer)
  (column-num 0 :type integer)
  (type-oid 0 :type integer)
  (type-size 0 :type integer)
  (type-modifier 0 :type integer)
  (format-code 0 :type integer))

;;; Query execution

(defun execute-simple-query (connection query)
  "Execute a simple query (not prepared)"
  (let ((stream (connection-stream connection))
        (msg (make-query-message query))
        (result (make-query-result))
        (columns '()))
    
    ;; Send query
    (write-sequence (str:string-to-octets msg) stream)
    (force-output stream)
    
    ;; Process response messages
    (loop
      (multiple-value-bind (msg-type data)
          (read-message stream)
        (case msg-type
          (#\T ; +row-description+
           (setf columns (parse-row-description data))
           (setf (query-result-columns result)
                 (mapcar (lambda (col)
                           (make-column-info
                            :name (getf col :name)
                            :table-oid (getf col :table-oid)
                            :column-num (getf col :column-num)
                            :type-oid (getf col :type-oid)
                            :type-size (getf col :type-size)
                            :type-modifier (getf col :type-modifier)
                            :format-code (getf col :format-code)))
                         columns)))
          
          (#\D ; +data-row+
           (let ((row-values (parse-data-row data (length columns))))
             ;; Decode values according to column types
             (let ((decoded-row
                    (loop for value in row-values
                          for col in columns
                          collect (if value
                                      (decode-column-value value 
                                                           (getf col :type-oid)
                                                           (getf col :format-code))
                                      nil))))
               (push decoded-row (query-result-rows result)))))
          
          (#\C ; +command-complete+
           (let ((tag (parse-command-complete data)))
             (setf (query-result-command-tag result) tag)
             ;; Parse affected rows from tag
             (when tag
               (let ((parts (str:split #\Space tag)))
                 (when (> (length parts) 1)
                   (let ((last-part (car (last parts))))
                     (when (every #'digit-char-p last-part)
                       (setf (query-result-affected-rows result)
                             (parse-integer last-part)))))))))
          
          (#\I ; +empty-query-response+
           ;; Empty query, nothing to do
           nil)
          
          (#\Z ; +ready-for-query+
           (setf (connection-transaction-status connection)
                 (parse-ready-for-query data))
           (return))  ; Query complete
          
          (#\E ; +error-response+
           (let ((error-fields (parse-error-response data)))
             (error 'postgres-query-error
                    :message (or (map:get error-fields "M") "Query error")
                    :detail (map:get error-fields "D")
                    :hint (map:get error-fields "H")
                    :position (map:get error-fields "P")
                    :code (map:get error-fields "C"))))
          
          (#\N ; +notice-response+
           (let ((notice-fields (parse-error-response data)))
             (warn 'postgres-warning
                   :message (or (map:get notice-fields "M") "PostgreSQL notice"))))
          
          (otherwise
           (error 'postgres-protocol-error
                  :message (format nil "Unexpected message during query: ~C" 
                                   msg-type))))))
    
    ;; Reverse rows (they were collected in reverse order)
    (setf (query-result-rows result) (nreverse (query-result-rows result)))
    result))

(defun query (connection sql &rest parameters)
  "Execute a query with optional parameters"
  (if parameters
      ;; Use prepared statement for parameterized queries
      (execute-prepared-query connection sql parameters)
      ;; Use simple query protocol
      (execute-simple-query connection sql)))

(defun execute (connection sql &rest parameters)
  "Execute a command (INSERT, UPDATE, DELETE) and return affected rows"
  (let ((result (apply #'query connection sql parameters)))
    (query-result-affected-rows result)))

(defun query-one (connection sql &rest parameters)
  "Execute query and return first row, or NIL if no rows"
  (let ((result (apply #'query connection sql parameters)))
    (first (query-result-rows result))))

(defun query-list (connection sql &rest parameters)
  "Execute query and return list of rows"
  (let ((result (apply #'query connection sql parameters)))
    (query-result-rows result)))

(defun query-alist (connection sql &rest parameters)
  "Execute query and return rows as association lists"
  (let ((result (apply #'query connection sql parameters)))
    (mapcar (lambda (row)
              (loop for value in row
                    for col in (query-result-columns result)
                    collect (cons (column-info-name col) value)))
            (query-result-rows result))))

(defun query-plist (connection sql &rest parameters)
  "Execute query and return rows as property lists"
  (let ((result (apply #'query connection sql parameters)))
    (mapcar (lambda (row)
              (loop for value in row
                    for col in (query-result-columns result)
                    append (list (intern (string-upcase (column-info-name col))
                                          :keyword)
                                 value)))
            (query-result-rows result))))

(defun query-value (connection sql &rest parameters)
  "Execute query and return first column of first row"
  (let ((row (apply #'query-one connection sql parameters)))
    (when row
      (first row))))

;;; Prepared statements

(defun execute-prepared-query (connection sql parameters)
  "Execute a parameterized query using prepared statements"
  (let* ((statement-name (format nil "stmt_~A" (sxhash sql)))
         (portal-name "")
         (stream (connection-stream connection)))
    
    ;; Check if statement is already prepared
    (unless (gethash sql (connection-prepared-statements connection))
      ;; Parse (prepare) the statement
      (let ((parse-msg (make-parse-message statement-name sql '())))
        (write-sequence (str:string-to-octets parse-msg) stream))
      
      ;; Send sync to complete preparation
      (let ((sync-msg (make-sync-message)))
        (write-sequence (str:string-to-octets sync-msg) stream))
      (force-output stream)
      
      ;; Process parse response
      (loop
        (multiple-value-bind (msg-type data)
            (read-message stream)
          (case msg-type
            (#\1 nil)  ; +parse-complete+ - Success
            (#\Z ; +ready-for-query+
             (setf (connection-transaction-status connection)
                   (parse-ready-for-query data))
             (return))
            (#\E ; +error-response+
             (let ((error-fields (parse-error-response data)))
               (error 'postgres-query-error
                      :message (or (map:get error-fields "M") "Parse error")
                      :detail (map:get error-fields "D")
                      :hint (map:get error-fields "H")
                      :position (map:get error-fields "P")
                      :code (map:get error-fields "C"))))
            (otherwise
             (error 'postgres-protocol-error
                    :message (format nil "Unexpected message during parse: ~C" 
                                     msg-type))))))
      
      ;; Mark statement as prepared
      (setf (gethash sql (connection-prepared-statements connection)) t))
    
    ;; Bind parameters to the prepared statement
    (let ((bind-msg (make-bind-message portal-name statement-name 
                                       parameters '())))
      (write-sequence (str:string-to-octets bind-msg) stream))
    
    ;; Execute the portal
    (let ((execute-msg (make-execute-message portal-name 0)))  ; 0 = all rows
      (write-sequence (str:string-to-octets execute-msg) stream))
    
    ;; Send sync
    (let ((sync-msg (make-sync-message)))
      (write-sequence (str:string-to-octets sync-msg) stream))
    (force-output stream)
    
    ;; Process execution response
    (let ((result (make-query-result))
          (columns '()))
      (loop
        (multiple-value-bind (msg-type data)
            (read-message stream)
          (case msg-type
            (#\2 nil)  ; +bind-complete+ - Bind successful
            
            (#\T ; +row-description+
             (setf columns (parse-row-description data))
             (setf (query-result-columns result)
                   (mapcar (lambda (col)
                             (make-column-info
                              :name (getf col :name)
                              :table-oid (getf col :table-oid)
                              :column-num (getf col :column-num)
                              :type-oid (getf col :type-oid)
                              :type-size (getf col :type-size)
                              :type-modifier (getf col :type-modifier)
                              :format-code (getf col :format-code)))
                           columns)))
            
            (#\D ; +data-row+
             (let ((row-values (parse-data-row data (length columns))))
               (let ((decoded-row
                      (loop for value in row-values
                            for col in columns
                            collect (if value
                                        (decode-column-value value 
                                                             (getf col :type-oid)
                                                             (getf col :format-code))
                                        nil))))
                 (push decoded-row (query-result-rows result)))))
            
            (#\C ; +command-complete+
             (let ((tag (parse-command-complete data)))
               (setf (query-result-command-tag result) tag)
               (when tag
                 (let ((parts (str:split #\Space tag)))
                   (when (> (length parts) 1)
                     (let ((last-part (car (last parts))))
                       (when (every #'digit-char-p last-part)
                         (setf (query-result-affected-rows result)
                               (parse-integer last-part)))))))))
            
            (#\Z ; +ready-for-query+
             (setf (connection-transaction-status connection)
                   (parse-ready-for-query data))
             (return))
            
            (#\E ; +error-response+
             (let ((error-fields (parse-error-response data)))
               (error 'postgres-query-error
                      :message (or (map:get error-fields "M") "Execution error")
                      :detail (map:get error-fields "D")
                      :hint (map:get error-fields "H")
                      :position (map:get error-fields "P")
                      :code (map:get error-fields "C"))))
            
            (#\N ; +notice-response+
             (let ((notice-fields (parse-error-response data)))
               (warn 'postgres-warning
                     :message (or (map:get notice-fields "M") "PostgreSQL notice"))))
            
            (otherwise
             (error 'postgres-protocol-error
                    :message (format nil "Unexpected message during execution: ~C" 
                                     msg-type))))))
      
      ;; Reverse rows and return result
      (setf (query-result-rows result) (nreverse (query-result-rows result)))
      result)))

(defun prepare (connection name sql)
  "Explicitly prepare a statement"
  (let ((stream (connection-stream connection))
        (parse-msg (make-parse-message name sql '())))
    
    ;; Send parse message
    (write-sequence (str:string-to-octets parse-msg) stream)
    
    ;; Send sync
    (let ((sync-msg (make-sync-message)))
      (write-sequence (str:string-to-octets sync-msg) stream))
    (force-output stream)
    
    ;; Process response
    (loop
      (multiple-value-bind (msg-type data)
          (read-message stream)
        (case msg-type
          (#\1 nil)  ; +parse-complete+ - Success
          (#\Z ; +ready-for-query+
           (setf (connection-transaction-status connection)
                 (parse-ready-for-query data))
           (return))
          (#\E ; +error-response+
           (let ((error-fields (parse-error-response data)))
             (error 'postgres-query-error
                    :message (or (map:get error-fields "M") "Parse error")
                    :detail (map:get error-fields "D")
                    :hint (map:get error-fields "H")
                    :position (map:get error-fields "P")
                    :code (map:get error-fields "C"))))
          (otherwise
           (error 'postgres-protocol-error
                  :message (format nil "Unexpected message during parse: ~C" 
                                   msg-type))))))
    
    ;; Mark statement as prepared
    (setf (gethash name (connection-prepared-statements connection)) t)
    name))

(defun execute-prepared (connection statement-name &rest parameters)
  "Execute a prepared statement"
  (let* ((portal-name "")
         (stream (connection-stream connection)))
    
    ;; Bind parameters
    (let ((bind-msg (make-bind-message portal-name statement-name 
                                       parameters '())))
      (write-sequence (str:string-to-octets bind-msg) stream))
    
    ;; Execute
    (let ((execute-msg (make-execute-message portal-name 0)))
      (write-sequence (str:string-to-octets execute-msg) stream))
    
    ;; Send sync
    (let ((sync-msg (make-sync-message)))
      (write-sequence (str:string-to-octets sync-msg) stream))
    (force-output stream)
    
    ;; Process results (similar to execute-prepared-query)
    (let ((result (make-query-result))
          (columns '()))
      (loop
        (multiple-value-bind (msg-type data)
            (read-message stream)
          (case msg-type
            (#\2 nil) ; +bind-complete+
            (#\T ; +row-description+
             (setf columns (parse-row-description data))
             (setf (query-result-columns result)
                   (mapcar (lambda (col)
                             (make-column-info
                              :name (getf col :name)
                              :type-oid (getf col :type-oid)))
                           columns)))
            (#\D ; +data-row+
             (let ((row-values (parse-data-row data (length columns))))
               (let ((decoded-row
                      (loop for value in row-values
                            for col in columns
                            collect (if value
                                        (decode-column-value value 
                                                             (getf col :type-oid)
                                                             (getf col :format-code))
                                        nil))))
                 (push decoded-row (query-result-rows result)))))
            (#\C ; +command-complete+
             (let ((tag (parse-command-complete data)))
               (setf (query-result-command-tag result) tag)))
            (#\Z ; +ready-for-query+
             (setf (connection-transaction-status connection)
                   (parse-ready-for-query data))
             (return))
            (#\E ; +error-response+
             (let ((error-fields (parse-error-response data)))
               (error 'postgres-query-error
                      :message (or (map:get error-fields "M") "Execution error"))))
            (otherwise
             (error 'postgres-protocol-error
                    :message (format nil "Unexpected message: ~C" msg-type))))))
      
      (setf (query-result-rows result) (nreverse (query-result-rows result)))
      result)))

(defun deallocate (connection statement-name)
  "Deallocate a prepared statement"
  (execute-simple-query connection 
                        (format nil "DEALLOCATE ~A" statement-name))
  (remhash statement-name (connection-prepared-statements connection)))

(defmacro with-prepared-statement ((stmt-var connection name sql) &body body)
  "Execute body with a prepared statement, cleaning up afterward"
  `(let ((,stmt-var (prepare ,connection ,name ,sql)))
     (unwind-protect
          (progn ,@body)
       (deallocate ,connection ,stmt-var))))

;;; Utility functions

(defun escape-string (string)
  "Escape a string for safe inclusion in SQL"
  (str:replace-all "'" "''" string))

(defun escape-identifier (identifier)
  "Escape an identifier (table name, column name, etc.)"
  (format nil "\"~A\"" (str:replace-all "\"" "\"\"" identifier)))

(defun format-query (query-string &rest args)
  "Format a query string with escaped arguments"
  (apply #'format nil query-string
         (mapcar (lambda (arg)
                   (typecase arg
                     (string (format nil "'~A'" (escape-string arg)))
                     (symbol (escape-identifier (string-downcase (symbol-name arg))))
                     (otherwise (princ-to-string arg))))
                 args)))