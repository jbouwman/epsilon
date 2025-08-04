;;;; PostgreSQL Connection Management
;;;;
;;;; Handles connection lifecycle, authentication, and parameter negotiation

(defpackage :epsilon.postgres.connection
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:str #:epsilon.string)
   (#:map #:epsilon.map)
   (#:env #:epsilon.sys.env)
   (#:bin #:epsilon.binary))
  (:export
   #:postgres-connection
   #:connect
   #:disconnect
   #:connection-alive-p
   #:with-connection
   #:postgres-error
   #:postgres-connection-error
   #:postgres-query-error
   #:postgres-warning))

(in-package :epsilon.postgres.connection)

;;; Conditions

(define-condition postgres-error (error)
  ()
  (:documentation "Base condition for all PostgreSQL errors"))

(define-condition postgres-connection-error (postgres-error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "PostgreSQL connection error: ~A" 
                     (error-message condition)))))

(define-condition postgres-query-error (postgres-error)
  ((message :initarg :message :reader error-message)
   (detail :initarg :detail :reader error-detail :initform nil)
   (hint :initarg :hint :reader error-hint :initform nil)
   (position :initarg :position :reader error-position :initform nil)
   (code :initarg :code :reader error-code :initform nil))
  (:report (lambda (condition stream)
             (format stream "PostgreSQL query error: ~A~@[ (~A)~]~@[~%Detail: ~A~]~@[~%Hint: ~A~]"
                     (error-message condition)
                     (error-code condition)
                     (error-detail condition)
                     (error-hint condition)))))

(define-condition postgres-warning (warning)
  ((message :initarg :message :reader warning-message))
  (:report (lambda (condition stream)
             (format stream "PostgreSQL warning: ~A" 
                     (warning-message condition)))))

;;; Basic Protocol Support (inline to avoid circular dependencies)

;; Protocol constants (subset needed for connection)
(defparameter +protocol-version+ #x00030000)  ; 3.0
(defparameter +password-message+ #\p)
(defparameter +query+ #\Q)
(defparameter +sync+ #\S)
(defparameter +terminate+ #\X)
(defparameter +authentication+ #\R)
(defparameter +backend-key-data+ #\K)
(defparameter +parameter-status+ #\S)
(defparameter +ready-for-query+ #\Z)
(defparameter +error-response+ #\E)
(defparameter +notice-response+ #\N)
(defparameter +auth-ok+ 0)
(defparameter +auth-cleartext-password+ 3)
(defparameter +auth-md5-password+ 5)

;; Basic protocol functions (minimal implementations)
(defun write-int32 (value stream)
  "Write 32-bit integer in network byte order"
  (write-byte (ldb (byte 8 24) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun read-int32 (stream)
  "Read 32-bit integer in network byte order"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)))

(defun write-cstring (string stream)
  "Write null-terminated string"
  (write-sequence (str:string-to-octets string) stream)
  (write-byte 0 stream))

(defun read-bytes (stream length)
  "Read specified number of bytes"
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

(defun octets-to-uint32 (octets)
  "Convert 4 octets to 32-bit unsigned integer (big-endian)"
  (logior (ash (aref octets 0) 24)
          (ash (aref octets 1) 16)
          (ash (aref octets 2) 8)
          (aref octets 3)))

(defun make-message (type data)
  "Create a protocol message with type byte and length"
  (let ((data-bytes (if (stringp data)
                        (str:string-to-octets data)
                        data)))
    (let ((message (make-array (+ (if type 1 0) 4 (length data-bytes))
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0)))
      (when type
        (vector-push (char-code type) message))
      ;; Write length (4 bytes for length field + data length)
      (let ((length (+ 4 (length data-bytes))))
        (vector-push (ldb (byte 8 24) length) message)
        (vector-push (ldb (byte 8 16) length) message)
        (vector-push (ldb (byte 8 8) length) message)
        (vector-push (ldb (byte 8 0) length) message))
      ;; Write data
      (loop for byte across data-bytes
            do (vector-push byte message))
      message)))

(defun make-startup-message (parameters)
  "Create startup message with parameters"
  (let ((param-data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    ;; Build parameter data
    (map:each (lambda (k v)
                (let ((k-bytes (str:string-to-octets k))
                      (v-bytes (str:string-to-octets v)))
                  (loop for byte across k-bytes do (vector-push-extend byte param-data))
                  (vector-push-extend 0 param-data) ; null terminator
                  (loop for byte across v-bytes do (vector-push-extend byte param-data))
                  (vector-push-extend 0 param-data))) ; null terminator
              parameters)
    (vector-push-extend 0 param-data) ; Final null terminator
    
    ;; Build complete startup message
    (let* ((total-length (+ 8 (length param-data))) ; 4 bytes length + 4 bytes version + params
           (message (make-array total-length :element-type '(unsigned-byte 8) :fill-pointer 0)))
      ;; Write length
      (vector-push (ldb (byte 8 24) total-length) message)
      (vector-push (ldb (byte 8 16) total-length) message)
      (vector-push (ldb (byte 8 8) total-length) message)
      (vector-push (ldb (byte 8 0) total-length) message)
      ;; Write version
      (vector-push (ldb (byte 8 24) +protocol-version+) message)
      (vector-push (ldb (byte 8 16) +protocol-version+) message)
      (vector-push (ldb (byte 8 8) +protocol-version+) message)
      (vector-push (ldb (byte 8 0) +protocol-version+) message)
      ;; Write parameter data
      (loop for byte across param-data do (vector-push byte message))
      message)))

(defun make-terminate-message ()
  "Create terminate message"
  (make-message +terminate+ ""))

(defun make-sync-message ()
  "Create sync message"
  (make-message +sync+ ""))

(defun read-message (stream)
  "Read a backend message"
  (let* ((type (read-char stream))
         (length (read-int32 stream))
         (data-length (- length 4)))  ; Subtract length field itself
    (when (< data-length 0)
      (error 'postgres-protocol-error 
             :message "Invalid message length"))
    (let ((data (when (> data-length 0)
                  (read-bytes stream data-length))))
      (values type data))))

(defun parse-error-response (data)
  "Parse error or notice response"
  (let ((fields (map:make-map))
        (pos 0))
    (loop while (< pos (length data))
          do (let ((field-type (char data pos)))
               (when (char= field-type #\Nul)
                 (return))
               (incf pos)
               (let ((end (position #\Nul data :start pos)))
                 (unless end
                   (error 'postgres-protocol-error
                          :message "Unterminated error field"))
                 (let ((value (subseq data pos end)))
                   (setf fields (map:assoc fields 
                                           (string field-type) 
                                           value))
                   (setf pos (1+ end))))))
    fields))

;;; Connection structure

(defclass postgres-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (stream :initarg :stream :accessor connection-stream)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (database :initarg :database :accessor connection-database)
   (user :initarg :user :accessor connection-user)
   (password :initarg :password :accessor connection-password :initform nil)
   (options :initarg :options :accessor connection-options :initform nil)
   (ssl-mode :initarg :ssl-mode :accessor connection-ssl-mode :initform :prefer)
   (application-name :initarg :application-name :accessor connection-application-name 
                     :initform "epsilon-postgres")
   (connected-p :initform nil :accessor connection-connected-p)
   (transaction-status :initform #\I :accessor connection-transaction-status)
   (backend-pid :initform nil :accessor connection-backend-pid)
   (backend-key :initform nil :accessor connection-backend-key)
   (server-version :initform nil :accessor connection-server-version)
   (server-encoding :initform "UTF8" :accessor connection-server-encoding)
   (client-encoding :initform "UTF8" :accessor connection-client-encoding)
   (parameters :initform (map:make-map) :accessor connection-parameters)
   (prepared-statements :initform (make-hash-table :test 'equal) 
                        :accessor connection-prepared-statements)
   (type-map :initform (make-hash-table) :accessor connection-type-map)))

(defmethod print-object ((conn postgres-connection) stream)
  (print-unreadable-object (conn stream :type t :identity t)
    (format stream "~A@~A:~D/~A~:[~; CONNECTED~]"
            (connection-user conn)
            (connection-host conn)
            (connection-port conn)
            (connection-database conn)
            (connection-connected-p conn))))

;;; Connection management

(defun parse-connection-string (connection-string)
  "Parse PostgreSQL connection string"
  ;; Format: postgresql://[user[:password]@]host[:port][/database][?param=value&...]
  (let* ((url (str:trim connection-string))
         (postgres-prefix "postgresql://")
         (postgres-prefix2 "postgres://")
         (url-without-prefix 
           (cond
             ((str:starts-with-p url postgres-prefix)
              (subseq url (length postgres-prefix)))
             ((str:starts-with-p url postgres-prefix2)
              (subseq url (length postgres-prefix2)))
             (t url)))
         ;; Parse URL components
         (query-pos (position #\? url-without-prefix))
         (params (when query-pos
                   (parse-connection-params 
                    (subseq url-without-prefix (1+ query-pos)))))
         (url-base (if query-pos
                       (subseq url-without-prefix 0 query-pos)
                       url-without-prefix))
         ;; Check for user info
         (at-pos (position #\@ url-base))
         (user-info (when at-pos
                      (subseq url-base 0 at-pos)))
         (host-part (if at-pos
                        (subseq url-base (1+ at-pos))
                        url-base))
         ;; Parse user and password
         (colon-pos (when user-info (position #\: user-info)))
         (user (if user-info
                   (if colon-pos
                       (subseq user-info 0 colon-pos)
                       user-info)
                   (or (env:getenv "USER") "postgres")))
         (password (when (and user-info colon-pos)
                     (subseq user-info (1+ colon-pos))))
         ;; Parse host, port, database
         (slash-pos (position #\/ host-part))
         (database (if slash-pos
                       (subseq host-part (1+ slash-pos))
                       "postgres"))
         (host-port (if slash-pos
                        (subseq host-part 0 slash-pos)
                        host-part))
         (port-colon-pos (position #\: host-port :from-end t))
         (host (if port-colon-pos
                   (subseq host-port 0 port-colon-pos)
                   host-port))
         (port (if port-colon-pos
                   (parse-integer (subseq host-port (1+ port-colon-pos)))
                   5432)))
    (values host port database user password params)))

(defun parse-connection-params (param-string)
  "Parse URL query parameters"
  (let ((params (map:make-map)))
    (dolist (param (str:split #\& param-string))
      (let ((eq-pos (position #\= param)))
        (when eq-pos
          (let ((key (subseq param 0 eq-pos))
                (value (subseq param (1+ eq-pos))))
            (setf params (map:assoc params key value))))))
    params))

(defun connect (host &key (port 5432) (database "postgres") 
                       (user (or (env:getenv "USER") "postgres"))
                       password
                       (options nil)
                       (ssl-mode :prefer)
                       (application-name "epsilon-postgres")
                       (timeout 30))
  "Connect to PostgreSQL server"
  (let* ((address (net:make-socket-address host port))
         (connection (make-instance 'postgres-connection
                                    :socket nil
                                    :host host
                                    :port port
                                    :database database
                                    :user user
                                    :password password
                                    :options options
                                    :ssl-mode ssl-mode
                                    :application-name application-name)))
    (handler-case
        (progn
          ;; Connect to server using high-level TCP API
          (let ((tcp-stream (if timeout
                                (net:tcp-connect-with-timeout address timeout)
                                (net:tcp-connect address))))
            (setf (connection-stream connection) tcp-stream)
            (setf (connection-socket connection) tcp-stream) ; Store stream as socket too
            
            ;; Perform handshake
            (handshake connection)
            
            ;; Mark as connected
            (setf (connection-connected-p connection) t)
            connection))
      (error (e)
        (error 'postgres-connection-error 
               :message (format nil "Failed to connect to ~A:~D: ~A" 
                                host port e))))))

(defun handshake (connection)
  "Perform PostgreSQL connection handshake"
  (let ((stream (connection-stream connection)))
    ;; Send startup message
    (let ((params (map:make-map
                   "user" (connection-user connection)
                   "database" (connection-database connection)
                   "application_name" (connection-application-name connection)
                   "client_encoding" "UTF8")))
      ;; Add any custom options
      (when (connection-options connection)
        (setf params (map:assoc params "options" (connection-options connection))))
      
      (let ((startup-msg (make-startup-message params)))
        (write-sequence startup-msg stream)
        (force-output stream)))
    
    ;; Process authentication and startup
    (loop
     (multiple-value-bind (msg-type data)
         (read-message stream)
       (case msg-type
         (+authentication+
          (let ((auth-type (octets-to-uint32 (subseq data 0 4))))
            (case auth-type
              (+auth-ok+ nil)  ; Continue to next message
              (+auth-cleartext-password+
               (send-password-message connection (connection-password connection)))
              (+auth-md5-password+
               (let ((salt (subseq data 4 8)))
                 (send-md5-password connection salt)))
              (otherwise
               (error 'postgres-connection-error
                      :message (format nil "Unsupported authentication method: ~D" 
                                       auth-type))))))
         
         (+backend-key-data+
          (setf (connection-backend-pid connection)
                (octets-to-uint32 (subseq data 0 4)))
          (setf (connection-backend-key connection)
                (octets-to-uint32 (subseq data 4 8))))
         
         (+parameter-status+
          (let* ((param-end (position 0 data))
                 (param-name (str:octets-to-string (subseq data 0 param-end)))
                 (value-start (1+ param-end))
                 (value-end (position 0 data :start value-start))
                 (param-value (str:octets-to-string 
                               (subseq data value-start value-end))))
            (setf (connection-parameters connection)
                  (map:assoc (connection-parameters connection) 
                             param-name param-value))
            ;; Track important parameters
            (cond
              ((string= param-name "server_version")
               (setf (connection-server-version connection) param-value))
              ((string= param-name "server_encoding")
               (setf (connection-server-encoding connection) param-value))
              ((string= param-name "client_encoding")
               (setf (connection-client-encoding connection) param-value)))))
         
         (+ready-for-query+
          (setf (connection-transaction-status connection) 
                (char (str:octets-to-string data) 0))
          (return))  ; Handshake complete
         
         (+error-response+
          (let ((error-fields (parse-error-response data)))
            (error 'postgres-connection-error
                   :message (or (map:get error-fields "M")
                                "Unknown connection error"))))
         
         (+notice-response+
          (let ((notice-fields (parse-error-response data)))
            (warn 'postgres-warning
                  :message (or (map:get notice-fields "M")
                               "PostgreSQL notice"))))
         
         (otherwise
          (error 'postgres-protocol-error
                 :message (format nil "Unexpected message during handshake: ~C" 
                                  msg-type))))))))

(defun send-password-message (connection password)
  "Send cleartext password"
  (let ((stream (connection-stream connection))
        (msg (let ((pwd (or password "")))
               (let ((pwd-bytes (str:string-to-octets pwd))
                     (data (make-array (+ (length pwd) 1) :element-type '(unsigned-byte 8) :fill-pointer 0)))
                 (loop for byte across pwd-bytes do (vector-push byte data))
                 (vector-push 0 data) ; null terminator
                 (make-message +password-message+ data)))))
    (write-sequence msg stream)
    (force-output stream)))

(defun send-md5-password (connection salt)
  "Send MD5 encrypted password"
  (declare (ignore salt))
  ;; MD5 authentication not yet implemented
  (error 'postgres-connection-error
         :message "MD5 password authentication not yet implemented"))

(defun disconnect (connection)
  "Disconnect from PostgreSQL server"
  (when (connection-connected-p connection)
    (ignore-errors
     ;; Send terminate message
     (let ((stream (connection-stream connection))
           (msg (make-terminate-message)))
       (write-sequence msg stream)
       (force-output stream)))
    
    ;; Close TCP stream (which includes socket)
    (ignore-errors
     (net:tcp-shutdown (connection-stream connection)))
    
    (setf (connection-connected-p connection) nil)))

(defun connection-alive-p (connection)
  "Check if connection is still alive"
  (and (connection-connected-p connection)
       (handler-case
           (progn
             ;; Send sync and wait for ready
             (let ((stream (connection-stream connection))
                   (msg (make-sync-message)))
               (write-sequence msg stream)
               (force-output stream))
             ;; Read response
             (multiple-value-bind (msg-type data)
                 (read-message (connection-stream connection))
               (eq msg-type +ready-for-query+)))
         (error () nil))))

(defun reconnect (connection)
  "Reconnect to PostgreSQL server"
  (disconnect connection)
  (let ((new-conn (connect (connection-host connection)
                           :port (connection-port connection)
                           :database (connection-database connection)
                           :user (connection-user connection)
                           :password (connection-password connection)
                           :options (connection-options connection)
                           :ssl-mode (connection-ssl-mode connection)
                           :application-name (connection-application-name connection))))
    ;; Copy new connection details
    (setf (connection-socket connection) (connection-socket new-conn))
    (setf (connection-stream connection) (connection-stream new-conn))
    (setf (connection-connected-p connection) t)
    (setf (connection-backend-pid connection) (connection-backend-pid new-conn))
    (setf (connection-backend-key connection) (connection-backend-key new-conn))
    (setf (connection-parameters connection) (connection-parameters new-conn))
    (setf (connection-transaction-status connection) (connection-transaction-status new-conn))
    connection))

;;; Connection macros

(defmacro with-connection ((var connection-spec) &body body)
  "Execute body with a PostgreSQL connection"
  (let ((conn-sym (gensym "CONN")))
    `(let ((,conn-sym ,(if (stringp connection-spec)
                           `(multiple-value-bind (host port database user password params)
                                (parse-connection-string ,connection-spec)
                              (connect host :port port 
                                            :database database
                                            :user user
                                            :password password))
                           connection-spec)))
       (let ((,var ,conn-sym))
         (unwind-protect
              (progn ,@body)
           (when (and ,conn-sym 
                      (not (eq ,conn-sym ,connection-spec)))
             (disconnect ,conn-sym)))))))
