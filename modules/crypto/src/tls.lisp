;;;; TLS/SSL Implementation
;;;;
;;;; This file provides TLS/SSL functionality using OpenSSL

(defpackage :epsilon.crypto.tls
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (alpn epsilon.crypto.alpn)
   (#:ffi #:epsilon.crypto.ffi)
   (#:net #:epsilon.net))
  (:import-from :epsilon.crypto
		;; Import all needed symbols
		#:+tls-verify-none+
		#:+tls-verify-peer+
		#:+ssl-filetype-pem+
		#:+ssl-verify-peer+
		#:+ssl-verify-none+
		#:+ssl-verify-fail-if-no-peer-cert+
		#:tls-context
		#:tls-context-p
		#:tls-context-server-p
		#:tls-context-cert-file
		#:tls-context-key-file
		#:tls-context-verify-mode
		#:tls-connection
		#:tls-connection-p
		#:tls-connection-socket
		#:tls-connection-connected-p
		#:tls-connection-handshake-complete-p
		#:openssl-context
		#:openssl-context-p
		#:openssl-context-handle
		#:openssl-context-server-p
		#:openssl-context-cert-file
		#:openssl-context-key-file
		#:openssl-context-verify-mode
		#:make-openssl-context
		#:openssl-connection
		#:openssl-connection-p
		#:openssl-connection-ssl
		#:openssl-connection-socket
		#:openssl-connection-context
		#:openssl-connection-connected-p
		#:make-openssl-connection
		#:x509-certificate
		#:x509-certificate-p
		#:x509-certificate-subject
		#:x509-certificate-issuer
		#:make-x509-certificate
		#:crypto-error)
  (:export
   #:create-openssl-context
   #:get-peer-certificate
   #:make-x509-certificate
   #:openssl-connection-p
   #:openssl-connection-ssl
   #:openssl-context-handle
   #:tls-cipher
   #:tls-close
   #:tls-connect
   #:tls-version
   #:verify-peer-certificate
   #:x509-certificate-p
   #:x509-certificate-subject))

(in-package :epsilon.crypto.tls)

;;;; Helper Functions

(defun get-openssl-error-string ()
  "Get the OpenSSL error string from the error queue"
  (let ((err (ffi:%err-get-error)))
    (if (zerop err)
        "No OpenSSL error"
        (let ((str-ptr (ffi:%err-error-string err (sb-sys:int-sap 0))))
          (if (or (null str-ptr) (sb-sys:sap= str-ptr (sb-sys:int-sap 0)))
              (format nil "OpenSSL error code: ~A" err)
              (handler-case
                  (sb-alien:cast str-ptr sb-alien:c-string)
                (error () (format nil "OpenSSL error code: ~A" err))))))))

(defun get-all-openssl-errors ()
  "Get all OpenSSL errors from the error queue"
  (let ((errors '()))
    (loop for err = (ffi:%err-get-error)
          while (not (zerop err))
          do (let ((str-ptr (ffi:%err-error-string err (sb-sys:int-sap 0))))
               (push (if (or (null str-ptr) (sb-sys:sap= str-ptr (sb-sys:int-sap 0)))
                         (format nil "error:~8,'0X" err)
                         (handler-case
                             (sb-alien:cast str-ptr sb-alien:c-string)
                           (error () (format nil "error:~8,'0X" err))))
                     errors)))
    (nreverse errors)))

;;;; TLS Context Management

(defun create-tls-context (&key server-p cert-file key-file 
                                ca-file (verify-mode +tls-verify-peer+)
                                require-client-cert verify-depth)
  "Create a new TLS context with specified configuration.
   Parameters:
   - server-p: Whether this is a server context
   - cert-file: Path to certificate file
   - key-file: Path to private key file
   - ca-file: Path to CA certificates for verification
   - verify-mode: SSL verification mode
   - require-client-cert: For servers, require client certificate
   - verify-depth: Maximum certificate chain verification depth"
  (declare (ignore ca-file verify-depth)) ; These are used in create-openssl-context
  (make-tls-context :server-p server-p
                    :cert-file cert-file
                    :key-file key-file
                    :verify-mode (if (and server-p require-client-cert)
                                     (logior +ssl-verify-peer+ 
                                             +ssl-verify-fail-if-no-peer-cert+)
                                   verify-mode)))

(defun create-openssl-context (&key server-p cert-file key-file 
                                    ca-file (verify-mode +ssl-verify-peer+)
                                    require-client-cert verify-depth
                                    alpn-protocols session-cache-p)
  "Create an OpenSSL-backed TLS context with  security configuration.
   
   Creates a low-level OpenSSL SSL_CTX for advanced TLS operations including
   mutual authentication, certificate validation, and cipher suite control.
   
   Parameters:
     server-p (boolean): Create server context if T, client context if NIL
     cert-file (string): Path to X.509 certificate in PEM format
     key-file (string): Path to private key in PEM format
     ca-file (string): Path to CA certificates for peer verification
     verify-mode (integer): SSL verification mode (default: +SSL-VERIFY-PEER+)
     require-client-cert (boolean): Require client certificates (server only)
     verify-depth (integer): Maximum certificate chain depth to verify
   
   Returns:
     OPENSSL-CONTEXT structure with configured SSL_CTX handle
   
   Security Configuration:
     - Automatic certificate/key matching verification
     - CA certificate chain loading for peer validation
     - Client certificate requirement for mutual TLS
     - Configurable verification depth to prevent long chains
   
   Common verify-mode values:
     +SSL-VERIFY-NONE+ (0): No certificate verification (insecure)
     +SSL-VERIFY-PEER+ (1): Verify peer certificate
     +SSL-VERIFY-FAIL-IF-NO-PEER-CERT+ (2): Fail if no peer cert
     
   Security Notes:
     - Always use +SSL-VERIFY-PEER+ in production
     - Set require-client-cert for mutual TLS authentication
     - Limit verify-depth to prevent DoS from long chains (typical: 4-10)
     - Ensure cert-file and key-file have proper permissions (0600)
   
   Errors:
     Signals CRYPTO-ERROR if:
     - SSL context creation fails
     - Certificate or key loading fails
     - Certificate/key mismatch detected
     - CA certificate loading fails"
  (let* ((method (if server-p (ffi:%tls-server-method) (ffi:%tls-client-method)))
         (ctx (ffi:%ssl-ctx-new method)))
    
    (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create SSL context"))
    
    ;; Load certificate if provided
    (when cert-file
      (when (zerop (ffi:%ssl-ctx-use-certificate-file ctx cert-file +ssl-filetype-pem+))
        (ffi:%ssl-ctx-free ctx)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message (format nil "Failed to load certificate: ~A" cert-file))))
    
    ;; Load private key if provided
    (when key-file
      (when (zerop (ffi:%ssl-ctx-use-privatekey-file ctx key-file +ssl-filetype-pem+))
        (ffi:%ssl-ctx-free ctx)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message (format nil "Failed to load private key: ~A" key-file)))
      
      ;; Verify that private key matches certificate
      (when (zerop (ffi:%ssl-ctx-check-private-key ctx))
        (ffi:%ssl-ctx-free ctx)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Private key does not match certificate")))
    
    ;; Load CA certificates if provided
    (when ca-file
      ;; Use nil for ca-path parameter
      (when (zerop (handler-case
                       (ffi:%ssl-ctx-load-verify-locations ctx ca-file (sb-sys:int-sap 0))
                     (error ()
                       ;; Some OpenSSL versions may have issues with NULL path
                       0)))
        (ffi:%ssl-ctx-free ctx)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message (format nil "Failed to load CA certificates: ~A" ca-file)))
      
      ;; For servers requiring client certificates, set the CA list
      (when (and server-p require-client-cert)
        (let ((ca-list (ffi:%ssl-load-client-ca-file ca-file)))
          (when (sb-sys:sap= ca-list (sb-sys:int-sap 0))
            (ffi:%ssl-ctx-free ctx)
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "Failed to load client CA list"))
          (ffi:%ssl-ctx-set-client-ca-list ctx ca-list))))
    
    ;; Set verification mode and depth
    (let ((final-verify-mode (if (and server-p require-client-cert)
                                 (logior +ssl-verify-peer+ 
                                         +ssl-verify-fail-if-no-peer-cert+)
                               verify-mode)))
      (ffi:%ssl-ctx-set-verify ctx final-verify-mode (sb-sys:int-sap 0)))
    
    (when verify-depth
      (ffi:%ssl-ctx-ctrl ctx 35 verify-depth (sb-sys:int-sap 0)))  ; SSL_CTRL_SET_VERIFY_DEPTH
    
    ;; Set ALPN protocols if provided
    (when alpn-protocols
      (alpn:set-alpn-protocols ctx alpn-protocols :context-p t))
    
    ;; Configure session caching  
    (when session-cache-p
      ;; Session caching is enabled by default in OpenSSL
      ;; Just skip explicit configuration if function not available
      (ignore-errors
        (ffi:%ssl-ctx-set-session-cache-mode ctx 2)))  ; SSL_SESS_CACHE_SERVER
    
    (make-openssl-context :handle ctx
                          :server-p server-p
                          :cert-file cert-file
                          :key-file key-file
                          :verify-mode verify-mode)))

(defun load-cert-file (context cert-file)
  "Load an X.509 certificate from file into TLS context.
   
   Parameters:
     context: TLS-CONTEXT or OPENSSL-CONTEXT structure
     cert-file (string): Path to certificate file in PEM format
   
   Side Effects:
     Updates the context's certificate configuration
   
   Security Notes:
     - Certificate should match the previously loaded private key
     - File should contain the full certificate chain if needed
     - PEM format supports multiple certificates in one file
   
   Errors:
     Signals CRYPTO-ERROR if certificate loading fails or format is invalid"
  (cond
   ((openssl-context-p context)
    (when (zerop (ffi:%ssl-ctx-use-certificate-file 
                  (openssl-context-handle context) cert-file +ssl-filetype-pem+))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message (format nil "Failed to load certificate: ~A" cert-file)))
    (setf (openssl-context-cert-file context) cert-file))
   ((tls-context-p context)
    (setf (tls-context-cert-file context) cert-file))
   (t (error "Invalid context type"))))

(defun load-key-file (context key-file)
  "Load private key file into TLS context"
  (cond
   ((openssl-context-p context)
    (when (zerop (ffi:%ssl-ctx-use-privatekey-file 
                  (openssl-context-handle context) key-file +ssl-filetype-pem+))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message (format nil "Failed to load private key: ~A" key-file)))
    (when (zerop (ffi:%ssl-ctx-check-private-key (openssl-context-handle context)))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Private key does not match certificate"))
    (setf (openssl-context-key-file context) key-file))
   ((tls-context-p context)
    (setf (tls-context-key-file context) key-file))
   (t (error "Invalid context type"))))

(defun set-verify-mode (context mode)
  "Set verification mode for TLS context"
  (cond
   ((openssl-context-p context)
    (ffi:%ssl-ctx-set-verify (openssl-context-handle context) mode (sb-sys:int-sap 0))
    (setf (openssl-context-verify-mode context) mode))
   ((tls-context-p context)
    (setf (tls-context-verify-mode context) mode))
   (t (error "Invalid context type"))))

;;;; TLS Connection Management

(defun openssl-connect (socket context &key hostname alpn-protocols)
  "Establish TLS connection as client with SNI and ALPN support.
   Parameters:
   - socket: Network socket or file descriptor
   - context: OpenSSL context
   - hostname: Server hostname for SNI
   - alpn-protocols: List of ALPN protocols to advertise"
  
  (unless (openssl-context-p context)
    (error "OpenSSL context required"))
  
  (let* ((ssl (ffi:%ssl-new (openssl-context-handle context))))
    (when (sb-sys:sap= ssl (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create SSL connection"))
    
    ;; Set SNI hostname if provided
    (when hostname
      (ffi:%ssl-set-tlsext-host-name ssl hostname))
    
    ;; Set ALPN protocols if provided
    (when alpn-protocols
      (epsilon.crypto.alpn:set-alpn-protocols ssl alpn-protocols :context-p nil))

    ;; Set socket file descriptor
    ;; Extract FD from tcp-stream if needed
    (let ((fd (cond
                ((integerp socket) socket)
                ((typep socket 'net:tcp-stream)
                 (net:tcp-stream-handle socket))
                (t (error 'crypto-error :code 0
                          :message (format nil "Cannot extract FD from socket: ~A" socket))))))
      (when (zerop (ffi:%ssl-set-fd ssl fd))
        (ffi:%ssl-free ssl)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set socket FD")))

    ;; Perform handshake
    (let ((ret (ffi:%ssl-connect ssl)))
      (when (<= ret 0)
        (let* ((err (ffi:%ssl-get-error ssl ret))
               (openssl-errors (get-all-openssl-errors))
               (error-details (if openssl-errors
                                  (format nil "SSL_ERROR=~A: ~{~A~^; ~}" err openssl-errors)
                                  (format nil "SSL_ERROR=~A" err))))
          (ffi:%ssl-free ssl)
          (error 'crypto-error :code err
                 :message (format nil "SSL connect handshake failed: ~A" error-details)))))

    (make-openssl-connection :ssl ssl
                             :socket socket
                             :context context
                             :connected-p t)))

(defun openssl-accept (socket context)
  "Accept TLS connection as server"
  (unless (openssl-context-p context)
    (error "OpenSSL context required"))

  (let* ((ssl (ffi:%ssl-new (openssl-context-handle context))))
    (when (sb-sys:sap= ssl (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create SSL connection"))

    ;; Set socket file descriptor
    ;; Extract FD from tcp-stream if needed
    (let ((fd (cond
                ((integerp socket) socket)
                ((typep socket 'net:tcp-stream)
                 (net:tcp-stream-handle socket))
                (t (error 'crypto-error :code 0
                          :message (format nil "Cannot extract FD from socket: ~A" socket))))))
      (when (zerop (ffi:%ssl-set-fd ssl fd))
        (ffi:%ssl-free ssl)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set socket FD")))
    
    ;; Accept handshake
    (let ((ret (ffi:%ssl-accept ssl)))
      (when (<= ret 0)
        (let* ((err (ffi:%ssl-get-error ssl ret))
               (openssl-errors (get-all-openssl-errors))
               (error-details (if openssl-errors
                                  (format nil "SSL_ERROR=~A: ~{~A~^; ~}" err openssl-errors)
                                  (format nil "SSL_ERROR=~A" err))))
          (ffi:%ssl-free ssl)
          (error 'crypto-error :code err
                 :message (format nil "SSL accept handshake failed: ~A" error-details)))))

    (make-openssl-connection :ssl ssl
                             :socket socket
                             :context context
                             :connected-p t)))

(defun openssl-close (connection)
  "Close TLS connection"
  (when (openssl-connection-p connection)
    (when (openssl-connection-ssl connection)
      (ffi:%ssl-shutdown (openssl-connection-ssl connection))
      (ffi:%ssl-free (openssl-connection-ssl connection))
      (setf (openssl-connection-ssl connection) nil
            (openssl-connection-connected-p connection) nil)))
  t)

;;;; TLS I/O Operations

(defun openssl-read (connection buffer &key (start 0) (end (length buffer)))
  "Read data from TLS connection"
  (unless (and (openssl-connection-p connection)
               (openssl-connection-connected-p connection))
    (error "Not connected"))

  (let ((len (- end start)))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 65536)))
                         (let ((bytes-read (ffi:%ssl-read (openssl-connection-ssl connection)
                                                          (sb-alien:alien-sap buf)
                                                          (min len 65536))))
                           (cond
                            ((> bytes-read 0)
                             ;; Copy data to buffer, handling both strings and byte arrays
                             (loop for i from 0 below bytes-read
                                   do (setf (aref buffer (+ start i))
                                            (if (stringp buffer)
                                                (code-char (sb-alien:deref buf i))
                                                (sb-alien:deref buf i))))
                             bytes-read)
                            ((= bytes-read 0)
                             ;; Connection closed
                             0)
                            (t
                             ;; Error occurred
                             (let ((err (ffi:%ssl-get-error (openssl-connection-ssl connection) bytes-read)))
                               (error 'crypto-error :code err
                                      :message (format nil "SSL read error: ~A" err)))))))))

(defun openssl-write (connection buffer &key (start 0) (end (length buffer)))
  "Write data to TLS connection"
  (unless (and (openssl-connection-p connection)
               (openssl-connection-connected-p connection))
    (error "Not connected"))
  
  (let ((len (- end start)))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 65536)))
			 ;; Copy data to alien buffer
			 (loop for i from 0 below (min len 65536)
			       do (setf (sb-alien:deref buf i)
					(if (stringp buffer)
					    (char-code (char buffer (+ start i)))
					  (aref buffer (+ start i)))))
			 
			 (let ((bytes-written (ffi:%ssl-write (openssl-connection-ssl connection)
							      (sb-alien:alien-sap buf)
							      (min len 65536))))
			   (cond
			    ((> bytes-written 0)
			     bytes-written)
			    (t
			     ;; Error occurred
			     (let ((err (ffi:%ssl-get-error (openssl-connection-ssl connection) bytes-written)))
			       (error 'crypto-error :code err
				      :message (format nil "SSL write error: ~A" err)))))))))

(defun openssl-pending (connection)
  "Get number of bytes pending in SSL buffer"
  (if (and (openssl-connection-p connection)
           (openssl-connection-connected-p connection))
      (ffi:%ssl-pending (openssl-connection-ssl connection))
    0))

;;;; High-level TLS Functions

(defun tls-connect (socket &key context hostname)
  "Establish TLS connection as client"
  (let ((ctx (or context (create-openssl-context :server-p nil))))
    (openssl-connect socket ctx :hostname hostname)))

(defun tls-accept (socket &key context)
  "Accept TLS connection as server"
  (let ((ctx (or context (error "Server context required"))))
    (openssl-accept socket ctx)))

(defun tls-close (connection)
  "Close TLS connection"
  (cond
   ((openssl-connection-p connection)
    (openssl-close connection))
   ((tls-connection-p connection)
    (setf (tls-connection-connected-p connection) nil
          (tls-connection-handshake-complete-p connection) nil))
   (t (error "Invalid connection type"))))

(defun tls-read (connection buffer &key (start 0) (end (length buffer)))
  "Read from TLS connection"
  (cond
   ((openssl-connection-p connection)
    (openssl-read connection buffer :start start :end end))
   ((tls-connection-p connection)
    ;; Stub implementation
    (error "Stub TLS connection cannot read"))
   (t (error "Invalid connection type"))))

(defun tls-write (connection data)
  "Write to TLS connection"
  (cond
   ((openssl-connection-p connection)
    (openssl-write connection data))
   ((tls-connection-p connection)
    ;; Stub implementation
    (error "Stub TLS connection cannot write"))
   (t (error "Invalid connection type"))))

;;;; TLS Information Functions

(defun tls-version (connection)
  "Get TLS/SSL version string"
  (when (openssl-connection-p connection)
    (let ((version-ptr (ffi:%ssl-get-version (openssl-connection-ssl connection))))
      (unless (sb-sys:sap= version-ptr (sb-sys:int-sap 0))
        (sb-alien:cast version-ptr sb-alien:c-string)))))

(defun tls-cipher (connection)
  "Get current cipher name"
  (when (openssl-connection-p connection)
    (let ((cipher-ptr (ffi:%ssl-get-cipher (openssl-connection-ssl connection))))
      (unless (sb-sys:sap= cipher-ptr (sb-sys:int-sap 0))
        (sb-alien:cast cipher-ptr sb-alien:c-string)))))

(defun tls-selected-alpn-protocol (connection)
  "Get the ALPN protocol selected during handshake"
  (when (openssl-connection-p connection)
    (epsilon.crypto.alpn:get-selected-protocol (openssl-connection-ssl connection))))

(defun get-peer-certificate (connection)
  "Get peer's X.509 certificate from TLS connection"
  (when (openssl-connection-p connection)
    (let ((x509 (ffi:%ssl-get-peer-certificate (openssl-connection-ssl connection))))
      (unless (sb-sys:sap= x509 (sb-sys:int-sap 0))
        ;; Extract certificate details
        (let ((subject-name (ffi:%x509-get-subject-name x509))
              (issuer-name (ffi:%x509-get-issuer-name x509)))
          
          (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
			       (let ((subject-str (ffi:%x509-name-oneline subject-name 
									  (sb-alien:alien-sap buf) 256))
				     (issuer-str nil))
				 (setf subject-str (sb-alien:cast subject-str sb-alien:c-string))
				 
				 (setf issuer-str (ffi:%x509-name-oneline issuer-name
									  (sb-alien:alien-sap buf) 256))
				 (setf issuer-str (sb-alien:cast issuer-str sb-alien:c-string))
				 
				 (prog1
				     (make-x509-certificate :handle x509
							    :subject subject-str
							    :issuer issuer-str)
				   ;; Don't free x509 - it's still owned by the SSL connection
				   nil))))))))

(defun verify-peer-certificate (connection)
  "Verify the peer's certificate in the TLS connection"
  (when (openssl-connection-p connection)
    (let ((result (ffi:%ssl-get-verify-result (openssl-connection-ssl connection))))
      (cond
       ((= result 0) ; X509_V_OK
        (values t nil))
       (t
        (values nil (format nil "Certificate verification failed: code ~A" result))))))

  (defun get-certificate-subject (certificate)
    "Get the subject name from an X.509 certificate"
    (cond
     ((x509-certificate-p certificate)
      (x509-certificate-subject certificate))
     (t
      (error "Invalid certificate type"))))

  (defun get-certificate-issuer (certificate)
    "Get the issuer name from an X.509 certificate"
    (cond
     ((x509-certificate-p certificate)
      (x509-certificate-issuer certificate))
     (t
      (error "Invalid certificate type")))))

;;;; TLS Stream Interface

(defclass tls-stream (sb-gray:fundamental-binary-input-stream
                      sb-gray:fundamental-binary-output-stream
                      sb-gray:fundamental-character-input-stream
                      sb-gray:fundamental-character-output-stream)
  ((connection :initarg :connection :reader tls-stream-connection)
   (buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
           :reader tls-stream-buffer)
   (line-column :initform 0 :accessor tls-stream-line-column)))

;; Binary I/O methods
(defmethod sb-gray:stream-read-byte ((stream tls-stream))
  "Read a single byte from TLS stream"
  (let ((buffer (tls-stream-buffer stream)))
    (if (plusp (tls-read (tls-stream-connection stream) buffer :end 1))
        (aref buffer 0)
        :eof)))

(defmethod sb-gray:stream-write-byte ((stream tls-stream) byte)
  "Write a single byte to TLS stream"
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-element byte)))
    (tls-write (tls-stream-connection stream) buffer))
  byte)

;; Character I/O methods
(defmethod sb-gray:stream-read-char ((stream tls-stream))
  "Read a single character from TLS stream"
  (let ((byte (sb-gray:stream-read-byte stream)))
    (if (eq byte :eof)
        :eof
        (code-char byte))))

(defmethod sb-gray:stream-write-char ((stream tls-stream) char)
  "Write a single character to TLS stream"
  (sb-gray:stream-write-byte stream (char-code char))
  char)

(defmethod sb-gray:stream-write-string ((stream tls-stream) string &optional start end)
  "Write a string to TLS stream"
  (let* ((start (or start 0))
         (end (or end (length string)))
         (len (- end start)))
    (when (> len 0)
      (tls-write (tls-stream-connection stream)
                 (subseq string start end))))
  string)

(defmethod sb-gray:stream-line-column ((stream tls-stream))
  "Return current column position"
  (tls-stream-line-column stream))

(defmethod sb-gray:stream-force-output ((stream tls-stream))
  "Force output - no-op for TLS streams as writes are immediate"
  nil)

(defmethod sb-gray:stream-finish-output ((stream tls-stream))
  "Finish output - no-op for TLS streams as writes are immediate"
  nil)

(defmethod sb-gray:stream-read-char-no-hang ((stream tls-stream))
  "Non-blocking read - just call read-char for now"
  (sb-gray:stream-read-char stream))

(defmethod sb-gray:stream-unread-char ((stream tls-stream) char)
  "Unread a character - not implemented"
  (declare (ignore char))
  (error "UNREAD-CHAR not supported on TLS streams"))

(defun openssl-stream (connection)
  "Create a stream interface for TLS connection"
  (make-instance 'tls-stream :connection connection))

(defun tls-stream (connection)
  "Create a stream interface for TLS connection"
  (openssl-stream connection))
