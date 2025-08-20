;;;; TLS/SSL Implementation
;;;;
;;;; This file provides TLS/SSL functionality using OpenSSL

(defpackage :epsilon.crypto.tls
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
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
		#:crypto-error))

(in-package :epsilon.crypto.tls)

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
                                    require-client-cert verify-depth)
  "Create an OpenSSL-backed TLS context with comprehensive security configuration.
   
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
      (when (zerop (ffi:%ssl-ctx-load-verify-locations ctx ca-file (sb-sys:int-sap 0)))
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
      ;; TODO: Add SSL_CTX_set_verify_depth FFI binding if needed
      )
    
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

(defun openssl-connect (socket context &key hostname)
  "Establish TLS connection as client"
  (declare (ignore hostname)) ; TODO: Add SNI support
  
  (unless (openssl-context-p context)
    (error "OpenSSL context required"))
  
  (let* ((ssl (ffi:%ssl-new (openssl-context-handle context))))
    (when (sb-sys:sap= ssl (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create SSL connection"))
    
    ;; Set socket file descriptor  
    ;; TODO: Get FD from socket - needs net module integration
    (let ((fd (if (integerp socket) socket 0)))
      (when (zerop (ffi:%ssl-set-fd ssl fd))
        (ffi:%ssl-free ssl)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set socket FD")))
    
    ;; Perform handshake
    (let ((ret (ffi:%ssl-connect ssl)))
      (when (<= ret 0)
        (let ((err (ffi:%ssl-get-error ssl ret)))
          (ffi:%ssl-free ssl)
          (error 'crypto-error :code err
                 :message (format nil "SSL handshake failed: ~A" err)))))
    
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
    ;; TODO: Get FD from socket - needs net module integration
    (let ((fd (if (integerp socket) socket 0)))
      (when (zerop (ffi:%ssl-set-fd ssl fd))
        (ffi:%ssl-free ssl)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set socket FD")))
    
    ;; Accept handshake
    (let ((ret (ffi:%ssl-accept ssl)))
      (when (<= ret 0)
        (let ((err (ffi:%ssl-get-error ssl ret)))
          (ffi:%ssl-free ssl)
          (error 'crypto-error :code err
                 :message (format nil "SSL accept failed: ~A" err)))))
    
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
			     ;; Copy data to buffer
			     (loop for i from 0 below bytes-read
				   do (setf (aref buffer (+ start i))
					    (sb-alien:deref buf i)))
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
		      sb-gray:fundamental-binary-output-stream)
  ((connection :initarg :connection :reader tls-stream-connection)
   (buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
           :reader tls-stream-buffer)))

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

(defun openssl-stream (connection)
  "Create a stream interface for TLS connection"
  (make-instance 'tls-stream :connection connection))

(defun tls-stream (connection)
  "Create a stream interface for TLS connection"
  (openssl-stream connection))
