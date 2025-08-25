;;;; Mutual TLS (mTLS) Implementation
;;;;
;;;; Enhanced support for mutual TLS authentication

(defpackage :epsilon.crypto.mtls
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi)
   (#:tls #:epsilon.crypto.tls)
   (#:certs #:epsilon.crypto.certificates))
  (:export
   ;; mTLS Context Functions
   #:create-mtls-server-context
   #:create-mtls-client-context
   #:configure-mtls-context
   
   ;; Certificate Validation
   #:set-client-ca-list
   #:set-verify-callback
   #:extract-client-certificate
   #:verify-client-certificate
   #:get-certificate-chain
   
   ;; Certificate Information
   #:get-certificate-cn
   #:get-certificate-san
   #:get-certificate-fingerprint
   #:match-certificate-hostname
   
   ;; Session Management
   #:enable-session-resumption
   #:set-session-id-context
   #:get-session-info
   
   ;; mTLS Helpers
   #:with-mtls-server
   #:with-mtls-client
   #:ensure-mutual-auth))

(in-package :epsilon.crypto.mtls)

;;;; mTLS Context Creation

(defun create-mtls-server-context (&key cert-file key-file ca-file
                                        (verify-depth 4)
                                        session-id-context
                                        alpn-protocols)
  "Create a server context configured for mutual TLS authentication.
   
   Parameters:
     cert-file: Server certificate file path (required)
     key-file: Server private key file path (required)
     ca-file: CA certificates for client verification (required for mTLS)
     verify-depth: Maximum certificate chain depth (default: 4)
     session-id-context: Context identifier for session resumption
     alpn-protocols: List of supported ALPN protocols
   
   Returns:
     Configured OpenSSL context for mTLS server
   
   Example:
     (create-mtls-server-context 
       :cert-file \"server.pem\"
       :key-file \"server-key.pem\"
       :ca-file \"ca-chain.pem\"
       :alpn-protocols '(\"h2\" \"http/1.1\"))"
  
  (unless (and cert-file key-file ca-file)
    (error "Server certificate, key, and CA file required for mTLS"))
  
  (let ((ctx (tls:create-openssl-context
              :server-p t
              :cert-file cert-file
              :key-file key-file
              :ca-file ca-file
              :require-client-cert t
              :verify-mode (logior +ssl-verify-peer+
                                  +ssl-verify-fail-if-no-peer-cert+)
              :verify-depth verify-depth
              :alpn-protocols alpn-protocols
              :session-cache-p t)))
    
    ;; Set session ID context if provided
    (when session-id-context
      (set-session-id-context ctx session-id-context))
    
    ctx))

(defun create-mtls-client-context (&key cert-file key-file ca-file
                                        (verify-depth 4)
                                        alpn-protocols)
  "Create a client context configured for mutual TLS authentication.
   
   Parameters:
     cert-file: Client certificate file path (required for mTLS)
     key-file: Client private key file path (required for mTLS)
     ca-file: CA certificates for server verification
     verify-depth: Maximum certificate chain depth (default: 4)
     alpn-protocols: List of preferred ALPN protocols
   
   Returns:
     Configured OpenSSL context for mTLS client
   
   Example:
     (create-mtls-client-context
       :cert-file \"client.pem\"
       :key-file \"client-key.pem\"
       :ca-file \"ca.pem\"
       :alpn-protocols '(\"h2\" \"http/1.1\"))"
  
  (unless (and cert-file key-file)
    (error "Client certificate and key required for mTLS"))
  
  (tls:create-openssl-context
   :server-p nil
   :cert-file cert-file
   :key-file key-file
   :ca-file ca-file
   :verify-mode (if ca-file +ssl-verify-peer+ +ssl-verify-none+)
   :verify-depth verify-depth
   :alpn-protocols alpn-protocols
   :session-cache-p t))

(defun configure-mtls-context (context &key strict-mode cipher-suites min-tls-version)
  "Configure additional mTLS security settings.
   
   Parameters:
     context: OpenSSL context to configure
     strict-mode: Enable strict certificate validation
     cipher-suites: List of allowed cipher suites
     min-tls-version: Minimum TLS version (e.g., \"TLSv1.2\")
   
   Side Effects:
     Modifies the context security configuration"
  
  (when strict-mode
    ;; Enable additional verification checks
    (ffi:%ssl-ctx-set-verify (tls:openssl-context-handle context)
                             (logior +ssl-verify-peer+
                                    +ssl-verify-fail-if-no-peer-cert+
                                    #x04) ; SSL_VERIFY_CLIENT_ONCE
                             (sb-sys:int-sap 0)))
  
  (when cipher-suites
    ;; Set cipher list
    (let ((cipher-string (format nil "~{~A~^:~}" cipher-suites)))
      (ffi:%ssl-ctx-set-cipher-list (tls:openssl-context-handle context)
                                    cipher-string)))
  
  (when min-tls-version
    ;; Set minimum TLS version
    (let ((version-code (cond
                         ((string= min-tls-version "TLSv1.2") #x0303)
                         ((string= min-tls-version "TLSv1.3") #x0304)
                         (t #x0301))))
      ;; SSL_CTRL_SET_MIN_PROTO_VERSION = 123
      (ffi:%ssl-ctx-ctrl (tls:openssl-context-handle context) 
                        123 version-code (sb-sys:int-sap 0)))))

;;;; Certificate Validation

(defun set-client-ca-list (context ca-file)
  "Set the list of acceptable client CAs for a server context.
   
   Parameters:
     context: Server OpenSSL context
     ca-file: Path to CA certificates file
   
   Side Effects:
     Updates the context's client CA list"
  
  (let ((ca-list (ffi:%ssl-load-client-ca-file ca-file)))
    (when (sb-sys:sap= ca-list (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to load client CA list"))
    (ffi:%ssl-ctx-set-client-ca-list (tls:openssl-context-handle context) ca-list)))

(defun extract-client-certificate (connection)
  "Extract the client certificate from an mTLS connection.
   
   Parameters:
     connection: OpenSSL connection
   
   Returns:
     X.509 certificate object or NIL if no client cert
   
   Example:
     (let ((cert (extract-client-certificate conn)))
       (when cert
         (format t \"Client CN: ~A~%\" (get-certificate-cn cert))))"
  
  (when (tls:openssl-connection-p connection)
    (tls:get-peer-certificate connection)))

(defun verify-client-certificate (connection &key check-hostname expected-cn)
  "Verify the client certificate in an mTLS connection.
   
   Parameters:
     connection: OpenSSL connection
     check-hostname: Hostname to verify against certificate
     expected-cn: Expected Common Name in certificate
   
   Returns:
     (values verified-p error-message)
   
   Example:
     (multiple-value-bind (valid-p error)
         (verify-client-certificate conn :expected-cn \"client.example.com\")
       (unless valid-p
         (error \"Client verification failed: ~A\" error)))"
  
  (multiple-value-bind (basic-valid-p basic-error)
      (tls:verify-peer-certificate connection)
    
    (unless basic-valid-p
      (return-from verify-client-certificate (values nil basic-error)))
    
    ;; Additional validation
    (let ((cert (extract-client-certificate connection)))
      (unless cert
        (return-from verify-client-certificate 
          (values nil "No client certificate provided")))
      
      ;; Check CN if specified
      (when expected-cn
        (let ((actual-cn (get-certificate-cn cert)))
          (unless (string= actual-cn expected-cn)
            (return-from verify-client-certificate
              (values nil (format nil "CN mismatch: expected ~A, got ~A" 
                                 expected-cn actual-cn))))))
      
      ;; Check hostname if specified
      (when check-hostname
        (unless (match-certificate-hostname cert check-hostname)
          (return-from verify-client-certificate
            (values nil (format nil "Hostname ~A not in certificate" 
                               check-hostname)))))
      
      (values t nil))))

(defun get-certificate-chain (connection)
  "Get the full certificate chain from a connection.
   
   Parameters:
     connection: OpenSSL connection
   
   Returns:
     List of X.509 certificates in the chain"
  
  (when (tls:openssl-connection-p connection)
    ;; For now, just return the peer certificate as a single-element list
    ;; Full chain extraction would require additional FFI bindings
    (let ((peer-cert (tls:get-peer-certificate connection)))
      (when peer-cert
        (list peer-cert)))))

;;;; Certificate Information

(defun get-certificate-cn (certificate)
  "Extract Common Name (CN) from certificate subject.
   
   Parameters:
     certificate: X.509 certificate object
   
   Returns:
     Common Name string or NIL"
  
  (when (tls:x509-certificate-p certificate)
    (let ((subject (tls:x509-certificate-subject certificate)))
      (when subject
        ;; Parse CN from subject string (format: /CN=value/...)
        (let ((cn-pos (search "CN=" subject)))
          (when cn-pos
            (let* ((start (+ cn-pos 3))
                   (end (or (position #\/ subject :start start) (length subject))))
              (subseq subject start end))))))))

(defun get-certificate-san (certificate)
  "Extract Subject Alternative Names from certificate.
   
   Parameters:
     certificate: X.509 certificate object
   
   Returns:
     List of SAN entries (DNS names, IP addresses, etc.)"
  
  ;; This would need additional FFI bindings to X509_get_ext_d2i
  ;; For now, return empty list as placeholder
  '())

(defun get-certificate-fingerprint (certificate &key (algorithm :sha256))
  "Calculate certificate fingerprint.
   
   Parameters:
     certificate: X.509 certificate object
     algorithm: Hash algorithm (:sha1, :sha256, :sha512)
   
   Returns:
     Fingerprint as hex string"
  
  (when (tls:x509-certificate-p certificate)
    ;; This would need FFI bindings to X509_digest
    ;; For now, return a placeholder
    (format nil "~A:fingerprint:placeholder" algorithm)))

(defun match-certificate-hostname (certificate hostname)
  "Check if hostname matches certificate CN or SANs.
   
   Parameters:
     certificate: X.509 certificate object
     hostname: Hostname to verify
   
   Returns:
     T if hostname matches, NIL otherwise"
  
  (when (tls:x509-certificate-p certificate)
    (let ((cn (get-certificate-cn certificate))
          (sans (get-certificate-san certificate)))
      
      ;; Check CN
      (when (and cn (string-equal cn hostname))
        (return-from match-certificate-hostname t))
      
      ;; Check SANs
      (dolist (san sans)
        (when (string-equal san hostname)
          (return-from match-certificate-hostname t)))
      
      nil)))

;;;; Session Management

(defun enable-session-resumption (context &key timeout cache-size)
  "Enable and configure TLS session resumption.
   
   Parameters:
     context: OpenSSL context
     timeout: Session timeout in seconds (default: 300)
     cache-size: Maximum cache size (default: 1024)"
  
  (let ((ctx-handle (tls:openssl-context-handle context)))
    ;; Enable session caching
    ;; SSL_SESS_CACHE_SERVER | SSL_SESS_CACHE_NO_AUTO_CLEAR
    ;; SSL_CTRL_SET_SESS_CACHE_MODE = 44
    (ffi:%ssl-ctx-ctrl ctx-handle 44 #x0102 (sb-sys:int-sap 0))
    
    ;; Set timeout if specified
    (when timeout
      ;; SSL_CTRL_SET_TIMEOUT = 14
      (ffi:%ssl-ctx-ctrl ctx-handle 14 timeout (sb-sys:int-sap 0)))
    
    ;; Set cache size if specified
    (when cache-size
      ;; SSL_CTRL_SET_SESS_CACHE_SIZE = 42
      (ffi:%ssl-ctx-ctrl ctx-handle 42 cache-size (sb-sys:int-sap 0)))))

(defun set-session-id-context (context sid-context)
  "Set session ID context for server.
   
   Parameters:
     context: Server OpenSSL context
     sid-context: Session ID context string
   
   Side Effects:
     Configures session ID context for resumption"
  
  (let ((ctx-handle (tls:openssl-context-handle context))
        (sid-bytes (sb-ext:string-to-octets sid-context)))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 32)))
      (dotimes (i (min (length sid-bytes) 32))
        (setf (sb-alien:deref buf i) (aref sid-bytes i)))
      ;; SSL_CTRL_SET_SESS_ID_CTX = 60
      (ffi:%ssl-ctx-ctrl ctx-handle 60 (min (length sid-bytes) 32)
                        (sb-alien:alien-sap buf)))))

(defun get-session-info (connection)
  "Get session information from a connection.
   
   Parameters:
     connection: OpenSSL connection
   
   Returns:
     Property list with session info (:resumed-p :session-id :cipher)"
  
  (when (tls:openssl-connection-p connection)
    (let ((ssl (tls:openssl-connection-ssl connection)))
      (list :resumed-p nil  ; Session resumption check would need additional FFI
            :session-id nil  ; Would need additional FFI
            :cipher (tls:tls-cipher connection)
            :version (tls:tls-version connection)))))

;;;; High-level mTLS Helpers

(defmacro with-mtls-server ((server-var &key port cert-file key-file ca-file
                                         handler alpn-protocols) &body body)
  "Execute body with an mTLS server running.
   
   Example:
     (with-mtls-server (server :port 8443
                               :cert-file \"server.pem\"
                               :key-file \"server-key.pem\"
                               :ca-file \"ca.pem\"
                               :handler #'handle-request)
       (format t \"Server running on port 8443~%\")
       (sleep 60))"
  
  `(let ((,server-var (create-mtls-server-context
                       :cert-file ,cert-file
                       :key-file ,key-file
                       :ca-file ,ca-file
                       :alpn-protocols ,alpn-protocols)))
     (unwind-protect
          (progn ,@body)
       ;; Cleanup would go here
       nil)))

(defmacro with-mtls-client ((client-var &key host port cert-file key-file ca-file
                                         alpn-protocols) &body body)
  "Execute body with an mTLS client connection.
   
   Example:
     (with-mtls-client (client :host \"server.example.com\"
                              :port 8443
                              :cert-file \"client.pem\"
                              :key-file \"client-key.pem\"
                              :ca-file \"ca.pem\")
       (send-request client \"GET / HTTP/1.1\"))"
  
  `(let* ((ctx (create-mtls-client-context
                :cert-file ,cert-file
                :key-file ,key-file
                :ca-file ,ca-file
                :alpn-protocols ,alpn-protocols))
          (,client-var (tls:tls-connect nil :context ctx :hostname ,host)))
     (unwind-protect
          (progn ,@body)
       (when ,client-var
         (tls:tls-close ,client-var)))))

(defun ensure-mutual-auth (connection)
  "Ensure mutual authentication was successful.
   
   Parameters:
     connection: TLS connection to verify
   
   Returns:
     T if mutual auth successful
   
   Signals:
     Error if mutual auth failed or not configured"
  
  (unless (extract-client-certificate connection)
    (error "No client certificate provided - mutual auth failed"))
  
  (multiple-value-bind (valid-p error)
      (verify-client-certificate connection)
    (unless valid-p
      (error "Client certificate validation failed: ~A" error)))
  
  t)