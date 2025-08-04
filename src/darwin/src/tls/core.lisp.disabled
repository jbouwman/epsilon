(defpackage epsilon.tls
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   ;; TLS Context
   make-tls-context
   tls-context-p
   load-cert-file
   load-key-file
   set-verify-mode
   
   ;; TLS Connection
   tls-connect
   tls-accept
   tls-close
   with-tls-connection
   
   ;; TLS I/O
   tls-read
   tls-write
   tls-stream
   
   ;; TLS State
   tls-connected-p
   tls-get-peer-certificate
   tls-get-cipher
   
   ;; Constants
   +tls-verify-none+
   +tls-verify-peer+
   +tls-verify-fail-if-no-peer-cert+
   +tls-verify-client-once+))

(in-package :epsilon.tls)

;;;; LibreSSL/OpenSSL Constants

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)

(defconstant +ssl-verify-none+ 0)
(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-verify-fail-if-no-peer-cert+ 2)
(defconstant +ssl-verify-client-once+ 4)

;; Export the verify constants with TLS prefix
(defconstant +tls-verify-none+ +ssl-verify-none+)
(defconstant +tls-verify-peer+ +ssl-verify-peer+)
(defconstant +tls-verify-fail-if-no-peer-cert+ +ssl-verify-fail-if-no-peer-cert+)
(defconstant +tls-verify-client-once+ +ssl-verify-client-once+)

(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-zero-return+ 6)

;;;; LibreSSL FFI Bindings

;; SSL Library initialization
(lib:defshared %ssl-library-init "SSL_library_init" "libssl" :int ()
  :documentation "Initialize SSL library")

(lib:defshared %ssl-load-error-strings "SSL_load_error_strings" "libssl" :void ()
  :documentation "Load SSL error strings")

(lib:defshared %openssl-add-all-algorithms "OpenSSL_add_all_algorithms" "libssl" :void ()
  :documentation "Add all algorithms")

;; SSL Context management
(lib:defshared %tls-client-method "TLS_client_method" "libssl" :pointer ()
  :documentation "Get TLS client method")

(lib:defshared %tls-server-method "TLS_server_method" "libssl" :pointer ()
  :documentation "Get TLS server method")

(lib:defshared %ssl-ctx-new "SSL_CTX_new" "libssl" :pointer (method :pointer)
  :documentation "Create new SSL context")

(lib:defshared %ssl-ctx-free "SSL_CTX_free" "libssl" :void (ctx :pointer)
  :documentation "Free SSL context")

(lib:defshared %ssl-ctx-use-certificate-file "SSL_CTX_use_certificate_file" "libssl" :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load certificate file")

(lib:defshared %ssl-ctx-use-privatekey-file "SSL_CTX_use_PrivateKey_file" "libssl" :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load private key file")

(lib:defshared %ssl-ctx-check-private-key "SSL_CTX_check_private_key" "libssl" :int
  (ctx :pointer)
  :documentation "Check private key matches certificate")

(lib:defshared %ssl-ctx-set-verify "SSL_CTX_set_verify" "libssl" :void
  (ctx :pointer) (mode :int) (callback :pointer)
  :documentation "Set verification mode")

;; SSL Connection management
(lib:defshared %ssl-new "SSL_new" "libssl" :pointer (ctx :pointer)
  :documentation "Create new SSL connection")

(lib:defshared %ssl-free "SSL_free" "libssl" :void (ssl :pointer)
  :documentation "Free SSL connection")

(lib:defshared %ssl-set-fd "SSL_set_fd" "libssl" :int
  (ssl :pointer) (fd :int)
  :documentation "Set file descriptor for SSL connection")

(lib:defshared %ssl-connect "SSL_connect" "libssl" :int (ssl :pointer)
  :documentation "Initiate SSL handshake as client")

(lib:defshared %ssl-accept "SSL_accept" "libssl" :int (ssl :pointer)
  :documentation "Accept SSL handshake as server")

(lib:defshared %ssl-shutdown "SSL_shutdown" "libssl" :int (ssl :pointer)
  :documentation "Shutdown SSL connection")

;; SSL I/O operations
(lib:defshared %ssl-read "SSL_read" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Read from SSL connection")

(lib:defshared %ssl-write "SSL_write" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Write to SSL connection")

(lib:defshared %ssl-pending "SSL_pending" "libssl" :int (ssl :pointer)
  :documentation "Check if data is pending in SSL buffers")

;; SSL State and information
(lib:defshared %ssl-get-error "SSL_get_error" "libssl" :int
  (ssl :pointer) (ret :int)
  :documentation "Get SSL error code")

(lib:defshared %ssl-get-peer-certificate "SSL_get_peer_certificate" "libssl" :pointer
  (ssl :pointer)
  :documentation "Get peer certificate")

(lib:defshared %ssl-get-current-cipher "SSL_get_current_cipher" "libssl" :pointer
  (ssl :pointer)
  :documentation "Get current cipher")

(lib:defshared %ssl-cipher-get-name "SSL_CIPHER_get_name" "libssl" :string
  (cipher :pointer)
  :documentation "Get cipher name")

;;;; High-Level TLS Interface

(defvar *ssl-initialized* nil)

(defun ensure-ssl-initialized ()
  "Ensure SSL library is initialized"
  (unless *ssl-initialized*
    (%ssl-library-init)
    (%ssl-load-error-strings)
    (%openssl-add-all-algorithms)
    (setf *ssl-initialized* t)))

(defclass tls-context ()
  ((ssl-ctx :initarg :ssl-ctx :accessor context-ssl-ctx)
   (client-p :initarg :client-p :accessor context-client-p :initform t)
   (cert-file :initarg :cert-file :accessor context-cert-file :initform nil)
   (key-file :initarg :key-file :accessor context-key-file :initform nil)
   (verify-mode :initarg :verify-mode :accessor context-verify-mode 
                :initform +tls-verify-none+)))

(defclass tls-connection ()
  ((ssl :initarg :ssl :accessor connection-ssl)
   (socket :initarg :socket :accessor connection-socket)
   (context :initarg :context :accessor connection-context)
   (connected-p :initform nil :accessor connection-connected-p)))

(defun make-tls-context (&key (client-p t) cert-file key-file 
                               (verify-mode +tls-verify-none+))
  "Create a new TLS context"
  (ensure-ssl-initialized)
  
  (let* ((method (if client-p (%tls-client-method) (%tls-server-method)))
         (ssl-ctx (%ssl-ctx-new method)))
    
    (when (zerop (sb-sys:sap-int (sb-alien:alien-sap ssl-ctx)))
      (error "Failed to create SSL context"))
    
    (let ((ctx (make-instance 'tls-context
                              :ssl-ctx ssl-ctx
                              :client-p client-p
                              :cert-file cert-file
                              :key-file key-file
                              :verify-mode verify-mode)))
      
      ;; Load certificate and key if provided
      (when cert-file
        (load-cert-file ctx cert-file))
      (when key-file
        (load-key-file ctx key-file))
      
      ;; Set verification mode
      (set-verify-mode ctx verify-mode)
      
      ctx)))

(defun tls-context-p (obj)
  "Check if object is a TLS context"
  (typep obj 'tls-context))

(defun load-cert-file (context cert-file)
  "Load certificate file into context"
  (let ((result (%ssl-ctx-use-certificate-file 
                 (context-ssl-ctx context) 
                 cert-file 
                 +ssl-filetype-pem+)))
    (when (<= result 0)
      (error "Failed to load certificate file: ~A" cert-file))
    (setf (context-cert-file context) cert-file)
    t))

(defun load-key-file (context key-file)
  "Load private key file into context"
  (let ((result (%ssl-ctx-use-privatekey-file 
                 (context-ssl-ctx context) 
                 key-file 
                 +ssl-filetype-pem+)))
    (when (<= result 0)
      (error "Failed to load private key file: ~A" key-file))
    
    ;; Verify key matches certificate
    (let ((check-result (%ssl-ctx-check-private-key (context-ssl-ctx context))))
      (when (<= check-result 0)
        (error "Private key does not match certificate")))
    
    (setf (context-key-file context) key-file)
    t))

(defun set-verify-mode (context mode)
  "Set certificate verification mode"
  (%ssl-ctx-set-verify (context-ssl-ctx context) mode (sb-sys:int-sap 0))
  (setf (context-verify-mode context) mode))

(defun tls-connect (context socket)
  "Create TLS connection over existing socket (client)"
  (let* ((ssl (%ssl-new (context-ssl-ctx context)))
         (socket-fd (epsilon.net:socket-handle socket)))
    
    (when (zerop (sb-sys:sap-int (sb-alien:alien-sap ssl)))
      (error "Failed to create SSL connection"))
    
    (let ((result (%ssl-set-fd ssl socket-fd)))
      (when (<= result 0)
        (%ssl-free ssl)
        (error "Failed to set socket file descriptor")))
    
    (let ((conn (make-instance 'tls-connection
                               :ssl ssl
                               :socket socket
                               :context context)))
      
      ;; Perform handshake
      (let ((handshake-result (%ssl-connect ssl)))
        (when (<= handshake-result 0)
          (%ssl-free ssl)
          (error "TLS handshake failed")))
      
      (setf (connection-connected-p conn) t)
      conn)))

(defun tls-accept (context socket)
  "Accept TLS connection over existing socket (server)"
  (let* ((ssl (%ssl-new (context-ssl-ctx context)))
         (socket-fd (epsilon.net:socket-handle socket)))
    
    (when (zerop (sb-sys:sap-int (sb-alien:alien-sap ssl)))
      (error "Failed to create SSL connection"))
    
    (let ((result (%ssl-set-fd ssl socket-fd)))
      (when (<= result 0)
        (%ssl-free ssl)
        (error "Failed to set socket file descriptor")))
    
    (let ((conn (make-instance 'tls-connection
                               :ssl ssl
                               :socket socket
                               :context context)))
      
      ;; Perform handshake
      (let ((handshake-result (%ssl-accept ssl)))
        (when (<= handshake-result 0)
          (%ssl-free ssl)
          (error "TLS handshake failed")))
      
      (setf (connection-connected-p conn) t)
      conn)))

(defun tls-close (connection)
  "Close TLS connection"
  (when (connection-connected-p connection)
    (%ssl-shutdown (connection-ssl connection))
    (%ssl-free (connection-ssl connection))
    (setf (connection-connected-p connection) nil))
  (epsilon.net:socket-close (connection-socket connection)))

(defmacro with-tls-connection ((conn context socket) &body body)
  "Execute body with TLS connection, automatically closing on exit"
  `(let ((,conn (tls-connect ,context ,socket)))
     (unwind-protect
          (progn ,@body)
       (when ,conn
         (tls-close ,conn)))))

(defun tls-read (connection buffer &optional (length (length buffer)))
  "Read data from TLS connection"
  (lib:with-foreign-memory ((buf :char :count length))
    (let ((bytes-read (%ssl-read (connection-ssl connection) buf length)))
      (when (> bytes-read 0)
        (loop for i from 0 below bytes-read
              do (setf (aref buffer i) 
                       (code-char (sb-alien:deref buf i))))
        bytes-read))))

(defun tls-write (connection data)
  "Write data to TLS connection"
  (let* ((data-string (if (stringp data) data (format nil "~A" data)))
         (data-length (length data-string)))
    (lib:with-foreign-memory ((buf :char :count data-length))
      (loop for i from 0 below data-length
            do (setf (sb-alien:deref buf i) 
                     (char-code (char data-string i))))
      (let ((bytes-written (%ssl-write (connection-ssl connection) buf data-length)))
        (when (<= bytes-written 0)
          (error "Failed to write TLS data"))
        bytes-written))))

(defclass tls-stream ()
  ((connection :initarg :connection :accessor stream-connection)
   (input-buffer :initform (make-string 4096) :accessor stream-input-buffer)
   (output-buffer :initform (make-string 4096) :accessor stream-output-buffer)))

(defun tls-stream (connection)
  "Create a stream interface for TLS connection"
  (make-instance 'tls-stream :connection connection))

(defun tls-connected-p (connection)
  "Check if TLS connection is connected"
  (connection-connected-p connection))

(defun tls-get-peer-certificate (connection)
  "Get peer certificate (returns pointer, needs more work for full parsing)"
  (%ssl-get-peer-certificate (connection-ssl connection)))

(defun tls-get-cipher (connection)
  "Get current cipher name"
  (let ((cipher (%ssl-get-current-cipher (connection-ssl connection))))
    (unless (zerop (sb-sys:sap-int (sb-alien:alien-sap cipher)))
      (%ssl-cipher-get-name cipher))))

;;;; Integration with epsilon.net

;; Add finalizer to clean up SSL contexts
(defmethod initialize-instance :after ((ctx tls-context) &key)
  (sb-ext:finalize ctx 
    (lambda () 
      (%ssl-ctx-free (context-ssl-ctx ctx)))))

;; Add finalizer to clean up SSL connections  
(defmethod initialize-instance :after ((conn tls-connection) &key)
  (sb-ext:finalize conn
    (lambda ()
      (when (connection-connected-p conn)
        (%ssl-shutdown (connection-ssl conn))
        (%ssl-free (connection-ssl conn))))))
