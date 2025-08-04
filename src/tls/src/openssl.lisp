;;;; OpenSSL-based TLS Implementation
;;;;
;;;; Production TLS support using OpenSSL via epsilon.foreign

(defpackage :epsilon.tls.openssl
  (:use :cl)
  (:local-nicknames
   (#:lib #:epsilon.foreign)
   (#:net #:epsilon.net)
   (#:map #:epsilon.map))
  (:export
   ;; Classes
   #:openssl-context
   #:openssl-connection
   
   ;; TLS Context
   #:make-openssl-context
   #:openssl-context-p
   #:load-cert-file
   #:load-key-file
   #:set-verify-mode
   #:set-cipher-list
   
   ;; TLS Connection
   #:openssl-connect
   #:openssl-accept
   #:openssl-close
   #:with-openssl-connection
   
   ;; TLS I/O
   #:openssl-read
   #:openssl-write
   #:openssl-stream
   #:openssl-pending
   
   ;; TLS State
   #:openssl-connected-p
   #:openssl-get-peer-certificate
   #:openssl-get-cipher
   #:openssl-get-version
   
   ;; Error handling
   #:openssl-get-error
   #:openssl-error-string
   
   ;; Constants
   #:+ssl-filetype-pem+
   #:+ssl-filetype-asn1+
   #:+ssl-verify-none+
   #:+ssl-verify-peer+
   #:+ssl-verify-fail-if-no-peer-cert+
   #:+ssl-verify-client-once+))

(in-package :epsilon.tls.openssl)

;;;; OpenSSL Constants

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)

(defconstant +ssl-verify-none+ 0)
(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-verify-fail-if-no-peer-cert+ 2)
(defconstant +ssl-verify-client-once+ 4)

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)
(defconstant +ssl-error-want-accept+ 8)

;;;; OpenSSL Library FFI Bindings

;; SSL Library initialization - Not needed in OpenSSL 3.x (automatic initialization)
;; Legacy initialization functions are deprecated and may not be available
;; (lib:defshared %ssl-library-init "SSL_library_init" "libssl" :int ()
;;   :documentation "Initialize SSL library (deprecated)")
;; 
;; (lib:defshared %ssl-load-error-strings "SSL_load_error_strings" "libssl" :void ()
;;   :documentation "Load SSL error strings (deprecated)")
;; 
;; (lib:defshared %openssl-add-all-algorithms "OpenSSL_add_all_algorithms" "libssl" :void ()
;;   :documentation "Add all algorithms (deprecated)")

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

(lib:defshared %ssl-ctx-set-cipher-list "SSL_CTX_set_cipher_list" "libssl" :int
  (ctx :pointer) (str :string)
  :documentation "Set cipher list")

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

(lib:defshared %ssl-get-version "SSL_get_version" "libssl" :string
  (ssl :pointer)
  :documentation "Get SSL/TLS version")

;; Error handling
(lib:defshared %err-get-error "ERR_get_error" "libssl" :unsigned-long ()
  :documentation "Get error code")

(lib:defshared %err-error-string-n "ERR_error_string_n" "libssl" :void
  (e :unsigned-long) (buf :pointer) (len :size)
  :documentation "Get error string")

;;;; High-Level Interface

(defvar *ssl-initialized* nil)

(defun ensure-ssl-initialized ()
  "Ensure SSL library is initialized"
  (unless *ssl-initialized*
    ;; For OpenSSL 3.x, initialization is automatic and not required
    ;; We just check that we can create a context to verify the library works
    (handler-case
        (let ((method (%tls-client-method)))
          (when method
            (let ((ctx (%ssl-ctx-new method)))
              (when ctx
                (%ssl-ctx-free ctx)
                (setf *ssl-initialized* t)))))
      (error (e)
        (error "Failed to initialize SSL library: ~A" e)))))

(defclass openssl-context ()
  ((ssl-ctx :initarg :ssl-ctx :accessor context-ssl-ctx)
   (server-p :initarg :server-p :accessor context-server-p :initform nil)
   (cert-file :initarg :cert-file :accessor context-cert-file :initform nil)
   (key-file :initarg :key-file :accessor context-key-file :initform nil)
   (verify-mode :initarg :verify-mode :accessor context-verify-mode 
                :initform +ssl-verify-none+)
   (cipher-list :initarg :cipher-list :accessor context-cipher-list
                :initform "HIGH:!aNULL:!MD5")))

(defclass openssl-connection ()
  ((ssl :initarg :ssl :accessor connection-ssl)
   (socket :initarg :socket :accessor connection-socket)
   (context :initarg :context :accessor connection-context)
   (connected-p :initform nil :accessor connection-connected-p)))

(defun make-openssl-context (&key server-p cert-file key-file 
                                   (verify-mode +ssl-verify-none+)
                                   (cipher-list "HIGH:!aNULL:!MD5"))
  "Create a new OpenSSL context"
  (ensure-ssl-initialized)
  
  (let* ((method (if server-p (%tls-server-method) (%tls-client-method)))
         (ssl-ctx (%ssl-ctx-new method)))
    
    (when (or (not ssl-ctx) (zerop (sb-sys:sap-int ssl-ctx)))
      (error "Failed to create SSL context"))
    
    (let ((ctx (make-instance 'openssl-context
                              :ssl-ctx ssl-ctx
                              :server-p server-p
                              :cert-file cert-file
                              :key-file key-file
                              :verify-mode verify-mode
                              :cipher-list cipher-list)))
      
      ;; Set cipher list
      (set-cipher-list ctx cipher-list)
      
      ;; Load certificate and key if provided
      (when cert-file
        (load-cert-file ctx cert-file))
      (when key-file
        (load-key-file ctx key-file))
      
      ;; Set verification mode
      (set-verify-mode ctx verify-mode)
      
      ctx)))

(defun openssl-context-p (obj)
  "Check if object is an OpenSSL context"
  (typep obj 'openssl-context))

(defun load-cert-file (context cert-file)
  "Load certificate file into context"
  (let ((result (%ssl-ctx-use-certificate-file 
                 (context-ssl-ctx context) 
                 cert-file 
                 +ssl-filetype-pem+)))
    (when (<= result 0)
      (error "Failed to load certificate file: ~A (~A)" 
             cert-file (openssl-error-string)))
    (setf (context-cert-file context) cert-file)
    t))

(defun load-key-file (context key-file)
  "Load private key file into context"
  (let ((result (%ssl-ctx-use-privatekey-file 
                 (context-ssl-ctx context) 
                 key-file 
                 +ssl-filetype-pem+)))
    (when (<= result 0)
      (error "Failed to load private key file: ~A (~A)" 
             key-file (openssl-error-string)))
    
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

(defun set-cipher-list (context cipher-list)
  "Set allowed cipher list"
  (let ((result (%ssl-ctx-set-cipher-list (context-ssl-ctx context) cipher-list)))
    (when (<= result 0)
      (error "Failed to set cipher list: ~A" cipher-list))
    (setf (context-cipher-list context) cipher-list)
    t))

(defun get-socket-fd (socket)
  "Get file descriptor from socket"
  ;; Handle both raw sockets and epsilon.net sockets
  (cond
    ((typep socket 'sb-bsd-sockets:socket)
     (sb-bsd-sockets:socket-file-descriptor socket))
    ((and (find-package :epsilon.net)
          (typep socket (find-symbol "SOCKET" :epsilon.net)))
     ;; Get the handle from epsilon.net socket
     (let ((handle (funcall (find-symbol "SOCKET-HANDLE" :epsilon.net) socket)))
       (sb-bsd-sockets:socket-file-descriptor handle)))
    (t (error "Unknown socket type: ~A" (type-of socket)))))

(defun openssl-connect (context socket)
  "Create TLS connection over existing socket (client)"
  (let* ((ssl (%ssl-new (context-ssl-ctx context)))
         (socket-fd (get-socket-fd socket)))
    
    (when (zerop (sb-sys:sap-int (sb-alien:alien-sap ssl)))
      (error "Failed to create SSL connection"))
    
    (let ((result (%ssl-set-fd ssl socket-fd)))
      (when (<= result 0)
        (%ssl-free ssl)
        (error "Failed to set socket file descriptor")))
    
    (let ((conn (make-instance 'openssl-connection
                               :ssl ssl
                               :socket socket
                               :context context)))
      
      ;; Perform handshake
      (let ((handshake-result (%ssl-connect ssl)))
        (when (<= handshake-result 0)
          (let ((err (openssl-get-error conn handshake-result)))
            (%ssl-free ssl)
            (error "TLS handshake failed: ~A" err))))
      
      (setf (connection-connected-p conn) t)
      conn)))

(defun openssl-accept (context socket)
  "Accept TLS connection over existing socket (server)"
  (let* ((ssl (%ssl-new (context-ssl-ctx context)))
         (socket-fd (get-socket-fd socket)))
    
    (when (zerop (sb-sys:sap-int (sb-alien:alien-sap ssl)))
      (error "Failed to create SSL connection"))
    
    (let ((result (%ssl-set-fd ssl socket-fd)))
      (when (<= result 0)
        (%ssl-free ssl)
        (error "Failed to set socket file descriptor")))
    
    (let ((conn (make-instance 'openssl-connection
                               :ssl ssl
                               :socket socket
                               :context context)))
      
      ;; Perform handshake
      (let ((handshake-result (%ssl-accept ssl)))
        (when (<= handshake-result 0)
          (let ((err (openssl-get-error conn handshake-result)))
            (%ssl-free ssl)
            (error "TLS handshake failed: ~A" err))))
      
      (setf (connection-connected-p conn) t)
      conn)))

(defun openssl-close (connection)
  "Close TLS connection"
  (when (connection-connected-p connection)
    (%ssl-shutdown (connection-ssl connection))
    (%ssl-free (connection-ssl connection))
    (setf (connection-connected-p connection) nil)))

(defmacro with-openssl-connection ((conn context socket) &body body)
  "Execute body with TLS connection, automatically closing on exit"
  `(let ((,conn (openssl-connect ,context ,socket)))
     (unwind-protect
          (progn ,@body)
       (when ,conn
         (openssl-close ,conn)))))

(defun openssl-read (connection buffer &optional (length (length buffer)))
  "Read data from TLS connection into buffer"
  (lib:with-foreign-memory ((buf :unsigned-char :count length))
    (let ((bytes-read (%ssl-read (connection-ssl connection) buf length)))
      (cond
        ((> bytes-read 0)
         ;; Copy data to buffer
         (dotimes (i bytes-read)
           (setf (aref buffer i) (sb-alien:deref buf i)))
         bytes-read)
        ((<= bytes-read 0)
         ;; Check for errors
         (let ((error-code (%ssl-get-error (connection-ssl connection) bytes-read)))
           (case error-code
             (#.+ssl-error-zero-return+ 0) ; Clean shutdown
             (#.+ssl-error-want-read+ :want-read)
             (#.+ssl-error-want-write+ :want-write)
             (t (error "SSL read error: ~A" (openssl-error-string))))))))))

(defun openssl-write (connection data &key (start 0) (end (length data)))
  "Write data to TLS connection"
  (let* ((data-vector (cond
                        ((stringp data) 
                         (map 'vector #'char-code (subseq data start end)))
                        ((vectorp data)
                         (subseq data start end))
                        (t (error "Data must be string or vector"))))
         (data-length (length data-vector)))
    
    (lib:with-foreign-memory ((buf :unsigned-char :count data-length))
      ;; Copy data to foreign buffer
      (dotimes (i data-length)
        (setf (sb-alien:deref buf i) (aref data-vector i)))
      
      (let ((bytes-written (%ssl-write (connection-ssl connection) buf data-length)))
        (cond
          ((> bytes-written 0) bytes-written)
          ((<= bytes-written 0)
           (let ((error-code (%ssl-get-error (connection-ssl connection) bytes-written)))
             (case error-code
               (#.+ssl-error-want-read+ :want-read)
               (#.+ssl-error-want-write+ :want-write)
               (t (error "SSL write error: ~A" (openssl-error-string)))))))))))

(defun openssl-pending (connection)
  "Return number of bytes pending in SSL buffers"
  (%ssl-pending (connection-ssl connection)))

(defun openssl-stream (connection)
  "Create a stream interface for TLS connection"
  ;; This would need a proper Gray streams implementation
  ;; For now, return a simple wrapper
  (make-instance 'openssl-stream-wrapper :connection connection))

(defclass openssl-stream-wrapper ()
  ((connection :initarg :connection :accessor stream-connection)))

(defun openssl-connected-p (connection)
  "Check if TLS connection is connected"
  (connection-connected-p connection))

(defun openssl-get-peer-certificate (connection)
  "Get peer certificate information"
  (let ((cert (%ssl-get-peer-certificate (connection-ssl connection))))
    (unless (zerop (sb-sys:sap-int (sb-alien:alien-sap cert)))
      ;; Would need X509 parsing functions to extract details
      ;; For now, just return that we have a certificate
      (map:make-map :has-certificate t))))

(defun openssl-get-cipher (connection)
  "Get current cipher name"
  (let ((cipher (%ssl-get-current-cipher (connection-ssl connection))))
    (unless (zerop (sb-sys:sap-int (sb-alien:alien-sap cipher)))
      (%ssl-cipher-get-name cipher))))

(defun openssl-get-version (connection)
  "Get SSL/TLS version"
  (%ssl-get-version (connection-ssl connection)))

(defun openssl-get-error (connection ret-code)
  "Get SSL error for operation result"
  (let ((error-code (%ssl-get-error (connection-ssl connection) ret-code)))
    (case error-code
      (#.+ssl-error-none+ :none)
      (#.+ssl-error-ssl+ :ssl)
      (#.+ssl-error-want-read+ :want-read)
      (#.+ssl-error-want-write+ :want-write)
      (#.+ssl-error-want-x509-lookup+ :want-x509-lookup)
      (#.+ssl-error-syscall+ :syscall)
      (#.+ssl-error-zero-return+ :zero-return)
      (#.+ssl-error-want-connect+ :want-connect)
      (#.+ssl-error-want-accept+ :want-accept)
      (t error-code))))

(defun openssl-error-string ()
  "Get human-readable error string for last SSL error"
  (let ((error-code (%err-get-error)))
    (if (zerop error-code)
        "No error"
        (lib:with-foreign-memory ((buf :char :count 256))
          (%err-error-string-n error-code buf 256)
          ;; Convert null-terminated C string to Lisp string
          (let ((str (make-string 256)))
            (dotimes (i 256)
              (let ((c (sb-alien:deref buf i)))
                (if (zerop c)
                    (return-from openssl-error-string (subseq str 0 i))
                    (setf (char str i) (code-char c)))))
            str)))))

;;;; Finalizers

(defmethod initialize-instance :after ((ctx openssl-context) &key)
  (sb-ext:finalize ctx 
    (let ((ssl-ctx (context-ssl-ctx ctx)))
      (lambda () 
        (%ssl-ctx-free ssl-ctx)))))

(defmethod initialize-instance :after ((conn openssl-connection) &key)
  (sb-ext:finalize conn
    (let ((ssl (connection-ssl conn)))
      (lambda ()
        (ignore-errors
          (%ssl-shutdown ssl)
          (%ssl-free ssl))))))