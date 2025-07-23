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

;;;; OpenSSL Constants

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

;;;; OpenSSL FFI Definitions

(lib:define "libssl.so.3" "libssl.so.1.1" "libssl.so")
(lib:define "libcrypto.so.3" "libcrypto.so.1.1" "libcrypto.so")

;; SSL library initialization
(lib:c "OPENSSL_init_ssl" :int ((flags :uint64) (settings :pointer)))
(lib:c "SSL_library_init" :int ())
(lib:c "SSL_load_error_strings" :void ())
(lib:c "ERR_load_crypto_strings" :void ())

;; SSL Context functions
(lib:c "TLS_method" :pointer ())
(lib:c "TLS_server_method" :pointer ())
(lib:c "TLS_client_method" :pointer ())
(lib:c "SSL_CTX_new" :pointer ((method :pointer)))
(lib:c "SSL_CTX_free" :void ((ctx :pointer)))
(lib:c "SSL_CTX_use_certificate_file" :int ((ctx :pointer) (file :string) (type :int)))
(lib:c "SSL_CTX_use_PrivateKey_file" :int ((ctx :pointer) (file :string) (type :int)))
(lib:c "SSL_CTX_check_private_key" :int ((ctx :pointer)))
(lib:c "SSL_CTX_set_verify" :void ((ctx :pointer) (mode :int) (callback :pointer)))
(lib:c "SSL_CTX_load_verify_locations" :int ((ctx :pointer) (ca-file :string) (ca-path :string)))

;; SSL Connection functions
(lib:c "SSL_new" :pointer ((ctx :pointer)))
(lib:c "SSL_free" :void ((ssl :pointer)))
(lib:c "SSL_set_fd" :int ((ssl :pointer) (fd :int)))
(lib:c "SSL_connect" :int ((ssl :pointer)))
(lib:c "SSL_accept" :int ((ssl :pointer)))
(lib:c "SSL_shutdown" :int ((ssl :pointer)))
(lib:c "SSL_get_error" :int ((ssl :pointer) (ret :int)))

;; SSL I/O functions
(lib:c "SSL_read" :int ((ssl :pointer) (buf :pointer) (num :int)))
(lib:c "SSL_write" :int ((ssl :pointer) (buf :pointer) (num :int)))
(lib:c "SSL_pending" :int ((ssl :pointer)))

;; SSL State functions
(lib:c "SSL_get_peer_certificate" :pointer ((ssl :pointer)))
(lib:c "SSL_get_cipher" :string ((ssl :pointer)))
(lib:c "SSL_is_init_finished" :int ((ssl :pointer)))

;; Error handling
(lib:c "ERR_get_error" :ulong ())
(lib:c "ERR_error_string" :string ((error :ulong) (buf :pointer)))

;;;; Initialize OpenSSL

(defvar *ssl-initialized* nil)

(defun ensure-ssl-initialized ()
  (unless *ssl-initialized*
    ;; Try modern initialization first
    (handler-case
        (progn
          (OPENSSL_init_ssl 0 (sb-alien:null-alien))
          (setf *ssl-initialized* t))
      (error ()
        ;; Fall back to legacy initialization
        (SSL_library_init)
        (SSL_load_error_strings)
        (ERR_load_crypto_strings)
        (setf *ssl-initialized* t)))))

;;;; TLS Context

(defclass tls-context ()
  ((ctx :initarg :ctx :reader ctx)
   (server-p :initarg :server-p :reader server-p)))

(defun tls-context-p (obj)
  (typep obj 'tls-context))

(defun make-tls-context (&key server-p)
  "Create a new TLS context for client or server use"
  (ensure-ssl-initialized)
  (let* ((method (if server-p
                     (TLS_server_method)
                     (TLS_client_method)))
         (ctx (SSL_CTX_new method)))
    (when (sb-alien:null-alien-p ctx)
      (error "Failed to create SSL context"))
    (make-instance 'tls-context :ctx ctx :server-p server-p)))

(defun load-cert-file (context cert-file)
  "Load certificate from PEM file"
  (let ((result (SSL_CTX_use_certificate_file (ctx context) cert-file +ssl-filetype-pem+)))
    (unless (= result 1)
      (error "Failed to load certificate from ~A" cert-file))
    t))

(defun load-key-file (context key-file)
  "Load private key from PEM file"
  (let ((result (SSL_CTX_use_PrivateKey_file (ctx context) key-file +ssl-filetype-pem+)))
    (unless (= result 1)
      (error "Failed to load private key from ~A" key-file)))
  ;; Verify that the key matches the certificate
  (let ((result (SSL_CTX_check_private_key (ctx context))))
    (unless (= result 1)
      (error "Private key does not match certificate")))
  t)

(defun set-verify-mode (context mode)
  "Set certificate verification mode"
  (SSL_CTX_set_verify (ctx context) mode (sb-alien:null-alien))
  t)

;;;; TLS Connection

(defclass tls-connection ()
  ((ssl :initarg :ssl :reader ssl)
   (socket :initarg :socket :reader socket)
   (context :initarg :context :reader context)))

(defun ssl-check-error (ssl ret operation)
  "Check SSL operation result and signal appropriate error"
  (when (<= ret 0)
    (let ((err (SSL_get_error ssl ret)))
      (case err
        (0 nil) ; SSL_ERROR_NONE
        (1 (error "SSL ~A: SSL_ERROR_SSL" operation))
        (2 nil) ; SSL_ERROR_WANT_READ - would block
        (3 nil) ; SSL_ERROR_WANT_WRITE - would block
        (5 (error "SSL ~A: SSL_ERROR_SYSCALL" operation))
        (6 (error "SSL ~A: SSL_ERROR_ZERO_RETURN - Connection closed" operation))
        (otherwise (error "SSL ~A: Unknown error ~D" operation err))))))

(defun tls-connect (context socket &key hostname)
  "Establish TLS connection as client"
  (declare (ignore hostname)) ; TODO: Add SNI support
  (let* ((ssl (SSL_new (ctx context)))
         (fd (epsilon.net:socket-fd socket)))
    (when (sb-alien:null-alien-p ssl)
      (error "Failed to create SSL connection"))
    (SSL_set_fd ssl fd)
    (let ((ret (SSL_connect ssl)))
      (ssl-check-error ssl ret "connect")
      (make-instance 'tls-connection 
                     :ssl ssl 
                     :socket socket 
                     :context context))))

(defun tls-accept (context socket)
  "Accept TLS connection as server"
  (let* ((ssl (SSL_new (ctx context)))
         (fd (epsilon.net:socket-fd socket)))
    (when (sb-alien:null-alien-p ssl)
      (error "Failed to create SSL connection"))
    (SSL_set_fd ssl fd)
    (let ((ret (SSL_accept ssl)))
      (ssl-check-error ssl ret "accept")
      (make-instance 'tls-connection 
                     :ssl ssl 
                     :socket socket 
                     :context context))))

(defun tls-close (connection)
  "Close TLS connection"
  (SSL_shutdown (ssl connection))
  (SSL_free (ssl connection))
  (epsilon.net:socket-close (socket connection)))

(defmacro with-tls-connection ((var context socket &key hostname server-p) &body body)
  "Execute body with TLS connection, ensuring cleanup"
  `(let ((,var ,(if server-p
                    `(tls-accept ,context ,socket)
                    `(tls-connect ,context ,socket :hostname ,hostname))))
     (unwind-protect
          (progn ,@body)
       (tls-close ,var))))

;;;; TLS I/O

(defun tls-read (connection buffer &key (start 0) (end (length buffer)))
  "Read data from TLS connection into buffer"
  (let* ((len (- end start))
         (ret (sb-sys:with-pinned-objects (buffer)
                (SSL_read (ssl connection)
                          (sb-sys:vector-sap buffer start)
                          len))))
    (ssl-check-error (ssl connection) ret "read")
    (if (> ret 0) ret 0)))

(defun tls-write (connection buffer &key (start 0) (end (length buffer)))
  "Write data from buffer to TLS connection"
  (let* ((len (- end start))
         (ret (sb-sys:with-pinned-objects (buffer)
                (SSL_write (ssl connection)
                           (sb-sys:vector-sap buffer start)
                           len))))
    (ssl-check-error (ssl connection) ret "write")
    (if (> ret 0) ret 0)))

;;;; TLS Stream

(defclass tls-stream (fundamental-binary-input-stream
                      fundamental-binary-output-stream
                      fundamental-character-input-stream
                      fundamental-character-output-stream)
  ((connection :initarg :connection :reader connection)
   (buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
           :reader buffer)
   (buffer-pos :initform 0 :accessor buffer-pos)
   (buffer-end :initform 0 :accessor buffer-end)))

(defun tls-stream (connection)
  "Create a stream interface for TLS connection"
  (make-instance 'tls-stream :connection connection))

(defmethod stream-read-byte ((stream tls-stream))
  (with-slots (connection buffer buffer-pos buffer-end) stream
    (when (>= buffer-pos buffer-end)
      ;; Refill buffer
      (let ((n (tls-read connection buffer)))
        (if (zerop n)
            (return-from stream-read-byte :eof)
            (setf buffer-pos 0
                  buffer-end n))))
    (prog1 (aref buffer buffer-pos)
      (incf buffer-pos))))

(defmethod stream-write-byte ((stream tls-stream) byte)
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte)))
    (tls-write (connection stream) buf))
  byte)

(defmethod stream-write-sequence ((stream tls-stream) sequence start end &key)
  (let* ((len (- end start))
         (written (tls-write (connection stream) sequence :start start :end end)))
    (+ start written)))

(defmethod stream-read-sequence ((stream tls-stream) sequence start end &key)
  (let ((pos start))
    (loop while (< pos end)
          for byte = (stream-read-byte stream)
          do (if (eq byte :eof)
                 (return pos)
                 (setf (aref sequence pos) byte))
          do (incf pos))
    pos))

(defmethod close ((stream tls-stream) &key abort)
  (declare (ignore abort))
  (tls-close (connection stream)))

;;;; TLS State

(defun tls-connected-p (connection)
  "Check if TLS connection is established"
  (= 1 (SSL_is_init_finished (ssl connection))))

(defun tls-get-peer-certificate (connection)
  "Get peer certificate info (returns opaque pointer for now)"
  (SSL_get_peer_certificate (ssl connection)))

(defun tls-get-cipher (connection)
  "Get current cipher suite name"
  (SSL_get_cipher (ssl connection)))
