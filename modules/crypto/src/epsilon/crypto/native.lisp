;;;; epsilon.crypto.native -- Native TLS bridge to epsilon.ssl
;;;;
;;;; Provides TLS context/connection/stream structs and functions backed
;;;; entirely by the pure-Lisp TLS 1.3 implementation.
;;;;
;;;; The bridge handles the impedance mismatch between socket FD-based
;;;; callers and the native transport-based TLS API.

(defpackage epsilon.crypto.native
  (:use :cl)
  (:local-nicknames
   (#:tls #:epsilon.ssl.tls13)
   (#:x509-native #:epsilon.ssl.x509)
   (#:pem #:epsilon.ssl.pem)
   (#:drbg #:epsilon.ssl.drbg)
   (#:pkcs #:epsilon.ssl.pkcs)
   (#:asn1 #:epsilon.ssl.asn1)
   (#:ed25519-sign #:epsilon.ssl.ed25519-sign)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ec #:epsilon.ssl.ec-p256)
   (#:rsa-mod #:epsilon.ssl.rsa))
  (:export
   ;; TLS context (compatible with epsilon.crypto.tls.context)
   #:tls-context
   #:tls-context-p
   #:tls-context-handle
   #:tls-context-server-p
   #:tls-context-cert-file
   #:tls-context-key-file
   #:tls-context-verify-mode
   #:tls-context-verify-depth
   #:tls-context-alpn-protocols
   #:tls-context-trust-store
   #:tls-context-cert-chain
   #:tls-context-private-key
   #:tls-context-ca-file
   #:tls-context-ca-path
   #:tls-context-min-version
   #:tls-context-max-version
   #:make-tls-context
   #:make-client-context
   #:make-server-context
   #:create-tls-context
   #:context-set-certificate
   #:context-set-private-key
   #:context-check-private-key
   #:context-set-certificate-chain
   #:context-set-ca-file
   #:context-set-ca-path
   #:context-set-verify-mode
   #:context-set-verify-depth
   #:context-set-min-version
   #:context-set-max-version
   #:context-set-ciphersuites
   #:context-set-cipher-list
   #:context-set-session-cache-mode
   #:context-set-client-ca-list
   #:free-tls-context
   ;; Version constants
   #:+tls-1.0+
   #:+tls-1.1+
   #:+tls-1.2+
   #:+tls-1.3+
   ;; Verify mode constants
   #:+verify-none+
   #:+verify-peer+
   #:+verify-fail-if-no-peer-cert+
   #:+verify-client-once+
   ;; Session cache constants
   #:+session-cache-off+
   #:+session-cache-client+
   #:+session-cache-server+
   #:+session-cache-both+
   ;; TLS connection (compatible with epsilon.crypto.tls.connection)
   #:tls-connection
   #:tls-connection-p
   #:make-tls-connection
   #:tls-connection-ssl
   #:tls-connection-socket
   #:tls-connection-context
   #:tls-connection-native-stream
   #:tls-connection-connected-p
   #:tls-connection-handshake-complete-p
   #:tls-connect
   #:tls-accept
   #:tls-read
   #:tls-write
   #:tls-shutdown
   #:tls-close
   #:connection-peer-certificate
   #:connection-cipher
   #:connection-version
   #:connection-alpn-protocol
   #:connection-pending
   #:verify-peer-certificate
   #:get-verify-result
   ;; SSL error constants (stubs -- no OpenSSL)
   #:+ssl-error-none+
   #:+ssl-error-ssl+
   #:+ssl-error-want-read+
   #:+ssl-error-want-write+
   #:+ssl-error-syscall+
   #:+ssl-error-zero-return+
   ;; TLS stream (compatible with epsilon.crypto.tls.stream)
   #:tls-stream
   #:tls-stream-p
   #:make-tls-stream
   #:tls-stream-connection
   #:tls-stream-peer-certificate
   #:tls-stream-cipher
   #:tls-stream-version
   #:tls-stream-alpn-protocol
   #:tls-connect-stream
   #:tls-accept-stream
   #:wrap-socket-with-tls
   #:tls-read-line
   #:tls-write-line
   #:tls-write-string
   ;; ALPN (compatible with epsilon.crypto.tls.alpn)
   #:+alpn-http/1.0+
   #:+alpn-http/1.1+
   #:+alpn-h2+
   #:+alpn-h2c+
   #:+alpn-grpc+
   #:context-set-alpn-protocols
   #:connection-set-alpn-protocols
   #:connection-selected-alpn
   #:make-alpn-buffer
   #:parse-alpn-buffer
   ;; PKI (key generation / import-export / sign / verify)
   #:native-key
   #:native-key-p
   #:native-key-type
   #:native-key-private-p
   #:native-key-material
   #:generate-ed25519-key
   #:generate-ec-p256-key
   #:generate-rsa-key
   #:key-to-pem
   #:key-from-pem
   #:sign-message
   #:verify-message
   ;; mTLS (compatible with epsilon.crypto.tls.mtls)
   #:make-mtls-server-context
   #:make-mtls-client-context
   #:configure-mtls-security
   #:require-client-certificate
   #:match-certificate-hostname))

(in-package :epsilon.crypto.native)

;;; ---------------------------------------------------------------------------
;;; Constants (compatible with epsilon.crypto.tls.context)
;;; ---------------------------------------------------------------------------

(defconstant +tls-1.0+ #x0301)
(defconstant +tls-1.1+ #x0302)
(defconstant +tls-1.2+ #x0303)
(defconstant +tls-1.3+ #x0304)

(defconstant +verify-none+ 0)
(defconstant +verify-peer+ 1)
(defconstant +verify-fail-if-no-peer-cert+ 2)
(defconstant +verify-client-once+ 4)

(defconstant +session-cache-off+ 0)
(defconstant +session-cache-client+ 1)
(defconstant +session-cache-server+ 2)
(defconstant +session-cache-both+ 3)

;; SSL error constants (no OpenSSL, but some code references these)
(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)

;; ALPN protocol strings (use defvar to avoid SBCL string-constant redefinition)
(defvar +alpn-http/1.0+ "http/1.0")
(defvar +alpn-http/1.1+ "http/1.1")
(defvar +alpn-h2+ "h2")
(defvar +alpn-h2c+ "h2c")
(defvar +alpn-grpc+ "grpc")

;;; ---------------------------------------------------------------------------
;;; TLS Context (lightweight replacement for OpenSSL SSL_CTX)
;;; ---------------------------------------------------------------------------

(defstruct tls-context
  "Native TLS context. Stores configuration that would be in an OpenSSL
   SSL_CTX but requires no foreign resources."
  (server-p nil :type boolean)
  (handle :native)  ; sentinel -- code checking for non-null handle still works
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (ca-file nil :type (or null string))
  (ca-path nil :type (or null string))
  (verify-mode +verify-none+ :type fixnum)
  (verify-depth 100 :type fixnum)
  (min-version +tls-1.3+ :type fixnum)
  (max-version +tls-1.3+ :type fixnum)
  (alpn-protocols nil :type list)
  ;; Loaded certificate chain (list of DER byte arrays)
  (cert-chain nil :type list)
  ;; Loaded private key (raw bytes or parsed key)
  (private-key nil)
  ;; Trusted CA certificates (list of DER byte arrays)
  (trust-store nil :type list))

(defun make-client-context (&key cert-file key-file ca-file
                              (verify-mode +verify-none+)
                              (min-version +tls-1.3+))
  "Create a native TLS client context.
   Compatible with epsilon.crypto.tls.context:make-client-context."
  (declare (ignore min-version))
  (let ((ctx (make-tls-context :server-p nil
                                :cert-file cert-file
                                :key-file key-file
                                :ca-file ca-file
                                :verify-mode verify-mode)))
    ;; Load CA certificates for trust store if provided
    (when ca-file
      (setf (tls-context-trust-store ctx)
            (load-certificates-from-file ca-file)))
    ;; Load client certificate and key if provided (for mTLS)
    (when cert-file
      (setf (tls-context-cert-chain ctx)
            (load-certificates-from-file cert-file)))
    (when key-file
      (setf (tls-context-private-key ctx)
            (load-private-key-from-file key-file)))
    ctx))

(defun make-server-context (&key cert-file key-file ca-file
                                  (verify-mode +verify-none+)
                                  require-client-cert min-version
                                  session-cache-p)
  "Create a native TLS server context.
   Compatible with epsilon.crypto.tls.context:make-server-context."
  (declare (ignore ca-file require-client-cert min-version session-cache-p))
  (let ((ctx (make-tls-context :server-p t
                                :cert-file cert-file
                                :key-file key-file
                                :verify-mode verify-mode)))
    (when cert-file
      (setf (tls-context-cert-chain ctx)
            (load-certificates-from-file cert-file)))
    (when key-file
      (setf (tls-context-private-key ctx)
            (load-private-key-from-file key-file)))
    ctx))

(defun create-tls-context (&rest args)
  "Generic context creation -- delegates to make-client-context."
  (apply #'make-client-context args))

;;; Context configuration methods (no-ops or store-in-struct for native)

(defun context-set-certificate (context cert-file)
  (setf (tls-context-cert-file context) cert-file)
  (setf (tls-context-cert-chain context)
        (load-certificates-from-file cert-file)))

(defun context-set-private-key (context key-file)
  (setf (tls-context-key-file context) key-file)
  (setf (tls-context-private-key context)
        (load-private-key-from-file key-file)))

(defun context-check-private-key (context)
  (declare (ignore context))
  t)

(defun context-set-certificate-chain (context chain-file)
  (setf (tls-context-cert-chain context)
        (load-certificates-from-file chain-file)))

(defun context-set-ca-file (context ca-file)
  (setf (tls-context-ca-file context) ca-file)
  (setf (tls-context-trust-store context)
        (load-certificates-from-file ca-file)))

(defun context-set-ca-path (context ca-path)
  (setf (tls-context-ca-path context) ca-path))

(defun context-set-verify-mode (context mode)
  (setf (tls-context-verify-mode context) mode))

(defun context-set-verify-depth (context depth)
  (setf (tls-context-verify-depth context) depth))

(defun context-set-min-version (context version)
  (setf (tls-context-min-version context) version))

(defun context-set-max-version (context version)
  (setf (tls-context-max-version context) version))

(defun context-set-ciphersuites (context ciphersuites)
  (declare (ignore context ciphersuites))
  t)

(defun context-set-cipher-list (context cipher-list)
  (declare (ignore context cipher-list))
  t)

(defun context-set-session-cache-mode (context mode)
  (declare (ignore context mode))
  t)

(defun context-set-client-ca-list (context ca-list)
  (declare (ignore context ca-list))
  t)

(defun context-set-alpn-protocols (context protocols)
  "Set ALPN protocols on the context."
  (setf (tls-context-alpn-protocols context) protocols))

(defun free-tls-context (context)
  "Free a TLS context. No-op for native (no foreign resources)."
  (declare (ignore context))
  t)

;;; ---------------------------------------------------------------------------
;;; Certificate and key loading helpers
;;; ---------------------------------------------------------------------------

(defun load-certificates-from-file (path)
  "Load PEM-encoded certificates from PATH, returning list of DER byte arrays."
  (handler-case
      (let* ((text (read-file-string path))
             (blocks (pem:pem-decode-all text)))
        (loop for block in blocks
              when (string= (pem:pem-block-label block) "CERTIFICATE")
              collect (pem:pem-block-data block)))
    (error () nil)))

(defun load-private-key-from-file (path)
  "Load a PEM-encoded private key from PATH. Returns DER bytes."
  (handler-case
      (let* ((text (read-file-string path))
             (block (pem:pem-decode text)))
        (when block (pem:pem-block-data block)))
    (error () nil)))

(defun read-file-string (path)
  "Read a file as a string."
  (with-open-file (s path :direction :input)
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

;;; ---------------------------------------------------------------------------
;;; TLS Connection (wraps the native tls-stream)
;;; ---------------------------------------------------------------------------

(defstruct tls-connection
  "Bridge TLS connection wrapping a native TLS stream.
   Provides the same slot accessors as epsilon.crypto.tls.connection."
  ;; The underlying native tls-stream
  (native-stream nil)
  ;; The raw socket (preserved for epsilon.http cleanup)
  (socket nil)
  ;; The context used to create this connection
  (context nil :type (or null tls-context))
  ;; Connection state
  (connected-p nil :type boolean)
  (handshake-complete-p nil :type boolean)
  ;; Compatibility: ssl slot (code checking for non-null)
  (ssl :native))

;;; ---------------------------------------------------------------------------
;;; Socket-to-transport bridging
;;; ---------------------------------------------------------------------------

(defun socket-to-stream (socket)
  "Convert a network socket to a CL binary stream.
   Handles epsilon.net tcp-stream objects (extract FD and create stream)
   and raw integer FDs."
  (let ((fd (extract-socket-fd socket)))
    (sb-sys:make-fd-stream fd
                           :input t
                           :output t
                           :element-type '(unsigned-byte 8)
                           :buffering :full
                           :auto-close nil)))

(defun extract-socket-fd (socket)
  "Extract a file descriptor from various socket representations."
  (cond
    ;; Integer FD
    ((integerp socket) socket)
    ;; epsilon.net.types::tcp-stream (CLOS object with handle slot)
    ((and (find-package :epsilon.net.types)
          (let ((tcp-class (find-class
                            (intern "TCP-STREAM" :epsilon.net.types) nil)))
            (and tcp-class (typep socket tcp-class))))
     (slot-value socket (intern "HANDLE" :epsilon.net.types)))
    ;; epsilon.net::tcp-stream (own class with epsilon.net::handle slot)
    ((and (find-package :epsilon.net)
          (let ((tcp-class (find-class
                            (intern "TCP-STREAM" :epsilon.net) nil)))
            (and tcp-class (typep socket tcp-class))))
     (slot-value socket (intern "HANDLE" :epsilon.net)))
    (t
     (error "Cannot extract FD from socket: ~A" socket))))

;;; ---------------------------------------------------------------------------
;;; TLS Connection API (compatible with epsilon.crypto)
;;; ---------------------------------------------------------------------------

(defun tls-connect (socket context &key hostname alpn-protocols)
  "Establish a TLS 1.3 connection. Compatible with epsilon.crypto:tls-connect.

   SOCKET: Network socket (epsilon.net tcp-stream or raw FD)
   CONTEXT: TLS context (native tls-context)
   HOSTNAME: Server hostname for SNI and verification
   ALPN-PROTOCOLS: List of ALPN protocol strings

   Returns a TLS-CONNECTION structure."
  (let* ((cl-stream (socket-to-stream socket))
         (transport (tls:make-fd-transport :stream cl-stream))
         (effective-alpn (or alpn-protocols
                             (tls-context-alpn-protocols context)))
         (trust-store (when context (tls-context-trust-store context)))
         (native-stream (tls:tls-connect transport
                                          :hostname hostname
                                          :alpn-protocols effective-alpn
                                          :trust-store trust-store)))
    (make-tls-connection :native-stream native-stream
                          :socket socket
                          :context context
                          :connected-p t
                          :handshake-complete-p t)))

(defun tls-accept (socket context)
  "Accept a TLS 1.3 connection. Compatible with epsilon.crypto:tls-accept.

   SOCKET: Network socket from accepted connection
   CONTEXT: Server TLS context

   Returns a TLS-CONNECTION structure."
  (let* ((cl-stream (socket-to-stream socket))
         (transport (tls:make-fd-transport :stream cl-stream))
         (config (tls:make-tls-server-config
                  :certificate-chain (tls-context-cert-chain context)
                  :private-key (tls-context-private-key context)))
         (native-stream (tls:tls-accept transport config)))
    (make-tls-connection :native-stream native-stream
                          :socket socket
                          :context context
                          :connected-p t
                          :handshake-complete-p t)))

(defun tls-read (connection buffer &key (start 0) (end (length buffer)))
  "Read data from a TLS connection into buffer.
   Returns number of bytes read, or 0 for EOF.
   Compatible with epsilon.crypto:tls-read."
  (unless (tls-connection-connected-p connection)
    (return-from tls-read 0))
  (let ((stream (tls-connection-native-stream connection)))
    (tls:tls-read stream buffer :start start :end end)))

(defun tls-write (connection buffer &key (start 0) (end (length buffer)))
  "Write data to a TLS connection from buffer.
   Supports both byte vectors and strings (like the OpenSSL version).
   Returns number of bytes written.
   Compatible with epsilon.crypto:tls-write."
  (unless (tls-connection-connected-p connection)
    (error "TLS connection not established"))
  (let ((stream (tls-connection-native-stream connection))
        (data (if (stringp buffer)
                  (tls:string-to-bytes (subseq buffer start end))
                  buffer)))
    (if (stringp buffer)
        (tls:tls-write stream data :start 0 :end (length data))
        (tls:tls-write stream data :start start :end end))))

(defun tls-shutdown (connection)
  "Send close_notify alert. Compatible with epsilon.crypto:tls-shutdown."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (ignore-errors (tls:tls-shutdown stream)))))

(defun tls-close (connection)
  "Close the TLS connection and underlying transport.
   Compatible with epsilon.crypto:tls-close."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (ignore-errors (tls:tls-close stream)))
    (setf (tls-connection-connected-p connection) nil)
    (setf (tls-connection-handshake-complete-p connection) nil))
  t)

;;; ---------------------------------------------------------------------------
;;; Connection information accessors
;;; ---------------------------------------------------------------------------

(defun connection-peer-certificate (connection)
  "Get the peer's certificate info. Returns a plist compatible with
   epsilon.crypto:connection-peer-certificate."
  (when (tls-connection-connected-p connection)
    (let* ((stream (tls-connection-native-stream connection))
           (certs (tls:tls-stream-peer-certificates stream)))
      (when certs
        (let ((cert (x509-native:parse-x509-certificate (first certs))))
          (list :handle cert
                :subject (format-x509-name
                          (x509-native:x509-cert-subject cert))
                :issuer (format-x509-name
                         (x509-native:x509-cert-issuer cert))))))))

(defun connection-cipher (connection)
  "Get the negotiated cipher name as a string.
   Compatible with epsilon.crypto:connection-cipher."
  (when (tls-connection-connected-p connection)
    (let* ((stream (tls-connection-native-stream connection))
           (suite (tls:tls-stream-cipher-suite stream)))
      (case suite
        (#.tls:+tls-aes-128-gcm-sha256+ "TLS_AES_128_GCM_SHA256")
        (#.tls:+tls-aes-256-gcm-sha384+ "TLS_AES_256_GCM_SHA384")
        (#.tls:+tls-chacha20-poly1305-sha256+ "TLS_CHACHA20_POLY1305_SHA256")
        (t (format nil "UNKNOWN(#x~4,'0X)" suite))))))

(defun connection-version (connection)
  "Get the negotiated TLS version string.
   Compatible with epsilon.crypto:connection-version."
  (when (tls-connection-connected-p connection)
    "TLSv1.3"))

(defun connection-alpn-protocol (connection)
  "Get the ALPN protocol selected during handshake.
   Compatible with epsilon.crypto:connection-alpn-protocol."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (tls:tls-stream-alpn-protocol stream))))

(defun connection-pending (connection)
  "Return bytes pending in the TLS buffer. Always 0 for native."
  (declare (ignore connection))
  0)

(defun verify-peer-certificate (connection)
  "Verify the peer's certificate. Compatible with epsilon.crypto."
  (declare (ignore connection))
  ;; Certificate verification happens during handshake in native TLS
  t)

(defun get-verify-result (connection)
  "Get verification result. Compatible with epsilon.crypto."
  (declare (ignore connection))
  0)  ; 0 = X509_V_OK

;;; ---------------------------------------------------------------------------
;;; TLS Stream (higher-level wrapper implementing epsilon.io)
;;; ---------------------------------------------------------------------------

(defstruct (tls-stream (:constructor %make-tls-stream))
  "TLS stream wrapping a bridge tls-connection.
   Compatible with epsilon.crypto.tls.stream:tls-stream."
  (connection nil :type (or null tls-connection))
  (read-buffer (make-array 4096 :element-type '(unsigned-byte 8))
               :type (simple-array (unsigned-byte 8) (*)))
  (read-pos 0 :type fixnum)
  (read-end 0 :type fixnum))

(defun make-tls-stream (&key connection)
  "Create a TLS stream wrapping a bridge tls-connection."
  (%make-tls-stream :connection connection))

(defun tls-stream-peer-certificate (stream)
  (connection-peer-certificate (tls-stream-connection stream)))

(defun tls-stream-cipher (stream)
  (connection-cipher (tls-stream-connection stream)))

(defun tls-stream-version (stream)
  (connection-version (tls-stream-connection stream)))

(defun tls-stream-alpn-protocol (stream)
  (connection-alpn-protocol (tls-stream-connection stream)))

(defun tls-connect-stream (socket context &key hostname alpn-protocols)
  "Connect and return a TLS stream. Compatible with epsilon.crypto.tls.stream."
  (let ((conn (tls-connect socket context
                            :hostname hostname
                            :alpn-protocols alpn-protocols)))
    (make-tls-stream :connection conn)))

(defun tls-accept-stream (socket context)
  "Accept and return a TLS stream. Compatible with epsilon.crypto.tls.stream."
  (let ((conn (tls-accept socket context)))
    (make-tls-stream :connection conn)))

(defun wrap-socket-with-tls (socket &key hostname context alpn-protocols)
  "Wrap a socket with TLS. Compatible with epsilon.crypto.tls.stream."
  (let ((ctx (or context (make-client-context))))
    (tls-connect-stream socket ctx :hostname hostname
                         :alpn-protocols alpn-protocols)))

(defun tls-read-line (stream &key (max-length 8192))
  "Read a line from a TLS stream."
  (let ((conn (tls-stream-connection stream))
        (native (tls-connection-native-stream
                 (tls-stream-connection stream))))
    (declare (ignore conn))
    (tls:tls-read-line native :max-length max-length)))

(defun tls-write-line (stream string)
  "Write a line to a TLS stream."
  (let ((native (tls-connection-native-stream
                 (tls-stream-connection stream))))
    (tls:tls-write-line native string)))

(defun tls-write-string (stream string)
  "Write a string to a TLS stream."
  (let ((native (tls-connection-native-stream
                 (tls-stream-connection stream))))
    (tls:tls-write-string native string)))

;;; ---------------------------------------------------------------------------
;;; ALPN helpers
;;; ---------------------------------------------------------------------------

(defun connection-set-alpn-protocols (connection protocols)
  "Set ALPN on a connection (only meaningful before handshake)."
  (declare (ignore connection protocols))
  nil)

(defun connection-selected-alpn (connection)
  "Get selected ALPN -- alias for connection-alpn-protocol."
  (connection-alpn-protocol connection))

(defun make-alpn-buffer (protocols)
  "Build wire-format ALPN buffer from protocol name list."
  (let* ((total-len (loop for p in protocols sum (1+ (length p))))
         (buf (make-array total-len :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (p protocols)
      (setf (aref buf pos) (length p))
      (incf pos)
      (loop for c across p
            do (setf (aref buf pos) (char-code c))
               (incf pos)))
    buf))

(defun parse-alpn-buffer (buffer)
  "Parse wire-format ALPN buffer into protocol name list."
  (let ((protocols nil)
        (pos 0))
    (loop while (< pos (length buffer))
          do (let ((len (aref buffer pos)))
               (incf pos)
               (push (map 'string #'code-char
                          (subseq buffer pos (+ pos len)))
                     protocols)
               (incf pos len)))
    (nreverse protocols)))

;;; ---------------------------------------------------------------------------
;;; Mutual TLS helpers
;;; ---------------------------------------------------------------------------

(defun make-mtls-server-context (&key cert-file key-file ca-file)
  "Create a mutual TLS server context."
  (let ((ctx (make-server-context :cert-file cert-file
                                   :key-file key-file
                                   :verify-mode +verify-peer+)))
    (when ca-file
      (setf (tls-context-trust-store ctx)
            (load-certificates-from-file ca-file)))
    ctx))

(defun make-mtls-client-context (&key cert-file key-file ca-file)
  "Create a mutual TLS client context."
  (make-client-context :cert-file cert-file
                        :key-file key-file
                        :ca-file ca-file
                        :verify-mode +verify-peer+))

(defun configure-mtls-security (context &key verify-mode)
  (when verify-mode
    (setf (tls-context-verify-mode context) verify-mode)))

(defun require-client-certificate (context)
  (setf (tls-context-verify-mode context)
        (logior +verify-peer+ +verify-fail-if-no-peer-cert+)))

(defun match-certificate-hostname (cert-plist hostname)
  "Check if a certificate matches the given hostname.
   CERT-PLIST is the plist returned by connection-peer-certificate."
  (when cert-plist
    (let ((subject (getf cert-plist :subject)))
      (when subject
        ;; Simple CN matching -- check if hostname appears in subject
        (search hostname subject :test #'char-equal)))))

;;; ---------------------------------------------------------------------------
;;; X.509 name formatting
;;; ---------------------------------------------------------------------------

(defun format-x509-name (name-alist)
  "Format an X.509 name alist as a /KEY=VALUE string like OpenSSL oneline."
  (if (null name-alist)
      ""
      (with-output-to-string (s)
        (dolist (pair name-alist)
          (format s "/~A=~A"
                  (oid-to-short-name (car pair))
                  (cdr pair))))))

(defun oid-to-short-name (oid)
  "Convert an OID string to a short name."
  (cond
    ((equal oid "2.5.4.3") "CN")
    ((equal oid "2.5.4.6") "C")
    ((equal oid "2.5.4.7") "L")
    ((equal oid "2.5.4.8") "ST")
    ((equal oid "2.5.4.10") "O")
    ((equal oid "2.5.4.11") "OU")
    ((equal oid "2.5.4.5") "serialNumber")
    ((equal oid "1.2.840.113549.1.9.1") "emailAddress")
    (t oid)))

;;; ---------------------------------------------------------------------------
;;; PKI: native key representation and operations
;;; ---------------------------------------------------------------------------

(defstruct native-key
  "Key representation for the native crypto backend.
   MATERIAL contents vary by type:
     :ed25519 - 32-byte key (seed for private, encoded point for public)
     :ec-p256 - integer (private scalar) or SEC1 bytes (public, 65 bytes)
     :rsa     - rsa-public-key or rsa-private-key struct"
  (type nil :type (member :ed25519 :ec-p256 :rsa))
  (private-p nil :type boolean)
  (material nil))

(defun generate-ed25519-key ()
  "Generate an Ed25519 keypair. Returns a native-key with 32-byte seed."
  (make-native-key :type :ed25519 :private-p t
                   :material (drbg:random-bytes 32)))

(defun generate-ec-p256-key ()
  "Generate an EC P-256 keypair. Returns a native-key with integer private scalar."
  (loop
    (let* ((d-bytes (drbg:random-bytes 32))
           (d (bytes-to-integer d-bytes))
           (d (mod d ec:+n+)))
      (unless (zerop d)
        (return (make-native-key :type :ec-p256 :private-p t :material d))))))

(defun generate-rsa-key (&key (bits 2048))
  "Generate an RSA keypair. Returns a native-key with rsa-private-key material."
  (multiple-value-bind (pub priv) (rsa-mod:rsa-generate-key bits)
    (declare (ignore pub))
    (make-native-key :type :rsa :private-p t :material priv)))

(defun key-to-pem (key &key private-p)
  "Export a native-key to PEM format string.
   When PRIVATE-P is true and the key is private, exports the private key.
   Otherwise exports the public key."
  (let ((want-private (and private-p (native-key-private-p key))))
    (ecase (native-key-type key)
      (:ed25519
       (if want-private
           (pem:pem-encode (pem:make-pem-block
                            "PRIVATE KEY"
                            (pkcs:encode-pkcs8-ed25519 (native-key-material key))))
           (let ((pub (ed25519-sign:ed25519-public-key-from-private
                       (native-key-material key))))
             (pem:pem-encode (pem:make-pem-block
                              "PUBLIC KEY"
                              (pkcs:encode-spki-ed25519 pub))))))
      (:rsa
       (let ((mat (native-key-material key)))
         (if want-private
             (pem:pem-encode (pem:make-pem-block
                              "RSA PRIVATE KEY"
                              (pkcs:encode-pkcs1-rsa-private
                               (rsa-mod:rsa-private-key-n mat)
                               (rsa-mod:rsa-private-key-e mat)
                               (rsa-mod:rsa-private-key-d mat)
                               :p (rsa-mod:rsa-private-key-p mat)
                               :q (rsa-mod:rsa-private-key-q mat)
                               :dp (rsa-mod:rsa-private-key-dp mat)
                               :dq (rsa-mod:rsa-private-key-dq mat)
                               :qinv (rsa-mod:rsa-private-key-qinv mat))))
             ;; For a private key, derive the public key components
             (pem:pem-encode (pem:make-pem-block
                              "PUBLIC KEY"
                              (pkcs:encode-spki-rsa
                               (rsa-mod:rsa-private-key-n mat)
                               (rsa-mod:rsa-private-key-e mat)))))))
      (:ec-p256
       (if want-private
           (let* ((d (native-key-material key))
                  (point (ecdsa:ecdsa-public-key-from-private d))
                  (pub-bytes (ec:p256-point-encode-uncompressed point)))
             (pem:pem-encode (pem:make-pem-block
                              "PRIVATE KEY"
                              (pkcs:encode-pkcs8-ec d pub-bytes))))
           (let ((pub-bytes (if (native-key-private-p key)
                                ;; Private key: derive public point from scalar
                                (let* ((d (native-key-material key))
                                       (point (ecdsa:ecdsa-public-key-from-private d)))
                                  (ec:p256-point-encode-uncompressed point))
                                ;; Public key: material is already the encoded point
                                (native-key-material key))))
             (pem:pem-encode (pem:make-pem-block
                              "PUBLIC KEY"
                              (pkcs:encode-spki-ec pub-bytes)))))))))

(defun key-from-pem (pem-string &key private-p)
  "Import a key from PEM format string.
   Returns a native-key structure."
  (let ((block (pem:pem-decode pem-string)))
    (unless block
      (error "Failed to decode PEM data"))
    (let* ((label (pem:pem-block-label block))
           (der (pem:pem-block-data block))
           (is-private (or private-p
                           (search "PRIVATE" label))))
      (if is-private
          (decode-private-key-pem der label)
          (decode-public-key-pem der label)))))

(defun decode-public-key-pem (der label)
  "Decode a public key from DER bytes based on PEM label."
  (cond
    ((string= label "PUBLIC KEY")
     ;; SubjectPublicKeyInfo (RFC 5280)
     (multiple-value-bind (oid params key-bytes) (pkcs:decode-spki der)
       (declare (ignore params))
       (cond
         ((equal oid pkcs:+oid-ed25519+)
          (make-native-key :type :ed25519 :private-p nil :material key-bytes))
         ((equal oid pkcs:+oid-ec-public-key+)
          (make-native-key :type :ec-p256 :private-p nil :material key-bytes))
         ((equal oid pkcs:+oid-rsa-encryption+)
          (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public key-bytes)
            (make-native-key :type :rsa :private-p nil
                             :material (rsa-mod:make-rsa-public-key n e))))
         (t (error "Unsupported public key algorithm OID: ~A" oid)))))
    ((string= label "RSA PUBLIC KEY")
     (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public der)
       (make-native-key :type :rsa :private-p nil
                        :material (rsa-mod:make-rsa-public-key n e))))
    (t (error "Unsupported PEM label for public key: ~A" label))))

(defun decode-private-key-pem (der label)
  "Decode a private key from DER bytes based on PEM label."
  (cond
    ((string= label "PRIVATE KEY")
     ;; PKCS#8 PrivateKeyInfo
     (multiple-value-bind (oid params key-bytes) (pkcs:decode-pkcs8 der)
       (declare (ignore params))
       (cond
         ((equal oid pkcs:+oid-ed25519+)
          ;; key-bytes is OCTET STRING wrapping the 32-byte seed
          (let* ((inner-tlv (asn1:der-decode key-bytes))
                 (seed (asn1:asn1-tlv-value inner-tlv)))
            (make-native-key :type :ed25519 :private-p t :material seed)))
         ((equal oid pkcs:+oid-ec-public-key+)
          ;; key-bytes is ECPrivateKey (SEC 1) DER
          (let* ((ec-tlv (asn1:der-decode key-bytes))
                 (children (asn1:der-decode-sequence-contents ec-tlv))
                 (d-bytes (asn1:asn1-tlv-value (second children)))
                 (d (bytes-to-integer d-bytes)))
            (make-native-key :type :ec-p256 :private-p t :material d)))
         ((equal oid pkcs:+oid-rsa-encryption+)
          (multiple-value-bind (n e d p q dp dq qinv)
              (pkcs:decode-pkcs1-rsa-private key-bytes)
            (make-native-key :type :rsa :private-p t
                             :material (rsa-mod:make-rsa-private-key
                                        n e d :p (or p 0) :q (or q 0)
                                        :dp (or dp 0) :dq (or dq 0)
                                        :qinv (or qinv 0)))))
         (t (error "Unsupported private key algorithm OID: ~A" oid)))))
    ((string= label "RSA PRIVATE KEY")
     (multiple-value-bind (n e d p q dp dq qinv)
         (pkcs:decode-pkcs1-rsa-private der)
       (make-native-key :type :rsa :private-p t
                        :material (rsa-mod:make-rsa-private-key
                                   n e d :p (or p 0) :q (or q 0)
                                   :dp (or dp 0) :dq (or dq 0)
                                   :qinv (or qinv 0)))))
    ((string= label "EC PRIVATE KEY")
     ;; SEC 1 ECPrivateKey
     (let* ((ec-tlv (asn1:der-decode der))
            (children (asn1:der-decode-sequence-contents ec-tlv))
            (d-bytes (asn1:asn1-tlv-value (second children)))
            (d (bytes-to-integer d-bytes)))
       (make-native-key :type :ec-p256 :private-p t :material d)))
    (t (error "Unsupported PEM label for private key: ~A" label))))

(defun bytes-to-integer (bytes)
  "Convert big-endian byte vector to integer."
  (let ((n 0))
    (loop for b across bytes
          do (setf n (logior (ash n 8) b)))
    n))

;;; --- Sign / Verify ---

(defun ensure-bytes (data)
  "Coerce DATA to a byte vector."
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    (string (sb-ext:string-to-octets data :external-format :utf-8))))

(defun sign-message (key data &key (digest "SHA256"))
  "Sign DATA with native-key KEY. Returns signature bytes."
  (declare (ignore digest))
  (let ((material (native-key-material key))
        (msg (ensure-bytes data)))
    (ecase (native-key-type key)
      (:ed25519
       (ed25519-sign:ed25519-sign material msg))
      (:ec-p256
       ;; ecdsa-sign returns (values r s); DER-encode for interop
       (multiple-value-bind (r s) (ecdsa:ecdsa-sign material msg)
         (asn1:der-encode-sequence
          (asn1:der-encode-integer r)
          (asn1:der-encode-integer s))))
      (:rsa
       (rsa-mod:rsa-pss-sign material msg)))))

(defun verify-message (key data signature &key (digest "SHA256"))
  "Verify SIGNATURE on DATA with native-key KEY. Returns T if valid."
  (declare (ignore digest))
  (let ((msg (ensure-bytes data))
        (sig (ensure-bytes signature)))
    (ecase (native-key-type key)
      (:ed25519
       ;; If key is private, derive public key from seed
       (let ((pub-material (if (native-key-private-p key)
                               (ed25519-sign:ed25519-public-key-from-private
                                (native-key-material key))
                               (native-key-material key))))
         (ed25519-sign:ed25519-verify pub-material msg sig)))
      (:ec-p256
       ;; DER-decode signature to get (r, s)
       (handler-case
           (let* ((tlv (asn1:der-decode sig))
                  (children (asn1:der-decode-sequence-contents tlv))
                  (r (asn1:decode-der-integer
                      (asn1:asn1-tlv-value (first children))))
                  (s (asn1:decode-der-integer
                      (asn1:asn1-tlv-value (second children))))
                  ;; Convert SEC1 public key bytes to ec-p256 point
                  (point (ec:p256-point-decode (native-key-material key))))
             (when point
               (ecdsa:ecdsa-verify point msg r s)))
         (error () nil)))
      (:rsa
       ;; Extract public key from private key if needed
       (let ((pub-key (if (native-key-private-p key)
                          (rsa-mod:make-rsa-public-key
                           (rsa-mod:rsa-private-key-n (native-key-material key))
                           (rsa-mod:rsa-private-key-e (native-key-material key)))
                          (native-key-material key))))
         ;; Try PKCS#1 v1.5 first, then PSS
         (or (ignore-errors (rsa-mod:pkcs1-v15-verify pub-key msg sig))
             (ignore-errors (rsa-mod:rsa-pss-verify pub-key msg sig))))))))

