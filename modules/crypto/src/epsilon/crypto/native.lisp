;;;; epsilon.crypto.native -- Native TLS bridge to epsilon.crypto
;;;;
;;;; Provides TLS context/connection/stream structs and functions backed
;;;; entirely by the pure-Lisp TLS 1.3 implementation.
;;;;
;;;; The bridge handles the impedance mismatch between socket FD-based
;;;; callers and the native transport-based TLS API.

(defpackage epsilon.crypto.native
  (:use :cl)
  (:import
   (epsilon.crypto.tls13 tls)
   (epsilon.crypto.tls12 tls12)
   (epsilon.crypto.tls-session-ticket-store stek)
   (epsilon.crypto.x509 x509-native)
   (epsilon.crypto.pem pem)
   (epsilon.crypto.pem-enc pem-enc)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.sha256 sha256-mod)
   (epsilon.crypto.ed25519-sign ed25519-sign)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ec-p256 ec)
   (epsilon.crypto.rsa rsa-mod)
   (epsilon.net net)
   (epsilon.io.protocol io-proto)
   (epsilon.typeclass tc)
   (epsilon.foreign lib)
   (epsilon.scheduler.fiber-io fio))
  (:export
   ;; TLS context (compatible with epsilon.crypto.tls.context)
   #:tls-context
   #:tls-context-p
   #:tls-context-server-p
   #:tls-context-verify-mode
   #:tls-context-alpn-protocols
   #:tls-context-trust-store
   #:tls-context-cert-chain
   #:tls-context-private-key
   #:tls-context-ca-file
   #:tls-context-min-version
   #:tls-context-max-version
   #:make-tls-context
   #:make-client-context
   #:make-server-context
   #:create-tls-context
   #:load-certificates-from-file
   #:load-private-key-from-file
   #:context-set-identity
   #:context-set-certificate
   #:context-set-private-key
   #:context-set-certificate-chain
   #:context-set-ca-file
   #:context-set-verify-mode
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
   #:tls-connection-tls13-stream
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
   #:connection-negotiated-group
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
   #:context-enable-session-tickets
   #:tls-context-session-ticket-store
   #:tls-context-session-ticket-lifetime
   #:tls-context-ocsp-staple
   #:connection-set-alpn-protocols
   #:connection-selected-alpn
   #:make-alpn-buffer
   #:parse-alpn-buffer
   ;; PKI (key generation / import-export / sign / verify)
   #:native-key
   #:native-key-p
   #:make-native-key
   #:native-key-type
   #:native-key-private-p
   #:native-key-material
   #:generate-ed25519-key
   #:generate-ec-p256-key
   #:generate-rsa-key
   #:key-to-pem
   #:key-from-pem
   #:key-from-der
   #:key-to-der
   #:sign-message
   #:verify-message
   ;; mTLS (compatible with epsilon.crypto.tls.mtls)
   #:make-mtls-server-context
   #:make-mtls-client-context
   #:configure-mtls-security
   #:require-client-certificate
   #:match-certificate-hostname
   ;; Async TCP transport for fiber-scheduled TLS
   #:async-tcp-transport
   #:async-tcp-transport-p
   #:make-async-socket-transport
   #:tls-async-timeout
   #:tls-async-timeout-elapsed))

(in-package :epsilon.crypto.native)

;;; ---------------------------------------------------------------------------
;;; PKI: native key representation (defined early so accessors are inlinable)
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
  "Native TLS context. Stores configuration for the pure-Lisp TLS stack."
  (server-p nil :type boolean)
  (ca-file nil :type (or null string))
  (verify-mode +verify-none+ :type fixnum)
  (min-version +tls-1.3+ :type fixnum)
  (max-version +tls-1.3+ :type fixnum)
  (alpn-protocols nil :type list)
  ;; Optional STEK store. When set on a server context, the native TLS
  ;; server will issue NewSessionTicket messages sealed with this store.
  (session-ticket-store nil)
  ;; Lifetime (seconds) advertised in issued tickets; 24h default.
  (session-ticket-lifetime 86400 :type integer)
  ;; Latest OCSP staple bytes (inner BasicOCSPResponse DER). Updated
  ;; by the proxy's OCSP fetcher thread; read on each tls-accept and
  ;; forwarded into the per-connection tls-server-config. NIL disables
  ;; stapling for this context.
  (ocsp-staple nil)
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
                                  require-client-cert
                                  request-client-cert
                                  (min-version +tls-1.2+)
                                  session-cache-p)
  "Create a native TLS server context.
   Compatible with epsilon.crypto.tls.context:make-server-context.
   Signals an error if cert-file or key-file is specified but fails to load.
   MIN-VERSION defaults to :tls-1.2 so the server accepts both TLS 1.2
   and TLS 1.3 clients (the version dispatch in tls-accept routes each
   to the correct handshake implementation).

   Client-cert mode (mutually exclusive; both default to nil):
     :require-client-cert t -- send CertificateRequest and fail the
       handshake if the client presents nothing or fails verification.
     :request-client-cert t -- send CertificateRequest but accept
       handshakes where the client presents nothing or an
       unverifiable cert; the cert (if any) reaches the request via
       crypto:connection-peer-certificate. Use this when the
       application authorises off the cert fingerprint rather than
       the chain (e.g. allowlist matching)."
  (declare (ignore ca-file session-cache-p))
  (let* ((effective-mode
          (cond (require-client-cert
                 (logior +verify-peer+ +verify-fail-if-no-peer-cert+))
                (request-client-cert
                 +verify-peer+)
                (t verify-mode)))
         (ctx (make-tls-context :server-p t
                                :verify-mode effective-mode)))
    (when cert-file
      (let ((certs (load-certificates-from-file cert-file)))
        (unless certs
          (error "TLS certificate failed to load from ~A" cert-file))
        (setf (tls-context-cert-chain ctx) certs)))
    (when key-file
      (let ((key (load-private-key-from-file key-file)))
        (unless key
          (error "TLS private key failed to load from ~A" key-file))
        (setf (tls-context-private-key ctx) key)))
    ;; Wire min-version so version dispatch in tls-accept honors it.
    (when min-version
      (context-set-min-version ctx min-version))
    ctx))

(defun create-tls-context (&rest args)
  "Generic context creation -- delegates to make-client-context."
  (apply #'make-client-context args))

;;; Context configuration methods (no-ops or store-in-struct for native)

(defun context-set-certificate (context cert-file)
  (setf (tls-context-cert-chain context)
        (load-certificates-from-file cert-file)))

(defun context-set-private-key (context key-file)
  (setf (tls-context-private-key context)
        (load-private-key-from-file key-file)))

(defun context-set-identity (context cert-chain private-key)
  "Atomically swap the certificate chain and private key on CONTEXT.
   CERT-CHAIN is a list of DER byte arrays. PRIVATE-KEY is raw key bytes.
   The displaced private key is zeroed before being released."
  (let ((old-key (tls-context-private-key context)))
    ;; Atomic swap: set both before any handshake can see a mismatch
    (setf (tls-context-cert-chain context) cert-chain
          (tls-context-private-key context) private-key)
    ;; Zero the old key material
    (when (and old-key (typep old-key '(simple-array (unsigned-byte 8) (*))))
      (epsilon.crypto.ct:ct-zero-memory old-key))))

(defun context-set-certificate-chain (context chain-file)
  (setf (tls-context-cert-chain context)
        (load-certificates-from-file chain-file)))

(defun context-set-ca-file (context ca-file)
  (setf (tls-context-ca-file context) ca-file)
  (setf (tls-context-trust-store context)
        (load-certificates-from-file ca-file)))

(defun context-set-verify-mode (context mode)
  (setf (tls-context-verify-mode context) mode))

(defun %normalize-tls-version (v)
  "Normalize a TLS version keyword or integer to the wire constant."
  (etypecase v
    (keyword (ecase v
               ((:tls-1.0 :tls1_0) +tls-1.0+)
               ((:tls-1.1 :tls1_1) +tls-1.1+)
               ((:tls-1.2 :tls1_2) +tls-1.2+)
               ((:tls-1.3 :tls1_3) +tls-1.3+)))
    (integer v)))

(defun context-set-min-version (context version)
  (setf (tls-context-min-version context) (%normalize-tls-version version)))

(defun context-set-max-version (context version)
  (setf (tls-context-max-version context) (%normalize-tls-version version)))

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

(defun context-enable-session-tickets (context &key store (lifetime 86400))
  "Enable server-side TLS 1.3 session ticket issuance on CONTEXT. If
   STORE is NIL, a fresh STEK store is created. Returns the store so
   the caller can drive rotation."
  (let ((s (or store (stek:make-stek-store))))
    (setf (tls-context-session-ticket-store context) s)
    (setf (tls-context-session-ticket-lifetime context) lifetime)
    s))

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
;;; TCP transport -- uses epsilon.net syscall wrappers (IMPL-340)
;;;
;;; Implements the TLS transport protocol on top of net:tcp-read /
;;; net:tcp-write, which call recv(2) / send(2) directly. No CL stream
;;; layer, no SBCL fd-stream ibuf/obuf -- just syscalls. Also handles
;;; EAGAIN internally via epoll, so O_NONBLOCK sockets work correctly.
;;;
;;; Also implements epsilon.io Reader/Writer/Closer protocols so that
;;; TLS streams compose with epsilon.io abstractions.
;;; ---------------------------------------------------------------------------

(defstruct tcp-transport
  (stream nil)
  (open-p t :type boolean))

(defmethod tls:tls-transport-read ((transport tcp-transport) buffer start end)
  (unless (tcp-transport-open-p transport)
    (return-from tls:tls-transport-read 0))
  (net:tcp-read (tcp-transport-stream transport) buffer
                :start start :end end))

(defmethod tls:tls-transport-write ((transport tcp-transport) buffer start end)
  (unless (tcp-transport-open-p transport)
    (error "Transport closed"))
  (net:tcp-write (tcp-transport-stream transport) buffer
                 :start start :end end))

(defmethod tls:tls-transport-close ((transport tcp-transport))
  (setf (tcp-transport-open-p transport) nil))

(tc:definstance io-proto:closer tcp-transport
  (io-proto:close* (transport)
    (tls:tls-transport-close transport)
    t)
  (io-proto:open-p (transport)
    (tcp-transport-open-p transport)))

(tc:definstance io-proto:reader tcp-transport
  (io-proto:read-into (transport buffer &key (start 0) (end (length buffer)))
    (tls:tls-transport-read transport buffer start end)))

(tc:definstance io-proto:writer tcp-transport
  (io-proto:write-from (transport buffer &key (start 0) (end (length buffer)))
    (tls:tls-transport-write transport buffer start end))
  (io-proto:flush (transport)
    transport))

(defun make-socket-transport (socket)
  "Create a tcp-transport from an epsilon.net tcp-stream.
   Clears O_NONBLOCK so recv()/send() block naturally -- the TLS record
   layer expects blocking semantics."
  (let ((fd (net:tcp-stream-handle socket)))
    (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
      (when (not (zerop (logand flags sb-posix:o-nonblock)))
        (sb-posix:fcntl fd sb-posix:f-setfl
                        (logandc2 flags sb-posix:o-nonblock)))))
  (make-tcp-transport :stream socket))

;;; ---------------------------------------------------------------------------
;;; Async TCP transport -- non-blocking recv/send that parks the carrying
;;; fiber via epsilon.scheduler.fiber-io (IMPL-351).
;;;
;;; Unlike tcp-transport (which relies on net:tcp-read's internal epoll
;;; wait, blocking the calling OS thread), async-tcp-transport keeps the
;;; socket in O_NONBLOCK and dispatches every read/write through
;;; FIBER-READ / FIBER-WRITE.  Those primitives yield the fiber when the
;;; underlying syscall returns EAGAIN; the carrier remains free to run
;;; other coroutines while we wait for fd readiness.
;;;
;;; The deadline is an internal-real-time absolute timestamp (or NIL for
;;; no deadline).  Each read/write passes the remaining seconds as
;;; FIBER-READ's :TIMEOUT; FIBER-IO-TIMEOUT signalled out of the
;;; underlying loop is caught here and re-raised as TLS-ASYNC-TIMEOUT.
;;; ---------------------------------------------------------------------------

(define-condition tls-async-timeout (error)
  ((elapsed :initarg :elapsed :reader tls-async-timeout-elapsed))
  (:report (lambda (c s)
             (format s "TLS async transport I/O deadline exceeded after ~,1Fs"
                     (tls-async-timeout-elapsed c)))))

(defstruct async-tcp-transport
  "Non-blocking TCP transport that parks the fiber on EAGAIN via the
   scheduler's reactor.  See comment block above for semantics."
  (stream nil)
  (fd 0 :type fixnum)
  (open-p t :type boolean)
  (deadline nil)
  (start-time 0 :type fixnum))

(defun %async-remaining-seconds (transport)
  "Seconds remaining until DEADLINE, or NIL if no deadline."
  (let ((deadline (async-tcp-transport-deadline transport)))
    (when deadline
      (max 0
           (/ (- deadline (get-internal-real-time))
              (float internal-time-units-per-second))))))

(defun %async-elapsed (transport)
  (/ (- (get-internal-real-time)
        (async-tcp-transport-start-time transport))
     (float internal-time-units-per-second)))

(defmethod tls:tls-transport-read ((transport async-tcp-transport) buffer start end)
  (unless (async-tcp-transport-open-p transport)
    (return-from tls:tls-transport-read 0))
  (handler-case
      (let ((n (fio:fiber-read (async-tcp-transport-fd transport) buffer
                               :start start :end end
                               :timeout (%async-remaining-seconds transport))))
        (when (zerop n)
          ;; EOF -- peer closed the connection.
          (setf (async-tcp-transport-open-p transport) nil))
        n)
    (fio:fiber-io-timeout ()
      (error 'tls-async-timeout :elapsed (%async-elapsed transport)))
    (fio:fiber-io-error (e)
      (declare (ignore e))
      (setf (async-tcp-transport-open-p transport) nil)
      (error "async-tcp-transport read failed"))))

(defmethod tls:tls-transport-write ((transport async-tcp-transport) buffer start end)
  (unless (async-tcp-transport-open-p transport)
    (error "async-tcp-transport closed"))
  (handler-case
      (fio:fiber-write (async-tcp-transport-fd transport) buffer
                       :start start :end end
                       :timeout (%async-remaining-seconds transport))
    (fio:fiber-io-timeout ()
      (error 'tls-async-timeout :elapsed (%async-elapsed transport)))
    (fio:fiber-io-error (e)
      (declare (ignore e))
      (setf (async-tcp-transport-open-p transport) nil)
      (error "async-tcp-transport write failed"))))

(defmethod tls:tls-transport-close ((transport async-tcp-transport))
  (setf (async-tcp-transport-open-p transport) nil))

(defun make-async-socket-transport (socket &key timeout)
  "Create an async-tcp-transport from an epsilon.net tcp-stream.  Sets
   the socket to O_NONBLOCK and (if TIMEOUT is supplied, in seconds)
   records an absolute deadline for all subsequent I/O.

   Must be used from inside a coroutine running under epsilon.scheduler,
   since any read or write that would block parks the fiber on the
   scheduler's reactor."
  (let* ((fd (net:tcp-stream-handle socket))
         (flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (when (zerop (logand flags sb-posix:o-nonblock))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior flags sb-posix:o-nonblock)))
    (let ((now (get-internal-real-time)))
      (make-async-tcp-transport
       :stream socket
       :fd fd
       :start-time now
       :deadline (when timeout
                   (+ now (ceiling (* timeout internal-time-units-per-second))))))))

;;; ---------------------------------------------------------------------------
;;; TLS Connection API (compatible with epsilon.crypto)
;;; ---------------------------------------------------------------------------

(defun tls-connect (socket context &key hostname alpn-protocols transport)
  "Establish a TLS 1.3 connection. Compatible with epsilon.crypto:tls-connect.

   SOCKET: Network socket (epsilon.net tcp-stream or raw FD)
   CONTEXT: TLS context (native tls-context)
   HOSTNAME: Server hostname for SNI and verification
   ALPN-PROTOCOLS: List of ALPN protocol strings
   TRANSPORT: Optional TLS transport overriding the default blocking
   transport. Pass an ASYNC-TCP-TRANSPORT (see MAKE-ASYNC-SOCKET-TRANSPORT)
   to drive the handshake under the scheduler's fiber-based reactor
   so that a slow TLS server doesn't block the carrier thread.

   Returns a TLS-CONNECTION structure."
  (let* ((transport (or transport (make-socket-transport socket)))
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

(defun detect-key-type-from-der (der)
  "Detect private key type from DER-encoded key bytes.
   Returns :ecdsa-p256, :rsa, or :ed25519."
  (let ((content-offset (if (and (> (length der) 1)
                                (= (aref der 0) #x30)
                                (= (logand (aref der 1) #x80) #x80))
                            ;; Multi-byte length: 30 81 xx or 30 82 xx xx
                            (+ 2 (logand (aref der 1) #x7f))
                            ;; Single-byte length: 30 xx
                            2)))
  (cond
    ;; SEC 1 ECPrivateKey: SEQUENCE { INTEGER(1), OCTET STRING, ... }
    ;; Content starts at content-offset: 02 01 01 04 ...
    ((and (> (length der) (+ content-offset 4))
          (= (aref der 0) #x30)                        ; SEQUENCE
          (= (aref der content-offset) #x02)            ; INTEGER tag
          (= (aref der (+ content-offset 1)) #x01)     ; length 1
          (= (aref der (+ content-offset 2)) #x01)     ; version 1
          (= (aref der (+ content-offset 3)) #x04))    ; OCTET STRING tag
     :ecdsa-p256)
    ;; Both PKCS#1 RSAPrivateKey and PKCS#8 PrivateKeyInfo start with
    ;; SEQUENCE { INTEGER(0), ... }. They diverge at the second element:
    ;;   PKCS#1 RSA:   next is INTEGER (the modulus n)    -- tag 0x02
    ;;   PKCS#8:       next is SEQUENCE (AlgorithmIdentifier) -- tag 0x30
    ((and (> (length der) (+ content-offset 3))
          (= (aref der 0) #x30)                        ; outer SEQUENCE
          (= (aref der content-offset) #x02)            ; version INTEGER
          (= (aref der (+ content-offset 1)) #x01)     ; length 1
          (= (aref der (+ content-offset 2)) #x00)     ; version 0
          (= (aref der (+ content-offset 3)) #x02))    ; next element is INTEGER
     ;; PKCS#1 bare RSAPrivateKey
     :rsa)
    ;; PKCS#8: SEQUENCE { INTEGER(0), SEQUENCE { OID, ... }, OCTET STRING }
    ((and (> (length der) (+ content-offset 6))
          (= (aref der 0) #x30)                        ; outer SEQUENCE
          (= (aref der content-offset) #x02)            ; version INTEGER
          (= (aref der (+ content-offset 1)) #x01)     ; length 1
          (= (aref der (+ content-offset 2)) #x00))    ; version 0 (PKCS#8)
     ;; Walk into AlgorithmIdentifier SEQUENCE to find OID
     (let ((alg-seq (+ content-offset 3)))
       (if (and (> (length der) alg-seq)
                (= (aref der alg-seq) #x30))             ; AlgorithmIdentifier SEQUENCE
           ;; Skip AlgorithmIdentifier header: tag + length (short or long form)
           (let* ((alg-len-byte (aref der (+ alg-seq 1)))
                  (oid-start (if (zerop (logand alg-len-byte #x80))
                                 (+ alg-seq 2)  ; short form
                                 (+ alg-seq 2 (logand alg-len-byte #x7f))))) ; long form
             (if (and (> (length der) (+ oid-start 10))
                      (= (aref der oid-start) #x06))    ; OID tag
                 ;; OID 1.2.840.113549.1.1.1 = RSA: 06 09 2a 86 48 86 f7 0d 01 01 01
                 (cond
                   ((and (= (aref der (+ oid-start 1)) #x09)
                         (= (aref der (+ oid-start 2)) #x2a)
                         (= (aref der (+ oid-start 3)) #x86)
                         (= (aref der (+ oid-start 4)) #x48)
                         (= (aref der (+ oid-start 5)) #x86)
                         (= (aref der (+ oid-start 6)) #xf7)
                         (= (aref der (+ oid-start 7)) #x0d)
                         (= (aref der (+ oid-start 8)) #x01)
                         (= (aref der (+ oid-start 9)) #x01)
                         (= (aref der (+ oid-start 10)) #x01))
                    :rsa)
                   ;; OID 1.2.840.10045.2.1 = EC public key: 06 07 2a 86 48 ce 3d 02 01
                   ((and (= (aref der (+ oid-start 1)) #x07)
                         (= (aref der (+ oid-start 2)) #x2a)
                         (= (aref der (+ oid-start 3)) #x86)
                         (= (aref der (+ oid-start 4)) #x48)
                         (= (aref der (+ oid-start 5)) #xce)
                         (= (aref der (+ oid-start 6)) #x3d)
                         (= (aref der (+ oid-start 7)) #x02)
                         (= (aref der (+ oid-start 8)) #x01))
                    :ec-p256)
                   ;; OID 1.3.101.112 = Ed25519: 06 03 2b 65 70
                   ((and (= (aref der (+ oid-start 1)) #x03)
                         (= (aref der (+ oid-start 2)) #x2b)
                         (= (aref der (+ oid-start 3)) #x65)
                         (= (aref der (+ oid-start 4)) #x70))
                    :ed25519)
                   (t :unknown))
                 :unknown))
           :unknown)))
    ;; 32 bytes -> Ed25519 seed
    ((= (length der) 32) :ed25519)
    ;; Default: refuse to guess
    (t :unknown))))

(defun extract-ec-private-key (der)
  "Extract private key integer from SEC 1 ECPrivateKey DER.
   Handles both single-byte and multi-byte SEQUENCE length encoding.
   Returns an integer suitable for ecdsa-sign."
  (let* ((content-offset (if (and (> (length der) 1)
                                  (= (aref der 0) #x30)
                                  (= (logand (aref der 1) #x80) #x80))
                             (+ 2 (logand (aref der 1) #x7f))
                             2))
         ;; Skip version INTEGER(1): 02 01 01
         ;; Then OCTET STRING: 04 20 <32 bytes>
         (octet-offset (+ content-offset 3))
         (key-bytes (if (and (> (length der) (+ octet-offset 34))
                             (= (aref der octet-offset) #x04)       ; OCTET STRING tag
                             (= (aref der (+ octet-offset 1)) #x20)) ; length 32
                        (subseq der (+ octet-offset 2) (+ octet-offset 34))
                        der)))
    (bytes-to-integer key-bytes)))

;;; ---------------------------------------------------------------------------
;;; Version-sniffing accept
;;;
;;; A single entry point that peeks the client's first record, parses
;;; the ClientHello, and dispatches to the TLS 1.3 or TLS 1.2 server
;;; path based on the supported_versions extension. Respects the
;;; context's min-version: a TLS 1.3-only context rejects TLS 1.2
;;; ClientHellos with a fatal protocol_version alert; a context that
;;; allows both accepts either.
;;; ---------------------------------------------------------------------------

(defstruct replay-transport
  "Wraps an underlying transport and serves BUFFERED bytes from a
   pre-read chunk before reading from the real transport. Used so that
   we can peek a ClientHello to pick a version dispatch target and
   then hand the same bytes to the chosen handshake implementation."
  (underlying nil)
  (buffered (make-array 0 :element-type '(unsigned-byte 8))
            :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum))

(defmethod tls:tls-transport-read ((transport replay-transport) buffer start end)
  (let* ((buf (replay-transport-buffered transport))
         (pos (replay-transport-pos transport))
         (avail (- (length buf) pos)))
    (cond
      ((plusp avail)
       (let ((n (min avail (- end start))))
         (replace buffer buf :start1 start :end1 (+ start n) :start2 pos)
         (setf (replay-transport-pos transport) (+ pos n))
         n))
      (t
       (tls:tls-transport-read (replay-transport-underlying transport)
                               buffer start end)))))

(defmethod tls:tls-transport-write ((transport replay-transport) buffer start end)
  (tls:tls-transport-write (replay-transport-underlying transport)
                           buffer start end))

(defmethod tls:tls-transport-close ((transport replay-transport))
  (tls:tls-transport-close (replay-transport-underlying transport)))

(defun %version-allows-1.2-p (context)
  "True when the context's min-version keyword permits TLS 1.2."
  (let ((mv (tls-context-min-version context)))
    (or (eql mv :tls-1.2) (eql mv :tls1_2) (eql mv :tls-1.0) (eql mv :tls-1.1)
        ;; A numeric fallback: accept anything <= #x0303.
        (and (integerp mv) (<= mv #x0303)))))

(defun %version-allows-1.3-p (context)
  (let ((mv (tls-context-max-version context)))
    (or (null mv)
        (eql mv :tls-1.3)
        (and (integerp mv) (>= mv #x0304)))))

(defun %peek-initial-client-hello (transport)
  "Read the first TLS record off TRANSPORT and return
   (values raw-record-bytes parsed-client-hello). The client-hello
   struct is a tls13:parsed-client-hello, usable for either dispatch
   target because the ClientHello wire format is shared."
  (let ((header (make-array 5 :element-type '(unsigned-byte 8)))
        (got 0))
    (loop while (< got 5) do
      (let ((n (tls:tls-transport-read transport header got 5)))
        (when (or (null n) (zerop n))
          (error "tls-accept: client closed before ClientHello"))
        (incf got n)))
    (let* ((len (logior (ash (aref header 3) 8) (aref header 4)))
           (body (make-array len :element-type '(unsigned-byte 8))))
      (let ((got 0))
        (loop while (< got len) do
          (let ((n (tls:tls-transport-read transport body got len)))
            (when (or (null n) (zerop n))
              (error "tls-accept: client closed mid-ClientHello"))
            (incf got n))))
      (let* ((wire (make-array (+ 5 len) :element-type '(unsigned-byte 8))))
        (replace wire header)
        (replace wire body :start1 5)
        ;; Parse: body is a single handshake record containing a
        ;; ClientHello. Skip the 4-byte handshake header and hand the
        ;; payload to tls13:parse-client-hello (wire format matches).
        (unless (= (aref header 0) 22) ; content type = handshake
          ;; Send protocol_version alert so the peer gets a clean rejection
          ;; instead of a silent connection drop.
          (ignore-errors
            (tls:tls-transport-write transport
              (coerce #(21 3 3 0 2 2 70) '(simple-array (unsigned-byte 8) (*))) 0 7))
          (error "tls-accept: first record is not a handshake (type ~D~A)"
                 (aref header 0)
                 (if (>= (aref header 0) #x80)
                     "; looks like SSLv2 -- not supported"
                     "")))
        (when (< len 4)
          (ignore-errors
            (tls:tls-transport-write transport
              (coerce #(21 3 3 0 2 2 50) '(simple-array (unsigned-byte 8) (*))) 0 7))
          (error "tls-accept: truncated handshake header"))
        (let* ((hs-type (aref body 0))
               (hs-len (logior (ash (aref body 1) 16)
                               (ash (aref body 2) 8)
                               (aref body 3))))
          (unless (= hs-type 1)  ; client_hello
            (ignore-errors
              (tls:tls-transport-write transport
                (coerce #(21 3 3 0 2 2 10) '(simple-array (unsigned-byte 8) (*))) 0 7))
            (error "tls-accept: expected ClientHello, got handshake type ~D"
                   hs-type))
          (when (> hs-len (- len 4))
            (ignore-errors
              (tls:tls-transport-write transport
                (coerce #(21 3 3 0 2 2 50) '(simple-array (unsigned-byte 8) (*))) 0 7))
            (error "tls-accept: handshake length exceeds record"))
          (let ((ch (tls:parse-client-hello (subseq body 4 (+ 4 hs-len)))))
            (values wire ch)))))))

(defun %client-wants-1.3-p (parsed-ch)
  "True when the client's supported_versions extension lists TLS 1.3."
  (some (lambda (v) (= v #x0304))
        (tls:parsed-client-hello-supported-versions parsed-ch)))

(defun tls-accept (socket context &key transport)
  "Accept a TLS connection, version-dispatched between TLS 1.3 and
   TLS 1.2 based on the client's advertised supported_versions and
   the context's configured min-version/max-version.
   TRANSPORT, when provided, overrides the default blocking transport.

   Compatible with epsilon.crypto:tls-accept."
  (let* ((transport (or transport (make-socket-transport socket)))
         (raw-key (tls-context-private-key context))
         (key (handler-case (key-from-der raw-key :private-p t)
                (error (e)
                  (error "TLS: cannot decode server private key (~D bytes): ~A"
                         (length raw-key) e))))
         (tls-key-type (case (native-key-type key)
                         (:ec-p256 :ecdsa-p256)
                         (t (native-key-type key))))
         (private-key (native-key-material key)))
    (multiple-value-bind (ch-bytes parsed-ch)
        (handler-case (%peek-initial-client-hello transport)
          (error (e)
            (error "tls-accept: failed reading ClientHello: ~A" e)))
      (let* ((client-wants-13 (%client-wants-1.3-p parsed-ch))
             (use-13 (and client-wants-13 (%version-allows-1.3-p context)))
             (use-12 (and (not use-13) (%version-allows-1.2-p context)))
             (client-suites (tls:parsed-client-hello-cipher-suites parsed-ch))
             (client-sni (tls:parsed-client-hello-hostname parsed-ch)))
        (cond
          (use-13
           (handler-case
               (let* ((replay (make-replay-transport :underlying transport
                                                     :buffered ch-bytes))
                      (config (tls:make-tls-server-config
                               :certificate-chain (tls-context-cert-chain context)
                               :private-key private-key
                               :key-type tls-key-type
                               :alpn-protocols (tls-context-alpn-protocols context)
                               :session-ticket-store (tls-context-session-ticket-store context)
                               :session-ticket-lifetime (tls-context-session-ticket-lifetime context)
                               :ocsp-staple (tls-context-ocsp-staple context)
                               ;; mTLS: forward the OpenSSL-style verify-mode
                               ;; bits onto the TLS 1.3 server config so the
                               ;; handshake emits CertificateRequest when
                               ;; +verify-peer+ is set.
                               :verify-mode (tls-context-verify-mode context)))
                      (native-stream (tls:tls-accept replay config)))
                 (make-tls-connection :native-stream native-stream
                                      :socket socket
                                      :context context
                                      :connected-p t
                                      :handshake-complete-p t))
             (error (e)
               (error "tls-accept(1.3): SNI=~A suites=~{~4,'0X~^ ~}: ~A"
                      (or client-sni "-") client-suites e))))
          (use-12
           (handler-case
               (let* ((config (tls12:make-tls12-server-config
                               :certificate-chain (tls-context-cert-chain context)
                               :private-key private-key
                               :key-type tls-key-type
                               :alpn-protocols (tls-context-alpn-protocols context)
                               :session-ticket-store (tls-context-session-ticket-store context)
                               :session-ticket-lifetime (tls-context-session-ticket-lifetime context)
                               :ocsp-staple (tls-context-ocsp-staple context)
                               ;; mTLS: same forwarding as the TLS 1.3 branch.
                               :verify-mode (tls-context-verify-mode context)))
                      (native-stream (tls12:tls12-accept transport config
                                                         :client-hello-bytes ch-bytes)))
                 (make-tls-connection :native-stream native-stream
                                      :socket socket
                                      :context context
                                      :connected-p t
                                      :handshake-complete-p t))
             (error (e)
               (error "tls-accept(1.2): SNI=~A suites=~{~4,'0X~^ ~}: ~A"
                      (or client-sni "-") client-suites e))))
          (t
           ;; Neither version is allowed by this context. Send a fatal
           ;; protocol_version alert and drop the connection.
           (ignore-errors
            (tls:tls-transport-write
             transport
             (coerce #(21 3 3 0 2 2 70) '(simple-array (unsigned-byte 8) (*)))
             0 7))
           (ignore-errors (tls:tls-transport-close transport))
           (error "tls-accept: no common version (min=~A max=~A client-wants-1.3=~A SNI=~A suites=~{~4,'0X~^ ~})"
                  (tls-context-min-version context)
                  (tls-context-max-version context)
                  client-wants-13
                  (or client-sni "-")
                  client-suites)))))))

(defun %tls12-stream-p (s) (tls12:tls12-stream-p s))

(defun tls-connection-tls13-stream (connection)
  "Return the underlying TLS 1.3 stream if CONNECTION negotiated TLS 1.3,
   or NIL for TLS 1.2 (where the bridge native-stream is a tls12-stream).
   Used by callers that want to read TLS 1.3-specific slot accessors
   (issued tickets, KeyUpdate counters, etc.)."
  (let ((stream (tls-connection-native-stream connection)))
    (when (and stream (not (%tls12-stream-p stream)))
      stream)))

(defun tls-read (connection buffer &key (start 0) (end (length buffer)))
  "Read data from a TLS connection into buffer.
   Returns number of bytes read, or 0 for EOF.
   Compatible with epsilon.crypto:tls-read."
  (unless (tls-connection-connected-p connection)
    (return-from tls-read 0))
  (let ((stream (tls-connection-native-stream connection)))
    (if (%tls12-stream-p stream)
        (tls12:tls12-read stream buffer :start start :end end)
        (tls:tls-read stream buffer :start start :end end))))

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
    (cond
      ((%tls12-stream-p stream)
       (if (stringp buffer)
           (tls12:tls12-write stream data :start 0 :end (length data))
           (tls12:tls12-write stream data :start start :end end)))
      (t
       (if (stringp buffer)
           (tls:tls-write stream data :start 0 :end (length data))
           (tls:tls-write stream data :start start :end end))))))

(defun tls-shutdown (connection)
  "Send close_notify alert. Compatible with epsilon.crypto:tls-shutdown."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (ignore-errors
       (if (%tls12-stream-p stream)
           (tls12:tls12-shutdown stream)
           (tls:tls-shutdown stream))))))

(defun tls-close (connection)
  "Close the TLS connection and underlying transport.
   Compatible with epsilon.crypto:tls-close.

   Also closes the underlying TCP socket (close(2) on the fd).  Without
   this, every request leaks a kernel CLOSE-WAIT socket -- the
   transport-close method on tcp-transport / async-tcp-transport only
   sets an open-p flag, and the application's tls-close call then
   never reaches the OS.  Under sustained load this saturates both
   proxy frontends and http servers (via epsilon.http.server's
   close path), which depend on tls-close for cleanup and have no
   other tcp-close call in their teardown."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (ignore-errors
       (if (%tls12-stream-p stream)
           (tls12:tls12-close stream)
           (tls:tls-close stream))))
    (setf (tls-connection-connected-p connection) nil)
    (setf (tls-connection-handshake-complete-p connection) nil)
    ;; Close the underlying TCP fd.  Idempotent, swallow errors so a
    ;; partially-set-up connection (e.g. handshake aborted before
    ;; tcp-close was reachable) still gets best-effort cleanup.
    (let ((socket (tls-connection-socket connection)))
      (when socket
        (handler-case (net:tcp-close socket) (error () nil)))))
  t)

;;; ---------------------------------------------------------------------------
;;; Connection information accessors
;;; ---------------------------------------------------------------------------

(defun connection-peer-certificate (connection)
  "Get the peer's certificate info. Returns a plist compatible with
   epsilon.crypto:connection-peer-certificate. The :fingerprint field
   is the SHA-256 of the leaf cert's DER encoding, in the standard
   lowercase-hex `sha256:<hex>' shape used by allowlists.

   Handles both representations of the underlying peer-cert slot: TLS
   1.2 stores raw DER byte vectors, TLS 1.3 stores already-parsed
   x509-certificate structs."
  (when (tls-connection-connected-p connection)
    (let* ((stream (tls-connection-native-stream connection))
           (certs (if (%tls12-stream-p stream)
                      (tls12:tls12-stream-peer-certificates stream)
                      (tls:tls-stream-peer-certificates stream))))
      (when certs
        (let* ((leaf (first certs))
               (already-parsed (not (and (vectorp leaf)
                                         (typep leaf '(simple-array (unsigned-byte 8) (*))))))
               (cert (if already-parsed
                         leaf
                         (x509-native:parse-x509-certificate leaf)))
               (leaf-der (if already-parsed
                             (x509-native:x509-cert-raw-bytes cert)
                             leaf)))
          (list :handle cert
                :subject (format-x509-name
                          (x509-native:x509-cert-subject cert))
                :issuer (format-x509-name
                         (x509-native:x509-cert-issuer cert))
                :fingerprint (format nil "sha256:~A"
                                     (sha256-mod:sha256-hex leaf-der))))))))

(defun connection-cipher (connection)
  "Get the negotiated cipher name as a string.
   Compatible with epsilon.crypto:connection-cipher."
  (when (tls-connection-connected-p connection)
    (let* ((stream (tls-connection-native-stream connection))
           (suite (if (%tls12-stream-p stream)
                      (tls12:tls12-stream-cipher-suite stream)
                      (tls:tls-stream-cipher-suite stream))))
      (cond
        ((%tls12-stream-p stream)
         (cond
           ((= suite tls12:+tls-ecdhe-ecdsa-aes-128-gcm-sha256+)
            "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256")
           ((= suite tls12:+tls-ecdhe-ecdsa-aes-256-gcm-sha384+)
            "TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384")
           ((= suite tls12:+tls-ecdhe-ecdsa-chacha20-poly1305-sha256+)
            "TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256")
           ((= suite tls12:+tls-ecdhe-rsa-aes-128-gcm-sha256+)
            "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256")
           ((= suite tls12:+tls-ecdhe-rsa-aes-256-gcm-sha384+)
            "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384")
           ((= suite tls12:+tls-ecdhe-rsa-chacha20-poly1305-sha256+)
            "TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256")
           (t (format nil "UNKNOWN(#x~4,'0X)" suite))))
        (t
         (case suite
           (#.tls:+tls-aes-128-gcm-sha256+ "TLS_AES_128_GCM_SHA256")
           (#.tls:+tls-aes-256-gcm-sha384+ "TLS_AES_256_GCM_SHA384")
           (#.tls:+tls-chacha20-poly1305-sha256+ "TLS_CHACHA20_POLY1305_SHA256")
           (t (format nil "UNKNOWN(#x~4,'0X)" suite))))))))

(defun connection-version (connection)
  "Get the negotiated TLS version string.
   Compatible with epsilon.crypto:connection-version."
  (when (tls-connection-connected-p connection)
    (if (%tls12-stream-p (tls-connection-native-stream connection))
        "TLSv1.2"
        "TLSv1.3")))

(defun connection-alpn-protocol (connection)
  "Get the ALPN protocol selected during handshake.
   Compatible with epsilon.crypto:connection-alpn-protocol."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (if (%tls12-stream-p stream)
          (tls12:tls12-stream-alpn-protocol stream)
          (tls:tls-stream-alpn-protocol stream)))))

(defun connection-negotiated-group (connection)
  "Return the named-group codepoint the server selected during the
   TLS 1.3 handshake (or NIL on TLS 1.2 / pre-handshake / closed).
   Used by IMPL-380 interop diagnostics to confirm hybrid PQ actually
   triggered."
  (when (tls-connection-connected-p connection)
    (let ((stream (tls-connection-native-stream connection)))
      (unless (%tls12-stream-p stream)
        (tls:tls-connection-negotiated-group
         (tls:tls-stream-connection stream))))))

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
;;; IO Protocol implementations for tls-stream
;;; ---------------------------------------------------------------------------

(tc:definstance io-proto:closer tls-stream
  (io-proto:close* (stream)
    (let ((conn (tls-stream-connection stream)))
      (when (and conn (tls-connection-connected-p conn))
        (handler-case (tls-shutdown conn)
          (error () nil))
        t)))
  (io-proto:open-p (stream)
    (let ((conn (tls-stream-connection stream)))
      (and conn (tls-connection-connected-p conn)))))

(tc:definstance io-proto:reader tls-stream
  (io-proto:read-into (stream buffer &key (start 0) (end (length buffer)))
    (let ((conn (tls-stream-connection stream)))
      (unless (and conn (tls-connection-connected-p conn))
        (return-from io-proto:read-into 0))
      (tls-read conn buffer :start start :end end))))

(tc:definstance io-proto:writer tls-stream
  (io-proto:write-from (stream buffer &key (start 0) (end (length buffer)))
    (let ((conn (tls-stream-connection stream)))
      (unless (and conn (tls-connection-connected-p conn))
        (return-from io-proto:write-from 0))
      (tls-write conn buffer :start start :end end)))
  (io-proto:flush (stream)
    (declare (ignore stream))
    nil))

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

(defun %oid-to-string (oid)
  "Coerce an OID -- either a dotted string already, or an integer
   list as parsed by epsilon.crypto.x509 -- to a dotted-decimal string."
  (cond
    ((stringp oid) oid)
    ((listp oid)
     (with-output-to-string (s)
       (let ((first-p t))
         (dolist (component oid)
           (unless first-p (write-char #\. s))
           (princ component s)
           (setf first-p nil)))))
    (t (princ-to-string oid))))

(defun format-x509-name (name)
  "Format an X.509 name as a /KEY=VALUE string like OpenSSL oneline.
   Accepts either an x509-name struct (post-tls13 parse) or a raw
   alist of (oid . value) pairs (the legacy shape). OIDs may be
   dotted strings or integer lists; both round-trip through
   oid-to-short-name."
  (let ((entries (cond
                   ((null name) nil)
                   ((listp name) name)
                   ((typep name 'x509-native:x509-name)
                    (x509-native:x509-name-entries name))
                   (t (error "format-x509-name: unsupported name shape ~S" name)))))
    (if (null entries)
        ""
        (with-output-to-string (s)
          (dolist (pair entries)
            (format s "/~A=~A"
                    (oid-to-short-name (%oid-to-string (car pair)))
                    (cdr pair)))))))

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

(defun %rsa-private-der (mat format)
  "DER-encode an RSA private key in the requested format. FORMAT is
   :pkcs1 (bare RSAPrivateKey) or :pkcs8 (PKCS#8 PrivateKeyInfo)."
  (let ((pkcs1-bytes
          (pkcs:encode-pkcs1-rsa-private
           (rsa-mod:rsa-private-key-n mat)
           (rsa-mod:rsa-private-key-e mat)
           (rsa-mod:rsa-private-key-d mat)
           :p (rsa-mod:rsa-private-key-p mat)
           :q (rsa-mod:rsa-private-key-q mat)
           :dp (rsa-mod:rsa-private-key-dp mat)
           :dq (rsa-mod:rsa-private-key-dq mat)
           :qinv (rsa-mod:rsa-private-key-qinv mat))))
    (ecase format
      (:pkcs1 pkcs1-bytes)
      (:pkcs8
       ;; PKCS#8 PrivateKeyInfo wrapping the PKCS#1 RSAPrivateKey.
       (asn1:der-encode-sequence
        (asn1:der-encode-integer 0)
        (asn1:der-encode-sequence
         (asn1:der-encode-oid pkcs:+oid-rsa-encryption+)
         (asn1:der-encode-null))
        (asn1:der-encode-octet-string pkcs1-bytes))))))

(defun key-to-pem (key &key private-p (format :auto))
  "Export a native-key to PEM format string.
   When PRIVATE-P is true and the key is private, exports the private key
   and the FORMAT keyword selects the container:
     :auto  -- RSA => PKCS#1 (`RSA PRIVATE KEY`), EC/Ed25519 => PKCS#8.
     :pkcs1 -- RSA only; bare PKCS#1 RSAPrivateKey (`RSA PRIVATE KEY`).
     :pkcs8 -- PKCS#8 PrivateKeyInfo (`PRIVATE KEY`). Works for all types.
     :sec1  -- EC only; SEC1 ECPrivateKey (`EC PRIVATE KEY`).
   When exporting a public key, FORMAT is ignored and SPKI is emitted."
  (let ((want-private (and private-p (native-key-private-p key))))
    (ecase (native-key-type key)
      (:ed25519
       (if want-private
           (ecase format
             ((:auto :pkcs8)
              (pem:pem-encode (pem:make-pem-block
                               "PRIVATE KEY"
                               (pkcs:encode-pkcs8-ed25519 (native-key-material key))))))
           (let ((pub (ed25519-sign:ed25519-public-key-from-private
                       (native-key-material key))))
             (pem:pem-encode (pem:make-pem-block
                              "PUBLIC KEY"
                              (pkcs:encode-spki-ed25519 pub))))))
      (:rsa
       (let ((mat (native-key-material key)))
         (if want-private
             (ecase format
               ((:auto :pkcs1)
                (pem:pem-encode (pem:make-pem-block
                                 "RSA PRIVATE KEY"
                                 (%rsa-private-der mat :pkcs1))))
               (:pkcs8
                (pem:pem-encode (pem:make-pem-block
                                 "PRIVATE KEY"
                                 (%rsa-private-der mat :pkcs8)))))
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
             (ecase format
               ((:auto :pkcs8)
                (pem:pem-encode (pem:make-pem-block
                                 "PRIVATE KEY"
                                 (pkcs:encode-pkcs8-ec d pub-bytes))))
               (:sec1
                ;; SEC1 ECPrivateKey (bare, without PKCS#8 wrapper)
                (pem:pem-encode
                 (pem:make-pem-block
                  "EC PRIVATE KEY"
                  (asn1:der-encode-sequence
                   (asn1:der-encode-integer 1)
                   (asn1:der-encode-octet-string (%integer-to-fixed-bytes d 32))
                   (asn1:der-encode-context 0
                                            (asn1:der-encode-oid pkcs:+oid-prime256v1+)
                                            :constructed t)
                   (asn1:der-encode-context 1
                                            (asn1:der-encode-bit-string pub-bytes)
                                            :constructed t)))))))
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

(defun key-from-pem (pem-string &key private-p password)
  "Import a key from PEM format string. Returns a native-key.
   The PEM label (e.g. `RSA PRIVATE KEY`, `EC PRIVATE KEY`, `PRIVATE KEY`,
   `PUBLIC KEY`) determines the format. PRIVATE-P can be passed to treat
   an ambiguously labelled block as private.

   When the block carries legacy RFC 1421 headers
   (`Proc-Type: 4,ENCRYPTED` + `DEK-Info: AES-*-CBC,<iv>`), PASSWORD is
   required and the block is decrypted with EVP_BytesToKey+AES-CBC before
   the inner DER is decoded. This is the OpenSSL `-pass` format used by
   the internal CA's on-disk private keys."
  (let ((block (pem:pem-decode pem-string)))
    (unless block
      (error "Failed to decode PEM data"))
    (cond
      ((pem-enc:legacy-encrypted-pem-p block)
       (unless password
         (error "key-from-pem: block ~S is encrypted (Proc-Type: 4,ENCRYPTED) ~
                 but no :password was supplied"
                (pem:pem-block-label block)))
       (multiple-value-bind (der label)
           (pem-enc:decrypt-legacy-pem pem-string password)
         (declare (ignorable label))
         ;; Post-decryption we always have a private-key body -- the
         ;; OpenSSL legacy format never wraps public material.
         (decode-private-key-pem der (pem:pem-block-label block))))
      (t
       (let* ((label (pem:pem-block-label block))
              (der (pem:pem-block-data block))
              (is-private (or private-p
                              (search "PRIVATE" label))))
         (if is-private
             (decode-private-key-pem der label)
             (decode-public-key-pem der label)))))))

(defun %der-outer-content-offset (der)
  "Offset to the contents of a top-level SEQUENCE, handling long-form length."
  (if (and (> (length der) 1)
           (= (aref der 0) #x30)
           (= (logand (aref der 1) #x80) #x80))
      (+ 2 (logand (aref der 1) #x7f))
      2))

(defun key-from-der (der &key private-p)
  "Import a key from DER bytes, auto-detecting the container format.
   Returns a native-key.

   Recognised private-key formats:
     - PKCS#1 RSAPrivateKey  (bare RSA, no PKCS#8 wrapper)
     - SEC1 ECPrivateKey      (bare EC, no PKCS#8 wrapper)
     - PKCS#8 PrivateKeyInfo  (RSA, EC P-256, or Ed25519)
     - Raw 32-byte Ed25519 seed

   Recognised public-key format:
     - SubjectPublicKeyInfo (RFC 5280) for RSA, EC P-256, Ed25519

   PRIVATE-P forces the private decoder even if the structure is
   ambiguous. When NIL, the decoder first attempts an SPKI parse if
   the outer structure looks like a public key, otherwise falls back
   to private-key detection."
  (cond
    ;; Raw 32-byte Ed25519 seed is unambiguous and the simplest case.
    ((= (length der) 32)
     (make-native-key :type :ed25519 :private-p t :material der))
    (t
     (let* ((co (%der-outer-content-offset der))
            ;; Public SPKI: outer SEQUENCE { SEQUENCE (AlgorithmIdentifier), BIT STRING }
            ;; First inner element is SEQUENCE (tag 0x30).
            ;; Private formats begin with INTEGER (tag 0x02).
            (inner-tag (and (> (length der) co) (aref der co))))
       (cond
         ;; Forced-private or definitely-private (starts with INTEGER)
         ((or private-p (eql inner-tag #x02))
          (%decode-private-der der))
         ;; Looks like SPKI (starts with SEQUENCE)
         ((eql inner-tag #x30)
          (%decode-public-der der))
         (t (error "Unrecognised DER key structure")))))))

(defun %decode-public-der (der)
  "Decode SubjectPublicKeyInfo DER into a native public key."
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

(defun %decode-private-der (der)
  "Decode a private key from DER, auto-selecting between PKCS#1,
   SEC1, and PKCS#8 container formats."
  (let ((kind (detect-key-type-from-der der)))
    (ecase kind
      (:rsa
       ;; May be bare PKCS#1 or PKCS#8-wrapped. Distinguish by the
       ;; second CRI element's tag (see detect-key-type-from-der).
       (let* ((co (%der-outer-content-offset der))
              (pkcs1-bytes
                (if (and (> (length der) (+ co 3))
                         (= (aref der (+ co 3)) #x02))
                    der
                    (multiple-value-bind (oid params kb) (pkcs:decode-pkcs8 der)
                      (declare (ignore oid params))
                      kb))))
         (multiple-value-bind (n e d p q dp dq qinv)
             (pkcs:decode-pkcs1-rsa-private pkcs1-bytes)
           (make-native-key :type :rsa :private-p t
                            :material (rsa-mod:make-rsa-private-key
                                       n e d :p (or p 0) :q (or q 0)
                                       :dp (or dp 0) :dq (or dq 0)
                                       :qinv (or qinv 0))))))
      (:ecdsa-p256
       ;; SEC1 ECPrivateKey: SEQUENCE { INTEGER(1), OCTET STRING(d), ... }
       (let* ((ec-tlv (asn1:der-decode der))
              (children (asn1:der-decode-sequence-contents ec-tlv))
              (d-bytes (asn1:asn1-tlv-value (second children)))
              (d (bytes-to-integer d-bytes)))
         (make-native-key :type :ec-p256 :private-p t :material d)))
      (:ec-p256
       (%decode-private-der-pkcs8 der))
      (:ed25519
       (cond
         ((= (length der) 32)
          (make-native-key :type :ed25519 :private-p t :material der))
         (t (%decode-private-der-pkcs8 der))))
      (:unknown
       (error "Unrecognised private key DER (not PKCS#1, SEC1, PKCS#8, or raw Ed25519)")))))

(defun %decode-private-der-pkcs8 (der)
  "Decode a PKCS#8 PrivateKeyInfo DER into a native-key."
  (multiple-value-bind (oid params key-bytes) (pkcs:decode-pkcs8 der)
    (declare (ignore params))
    (cond
      ((equal oid pkcs:+oid-ed25519+)
       (let* ((inner-tlv (asn1:der-decode key-bytes))
              (seed (asn1:asn1-tlv-value inner-tlv)))
         (make-native-key :type :ed25519 :private-p t :material seed)))
      ((equal oid pkcs:+oid-ec-public-key+)
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
      (t (error "Unsupported PKCS#8 algorithm OID: ~A" oid)))))

(defun key-to-der (key &key private-p (format :auto))
  "Export a native-key to DER bytes. See `key-to-pem` for FORMAT semantics."
  (let ((pem (key-to-pem key :private-p private-p :format format)))
    (pem:pem-block-data (pem:pem-decode pem))))

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

(defun %integer-to-fixed-bytes (n size)
  "Encode integer N as a big-endian byte vector of exactly SIZE bytes."
  (let ((result (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from (1- size) downto 0
          for shift from 0 by 8
          while (plusp n)
          do (setf (aref result i) (logand n #xFF))
             (setf n (ash n -8)))
    result))

(defun sign-message (key data &key (digest "SHA256") (format :default))
  "Sign DATA with native-key KEY. Returns signature bytes.
FORMAT selects the signature encoding for the key type:
  EC P-256:
    :default or :der -- DER-encoded SEQUENCE{INTEGER,INTEGER} (for TLS/X.509)
    :raw             -- Fixed-width R||S (for JWS/JOSE, 64 bytes for P-256)
  RSA:
    :default or :pss -- RSASSA-PSS (for JWS RS256, modern protocols)
    :pkcs1           -- RSASSA-PKCS1-v1_5 (for X.509 CSRs/certificates)"
  (declare (ignore digest))
  (let ((material (native-key-material key))
        (msg (ensure-bytes data)))
    (ecase (native-key-type key)
      (:ed25519
       (ed25519-sign:ed25519-sign material msg))
      (:ec-p256
       (multiple-value-bind (r s) (ecdsa:ecdsa-sign material msg)
         (ecase format
           ((:default :der)
            (asn1:der-encode-sequence
             (asn1:der-encode-integer r)
             (asn1:der-encode-integer s)))
           (:raw
            ;; JWS ES256: two 32-byte big-endian integers concatenated
            (concatenate '(vector (unsigned-byte 8))
                         (%integer-to-fixed-bytes r 32)
                         (%integer-to-fixed-bytes s 32))))))
      (:rsa
       (ecase format
         ((:default :pss)
          (rsa-mod:rsa-pss-sign material msg))
         (:pkcs1
          (rsa-mod:pkcs1-v15-sign material msg)))))))

(defun %bytes-to-integer (bytes start end)
  "Decode big-endian BYTES[START,END) into a non-negative integer."
  (let ((n 0))
    (loop for i from start below end do
      (setf n (logior (ash n 8) (aref bytes i))))
    n))

(defun verify-message (key data signature &key (digest "SHA256") (format :default))
  "Verify SIGNATURE on DATA with native-key KEY. Returns T if valid.
FORMAT selects the expected signature encoding and must match the format
used when signing:
  EC P-256:
    :default or :der -- DER-encoded SEQUENCE{INTEGER,INTEGER} (for TLS/X.509)
    :raw             -- Fixed-width R||S (for JWS/JOSE, 64 bytes for P-256)
  RSA:
    :default         -- Try PKCS#1 v1.5 first, then PSS (lenient)
    :pss             -- RSASSA-PSS only
    :pkcs1           -- RSASSA-PKCS1-v1_5 only"
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
       (handler-case
           (let ((point (if (native-key-private-p key)
                            ;; Derive public point from private scalar
                            (ec:p256-scalar-mul (native-key-material key)
                                                (ec:p256-base-point))
                            (ec:p256-point-decode (native-key-material key)))))
             (when point
               (multiple-value-bind (r s)
                   (ecase format
                     ((:default :der)
                      (let* ((tlv (asn1:der-decode sig))
                             (children (asn1:der-decode-sequence-contents tlv)))
                        (values (asn1:decode-der-integer
                                 (asn1:asn1-tlv-value (first children)))
                                (asn1:decode-der-integer
                                 (asn1:asn1-tlv-value (second children))))))
                     (:raw
                      ;; JWS ES256: exactly 64 bytes, R||S big-endian
                      (unless (= (length sig) 64)
                        (return-from verify-message nil))
                      (values (%bytes-to-integer sig 0 32)
                              (%bytes-to-integer sig 32 64))))
                 (ecdsa:ecdsa-verify point msg r s))))
         (error () nil)))
      (:rsa
       ;; Extract public key from private key if needed
       (let ((pub-key (if (native-key-private-p key)
                          (rsa-mod:make-rsa-public-key
                           (rsa-mod:rsa-private-key-n (native-key-material key))
                           (rsa-mod:rsa-private-key-e (native-key-material key)))
                          (native-key-material key))))
         (ecase format
           (:pkcs1
            (and (ignore-errors (rsa-mod:pkcs1-v15-verify pub-key msg sig)) t))
           (:pss
            (and (ignore-errors (rsa-mod:rsa-pss-verify pub-key msg sig)) t))
           (:default
            ;; Lenient: accept either format. Preserved for callers that
            ;; do not know which format the signer used.
            (or (ignore-errors (rsa-mod:pkcs1-v15-verify pub-key msg sig))
                (ignore-errors (rsa-mod:rsa-pss-verify pub-key msg sig))))))))))

