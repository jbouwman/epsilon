;;;; Tests for epsilon.ssl.bridge
;;;;
;;;; Verifies that the bridge provides API compatibility with
;;;; epsilon.crypto's TLS interface.

(defpackage epsilon.ssl.bridge-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:bridge #:epsilon.crypto.native)
   (#:tls #:epsilon.ssl.tls13)
   (#:x509 #:epsilon.ssl.x509)
   (#:drbg #:epsilon.ssl.drbg)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:net #:epsilon.net))
  (:enter t))

(in-package :epsilon.ssl.bridge-tests)

;;; ---------------------------------------------------------------------------
;;; Context tests
;;; ---------------------------------------------------------------------------

(deftest test-make-client-context
  "Create a client TLS context"
  (let ((ctx (bridge:make-client-context)))
    (assert-true (bridge:tls-context-p ctx))
    (assert-not (bridge:tls-context-server-p ctx))
    (assert-= (bridge:tls-context-verify-mode ctx) bridge:+verify-none+)))

(deftest test-make-client-context-with-verify
  "Create a client context with peer verification"
  (let ((ctx (bridge:make-client-context :verify-mode bridge:+verify-peer+)))
    (assert-= (bridge:tls-context-verify-mode ctx) bridge:+verify-peer+)))

(deftest test-make-server-context
  "Create a server TLS context"
  (let ((ctx (bridge:make-server-context)))
    (assert-true (bridge:tls-context-p ctx))
    (assert-true (bridge:tls-context-server-p ctx))))

(deftest test-context-set-alpn
  "Set ALPN protocols on context"
  (let ((ctx (bridge:make-client-context)))
    (bridge:context-set-alpn-protocols ctx '("h2" "http/1.1"))
    (assert-equal (bridge:tls-context-alpn-protocols ctx) '("h2" "http/1.1"))))

(deftest test-context-configuration
  "Configure context settings"
  (let ((ctx (bridge:make-client-context)))
    (bridge:context-set-verify-mode ctx bridge:+verify-peer+)
    (assert-= (bridge:tls-context-verify-mode ctx) bridge:+verify-peer+)
    (bridge:context-set-verify-depth ctx 10)
    (assert-= (bridge:tls-context-verify-depth ctx) 10)
    ;; These are no-ops but should not error
    (bridge:context-set-ciphersuites ctx "TLS_AES_128_GCM_SHA256")
    (bridge:context-set-cipher-list ctx "HIGH:!aNULL")
    (bridge:context-set-session-cache-mode ctx bridge:+session-cache-client+)))

(deftest test-free-context
  "Free context is a no-op but should not error"
  (let ((ctx (bridge:make-client-context)))
    (assert-true (bridge:free-tls-context ctx))))

;;; ---------------------------------------------------------------------------
;;; Constants tests
;;; ---------------------------------------------------------------------------

(deftest test-version-constants
  "TLS version constants match expected values"
  (assert-= bridge:+tls-1.0+ #x0301)
  (assert-= bridge:+tls-1.1+ #x0302)
  (assert-= bridge:+tls-1.2+ #x0303)
  (assert-= bridge:+tls-1.3+ #x0304))

(deftest test-verify-constants
  "Verify mode constants"
  (assert-= bridge:+verify-none+ 0)
  (assert-= bridge:+verify-peer+ 1)
  (assert-= bridge:+verify-fail-if-no-peer-cert+ 2))

(deftest test-alpn-constants
  "ALPN protocol string constants"
  (assert-equal bridge:+alpn-http/1.1+ "http/1.1")
  (assert-equal bridge:+alpn-h2+ "h2"))

;;; ---------------------------------------------------------------------------
;;; ALPN buffer encoding/decoding
;;; ---------------------------------------------------------------------------

(deftest test-alpn-buffer-roundtrip
  "ALPN wire format encode/decode"
  (let* ((protocols '("h2" "http/1.1"))
         (buf (bridge:make-alpn-buffer protocols))
         (decoded (bridge:parse-alpn-buffer buf)))
    (assert-equal decoded protocols)))

;;; ---------------------------------------------------------------------------
;;; Connection struct tests
;;; ---------------------------------------------------------------------------

(deftest test-tls-connection-struct
  "TLS connection struct creation"
  (let ((conn (bridge:make-tls-connection :connected-p t
                                           :handshake-complete-p t)))
    (assert-true (bridge:tls-connection-p conn))
    (assert-true (bridge:tls-connection-connected-p conn))
    (assert-true (bridge:tls-connection-handshake-complete-p conn))))

;;; ---------------------------------------------------------------------------
;;; mTLS helpers
;;; ---------------------------------------------------------------------------

(deftest test-mtls-client-context
  "Create mTLS client context"
  (let ((ctx (bridge:make-mtls-client-context)))
    (assert-true (bridge:tls-context-p ctx))
    (assert-= (bridge:tls-context-verify-mode ctx) bridge:+verify-peer+)))

(deftest test-mtls-server-context
  "Create mTLS server context"
  (let ((ctx (bridge:make-mtls-server-context)))
    (assert-true (bridge:tls-context-p ctx))
    (assert-true (bridge:tls-context-server-p ctx))
    (assert-= (bridge:tls-context-verify-mode ctx) bridge:+verify-peer+)))

(deftest test-require-client-certificate
  "Require client certificate sets correct verify mode"
  (let ((ctx (bridge:make-server-context)))
    (bridge:require-client-certificate ctx)
    (assert-= (bridge:tls-context-verify-mode ctx)
              (logior bridge:+verify-peer+
                      bridge:+verify-fail-if-no-peer-cert+))))

;;; ---------------------------------------------------------------------------
;;; Loopback TLS tests (local TCP, no external network)
;;; ---------------------------------------------------------------------------

(defun make-loopback-listener ()
  "Create a TCP listener on localhost with an ephemeral port.
   Returns (values listener port)."
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0)
                                :backlog 1)))
    (values listener
            (net:socket-address-port (net:tcp-local-addr listener)))))

(defun connect-to-loopback (port)
  "Connect to localhost:PORT. Returns (values fd tcp-stream)."
  (let ((tcp (net:tcp-connect (net:make-socket-address "127.0.0.1" port))))
    (values (net:tcp-stream-handle tcp) tcp)))

(defun accept-connection (listener)
  "Accept a connection on LISTENER. Returns (values fd tcp-stream)."
  (let ((tcp (net:tcp-accept listener)))
    (values (net:tcp-stream-handle tcp) tcp)))

(defun make-test-server-context ()
  "Create a server context with Ed25519 self-signed cert for testing."
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :subject "localhost"
                    :dns-names '("localhost")))
         (ctx (bridge:make-server-context)))
    (setf (bridge:tls-context-cert-chain ctx) (list cert-der))
    (setf (bridge:tls-context-private-key ctx) sk)
    ctx))

(deftest test-bridge-tls-loopback
  "Full TLS handshake and data exchange over loopback TCP"
  (multiple-value-bind (listener port) (make-loopback-listener)
    (let* ((server-ctx (make-test-server-context))
           (client-ctx (bridge:make-client-context))
           (server-conn nil)
           (server-error nil)
           (client-tcp nil)
           (accepted-tcp nil))
      (bridge:context-set-alpn-protocols client-ctx '("http/1.1"))
      ;; Server thread: accept and TLS-accept
      (let ((server-thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (multiple-value-bind (fd tcp) (accept-connection listener)
                      (setf accepted-tcp tcp)
                      (setf server-conn (bridge:tls-accept fd server-ctx)))
                  (error (e) (setf server-error e))))
              :name "tls-test-server")))
        (unwind-protect
            (progn
              ;; Client connects
              (multiple-value-bind (fd tcp) (connect-to-loopback port)
                (setf client-tcp tcp)
                (let ((client-conn (bridge:tls-connect fd client-ctx
                                                        :hostname "localhost")))
                  ;; Wait for server
                  (sb-thread:join-thread server-thread)
                  (when server-error (error server-error))
                  ;; Both sides connected
                  (assert-true (bridge:tls-connection-p client-conn))
                  (assert-true (bridge:tls-connection-connected-p client-conn))
                  (assert-true (bridge:tls-connection-p server-conn))
                  (assert-true (bridge:tls-connection-connected-p server-conn))
                  ;; Client writes, server reads
                  (bridge:tls-write client-conn "hello from client")
                  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                    (let ((n (bridge:tls-read server-conn buf)))
                      (assert-true (> n 0))
                      (assert-equal (map 'string #'code-char (subseq buf 0 n))
                                    "hello from client")))
                  ;; Server writes, client reads
                  (bridge:tls-write server-conn "hello from server")
                  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                    (let ((n (bridge:tls-read client-conn buf)))
                      (assert-true (> n 0))
                      (assert-equal (map 'string #'code-char (subseq buf 0 n))
                                    "hello from server")))
                  (ignore-errors (bridge:tls-close client-conn))
                  (ignore-errors (bridge:tls-close server-conn)))))
          ;; Clean up sockets
          (ignore-errors (net:tcp-close listener))
          (ignore-errors (when client-tcp (net:tcp-close client-tcp)))
          (ignore-errors (when accepted-tcp (net:tcp-close accepted-tcp))))))))

;;; ---------------------------------------------------------------------------
;;; Crypto facade test
;;; ---------------------------------------------------------------------------

(deftest test-crypto-facade-creates-native-context
  "epsilon.crypto facade creates a bridge tls-context"
  (when (find-package :epsilon.crypto)
    (let* ((make-ctx (find-symbol "MAKE-CLIENT-CONTEXT"
                                   (find-package :epsilon.crypto)))
           (ctx (funcall make-ctx)))
      (assert-true (bridge:tls-context-p ctx)))))
