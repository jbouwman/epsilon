(defpackage :net.tls
  (:use :common-lisp
        #:sb-gray
        #:lib.binding
        #:lib.char
        #:lib.list
        #:lib.stream
        #:lib.symbol
        #:sys.sync.lock
        #:sys.sync.thread)
  (:local-nicknames
   (#:ffi #:sys.ffi))
  (:export
   ;;; Create TLS stream over TCP stream
   make-ssl-client-stream
   make-ssl-server-stream

   ;;; Custom binding for the global SSL_CTX
   with-global-context

   ;;; Custom SSL_CTX creation
   make-context
   ssl-ctx-free

   ;;; Configure the global SSL_CTX
   use-certificate-chain-file
   ssl-load-global-verify-locations
   ssl-set-global-default-verify-paths

   ;;; PEM file reading
   with-pem-password

   ;;; Properties of an established TLS session
   get-selected-alpn-protocol

   ;;; x509 Certificates
   ;;; Obtain
   decode-certificate-from-file
   decode-certificate
   ssl-stream-x509-certificate
   ;;; Release
   x509-free
   ;;; Accessors
   certificate-not-after-time
   certificate-not-before-time
   certificate-subject-common-names
   certificate-fingerprint
   ;; (low level function, already
   ;; employed by make-ssl-client-stream
   ;; if verification is enabled and hostname
   ;; is passed in)
   verify-hostname

   ;;; Saving / loading Lisp image
   reload

   ;;; Various
   stream-fd
   random-bytes
   ensure-initialized

   ;;; Default values
   *default-cipher-list*
   *default-buffer-size*
   *make-ssl-client-stream-verify-default*
   *default-unwrap-stream-p*

   ;;; Error conditions.
   ;;; Not full list, there are more non-exported,
   ;;; including the base classes.
   ;;; Should we export them all?
   ssl-error-verify
   ssl-error-initialize
   ;;; accessors of ssl-error-verify
   ssl-error-stream
   ssl-error-code

   ;;; OpenSSL API constants
   +ssl-verify-none+
   +ssl-verify-peer+
   +ssl-verify-fail-if-no-peer-cert+
   +ssl-verify-client-once+

   +ssl-op-no-sslv2+
   +ssl-op-no-sslv3+
   +ssl-op-no-tlsv1+
   +ssl-op-no-tlsv1-1+
   +ssl-op-no-tlsv1-2+

   +ssl-sess-cache-off+
   +ssl-sess-cache-client+
   +ssl-sess-cache-server+
   +ssl-sess-cache-both+
   +ssl-sess-cache-no-auto-clear+
   +ssl-sess-cache-no-internal-lookup+
   +ssl-sess-cache-no-internal-store+
   +ssl-sess-cache-no-internal+

   ;;; DEPRECATED.

   ;; Use the (MAKE-SSL-CLIENT-STREAM .. :VERIFY ?) to enable/disable verification.
   ;; MAKE-CONTEXT also allows to enab/disable verification.
   ssl-check-verify-p
   ))

(import '(net.tls/config::libssl
          net.tls/config::libcrypto))
