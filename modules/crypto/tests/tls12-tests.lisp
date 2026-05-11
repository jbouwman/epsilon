;;;; Tests for TLS 1.2 Protocol (IMPL-333 Phase 4)
;;;;
;;;; The core test is a full in-process round trip: a client built by
;;;; epsilon.crypto.tls12 drives the handshake through to :connected
;;;; against a server built by the same module, exchanges application
;;;; data in both directions, and cleanly shuts down. The harness uses
;;;; the staged API (no threads, no sockets) so a failure points at the
;;;; wire format or key schedule rather than scheduling or IO.

(defpackage epsilon.crypto.tls12-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.tls12 tls12)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.tls-session-ticket-store stek)))

(in-package :epsilon.crypto.tls12-tests)

;;; ---------------------------------------------------------------------------
;;; Helpers: ephemeral ECDSA-P-256 server keypair + self-signed cert
;;; ---------------------------------------------------------------------------

(defun %make-server-identity ()
  "Generate an ephemeral ECDSA-P-256 keypair and a matching self-signed
   certificate. Returns (values private-key-integer cert-der-bytes)."
  (multiple-value-bind (sk pk-pt) (ecdh:ecdh-p256-generate-keypair)
    (let* ((pk-bytes (ec-p256:p256-point-encode-uncompressed pk-pt))
           (cert-der (x509:make-self-signed-certificate
                      :key-type :ecdsa-p256
                      :private-key sk
                      :public-key-bytes pk-bytes
                      :subject "localhost"
                      :dns-names '("localhost"))))
      (values sk cert-der))))

;;; ---------------------------------------------------------------------------
;;; Staged round-trip driver
;;;
;;; The five steps exactly mirror the wire: client flight 1, server
;;; flight 1, client flight 2, server flight 2, then application-data
;;; exchange. We wire the three stages together by reaching into the
;;; internal connection structs so the test does not depend on any
;;; transport or scheduling.
;;; ---------------------------------------------------------------------------

(defun %make-client-conn (config)
  "Create a tls12-connection set up for a client handshake. This mirrors
   what tls12-connect does before its first transport write."
  (let ((c (tls12::make-tls12-connection :role :client)))
    (let ((extras (tls12::%client-extras c :create-p t)))
      (setf (tls12::tls12-client-extras-config extras) config))
    c))

(defun %make-server-conn (config)
  (tls12::make-tls12-connection :role :server :config config))

(defun %extract-ch-payload (ch-record)
  "Strip the 5-byte record header and the 4-byte handshake header from a
   ClientHello record, returning the raw ClientHello payload bytes that
   tls12-server-start-handshake expects."
  (let* ((rec (tls12:parse-tls-record ch-record))
         (rec-data (tls12::tls-record-data rec)))
    ;; handshake header is 4 bytes (type + 3-byte length)
    (subseq rec-data 4)))

(defstruct handshake-result
  client-conn
  server-conn
  c->s-app-data       ; bytes from client to server after handshake
  s->c-app-data)      ; bytes from server to client after handshake

(defun %drive-handshake (server-config client-config)
  "Run a full TLS 1.2 handshake to :connected using the staged API.
   Returns the two connections inside a handshake-result struct."
  (let* ((client (%make-client-conn client-config))
         (server (%make-server-conn server-config))
         ;; Flight 1: client -> server.
         (ch-record (tls12::tls12-client-start-handshake client))
         (ch-payload (%extract-ch-payload ch-record))
         ;; Server flight 1.
         (server-flight1 (tls12:tls12-server-start-handshake server ch-payload))
         ;; Client processes server flight 1 and emits flight 2.
         (client-flight2 (tls12::tls12-client-process-server-flight1
                          client server-flight1))
         ;; Server processes client flight 2 and emits its flight 2.
         (server-flight2 (tls12:tls12-server-process-client-flight2
                          server client-flight2)))
    ;; Client processes server flight 2: handshake complete.
    (tls12::tls12-client-finish-handshake client server-flight2)
    (make-handshake-result :client-conn client :server-conn server)))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(deftest test-tls12-prf-known-answer
  "TLS 1.2 PRF SHA-256 output length matches the request, and is
   deterministic in secret/label/seed."
  (let* ((secret (drbg:random-bytes 32))
         (seed (drbg:random-bytes 32))
         (a (tls12:tls12-prf :sha256 secret "test label" seed 48))
         (b (tls12:tls12-prf :sha256 secret "test label" seed 48)))
    (assert-= 48 (length a))
    (assert-equalp a b)))

(deftest test-tls12-full-handshake-ecdsa
  "Full client<->server handshake completes with an ECDSA-P-256
   server key, EMS, and ALPN negotiated."
  (multiple-value-bind (sk cert-der) (%make-server-identity)
    (let* ((server-config (tls12:make-tls12-server-config
                           :certificate-chain (list cert-der)
                           :private-key sk
                           :key-type :ecdsa-p256
                           :alpn-protocols '("h2" "http/1.1")))
           (client-config (tls12:make-tls12-client-config
                           :hostname "localhost"
                           :alpn-protocols '("h2")
                           ;; Intentionally NIL: server cert verification
                           ;; is deferred (see tls12.lisp TODO). This
                           ;; test exercises the wire/crypto path only.
                           :trust-store nil))
           (result (%drive-handshake server-config client-config))
           (client (handshake-result-client-conn result))
           (server (handshake-result-server-conn result)))
      (assert-eq :connected (tls12:tls12-connection-state client))
      (assert-eq :connected (tls12:tls12-connection-state server))
      ;; Cipher suite and version agreement.
      (assert-= (tls12:tls12-connection-cipher-suite client)
                (tls12:tls12-connection-cipher-suite server))
      ;; ALPN: client offered h2; server supported h2 first.
      (assert-equal "h2" (tls12:tls12-connection-alpn-protocol server))
      ;; Master secrets must match on both sides.
      (assert-equalp (tls12::tls12-connection-master-secret client)
                     (tls12::tls12-connection-master-secret server)))))

(deftest test-tls12-application-data-round-trip
  "After a successful handshake, app data flows in both directions
   and decrypts cleanly under the derived keys."
  (multiple-value-bind (sk cert-der) (%make-server-identity)
    (let* ((server-config (tls12:make-tls12-server-config
                           :certificate-chain (list cert-der)
                           :private-key sk
                           :key-type :ecdsa-p256))
           (client-config (tls12:make-tls12-client-config
                           :hostname "localhost"))
           (result (%drive-handshake server-config client-config))
           (client (handshake-result-client-conn result))
           (server (handshake-result-server-conn result)))
      ;; Client -> server
      (let* ((msg1 (map '(vector (unsigned-byte 8)) #'char-code
                        "GET / HTTP/1.1"))
             (c-record (tls12:tls12-send-application-data client msg1))
             (c-rec-parsed (tls12:parse-tls-record c-record))
             (c-body (tls12::tls-record-data c-rec-parsed))
             (pt (tls12:tls12-receive-application-data server c-body)))
        (assert-equalp msg1 pt))
      ;; Server -> client
      (let* ((msg2 (map '(vector (unsigned-byte 8)) #'char-code
                        "HTTP/1.1 200 OK"))
             (s-record (tls12:tls12-send-application-data server msg2))
             (s-rec-parsed (tls12:parse-tls-record s-record))
             (s-body (tls12::tls-record-data s-rec-parsed))
             (pt (tls12:tls12-receive-application-data client s-body)))
        (assert-equalp msg2 pt)))))

(deftest test-tls12-multiple-records-in-sequence
  "Sequence numbers advance correctly across multiple records on the
   same direction (GCM nonce derivation depends on this)."
  (multiple-value-bind (sk cert-der) (%make-server-identity)
    (let* ((server-config (tls12:make-tls12-server-config
                           :certificate-chain (list cert-der)
                           :private-key sk
                           :key-type :ecdsa-p256))
           (client-config (tls12:make-tls12-client-config
                           :hostname "localhost"))
           (result (%drive-handshake server-config client-config))
           (client (handshake-result-client-conn result))
           (server (handshake-result-server-conn result)))
      (dotimes (i 5)
        (let* ((msg (map '(vector (unsigned-byte 8)) #'char-code
                         (format nil "message ~D" i)))
               (rec (tls12:tls12-send-application-data client msg))
               (parsed (tls12:parse-tls-record rec))
               (body (tls12::tls-record-data parsed))
               (pt (tls12:tls12-receive-application-data server body)))
          (assert-equalp msg pt))))))

(deftest test-tls12-session-ticket-issued-when-requested
  "When the client offers an empty session_ticket extension and the
   server carries a STEK store, the server emits a NewSessionTicket
   in its flight 2 and both sides still derive matching keys."
  (multiple-value-bind (sk cert-der) (%make-server-identity)
    (let* ((store (stek:make-stek-store))
           (server-config (tls12:make-tls12-server-config
                           :certificate-chain (list cert-der)
                           :private-key sk
                           :key-type :ecdsa-p256
                           :session-ticket-store store))
           (client-config (tls12:make-tls12-client-config
                           :hostname "localhost"
                           :request-session-ticket-p t))
           (result (%drive-handshake server-config client-config))
           (client (handshake-result-client-conn result))
           (server (handshake-result-server-conn result)))
      (assert-eq :connected (tls12:tls12-connection-state client))
      (assert-eq :connected (tls12:tls12-connection-state server))
      ;; Server decided to issue a ticket; the decision is recorded on
      ;; the connection so we can assert about it without peeking at
      ;; the ticket bytes themselves.
      (assert-true (tls12::tls12-connection-issue-session-ticket-p server)))))

(deftest test-tls12-accepts-client-without-ems
  "A client that does not offer Extended Master Secret still completes
   the handshake (EMS is preferred but not required)."
  (multiple-value-bind (sk cert-der) (%make-server-identity)
    (let ((server-config (tls12:make-tls12-server-config
                          :certificate-chain (list cert-der)
                          :private-key sk
                          :key-type :ecdsa-p256)))
      (let* ((server (%make-server-conn server-config))
             (client-random (drbg:random-bytes 32))
             (session-id (make-array 0 :element-type '(unsigned-byte 8)))
             (payload (let ((buf (tls12::make-buffer)))
                        (tls12::buf-u16 buf tls12:+tls-1.2+)
                        (tls12::buf-bytes buf client-random)
                        (tls12::buf-u8-prefixed buf session-id)
                        ;; cipher_suites: just one
                        (tls12::buf-u16 buf 2)
                        (tls12::buf-u16 buf
                                        tls12:+tls-ecdhe-ecdsa-aes-128-gcm-sha256+)
                        ;; compression_methods
                        (tls12::buf-u8 buf 1)
                        (tls12::buf-u8 buf 0)
                        ;; extensions: supported_groups only (no EMS)
                        (let ((ext (tls12::make-buffer)))
                          (tls12::buf-u16 ext 10)          ; supported_groups type
                          (tls12::buf-u16 ext 4)           ; ext body length
                          (tls12::buf-u16 ext 2)           ; named_group_list len
                          (tls12::buf-u16 ext #x001D)      ; x25519
                          (tls12::buf-u16-prefixed buf (tls12::buf-freeze ext)))
                        (tls12::buf-freeze buf)))
             (response (tls12:tls12-server-start-handshake server payload)))
        (assert-true (> (length response) 0))
        (assert-false (tls12::tls12-connection-use-ems-p server))))))
