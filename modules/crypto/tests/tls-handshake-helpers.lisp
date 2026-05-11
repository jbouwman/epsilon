;;;; Shared in-process TLS 1.3 handshake helpers for the epsilon.crypto
;;;; test suite. The framing here is delicate enough (record lengths
;;;; computed from the wire bytes, the post-handshake NewSessionTicket
;;;; record drained by hand, the in-memory STEK store wired into the
;;;; server config) that two near-identical copies inevitably drift.
;;;; Concentrating it here keeps tls13-tests.lisp and the
;;;; tls-session-ticket-cache tests honest about what they're driving.

(defpackage epsilon.crypto.tls-handshake-helpers
  (:use :cl)
  (:import
   (epsilon.crypto.tls13 tls)
   (epsilon.crypto.tls-session-ticket-store stek)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.drbg drbg))
  (:export
   #:make-resumption-server-config
   #:drive-fresh-handshake
   #:drive-psk-handshake-over-pipes))

(in-package :epsilon.crypto.tls-handshake-helpers)

(defun make-resumption-server-config (store)
  "Self-signed Ed25519 cert + caller-supplied STEK store. The STEK
   store lets two successive handshakes share ticket state across
   server connections."
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :subject "localhost"
                    :dns-names '("localhost"))))
    (tls:make-tls-server-config
     :certificate-chain (list cert-der)
     :private-key sk
     :key-type :ed25519
     :session-ticket-store store)))

(defun %iter-records (bytes thunk)
  "Walk the concatenated TLS records in BYTES, calling THUNK on each
   record's full wire bytes."
  (let ((pos 0))
    (loop while (< pos (length bytes))
          do (let* ((rlen (+ 5 (ash (aref bytes (+ pos 3)) 8)
                               (aref bytes (+ pos 4))))
                    (rbytes (subseq bytes pos (+ pos rlen))))
               (funcall thunk rbytes)
               (incf pos rlen)))))

(defun drive-fresh-handshake (config &key callback)
  "Run a fresh client+server handshake against CONFIG, deliver the
   client's Finished, then deliver the server's NewSessionTicket back
   to the client.

   When CALLBACK is non-NIL it is wired into the client connection's
   :new-session-ticket-callback slot and fires for every issued
   ticket as the client drains the post-handshake record.

   Returns (values client server first-ticket), where FIRST-TICKET
   is the first NewSessionTicket the client received (or NIL if the
   server didn't issue one)."
  (let ((client (tls:make-tls-connection :hostname "localhost"
                                         :new-session-ticket-callback callback))
        (server (tls:make-tls-connection :role :server)))
    (setf (tls::tls-connection-server-config server) config)
    (let* ((ch-record-bytes (tls::tls-start-handshake client))
           (ch-record (tls:parse-tls-record ch-record-bytes))
           (ch-payload (subseq (tls:tls-record-data ch-record) 4))
           (server-response (tls:tls-server-start-handshake server ch-payload))
           (client-finished nil))
      ;; Drive the client through the server's first flight; the last
      ;; non-NIL byte-vector return is the client's Finished message.
      (%iter-records server-response
                     (lambda (rbytes)
                       (let ((resp (tls:tls-handshake-step client rbytes)))
                         (when (typep resp '(simple-array (unsigned-byte 8) (*)))
                           (setf client-finished resp)))))
      ;; Hand client Finished back to the server; collect any
      ;; post-handshake records (NewSessionTicket lives here).
      (let ((ticket-records nil))
        (%iter-records client-finished
                       (lambda (rbytes)
                         (let ((record (tls:parse-tls-record rbytes)))
                           (when (= (tls:tls-record-content-type record)
                                    tls:+content-application-data+)
                             (setf ticket-records
                                   (tls:tls-server-process-finished
                                    server (tls:tls-record-data record)))))))
        ;; Drain the server's NewSessionTicket on the client side --
        ;; this is what fires tls-process-post-handshake and, in turn,
        ;; the callback.
        (when ticket-records
          (%iter-records ticket-records
                         (lambda (rbytes)
                           (let ((record (tls:parse-tls-record rbytes)))
                             (tls:tls-receive-application-data
                              client (tls:tls-record-data record))))))))
    (values client
            server
            (first (tls::tls-connection-received-tickets client)))))
