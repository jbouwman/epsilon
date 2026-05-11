;;;; Tests for the client-side TLS session ticket cache.

(defpackage epsilon.crypto.tls-session-ticket-cache-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.tls-session-ticket-cache cache)
   (epsilon.crypto.tls13 tls)
   (epsilon.crypto.tls-session-ticket-store stek)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.tls-handshake-helpers helpers)))

(in-package :epsilon.crypto.tls-session-ticket-cache-tests)

;;; A nominal port we use for every cache key in this file. The TLS
;;; layer is in-process (no real socket); the value just exercises the
;;; (host, port) keying.
(defparameter +test-port+ 4433)

;;; ---------------------------------------------------------------------------
;;; Synthetic-ticket helpers
;;; ---------------------------------------------------------------------------

(defun %fingerprint-of-bytes (n &optional (seed 0))
  (let ((v (make-array 32 :element-type '(unsigned-byte 8))))
    (dotimes (i 32) (setf (aref v i) (mod (+ n seed (* 13 i)) 256)))
    v))

(defun %make-ticket (&key (lifetime 3600) (creation-time (get-universal-time))
                          (cipher-suite tls:+tls-aes-128-gcm-sha256+))
  (tls:make-tls-session-ticket
   :ticket (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7)
   :lifetime lifetime
   :age-add 12345
   :nonce (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
   :max-early-data 0
   :cipher-suite cipher-suite
   :resumption-secret (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1)
   :creation-time creation-time))

;;; ---------------------------------------------------------------------------
;;; Cache mechanics
;;; ---------------------------------------------------------------------------

(deftest test-empty-cache-returns-nil
  "A fresh cache returns NIL for any endpoint."
  (let ((c (cache:make-session-ticket-cache)))
    (assert-nil (cache:session-ticket-cache-get c "example.com" +test-port+))
    (assert-= 0 (cache:session-ticket-cache-size c))))

(deftest test-put-then-get-roundtrip
  "Put then get returns the same ticket and increments size."
  (let ((c (cache:make-session-ticket-cache))
        (fp (%fingerprint-of-bytes 1))
        (t1 (%make-ticket)))
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp t1)
    (assert-= 1 (cache:session-ticket-cache-size c))
    (assert-eq t1 (cache:session-ticket-cache-get c "example.com" +test-port+))
    (assert-equalp fp
                   (cache:session-ticket-cache-fingerprint
                    c "example.com" +test-port+))))

(deftest test-port-disambiguates-host
  "Same host on two different ports holds two independent entries."
  (let ((c (cache:make-session-ticket-cache))
        (fp1 (%fingerprint-of-bytes 1))
        (fp2 (%fingerprint-of-bytes 2))
        (t1 (%make-ticket))
        (t2 (%make-ticket)))
    (cache:session-ticket-cache-put c "example.com" 443 fp1 t1)
    (cache:session-ticket-cache-put c "example.com" 8443 fp2 t2)
    (assert-= 2 (cache:session-ticket-cache-size c))
    (assert-eq t1 (cache:session-ticket-cache-get c "example.com" 443))
    (assert-eq t2 (cache:session-ticket-cache-get c "example.com" 8443))
    ;; Invalidating one does not touch the other.
    (cache:session-ticket-cache-invalidate c "example.com" 443)
    (assert-nil (cache:session-ticket-cache-get c "example.com" 443))
    (assert-eq t2 (cache:session-ticket-cache-get c "example.com" 8443))))

(deftest test-same-fingerprint-update-replaces-ticket
  "Re-putting under the same fingerprint replaces the ticket but keeps the entry."
  (let ((c (cache:make-session-ticket-cache))
        (fp (%fingerprint-of-bytes 1))
        (t1 (%make-ticket))
        (t2 (%make-ticket)))
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp t1)
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp t2)
    (assert-= 1 (cache:session-ticket-cache-size c))
    (assert-eq t2 (cache:session-ticket-cache-get c "example.com" +test-port+))))

(deftest test-cert-rotation-replaces-entry
  "Putting under a different fingerprint atomically replaces the prior entry."
  (let ((c (cache:make-session-ticket-cache))
        (fp1 (%fingerprint-of-bytes 1))
        (fp2 (%fingerprint-of-bytes 2))
        (t1 (%make-ticket))
        (t2 (%make-ticket)))
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp1 t1)
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp2 t2)
    (assert-= 1 (cache:session-ticket-cache-size c))
    (assert-eq t2 (cache:session-ticket-cache-get c "example.com" +test-port+))
    (assert-equalp fp2
                   (cache:session-ticket-cache-fingerprint
                    c "example.com" +test-port+))))

(deftest test-expired-ticket-evicted-on-get
  "A get past the ticket lifetime returns NIL and removes the entry."
  (let* ((c (cache:make-session-ticket-cache))
         (fp (%fingerprint-of-bytes 1))
         (now (get-universal-time))
         (expired (%make-ticket :lifetime 3600
                                :creation-time (- now 7200))))
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp expired)
    (assert-= 1 (cache:session-ticket-cache-size c))
    (assert-nil (cache:session-ticket-cache-get c "example.com" +test-port+))
    (assert-= 0 (cache:session-ticket-cache-size c))))

(deftest test-zero-lifetime-treated-as-expired
  "A ticket with lifetime=0 is unusable and never returned."
  (let ((c (cache:make-session-ticket-cache))
        (fp (%fingerprint-of-bytes 1))
        (t1 (%make-ticket :lifetime 0)))
    (cache:session-ticket-cache-put c "example.com" +test-port+ fp t1)
    (assert-nil (cache:session-ticket-cache-get c "example.com" +test-port+))))

(deftest test-invalidate-removes-entry
  "Invalidate drops an endpoint's entry; subsequent gets return NIL."
  (let ((c (cache:make-session-ticket-cache)))
    (cache:session-ticket-cache-put c "a.example" +test-port+
                                    (%fingerprint-of-bytes 1) (%make-ticket))
    (cache:session-ticket-cache-put c "b.example" +test-port+
                                    (%fingerprint-of-bytes 2) (%make-ticket))
    (cache:session-ticket-cache-invalidate c "a.example" +test-port+)
    (assert-nil (cache:session-ticket-cache-get c "a.example" +test-port+))
    (assert-not-null (cache:session-ticket-cache-get c "b.example" +test-port+))
    (assert-= 1 (cache:session-ticket-cache-size c))))

(deftest test-clear-empties-cache
  "Clear removes every entry."
  (let ((c (cache:make-session-ticket-cache)))
    (dotimes (i 5)
      (cache:session-ticket-cache-put c (format nil "host-~D" i) +test-port+
                                      (%fingerprint-of-bytes i)
                                      (%make-ticket)))
    (assert-= 5 (cache:session-ticket-cache-size c))
    (cache:session-ticket-cache-clear c)
    (assert-= 0 (cache:session-ticket-cache-size c))))

(deftest test-lru-eviction-on-overflow
  "Beyond max-entries, the least-recently-used entry is dropped."
  (let ((c (cache:make-session-ticket-cache :max-entries 3)))
    (cache:session-ticket-cache-put c "h1" +test-port+
                                    (%fingerprint-of-bytes 1) (%make-ticket))
    (sleep 1)
    (cache:session-ticket-cache-put c "h2" +test-port+
                                    (%fingerprint-of-bytes 2) (%make-ticket))
    (sleep 1)
    (cache:session-ticket-cache-put c "h3" +test-port+
                                    (%fingerprint-of-bytes 3) (%make-ticket))
    (sleep 1)
    (cache:session-ticket-cache-put c "h4" +test-port+
                                    (%fingerprint-of-bytes 4) (%make-ticket))
    (assert-= 3 (cache:session-ticket-cache-size c))
    (assert-nil (cache:session-ticket-cache-get c "h1" +test-port+))
    (assert-not-null (cache:session-ticket-cache-get c "h4" +test-port+))))

(deftest test-cert-fingerprint-is-sha256
  "cert-fingerprint computes a 32-byte SHA-256 of the input."
  (let* ((bytes (make-array 64 :element-type '(unsigned-byte 8)))
         (fp (cache:cert-fingerprint bytes)))
    (dotimes (i 64) (setf (aref bytes i) (mod (* 5 i) 256)))
    (assert-= 32 (length fp))
    ;; Different input -> different fingerprint.
    (assert-not (equalp fp (cache:cert-fingerprint bytes)))))

;;; ---------------------------------------------------------------------------
;;; tls-connect bridge: end-to-end against the in-process server config.
;;; ---------------------------------------------------------------------------

(deftest test-callback-fires-with-leaf-cert-fingerprint
  "On a fresh handshake, the registered callback receives the new ticket
   and the connection still carries the server cert chain so the
   cache-callback can compute a leaf fingerprint."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (received-tickets (list nil))
         (received-fingerprints (list nil))
         (cb (lambda (conn ticket)
               (push ticket (first received-tickets))
               (let ((leaf (first (tls:tls-connection-server-certificates conn))))
                 (when leaf
                   (push (cache:cert-fingerprint (x509:x509-cert-raw-bytes leaf))
                         (first received-fingerprints)))))))
    (helpers:drive-fresh-handshake config :callback cb)
    (assert-true (first received-tickets))
    (assert-true (first received-fingerprints))
    (let ((fp (first (first received-fingerprints))))
      (assert-= 32 (length fp)))))

(deftest test-cache-callback-populates-on-fresh-handshake
  "make-cache-callback wired into a fresh handshake populates the cache
   with the issued ticket under the leaf cert's SHA-256."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    (multiple-value-bind (client server first-ticket)
        (helpers:drive-fresh-handshake config :callback cb)
      (declare (ignore server first-ticket))
      (assert-true (cache:session-ticket-cache-get c "localhost" +test-port+))
      (let ((leaf (first (tls:tls-connection-server-certificates client))))
        (assert-equalp (cache:cert-fingerprint (x509:x509-cert-raw-bytes leaf))
                       (cache:session-ticket-cache-fingerprint
                        c "localhost" +test-port+))))))

(deftest test-callback-errors-do-not-crash-handshake
  "A callback that signals must not propagate past tls-process-post-handshake."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (cb (lambda (conn ticket)
               (declare (ignore conn ticket))
               (error "intentional"))))
    ;; Should complete without an unhandled error.
    (multiple-value-bind (client server ticket)
        (helpers:drive-fresh-handshake config :callback cb)
      (declare (ignore server))
      (assert-true client)
      (assert-true ticket))))

(deftest test-psk-resumption-after-cert-rotation-keeps-stale-fingerprint
  "Document the stored-fingerprint-can-lag invariant: when the server
   rotates its cert but its STEK still validates a prior ticket, a PSK
   resumption succeeds without sending Certificate, and any newly
   issued ticket is tagged with the *old* fingerprint -- there is no
   cert evidence to record otherwise. The next fresh handshake (when
   the old ticket expires or is rejected) is what catches up.

   This test pins that behavior so a future refactor doesn't silently
   start clobbering the fingerprint to NIL on PSK or skipping the put
   entirely."
  (let* ((store (stek:make-stek-store))
         (config-a (helpers:make-resumption-server-config store))
         (config-b (helpers:make-resumption-server-config store))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    ;; Handshake 1: against server config A -> populates cache with fp(A).
    (helpers:drive-fresh-handshake config-a :callback cb)
    (let ((fp-after-a
            (cache:session-ticket-cache-fingerprint c "localhost" +test-port+)))
      (assert-not-null fp-after-a)
      ;; Handshake 2: also a fresh handshake, but now against config B
      ;; (a different self-signed cert). This is the cert-rotation
      ;; replacement path -- fp should change in-place.
      (helpers:drive-fresh-handshake config-b :callback cb)
      (let ((fp-after-b
              (cache:session-ticket-cache-fingerprint c "localhost" +test-port+)))
        (assert-not-null fp-after-b)
        (assert-not (equalp fp-after-a fp-after-b))
        ;; Cache still holds exactly one entry for this endpoint.
        (assert-= 1 (cache:session-ticket-cache-size c))))))

;;; ---------------------------------------------------------------------------
;;; End-to-end: cache populated by handshake #1 actually drives PSK
;;; resumption on handshake #2. Pins the cache's whole reason to exist.
;;; ---------------------------------------------------------------------------

(defun %psk-handshake-using-cached-ticket (config c hostname port)
  "Drive a second handshake that picks up its session ticket from C
   via the cache lookup, then performs PSK resumption against CONFIG.
   Returns (values client-conn server-conn) for inspection."
  (let* ((cached (cache:session-ticket-cache-get c hostname port))
         (cb (cache:make-cache-callback c hostname port))
         (client (tls:make-tls-connection :hostname hostname
                                          :new-session-ticket-callback cb))
         (server (tls:make-tls-connection :role :server)))
    (assert cached () "cache-driven test requires a populated cache entry")
    (setf (tls::tls-connection-server-config server) config)
    (let* ((ch-record-bytes (tls:build-psk-client-hello client cached))
           (ch-record (tls:parse-tls-record ch-record-bytes))
           (ch-payload (subseq (tls:tls-record-data ch-record) 4))
           (server-response (tls:tls-server-start-handshake server ch-payload))
           (client-finished nil)
           (pos 0))
      (loop while (< pos (length server-response))
            do (let* ((rlen (+ 5 (ash (aref server-response (+ pos 3)) 8)
                                 (aref server-response (+ pos 4))))
                      (rbytes (subseq server-response pos (+ pos rlen)))
                      (resp (tls:tls-handshake-step client rbytes)))
                 (when (typep resp '(simple-array (unsigned-byte 8) (*)))
                   (setf client-finished resp))
                 (incf pos rlen)))
      ;; Hand client Finished back; fold any post-handshake records
      ;; (NewSessionTicket on this PSK-resumed handshake) back into
      ;; the client so the callback fires.
      (let ((fpos 0)
            (post-records nil))
        (loop while (< fpos (length client-finished))
              do (let* ((rlen (+ 5 (ash (aref client-finished (+ fpos 3)) 8)
                                   (aref client-finished (+ fpos 4))))
                        (rbytes (subseq client-finished fpos (+ fpos rlen)))
                        (record (tls:parse-tls-record rbytes)))
                   (when (= (tls:tls-record-content-type record)
                            tls:+content-application-data+)
                     (setf post-records
                           (tls:tls-server-process-finished
                            server (tls:tls-record-data record))))
                   (incf fpos rlen)))
        (when post-records
          (let ((tpos 0))
            (loop while (< tpos (length post-records))
                  do (let* ((rlen (+ 5 (ash (aref post-records (+ tpos 3)) 8)
                                       (aref post-records (+ tpos 4))))
                            (rbytes (subseq post-records tpos (+ tpos rlen)))
                            (record (tls:parse-tls-record rbytes)))
                       (tls:tls-receive-application-data
                        client (tls:tls-record-data record))
                       (incf tpos rlen)))))))
    (values client server)))

(deftest test-cached-ticket-drives-second-handshake-as-psk
  "End-to-end: handshake #1 populates the cache via the callback;
   handshake #2 reads the ticket out of the cache, presents it, and
   the server reports the connection as resumed. Without this test,
   a future refactor of tls-connect's keyword forwarding could
   silently break the PSK path and the rest of the suite would still
   pass."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    ;; Handshake 1: fresh, populates cache.
    (helpers:drive-fresh-handshake config :callback cb)
    (assert-true (cache:session-ticket-cache-get c "localhost" +test-port+))
    ;; Handshake 2: cache lookup -> PSK ClientHello -> resumed.
    (multiple-value-bind (client server)
        (%psk-handshake-using-cached-ticket config c "localhost" +test-port+)
      (declare (ignore client))
      (assert-true (tls:tls-connection-resumed-p server)))))

;;; ---------------------------------------------------------------------------
;;; PSK rejection: client must observe the no-pre_shared_key signal so
;;; the cache wrapper can invalidate stale entries.
;;; ---------------------------------------------------------------------------

(deftest test-client-resumed-p-set-when-psk-accepted
  "After a successful PSK handshake, both client and server connections
   carry resumed-p T. The client side is the new signal -- it's what
   tls-connect-cached uses to decide whether to keep or drop the
   cache entry."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    (helpers:drive-fresh-handshake config :callback cb)
    (multiple-value-bind (client server)
        (%psk-handshake-using-cached-ticket config c "localhost" +test-port+)
      (assert-true (tls:tls-connection-resumed-p server))
      (assert-true (tls:tls-connection-resumed-p client)))))

(deftest test-client-resumed-p-stays-nil-when-psk-rejected
  "If the server has a different STEK store, %try-pick-psk-resumption
   returns NIL and the server falls through to a fresh handshake.
   The client's resumed-p must stay NIL so a cache wrapper can tell
   the offered ticket was stale.

   Note: this also exercises the protocol-level fix that wipes the
   PSK-derived early-secret on PSK rejection -- without it, the
   handshake's AEAD would diverge between client and server."
  (let* ((store-a (stek:make-stek-store))
         (store-b (stek:make-stek-store))
         (config-a (helpers:make-resumption-server-config store-a))
         (config-b (helpers:make-resumption-server-config store-b))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    ;; Cache populated by config-a / store-a.
    (helpers:drive-fresh-handshake config-a :callback cb)
    (assert-true (cache:session-ticket-cache-get c "localhost" +test-port+))
    ;; Now offer that ticket to config-b, whose STEK can't open it.
    (multiple-value-bind (client server)
        (%psk-handshake-using-cached-ticket config-b c "localhost" +test-port+)
      (assert-not (tls:tls-connection-resumed-p server))
      (assert-not (tls:tls-connection-resumed-p client))
      ;; The server fell back to a fresh handshake: it must have sent
      ;; its certificate chain (PSK resumption skips Certificate).
      (assert-true (tls:tls-connection-server-certificates client)))))

;;; ---------------------------------------------------------------------------
;;; tls-connect-cached's auto-invalidate: with the protocol-level
;;; resumed-p signal in hand, the wrapper drops the cache entry when
;;; the server rejected the offered ticket. We exercise the wrapper's
;;; decision logic directly here, mirroring the resumed-p observation
;;; from the previous test.
;;; ---------------------------------------------------------------------------

(deftest test-cache-invalidated-after-psk-rejection
  "Simulating tls-connect-cached's post-connect bookkeeping: when the
   wrapper observes resumed-p NIL on a connection where it supplied
   the ticket from the cache, it invalidates the entry. Asserting
   directly because spinning up tls-connect over a real transport for
   this one bit would carry a TCP-loopback dependency this test
   doesn't otherwise need."
  (let* ((store-a (stek:make-stek-store))
         (store-b (stek:make-stek-store))
         (config-a (helpers:make-resumption-server-config store-a))
         (config-b (helpers:make-resumption-server-config store-b))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    (helpers:drive-fresh-handshake config-a :callback cb)
    (assert-true (cache:session-ticket-cache-get c "localhost" +test-port+))
    (multiple-value-bind (client server)
        (%psk-handshake-using-cached-ticket config-b c "localhost" +test-port+)
      (declare (ignore server))
      ;; This is the exact predicate from tls-connect-cached.
      (unless (tls:tls-connection-resumed-p client)
        (cache:session-ticket-cache-invalidate c "localhost" +test-port+)))
    (assert-nil (cache:session-ticket-cache-get c "localhost" +test-port+))
    (assert-= 0 (cache:session-ticket-cache-size c))))

(deftest test-cache-retained-after-successful-resumption
  "Mirror of the previous test for the happy path: the wrapper's
   predicate leaves the cache entry alone when resumed-p is T."
  (let* ((store (stek:make-stek-store))
         (config (helpers:make-resumption-server-config store))
         (c (cache:make-session-ticket-cache))
         (cb (cache:make-cache-callback c "localhost" +test-port+)))
    (helpers:drive-fresh-handshake config :callback cb)
    (multiple-value-bind (client server)
        (%psk-handshake-using-cached-ticket config c "localhost" +test-port+)
      (declare (ignore server))
      (unless (tls:tls-connection-resumed-p client)
        (cache:session-ticket-cache-invalidate c "localhost" +test-port+)))
    (assert-true (cache:session-ticket-cache-get c "localhost" +test-port+))))
