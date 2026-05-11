;;;; Client-side TLS session ticket cache.
;;;;
;;;; A persistent (in-process) bridge between the bare client primitives
;;;; (`build-psk-client-hello`, `parse-new-session-ticket`) and the
;;;; high-level `tls-connect` API. The caller hands a cache to many
;;;; `tls-connect` calls; the first connection to a (host, port) pair
;;;; costs a full handshake, subsequent connections to the same endpoint
;;;; present the PSK identity from the most recent NewSessionTicket and
;;;; skip the CertificateVerify round-trip.
;;;;
;;;; Keying: cache entries are addressed by (host, port). Two services
;;;; on different ports of the same host produce different STEKs; using
;;;; a port-blind key would let a ticket sealed by one server be offered
;;;; to another and trigger a fall-back to a fresh handshake every time.
;;;; Each entry carries the latest unexpired ticket and the SHA-256
;;;; fingerprint of the leaf certificate observed on the handshake that
;;;; produced it. A subsequent fresh handshake against the same endpoint
;;;; with a different fingerprint atomically replaces the prior entry --
;;;; tickets sealed under the old STEK are stale anyway, and presenting
;;;; them would only force a fall-back.
;;;;
;;;; Eviction:
;;;;   - Lazy: a get past the ticket's `creation_time + lifetime` returns
;;;;     NIL and removes the entry.
;;;;   - Bounded: when the entry count exceeds `max-entries`, the
;;;;     least-recently-used entry is dropped.
;;;;
;;;; Thread safety: a single mutex serialises every mutation and lookup.
;;;; The map is small (one entry per endpoint) and operations are O(1)
;;;; amortised, so a coarse lock is the right answer.

(defpackage epsilon.crypto.tls-session-ticket-cache
  (:use :cl)
  (:import
   (epsilon.sys.lock lock)
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.tls13 tls13)
   (epsilon.crypto.x509 x509))
  (:export
   #:session-ticket-cache
   #:session-ticket-cache-p
   #:make-session-ticket-cache
   #:session-ticket-cache-put
   #:session-ticket-cache-get
   #:session-ticket-cache-fingerprint
   #:session-ticket-cache-invalidate
   #:session-ticket-cache-clear
   #:session-ticket-cache-size
   #:cert-fingerprint
   #:make-cache-callback
   #:tls-connect-cached))

(in-package :epsilon.crypto.tls-session-ticket-cache)

(defstruct cache-entry
  "One ticket plus the leaf-cert fingerprint of the handshake that
   produced it. `last-used' is universal-time and drives LRU eviction."
  (fingerprint nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (ticket nil)
  (last-used 0 :type integer))

(defstruct (session-ticket-cache (:constructor %make-session-ticket-cache))
  "Persistent client cache of issued NewSessionTicket payloads, one
   entry per (host, port). Pass to repeated `tls-connect' calls to
   amortise the cost of the first full handshake across subsequent
   connects."
  (lock (lock:make-lock "tls-session-ticket-cache") :type lock:lock)
  (entries (make-hash-table :test 'equal) :type hash-table)
  (max-entries 1024 :type (integer 1)))

(defun make-session-ticket-cache (&key (max-entries 1024))
  "Create an empty cache. MAX-ENTRIES bounds how many distinct endpoints
   the cache will hold; on overflow the least-recently-used entry is
   evicted."
  (check-type max-entries (integer 1))
  (%make-session-ticket-cache :max-entries max-entries))

(defun cert-fingerprint (cert-der)
  "Return the SHA-256 fingerprint (32 bytes) of a DER-encoded
   certificate. The leaf cert is the conventional choice; the
   intermediates are not part of the identity from the cache's
   perspective."
  (declare (type (simple-array (unsigned-byte 8) (*)) cert-der))
  (sha256:sha256 cert-der))

(defun %endpoint-key (host port)
  (format nil "~A:~D" host port))

(defun %ticket-expired-p (ticket now)
  "RFC 8446 4.6.1: a ticket is valid for `lifetime` seconds after
   `creation_time`. We treat 0-lifetime as never-valid (servers should
   never issue this, but it would be wasteful to cache an unusable
   ticket)."
  (let ((lifetime (tls13:tls-session-ticket-lifetime ticket))
        (created (tls13:tls-session-ticket-creation-time ticket)))
    (or (zerop lifetime)
        (>= now (+ created lifetime)))))

(defun %evict-lru-locked (cache)
  "Drop the least-recently-used entry. Caller must hold the lock."
  (let ((entries (session-ticket-cache-entries cache))
        (oldest-key nil)
        (oldest-time most-positive-fixnum))
    (maphash (lambda (key entry)
               (when (< (cache-entry-last-used entry) oldest-time)
                 (setf oldest-time (cache-entry-last-used entry))
                 (setf oldest-key key)))
             entries)
    (when oldest-key
      (remhash oldest-key entries))))

(defun session-ticket-cache-put (cache host port fingerprint ticket)
  "Record TICKET against (HOST, PORT). FINGERPRINT is the SHA-256 of
   the handshake's leaf certificate (see `cert-fingerprint'). If the
   existing entry's fingerprint differs from FINGERPRINT, the prior
   entry is replaced wholesale -- a cert rotation has occurred and
   the prior ticket is no longer expected to validate."
  (check-type host string)
  (check-type port (integer 1 65535))
  (check-type fingerprint (simple-array (unsigned-byte 8) (*)))
  (lock:with-lock ((session-ticket-cache-lock cache))
    (let* ((entries (session-ticket-cache-entries cache))
           (key (%endpoint-key host port))
           (now (get-universal-time))
           (existing (gethash key entries)))
      (cond
        ((and existing (equalp (cache-entry-fingerprint existing) fingerprint))
         (setf (cache-entry-ticket existing) ticket)
         (setf (cache-entry-last-used existing) now))
        (t
         (setf (gethash key entries)
               (make-cache-entry :fingerprint fingerprint
                                 :ticket ticket
                                 :last-used now))
         (when (> (hash-table-count entries)
                  (session-ticket-cache-max-entries cache))
           (%evict-lru-locked cache))))))
  ticket)

(defun session-ticket-cache-get (cache host port)
  "Return the latest unexpired ticket for (HOST, PORT), or NIL. Touches
   the LRU timestamp on a hit. Expired entries are evicted as a side
   effect of a get -- there is no background sweeper."
  (check-type host string)
  (check-type port (integer 1 65535))
  (lock:with-lock ((session-ticket-cache-lock cache))
    (let* ((entries (session-ticket-cache-entries cache))
           (key (%endpoint-key host port))
           (entry (gethash key entries)))
      (cond
        ((null entry) nil)
        ((%ticket-expired-p (cache-entry-ticket entry) (get-universal-time))
         (remhash key entries)
         nil)
        (t
         (setf (cache-entry-last-used entry) (get-universal-time))
         (cache-entry-ticket entry))))))

(defun session-ticket-cache-fingerprint (cache host port)
  "Return the SHA-256 fingerprint stored alongside (HOST, PORT)'s
   ticket, or NIL. Useful for callers that want to confirm a peer
   cert matches the one observed on the cached ticket's originating
   handshake.

   Note: the stored fingerprint can lag the live cert. After a server
   rotates its cert but its STEK still validates the old ticket, a
   PSK resumption succeeds without sending Certificate; subsequent
   tickets issued during that resumption are tagged with the prior
   fingerprint, since there is no cert evidence to record otherwise.
   The next *fresh* handshake with the new cert is what actually
   replaces the entry."
  (check-type host string)
  (check-type port (integer 1 65535))
  (lock:with-lock ((session-ticket-cache-lock cache))
    (let ((entry (gethash (%endpoint-key host port)
                          (session-ticket-cache-entries cache))))
      (and entry (cache-entry-fingerprint entry)))))

(defun session-ticket-cache-invalidate (cache host port)
  "Drop (HOST, PORT)'s entry, if any. Called explicitly when application
   code knows a ticket has been rejected (e.g. server returned a fresh
   handshake response to a PSK ClientHello)."
  (check-type host string)
  (check-type port (integer 1 65535))
  (lock:with-lock ((session-ticket-cache-lock cache))
    (remhash (%endpoint-key host port)
             (session-ticket-cache-entries cache)))
  nil)

(defun session-ticket-cache-clear (cache)
  "Remove every entry."
  (lock:with-lock ((session-ticket-cache-lock cache))
    (clrhash (session-ticket-cache-entries cache)))
  nil)

(defun session-ticket-cache-size (cache)
  "Return the number of cached endpoint entries (after lazy eviction
   has been applied by previous gets; expired entries that have not
   been touched still count)."
  (lock:with-lock ((session-ticket-cache-lock cache))
    (hash-table-count (session-ticket-cache-entries cache))))

;;; ---------------------------------------------------------------------------
;;; tls-connect bridge
;;; ---------------------------------------------------------------------------
;;;
;;; The TLS layer publishes a `(callback conn ticket)` hook that fires
;;; for every NewSessionTicket received on a client connection. The two
;;; helpers below wire that hook to a session ticket cache:
;;;
;;;   - `make-cache-callback` returns a closure suitable for
;;;     `tls-connect`'s :new-session-ticket-callback. It infers the
;;;     leaf-cert fingerprint from the connection's server certificate
;;;     chain (fresh handshake) or from the cache's existing entry
;;;     (PSK resumption -- no Certificate is sent in that case).
;;;
;;;   - `tls-connect-cached` is the one-call ergonomic wrapper:
;;;     looks up an existing ticket for (HOSTNAME, PORT) and supplies
;;;     the callback in one step.

(defun make-cache-callback (cache hostname port)
  "Return a function that puts every NewSessionTicket received on a
   connection into CACHE under (HOSTNAME, PORT).

   On a fresh handshake, the fingerprint comes from the peer's leaf
   certificate. On a PSK resumption -- where no Certificate is sent --
   the cache's existing fingerprint is reused, which means the
   recorded fingerprint can lag the live cert: a server that rotated
   its cert but still accepts the old ticket via PSK will get its
   newly issued ticket tagged with the prior fingerprint. The next
   fresh handshake (whenever the old ticket expires or the server
   stops accepting it) is what replaces the entry's fingerprint with
   the new cert's. Callers that need point-in-time identity should
   verify the peer cert independently rather than rely on this slot."
  (check-type hostname string)
  (check-type port (integer 1 65535))
  (lambda (conn ticket)
    (let* ((certs (tls13:tls-connection-server-certificates conn))
           (leaf (and certs (first certs)))
           (fingerprint (cond
                          (leaf (cert-fingerprint (x509:x509-cert-raw-bytes leaf)))
                          (t (session-ticket-cache-fingerprint cache hostname port)))))
      (when fingerprint
        (session-ticket-cache-put cache hostname port fingerprint ticket)))))

(defun tls-connect-cached (transport cache &rest tls-connect-args
                           &key hostname port &allow-other-keys)
  "Wrap `tls13:tls-connect' with CACHE-backed PSK session resumption.

   On entry, the most recent unexpired ticket for (HOSTNAME, PORT) is
   offered in the ClientHello. On exit, the connection's
   :new-session-ticket-callback is wired so that any NewSessionTicket
   the server emits during or after the handshake refreshes the
   cache entry. Caller-supplied :session-ticket and
   :new-session-ticket-callback override the cache wiring.

   When a cache-supplied ticket is offered and the server rejects
   PSK (no pre_shared_key extension echoed in ServerHello), the
   cache entry for (HOSTNAME, PORT) is invalidated so the next
   connect doesn't keep re-presenting it. Caller-supplied
   :session-ticket bypasses this auto-invalidate -- the caller owns
   that ticket's lifecycle.

   PORT is required: the cache key includes it so that two services
   on different ports of the same host don't trample each other's
   tickets. The TLS layer itself doesn't know what port TRANSPORT is
   talking to -- that's why we ask the caller."
  (check-type hostname string)
  (check-type port (integer 1 65535))
  (let ((effective-args (copy-list tls-connect-args))
        (cache-supplied-ticket nil))
    ;; tls-connect doesn't accept :port; strip it before forwarding.
    (remf effective-args :port)
    (unless (getf effective-args :session-ticket)
      (let ((cached (session-ticket-cache-get cache hostname port)))
        (when cached
          (setf (getf effective-args :session-ticket) cached)
          (setf cache-supplied-ticket cached))))
    (unless (getf effective-args :new-session-ticket-callback)
      (setf (getf effective-args :new-session-ticket-callback)
            (make-cache-callback cache hostname port)))
    (let ((stream (apply #'tls13:tls-connect transport effective-args)))
      (when cache-supplied-ticket
        (let ((conn (tls13:tls-stream-connection stream)))
          (unless (tls13:tls-connection-resumed-p conn)
            (session-ticket-cache-invalidate cache hostname port))))
      stream)))
