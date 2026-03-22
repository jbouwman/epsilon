;;;; QUIC-TLS: Record-Layer-Free TLS 1.3 for QUIC (RFC 9001)
;;;;
;;;; QUIC replaces the TLS record layer with its own CRYPTO frames,
;;;; so this module provides raw handshake message handling without
;;;; record wrapping or encryption.
;;;;
;;;; Encryption levels map to QUIC packet number spaces:
;;;;   Initial (0)     - unencrypted / initial keys
;;;;   Early Data (1)  - 0-RTT keys
;;;;   Handshake (2)   - derived after ServerHello
;;;;   Application (3) - derived after Finished
;;;;
;;;; This file shares the epsilon.ssl.tls13 package and has direct
;;;; access to all internal functions (key schedule, transcript, etc.).

(in-package :epsilon.ssl.tls13)

;;; ============================================================================
;;; QUIC Encryption Levels
;;; ============================================================================

(defconstant +quic-level-initial+     0)
(defconstant +quic-level-early-data+  1)
(defconstant +quic-level-handshake+   2)
(defconstant +quic-level-application+ 3)

;;; ============================================================================
;;; QUIC-TLS Key Output
;;; ============================================================================

(defstruct (quic-tls-keys (:constructor %make-quic-tls-keys))
  "Traffic secrets at a given encryption level.
   Use quic-derive-keys to derive QUIC-specific key/iv/hp from these secrets."
  (level 0 :type fixnum)
  (cipher-suite +tls-aes-128-gcm-sha256+ :type fixnum)
  (client-secret nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (server-secret nil :type (or null (simple-array (unsigned-byte 8) (*)))))

;;; ============================================================================
;;; QUIC-TLS State Machine
;;; ============================================================================

(defstruct (quic-tls-state (:constructor %make-quic-tls-state))
  "QUIC-compatible TLS 1.3 handshake state.
   Uses tls-connection internally for key schedule and handshake processing."
  (conn nil :type tls-connection)
  ;; Output queues (push, drain with nreverse)
  (pending-send nil :type list)    ; list of (level . handshake-bytes)
  (pending-keys nil :type list)    ; list of quic-tls-keys
  ;; QUIC transport parameters (raw bytes, encoding is QUIC layer's responsibility)
  (local-transport-params nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (peer-transport-params nil :type (or null (simple-array (unsigned-byte 8) (*))))
  ;; Status
  (handshake-complete nil :type boolean))

(defun make-quic-tls-state (&key (role :client)
                                  hostname
                                  trust-store
                                  alpn-protocols
                                  server-config
                                  local-transport-params)
  "Create a QUIC-TLS state machine.
   ROLE: :client or :server
   LOCAL-TRANSPORT-PARAMS: pre-encoded QUIC transport parameters (raw bytes)"
  (%make-quic-tls-state
   :conn (%make-tls-connection
          :role role
          :hostname hostname
          :trust-store trust-store
          :alpn-protocols alpn-protocols
          :server-config server-config
          :send-buffer (make-buffer))
   :local-transport-params local-transport-params))

;;; ============================================================================
;;; Accessors
;;; ============================================================================

(defun quic-tls-role (state)
  "Return :client or :server."
  (tls-connection-role (quic-tls-state-conn state)))

(defun quic-tls-connected-p (state)
  "True when the handshake is complete."
  (quic-tls-state-handshake-complete state))

(defun quic-tls-alpn-protocol (state)
  "Return the negotiated ALPN protocol string, or NIL."
  (tls-connection-alpn-protocol (quic-tls-state-conn state)))

;;; ============================================================================
;;; Output Queue Draining
;;; ============================================================================

(defun quic-tls-drain-send (state)
  "Drain pending handshake data to send.
   Returns list of (level . handshake-bytes), earliest first."
  (prog1 (nreverse (quic-tls-state-pending-send state))
    (setf (quic-tls-state-pending-send state) nil)))

(defun quic-tls-drain-keys (state)
  "Drain newly derived key sets.
   Returns list of quic-tls-keys structs, earliest first."
  (prog1 (nreverse (quic-tls-state-pending-keys state))
    (setf (quic-tls-state-pending-keys state) nil)))

;;; ============================================================================
;;; QUIC-Specific Key Derivation (RFC 9001 Section 5.1)
;;; ============================================================================

(defun quic-derive-keys (cipher-suite secret)
  "Derive QUIC traffic keys from a traffic secret.
   Returns (values key iv hp-key).
   Uses QUIC-specific labels: 'quic key', 'quic iv', 'quic hp'."
  (multiple-value-bind (hash-fn hash-len) (cipher-suite-hash cipher-suite)
    (declare (ignore hash-len))
    (let ((key-len (cipher-suite-key-len cipher-suite)))
      (values
       (hkdf:hkdf-expand-label hash-fn secret "quic key" +empty-context+ key-len)
       (hkdf:hkdf-expand-label hash-fn secret "quic iv" +empty-context+ 12)
       (hkdf:hkdf-expand-label hash-fn secret "quic hp" +empty-context+ key-len)))))

;;; ============================================================================
;;; Client Handshake Start
;;; ============================================================================

(defun quic-tls-start (state)
  "Begin the QUIC-TLS handshake.
   For clients: generates ClientHello and queues it for sending at Initial level.
   For servers: no-op (server waits for ClientHello).
   Use quic-tls-drain-send to retrieve pending data."
  (ecase (quic-tls-role state)
    (:client (%quic-tls-client-start state))
    (:server nil)))

(defun %quic-tls-client-start (state)
  "Generate ClientHello for QUIC client."
  (let* ((conn (quic-tls-state-conn state))
         (key-shares (tls-generate-key-shares conn))
         ;; Build QUIC transport parameters extension
         (extra-exts
           (when (quic-tls-state-local-transport-params state)
             (list (make-extension +ext-quic-transport-parameters+
                                   (quic-tls-state-local-transport-params state)))))
         ;; Build ClientHello
         (ch (build-client-hello
              :hostname (tls-connection-hostname conn)
              :key-shares key-shares
              :alpn (tls-connection-alpn-protocols conn)
              :extra-extensions extra-exts)))
    ;; Save for transcript re-hashing if cipher suite changes
    (setf (tls-connection-client-hello-msg conn) ch)
    ;; Initialize key schedule (tentative SHA-256, may change after ServerHello)
    (setf (tls-connection-key-schedule conn)
          (make-tls13-key-schedule :cipher-suite +tls-aes-128-gcm-sha256+))
    (transcript-update (tls-connection-key-schedule conn) ch)
    (setf (tls-connection-state conn) :wait-server-hello)
    ;; Queue for sending at Initial level
    (push (cons +quic-level-initial+ ch)
          (quic-tls-state-pending-send state))))

;;; ============================================================================
;;; Process Incoming Handshake Bytes
;;; ============================================================================

(defun quic-tls-process (state level data)
  "Process received handshake bytes at the given encryption level.
   DATA is raw handshake message bytes (from QUIC CRYPTO frames, reassembled).
   Pending output available via quic-tls-drain-send and quic-tls-drain-keys."
  (ecase (quic-tls-role state)
    (:client (%quic-tls-client-process state level data))
    (:server (%quic-tls-server-process state level data))))

;;; ============================================================================
;;; Client-Side Processing
;;; ============================================================================

(defun %quic-tls-client-process (state level data)
  "Process handshake bytes received by QUIC client."
  (declare (ignore level))
  (let* ((conn (quic-tls-state-conn state))
         (pos 0))
    (loop while (< pos (length data))
          do (multiple-value-bind (hs-type payload next-pos)
                 (parse-handshake-header data pos)
               (unless hs-type (return))
               (let ((msg-bytes (subseq data pos next-pos)))
                 (cond
                   ;; ServerHello: special handling for key exchange
                   ((and (eq (tls-connection-state conn) :wait-server-hello)
                         (= hs-type +handshake-server-hello+))
                    (%quic-client-process-server-hello state payload msg-bytes))
                   ;; All other handshake messages
                   (t
                    ;; Save transcript hash before CertificateVerify/Finished
                    (when (or (= hs-type +handshake-certificate-verify+)
                              (= hs-type +handshake-finished+))
                      (setf (tls-connection-transcript-before-cv conn)
                            (transcript-current-hash (tls-connection-key-schedule conn))))
                    ;; Update transcript
                    (transcript-update (tls-connection-key-schedule conn) msg-bytes)
                    ;; Process
                    (%quic-client-process-message state hs-type payload))))
               (setf pos next-pos)))))

(defun %quic-client-process-server-hello (state payload msg-bytes)
  "Process ServerHello, derive handshake secrets, report handshake keys."
  (let* ((conn (quic-tls-state-conn state))
         (sh (parse-server-hello payload))
         (cipher-suite (parsed-server-hello-cipher-suite sh))
         (ks (tls-connection-key-schedule conn)))
    ;; Verify TLS 1.3
    (let ((version (server-hello-get-supported-version sh)))
      (unless (and version (= version +tls-1.3-supported-version+))
        (error "Server does not support TLS 1.3")))
    ;; Handle cipher suite change (e.g. server chose SHA-384)
    (unless (= cipher-suite (tls13-key-schedule-cipher-suite ks))
      (setf ks (make-tls13-key-schedule :cipher-suite cipher-suite))
      (setf (tls-connection-key-schedule conn) ks)
      (when (tls-connection-client-hello-msg conn)
        (transcript-update ks (tls-connection-client-hello-msg conn))))
    (setf (tls-connection-cipher-suite conn) cipher-suite)
    (setf (tls-connection-server-random conn) (parsed-server-hello-random sh))
    ;; Key exchange
    (multiple-value-bind (group-id server-key) (server-hello-get-key-share sh)
      (let ((shared-secret
              (cond
                ((= group-id +group-x25519+)
                 (x25519:x25519 (tls-connection-client-x25519-private conn) server-key))
                ((= group-id +group-secp256r1+)
                 (let ((server-point (ec-p256:p256-point-decode server-key)))
                   (ecdh:ecdh-p256-shared-secret
                    (tls-connection-client-p256-private conn) server-point)))
                (t (error "Unsupported key exchange group: ~X" group-id)))))
        ;; Update transcript with ServerHello
        (transcript-update ks msg-bytes)
        ;; Derive handshake secrets
        (derive-early-secret ks)
        (derive-handshake-secret ks shared-secret)
        ;; Derive and report handshake traffic secrets
        (let* ((hs-secret (tls13-key-schedule-handshake-secret ks))
               (c-hs-secret (ks-derive-secret ks hs-secret "c hs traffic"))
               (s-hs-secret (ks-derive-secret ks hs-secret "s hs traffic")))
          (setf (tls-connection-client-hs-secret conn) c-hs-secret)
          (setf (tls-connection-server-hs-secret conn) s-hs-secret)
          ;; Report handshake keys to QUIC layer
          (push (%make-quic-tls-keys
                 :level +quic-level-handshake+
                 :cipher-suite cipher-suite
                 :client-secret c-hs-secret
                 :server-secret s-hs-secret)
                (quic-tls-state-pending-keys state)))
        (setf (tls-connection-state conn) :wait-encrypted-extensions)))))

(defun %quic-client-process-message (state type payload)
  "Process a single handshake message during QUIC client handshake."
  (let* ((conn (quic-tls-state-conn state))
         (tls-state (tls-connection-state conn)))
    (cond
      ;; EncryptedExtensions
      ((and (eq tls-state :wait-encrypted-extensions)
            (= type +handshake-encrypted-extensions+))
       (let ((exts (parse-encrypted-extensions payload)))
         ;; ALPN
         (let ((alpn-ext (assoc +ext-alpn+ exts)))
           (when alpn-ext
             (let* ((data (cdr alpn-ext))
                    (pos 0))
               (multiple-value-bind (list-len new-pos) (read-u16 data pos)
                 (declare (ignore list-len))
                 (multiple-value-bind (proto-len new-pos2) (read-u8 data new-pos)
                   (setf (tls-connection-alpn-protocol conn)
                         (map 'string #'code-char
                              (subseq data new-pos2 (+ new-pos2 proto-len)))))))))
         ;; QUIC Transport Parameters
         (let ((tp-ext (assoc +ext-quic-transport-parameters+ exts)))
           (when tp-ext
             (setf (quic-tls-state-peer-transport-params state) (cdr tp-ext))))
         (setf (tls-connection-state conn) :wait-certificate-request)))

      ;; CertificateRequest (optional, skip for now)
      ((and (eq tls-state :wait-certificate-request)
            (= type +handshake-certificate-request+))
       (setf (tls-connection-state conn) :wait-certificate))

      ;; Certificate (may come instead of CertificateRequest)
      ((and (member tls-state '(:wait-certificate-request :wait-certificate))
            (= type +handshake-certificate+))
       (let ((cert-data-list (parse-certificate-message payload)))
         (setf (tls-connection-server-certificates conn)
               (mapcar #'x509:parse-x509-certificate cert-data-list)))
       (setf (tls-connection-state conn) :wait-certificate-verify))

      ;; CertificateVerify
      ((and (eq tls-state :wait-certificate-verify)
            (= type +handshake-certificate-verify+))
       (multiple-value-bind (scheme signature) (parse-certificate-verify payload)
         (let ((transcript-hash (tls-connection-transcript-before-cv conn)))
           (when transcript-hash
             (verify-certificate-verify-signature conn scheme signature transcript-hash))))
       (setf (tls-connection-state conn) :wait-finished))

      ;; Finished
      ((and (eq tls-state :wait-finished)
            (= type +handshake-finished+))
       (%quic-client-process-finished state payload))

      (t
       (error "Unexpected handshake message type ~D in QUIC-TLS state ~A"
              type tls-state)))))

(defun %quic-client-process-finished (state verify-data)
  "Process server Finished, build client Finished, derive application keys."
  (let* ((conn (quic-tls-state-conn state))
         (ks (tls-connection-key-schedule conn))
         (transcript-hash (tls-connection-transcript-before-cv conn)))
    ;; Verify server Finished
    (when transcript-hash
      (let* ((finished-key (derive-finished-key ks (tls-connection-server-hs-secret conn)))
             (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                  finished-key transcript-hash)))
        (unless (equalp verify-data expected)
          (error "QUIC-TLS: server Finished verification failed"))))
    ;; Build client Finished (raw handshake message, no record wrapping)
    (let ((client-finished (build-finished ks (tls-connection-client-hs-secret conn))))
      ;; Derive master secret BEFORE adding client Finished to transcript
      ;; (RFC 8446: app keys use transcript through server Finished)
      (derive-master-secret ks)
      ;; Report application traffic secrets
      (let ((master-secret (tls13-key-schedule-master-secret ks)))
        (let ((c-app-secret (ks-derive-secret ks master-secret "c ap traffic"))
              (s-app-secret (ks-derive-secret ks master-secret "s ap traffic")))
          (push (%make-quic-tls-keys
                 :level +quic-level-application+
                 :cipher-suite (tls-connection-cipher-suite conn)
                 :client-secret c-app-secret
                 :server-secret s-app-secret)
                (quic-tls-state-pending-keys state))))
      ;; NOW add client Finished to transcript (needed for resumption PSK)
      (transcript-update ks client-finished)
      ;; Queue client Finished for sending at Handshake level
      (push (cons +quic-level-handshake+ client-finished)
            (quic-tls-state-pending-send state))
      ;; Handshake complete
      (setf (tls-connection-state conn) :connected)
      (setf (quic-tls-state-handshake-complete state) t))))

;;; ============================================================================
;;; Server-Side Processing
;;; ============================================================================

(defun %quic-tls-server-process (state level data)
  "Process handshake bytes received by QUIC server."
  (let ((conn (quic-tls-state-conn state)))
    (cond
      ;; ClientHello at Initial level
      ((and (eq (tls-connection-state conn) :start)
            (= level +quic-level-initial+))
       (%quic-server-process-client-hello state data))
      ;; Client Finished at Handshake level
      ((and (eq (tls-connection-state conn) :wait-finished)
            (= level +quic-level-handshake+))
       (%quic-server-process-client-finished state data))
      (t
       (error "Unexpected QUIC-TLS message at level ~D in state ~A"
              level (tls-connection-state conn))))))

(defun %quic-server-process-client-hello (state data)
  "Process ClientHello and generate entire server handshake flight."
  (let* ((conn (quic-tls-state-conn state))
         (config (tls-connection-server-config conn)))
    ;; Parse handshake header
    (multiple-value-bind (hs-type payload next-pos)
        (parse-handshake-header data 0)
      (declare (ignore next-pos))
      (unless (= hs-type +handshake-client-hello+)
        (error "Expected ClientHello, got type ~D" hs-type))
      (let* ((ch (parse-client-hello payload))
             ;; Negotiate cipher suite
             (suite (negotiate-cipher-suite
                     (parsed-client-hello-cipher-suites ch)
                     (tls-server-config-cipher-suites config)))
             ;; Find key share (prefer X25519)
             (x25519-share (find +group-x25519+
                                 (parsed-client-hello-key-shares ch)
                                 :key #'car))
             (ks (make-tls13-key-schedule :cipher-suite suite)))
        (unless suite (error "No common cipher suite"))
        (unless x25519-share (error "No supported key share group"))
        (setf (tls-connection-key-schedule conn) ks)
        (setf (tls-connection-cipher-suite conn) suite)
        ;; Extract QUIC transport parameters from client
        (when (parsed-client-hello-quic-transport-params ch)
          (setf (quic-tls-state-peer-transport-params state)
                (parsed-client-hello-quic-transport-params ch)))
        ;; Add ClientHello to transcript
        (let ((ch-msg (make-handshake-message +handshake-client-hello+ payload)))
          (transcript-update ks ch-msg))
        ;; Generate server key pair and compute shared secret
        (let* ((server-sk (drbg:random-bytes 32))
               (server-pk (x25519:x25519-base server-sk))
               (client-pk (cdr x25519-share))
               (shared-secret (x25519:x25519 server-sk client-pk)))
          ;; Derive handshake secrets
          (derive-early-secret ks)
          (derive-handshake-secret ks shared-secret)
          ;; Build ServerHello
          (let* ((server-random (drbg:random-bytes 32))
                 (session-id (parsed-client-hello-session-id ch))
                 (sh-payload (build-server-hello-payload
                              server-random session-id suite server-pk))
                 (sh-msg (make-handshake-message +handshake-server-hello+ sh-payload)))
            (transcript-update ks sh-msg)
            ;; Queue ServerHello at Initial level
            (push (cons +quic-level-initial+ sh-msg)
                  (quic-tls-state-pending-send state))
            ;; Derive and report handshake traffic secrets
            (let* ((hs-secret (tls13-key-schedule-handshake-secret ks))
                   (c-hs-secret (ks-derive-secret ks hs-secret "c hs traffic"))
                   (s-hs-secret (ks-derive-secret ks hs-secret "s hs traffic")))
              (setf (tls-connection-server-hs-secret conn) s-hs-secret)
              (setf (tls-connection-client-hs-secret conn) c-hs-secret)
              (push (%make-quic-tls-keys
                     :level +quic-level-handshake+
                     :cipher-suite suite
                     :client-secret c-hs-secret
                     :server-secret s-hs-secret)
                    (quic-tls-state-pending-keys state)))
            ;; Negotiate ALPN
            (let ((client-alpn (parsed-client-hello-alpn-protocols ch))
                  (server-alpn (tls-server-config-alpn-protocols config)))
              (when (and client-alpn server-alpn)
                (setf (tls-connection-alpn-protocol conn)
                      (find-if (lambda (p) (member p client-alpn :test #'string=))
                               server-alpn))))
            ;; Build encrypted handshake flight (raw messages, no encryption)
            (let ((flight-buf (make-buffer)))
              ;; EncryptedExtensions (with QUIC transport params)
              (let* ((ee-payload (%quic-build-encrypted-extensions state))
                     (ee-msg (make-handshake-message
                              +handshake-encrypted-extensions+ ee-payload)))
                (transcript-update ks ee-msg)
                (buf-append-bytes flight-buf ee-msg))
              ;; Certificate
              (let ((cert-msg (build-certificate-message
                               (tls-server-config-certificate-chain config))))
                (transcript-update ks cert-msg)
                (buf-append-bytes flight-buf cert-msg))
              ;; CertificateVerify
              (let ((cv-msg (build-certificate-verify conn config)))
                (transcript-update ks cv-msg)
                (buf-append-bytes flight-buf cv-msg))
              ;; Finished
              (let ((finished-msg (build-finished ks (tls-connection-server-hs-secret conn))))
                (transcript-update ks finished-msg)
                (buf-append-bytes flight-buf finished-msg))
              ;; Queue handshake flight at Handshake level
              (push (cons +quic-level-handshake+ (buf-freeze flight-buf))
                    (quic-tls-state-pending-send state)))
            ;; Derive master secret and application traffic secrets
            (derive-master-secret ks)
            (let ((master-secret (tls13-key-schedule-master-secret ks)))
              (let ((c-app-secret (ks-derive-secret ks master-secret "c ap traffic"))
                    (s-app-secret (ks-derive-secret ks master-secret "s ap traffic")))
                (push (%make-quic-tls-keys
                       :level +quic-level-application+
                       :cipher-suite suite
                       :client-secret c-app-secret
                       :server-secret s-app-secret)
                      (quic-tls-state-pending-keys state))))
            (setf (tls-connection-state conn) :wait-finished)))))))

(defun %quic-build-encrypted-extensions (state)
  "Build EncryptedExtensions payload including QUIC transport parameters."
  (let ((conn (quic-tls-state-conn state))
        (buf (make-buffer)))
    (let ((ext-buf (make-buffer)))
      ;; ALPN
      (when (tls-connection-alpn-protocol conn)
        (let ((proto (tls-connection-alpn-protocol conn))
              (alpn-buf (make-buffer)))
          (let ((proto-bytes (map '(vector (unsigned-byte 8)) #'char-code proto)))
            (let ((list-buf (make-buffer)))
              (buf-append-u8-prefixed list-buf proto-bytes)
              (buf-append-u16-prefixed alpn-buf (buf-freeze list-buf))))
          (buf-append-u16 ext-buf +ext-alpn+)
          (buf-append-u16-prefixed ext-buf (buf-freeze alpn-buf))))
      ;; QUIC Transport Parameters
      (when (quic-tls-state-local-transport-params state)
        (buf-append-u16 ext-buf +ext-quic-transport-parameters+)
        (buf-append-u16-prefixed ext-buf
                                 (quic-tls-state-local-transport-params state)))
      (buf-append-u16-prefixed buf (buf-freeze ext-buf)))
    (buf-freeze buf)))

(defun %quic-server-process-client-finished (state data)
  "Process client Finished and complete the server handshake."
  (let ((conn (quic-tls-state-conn state)))
    (multiple-value-bind (hs-type payload next-pos)
        (parse-handshake-header data 0)
      (declare (ignore next-pos))
      (unless (= hs-type +handshake-finished+)
        (error "Expected Finished, got type ~D" hs-type))
      (let* ((ks (tls-connection-key-schedule conn))
             (transcript-hash (transcript-current-hash ks)))
        ;; Verify client Finished
        (let* ((finished-key (derive-finished-key ks (tls-connection-client-hs-secret conn)))
               (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                    finished-key transcript-hash)))
          (unless (equalp payload expected)
            (error "QUIC-TLS: client Finished verification failed")))
        ;; Update transcript with client Finished
        (transcript-update ks data)
        ;; Handshake complete
        (setf (tls-connection-state conn) :connected)
        (setf (quic-tls-state-handshake-complete state) t)))))
