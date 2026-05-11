;;;; TLS 1.3 Protocol (RFC 8446)
;;;;
;;;; Implements the TLS 1.3 record layer, handshake protocol, key schedule,
;;;; and state machine. TLS 1.3 only -- no support for legacy versions.

(defpackage epsilon.crypto.tls13
  (:use :cl)
  (:import
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.sha512 sha512)
   (epsilon.crypto.md5 md5)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.hkdf hkdf)
   (epsilon.crypto.aes-gcm aes-gcm)
   (epsilon.crypto.chacha20-poly1305 chacha)
   (epsilon.crypto.curve25519 x25519)
   (epsilon.crypto.ml-kem-hybrid hybrid)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.ec-p384 ec-p384)
   (epsilon.crypto.ec-p521 ec-p521)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ecdsa-p384 ecdsa-p384)
   (epsilon.crypto.ecdsa-p521 ecdsa-p521)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.tls-session-ticket-store stek)
   (epsilon.crypto.ct ct))
  (:export
   ;; Content types
   #:+content-change-cipher-spec+
   #:+content-alert+
   #:+content-handshake+
   #:+content-application-data+
   ;; Handshake types
   #:+handshake-client-hello+
   #:+handshake-server-hello+
   #:+handshake-new-session-ticket+
   #:+handshake-encrypted-extensions+
   #:+handshake-certificate+
   #:+handshake-certificate-request+
   #:+handshake-certificate-verify+
   #:+handshake-finished+
   #:+handshake-key-update+
   #:+handshake-end-of-early-data+
   ;; Cipher suites
   #:+tls-aes-128-gcm-sha256+
   #:+tls-aes-256-gcm-sha384+
   #:+tls-chacha20-poly1305-sha256+
   ;; Alert constants
   #:+alert-close-notify+
   #:+alert-unexpected-message+
   #:+alert-bad-record-mac+
   #:+alert-handshake-failure+
   #:+alert-illegal-parameter+
   #:+alert-level-warning+
   #:+alert-level-fatal+
   #:+alert-bad-certificate+
   #:+alert-certificate-expired+
   #:+alert-unknown-ca+
   #:+alert-decode-error+
   #:+alert-decrypt-error+
   #:+alert-protocol-version+
   #:+alert-internal-error+
   #:+alert-missing-extension+
   #:+alert-certificate-required+
   ;; Alert protocol
   #:tls-alert
   #:tls-alert-level
   #:tls-alert-description
   #:make-alert-record
   #:parse-alert
   #:tls-close-notify
   #:alert-description-name
   ;; Structured alert condition (raised when peer sends a fatal alert
   ;; record while we're in the handshake; lets HTTP-client / mirror code
   ;; surface the actual reason instead of "Unexpected record type 21").
   #:tls-alert-error
   #:tls-alert-error-level
   #:tls-alert-error-description
   #:tls-alert-error-state
   ;; Version-mismatch -- raised when ServerHello selects a non-1.3
   ;; version. tls-connect-with-fallback (umbrella package) catches this
   ;; to retry via tls12.
   #:tls-version-mismatch-error
   #:tls-version-mismatch-negotiated-version
   #:tls-version-mismatch-legacy-version
   ;; Key schedule
   #:tls13-key-schedule
   #:make-tls13-key-schedule
   #:derive-early-secret
   #:derive-handshake-secret
   #:derive-master-secret
   #:derive-handshake-traffic-keys
   #:derive-application-traffic-keys
   ;; Record layer
   #:tls-record
   #:make-tls-record
   #:tls-record-content-type
   #:tls-record-legacy-version
   #:tls-record-data
   #:parse-tls-record
   #:encrypt-record
   #:decrypt-record
   ;; Handshake messages
   #:build-client-hello
   #:parse-server-hello
   #:parse-encrypted-extensions
   #:parse-certificate-message
   #:parse-certificate-verify
   #:parse-finished
   #:build-finished
   ;; PSK / session resumption
   #:tls-session-ticket
   #:make-tls-session-ticket
   #:tls-session-ticket-ticket
   #:tls-session-ticket-lifetime
   #:tls-session-ticket-age-add
   #:tls-session-ticket-nonce
   #:tls-session-ticket-max-early-data
   #:tls-session-ticket-cipher-suite
   #:tls-session-ticket-resumption-secret
   #:tls-session-ticket-creation-time
   #:parse-new-session-ticket
   #:build-psk-client-hello
   #:tls-connection-new-session-ticket-callback
   ;; 0-RTT early data
   #:tls-send-early-data
   ;; HelloRetryRequest
   #:+hello-retry-request-magic+
   #:hello-retry-request-p
   #:tls-process-hello-retry-request
   #:hrr-selected-group
   #:hrr-cookie
   ;; Negotiated key-exchange group (post-handshake)
   #:tls-connection-negotiated-group
   #:named-group-name
   ;; Dimension-extraction helpers for metric labels and structured logs
   #:cipher-suite-name
   #:cert-key-type-label
   #:server-hello-psk-selected-p
   ;; Named-group codepoints (so callers can compare without the
   ;; double-colon access that integration tests otherwise need).
   #:+group-x25519+
   #:+group-secp256r1+
   #:+group-secp384r1+
   #:+group-secp521r1+
   #:+group-x25519-mlkem768+
   ;; KeyUpdate
   #:build-key-update
   #:process-key-update
   ;; Server-side handshake
   #:tls-server-config
   #:make-tls-server-config
   #:tls-server-start-handshake
   #:tls-server-process-finished
   #:parse-client-hello
   #:tls-decode-error
   #:tls-decode-error-reason
   #:parsed-client-hello-cipher-suites
   #:parsed-client-hello-key-shares
   #:parsed-client-hello-hostname
   #:parsed-client-hello-alpn-protocols
   #:parsed-client-hello-supported-versions
   #:parsed-client-hello-signature-algorithms
   #:parsed-client-hello-random
   ;; State machine
   #:tls-connection
   #:make-tls-connection
   #:tls-handshake-step
   #:tls-connection-state
   #:tls-connection-alpn-protocol
   #:tls-connection-resumed-p
   #:tls-connection-ocsp-staple
   #:tls-connection-server-certificates
   #:tls-send-application-data
   #:tls-receive-application-data
   ;; Stream adapter
   #:tls-stream
   #:tls-stream-p
   #:make-tls-stream
   #:tls-stream-connection
   #:tls-stream-closed-p
   #:tls-connect
   #:tls-accept
   #:tls-read
   #:tls-write
   #:tls-shutdown
   #:tls-close
   #:tls-stream-peer-certificates
   #:tls-stream-cipher-suite
   #:tls-stream-alpn-protocol
   #:tls-read-line
   #:tls-write-line
   #:tls-write-string
   ;; Transport protocol
   #:tls-transport
   #:tls-transport-read
   #:tls-transport-write
   #:tls-transport-close
   ;; fd-transport (wraps CL binary streams)
   #:fd-transport
   #:make-fd-transport
   #:fd-transport-p
   ;; Utility functions
   #:string-to-bytes
   #:bytes-to-string
   ;; QUIC-TLS (RFC 9001)
   #:+ext-quic-transport-parameters+
   #:+quic-level-initial+
   #:+quic-level-early-data+
   #:+quic-level-handshake+
   #:+quic-level-application+
   ;; QUIC-TLS key output
   #:quic-tls-keys
   #:quic-tls-keys-p
   #:quic-tls-keys-level
   #:quic-tls-keys-cipher-suite
   #:quic-tls-keys-client-secret
   #:quic-tls-keys-server-secret
   ;; QUIC-TLS state
   #:quic-tls-state
   #:make-quic-tls-state
   #:quic-tls-state-p
   #:quic-tls-role
   #:quic-tls-connected-p
   #:quic-tls-alpn-protocol
   #:quic-tls-state-peer-transport-params
   ;; QUIC-TLS operations
   #:quic-tls-start
   #:quic-tls-process
   #:quic-tls-drain-send
   #:quic-tls-drain-keys
   ;; QUIC key derivation
   #:quic-derive-keys
   ;; Cipher suite info (for QUIC key derivation)
   #:cipher-suite-hash
   #:cipher-suite-key-len
   ;; ClientHello QUIC transport params
   #:parsed-client-hello-quic-transport-params
   #:client-hello-ja3-fingerprint
   #:client-hello-ja4-fingerprint))

(in-package :epsilon.crypto.tls13)

;;; ---------------------------------------------------------------------------
;;; TLS 1.3 Constants (RFC 8446)
;;; ---------------------------------------------------------------------------

;; Protocol version
(defconstant +tls-1.3+ #x0303)  ; legacy version for record layer
(defconstant +tls-1.3-supported-version+ #x0304)  ; real version in extension
(defconstant +tls-1.2-supported-version+ #x0303)  ; for advertising 1.2 fallback

;; Content types (RFC 8446 Section 5.1)
(defconstant +content-change-cipher-spec+ 20)
(defconstant +content-alert+ 21)
(defconstant +content-handshake+ 22)
(defconstant +content-application-data+ 23)

;; Handshake types (RFC 8446 Section 4)
(defconstant +handshake-client-hello+ 1)
(defconstant +handshake-server-hello+ 2)
(defconstant +handshake-new-session-ticket+ 4)
(defconstant +handshake-encrypted-extensions+ 8)
(defconstant +handshake-certificate+ 11)
(defconstant +handshake-certificate-request+ 13)
(defconstant +handshake-certificate-verify+ 15)
;; CompressedCertificate (RFC 8879 §4). Sent by a server in lieu of
;; a Certificate message when the client advertised
;; compress_certificate and the server chose to compress.
(defconstant +handshake-compressed-certificate+ 25)
(defconstant +handshake-finished+ 20)
(defconstant +handshake-key-update+ 24)
(defconstant +handshake-end-of-early-data+ 5)

;; Cipher suites (RFC 8446 Section B.4)
(defconstant +tls-aes-128-gcm-sha256+ #x1301)
(defconstant +tls-aes-256-gcm-sha384+ #x1302)
(defconstant +tls-chacha20-poly1305-sha256+ #x1303)

;; Sequence number limit (RFC 8446 Section 5.5)
(defconstant +max-sequence-number+ (1- (expt 2 64)))

;; Extension types (RFC 8446 Section 4.2)
(defconstant +ext-server-name+ 0)
(defconstant +ext-status-request+ 5)         ; RFC 6066, OCSP stapling
(defconstant +ext-supported-groups+ 10)
;; ec_point_formats (RFC 8422 §5.1.2). TLS 1.2-era; many servers still
;; gate on it being present (browsers always send `uncompressed').
;; Diagnosed as part of IMPL-380 follow-up Bucket 1.
(defconstant +ext-ec-point-formats+ 11)
(defconstant +ext-signature-algorithms+ 13)
(defconstant +ext-alpn+ 16)
(defconstant +ext-encrypt-then-mac+ 22)      ; RFC 7366 (TLS 1.2-era)
(defconstant +ext-extended-master-secret+ 23) ; RFC 7627 (TLS 1.2-era)
(defconstant +ext-session-ticket+ 35)         ; RFC 5077 (TLS 1.2-era)
(defconstant +ext-pre-shared-key+ 41)
(defconstant +ext-supported-versions+ 43)
;; cookie (RFC 8446 §4.2.2): opaque bytes the server emits with a
;; HelloRetryRequest and the client MUST echo unchanged in the retry
;; ClientHello. Used for stateless HRR / DDoS protection.
(defconstant +ext-cookie+ 44)
;; psk_key_exchange_modes (RFC 8446 §4.2.9). MUST be present if the
;; client offers pre_shared_key. Browsers and openssl s_client send
;; it unconditionally with `psk_dhe_ke' to signal full PSK-DHE
;; capability; some hardened TLS 1.3 servers (badssl edge) reject
;; clients that omit it even when no PSK is offered.
(defconstant +ext-psk-key-exchange-modes+ 45)
(defconstant +ext-key-share+ 51)
;; compress_certificate (RFC 8879). Codepoint 27. Lets the server
;; deliver the Certificate message body in zlib- or brotli-compressed
;; form to save handshake bytes when the cert chain is large.
(defconstant +ext-compress-certificate+ 27)
;; Compression algorithm IDs from the IANA registry.
(defconstant +cert-compression-zlib+ 1)
(defconstant +cert-compression-brotli+ 2)
;; PSK key exchange modes (RFC 8446 §4.2.9).
(defconstant +psk-ke+ 0)         ; PSK-only
(defconstant +psk-dhe-ke+ 1)     ; PSK with (EC)DHE

;; QUIC transport parameters (RFC 9001 Section 8.2)
(defconstant +ext-quic-transport-parameters+ #x39)

;; Named groups
(defconstant +group-x25519+ #x001D)
(defconstant +group-secp256r1+ #x0017)
;; FIPS 186-4 / NIST P-384 and P-521 named groups (RFC 8446 §B.3.1.4).
;; Required to interoperate with FIPS-leaning servers (.gov / .edu /
;; institutional nginx / Akamai conservative profiles); see
;; manual/implement/380_tls-client-cdn-compliance.md Stage 2.
(defconstant +group-secp384r1+ #x0018)
(defconstant +group-secp521r1+ #x0019)
(defconstant +group-x25519-mlkem768+ #x11EC
  "IANA codepoint for the X25519+ML-KEM-768 hybrid key exchange.
   draft-kwiatkowski-tls-ecdhe-mlkem / draft-ietf-tls-ecdhe-mlkem.")

;; Signature schemes
(defconstant +sig-ecdsa-secp256r1-sha256+ #x0403)
;; FIPS-curve ECDSA signature schemes (RFC 8446 §4.2.3, IANA codepoints).
;; Required so that we can verify CertificateVerify messages signed with
;; the curve the server actually picked from supported_groups; before
;; this, a P-384 server would complete the key exchange and then we'd
;; abort on the cert verify. See IMPL-380 Stage 3.
(defconstant +sig-ecdsa-secp384r1-sha384+ #x0503)
(defconstant +sig-ecdsa-secp521r1-sha512+ #x0603)
(defconstant +sig-ed25519+ #x0807)
(defconstant +sig-rsa-pss-rsae-sha256+ #x0804)
(defconstant +sig-rsa-pss-rsae-sha384+ #x0805)
(defconstant +sig-rsa-pss-rsae-sha512+ #x0806)

;; Alert descriptions
(defconstant +alert-close-notify+ 0)
(defconstant +alert-unexpected-message+ 10)
(defconstant +alert-bad-record-mac+ 20)
(defconstant +alert-handshake-failure+ 40)
(defconstant +alert-illegal-parameter+ 47)
(defconstant +alert-bad-certificate+ 42)
(defconstant +alert-certificate-expired+ 45)
(defconstant +alert-unknown-ca+ 48)
(defconstant +alert-decode-error+ 50)
(defconstant +alert-decrypt-error+ 51)
(defconstant +alert-protocol-version+ 70)
(defconstant +alert-internal-error+ 80)
(defconstant +alert-missing-extension+ 109)
(defconstant +alert-certificate-required+ 116)

;; Alert levels
(defconstant +alert-level-warning+ 1)
(defconstant +alert-level-fatal+ 2)

;; HelloRetryRequest magic random value (RFC 8446 Section 4.1.3)
(defparameter +hello-retry-request-magic+
  (make-array 32 :element-type '(unsigned-byte 8)
              :initial-contents '(#xCF #x21 #xAD #x74 #xE5 #x9A #x61 #x11
                                  #xBE #x1D #x8C #x02 #x1E #x65 #xB8 #x91
                                  #xC2 #xA2 #x11 #x16 #x7A #xBB #x8C #x5E
                                  #x07 #x9E #x09 #xE2 #xC8 #xA8 #x33 #x9C)))

;;; ---------------------------------------------------------------------------
;;; Byte buffer utilities
;;; ---------------------------------------------------------------------------

(defun make-buffer (&optional (initial-size 256))
  "Create a growable byte buffer."
  (make-array initial-size :element-type '(unsigned-byte 8)
              :adjustable t :fill-pointer 0))

(defun buf-append-byte (buf byte)
  (vector-push-extend byte buf))

(defun buf-append-u16 (buf val)
  (vector-push-extend (logand (ash val -8) #xFF) buf)
  (vector-push-extend (logand val #xFF) buf))

(defun buf-append-u24 (buf val)
  (vector-push-extend (logand (ash val -16) #xFF) buf)
  (vector-push-extend (logand (ash val -8) #xFF) buf)
  (vector-push-extend (logand val #xFF) buf))

(defun buf-append-bytes (buf bytes)
  (loop for b across bytes do (vector-push-extend b buf)))

(defun buf-append-u16-prefixed (buf bytes)
  "Append a 2-byte length prefix followed by the bytes."
  (buf-append-u16 buf (length bytes))
  (buf-append-bytes buf bytes))

(defun buf-append-u8-prefixed (buf bytes)
  "Append a 1-byte length prefix followed by the bytes."
  (buf-append-byte buf (length bytes))
  (buf-append-bytes buf bytes))

(defun buf-freeze (buf)
  "Return a simple-array copy of the buffer."
  (let ((result (make-array (length buf) :element-type '(unsigned-byte 8))))
    (replace result buf)
    result))

(defun tls13-need (data pos n what)
  "Signal TLS-DECODE-ERROR unless N bytes are available at POS in DATA."
  (unless (<= (+ pos n) (length data))
    (error 'tls-decode-error
           :reason (format nil "~A: need ~D byte~:P at offset ~D, have ~D"
                           what n pos (- (length data) pos)))))

(defun read-u8 (data pos &optional (what "u8"))
  (tls13-need data pos 1 what)
  (values (aref data pos) (1+ pos)))

(defun read-u16 (data pos &optional (what "u16"))
  (tls13-need data pos 2 what)
  (values (logior (ash (aref data pos) 8) (aref data (1+ pos)))
          (+ pos 2)))

(defun read-u24 (data pos &optional (what "u24"))
  (tls13-need data pos 3 what)
  (values (logior (ash (aref data pos) 16)
                  (ash (aref data (1+ pos)) 8)
                  (aref data (+ pos 2)))
          (+ pos 3)))

(defun read-bytes (data pos len &optional (what "bytes"))
  (tls13-need data pos len what)
  (values (subseq data pos (+ pos len)) (+ pos len)))

;;; ---------------------------------------------------------------------------
;;; TLS 1.3 Key Schedule (RFC 8446 Section 7.1)
;;; ---------------------------------------------------------------------------

(defstruct (tls13-key-schedule (:constructor %make-tls13-key-schedule))
  (cipher-suite +tls-aes-128-gcm-sha256+ :type fixnum)
  (hash-fn nil)           ; :sha256 or :sha384
  (hash-len 32 :type fixnum)
  ;; Secrets
  (early-secret nil)
  (handshake-secret nil)
  (master-secret nil)
  ;; Transcript hash state
  (transcript-hash nil)
  ;; Cached finalized digest (invalidated on each transcript-update)
  (transcript-hash-cache nil))

(defun make-tls13-key-schedule (&key (cipher-suite +tls-aes-128-gcm-sha256+))
  "Create a TLS 1.3 key schedule for the given cipher suite."
  (multiple-value-bind (hash-fn hash-len)
      (cipher-suite-hash cipher-suite)
    (%make-tls13-key-schedule
     :cipher-suite cipher-suite
     :hash-fn hash-fn
     :hash-len hash-len
     :transcript-hash (make-transcript-hash hash-fn))))

(defun cipher-suite-hash (suite)
  "Return (values hash-function-keyword hash-length) for a cipher suite."
  (case suite
    (#.+tls-aes-128-gcm-sha256+ (values :sha256 32))
    (#.+tls-aes-256-gcm-sha384+ (values :sha384 48))
    (#.+tls-chacha20-poly1305-sha256+ (values :sha256 32))
    (t (error "Unknown cipher suite: ~X" suite))))

(defun cipher-suite-key-len (suite)
  "Return the key length for a cipher suite."
  (case suite
    (#.+tls-aes-128-gcm-sha256+ 16)
    (#.+tls-aes-256-gcm-sha384+ 32)
    (#.+tls-chacha20-poly1305-sha256+ 32)
    (t (error "Unknown cipher suite: ~X" suite))))

(defun cipher-suite-iv-len (suite)
  "Return the IV length for a cipher suite."
  (declare (ignore suite))
  12)  ; All TLS 1.3 cipher suites use 12-byte IV

;;; ---------------------------------------------------------------------------
;;; Transcript hash
;;; ---------------------------------------------------------------------------

(defun make-transcript-hash (hash-fn)
  "Create a transcript hash state."
  (ecase hash-fn
    (:sha256 (sha256:make-sha256-state))
    (:sha384 (sha512:make-sha384-state))))

(defun transcript-update (ks data)
  "Update the transcript hash with handshake data.
   Invalidates the cached finalized digest."
  (setf (tls13-key-schedule-transcript-hash-cache ks) nil)
  (ecase (tls13-key-schedule-hash-fn ks)
    (:sha256 (sha256:sha256-update (tls13-key-schedule-transcript-hash ks) data))
    (:sha384 (sha512:sha384-update (tls13-key-schedule-transcript-hash ks) data))))

(defun transcript-current-hash (ks)
  "Get the current transcript hash (cached until next update).
   Avoids redundant copy+finalize when called multiple times between updates."
  (or (tls13-key-schedule-transcript-hash-cache ks)
      (setf (tls13-key-schedule-transcript-hash-cache ks)
            (ecase (tls13-key-schedule-hash-fn ks)
              (:sha256
               (let ((copy (sha256:sha256-copy (tls13-key-schedule-transcript-hash ks))))
                 (sha256:sha256-finalize copy)))
              (:sha384
               (let ((copy (sha512:sha384-copy (tls13-key-schedule-transcript-hash ks))))
                 (sha512:sha384-finalize copy)))))))

(defun hash-empty (hash-fn)
  "Return Hash(\"\") for the given hash function."
  (ecase hash-fn
    (:sha256 (sha256:sha256 (make-array 0 :element-type '(unsigned-byte 8))))
    (:sha384 (sha512:sha384 (make-array 0 :element-type '(unsigned-byte 8))))))

(defun ks-hkdf-extract (ks salt ikm)
  "HKDF-Extract using the key schedule's hash function."
  (hkdf:hkdf-extract (tls13-key-schedule-hash-fn ks) salt ikm))

(defun ks-hkdf-expand-label (ks secret label context length)
  "TLS 1.3 HKDF-Expand-Label (RFC 8446 Section 7.1)."
  (hkdf:hkdf-expand-label (tls13-key-schedule-hash-fn ks)
                           secret label context length))

(defun ks-derive-secret (ks secret label)
  "Derive-Secret(Secret, Label, Messages) = HKDF-Expand-Label(Secret, Label, Transcript-Hash, Hash.length)"
  (let ((transcript-hash (transcript-current-hash ks)))
    (ks-hkdf-expand-label ks secret label transcript-hash
                          (tls13-key-schedule-hash-len ks))))

;;; ---------------------------------------------------------------------------
;;; Key schedule derivation (RFC 8446 Section 7.1)
;;; ---------------------------------------------------------------------------

(defun derive-early-secret (ks &optional psk)
  "Derive the Early Secret from an optional PSK."
  (let* ((hash-len (tls13-key-schedule-hash-len ks))
         (zero-salt (make-array hash-len :element-type '(unsigned-byte 8) :initial-element 0))
         (ikm (or psk (make-array hash-len :element-type '(unsigned-byte 8) :initial-element 0)))
         (early-secret (ks-hkdf-extract ks zero-salt ikm)))
    (setf (tls13-key-schedule-early-secret ks) early-secret)
    early-secret))

(defun derive-handshake-secret (ks shared-secret)
  "Derive the Handshake Secret from the shared key exchange secret."
  (unless (tls13-key-schedule-early-secret ks)
    (derive-early-secret ks))
  (let* ((early-secret (tls13-key-schedule-early-secret ks))
         ;; derived = Derive-Secret(early_secret, "derived", "")
         (empty-hash (hash-empty (tls13-key-schedule-hash-fn ks)))
         (derived (ks-hkdf-expand-label ks early-secret "derived" empty-hash
                                        (tls13-key-schedule-hash-len ks)))
         (handshake-secret (ks-hkdf-extract ks derived shared-secret)))
    (setf (tls13-key-schedule-handshake-secret ks) handshake-secret)
    handshake-secret))

(defun derive-master-secret (ks)
  "Derive the Master Secret."
  (let* ((hash-len (tls13-key-schedule-hash-len ks))
         (handshake-secret (tls13-key-schedule-handshake-secret ks))
         (empty-hash (hash-empty (tls13-key-schedule-hash-fn ks)))
         (derived (ks-hkdf-expand-label ks handshake-secret "derived" empty-hash hash-len))
         (zero-ikm (make-array hash-len :element-type '(unsigned-byte 8) :initial-element 0))
         (master-secret (ks-hkdf-extract ks derived zero-ikm)))
    (setf (tls13-key-schedule-master-secret ks) master-secret)
    master-secret))

(defparameter +empty-context+
  (make-array 0 :element-type '(unsigned-byte 8))
  "Empty context for HKDF-Expand-Label calls.")

(defun derive-traffic-keys (ks secret)
  "Derive write key and IV from a traffic secret.
   Returns (values key iv)."
  (let ((key-len (cipher-suite-key-len (tls13-key-schedule-cipher-suite ks)))
        (iv-len (cipher-suite-iv-len (tls13-key-schedule-cipher-suite ks))))
    (values
     (ks-hkdf-expand-label ks secret "key" +empty-context+ key-len)
     (ks-hkdf-expand-label ks secret "iv" +empty-context+ iv-len))))

(defun derive-handshake-traffic-keys (ks)
  "Derive client and server handshake traffic keys.
   Returns (values client-key client-iv server-key server-iv)."
  (let ((hs-secret (tls13-key-schedule-handshake-secret ks)))
    (let ((client-hs-secret (ks-derive-secret ks hs-secret "c hs traffic"))
          (server-hs-secret (ks-derive-secret ks hs-secret "s hs traffic")))
      (multiple-value-bind (c-key c-iv) (derive-traffic-keys ks client-hs-secret)
        (multiple-value-bind (s-key s-iv) (derive-traffic-keys ks server-hs-secret)
          (values c-key c-iv s-key s-iv))))))

(defun derive-application-traffic-keys (ks)
  "Derive client and server application traffic keys.
   Returns (values client-key client-iv server-key server-iv)."
  (let ((master-secret (or (tls13-key-schedule-master-secret ks)
                           (derive-master-secret ks))))
    (let ((client-app-secret (ks-derive-secret ks master-secret "c ap traffic"))
          (server-app-secret (ks-derive-secret ks master-secret "s ap traffic")))
      (multiple-value-bind (c-key c-iv) (derive-traffic-keys ks client-app-secret)
        (multiple-value-bind (s-key s-iv) (derive-traffic-keys ks server-app-secret)
          (values c-key c-iv s-key s-iv))))))

(defun derive-finished-key (ks base-key)
  "Derive the finished key from a base key."
  (ks-hkdf-expand-label ks base-key "finished" +empty-context+
                        (tls13-key-schedule-hash-len ks)))

;;; ---------------------------------------------------------------------------
;;; TLS Record Layer (RFC 8446 Section 5)
;;; ---------------------------------------------------------------------------

(defstruct tls-record
  (content-type 0 :type (unsigned-byte 8))
  (legacy-version +tls-1.3+ :type fixnum)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*))))

(defun serialize-tls-record (record)
  "Serialize a TLS record to bytes (header + data)."
  (let* ((data (tls-record-data record))
         (len (length data))
         (result (make-array (+ 5 len) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) (tls-record-content-type record))
    (setf (aref result 1) (ash (tls-record-legacy-version record) -8))
    (setf (aref result 2) (logand (tls-record-legacy-version record) #xFF))
    (setf (aref result 3) (ash len -8))
    (setf (aref result 4) (logand len #xFF))
    (replace result data :start1 5)
    result))

(defun parse-tls-record (data &optional (pos 0))
  "Parse a TLS record from bytes.
   Returns (values tls-record next-pos) or NIL if insufficient data."
  (when (< (- (length data) pos) 5)
    (return-from parse-tls-record nil))
  (let ((ct (aref data pos))
        (version (logior (ash (aref data (+ pos 1)) 8) (aref data (+ pos 2))))
        (len (logior (ash (aref data (+ pos 3)) 8) (aref data (+ pos 4)))))
    ;; Sanity check: 2^14 + 256 (max padding) + 1 (content type) for encrypted records
    (when (> len 16641)
      (error "TLS record too large: ~D bytes" len))
    (when (< (- (length data) pos) (+ 5 len))
      (return-from parse-tls-record nil))
    (values
     (make-tls-record :content-type ct
                      :legacy-version version
                      :data (subseq data (+ pos 5) (+ pos 5 len)))
     (+ pos 5 len))))

;;; ---------------------------------------------------------------------------
;;; Record encryption (RFC 8446 Section 5.2)
;;; ---------------------------------------------------------------------------

(defun compute-nonce (iv seq-num)
  "Compute per-record nonce: IV XOR padded sequence number."
  (let ((nonce (make-array 12 :element-type '(unsigned-byte 8)))
        (iv-len (length iv)))
    (replace nonce iv)
    ;; XOR sequence number (8 bytes, right-aligned) into nonce
    (loop for i from 7 downto 0
          for j from (1- iv-len) downto (- iv-len 8)
          do (setf (aref nonce j) (logxor (aref nonce j)
                                          (logand (ash seq-num (* -8 (- 7 i))) #xFF))))
    nonce))

(defun encrypt-record (data content-type key iv seq-num cipher-suite)
  "Encrypt a TLS 1.3 record. Returns the ciphertext (including tag and inner content type).
   The outer record type will be application_data."
  (let* (;; Inner plaintext: data + content-type byte
         (inner (make-array (1+ (length data)) :element-type '(unsigned-byte 8)))
         (nonce (compute-nonce iv seq-num))
         ;; Additional data: record header
         (outer-len (+ (length data) 1 16))  ; inner + content-type + tag
         (aad (make-array 5 :element-type '(unsigned-byte 8))))
    ;; Build inner plaintext
    (replace inner data)
    (setf (aref inner (length data)) content-type)
    ;; Build AAD (outer record header)
    (setf (aref aad 0) +content-application-data+)
    (setf (aref aad 1) #x03)
    (setf (aref aad 2) #x03)
    (setf (aref aad 3) (ash outer-len -8))
    (setf (aref aad 4) (logand outer-len #xFF))
    ;; Encrypt
    (case cipher-suite
      (#.+tls-chacha20-poly1305-sha256+
       (multiple-value-bind (ct tag) (chacha:chacha20-poly1305-encrypt inner key nonce :aad aad)
         (let ((result (make-array (+ (length ct) (length tag))
                                   :element-type '(unsigned-byte 8))))
           (replace result ct)
           (replace result tag :start1 (length ct))
           result)))
      (t  ; AES-GCM
       (multiple-value-bind (ct tag) (aes-gcm:aes-gcm-encrypt inner key nonce :aad aad)
         (let ((result (make-array (+ (length ct) (length tag))
                                   :element-type '(unsigned-byte 8))))
           (replace result ct)
           (replace result tag :start1 (length ct))
           result))))))

(defun decrypt-record (ciphertext key iv seq-num cipher-suite)
  "Decrypt a TLS 1.3 record. Returns (values plaintext content-type) or signals error."
  (let* ((ct-len (- (length ciphertext) 16))  ; subtract tag
         (ct (subseq ciphertext 0 ct-len))
         (tag (subseq ciphertext ct-len))
         (nonce (compute-nonce iv seq-num))
         ;; AAD
         (aad (make-array 5 :element-type '(unsigned-byte 8))))
    (setf (aref aad 0) +content-application-data+)
    (setf (aref aad 1) #x03)
    (setf (aref aad 2) #x03)
    (setf (aref aad 3) (ash (length ciphertext) -8))
    (setf (aref aad 4) (logand (length ciphertext) #xFF))
    ;; Decrypt
    (let ((plaintext
            (case cipher-suite
              (#.+tls-chacha20-poly1305-sha256+
               (chacha:chacha20-poly1305-decrypt ct key nonce tag :aad aad))
              (t
               (aes-gcm:aes-gcm-decrypt ct key nonce tag :aad aad)))))
      (unless plaintext
        (error "Record decryption failed: bad MAC"))
      ;; Strip padding zeros and extract content type (last non-zero byte)
      (let ((end (1- (length plaintext))))
        (loop while (and (>= end 0) (zerop (aref plaintext end)))
              do (decf end))
        (when (< end 0)
          (error "Record decryption failed: empty inner plaintext"))
        (let ((inner-ct (aref plaintext end)))
          (values (subseq plaintext 0 end) inner-ct))))))

;;; ---------------------------------------------------------------------------
;;; Handshake message construction
;;; ---------------------------------------------------------------------------

(defun make-handshake-message (type payload)
  "Wrap a payload in a handshake message header."
  (let* ((len (length payload))
         (result (make-array (+ 4 len) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) type)
    (setf (aref result 1) (logand (ash len -16) #xFF))
    (setf (aref result 2) (logand (ash len -8) #xFF))
    (setf (aref result 3) (logand len #xFF))
    (replace result payload :start1 4)
    result))

(defun parse-handshake-header (data &optional (pos 0))
  "Parse a handshake message header.
   Returns (values type payload next-pos) or NIL."
  (when (< (- (length data) pos) 4)
    (return-from parse-handshake-header nil))
  (let ((type (aref data pos))
        (len (logior (ash (aref data (+ pos 1)) 16)
                     (ash (aref data (+ pos 2)) 8)
                     (aref data (+ pos 3)))))
    (when (< (- (length data) pos) (+ 4 len))
      (return-from parse-handshake-header nil))
    (values type (subseq data (+ pos 4) (+ pos 4 len)) (+ pos 4 len))))

;;; ---------------------------------------------------------------------------
;;; Extensions
;;; ---------------------------------------------------------------------------

(defun make-extension (type data)
  "Build a TLS extension."
  (let ((buf (make-buffer)))
    (buf-append-u16 buf type)
    (buf-append-u16-prefixed buf data)
    (buf-freeze buf)))

;;; ---------------------------------------------------------------------------
;;; GREASE (RFC 8701)
;;;
;;; The 16 codepoints below are reserved by RFC 8701 §3.1 and MUST be
;;; ignored by every TLS implementation. The client peppers a few of
;;; them across its ClientHello (cipher_suites, supported_versions,
;;; supported_groups + a matching empty key_share, signature_algorithms,
;;; ALPN, and one trailing extension) so that:
;;;
;;;   1. Servers that incorrectly hard-code "the only valid value here
;;;      is X" surface that bug at handshake time, not later.
;;;   2. Bot-management WAFs that fingerprint clients on the
;;;      *absence* of GREASE see us as browser-shaped.
;;;
;;; A fresh GREASE codepoint is picked per ClientHello (no state).
;;; ---------------------------------------------------------------------------

(declaim (type (simple-array (unsigned-byte 16) (16)) +grease-codepoints+))
(defparameter +grease-codepoints+
  (let ((vals #(#x0A0A #x1A1A #x2A2A #x3A3A
                 #x4A4A #x5A5A #x6A6A #x7A7A
                 #x8A8A #x9A9A #xAAAA #xBABA
                 #xCACA #xDADA #xEAEA #xFAFA)))
    (make-array 16 :element-type '(unsigned-byte 16) :initial-contents vals))
  "RFC 8701 §3.1 GREASE codepoints. Equivalent for cipher_suites,
   supported_versions, supported_groups, signature_algorithms, and
   extension types: an unknown-but-reserved 16-bit value.")

(defun %random-grease-value ()
  "Pick a GREASE codepoint uniformly at random from the reserved set.
   Stateless: each call may return a different value."
  (aref +grease-codepoints+ (random 16)))

(defun %compress-package ()
  "Return the loaded `epsilon.compression' package, or NIL if it is not
   available. Used to decide whether the client may advertise the
   `compress_certificate' extension and to dispatch decompression on
   inbound CompressedCertificate messages without forcing a hard
   dependency from `epsilon.crypto' onto the compression library."
  (find-package :epsilon.compression))

(defun %compress-shared-object-loadable-p (pkg avail-name probe-fn)
  "True when the shared object backing AVAIL-NAME / PROBE-FN in PKG
   actually opens. We prefer an explicit availability predicate
   (`brotli-available-p` is the canonical example -- it wraps
   `ensure-brotli-loaded' in handler-case and returns NIL on dlopen
   failure). When no such predicate exists in this version of
   epsilon.compression we fall back to invoking PROBE-FN on a 1-byte
   input inside `handler-case' and inspecting the condition: a
   missing-shared-object error means unavailable, any other error
   (bad input, decompress failure, etc.) means the library loaded
   fine and the algorithm is available."
  (let ((avail (find-symbol avail-name pkg)))
    (cond
      ((and avail (fboundp avail))
       (funcall avail))
      ((and probe-fn (fboundp probe-fn))
       (handler-case
           (progn (funcall probe-fn (make-array 1 :element-type
                                                  '(unsigned-byte 8)
                                                  :initial-element 0))
                  t)
         (error (c)
           (let ((msg (princ-to-string c)))
             (not (or (search "shared object" msg)
                      (search "cannot open shared" msg)
                      (search "library not loaded" msg)
                      (search "image not found" msg)))))))
      (t nil))))

(defun %cert-compression-algorithms-available ()
  "Return a list of `cert_compression_algorithm' IDs (RFC 8879
   IANA values) that the runtime can actually decompress, in
   preference order: brotli first, zlib second.

   Returns NIL when `epsilon.compression' is not loaded. For each
   candidate algorithm we additionally verify that the shared
   object actually opens before advertising the algorithm -- this
   keeps slimmed CI environments (where libbrotli is not in the
   Nix closure) from advertising an algorithm we cannot actually
   handle on the inbound side."
  (let ((pkg (%compress-package)))
    (when pkg
      (let ((algs nil))
        (when (%compress-shared-object-loadable-p
               pkg "BROTLI-AVAILABLE-P"
               (find-symbol "BROTLI-DECOMPRESS" pkg))
          (push +cert-compression-brotli+ algs))
        (when (%compress-shared-object-loadable-p
               pkg "ZLIB-AVAILABLE-P"
               (find-symbol "DECOMPRESS" pkg))
          (push +cert-compression-zlib+ algs))
        ;; Reverse so brotli ends up first when both are available.
        (nreverse algs)))))

(defun make-compress-certificate-ext (algorithms)
  "Build the `compress_certificate' extension (RFC 8879 §3) for a
   ClientHello. ALGORITHMS is a list of u16 algorithm IDs."
  (let ((buf (make-buffer)))
    (buf-append-byte buf (* 2 (length algorithms)))      ; u8 list length
    (dolist (alg algorithms) (buf-append-u16 buf alg))
    (make-extension +ext-compress-certificate+ (buf-freeze buf))))

(defun decompress-certificate-payload (payload)
  "Decode a CompressedCertificate handshake body (RFC 8879 §4):

       struct {
         CertificateCompressionAlgorithm algorithm; // u16
         uint24 uncompressed_length;
         opaque compressed_certificate_message<1..2^24-1>;
       } CompressedCertificate;

   Returns the decompressed bytes -- the body of an ordinary
   Certificate handshake message (RFC 8446 §4.4.2). Signals an error
   when the algorithm is not advertised, when `epsilon.compression' is
   not available at runtime, or when the inflated length does not
   match the announced uncompressed_length."
  (when (< (length payload) 7)
    (error "compressed_certificate: payload too short (~D bytes)" (length payload)))
  (let* ((alg (logior (ash (aref payload 0) 8) (aref payload 1)))
         (declared-len (logior (ash (aref payload 2) 16)
                               (ash (aref payload 3) 8)
                               (aref payload 4)))
         (compressed-len (logior (ash (aref payload 5) 16)
                                  (ash (aref payload 6) 8)
                                  (aref payload 7))))
    (unless (= (length payload) (+ 8 compressed-len))
      (error "compressed_certificate: declared length ~D bytes does not ~
              match payload tail ~D bytes" compressed-len (- (length payload) 8)))
    (let* ((compressed (subseq payload 8))
           (pkg (%compress-package))
           (decompress-fn
             (cond
               ((null pkg)
                (error "compressed_certificate: epsilon.compression is not loaded"))
               ((= alg +cert-compression-zlib+)
                (find-symbol "DECOMPRESS" pkg))
               ((= alg +cert-compression-brotli+)
                (find-symbol "BROTLI-DECOMPRESS" pkg))
               (t (error "compressed_certificate: unsupported algorithm ~D" alg)))))
      (unless (and decompress-fn (fboundp decompress-fn))
        (error "compressed_certificate: decompressor for algorithm ~D ~
                not available" alg))
      (let ((inflated (funcall decompress-fn compressed)))
        (unless (= (length inflated) declared-len)
          (error "compressed_certificate: declared uncompressed length ~D ~
                  != actual ~D" declared-len (length inflated)))
        inflated))))

(defun make-supported-versions-ext-client (&key grease)
  "Build supported_versions extension for ClientHello.

   Advertises TLS 1.3 first (preferred) AND TLS 1.2 (fallback). Without
   1.2 here, TLS-1.2-only servers see 'no shared version' and reply
   with `handshake_failure(40)' instead of a TLS 1.2 ServerHello. With
   1.2 advertised, the server picks 1.2 in its own supported_versions
   reply (or omits the extension), our `tls-process-server-hello'
   raises `tls-version-mismatch-error', and `tls-connect-with-fallback'
   retries via the TLS 1.2 client. This was the residual cause of the
   badssl.com handshake_failure cluster captured in IMPL-380's interop
   matrix.

   GREASE, when non-nil, is prepended to the version list."
  (let ((buf (make-buffer))
        (count (if grease 3 2)))
    (buf-append-byte buf (* 2 count))
    (when grease (buf-append-u16 buf grease))
    (buf-append-u16 buf +tls-1.3-supported-version+)
    (buf-append-u16 buf +tls-1.2-supported-version+)
    (make-extension +ext-supported-versions+ (buf-freeze buf))))

(defun make-key-share-ext-client (groups &key grease)
  "Build key_share extension for ClientHello.
   GROUPS is a list of (:KEYWORD key-bytes); supported keywords are
   :x25519, :secp256r1, :secp384r1, :secp521r1, :x25519-mlkem768.

   GREASE, when non-nil, is the codepoint to use for an empty
   key_share entry prepended to the list (RFC 8701 §3.4: the body
   for a GREASE group is a single zero byte)."
  (let ((entries-buf (make-buffer)))
    (when grease
      (buf-append-u16 entries-buf grease)
      ;; A 1-byte body distinguishes a GREASE entry from a malformed
      ;; zero-length one; its content is irrelevant since the server
      ;; MUST ignore the entry.
      (buf-append-u16-prefixed entries-buf
                                (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
    (dolist (group groups)
      (let ((group-id (ecase (first group)
                        (:x25519 +group-x25519+)
                        (:secp256r1 +group-secp256r1+)
                        (:secp384r1 +group-secp384r1+)
                        (:secp521r1 +group-secp521r1+)
                        (:x25519-mlkem768 +group-x25519-mlkem768+)))
            (key-data (second group)))
        (buf-append-u16 entries-buf group-id)
        (buf-append-u16-prefixed entries-buf key-data)))
    (let ((buf (make-buffer)))
      (buf-append-u16-prefixed buf (buf-freeze entries-buf))
      (make-extension +ext-key-share+ (buf-freeze buf)))))

(defun make-supported-groups-ext (&key grease)
  "Build supported_groups extension.

   Advertised in client preference order:
     X25519MLKEM768 (post-quantum hybrid) -> X25519 (modern default) ->
     P-256 -> P-384 -> P-521 (FIPS curves, in increasing strength).
   The FIPS curves are required to interoperate with conservative
   profiles (Akamai government tenants, institutional nginx, e.g.
   scholarsjunction.msstate.edu). See IMPL-380.

   GREASE, when non-nil, is prepended to the group list."
  (let* ((real-groups (list +group-x25519-mlkem768+
                            +group-x25519+
                            +group-secp256r1+
                            +group-secp384r1+
                            +group-secp521r1+))
         (groups (if grease (cons grease real-groups) real-groups))
         (buf (make-buffer)))
    (buf-append-u16 buf (* 2 (length groups)))
    (dolist (g groups) (buf-append-u16 buf g))
    (make-extension +ext-supported-groups+ (buf-freeze buf))))

(defun make-signature-algorithms-ext (&key grease)
  "Build signature_algorithms extension.

   Order is preference: tightly-paired ECDSA first (server picks based
   on its cert key's curve), then EdDSA, then RSA-PSS variants.
   ECDSA-P-384 and ECDSA-P-521 are required for FIPS deployments where
   the leaf cert is signed with the corresponding curve; without them
   the handshake completes the key exchange and then dies on the
   CertificateVerify.

   GREASE, when non-nil, is prepended to the scheme list."
  (let* ((real-schemes (list +sig-ecdsa-secp256r1-sha256+
                             +sig-ecdsa-secp384r1-sha384+
                             +sig-ecdsa-secp521r1-sha512+
                             +sig-ed25519+
                             +sig-rsa-pss-rsae-sha256+
                             +sig-rsa-pss-rsae-sha384+
                             +sig-rsa-pss-rsae-sha512+))
         (schemes (if grease (cons grease real-schemes) real-schemes))
         (buf (make-buffer)))
    (buf-append-u16 buf (* 2 (length schemes)))
    (dolist (s schemes) (buf-append-u16 buf s))
    (make-extension +ext-signature-algorithms+ (buf-freeze buf))))

(defun make-server-name-ext (hostname)
  "Build server_name extension (SNI)."
  (let ((name-bytes (map '(vector (unsigned-byte 8)) #'char-code hostname))
        (buf (make-buffer)))
    ;; Server name list
    (let ((entry-len (+ 3 (length name-bytes))))
      (buf-append-u16 buf entry-len)  ; server_name_list length
      (buf-append-byte buf 0)         ; host_name type
      (buf-append-u16-prefixed buf name-bytes))
    (make-extension +ext-server-name+ (buf-freeze buf))))

(defun make-alpn-ext (protocols &key grease)
  "Build ALPN extension. GREASE, when non-nil, is the codepoint to
   encode as the leading 2-byte protocol identifier (RFC 8701 §3.5)."
  (let ((protos-buf (make-buffer)))
    (when grease
      ;; A GREASE ALPN entry is a 2-byte protocol whose bytes encode
      ;; the GREASE codepoint big-endian.
      (let ((g (make-array 2 :element-type '(unsigned-byte 8))))
        (setf (aref g 0) (ldb (byte 8 8) grease)
              (aref g 1) (ldb (byte 8 0) grease))
        (buf-append-u8-prefixed protos-buf g)))
    (dolist (proto protocols)
      (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code proto)))
        (buf-append-u8-prefixed protos-buf bytes)))
    (let ((buf (make-buffer)))
      (buf-append-u16-prefixed buf (buf-freeze protos-buf))
      (make-extension +ext-alpn+ (buf-freeze buf)))))

;;; ---------------------------------------------------------------------------
;;; ClientHello (RFC 8446 Section 4.1.2)
;;; ---------------------------------------------------------------------------

(defun build-client-hello (&key hostname
                                (cipher-suites (list +tls-chacha20-poly1305-sha256+
                                                     +tls-aes-128-gcm-sha256+
                                                     +tls-aes-256-gcm-sha384+))
                                key-shares
                                (alpn nil)
                                (extra-extensions nil)
                                (random nil)
                                (session-id nil)
                                (cookie nil)
                                (grease t))
  "Build a ClientHello handshake message.
   KEY-SHARES: list of (:x25519 pub-bytes) or (:secp256r1 pub-bytes).
   EXTRA-EXTENSIONS: list of pre-encoded extension byte vectors (e.g. QUIC transport params).
   RANDOM: 32-byte client random (caller-supplied for HRR retransmits;
   freshly generated when NIL).
   SESSION-ID: 32-byte legacy_session_id (caller-supplied for HRR; fresh otherwise).
   COOKIE: opaque cookie bytes from a HelloRetryRequest cookie extension;
   when non-NIL, emitted as the RFC 8446 §4.2.2 cookie extension.
   GREASE: when non-nil (default), peppers RFC 8701 GREASE codepoints
   into cipher_suites, supported_versions, supported_groups (with a
   matching empty key_share entry), signature_algorithms, ALPN, and a
   trailing extension. Set GREASE=NIL when the test or caller needs a
   deterministic ClientHello.
   Returns the full handshake message bytes."
  ;; Pick distinct GREASE codepoints per slot so a fingerprinter
  ;; cannot trivially correlate them. RFC 8701 does not require
  ;; distinctness, but it matches what browsers do.
  (let ((g-cs   (when grease (%random-grease-value)))
        (g-ver  (when grease (%random-grease-value)))
        (g-grp  (when grease (%random-grease-value)))
        (g-sig  (when grease (%random-grease-value)))
        (g-alpn (when grease (%random-grease-value)))
        (g-ext  (when grease (%random-grease-value))))
    (let ((buf (make-buffer)))
      ;; legacy_version
      (buf-append-u16 buf +tls-1.3+)
      ;; random (32 bytes)
      (let ((random (or random (drbg:random-bytes 32))))
        (buf-append-bytes buf random))
      ;; legacy_session_id (for middlebox compatibility)
      (let ((session-id (or session-id (drbg:random-bytes 32))))
        (buf-append-u8-prefixed buf session-id))
      ;; cipher_suites (with leading GREASE entry if requested)
      (let ((all-suites (if g-cs (cons g-cs cipher-suites) cipher-suites)))
        (buf-append-u16 buf (* 2 (length all-suites)))
        (dolist (cs all-suites) (buf-append-u16 buf cs)))
      ;; legacy_compression_methods
      (buf-append-byte buf 1)   ; length
      (buf-append-byte buf 0)   ; null compression
      ;; extensions
      (let ((exts-buf (make-buffer)))
        ;; supported_versions (mandatory)
        (buf-append-bytes exts-buf (make-supported-versions-ext-client :grease g-ver))
        ;; supported_groups
        (buf-append-bytes exts-buf (make-supported-groups-ext :grease g-grp))
        ;; key_share -- the GREASE empty entry pairs with the GREASE
        ;; codepoint from supported_groups.
        (when key-shares
          (buf-append-bytes exts-buf
                            (make-key-share-ext-client key-shares :grease g-grp)))
        ;; signature_algorithms
        (buf-append-bytes exts-buf (make-signature-algorithms-ext :grease g-sig))
        ;; server_name
        (when hostname
          (buf-append-bytes exts-buf (make-server-name-ext hostname)))
        ;; ALPN
        (when alpn
          (buf-append-bytes exts-buf (make-alpn-ext alpn :grease g-alpn)))
      ;; status_request (RFC 6066): we are willing to receive an OCSP
      ;; staple. The body is an empty OCSPStatusRequest with no
      ;; responder_id_list and no extensions.
      (let ((sr-buf (make-buffer)))
        (buf-append-byte sr-buf 1)   ; status_type = ocsp
        (buf-append-u16 sr-buf 0)    ; responder_id_list <0..2^16-1> empty
        (buf-append-u16 sr-buf 0)    ; request_extensions <0..2^16-1> empty
        (buf-append-bytes exts-buf
                          (make-extension +ext-status-request+
                                          (buf-freeze sr-buf))))
      ;; cookie (RFC 8446 §4.2.2): echo the HelloRetryRequest cookie
      ;; verbatim so a stateless server can reconstruct its prior state.
      (when cookie
        (let ((cb (make-buffer)))
          (buf-append-u16-prefixed cb cookie)
          (buf-append-bytes exts-buf
                            (make-extension +ext-cookie+ (buf-freeze cb)))))
      ;; psk_key_exchange_modes (RFC 8446 §4.2.9). Browsers and openssl
      ;; s_client send `psk_dhe_ke' unconditionally; some TLS 1.3
      ;; servers (badssl edge, observed 2026-04-30) reject ClientHellos
      ;; that omit it even when no PSK is offered. We follow the
      ;; browser-typical pattern and always send it.
      (let ((mb (make-buffer)))
        (buf-append-byte mb 1)               ; modes-list length
        (buf-append-byte mb +psk-dhe-ke+)
        (buf-append-bytes exts-buf
                          (make-extension +ext-psk-key-exchange-modes+
                                          (buf-freeze mb))))
      ;; ec_point_formats (RFC 8422 §5.1.2): TLS 1.2-era extension, but
      ;; many servers still gate on it. Single entry: uncompressed.
      (let ((ef (make-buffer)))
        (buf-append-byte ef 1)               ; format-list length
        (buf-append-byte ef 0)               ; uncompressed
        (buf-append-bytes exts-buf
                          (make-extension +ext-ec-point-formats+
                                          (buf-freeze ef))))
      ;; extended_master_secret (RFC 7627): empty body. Required by some
      ;; conservative servers / TLS-inspecting middleboxes for downgrade
      ;; protection; harmless on TLS 1.3.
      (buf-append-bytes exts-buf
                        (make-extension +ext-extended-master-secret+
                                        (make-array 0 :element-type
                                                    '(unsigned-byte 8))))
      ;; compress_certificate (RFC 8879): advertise the algorithms we
      ;; can actually decompress. Only emitted when `epsilon.compression'
      ;; is loaded, so the SSL module does not gain a hard runtime
      ;; dependency on the compression bindings.
      (let ((cc-algs (%cert-compression-algorithms-available)))
        (when cc-algs
          (buf-append-bytes exts-buf (make-compress-certificate-ext cc-algs))))
      ;; Extra extensions (e.g. QUIC transport parameters)
      (dolist (ext extra-extensions)
        (buf-append-bytes exts-buf ext))
      ;; Trailing GREASE extension (RFC 8701 §3.6): one extra extension
      ;; with a GREASE codepoint type and an empty body. Servers MUST
      ;; ignore unknown extension types per RFC 8446 §4.2.
      (when g-ext
        (buf-append-bytes exts-buf
                          (make-extension g-ext
                                          (make-array 0 :element-type
                                                      '(unsigned-byte 8)))))
      ;; Write extensions with length prefix
      (buf-append-u16-prefixed buf (buf-freeze exts-buf)))
    ;; Wrap in handshake header
    (make-handshake-message +handshake-client-hello+ (buf-freeze buf)))))

;;; ---------------------------------------------------------------------------
;;; ServerHello parsing (RFC 8446 Section 4.1.3)
;;; ---------------------------------------------------------------------------

(defstruct parsed-server-hello
  (random nil)
  (session-id nil)
  (cipher-suite 0 :type fixnum)
  (extensions nil :type list))  ; alist of (type . data)

(defun parse-server-hello (payload)
  "Parse a ServerHello payload (without handshake header).
   Returns a parsed-server-hello struct."
  (let ((pos 0))
    ;; legacy_version
    (multiple-value-bind (version new-pos) (read-u16 payload pos)
      (declare (ignore version))
      (setf pos new-pos))
    ;; random
    (multiple-value-bind (random new-pos) (read-bytes payload pos 32)
      ;; session_id
      (multiple-value-bind (sid-len new-pos2) (read-u8 payload new-pos)
        (multiple-value-bind (session-id new-pos3) (read-bytes payload new-pos2 sid-len)
          ;; cipher_suite
          (multiple-value-bind (cipher-suite new-pos4) (read-u16 payload new-pos3)
            ;; compression_method
            (multiple-value-bind (comp new-pos5) (read-u8 payload new-pos4)
              (declare (ignore comp))
              ;; extensions
              (let ((extensions nil))
                (when (< new-pos5 (length payload))
                  (multiple-value-bind (ext-len new-pos6) (read-u16 payload new-pos5)
                    (let ((ext-end (+ new-pos6 ext-len))
                          (ext-pos new-pos6))
                      (loop while (< ext-pos ext-end)
                            do (multiple-value-bind (ext-type new-p) (read-u16 payload ext-pos)
                                 (multiple-value-bind (ext-data-len new-p2) (read-u16 payload new-p)
                                   (multiple-value-bind (ext-data new-p3) (read-bytes payload new-p2 ext-data-len)
                                     (push (cons ext-type ext-data) extensions)
                                     (setf ext-pos new-p3))))))))
                (make-parsed-server-hello
                 :random random
                 :session-id session-id
                 :cipher-suite cipher-suite
                 :extensions (nreverse extensions))))))))))

(defun server-hello-get-key-share (sh)
  "Extract key_share from ServerHello. Returns (values group-id key-data)."
  (let ((ext (assoc +ext-key-share+ (parsed-server-hello-extensions sh))))
    (when ext
      (let ((data (cdr ext)))
        (values (logior (ash (aref data 0) 8) (aref data 1))
                (subseq data 4 (+ 4 (logior (ash (aref data 2) 8) (aref data 3)))))))))

(defun server-hello-get-supported-version (sh)
  "Extract supported_versions from ServerHello."
  (let ((ext (assoc +ext-supported-versions+ (parsed-server-hello-extensions sh))))
    (when ext
      (let ((data (cdr ext)))
        (logior (ash (aref data 0) 8) (aref data 1))))))

(defun server-hello-psk-selected-p (sh)
  "T when ServerHello echoes a pre_shared_key extension (RFC 8446
   4.2.11), meaning the server accepted one of the client's offered
   PSK identities and resumption proceeds. Absence of the extension
   when the client *did* offer PSK is the server's signal of
   rejection -- the handshake falls through to a fresh certificate
   exchange and the client should treat the offered ticket as stale."
  (and (assoc +ext-pre-shared-key+ (parsed-server-hello-extensions sh)) t))

;;; ---------------------------------------------------------------------------
;;; EncryptedExtensions, Certificate, CertificateVerify, Finished parsing
;;; ---------------------------------------------------------------------------

(defun parse-encrypted-extensions (payload)
  "Parse EncryptedExtensions message. Returns alist of extensions."
  (let ((pos 0)
        (extensions nil))
    (multiple-value-bind (ext-len new-pos) (read-u16 payload pos)
      (let ((ext-end (+ new-pos ext-len))
            (ext-pos new-pos))
        (loop while (< ext-pos ext-end)
              do (multiple-value-bind (ext-type new-p) (read-u16 payload ext-pos)
                   (multiple-value-bind (ext-data-len new-p2) (read-u16 payload new-p)
                     (multiple-value-bind (ext-data new-p3) (read-bytes payload new-p2 ext-data-len)
                       (push (cons ext-type ext-data) extensions)
                       (setf ext-pos new-p3)))))))
    (nreverse extensions)))

(defun %extract-staple-from-cert-extensions (payload start length)
  "Scan a CertificateEntry extensions block for a status_request entry
   and return the embedded OCSP response bytes, or NIL when absent."
  (let ((pos start)
        (end (+ start length))
        (staple nil))
    (loop while (< pos end)
          do (multiple-value-bind (etype np) (read-u16 payload pos)
               (multiple-value-bind (elen np2) (read-u16 payload np)
                 (when (= etype +ext-status-request+)
                   ;; CertificateStatus: u8 type | u24 len | bytes
                   (when (and (>= elen 4) (= (aref payload np2) 1))
                     (let ((rlen (logior (ash (aref payload (+ np2 1)) 16)
                                         (ash (aref payload (+ np2 2)) 8)
                                         (aref payload (+ np2 3)))))
                       (setf staple (subseq payload (+ np2 4)
                                            (+ np2 4 rlen))))))
                 (setf pos (+ np2 elen)))))
    staple))

(defun parse-certificate-message (payload)
  "Parse Certificate handshake message.
   Returns (values cert-list leaf-ocsp-staple) where cert-list is a
   list of DER-encoded certificate byte arrays in chain order, and
   leaf-ocsp-staple is the OCSP response bytes attached to the leaf's
   status_request CertificateEntry extension, or NIL."
  (let ((pos 0)
        (certs nil)
        (first-p t)
        (leaf-staple nil))
    ;; certificate_request_context
    (multiple-value-bind (ctx-len new-pos) (read-u8 payload pos)
      (setf pos (+ new-pos ctx-len)))
    ;; certificate_list
    (multiple-value-bind (list-len new-pos) (read-u24 payload pos)
      (let ((list-end (+ new-pos list-len)))
        (setf pos new-pos)
        (loop while (< pos list-end)
              do ;; cert_data
                 (multiple-value-bind (cert-len new-p) (read-u24 payload pos)
                   (multiple-value-bind (cert-data new-p2) (read-bytes payload new-p cert-len)
                     ;; extensions (per-cert)
                     (multiple-value-bind (ext-len new-p3) (read-u16 payload new-p2)
                       (when first-p
                         (setf leaf-staple
                               (%extract-staple-from-cert-extensions
                                payload new-p3 ext-len))
                         (setf first-p nil))
                       (setf pos (+ new-p3 ext-len))
                       (push cert-data certs)))))))
    (values (nreverse certs) leaf-staple)))

(defun parse-certificate-verify (payload)
  "Parse CertificateVerify message.
   Returns (values signature-scheme signature-bytes)."
  (let ((pos 0))
    (multiple-value-bind (scheme new-pos) (read-u16 payload pos)
      (multiple-value-bind (sig-len new-pos2) (read-u16 payload new-pos)
        (multiple-value-bind (sig-data new-pos3) (read-bytes payload new-pos2 sig-len)
          (declare (ignore new-pos3))
          (values scheme sig-data))))))

;;; ---------------------------------------------------------------------------
;;; TLS 1.3 Connection State Machine
;;; ---------------------------------------------------------------------------

(deftype handshake-state ()
  '(member :start
           :wait-server-hello
           :wait-encrypted-extensions
           :wait-certificate-request
           :wait-certificate
           :wait-certificate-verify
           :wait-finished
           :connected
           :closed
           :error))

(defstruct (tls-connection (:constructor %make-tls-connection))
  (state :start :type symbol)
  (role :client :type symbol)         ; :client or :server
  (key-schedule nil)
  ;; Key exchange
  (client-x25519-private nil)
  (client-x25519-public nil)
  (client-p256-private nil)
  (client-p256-public nil)
  ;; NIST P-384 / P-521 ephemeral key shares. Held alongside the
  ;; classical curves so the client can offer a key_share for any
  ;; FIPS-profile server that prefers a higher-strength curve.
  (client-p384-private nil)
  (client-p384-public nil)
  (client-p521-private nil)
  (client-p521-public nil)
  ;; X25519+ML-KEM-768 hybrid (1216-byte public, 2432-byte private)
  (client-hybrid-private nil)
  (client-hybrid-public nil)
  ;; Negotiated parameters
  (cipher-suite 0 :type fixnum)
  ;; The named-group codepoint the server selected from supported_groups
  ;; (one of +group-x25519+, +group-secp256r1+, +group-secp384r1+,
  ;; +group-secp521r1+, +group-x25519-mlkem768+). NIL until ServerHello
  ;; is processed. Surfaced to callers (HTTP client, mirror code, the
  ;; IMPL-380 interop runner) so they can log which curve actually
  ;; negotiated.
  (negotiated-group nil)
  (server-random nil)
  (hostname nil :type (or null string))
  ;; Traffic keys
  (client-handshake-key nil)
  (client-handshake-iv nil)
  (server-handshake-key nil)
  (server-handshake-iv nil)
  (client-app-key nil)
  (client-app-iv nil)
  (server-app-key nil)
  (server-app-iv nil)
  ;; Sequence numbers
  (client-seq 0 :type integer)
  (server-seq 0 :type integer)
  ;; Handshake traffic secrets (needed for Finished)
  (client-hs-secret nil)
  (server-hs-secret nil)
  ;; Certificate chain. On the client side this is the server's chain;
  ;; on the server side, when mTLS is configured, this holds the
  ;; client's Certificate flight after :wait-client-certificate has
  ;; been processed. tls-stream-peer-certificates returns this slot
  ;; in either direction -- "the peer's certs" by role.
  (server-certificates nil :type list)
  ;; mTLS server-side bookkeeping: the transcript hash captured right
  ;; before the client's CertificateVerify is folded in (the input to
  ;; that signature, per RFC 8446 §4.4.3) and a flag set when the
  ;; server emitted a CertificateRequest in its handshake flight.
  (server-transcript-before-client-cv nil)
  (cert-requested-p nil :type boolean)
  ;; ALPN
  (alpn-protocol nil)             ; negotiated protocol (set during handshake)
  (alpn-protocols nil :type list) ; protocols to advertise (set before handshake)
  ;; Trust store
  (trust-store nil)
  ;; Outgoing data buffer
  (send-buffer nil)
  ;; PSK / session resumption
  (session-ticket nil)            ; tls-session-ticket for resumption
  (early-data nil)                ; early data to send (0-RTT)
  (early-data-accepted nil)       ; whether server accepted early data
  (resumption-secret nil)         ; for deriving new PSK
  ;; Saved ClientHello for transcript re-hashing on cipher suite change
  (client-hello-msg nil)
  ;; Saved random / session_id from the original ClientHello. RFC 8446
  ;; §4.1.4 requires both to be reused verbatim in the retry ClientHello
  ;; we send after a HelloRetryRequest -- otherwise the server treats
  ;; the second handshake as a brand-new session and aborts.
  (client-random nil)
  (client-session-id nil)
  ;; Set T after we've already processed one HelloRetryRequest; per
  ;; RFC 8446 §4.1.4 a server MUST NOT send a second HRR in the same
  ;; handshake, so a second one is a protocol error.
  (hrr-already-seen nil :type boolean)
  ;; Transcript hash snapshots (for CertificateVerify)
  (transcript-before-cv nil)      ; hash before CertificateVerify was added
  ;; Server config (for server role)
  (server-config nil)
  ;; Received session tickets
  (received-tickets nil :type list)
  ;; T when this connection was resumed via PSK rather than negotiated
  ;; with a fresh certificate. Set on the server side after a binder
  ;; validates; surfaced for observability and tests.
  (resumed-p nil :type boolean)
  ;; OCSP staple attached to the leaf certificate, if any. Set on the
  ;; client side when the server emits a status_request CertificateEntry
  ;; extension. Application code can validate it; the TLS layer just
  ;; carries the opaque bytes.
  (ocsp-staple nil)
  ;; Optional callback invoked once for every NewSessionTicket arriving
  ;; on a client connection. Signature: (callback conn ticket). Used by
  ;; the session-ticket cache to populate itself without this layer
  ;; depending on the cache module.
  (new-session-ticket-callback nil))

(defun make-tls-connection (&key (role :client) hostname trust-store alpn-protocols
                                 new-session-ticket-callback)
  "Create a new TLS 1.3 connection."
  (%make-tls-connection
   :role role
   :hostname hostname
   :trust-store trust-store
   :alpn-protocols alpn-protocols
   :new-session-ticket-callback new-session-ticket-callback
   :send-buffer (make-buffer)))

(defun verify-certificate-verify-signature (conn scheme signature transcript-hash)
  "Verify the CertificateVerify signature against the transcript hash.
   RFC 8446 Section 4.4.3: the signed content is:
   64 spaces + context string + 0x00 + transcript hash."
  (let* ((cert (first (tls-connection-server-certificates conn)))
         (pk-bytes (x509:x509-cert-public-key-bytes cert))
         ;; Build the content that was signed
         (context (if (eq (tls-connection-role conn) :client)
                      "TLS 1.3, server CertificateVerify"
                      "TLS 1.3, client CertificateVerify"))
         (context-bytes (map '(vector (unsigned-byte 8)) #'char-code context))
         ;; 64 spaces + context + 0x00 + hash
         (content-len (+ 64 (length context-bytes) 1 (length transcript-hash)))
         (content (make-array content-len :element-type '(unsigned-byte 8))))
    ;; 64 spaces (0x20)
    (fill content #x20 :end 64)
    ;; Context string
    (replace content context-bytes :start1 64)
    ;; Separator byte 0x00
    (setf (aref content (+ 64 (length context-bytes))) 0)
    ;; Transcript hash
    (replace content transcript-hash :start1 (+ 64 (length context-bytes) 1))
    ;; Verify based on signature scheme
    (cond
      ((= scheme +sig-ed25519+)
       (unless (ed-sign:ed25519-verify pk-bytes content signature)
         (error "CertificateVerify: Ed25519 signature verification failed")))
      ((= scheme +sig-ecdsa-secp256r1-sha256+)
       (let ((point (ec-p256:p256-point-decode pk-bytes)))
         (unless point
           (error "CertificateVerify: invalid P-256 public key"))
         ;; ECDSA signature is DER-encoded SEQUENCE { INTEGER r, INTEGER s }
         (let* ((sig-seq (asn1:der-decode signature))
                (sig-tlvs (asn1:der-decode-sequence-contents sig-seq))
                (r (asn1:decode-der-integer (asn1:asn1-tlv-value (first sig-tlvs))))
                (s (asn1:decode-der-integer (asn1:asn1-tlv-value (second sig-tlvs)))))
           (unless (ecdsa:ecdsa-verify point content r s)
             (error "CertificateVerify: ECDSA-P-256 signature verification failed")))))
      ((= scheme +sig-ecdsa-secp384r1-sha384+)
       ;; Server cert's SubjectPublicKey is a SEC1 uncompressed P-384
       ;; point (97 bytes); signature is DER SEQUENCE{INTEGER r, INTEGER s}.
       (let ((point (ec-p384:p384-point-decode pk-bytes)))
         (unless point
           (error "CertificateVerify: invalid P-384 public key"))
         (let* ((sig-seq (asn1:der-decode signature))
                (sig-tlvs (asn1:der-decode-sequence-contents sig-seq))
                (r (asn1:decode-der-integer (asn1:asn1-tlv-value (first sig-tlvs))))
                (s (asn1:decode-der-integer (asn1:asn1-tlv-value (second sig-tlvs)))))
           (unless (ecdsa-p384:ecdsa-p384-verify point content r s)
             (error "CertificateVerify: ECDSA-P-384 signature verification failed")))))
      ((= scheme +sig-ecdsa-secp521r1-sha512+)
       (let ((point (ec-p521:p521-point-decode pk-bytes)))
         (unless point
           (error "CertificateVerify: invalid P-521 public key"))
         (let* ((sig-seq (asn1:der-decode signature))
                (sig-tlvs (asn1:der-decode-sequence-contents sig-seq))
                (r (asn1:decode-der-integer (asn1:asn1-tlv-value (first sig-tlvs))))
                (s (asn1:decode-der-integer (asn1:asn1-tlv-value (second sig-tlvs)))))
           (unless (ecdsa-p521:ecdsa-p521-verify point content r s)
             (error "CertificateVerify: ECDSA-P-521 signature verification failed")))))
      ((or (= scheme +sig-rsa-pss-rsae-sha256+)
           (= scheme +sig-rsa-pss-rsae-sha384+)
           (= scheme +sig-rsa-pss-rsae-sha512+))
       (let ((pk-alg (x509:x509-cert-public-key-algorithm cert))
             (hash-fn (cond ((= scheme +sig-rsa-pss-rsae-sha256+) :sha256)
                            ((= scheme +sig-rsa-pss-rsae-sha384+) :sha384)
                            (t :sha512))))
         (unless (equal pk-alg pkcs:+oid-rsa-encryption+)
           (error "CertificateVerify: expected RSA key, got ~A" pk-alg))
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (unless (rsa:rsa-pss-verify pub-key content signature :hash hash-fn)
               (error "CertificateVerify: RSA-PSS signature verification failed"))))))
      (t (error "Unsupported signature scheme: #x~4,'0X" scheme)))))

(defun parse-finished (payload)
  "Parse Finished message. Returns the verify_data bytes."
  payload)

(defun build-finished (ks base-key)
  "Build a Finished handshake message.
   BASE-KEY is the client/server handshake traffic secret."
  (let* ((finished-key (derive-finished-key ks base-key))
         (transcript-hash (transcript-current-hash ks))
         (verify-data (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                 finished-key transcript-hash)))
    (make-handshake-message +handshake-finished+ verify-data)))

(defun verify-finished (ks base-key verify-data)
  "Verify a Finished message's verify_data."
  (let* ((finished-key (derive-finished-key ks base-key))
         (transcript-hash (transcript-current-hash ks))
         (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                              finished-key transcript-hash)))
    (equalp verify-data expected)))

(defun tls-generate-key-shares (conn)
  "Generate ephemeral key pairs for key exchange.

   Each curve costs a key generation up front but lets the server pick
   any group from supported_groups without forcing a HelloRetryRequest
   round-trip. The cost difference is dominated by the ML-KEM hybrid
   keygen (~2 KB private state, milliseconds of work) so adding the
   FIPS curves here is essentially free in handshake-completion latency
   and pays for itself the first time we hit a P-384-only server."
  ;; X25519+ML-KEM-768 hybrid
  (multiple-value-bind (hybrid-pk hybrid-sk) (hybrid:hybrid-keygen)
    (setf (tls-connection-client-hybrid-private conn) hybrid-sk)
    (setf (tls-connection-client-hybrid-public conn) hybrid-pk))
  ;; X25519
  (let ((x25519-sk (drbg:random-bytes 32)))
    (setf (tls-connection-client-x25519-private conn) x25519-sk)
    (setf (tls-connection-client-x25519-public conn) (x25519:x25519-base x25519-sk)))
  ;; P-256
  (multiple-value-bind (p256-sk p256-pk) (ecdh:ecdh-p256-generate-keypair)
    (setf (tls-connection-client-p256-private conn) p256-sk)
    (setf (tls-connection-client-p256-public conn)
          (ec-p256:p256-point-encode-uncompressed p256-pk)))
  ;; P-384
  (multiple-value-bind (p384-sk p384-pk) (ecdh:ecdh-p384-generate-keypair)
    (setf (tls-connection-client-p384-private conn) p384-sk)
    (setf (tls-connection-client-p384-public conn)
          (ec-p384:p384-point-encode-uncompressed p384-pk)))
  ;; P-521
  (multiple-value-bind (p521-sk p521-pk) (ecdh:ecdh-p521-generate-keypair)
    (setf (tls-connection-client-p521-private conn) p521-sk)
    (setf (tls-connection-client-p521-public conn)
          (ec-p521:p521-point-encode-uncompressed p521-pk)))
  ;; ClientHello key_share entries, in client preference order. The
  ;; supported_groups extension carries the same order; servers commonly
  ;; pick the first group from supported_groups for which the client
  ;; sent a key_share, so listing hybrid first means modern servers pick
  ;; it without round-tripping.
  (list (list :x25519-mlkem768 (tls-connection-client-hybrid-public conn))
        (list :x25519 (tls-connection-client-x25519-public conn))
        (list :secp256r1 (tls-connection-client-p256-public conn))
        (list :secp384r1 (tls-connection-client-p384-public conn))
        (list :secp521r1 (tls-connection-client-p521-public conn))))

(defun tls-start-handshake (conn)
  "Begin a TLS 1.3 handshake. Returns the ClientHello record bytes to send."
  (let* ((key-shares (tls-generate-key-shares conn))
         ;; Generate client_random and legacy_session_id up front so we
         ;; can save them on the connection. If a HelloRetryRequest
         ;; arrives we'll reuse both verbatim per RFC 8446 §4.1.4.
         (client-random (drbg:random-bytes 32))
         (session-id (drbg:random-bytes 32))
         (ch (build-client-hello
              :hostname (tls-connection-hostname conn)
              :key-shares key-shares
              :alpn (tls-connection-alpn-protocols conn)
              :random client-random
              :session-id session-id)))
    (setf (tls-connection-client-random conn) client-random)
    (setf (tls-connection-client-session-id conn) session-id)
    ;; Save ClientHello for transcript re-hashing if cipher suite changes
    (setf (tls-connection-client-hello-msg conn) ch)
    ;; Initialize key schedule (we don't know cipher suite yet, assume SHA-256)
    (setf (tls-connection-key-schedule conn)
          (make-tls13-key-schedule :cipher-suite +tls-aes-128-gcm-sha256+))
    ;; Update transcript
    (transcript-update (tls-connection-key-schedule conn) ch)
    ;; Wrap in record
    (setf (tls-connection-state conn) :wait-server-hello)
    (serialize-tls-record
     (make-tls-record :content-type +content-handshake+
                      :data ch))))

(defun tls-process-server-hello (conn payload)
  "Process a ServerHello message."
  (let* ((sh (parse-server-hello payload))
         (cipher-suite (parsed-server-hello-cipher-suite sh))
         (ks (tls-connection-key-schedule conn)))
    ;; Verify supported version. Servers that don't speak TLS 1.3 either
    ;; omit the supported_versions extension or echo a non-1.3 codepoint;
    ;; surface the structured `tls-version-mismatch-error' so callers
    ;; (notably `tls-connect-with-fallback') can retry over TLS 1.2.
    (let ((version (server-hello-get-supported-version sh)))
      (unless (and version (= version +tls-1.3-supported-version+))
        (error 'tls-version-mismatch-error
               :negotiated-version version)))
    ;; Update cipher suite if different (e.g. server chose SHA-384 instead of SHA-256)
    (unless (= cipher-suite (tls13-key-schedule-cipher-suite ks))
      (setf ks (make-tls13-key-schedule :cipher-suite cipher-suite))
      (setf (tls-connection-key-schedule conn) ks)
      ;; Re-hash the saved ClientHello into the new transcript
      (let ((ch-msg (tls-connection-client-hello-msg conn)))
        (when ch-msg
          (transcript-update ks ch-msg))))
    (setf (tls-connection-cipher-suite conn) cipher-suite)
    (setf (tls-connection-server-random conn) (parsed-server-hello-random sh))
    ;; PSK acceptance signal. The server echoes pre_shared_key in
    ;; ServerHello when it picks one of our offered identities; its
    ;; absence after we offered a ticket means the server rejected it
    ;; (STEK rotation, expired, binder mismatch) and the rest of this
    ;; flight will carry Certificate / CertificateVerify. Recording the
    ;; flag here lets callers (e.g. the session-ticket cache wrapper)
    ;; invalidate stale entries instead of re-presenting them.
    ;;
    ;; When the server rejected our PSK, the early-secret stashed by
    ;; build-psk-client-hello (PSK-derived) is wrong -- the server is
    ;; running a zero-PSK schedule. Wipe the slot so the unless-guard
    ;; on derive-early-secret below recomputes it from zeros and the
    ;; two sides agree on the handshake secret.
    (cond
      ((server-hello-psk-selected-p sh)
       (setf (tls-connection-resumed-p conn) t))
      ((tls-connection-session-ticket conn)
       (setf (tls13-key-schedule-early-secret ks) nil)))
    ;; Process key_share
    (multiple-value-bind (group-id server-key) (server-hello-get-key-share sh)
      (setf (tls-connection-negotiated-group conn) group-id)
      (let ((shared-secret
              (cond
                ((= group-id +group-x25519-mlkem768+)
                 ;; Hybrid: server's key-share is a ML-KEM ciphertext
                 ;; concatenated with the server's ephemeral X25519 pk.
                 ;; hybrid-decaps produces the 64-byte combined secret
                 ;; that HKDF-Extract will consume as the IKM for the
                 ;; handshake-secret derivation.
                 (hybrid:hybrid-decaps
                  (tls-connection-client-hybrid-private conn)
                  server-key))
                ((= group-id +group-x25519+)
                 (x25519:x25519 (tls-connection-client-x25519-private conn) server-key))
                ((= group-id +group-secp256r1+)
                 ;; Decode server public key, compute ECDH shared secret (x-coordinate)
                 (let ((server-point (ec-p256:p256-point-decode server-key)))
                   (unless server-point
                     (error 'tls-alert-error
                            :level +alert-level-fatal+
                            :description +alert-decode-error+
                            :state :wait-server-hello))
                   (ecdh:ecdh-p256-shared-secret
                    (tls-connection-client-p256-private conn)
                    server-point)))
                ((= group-id +group-secp384r1+)
                 (let ((server-point (ec-p384:p384-point-decode server-key)))
                   (unless server-point
                     (error 'tls-alert-error
                            :level +alert-level-fatal+
                            :description +alert-decode-error+
                            :state :wait-server-hello))
                   (ecdh:ecdh-p384-shared-secret
                    (tls-connection-client-p384-private conn)
                    server-point)))
                ((= group-id +group-secp521r1+)
                 (let ((server-point (ec-p521:p521-point-decode server-key)))
                   (unless server-point
                     (error 'tls-alert-error
                            :level +alert-level-fatal+
                            :description +alert-decode-error+
                            :state :wait-server-hello))
                   (ecdh:ecdh-p521-shared-secret
                    (tls-connection-client-p521-private conn)
                    server-point)))
                (t (error "Unsupported key exchange group: ~X" group-id)))))
        ;; Update transcript with ServerHello
        (let ((sh-msg (make-handshake-message +handshake-server-hello+ payload)))
          (transcript-update ks sh-msg))
        ;; Derive handshake secret. On a resumption attempt the client
        ;; has already extracted the early secret from the resumption
        ;; PSK in build-psk-client-hello; do not clobber it with the
        ;; zero-PSK derivation.
        (unless (tls13-key-schedule-early-secret ks)
          (derive-early-secret ks))
        (derive-handshake-secret ks shared-secret)
        ;; Derive handshake traffic keys
        (let ((hs-secret (tls13-key-schedule-handshake-secret ks)))
          (let ((c-hs-secret (ks-derive-secret ks hs-secret "c hs traffic"))
                (s-hs-secret (ks-derive-secret ks hs-secret "s hs traffic")))
            (setf (tls-connection-client-hs-secret conn) c-hs-secret)
            (setf (tls-connection-server-hs-secret conn) s-hs-secret)
            (multiple-value-bind (c-key c-iv) (derive-traffic-keys ks c-hs-secret)
              (multiple-value-bind (s-key s-iv) (derive-traffic-keys ks s-hs-secret)
                (setf (tls-connection-client-handshake-key conn) c-key)
                (setf (tls-connection-client-handshake-iv conn) c-iv)
                (setf (tls-connection-server-handshake-key conn) s-key)
                (setf (tls-connection-server-handshake-iv conn) s-iv)))))
        (setf (tls-connection-state conn) :wait-encrypted-extensions)))))

(defun tls-process-encrypted-handshake (conn ciphertext)
  "Decrypt and process an encrypted handshake record.
   Returns response bytes to send (e.g. client Finished) or NIL."
  (multiple-value-bind (plaintext inner-ct)
      (decrypt-record ciphertext
                      (tls-connection-server-handshake-key conn)
                      (tls-connection-server-handshake-iv conn)
                      (tls-connection-server-seq conn)
                      (tls-connection-cipher-suite conn))
    (incf (tls-connection-server-seq conn))
    (unless (= inner-ct +content-handshake+)
      (error "Expected handshake content type, got ~D" inner-ct))
    ;; Parse handshake messages (may be coalesced)
    (let ((pos 0)
          (response nil))
      (loop while (< pos (length plaintext))
            do (multiple-value-bind (type payload next-pos)
                   (parse-handshake-header plaintext pos)
                 (unless type (return))
                 ;; Save transcript hash before CertificateVerify/Finished
                 (when (or (= type +handshake-certificate-verify+)
                           (= type +handshake-finished+))
                   (setf (tls-connection-transcript-before-cv conn)
                         (transcript-current-hash (tls-connection-key-schedule conn))))
                 ;; Update transcript with the full handshake message
                 (let ((msg (subseq plaintext pos next-pos)))
                   (transcript-update (tls-connection-key-schedule conn) msg))
                 ;; Process based on state and message type
                 (let ((result (tls-process-handshake-message conn type payload)))
                   (when (and result (typep result '(simple-array (unsigned-byte 8) (*))))
                     (setf response result)))
                 (setf pos next-pos)))
      response)))

(defun tls-process-handshake-message (conn type payload)
  "Process a single handshake message in the current state."
  (let ((state (tls-connection-state conn)))
    (cond
      ;; EncryptedExtensions
      ((and (eq state :wait-encrypted-extensions)
            (= type +handshake-encrypted-extensions+))
       (let ((exts (parse-encrypted-extensions payload)))
         ;; Check ALPN
         (let ((alpn (assoc +ext-alpn+ exts)))
           (when alpn
             (let* ((data (cdr alpn))
                    (pos 0))
               (multiple-value-bind (list-len new-pos) (read-u16 data pos)
                 (declare (ignore list-len))
                 (multiple-value-bind (proto-len new-pos2) (read-u8 data new-pos)
                   (setf (tls-connection-alpn-protocol conn)
                         (map 'string #'code-char (subseq data new-pos2 (+ new-pos2 proto-len)))))))))
         (setf (tls-connection-state conn) :wait-certificate-request)))

      ;; CertificateRequest (optional, skip for now)
      ((and (eq state :wait-certificate-request)
            (= type +handshake-certificate-request+))
       ;; We don't support client certificates yet
       (setf (tls-connection-state conn) :wait-certificate))

      ;; Certificate (may come instead of CertificateRequest)
      ((and (member state '(:wait-certificate-request :wait-certificate))
            (= type +handshake-certificate+))
       (multiple-value-bind (cert-data-list leaf-staple)
           (parse-certificate-message payload)
         (setf (tls-connection-server-certificates conn)
               (mapcar #'x509:parse-x509-certificate cert-data-list))
         (when leaf-staple
           (setf (tls-connection-ocsp-staple conn) leaf-staple)))
       (setf (tls-connection-state conn) :wait-certificate-verify))

      ;; CompressedCertificate (RFC 8879 §4): inflate the body and
      ;; dispatch to the regular Certificate parser.
      ((and (member state '(:wait-certificate-request :wait-certificate))
            (= type +handshake-compressed-certificate+))
       (let ((decompressed (decompress-certificate-payload payload)))
         (multiple-value-bind (cert-data-list leaf-staple)
             (parse-certificate-message decompressed)
           (setf (tls-connection-server-certificates conn)
                 (mapcar #'x509:parse-x509-certificate cert-data-list))
           (when leaf-staple
             (setf (tls-connection-ocsp-staple conn) leaf-staple))))
       (setf (tls-connection-state conn) :wait-certificate-verify))

      ;; CertificateVerify
      ((and (eq state :wait-certificate-verify)
            (= type +handshake-certificate-verify+))
       (multiple-value-bind (scheme signature) (parse-certificate-verify payload)
         (let ((transcript-hash (tls-connection-transcript-before-cv conn)))
           (when transcript-hash
             (verify-certificate-verify-signature
              conn scheme signature transcript-hash))))
       (setf (tls-connection-state conn) :wait-finished))

      ;; Finished -- on the PSK resumption path the server omits
      ;; Certificate/CertificateVerify, so Finished arrives while the
      ;; client is still in :wait-certificate-request.
      ((and (member state '(:wait-finished :wait-certificate-request))
            (= type +handshake-finished+))
       (let ((verify-data (parse-finished payload))
             (ks (tls-connection-key-schedule conn))
             (transcript-hash (tls-connection-transcript-before-cv conn)))
         ;; Verify the Finished message
         (when transcript-hash
           (let* ((finished-key (derive-finished-key ks
                                  (tls-connection-server-hs-secret conn)))
                  (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                       finished-key transcript-hash)))
             (unless (equalp verify-data expected)
               (error "Finished message verification failed"))))
         ;; Build client Finished BEFORE deriving master secret
         ;; (transcript must include server Finished but not client Finished yet)
         (let* ((client-finished (build-finished ks (tls-connection-client-hs-secret conn)))
                ;; Encrypt with client handshake keys
                (encrypted-finished
                  (encrypt-record client-finished +content-handshake+
                                  (tls-connection-client-handshake-key conn)
                                  (tls-connection-client-handshake-iv conn)
                                  (tls-connection-client-seq conn)
                                  (tls-connection-cipher-suite conn)))
                ;; Build the record
                (finished-record
                  (serialize-tls-record
                   (make-tls-record :content-type +content-application-data+
                                    :data encrypted-finished)))
                ;; CCS for middlebox compatibility
                (ccs-record (make-array 6 :element-type '(unsigned-byte 8)
                                        :initial-contents '(20 3 3 0 1 1)))
                ;; Combined response: CCS + encrypted Finished
                (response (make-array (+ 6 (length finished-record))
                                      :element-type '(unsigned-byte 8))))
           (replace response ccs-record)
           (replace response finished-record :start1 6)
           (incf (tls-connection-client-seq conn))
           ;; Derive master secret and app keys BEFORE adding client Finished
           ;; to transcript (RFC 8446: app keys use transcript up to server Finished)
           (derive-master-secret ks)
           (multiple-value-bind (c-key c-iv s-key s-iv)
               (derive-application-traffic-keys ks)
             (setf (tls-connection-client-app-key conn) c-key)
             (setf (tls-connection-client-app-iv conn) c-iv)
             (setf (tls-connection-server-app-key conn) s-key)
             (setf (tls-connection-server-app-iv conn) s-iv))
           ;; NOW update transcript with client Finished (needed for resumption PSK)
           (transcript-update ks client-finished)
           ;; Reset sequence numbers for application traffic
           (setf (tls-connection-client-seq conn) 0)
           (setf (tls-connection-server-seq conn) 0)
           (setf (tls-connection-state conn) :connected)
           ;; Return the response bytes to send to the server
           response)))

      (t
       (error "Unexpected handshake message type ~D in state ~A" type state)))))

(defun tls-handshake-step (conn record-bytes)
  "Process an incoming TLS record during handshake.
   RECORD-BYTES is the raw bytes received from the network.
   Returns :need-data if more data is needed, :connected when handshake completes,
   or a byte vector of response data to send."
  (declare (notinline tls-alert-level tls-alert-description))
  (let ((record (parse-tls-record record-bytes)))
    (unless record
      (return-from tls-handshake-step :need-data))
    (let ((state (tls-connection-state conn))
          (ct (tls-record-content-type record))
          (data (tls-record-data record)))
      (cond
        ;; ServerHello arrives as plaintext handshake
        ((and (eq state :wait-server-hello)
              (= ct +content-handshake+))
         ;; data includes 4-byte handshake header; strip it for parse-server-hello
         (multiple-value-bind (hs-type payload)
             (parse-handshake-header data 0)
           (declare (ignore hs-type))
           (cond
             ;; HelloRetryRequest is wire-encoded as a ServerHello with a
             ;; specific magic random value. Detect first; if so, build
             ;; the retry ClientHello and return it for the caller to
             ;; send. State stays :wait-server-hello.
             ((hello-retry-request-p payload)
              (tls-process-hello-retry-request conn payload))
             (t
              (tls-process-server-hello conn payload)
              ;; After ServerHello, server sends encrypted handshake records
              nil))))
        ;; Encrypted handshake records (EncryptedExtensions, Certificate, etc.)
        ((and (member state '(:wait-encrypted-extensions :wait-certificate-request
                              :wait-certificate :wait-certificate-verify :wait-finished))
              (= ct +content-application-data+))
         (let ((response (tls-process-encrypted-handshake conn data)))
           (if (eq (tls-connection-state conn) :connected)
               (or response :connected)
               response)))
        ;; Change cipher spec (TLS 1.3 middlebox compatibility, ignore)
        ((= ct +content-change-cipher-spec+)
         nil)
        ;; Plaintext Alert during handshake -- always carry actionable
        ;; info (level + description) up to the caller so HTTPS clients
        ;; can log "TLS alert (fatal): handshake_failure" instead of
        ;; "Unexpected record type 21". This is the Stage-1 visibility
        ;; fix from manual/implement/380_tls-client-cdn-compliance.md;
        ;; without it every "no shared curve / no shared sigalg / cert
        ;; not trusted" failure surfaces as the same opaque message.
        ((= ct +content-alert+)
         (let* ((alert (parse-alert data))
                (level (tls-alert-level alert))
                (desc (tls-alert-description alert)))
           (error 'tls-alert-error :level level :description desc :state state)))
        (t
         (error "Unexpected record type ~D in state ~A" ct state))))))

;;; ---------------------------------------------------------------------------
;;; Application data
;;; ---------------------------------------------------------------------------

(defun connection-write-keys (conn)
  "Return (values key iv seq-accessor) for the sending side based on role."
  (if (eq (tls-connection-role conn) :server)
      (values (tls-connection-server-app-key conn)
              (tls-connection-server-app-iv conn)
              (tls-connection-server-seq conn))
      (values (tls-connection-client-app-key conn)
              (tls-connection-client-app-iv conn)
              (tls-connection-client-seq conn))))

(defun connection-read-keys (conn)
  "Return (values key iv seq) for the receiving side based on role."
  (if (eq (tls-connection-role conn) :server)
      (values (tls-connection-client-app-key conn)
              (tls-connection-client-app-iv conn)
              (tls-connection-client-seq conn))
      (values (tls-connection-server-app-key conn)
              (tls-connection-server-app-iv conn)
              (tls-connection-server-seq conn))))

(defun bump-write-seq (conn)
  "Increment the write sequence number.
   Signals an error at the 2^64-1 boundary (RFC 8446 S5.5)."
  (let ((seq (if (eq (tls-connection-role conn) :server)
                 (tls-connection-server-seq conn)
                 (tls-connection-client-seq conn))))
    (when (>= seq +max-sequence-number+)
      (error "TLS sequence number overflow: rekey required"))
    (if (eq (tls-connection-role conn) :server)
        (incf (tls-connection-server-seq conn))
        (incf (tls-connection-client-seq conn)))))

(defun bump-read-seq (conn)
  "Increment the read sequence number.
   Signals an error at the 2^64-1 boundary (RFC 8446 S5.5)."
  (let ((seq (if (eq (tls-connection-role conn) :server)
                 (tls-connection-client-seq conn)
                 (tls-connection-server-seq conn))))
    (when (>= seq +max-sequence-number+)
      (error "TLS sequence number overflow: rekey required"))
    (if (eq (tls-connection-role conn) :server)
        (incf (tls-connection-client-seq conn))
        (incf (tls-connection-server-seq conn)))))

(defun tls-send-application-data (conn data)
  "Encrypt application data for sending.
   Returns the TLS record bytes to send."
  (unless (eq (tls-connection-state conn) :connected)
    (error "Cannot send data: not connected (state: ~A)" (tls-connection-state conn)))
  (multiple-value-bind (key iv seq) (connection-write-keys conn)
    (let ((ciphertext (encrypt-record data +content-application-data+
                                      key iv seq
                                      (tls-connection-cipher-suite conn))))
      (bump-write-seq conn)
      (serialize-tls-record
       (make-tls-record :content-type +content-application-data+
                        :data ciphertext)))))

(defun tls-receive-application-data (conn ciphertext)
  "Decrypt received application data.
   Returns the plaintext bytes."
  (unless (eq (tls-connection-state conn) :connected)
    (error "Cannot receive data: not connected (state: ~A)" (tls-connection-state conn)))
  (multiple-value-bind (key iv seq) (connection-read-keys conn)
    (multiple-value-bind (plaintext inner-ct)
        (decrypt-record ciphertext key iv seq
                        (tls-connection-cipher-suite conn))
      (bump-read-seq conn)
      (cond
        ((= inner-ct +content-application-data+) plaintext)
        ((= inner-ct +content-alert+)
         (let ((alert-desc (if (> (length plaintext) 1) (aref plaintext 1) 0)))
           (if (zerop alert-desc)
               ;; close_notify (0) is a clean shutdown per RFC 8446
               :close-notify
               (error "TLS alert received: ~D" alert-desc))))
        ((= inner-ct +content-handshake+)
         ;; Post-handshake messages (NewSessionTicket, KeyUpdate)
         (tls-process-post-handshake conn plaintext)
         nil)
        (t (error "Unexpected inner content type: ~D" inner-ct))))))

;;; ---------------------------------------------------------------------------
;;; Alert Protocol (RFC 8446 Section 6)
;;; ---------------------------------------------------------------------------

(defstruct tls-alert
  (level 0 :type (unsigned-byte 8))
  (description 0 :type (unsigned-byte 8)))

(defun make-alert-record (level description)
  "Build a TLS alert record (2 bytes: level + description)."
  (let ((data (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref data 0) level)
    (setf (aref data 1) description)
    (serialize-tls-record
     (make-tls-record :content-type +content-alert+
                      :data data))))

(defun %send-alert-and-error (transport alert-description format-string &rest format-args)
  "Send a fatal TLS alert to TRANSPORT (unencrypted), then signal an error.
   Used during the pre-encryption handshake phase to ensure peers receive a
   proper rejection instead of a silent connection drop."
  (ignore-errors
    (write-bytes-to-transport transport
      (make-alert-record +alert-level-fatal+ alert-description)))
  (apply #'error format-string format-args))

(defun make-encrypted-alert-record (conn level description)
  "Build an encrypted TLS alert record for a connected session."
  (let ((data (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref data 0) level)
    (setf (aref data 1) description)
    (multiple-value-bind (key iv seq) (connection-write-keys conn)
      (let ((ciphertext (encrypt-record data +content-alert+
                                        key iv seq
                                        (tls-connection-cipher-suite conn))))
        (bump-write-seq conn)
        (serialize-tls-record
         (make-tls-record :content-type +content-application-data+
                          :data ciphertext))))))

(defun parse-alert (data)
  "Parse a 2-byte alert message. Returns a tls-alert struct."
  (when (< (length data) 2)
    (error "Alert message too short"))
  (make-tls-alert :level (aref data 0) :description (aref data 1)))

(defun tls-close-notify (conn)
  "Send a close_notify alert and transition to :closed state.
   Returns the record bytes to send."
  (prog1
      (if (eq (tls-connection-state conn) :connected)
          (make-encrypted-alert-record conn +alert-level-warning+ +alert-close-notify+)
          (make-alert-record +alert-level-warning+ +alert-close-notify+))
    (setf (tls-connection-state conn) :closed)))

(defun named-group-name (group-id)
  "Map a TLS named-group codepoint to a short keyword. Used for logging
   the key-exchange group the server actually picked."
  (cond
    ((null group-id) nil)
    ((= group-id +group-x25519+)         :x25519)
    ((= group-id +group-secp256r1+)      :secp256r1)
    ((= group-id +group-secp384r1+)      :secp384r1)
    ((= group-id +group-secp521r1+)      :secp521r1)
    ((= group-id +group-x25519-mlkem768+) :x25519-mlkem768)
    (t (intern (format nil "GROUP-0X~4,'0X" group-id) :keyword))))

(defun cipher-suite-name (suite-id)
  "Map a TLS 1.3 cipher-suite codepoint to a short keyword for logs and
   metric labels. Returns NIL for an unknown suite. The labels match
   the IANA registry shorthand minus the TLS_ prefix."
  (cond
    ((null suite-id) nil)
    ((= suite-id +tls-aes-128-gcm-sha256+)       :aes-128-gcm-sha256)
    ((= suite-id +tls-aes-256-gcm-sha384+)       :aes-256-gcm-sha384)
    ((= suite-id +tls-chacha20-poly1305-sha256+) :chacha20-poly1305-sha256)
    (t (intern (format nil "SUITE-0X~4,'0X" suite-id) :keyword))))

;; OIDs for the public-key algorithms TLS 1.3 servers commonly use.
;; Kept private to avoid leaking another OID surface; only consumed by
;; cert-key-type-label below.
(defparameter %oid-ed25519+    '(1 3 101 112))
(defparameter %oid-ec-public+  '(1 2 840 10045 2 1))
(defparameter %oid-rsa+        '(1 2 840 113549 1 1 1))

(defun cert-key-type-label (leaf-cert)
  "Short keyword identifying the leaf cert's public-key algorithm,
   suitable as a Prometheus label. Returns NIL if the OID isn't one
   we recognise. ECDSA isn't refined further by curve here -- the
   group label on the handshake metric already carries the ECDH
   curve, which is the more useful number."
  (when leaf-cert
    (let ((alg (x509:x509-cert-public-key-algorithm leaf-cert)))
      (cond
        ((equal alg %oid-ed25519+)   :ed25519)
        ((equal alg %oid-ec-public+) :ecdsa)
        ((equal alg %oid-rsa+)       :rsa)
        (t nil)))))

(defun alert-description-name (code)
  "Return a human-readable name for an alert description code."
  (case code
    (0 "close_notify") (10 "unexpected_message") (20 "bad_record_mac")
    (40 "handshake_failure") (42 "bad_certificate")
    (47 "illegal_parameter") (45 "certificate_expired")
    (48 "unknown_ca") (50 "decode_error") (51 "decrypt_error")
    (70 "protocol_version") (71 "insufficient_security")
    (80 "internal_error") (86 "inappropriate_fallback")
    (109 "missing_extension") (110 "unsupported_extension")
    (112 "unrecognized_name") (115 "unknown_psk_identity")
    (116 "certificate_required") (120 "no_application_protocol")
    (t (format nil "unknown(~D)" code))))

(define-condition tls-version-mismatch-error (error)
  ((negotiated-version :initarg :negotiated-version
                       :reader tls-version-mismatch-negotiated-version
                       :documentation
                       "The version code (e.g. #x0303 for TLS 1.2) the server
                        negotiated, or NIL if no usable version was signalled.")
   (legacy-version :initarg :legacy-version :initform nil
                   :reader tls-version-mismatch-legacy-version
                   :documentation
                   "ServerHello.legacy_version (always #x0303 for any modern peer)."))
  (:report (lambda (c s)
             (let ((nv (tls-version-mismatch-negotiated-version c)))
               (format s "TLS 1.3 client received a non-1.3 ServerHello~
                          ~@[ (negotiated version #x~4,'0X)~]; ~
                          tls-connect-with-fallback can retry over TLS 1.2"
                       nv))))
  (:documentation
   "Signaled when a TLS 1.3 ClientHello receives a ServerHello that does
    not select TLS 1.3 (no supported_versions extension or one that does
    not name #x0304). Distinct from `tls-alert-error': the peer didn't
    abort the handshake itself, it simply doesn't speak TLS 1.3, so the
    caller can retry with the TLS 1.2 stack."))

(define-condition tls-alert-error (error)
  ((level :initarg :level :reader tls-alert-error-level
          :documentation "1 = warning, 2 = fatal (RFC 8446 §6).")
   (description :initarg :description :reader tls-alert-error-description
                :documentation "AlertDescription byte (e.g. 40 for handshake_failure).")
   (state :initarg :state :reader tls-alert-error-state :initform nil
          :documentation "TLS state at receipt time (:wait-server-hello etc.) or NIL."))
  (:report (lambda (c s)
             (format s "TLS alert (~A): ~A [~D]~@[ during ~A~]"
                     (cond ((= (tls-alert-error-level c) +alert-level-fatal+) "fatal")
                           ((= (tls-alert-error-level c) +alert-level-warning+) "warning")
                           (t (format nil "level=~D" (tls-alert-error-level c))))
                     (alert-description-name (tls-alert-error-description c))
                     (tls-alert-error-description c)
                     (tls-alert-error-state c))))
  (:documentation
   "Signaled when the peer sends a TLS Alert record that the local stack
    has chosen to surface rather than absorb. The most common case is a
    handshake-time alert from the server (handshake_failure,
    protocol_version, insufficient_security, unknown_ca on the cert
    chain we presented, ...). The previous behavior was to error with
    `Unexpected record type 21' which threw away the alert payload."))

;;; ---------------------------------------------------------------------------
;;; PSK / Session Resumption (RFC 8446 Section 4.6.1, 4.2.11)
;;; ---------------------------------------------------------------------------

(defstruct tls-session-ticket
  (ticket nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (lifetime 0 :type integer)              ; seconds
  (age-add 0 :type (unsigned-byte 32))    ; obfuscated age
  (nonce nil)                              ; ticket nonce
  (max-early-data 0 :type integer)        ; max_early_data_size
  (cipher-suite 0 :type fixnum)           ; cipher suite used
  (resumption-secret nil)                  ; resumption master secret
  (creation-time 0 :type integer))         ; universal-time of creation

(defun parse-new-session-ticket (payload)
  "Parse a NewSessionTicket handshake message (RFC 8446 Section 4.6.1).
   Returns a tls-session-ticket struct."
  (let ((pos 0))
    ;; ticket_lifetime (4 bytes)
    (multiple-value-bind (lifetime new-pos) (read-u32 payload pos)
      (setf pos new-pos)
      ;; ticket_age_add (4 bytes)
      (multiple-value-bind (age-add new-pos2) (read-u32 payload pos)
        (setf pos new-pos2)
        ;; ticket_nonce (opaque <0..255>)
        (multiple-value-bind (nonce-len new-pos3) (read-u8 payload pos)
          (setf pos new-pos3)
          (let ((nonce (subseq payload pos (+ pos nonce-len))))
            (incf pos nonce-len)
            ;; ticket (opaque <1..2^16-1>)
            (multiple-value-bind (ticket-len new-pos4) (read-u16 payload pos)
              (setf pos new-pos4)
              (let ((ticket (subseq payload pos (+ pos ticket-len))))
                (incf pos ticket-len)
                ;; extensions (Extension extensions<0..2^16-2>)
                (multiple-value-bind (ext-len new-pos5) (read-u16 payload pos)
                  (setf pos new-pos5)
                  (let ((max-early-data 0))
                    ;; Parse extensions looking for early_data
                    (let ((ext-end (+ pos ext-len)))
                      (loop while (< pos ext-end)
                            do (multiple-value-bind (etype new-pos6) (read-u16 payload pos)
                                 (setf pos new-pos6)
                                 (multiple-value-bind (elen new-pos7) (read-u16 payload pos)
                                   (setf pos new-pos7)
                                   (when (= etype 42) ; early_data extension
                                     (setf max-early-data (read-u32 payload pos)))
                                   (incf pos elen)))))
                    (make-tls-session-ticket
                     :ticket ticket
                     :lifetime lifetime
                     :age-add age-add
                     :nonce nonce
                     :max-early-data max-early-data
                     :creation-time (get-universal-time))))))))))))

(defun read-u32 (data pos &optional (what "u32"))
  "Read a 4-byte big-endian unsigned integer."
  (tls13-need data pos 4 what)
  (values (logior (ash (aref data pos) 24)
                  (ash (aref data (+ pos 1)) 16)
                  (ash (aref data (+ pos 2)) 8)
                  (aref data (+ pos 3)))
          (+ pos 4)))

(defun derive-resumption-psk (ks ticket)
  "Derive the PSK for resumption from a session ticket.
   PSK = HKDF-Expand-Label(resumption_master_secret, \"resumption\", ticket_nonce, Hash.length)"
  (ks-hkdf-expand-label ks
                        (tls-session-ticket-resumption-secret ticket)
                        "resumption"
                        (tls-session-ticket-nonce ticket)
                        (tls13-key-schedule-hash-len ks)))

(defun compute-psk-binder (ks psk partial-client-hello)
  "Compute the PSK binder value for a ClientHello.
   RFC 8446 Section 4.2.11.2.

   binder_key = Derive-Secret(early_secret, \"res binder\", \"\")
              = HKDF-Expand-Label(early_secret, \"res binder\", Hash(\"\"), L)

   Note: the context for binder_key is Hash(\"\") (the hash of the
   empty string), NOT the empty byte vector. Derive-Secret always
   hashes its Messages input via Transcript-Hash before feeding it
   to HKDF-Expand-Label. Using the empty byte vector instead of
   Hash(\"\") produces a different binder_key and causes every real-
   world client's binder to fail validation."
  (let* ((hash-fn (tls13-key-schedule-hash-fn ks))
         (early-secret (ks-hkdf-extract ks
                         (make-array (tls13-key-schedule-hash-len ks)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         psk))
         ;; Derive-Secret context = Hash("") per RFC 8446 §7.1.
         (binder-key (ks-hkdf-expand-label ks early-secret
                                           "res binder"
                                           (hash-empty hash-fn)
                                           (tls13-key-schedule-hash-len ks)))
         ;; finished_key context is raw "" (HKDF-Expand-Label, not
         ;; Derive-Secret) per RFC 8446 §4.4.4.
         (finished-key (ks-hkdf-expand-label ks binder-key
                                             "finished" +empty-context+
                                             (tls13-key-schedule-hash-len ks)))
         ;; Hash of the partial ClientHello (everything before binders)
         (hash-fn (tls13-key-schedule-hash-fn ks))
         (transcript-hash (ecase hash-fn
                            (:sha256 (sha256:sha256 partial-client-hello))
                            (:sha384 (sha512:sha384 partial-client-hello)))))
    (hmac:hmac hash-fn finished-key transcript-hash)))

(defun build-psk-client-hello (conn ticket &key alpn)
  "Build a ClientHello with pre_shared_key extension for session resumption.
   Returns the ClientHello record bytes to send."
  (let* ((key-shares (tls-generate-key-shares conn))
         (ks-temp (make-tls13-key-schedule :cipher-suite (tls-session-ticket-cipher-suite ticket)))
         (psk (derive-resumption-psk ks-temp ticket))
         (obfuscated-age (logand (+ (- (get-universal-time)
                                       (tls-session-ticket-creation-time ticket))
                                    (tls-session-ticket-age-add ticket))
                                 #xFFFFFFFF))
         (binder-len (tls13-key-schedule-hash-len ks-temp)))
    ;; Store ticket info
    (setf (tls-connection-session-ticket conn) ticket)
    ;; Build the ClientHello body
    (let ((buf (make-buffer)))
      ;; client_version (legacy)
      (buf-append-u16 buf +tls-1.3+)
      ;; random
      (buf-append-bytes buf (drbg:random-bytes 32))
      ;; legacy_session_id
      (buf-append-u8-prefixed buf (drbg:random-bytes 32))
      ;; cipher_suites
      (buf-append-u16 buf 6)
      (buf-append-u16 buf +tls-aes-128-gcm-sha256+)
      (buf-append-u16 buf +tls-aes-256-gcm-sha384+)
      (buf-append-u16 buf +tls-chacha20-poly1305-sha256+)
      ;; legacy_compression_methods
      (buf-append-byte buf 1)
      (buf-append-byte buf 0)
      ;; Extensions
      (let ((exts-buf (make-buffer)))
        ;; Standard extensions
        (buf-append-bytes exts-buf (make-supported-versions-ext-client))
        (buf-append-bytes exts-buf (make-supported-groups-ext))
        (buf-append-bytes exts-buf (make-key-share-ext-client key-shares))
        (buf-append-bytes exts-buf (make-signature-algorithms-ext))
        (when (tls-connection-hostname conn)
          (buf-append-bytes exts-buf (make-server-name-ext (tls-connection-hostname conn))))
        (when alpn
          (buf-append-bytes exts-buf (make-alpn-ext alpn)))
        ;; early_data (if ticket allows it and data is queued)
        (when (and (tls-connection-early-data conn)
                   (> (tls-session-ticket-max-early-data ticket) 0))
          (buf-append-u16 exts-buf 42)   ; early_data extension type
          (buf-append-u16 exts-buf 0))   ; empty extension data
        ;; pre_shared_key MUST be last extension
        (let ((psk-buf (make-buffer)))
          ;; identities list
          (let ((id-buf (make-buffer)))
            (buf-append-u16-prefixed id-buf (tls-session-ticket-ticket ticket))
            ;; obfuscated_ticket_age (4 bytes)
            (buf-append-byte id-buf (logand (ash obfuscated-age -24) #xFF))
            (buf-append-byte id-buf (logand (ash obfuscated-age -16) #xFF))
            (buf-append-byte id-buf (logand (ash obfuscated-age -8) #xFF))
            (buf-append-byte id-buf (logand obfuscated-age #xFF))
            (buf-append-u16-prefixed psk-buf (buf-freeze id-buf)))
          ;; binders list (placeholder zeros, patched after computing)
          (let ((binders-buf (make-buffer)))
            (buf-append-u8-prefixed binders-buf
                                    (make-array binder-len :element-type '(unsigned-byte 8)
                                                :initial-element 0))
            (buf-append-u16-prefixed psk-buf (buf-freeze binders-buf)))
          ;; Write PSK extension
          (buf-append-u16 exts-buf +ext-pre-shared-key+)
          (buf-append-u16-prefixed exts-buf (buf-freeze psk-buf)))
        ;; Finalize extensions
        (buf-append-u16-prefixed buf (buf-freeze exts-buf)))
      ;; Now compute and patch the PSK binder
      (let* ((ch-content (buf-freeze buf))
             (ks (make-tls13-key-schedule
                  :cipher-suite (tls-session-ticket-cipher-suite ticket)))
             ;; Partial = handshake header + content up to but not including binder value
             ;; binder value is last binder-len bytes; preceded by 1-byte length + 2-byte binders list len
             (partial-len (- (length ch-content) binder-len 1 2))
             (partial-msg (make-array (+ 4 partial-len) :element-type '(unsigned-byte 8))))
        (setf (aref partial-msg 0) +handshake-client-hello+)
        (let ((total-len (length ch-content)))
          (setf (aref partial-msg 1) (logand (ash total-len -16) #xFF))
          (setf (aref partial-msg 2) (logand (ash total-len -8) #xFF))
          (setf (aref partial-msg 3) (logand total-len #xFF)))
        (replace partial-msg ch-content :start1 4 :end1 (+ 4 partial-len))
        ;; Compute binder and patch it in
        (let ((binder (compute-psk-binder ks psk partial-msg)))
          (replace ch-content binder :start1 (- (length ch-content) binder-len)))
        ;; Wrap in handshake message
        (let ((ch-msg (make-handshake-message +handshake-client-hello+ ch-content)))
          ;; Initialize key schedule with PSK
          (setf (tls-connection-key-schedule conn) ks)
          (derive-early-secret ks psk)
          (transcript-update ks ch-msg)
          (setf (tls-connection-state conn) :wait-server-hello)
          (serialize-tls-record
           (make-tls-record :content-type +content-handshake+
                            :data ch-msg)))))))

;;; ---------------------------------------------------------------------------
;;; 0-RTT Early Data (RFC 8446 Section 4.2.10)
;;;
;;; WARNING: 0-RTT data is inherently replayable. An attacker who
;;; captures the ClientHello + early data can replay both to the server.
;;; The server MUST implement application-level anti-replay (e.g. a
;;; single-use token or idempotency check) for any request carried as
;;; early data. Without anti-replay, 0-RTT should be disabled by setting
;;; max-early-data to 0 in session tickets.
;;; ---------------------------------------------------------------------------

(defun tls-send-early-data (conn data)
  "Encrypt and send early data (0-RTT) after sending a PSK ClientHello.
   Must be called after build-psk-client-hello and before processing ServerHello.
   WARNING: 0-RTT is replayable; only use with idempotent requests.
   Returns the record bytes to send."
  (unless (eq (tls-connection-state conn) :wait-server-hello)
    (error "Cannot send early data in state ~A" (tls-connection-state conn)))
  (unless (tls-connection-session-ticket conn)
    (error "Cannot send early data without a session ticket"))
  (let* ((ks (tls-connection-key-schedule conn))
         ;; Derive early traffic secret
         (c-early-secret (ks-derive-secret ks
                           (tls13-key-schedule-early-secret ks)
                           "c e traffic"))
         ;; Derive early traffic keys
         (key-len (cipher-suite-key-len (tls-connection-cipher-suite conn)))
         (key (ks-hkdf-expand-label ks c-early-secret "key" +empty-context+ key-len))
         (iv (ks-hkdf-expand-label ks c-early-secret "iv" +empty-context+ 12)))
    ;; Encrypt the early data
    (let ((ciphertext (encrypt-record data +content-application-data+
                                      key iv
                                      (tls-connection-client-seq conn)
                                      (tls-connection-cipher-suite conn))))
      (incf (tls-connection-client-seq conn))
      (serialize-tls-record
       (make-tls-record :content-type +content-application-data+
                        :data ciphertext)))))



;;; ---------------------------------------------------------------------------
;;; HelloRetryRequest (RFC 8446 Section 4.1.4)
;;; ---------------------------------------------------------------------------

(defun hello-retry-request-p (server-hello-payload)
  "Check if a ServerHello is actually a HelloRetryRequest.
   Identified by the special random value (constant-time comparison)."
  (and (>= (length server-hello-payload) 34)
       (ct:ct-equal (subseq server-hello-payload 2 34)
                    +hello-retry-request-magic+)))

(defun hrr-selected-group (sh)
  "Extract the selected_group (named-group codepoint) from a
   HelloRetryRequest's key_share extension. Returns NIL if absent.

   In a real ServerHello the key_share extension carries (group_id ||
   length-prefixed key_data); in HRR it carries only the 2-byte group_id
   per RFC 8446 §4.2.8."
  (let ((ext (assoc +ext-key-share+ (parsed-server-hello-extensions sh))))
    (when (and ext (>= (length (cdr ext)) 2))
      (let ((data (cdr ext)))
        (logior (ash (aref data 0) 8) (aref data 1))))))

(defun hrr-cookie (sh)
  "Extract the cookie bytes from a HelloRetryRequest's cookie extension.
   Per RFC 8446 §4.2.2 this is a single u16-prefixed opaque blob.
   Returns NIL when absent."
  (let ((ext (assoc +ext-cookie+ (parsed-server-hello-extensions sh))))
    (when (and ext (>= (length (cdr ext)) 2))
      (let* ((data (cdr ext))
             (len (logior (ash (aref data 0) 8) (aref data 1))))
        (when (>= (length data) (+ 2 len))
          (subseq data 2 (+ 2 len)))))))

(defun %group-id->keyword (group-id)
  "Map a TLS named-group codepoint to the keyword make-key-share-ext-client uses."
  (cond
    ((= group-id +group-x25519+)         :x25519)
    ((= group-id +group-secp256r1+)      :secp256r1)
    ((= group-id +group-secp384r1+)      :secp384r1)
    ((= group-id +group-secp521r1+)      :secp521r1)
    ((= group-id +group-x25519-mlkem768+) :x25519-mlkem768)
    (t nil)))

(defun %connection-public-for-group (conn group-id)
  "Return the previously-generated public key bytes for GROUP-ID held on
   CONN, or NIL if we don't already have a keypair for that group.
   tls-generate-key-shares populates keypairs for every supported curve
   so any of these slots is always live by the time we receive HRR."
  (cond
    ((= group-id +group-x25519+)
     (tls-connection-client-x25519-public conn))
    ((= group-id +group-secp256r1+)
     (tls-connection-client-p256-public conn))
    ((= group-id +group-secp384r1+)
     (tls-connection-client-p384-public conn))
    ((= group-id +group-secp521r1+)
     (tls-connection-client-p521-public conn))
    ((= group-id +group-x25519-mlkem768+)
     (tls-connection-client-hybrid-public conn))
    (t nil)))

(defun %make-synth-message-hash (hash-fn hash-len ch1-bytes)
  "Synthesise the message_hash handshake message that replaces the
   original ClientHello in the transcript after HelloRetryRequest, per
   RFC 8446 §4.4.1. Wire form:

     message_hash(254) || uint24 length=hash-len || Hash(ClientHello1)

   where ClientHello1 is the original ClientHello *with* its handshake
   header (because the transcript was updated with the wrapped form)."
  (let* ((ch1-hash (ecase hash-fn
                     (:sha256 (sha256:sha256 ch1-bytes))
                     (:sha384 (sha512:sha384 ch1-bytes))))
         (out (make-array (+ 4 hash-len) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) 254)                ; message_hash
    (setf (aref out 1) 0)
    (setf (aref out 2) 0)
    (setf (aref out 3) hash-len)
    (replace out ch1-hash :start1 4)
    out))

(defun tls-process-hello-retry-request (conn payload)
  "Handle a HelloRetryRequest. Generates a retry ClientHello and returns
   the wire bytes (a single record) to send back, leaving the connection
   in :wait-server-hello so the next ServerHello completes the handshake.

   Per RFC 8446 §4.1.4 / §4.4.1 we:
     - reject a second HRR (illegal)
     - update the cipher suite from the HRR if it changed
     - rebuild the transcript: message_hash(CH1) || HRR || CH2
     - reuse the original client_random + legacy_session_id in CH2
     - emit a key_share for exactly the selected group
     - echo the cookie extension if present"
  (when (tls-connection-hrr-already-seen conn)
    (error 'tls-alert-error
           :level +alert-level-fatal+
           :description +alert-unexpected-message+
           :state :wait-server-hello))
  (setf (tls-connection-hrr-already-seen conn) t)
  (let* ((sh (parse-server-hello payload))
         (cipher-suite (parsed-server-hello-cipher-suite sh))
         (selected-group (hrr-selected-group sh))
         (cookie (hrr-cookie sh)))
    (unless selected-group
      ;; HRR with neither a key_share nor (theoretically) just a cookie:
      ;; we refuse rather than guess. The server MUST send a key_share
      ;; in HRR if the original ClientHello was incompatible.
      (error 'tls-alert-error
             :level +alert-level-fatal+
             :description +alert-handshake-failure+
             :state :wait-server-hello))
    ;; Verify the named group is one we actually advertised in
    ;; supported_groups -- otherwise the server is asking us to use a
    ;; group we never offered (RFC 8446 §4.1.4 rejection rule).
    (unless (%group-id->keyword selected-group)
      (error 'tls-alert-error
             :level +alert-level-fatal+
             :description +alert-illegal-parameter+
             :state :wait-server-hello))
    ;; Switch cipher suite if HRR picked a different one. Per RFC the
    ;; cipher_suite chosen in HRR is the final cipher suite; the
    ;; subsequent ServerHello must echo it.
    (let ((ks (tls-connection-key-schedule conn)))
      (unless (= cipher-suite (tls13-key-schedule-cipher-suite ks))
        (setf ks (make-tls13-key-schedule :cipher-suite cipher-suite))
        (setf (tls-connection-key-schedule conn) ks))
      ;; Rebuild transcript: message_hash(CH1) || HRR || CH2.
      (let* ((ch1 (tls-connection-client-hello-msg conn))
             (hash-fn (tls13-key-schedule-hash-fn ks))
             (hash-len (tls13-key-schedule-hash-len ks))
             (synth (%make-synth-message-hash hash-fn hash-len ch1))
             (group-kw (%group-id->keyword selected-group))
             (group-pub (%connection-public-for-group conn selected-group))
             (ch2 (build-client-hello
                   :hostname (tls-connection-hostname conn)
                   :key-shares (list (list group-kw group-pub))
                   :alpn (tls-connection-alpn-protocols conn)
                   :random (tls-connection-client-random conn)
                   :session-id (tls-connection-client-session-id conn)
                   :cookie cookie)))
        ;; Reset transcript hash state so we can rebuild from scratch.
        (setf (tls13-key-schedule-transcript-hash ks)
              (make-transcript-hash hash-fn))
        (setf (tls13-key-schedule-transcript-hash-cache ks) nil)
        (transcript-update ks synth)
        ;; HRR is conveyed wire-format as a ServerHello; transcript
        ;; absorbs it with the handshake header, so wrap before update.
        (transcript-update ks
                           (make-handshake-message +handshake-server-hello+
                                                   payload))
        (transcript-update ks ch2)
        (setf (tls-connection-client-hello-msg conn) ch2)
        ;; Stay in :wait-server-hello so the next record (the real SH)
        ;; routes through tls-process-server-hello.
        (serialize-tls-record
         (make-tls-record :content-type +content-handshake+
                          :data ch2))))))

;;; ---------------------------------------------------------------------------
;;; KeyUpdate (RFC 8446 Section 4.6.3)
;;; ---------------------------------------------------------------------------

(defun build-key-update (conn &key (request-update t))
  "Build a KeyUpdate handshake message and update sending keys.
   REQUEST-UPDATE: if T, requests the peer to also update.
   Returns the encrypted record bytes to send."
  (unless (eq (tls-connection-state conn) :connected)
    (error "Cannot send KeyUpdate in state ~A" (tls-connection-state conn)))
  (let* ((payload (make-array 1 :element-type '(unsigned-byte 8)))
         (_ (setf (aref payload 0) (if request-update 1 0)))
         (msg (make-handshake-message +handshake-key-update+ payload))
         ;; Encrypt with current keys
         (ciphertext (encrypt-record msg +content-handshake+
                                     (tls-connection-client-app-key conn)
                                     (tls-connection-client-app-iv conn)
                                     (tls-connection-client-seq conn)
                                     (tls-connection-cipher-suite conn))))
    (declare (ignore _))
    (incf (tls-connection-client-seq conn))
    ;; Update sending keys
    (let* ((ks (tls-connection-key-schedule conn))
           (new-secret (ks-hkdf-expand-label ks
                         (tls-connection-client-hs-secret conn)
                         "traffic upd" +empty-context+
                         (tls13-key-schedule-hash-len ks)))
           (key-len (cipher-suite-key-len (tls-connection-cipher-suite conn)))
           (new-key (ks-hkdf-expand-label ks new-secret "key" +empty-context+ key-len))
           (new-iv (ks-hkdf-expand-label ks new-secret "iv" +empty-context+ 12)))
      (setf (tls-connection-client-app-key conn) new-key)
      (setf (tls-connection-client-app-iv conn) new-iv)
      (setf (tls-connection-client-seq conn) 0))
    ;; Return the record
    (serialize-tls-record
     (make-tls-record :content-type +content-application-data+
                      :data ciphertext))))

(defun process-key-update (conn payload)
  "Process an incoming KeyUpdate message and update receiving keys."
  (let* ((request-update (and (>= (length payload) 1) (= (aref payload 0) 1)))
         (ks (tls-connection-key-schedule conn))
         ;; Update receiving keys
         (new-secret (ks-hkdf-expand-label ks
                       (tls-connection-server-hs-secret conn)
                       "traffic upd" +empty-context+
                       (tls13-key-schedule-hash-len ks)))
         (key-len (cipher-suite-key-len (tls-connection-cipher-suite conn)))
         (new-key (ks-hkdf-expand-label ks new-secret "key" +empty-context+ key-len))
         (new-iv (ks-hkdf-expand-label ks new-secret "iv" +empty-context+ 12)))
    (setf (tls-connection-server-app-key conn) new-key)
    (setf (tls-connection-server-app-iv conn) new-iv)
    (setf (tls-connection-server-seq conn) 0)
    ;; If requested, send our own KeyUpdate
    (when request-update
      (build-key-update conn :request-update nil))))

;;; ---------------------------------------------------------------------------
;;; Post-handshake Message Processing
;;; ---------------------------------------------------------------------------

(defun tls-process-post-handshake (conn plaintext)
  "Process post-handshake messages (NewSessionTicket, KeyUpdate)."
  (let ((pos 0))
    (loop while (< pos (length plaintext))
          do (multiple-value-bind (type payload next-pos)
                 (parse-handshake-header plaintext pos)
               (unless type (return))
               (cond
                 ((= type +handshake-new-session-ticket+)
                  (let ((ticket (parse-new-session-ticket payload)))
                    ;; Derive resumption secret and store it in the ticket
                    (let* ((ks (tls-connection-key-schedule conn))
                           (res-secret (ks-derive-secret ks
                                         (tls13-key-schedule-master-secret ks)
                                         "res master")))
                      (setf (tls-session-ticket-resumption-secret ticket) res-secret)
                      (setf (tls-session-ticket-cipher-suite ticket)
                            (tls-connection-cipher-suite conn)))
                    (push ticket (tls-connection-received-tickets conn))
                    ;; Optional caller-supplied hook (e.g. session ticket
                    ;; cache) gets fed every issued ticket. Errors are
                    ;; swallowed so a misbehaving callback cannot break
                    ;; the connection, but logged at warn so they leave
                    ;; a breadcrumb for operators.
                    (let ((cb (tls-connection-new-session-ticket-callback conn)))
                      (when cb
                        (handler-case (funcall cb conn ticket)
                          (error (e)
                            (epsilon.log:warn
                             "new-session-ticket-callback for ~A signaled: ~A"
                             (tls-connection-hostname conn) e)))))))
                 ((= type +handshake-key-update+)
                  (let ((response (process-key-update conn payload)))
                    ;; If response is non-nil, caller should send it
                    (when response
                      ;; Store in send buffer for caller to retrieve
                      (setf (tls-connection-send-buffer conn) response))))
                 (t
                  (error "Unknown post-handshake message type ~D" type)))
               (setf pos next-pos)))))

;;; ---------------------------------------------------------------------------
;;; Server-Side Handshake (RFC 8446 Section 4)
;;; ---------------------------------------------------------------------------

(defstruct tls-server-config
  (certificate-chain nil :type list)   ; list of DER-encoded certificate bytes
  (private-key nil)                     ; private key (bytes or integer)
  (key-type :ed25519 :type symbol)     ; :ed25519, :ecdsa-p256, :rsa
  (alpn-protocols nil :type list)       ; supported ALPN protocols
  (cipher-suites (list +tls-chacha20-poly1305-sha256+
                       +tls-aes-128-gcm-sha256+
                       +tls-aes-256-gcm-sha384+)
                 :type list)
  ;; mTLS verify mode -- bitmask matching the OpenSSL flags exposed
  ;; from epsilon.crypto. 0 (default) means do not request a client
  ;; cert; +verify-peer+ alone means request but accept handshakes
  ;; where the client offers nothing or an unverifiable chain (the
  ;; peer cert reaches the application via X-Client-Cert-* headers
  ;; for fingerprint-based authorisation); +verify-peer+ |
  ;; +verify-fail-if-no-peer-cert+ means require -- the handshake
  ;; aborts with `certificate_required(116)' when the client returns
  ;; an empty Certificate.
  (verify-mode 0 :type integer)
  ;; If non-NIL, the server will issue a NewSessionTicket after the
  ;; handshake completes, sealed with this STEK store. NIL disables
  ;; ticket issuance entirely.
  (session-ticket-store nil)
  ;; Lifetime in seconds advertised in NewSessionTicket. RFC 8446 caps
  ;; this at 7 days; 24h is the customary default.
  (session-ticket-lifetime 86400 :type integer)
  ;; Cached OCSP response (DER-encoded BasicOCSPResponse, opaque to
  ;; this layer). When non-NIL and the client offers status_request,
  ;; the server attaches it to the leaf CertificateEntry's extensions
  ;; per RFC 8446 4.4.2.1. Population of this slot -- a background
  ;; OCSP fetcher that refreshes against the cert's AIA responder --
  ;; lives outside this module; we just hold the bytes.
  (ocsp-staple nil))

(defstruct parsed-client-hello
  (random nil)
  (session-id nil)
  (cipher-suites nil :type list)
  (key-shares nil :type list)           ; list of (group . key-data)
  (hostname nil)
  (alpn-protocols nil :type list)
  (supported-versions nil :type list)
  (signature-algorithms nil :type list)
  (psk-identities nil :type list)
  (psk-binders nil :type list)
  ;; T when the client offered RFC 6066 status_request (any contents).
  ;; The server uses this to decide whether to staple an OCSP response.
  (status-request-p nil)
  ;; Index (within the ClientHello payload) of the first byte of the
  ;; pre_shared_key extension's binder list length prefix. The bytes
  ;; payload[0..binder-prefix-end] together with the synthetic
  ;; handshake header form ClientHelloPrefix as defined in
  ;; RFC 8446 Section 4.2.11.2 -- the input to the PSK binder HMAC.
  (binder-prefix-end nil :type (or null fixnum))
  (early-data-p nil)
  (quic-transport-params nil))

(define-condition tls-decode-error (error)
  ((reason :initarg :reason :reader tls-decode-error-reason))
  (:report (lambda (c s)
             (format s "TLS decode_error: ~A" (tls-decode-error-reason c))))
  (:documentation
   "Signaled when a parser reads past the end of a handshake payload or
otherwise detects that the wire format does not match what RFC 8446
mandates. Corresponds to the decode_error(50) alert a TLS server must
send on such input. Surfaces as a proper condition type instead of
leaking raw SBCL aref/subseq errors, so servers can translate directly
to an alert instead of crashing the handshake thread."))

(defun %parse-client-hello (payload)
  "Parser body with bounds-checked readers."
  (let ((pos 0)
        (result (make-parsed-client-hello)))
    ;; client_version (2 bytes, legacy)
    (tls13-need payload pos 2 "legacy_version")
    (incf pos 2)
    ;; random (32 bytes)
    (multiple-value-bind (r p) (read-bytes payload pos 32 "random")
      (setf (parsed-client-hello-random result) r pos p))
    ;; session_id
    (multiple-value-bind (sid-len new-pos) (read-u8 payload pos "session_id length")
      (setf pos new-pos)
      (multiple-value-bind (sid p) (read-bytes payload pos sid-len "session_id")
        (setf (parsed-client-hello-session-id result) sid pos p)))
    ;; cipher_suites
    (multiple-value-bind (suites-len new-pos) (read-u16 payload pos "cipher_suites length")
      (setf pos new-pos)
      (tls13-need payload pos suites-len "cipher_suites")
      (let ((end (+ pos suites-len)))
        (loop while (< pos end)
              do (multiple-value-bind (suite new-pos2) (read-u16 payload pos "cipher_suite")
                   (setf pos new-pos2)
                   (push suite (parsed-client-hello-cipher-suites result))))
        (setf (parsed-client-hello-cipher-suites result)
              (nreverse (parsed-client-hello-cipher-suites result)))))
    ;; compression_methods (skip)
    (multiple-value-bind (cm-len new-pos) (read-u8 payload pos "compression_methods length")
      (setf pos new-pos)
      (tls13-need payload pos cm-len "compression_methods")
      (incf pos cm-len))
    ;; Extensions
    (when (< pos (length payload))
      (multiple-value-bind (ext-len new-pos) (read-u16 payload pos "extensions length")
        (setf pos new-pos)
        (tls13-need payload pos ext-len "extensions")
        (let ((ext-end (+ pos ext-len)))
          (loop while (< pos ext-end)
                do (multiple-value-bind (etype new-pos2) (read-u16 payload pos "extension type")
                     (setf pos new-pos2)
                     (multiple-value-bind (elen new-pos3) (read-u16 payload pos "extension length")
                       (setf pos new-pos3)
                       (cond
                         ;; pre_shared_key needs absolute offsets within
                         ;; the payload so the server can later compute
                         ;; ClientHelloPrefix for binder validation. Parse
                         ;; it inline rather than via the dispatcher.
                         ((= etype +ext-pre-shared-key+)
                          (tls13-need payload pos elen "psk extension")
                          (%parse-pre-shared-key-ext result payload pos elen))
                         (t
                          (multiple-value-bind (ext-data p)
                              (read-bytes payload pos elen "extension data")
                            (declare (ignore p))
                            (parse-client-hello-extension result etype ext-data))))
                       (incf pos elen)))))))
    result))

(defun parse-client-hello (payload)
  "Parse a ClientHello handshake message payload.

PAYLOAD is the bytes *inside* the handshake header (i.e. what follows
the (type, length) header). Returns a PARSED-CLIENT-HELLO struct on
success. Signals TLS-DECODE-ERROR on any malformed input, including
any length field that points past the end of PAYLOAD -- matching the
behavior RFC 8446 section 6.2 requires (the server must respond with
decode_error(50))."
  (%parse-client-hello payload))

(defun %parse-pre-shared-key-ext (ch payload ext-start ext-len)
  "Parse a pre_shared_key extension in place using absolute offsets
   within the ClientHello payload, so binder-prefix-end can be recorded
   for later binder validation."
  (declare (ignore ext-len))
  (let ((pos ext-start))
    ;; identities <7..2^16-1>
    (multiple-value-bind (id-len new-pos) (read-u16 payload pos "psk identities_length")
      (setf pos new-pos)
      (tls13-need payload pos id-len "psk identities")
      (let ((id-end (+ pos id-len)))
        (loop while (< pos id-end)
              do (multiple-value-bind (tlen new-pos2) (read-u16 payload pos "psk ticket_length")
                   (setf pos new-pos2)
                   (multiple-value-bind (ticket p) (read-bytes payload pos tlen "psk ticket")
                     (setf pos p)
                     (multiple-value-bind (age new-pos3) (read-u32 payload pos "psk obfuscated_age")
                       (setf pos new-pos3)
                       (push (cons ticket age)
                             (parsed-client-hello-psk-identities ch))))))
        (setf (parsed-client-hello-psk-identities ch)
              (nreverse (parsed-client-hello-psk-identities ch)))
        (setf (parsed-client-hello-binder-prefix-end ch) pos)))
    ;; binders <33..2^16-1>
    (multiple-value-bind (bind-len new-pos) (read-u16 payload pos "psk binders_length")
      (setf pos new-pos)
      (tls13-need payload pos bind-len "psk binders")
      (let ((bind-end (+ pos bind-len)))
        (loop while (< pos bind-end)
              do (multiple-value-bind (blen new-pos2) (read-u8 payload pos "psk binder_length")
                   (setf pos new-pos2)
                   (multiple-value-bind (binder p) (read-bytes payload pos blen "psk binder")
                     (push binder (parsed-client-hello-psk-binders ch))
                     (setf pos p))))
        (setf (parsed-client-hello-psk-binders ch)
              (nreverse (parsed-client-hello-psk-binders ch)))))))

(defun parse-client-hello-extension (ch ext-type data)
  "Parse a single ClientHello extension into the parsed-client-hello struct."
  (let ((dlen (length data)))
    (cond
      ;; SNI
      ((= ext-type +ext-server-name+)
       (when (>= dlen 2)
         (let ((pos 0))
           (multiple-value-bind (list-len new-pos) (read-u16 data pos "sni list_length")
             (declare (ignore list-len))
             (setf pos new-pos)
             (when (< pos dlen)
               (multiple-value-bind (name-type new-pos2) (read-u8 data pos "sni name_type")
                 (setf pos new-pos2)
                 (when (zerop name-type) ; host_name
                   (multiple-value-bind (name-len new-pos3) (read-u16 data pos "sni name_length")
                     (setf pos new-pos3)
                     (multiple-value-bind (name-bytes p)
                         (read-bytes data pos name-len "sni hostname")
                       (declare (ignore p))
                       (setf (parsed-client-hello-hostname ch)
                             (map 'string #'code-char name-bytes)))))))))))
      ;; supported_versions
      ((= ext-type +ext-supported-versions+)
       (when (>= dlen 1)
         (let ((pos 0))
           (multiple-value-bind (list-len new-pos) (read-u8 data pos "sv list_length")
             (declare (ignore list-len))
             (setf pos new-pos)
             (loop while (<= (+ pos 2) dlen)
                   do (multiple-value-bind (ver new-pos2) (read-u16 data pos "sv version")
                        (setf pos new-pos2)
                        (push ver (parsed-client-hello-supported-versions ch))))
             (setf (parsed-client-hello-supported-versions ch)
                   (nreverse (parsed-client-hello-supported-versions ch)))))))
      ;; key_share
      ((= ext-type +ext-key-share+)
       (when (>= dlen 2)
         (let ((pos 0))
           (multiple-value-bind (list-len new-pos) (read-u16 data pos "ks list_length")
             (declare (ignore list-len))
             (setf pos new-pos)
             (loop while (< pos dlen)
                   do (multiple-value-bind (group new-pos2) (read-u16 data pos "ks group")
                        (setf pos new-pos2)
                        (multiple-value-bind (klen new-pos3) (read-u16 data pos "ks key_length")
                          (setf pos new-pos3)
                          (multiple-value-bind (kdata p) (read-bytes data pos klen "ks key_data")
                            (push (cons group kdata)
                                  (parsed-client-hello-key-shares ch))
                            (setf pos p)))))
             (setf (parsed-client-hello-key-shares ch)
                   (nreverse (parsed-client-hello-key-shares ch)))))))
      ;; signature_algorithms
      ((= ext-type +ext-signature-algorithms+)
       (when (>= dlen 2)
         (let ((pos 0))
           (multiple-value-bind (list-len new-pos) (read-u16 data pos "sa list_length")
             (declare (ignore list-len))
             (setf pos new-pos)
             (loop while (<= (+ pos 2) dlen)
                   do (multiple-value-bind (sig new-pos2) (read-u16 data pos "sa algorithm")
                        (setf pos new-pos2)
                        (push sig (parsed-client-hello-signature-algorithms ch))))
             (setf (parsed-client-hello-signature-algorithms ch)
                   (nreverse (parsed-client-hello-signature-algorithms ch)))))))
      ;; ALPN
      ((= ext-type +ext-alpn+)
       (when (>= dlen 2)
         (let ((pos 0))
           (multiple-value-bind (list-len new-pos) (read-u16 data pos "alpn list_length")
             (declare (ignore list-len))
             (setf pos new-pos)
             (loop while (< pos dlen)
                   do (multiple-value-bind (plen new-pos2) (read-u8 data pos "alpn proto_length")
                        (setf pos new-pos2)
                        (multiple-value-bind (proto p) (read-bytes data pos plen "alpn protocol")
                          (push (map 'string #'code-char proto)
                                (parsed-client-hello-alpn-protocols ch))
                          (setf pos p))))
             (setf (parsed-client-hello-alpn-protocols ch)
                   (nreverse (parsed-client-hello-alpn-protocols ch)))))))
      ;; early_data
      ((= ext-type 42)
       (setf (parsed-client-hello-early-data-p ch) t))
      ;; QUIC transport parameters
      ((= ext-type +ext-quic-transport-parameters+)
       (setf (parsed-client-hello-quic-transport-params ch) data))
      ;; status_request (RFC 6066): the client is willing to receive
      ;; an OCSP staple. We do not need the contents of the extension
      ;; (responder_id_list, request_extensions) -- only the presence.
      ((= ext-type +ext-status-request+)
       (setf (parsed-client-hello-status-request-p ch) t))
      ;; pre_shared_key is parsed inline by parse-client-hello so the
      ;; binder-prefix offset can be recorded for binder validation.
      ;; Other extensions - ignore
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Server-side PSK resumption (RFC 8446 Section 4.2.11)
;;; ---------------------------------------------------------------------------

(defun %parse-server-ticket-payload (bytes)
  "Reverse of build-server-session-ticket-payload.
   Returns (values cipher-suite creation-time resumption-secret) or NIL
   on any structural failure."
  (when (and bytes (>= (length bytes) 11))
    (let* ((cs (logior (ash (aref bytes 0) 8) (aref bytes 1)))
           (ct (let ((v 0))
                 (dotimes (i 8 v) (setf v (logior (ash v 8) (aref bytes (+ 2 i)))))))
           (rs-len (aref bytes 10)))
      (when (= (length bytes) (+ 11 rs-len))
        (values cs ct (subseq bytes 11 (+ 11 rs-len)))))))

(defun %ct-equal-bytes (a b)
  "Constant-time byte vector equality."
  (and (= (length a) (length b))
       (let ((diff 0))
         (dotimes (i (length a) (zerop diff))
           (setf diff (logior diff (logxor (aref a i) (aref b i))))))))

(defun %build-binder-partial-msg (payload binder-prefix-end)
  "Construct ClientHelloPrefix per RFC 8446 4.2.11.2: a synthetic
   handshake header carrying the FULL ClientHello length, followed by
   the payload bytes up to (not including) the binder list length."
  (let* ((total (length payload))
         (out (make-array (+ 4 binder-prefix-end) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) +handshake-client-hello+)
    (setf (aref out 1) (logand (ash total -16) #xFF))
    (setf (aref out 2) (logand (ash total -8) #xFF))
    (setf (aref out 3) (logand total #xFF))
    (replace out payload :start1 4 :end2 binder-prefix-end)
    out))

(defstruct psk-resumption
  "Result of a successful PSK resumption attempt."
  (identity-index 0 :type fixnum)
  (psk nil)
  (cipher-suite 0 :type fixnum)
  (resumption-secret nil))

(defun %try-pick-psk-resumption (ch payload server-config server-suite)
  "Attempt PSK resumption against the configured STEK store.
   Returns a psk-resumption struct on success, NIL when no usable
   identity is offered (caller should fall back to a full handshake),
   or signals an error to abort the handshake when an identity is
   selected but its binder fails to validate (RFC 8446 4.2.11.2:
   server MUST send decrypt_error)."
  (let ((store (and server-config
                    (tls-server-config-session-ticket-store server-config))))
    (unless (and store
                 (parsed-client-hello-psk-identities ch)
                 (parsed-client-hello-binder-prefix-end ch))
      (return-from %try-pick-psk-resumption nil))
    (let* ((lifetime (tls-server-config-session-ticket-lifetime server-config))
           (identities (parsed-client-hello-psk-identities ch))
           (binders (parsed-client-hello-psk-binders ch))
           (now (get-universal-time))
           (selected nil))
      ;; Walk identities, picking the first one we can decrypt and that
      ;; matches the negotiated suite + ticket lifetime. Per RFC the
      ;; binder validation step that follows is not allowed to fall
      ;; back to a different identity.
      (loop for i from 0
            for entry in identities
            for ticket-bytes = (car entry)
            do (let ((opened (stek:stek-open store ticket-bytes)))
                 (when opened
                   (multiple-value-bind (cs ct rs)
                       (%parse-server-ticket-payload opened)
                     (when (and cs (= cs server-suite)
                                (<= (- now ct) lifetime))
                       (setf selected (list i cs ct rs))
                       (return))))))
      (unless selected
        (return-from %try-pick-psk-resumption nil))
      (destructuring-bind (idx cs ct rs) selected
        (declare (ignore ct))
        ;; Derive the actual PSK from the resumption_master_secret in
        ;; the ticket. The ticket nonce we issued is a single zero byte
        ;; (see build-new-session-ticket-message), so we use that here.
        (let* ((temp-ks (make-tls13-key-schedule :cipher-suite cs))
               (nonce (make-array 1 :element-type '(unsigned-byte 8)
                                  :initial-element 0))
               (psk (ks-hkdf-expand-label temp-ks rs "resumption" nonce
                                          (tls13-key-schedule-hash-len temp-ks)))
               (partial (%build-binder-partial-msg
                         payload
                         (parsed-client-hello-binder-prefix-end ch)))
               (expected (compute-psk-binder temp-ks psk partial))
               (offered  (nth idx binders)))
          (unless (and offered (%ct-equal-bytes expected offered))
            (error "TLS: PSK binder validation failed (decrypt_error)"))
          (make-psk-resumption :identity-index idx
                               :psk psk
                               :cipher-suite cs
                               :resumption-secret rs))))))

(defun tls-server-start-handshake (conn client-hello-data)
  "Process a ClientHello and generate the server's response messages.
   CONN must have role :server and a server-config set.
   Returns the concatenated record bytes to send (ServerHello + encrypted handshake)."
  (let* ((config (tls-connection-server-config conn))
         (ch (parse-client-hello client-hello-data))
         ;; Negotiate cipher suite
         (suite (negotiate-cipher-suite (parsed-client-hello-cipher-suites ch)
                                        (tls-server-config-cipher-suites config)))
         ;; Select key share group. Prefer the hybrid
         ;; X25519+ML-KEM-768 share when the client offers one, then
         ;; fall back to classical X25519.
         (client-key-shares (parsed-client-hello-key-shares ch))
         (hybrid-share (find +group-x25519-mlkem768+ client-key-shares :key #'car))
         (x25519-share (find +group-x25519+ client-key-shares :key #'car))
         (p256-share   (find +group-secp256r1+ client-key-shares :key #'car))
         (p384-share   (find +group-secp384r1+ client-key-shares :key #'car))
         (p521-share   (find +group-secp521r1+ client-key-shares :key #'car))
         ;; Initialize key schedule
         (ks (make-tls13-key-schedule :cipher-suite suite)))
    (unless suite
      (error "No common cipher suite"))
    (unless (or hybrid-share x25519-share p256-share p384-share p521-share)
      (error "No supported key share group"))
    (setf (tls-connection-key-schedule conn) ks)
    (setf (tls-connection-cipher-suite conn) suite)
    ;; Attempt PSK resumption against the configured ticket store. If
    ;; an identity is selected and its binder validates, the rest of
    ;; the flight skips Certificate / CertificateVerify and feeds the
    ;; resumption PSK into the early-secret derivation. If no usable
    ;; identity is offered, RESUMPTION is NIL and we fall through to a
    ;; full handshake. Binder validation failures abort the handshake.
    (let ((resumption (%try-pick-psk-resumption ch client-hello-data
                                                config suite)))
      (when resumption
        (setf (tls-connection-resumed-p conn) t))
      ;; Add ClientHello to transcript
      (let ((ch-msg (make-handshake-message +handshake-client-hello+ client-hello-data)))
        (transcript-update ks ch-msg))
    ;; Perform key exchange against whichever group we selected.
    ;; For the hybrid group the server calls hybrid-encaps against the
    ;; client's 1216-byte public key, and the "server-pk" placed in the
    ;; ServerHello key_share is actually the 1120-byte hybrid
    ;; ciphertext. For classical X25519 it's an ephemeral public key.
    (multiple-value-bind (server-pk shared-secret selected-group)
        (cond
          (hybrid-share
           (multiple-value-bind (ss ct)
               (hybrid:hybrid-encaps (cdr hybrid-share))
             (values ct ss +group-x25519-mlkem768+)))
          (x25519-share
           (let* ((server-sk (drbg:random-bytes 32))
                  (server-pk (x25519:x25519-base server-sk))
                  (client-pk (cdr x25519-share))
                  (ss (x25519:x25519 server-sk client-pk)))
             (values server-pk ss +group-x25519+)))
          (p256-share
           ;; secp256r1: ephemeral ECDH on P-256.
           (multiple-value-bind (server-sk server-pk-pt)
               (ecdh:ecdh-p256-generate-keypair)
             (let* ((client-pk-bytes (cdr p256-share))
                    (client-pk-pt (ec-p256:p256-point-decode client-pk-bytes))
                    (ss (ecdh:ecdh-p256-shared-secret server-sk client-pk-pt))
                    (server-pk-bytes
                      (ec-p256:p256-point-encode-uncompressed server-pk-pt)))
               (values server-pk-bytes ss +group-secp256r1+))))
          (p384-share
           ;; secp384r1: ephemeral ECDH on P-384. Mirror of the P-256
           ;; arm; emits a 97-byte uncompressed SEC1 server pubkey and a
           ;; 48-byte X-coordinate shared secret. Required to handshake
           ;; with the FIPS-leaning servers IMPL-380 targets.
           (multiple-value-bind (server-sk server-pk-pt)
               (ecdh:ecdh-p384-generate-keypair)
             (let* ((client-pk-bytes (cdr p384-share))
                    (client-pk-pt (ec-p384:p384-point-decode client-pk-bytes))
                    (ss (ecdh:ecdh-p384-shared-secret server-sk client-pk-pt))
                    (server-pk-bytes
                      (ec-p384:p384-point-encode-uncompressed server-pk-pt)))
               (values server-pk-bytes ss +group-secp384r1+))))
          (t
           ;; secp521r1: ephemeral ECDH on P-521 (133-byte server pubkey,
           ;; 66-byte shared secret).
           (multiple-value-bind (server-sk server-pk-pt)
               (ecdh:ecdh-p521-generate-keypair)
             (let* ((client-pk-bytes (cdr p521-share))
                    (client-pk-pt (ec-p521:p521-point-decode client-pk-bytes))
                    (ss (ecdh:ecdh-p521-shared-secret server-sk client-pk-pt))
                    (server-pk-bytes
                      (ec-p521:p521-point-encode-uncompressed server-pk-pt)))
               (values server-pk-bytes ss +group-secp521r1+)))))
      ;; Derive secrets. On the resumption path the early secret is
      ;; extracted from the resumption PSK; otherwise from all-zeros.
      (derive-early-secret ks (and resumption (psk-resumption-psk resumption)))
      (derive-handshake-secret ks shared-secret)
      ;; Build ServerHello. On resumption we echo the selected PSK
      ;; identity index in a pre_shared_key extension.
      (let* ((server-random (drbg:random-bytes 32))
             (session-id (parsed-client-hello-session-id ch))
             (sh-payload (build-server-hello-payload
                          server-random session-id suite server-pk
                          :group selected-group
                          :psk-identity-index
                          (and resumption (psk-resumption-identity-index resumption))))
             (sh-msg (make-handshake-message +handshake-server-hello+ sh-payload)))
        (transcript-update ks sh-msg)
        ;; Derive handshake traffic keys
        (multiple-value-bind (c-key c-iv s-key s-iv) (derive-handshake-traffic-keys ks)
          (setf (tls-connection-client-handshake-key conn) c-key)
          (setf (tls-connection-client-handshake-iv conn) c-iv)
          (setf (tls-connection-server-handshake-key conn) s-key)
          (setf (tls-connection-server-handshake-iv conn) s-iv))
        ;; Save handshake secrets for Finished
        (let ((hs-secret (tls13-key-schedule-handshake-secret ks)))
          (setf (tls-connection-server-hs-secret conn)
                (ks-derive-secret ks hs-secret "s hs traffic"))
          (setf (tls-connection-client-hs-secret conn)
                (ks-derive-secret ks hs-secret "c hs traffic")))
        ;; Negotiate ALPN
        (let ((client-alpn (parsed-client-hello-alpn-protocols ch))
              (server-alpn (tls-server-config-alpn-protocols config)))
          (when (and client-alpn server-alpn)
            (setf (tls-connection-alpn-protocol conn)
                  (find-if (lambda (p) (member p client-alpn :test #'string=))
                           server-alpn))))
        ;; Build encrypted handshake flight
        (let ((encrypted-msgs (make-buffer)))
          ;; EncryptedExtensions
          (let ((ee-payload (build-encrypted-extensions-payload conn)))
            (let ((ee-msg (make-handshake-message +handshake-encrypted-extensions+ ee-payload)))
              (transcript-update ks ee-msg)
              (buf-append-bytes encrypted-msgs ee-msg)))
          ;; Certificate / CertificateVerify -- omitted on the
          ;; resumption path; the PSK already authenticates the server
          ;; (RFC 8446 4.6.1, 2.2).
          (unless resumption
            ;; CertificateRequest (mTLS, RFC 8446 §4.3.2): emitted
            ;; only on a full handshake when the server config asks
            ;; for a client cert. The flag is captured on the
            ;; connection so the receive side knows to expect a
            ;; client Certificate flight (vs. going straight to
            ;; Finished).
            (when (%verify-peer-p (tls-server-config-verify-mode config))
              (let ((cr-msg (build-certificate-request-message)))
                (transcript-update ks cr-msg)
                (buf-append-bytes encrypted-msgs cr-msg))
              (setf (tls-connection-cert-requested-p conn) t))
            (let ((cert-msg (build-certificate-message
                             (tls-server-config-certificate-chain config)
                             :ocsp-staple
                             (and (parsed-client-hello-status-request-p ch)
                                  (tls-server-config-ocsp-staple config)))))
              (transcript-update ks cert-msg)
              (buf-append-bytes encrypted-msgs cert-msg))
            (let ((cv-msg (build-certificate-verify conn config)))
              (transcript-update ks cv-msg)
              (buf-append-bytes encrypted-msgs cv-msg)))
          ;; Finished
          (let ((finished-msg (build-finished ks (tls-connection-server-hs-secret conn))))
            (transcript-update ks finished-msg)
            (buf-append-bytes encrypted-msgs finished-msg))
          ;; Encrypt the flight
          (let* ((plain-bytes (buf-freeze encrypted-msgs))
                 (ciphertext (encrypt-record plain-bytes +content-handshake+
                                            (tls-connection-server-handshake-key conn)
                                            (tls-connection-server-handshake-iv conn)
                                            (tls-connection-server-seq conn)
                                            suite)))
            (incf (tls-connection-server-seq conn))
            ;; Derive master secret and application keys
            (derive-master-secret ks)
            (multiple-value-bind (c-key c-iv s-key s-iv)
                (derive-application-traffic-keys ks)
              (setf (tls-connection-client-app-key conn) c-key)
              (setf (tls-connection-client-app-iv conn) c-iv)
              (setf (tls-connection-server-app-key conn) s-key)
              (setf (tls-connection-server-app-iv conn) s-iv))
            (setf (tls-connection-client-seq conn) 0)
            (setf (tls-connection-server-seq conn) 0)
            ;; If we asked for a client cert, expect the client to
            ;; send Certificate (+ optionally CertificateVerify) before
            ;; Finished. Otherwise jump straight to :wait-finished.
            (setf (tls-connection-state conn)
                  (if (tls-connection-cert-requested-p conn)
                      :wait-client-certificate
                      :wait-finished))
            ;; Return: CCS + ServerHello record + encrypted handshake record
            (let ((ccs-record (make-array 6 :element-type '(unsigned-byte 8)
                                          :initial-contents
                                          '(20 #x03 #x03 0 1 1)))
                  (sh-record (serialize-tls-record
                              (make-tls-record :content-type +content-handshake+
                                               :data sh-msg)))
                  (enc-record (serialize-tls-record
                               (make-tls-record :content-type +content-application-data+
                                                :data ciphertext))))
              (let ((result (make-array (+ (length ccs-record) (length sh-record) (length enc-record))
                                        :element-type '(unsigned-byte 8))))
                (replace result sh-record)
                (replace result ccs-record :start1 (length sh-record))
                (replace result enc-record :start1 (+ (length sh-record) (length ccs-record)))
                result)))))))))

(defun negotiate-cipher-suite (client-suites server-suites)
  "Select the best cipher suite supported by both client and server."
  (dolist (suite server-suites)
    (when (member suite client-suites)
      (return suite))))

(defun build-server-hello-payload (server-random session-id cipher-suite server-pk
                                   &key (group +group-x25519+)
                                        psk-identity-index)
  "Build the ServerHello payload (without handshake header).
   GROUP selects the key_share named group (default X25519). For the
   hybrid X25519+ML-KEM-768 group, SERVER-PK is the 1120-byte ciphertext
   produced by hybrid:hybrid-encaps rather than an ephemeral public key."
  (let ((buf (make-buffer)))
    ;; server_version (legacy)
    (buf-append-u16 buf #x0303)
    ;; random
    (buf-append-bytes buf server-random)
    ;; session_id_echo
    (buf-append-u8-prefixed buf session-id)
    ;; cipher_suite
    (buf-append-u16 buf cipher-suite)
    ;; compression_method (legacy, always 0)
    (buf-append-byte buf 0)
    ;; Extensions
    (let ((ext-buf (make-buffer)))
      ;; supported_versions
      (buf-append-u16 ext-buf +ext-supported-versions+)
      (buf-append-u16 ext-buf 2)
      (buf-append-u16 ext-buf +tls-1.3-supported-version+)
      ;; key_share
      (buf-append-u16 ext-buf +ext-key-share+)
      (let ((ks-buf (make-buffer)))
        (buf-append-u16 ks-buf group)
        (buf-append-u16-prefixed ks-buf server-pk)
        (buf-append-u16-prefixed ext-buf
                                 (buf-freeze ks-buf)))
      ;; pre_shared_key (RFC 8446 4.2.11) -- only on resumption.
      (when psk-identity-index
        (buf-append-u16 ext-buf +ext-pre-shared-key+)
        (buf-append-u16 ext-buf 2)
        (buf-append-u16 ext-buf psk-identity-index))
      (buf-append-u16-prefixed buf (buf-freeze ext-buf)))
    (buf-freeze buf)))

(defun build-encrypted-extensions-payload (conn)
  "Build the EncryptedExtensions payload."
  (let ((buf (make-buffer)))
    (let ((ext-buf (make-buffer)))
      ;; ALPN
      (when (tls-connection-alpn-protocol conn)
        (let ((proto (tls-connection-alpn-protocol conn))
              (alpn-buf (make-buffer)))
          (let ((proto-bytes (map '(vector (unsigned-byte 8)) #'char-code proto)))
            (let ((list-buf (make-buffer)))
              (buf-append-u8-prefixed list-buf proto-bytes)
              (buf-append-u16-prefixed alpn-buf
                                       (buf-freeze list-buf))))
          (buf-append-u16 ext-buf +ext-alpn+)
          (buf-append-u16-prefixed ext-buf
                                   (buf-freeze alpn-buf))))
      (buf-append-u16-prefixed buf (buf-freeze ext-buf)))
    (buf-freeze buf)))

;;; ---------------------------------------------------------------------------
;;; CertificateRequest (RFC 8446 §4.3.2)
;;; ---------------------------------------------------------------------------

;; OpenSSL-compatible verify-mode bits, mirrored locally so callers
;; can flip them without depending on epsilon.crypto. The wire-level
;; effect: bit 1 set -> server emits CertificateRequest; bit 2 set
;; additionally aborts the handshake when the client returns an
;; empty Certificate.
(defconstant +tls13-verify-peer+ 1)
(defconstant +tls13-verify-fail-if-no-peer-cert+ 2)

;; Declaring INLINE here would warn because %verify-peer-p is called above
;; (in tls-server-handshake-step) before this point. Both predicates are
;; hot-path bit tests, so the call cost is already trivial.
(defun %verify-peer-p (mode)
  (logtest mode +tls13-verify-peer+))
(defun %verify-fail-if-missing-p (mode)
  (logtest mode +tls13-verify-fail-if-no-peer-cert+))

(defun build-certificate-request-payload (&key (sigalgs-grease nil))
  "Build the body of a CertificateRequest message (without handshake
   header). Server-initiated handshakes use an empty
   `certificate_request_context' (RFC 8446 §4.3.2). The advertised
   `signature_algorithms' set mirrors what `make-signature-algorithms-ext'
   advertises in the ClientHello -- ECDSA across P-256/384/521,
   Ed25519, and RSA-PSS variants -- which gives us the broadest
   client-cert interop. CertificateAuthorities is omitted; the
   application authorises off the cert fingerprint, not the chain."
  (let ((buf (make-buffer)))
    ;; certificate_request_context: empty
    (buf-append-byte buf 0)
    ;; extensions: signature_algorithms is mandatory (RFC 8446
    ;; §4.3.2). make-signature-algorithms-ext returns the full
    ;; type/length/value extension blob in one byte vector.
    (let ((ext-buf (make-buffer)))
      (buf-append-bytes ext-buf
                        (make-signature-algorithms-ext :grease sigalgs-grease))
      (buf-append-u16-prefixed buf (buf-freeze ext-buf)))
    (buf-freeze buf)))

(defun build-certificate-request-message ()
  "Build the full CertificateRequest handshake message (header + body)
   suitable for inclusion in the server's encrypted handshake flight."
  (make-handshake-message +handshake-certificate-request+
                          (build-certificate-request-payload)))

(defun %build-status-request-cert-extension (ocsp-response)
  "Build the CertificateEntry extension carrying an OCSP staple per
   RFC 8446 4.4.2.1, which embeds an RFC 6066 CertificateStatus:
     u8  status_type = 1 (ocsp)
     u24 ocsp_response_length
     opaque ocsp_response<1..2^24-1>
   Returned bytes are the *extension data*, not yet wrapped in the
   u16 extension type / u16 length envelope."
  (let* ((rlen (length ocsp-response))
         (out (make-array (+ 1 3 rlen) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) 1) ; ocsp
    (setf (aref out 1) (logand (ash rlen -16) #xFF))
    (setf (aref out 2) (logand (ash rlen -8) #xFF))
    (setf (aref out 3) (logand rlen #xFF))
    (replace out ocsp-response :start1 4)
    out))

(defun build-certificate-message (cert-chain &key ocsp-staple)
  "Build a Certificate handshake message from a list of DER-encoded
   certificates. When OCSP-STAPLE is non-NIL it is attached to the
   leaf certificate's extensions as a status_request entry; subsequent
   chain entries get an empty extensions list."
  (let ((buf (make-buffer)))
    ;; certificate_request_context (empty for server)
    (buf-append-byte buf 0)
    ;; certificate_list
    (let ((list-buf (make-buffer))
          (first-p t))
      (dolist (cert-der cert-chain)
        ;; cert_data
        (buf-append-u24 list-buf (length cert-der))
        (buf-append-bytes list-buf cert-der)
        ;; extensions
        (cond
          ((and first-p ocsp-staple)
           (let* ((ext-data (%build-status-request-cert-extension ocsp-staple))
                  (ext-buf (make-buffer)))
             (buf-append-u16 ext-buf +ext-status-request+)
             (buf-append-u16-prefixed ext-buf ext-data)
             (buf-append-u16-prefixed list-buf (buf-freeze ext-buf))))
          (t
           (buf-append-u16 list-buf 0)))
        (setf first-p nil))
      (let ((list-bytes (buf-freeze list-buf)))
        (buf-append-u24 buf (length list-bytes))
        (buf-append-bytes buf list-bytes)))
    (make-handshake-message +handshake-certificate+ (buf-freeze buf))))

(defun build-certificate-verify (conn config)
  "Build a CertificateVerify handshake message."
  (let* ((ks (tls-connection-key-schedule conn))
         (transcript-hash (transcript-current-hash ks))
         ;; Build the content to sign (RFC 8446 Section 4.4.3)
         (context "TLS 1.3, server CertificateVerify")
         (context-bytes (map '(vector (unsigned-byte 8)) #'char-code context))
         (content-len (+ 64 (length context-bytes) 1 (length transcript-hash)))
         (content (make-array content-len :element-type '(unsigned-byte 8)))
         (key-type (tls-server-config-key-type config))
         (private-key (tls-server-config-private-key config)))
    ;; 64 spaces
    (fill content #x20 :end 64)
    ;; Context string
    (replace content context-bytes :start1 64)
    ;; Separator
    (setf (aref content (+ 64 (length context-bytes))) 0)
    ;; Transcript hash
    (replace content transcript-hash :start1 (+ 64 (length context-bytes) 1))
    ;; Sign
    (let ((scheme 0)
          (signature nil))
      (ecase key-type
        (:ed25519
         (setf scheme +sig-ed25519+)
         (setf signature (ed-sign:ed25519-sign private-key content)))
        (:ecdsa-p256
         (setf scheme +sig-ecdsa-secp256r1-sha256+)
         (multiple-value-bind (r s) (ecdsa:ecdsa-sign private-key content)
           (setf signature (asn1:der-encode-sequence
                            (asn1:der-encode-integer r)
                            (asn1:der-encode-integer s)))))
        (:rsa
         (setf scheme +sig-rsa-pss-rsae-sha256+)
         (setf signature (rsa:rsa-pss-sign private-key content :hash :sha256))))
      ;; Build CertificateVerify message
      (let ((payload (make-array (+ 4 (length signature)) :element-type '(unsigned-byte 8))))
        (setf (aref payload 0) (logand (ash scheme -8) #xFF))
        (setf (aref payload 1) (logand scheme #xFF))
        (setf (aref payload 2) (logand (ash (length signature) -8) #xFF))
        (setf (aref payload 3) (logand (length signature) #xFF))
        (replace payload signature :start1 4)
        (make-handshake-message +handshake-certificate-verify+ payload)))))

(defun %encode-u64-be (n)
  (let ((out (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for i from 7 downto 0
          for v = n then (ash v -8)
          do (setf (aref out i) (logand v #xFF)))
    out))

(defun build-server-session-ticket-payload (cipher-suite resumption-secret)
  "Encode the server-side resumption state that will be sealed into the
   opaque ticket field. Format:
     u16 cipher-suite | u64 creation-time | u8 res-secret-len | bytes
   This is what the server reads back during resumption to recover the
   PSK and metadata."
  (let* ((rs-len (length resumption-secret))
         (out (make-array (+ 2 8 1 rs-len) :element-type '(unsigned-byte 8)))
         (now (get-universal-time)))
    (setf (aref out 0) (logand (ash cipher-suite -8) #xFF))
    (setf (aref out 1) (logand cipher-suite #xFF))
    (replace out (%encode-u64-be now) :start1 2)
    (setf (aref out 10) rs-len)
    (replace out resumption-secret :start1 11)
    out))

(defun build-new-session-ticket-message (lifetime sealed-ticket)
  "Build a NewSessionTicket handshake message (RFC 8446 §4.6.1) given a
   lifetime in seconds and the opaque sealed ticket bytes. Uses an empty
   ticket nonce and no extensions."
  (let ((buf (make-buffer)))
    ;; ticket_lifetime
    (buf-append-byte buf (logand (ash lifetime -24) #xFF))
    (buf-append-byte buf (logand (ash lifetime -16) #xFF))
    (buf-append-byte buf (logand (ash lifetime -8) #xFF))
    (buf-append-byte buf (logand lifetime #xFF))
    ;; ticket_age_add (random)
    (let ((age-add (drbg:random-bytes 4)))
      (buf-append-bytes buf age-add))
    ;; ticket_nonce <0..255> -- single zero byte
    (buf-append-byte buf 1)
    (buf-append-byte buf 0)
    ;; ticket <1..2^16-1>
    (buf-append-u16-prefixed buf sealed-ticket)
    ;; extensions <0..2^16-2> -- empty
    (buf-append-u16 buf 0)
    (make-handshake-message +handshake-new-session-ticket+ (buf-freeze buf))))

(defun %server-emit-session-ticket (conn)
  "If the server config has a STEK store, derive the resumption secret,
   seal a ticket payload, build a NewSessionTicket handshake message,
   encrypt it as application_data, and return the wire bytes. Otherwise
   return NIL."
  (let* ((config (tls-connection-server-config conn))
         (store (and config (tls-server-config-session-ticket-store config))))
    (unless store
      (return-from %server-emit-session-ticket nil))
    (let* ((ks (tls-connection-key-schedule conn))
           (master (tls13-key-schedule-master-secret ks))
           (res-secret (ks-derive-secret ks master "res master"))
           (cipher-suite (tls-connection-cipher-suite conn))
           (plaintext (build-server-session-ticket-payload cipher-suite res-secret))
           (sealed (stek:stek-seal store plaintext))
           (lifetime (tls-server-config-session-ticket-lifetime config))
           (nst-msg (build-new-session-ticket-message lifetime sealed))
           (ciphertext (encrypt-record nst-msg +content-handshake+
                                       (tls-connection-server-app-key conn)
                                       (tls-connection-server-app-iv conn)
                                       (tls-connection-server-seq conn)
                                       cipher-suite)))
      (incf (tls-connection-server-seq conn))
      (serialize-tls-record
       (make-tls-record :content-type +content-application-data+
                        :data ciphertext)))))

(defun %server-handle-client-certificate (conn payload-bytes message-bytes)
  "Process a Certificate handshake message in :wait-client-certificate.
   PAYLOAD-BYTES is the parsed body (without the handshake header);
   MESSAGE-BYTES is the full message including the header, used to
   advance the transcript. Stores the parsed cert chain on the
   connection (in tls-connection-server-certificates -- the slot is
   role-relative; for a server it holds the peer/client chain) and
   advances state to :wait-client-certificate-verify when at least
   one cert was presented, or :wait-finished when the client returned
   an empty Certificate (RFC 8446 §4.4.2.4 -- valid when the server
   used `verify-mode = +tls13-verify-peer+' alone). When the server
   was configured with require-cert and the client returned empty,
   signals an error so tls-accept can translate it to a
   `certificate_required(116)' alert."
  (let ((ks (tls-connection-key-schedule conn))
        (config (tls-connection-server-config conn)))
    (multiple-value-bind (cert-data-list leaf-staple)
        (parse-certificate-message payload-bytes)
      (declare (ignore leaf-staple))
      (let ((parsed (mapcar #'x509:parse-x509-certificate cert-data-list)))
        (setf (tls-connection-server-certificates conn) parsed)
        (cond
          ((null parsed)
           (when (and config
                      (%verify-fail-if-missing-p
                       (tls-server-config-verify-mode config)))
             (error "client certificate required but Certificate was empty"))
           ;; Soft mode: accept handshake without a peer cert. Skip
           ;; CertificateVerify (the client won't send one) and go
           ;; straight to Finished.
           (transcript-update ks message-bytes)
           (setf (tls-connection-state conn) :wait-finished))
          (t
           ;; Snapshot the transcript hash *before* CertificateVerify
           ;; is folded in -- that's the input to the client's CV
           ;; signature (RFC 8446 §4.4.3).
           (transcript-update ks message-bytes)
           (setf (tls-connection-server-transcript-before-client-cv conn)
                 (transcript-current-hash ks))
           (setf (tls-connection-state conn)
                 :wait-client-certificate-verify)))))))

(defun %server-handle-client-certificate-verify (conn payload-bytes message-bytes)
  "Process a CertificateVerify in :wait-client-certificate-verify.
   Verifies the client's signature over the captured pre-CV
   transcript hash, then advances state to :wait-finished."
  (let ((ks (tls-connection-key-schedule conn))
        (transcript-hash
         (tls-connection-server-transcript-before-client-cv conn)))
    (unless transcript-hash
      (error "internal: no pre-CV transcript captured"))
    (multiple-value-bind (scheme signature)
        (parse-certificate-verify payload-bytes)
      (verify-certificate-verify-signature conn scheme signature transcript-hash))
    (transcript-update ks message-bytes)
    (setf (tls-connection-state conn) :wait-finished)))

(defun %server-handle-client-finished (conn payload-bytes message-bytes)
  "Process the client's Finished. On success transitions the
   connection to :connected and (if a STEK store is configured)
   returns the wire bytes of one or more NewSessionTicket records to
   emit. Otherwise NIL."
  (let* ((verify-data (parse-finished payload-bytes))
         (ks (tls-connection-key-schedule conn))
         (transcript-hash (transcript-current-hash ks))
         (finished-key (derive-finished-key ks (tls-connection-client-hs-secret conn)))
         (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                              finished-key transcript-hash)))
    (unless (equalp verify-data expected)
      (error "Client Finished verification failed"))
    ;; Update transcript with the client Finished so the resumption
    ;; master secret is computed over the full handshake (RFC 8446 §7.1).
    (transcript-update ks message-bytes)
    ;; Reset sequence numbers for application traffic
    (setf (tls-connection-client-seq conn) 0)
    (setf (tls-connection-server-seq conn) 0)
    (setf (tls-connection-state conn) :connected)
    (%server-emit-session-ticket conn)))

(defun tls-server-process-finished (conn ciphertext)
  "Process the client's post-flight encrypted handshake record.

   On a non-mTLS handshake this is a single Finished message and we
   take the original (pre-IMPL-397) single-shot path verbatim --
   parse one handshake header, verify Finished, transition to
   :connected. With mTLS (cert-requested-p) the same record can
   carry Certificate / CertificateVerify / Finished back-to-back, so
   we decrypt once and walk the plaintext message-by-message.

   Splitting on the flag rather than running every handshake through
   the state machine preserves the byte-for-byte shape the older
   single-message path produced for non-mTLS connections, including
   the historical `(transcript-update ks plaintext)' fold (where
   `plaintext' is whatever decrypt-record handed back, including any
   trailing bytes the multi-message walk would otherwise drop)."
  (multiple-value-bind (plaintext inner-ct)
      (decrypt-record ciphertext
                      (tls-connection-client-handshake-key conn)
                      (tls-connection-client-handshake-iv conn)
                      (tls-connection-client-seq conn)
                      (tls-connection-cipher-suite conn))
    (incf (tls-connection-client-seq conn))
    (unless (= inner-ct +content-handshake+)
      (error "Expected handshake, got content-type ~D" inner-ct))
    (cond
      ((tls-connection-cert-requested-p conn)
       ;; mTLS path: walk the plaintext message-by-message dispatching
       ;; on the current state.
       (let ((pos 0)
             (nst-bytes nil))
         (loop while (< pos (length plaintext))
               do (multiple-value-bind (type payload next-pos)
                      (parse-handshake-header plaintext pos)
                    (unless type
                      (error "Truncated handshake message at offset ~D" pos))
                    (let ((message-bytes (subseq plaintext pos next-pos))
                          (state (tls-connection-state conn)))
                      (cond
                        ((and (eq state :wait-client-certificate)
                              (= type +handshake-certificate+))
                         (%server-handle-client-certificate
                          conn payload message-bytes))
                        ((and (eq state :wait-client-certificate-verify)
                              (= type +handshake-certificate-verify+))
                         (%server-handle-client-certificate-verify
                          conn payload message-bytes))
                        ((and (eq state :wait-finished)
                              (= type +handshake-finished+))
                         (setf nst-bytes
                               (%server-handle-client-finished
                                conn payload message-bytes)))
                        (t
                         (error "Unexpected handshake type ~D in state ~A"
                                type state)))
                      (setf pos next-pos))))
         nst-bytes))
      (t
       ;; Non-mTLS: pre-IMPL-397 single-message Finished path.
       (multiple-value-bind (type payload) (parse-handshake-header plaintext)
         (unless (= type +handshake-finished+)
           (error "Expected Finished message, got type ~D" type))
         (let* ((verify-data (parse-finished payload))
                (ks (tls-connection-key-schedule conn))
                (transcript-hash (transcript-current-hash ks))
                (finished-key (derive-finished-key
                               ks (tls-connection-client-hs-secret conn)))
                (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                     finished-key transcript-hash)))
           (unless (equalp verify-data expected)
             (error "Client Finished verification failed"))
           (transcript-update ks plaintext)
           (setf (tls-connection-client-seq conn) 0)
           (setf (tls-connection-server-seq conn) 0)
           (setf (tls-connection-state conn) :connected)
           (%server-emit-session-ticket conn)))))))

;;; ===========================================================================
;;; TLS Stream Adapter
;;; ===========================================================================
;;;
;;; Provides tls-connect, tls-accept, tls-read, tls-write, tls-shutdown, and
;;; tls-close matching the epsilon.crypto API but using the pure-Lisp TLS 1.3
;;; implementation instead of OpenSSL.
;;;
;;; The transport abstraction uses a generic function protocol so that callers
;;; can provide any byte transport (TCP sockets, in-memory buffers, etc.).

;;; ---------------------------------------------------------------------------
;;; Transport Protocol (generic functions)
;;; ---------------------------------------------------------------------------

(defgeneric tls-transport-read (transport buffer start end)
  (:documentation
   "Read bytes from the transport into BUFFER[START..END).
    Returns the number of bytes actually read (0 at EOF)."))

(defgeneric tls-transport-write (transport buffer start end)
  (:documentation
   "Write bytes from BUFFER[START..END) to the transport.
    Returns the number of bytes actually written."))

(defgeneric tls-transport-close (transport)
  (:documentation
   "Close the transport."))

;;; ---------------------------------------------------------------------------
;;; FD-based transport (POSIX sockets) - uses CL streams
;;; ---------------------------------------------------------------------------

(defstruct fd-transport
  "Transport backed by a Common Lisp binary stream."
  (stream nil)
  (open-p t :type boolean))

(defmethod tls-transport-read ((transport fd-transport) buffer start end)
  (unless (fd-transport-open-p transport)
    (return-from tls-transport-read 0))
  (let ((n (read-sequence buffer (fd-transport-stream transport)
                          :start start :end end)))
    (- n start)))

(defmethod tls-transport-write ((transport fd-transport) buffer start end)
  (unless (fd-transport-open-p transport)
    (error "Transport closed"))
  (let ((n (- end start)))
    (write-sequence buffer (fd-transport-stream transport)
                    :start start :end end)
    (force-output (fd-transport-stream transport))
    n))

(defmethod tls-transport-close ((transport fd-transport))
  (when (fd-transport-open-p transport)
    (setf (fd-transport-open-p transport) nil)
    (ignore-errors (close (fd-transport-stream transport)))))

;;; ---------------------------------------------------------------------------
;;; In-memory transport (for testing)
;;; ---------------------------------------------------------------------------

(defstruct memory-transport
  "Transport backed by in-memory buffers (for testing).
   INCOMING is data to be read (simulates network receive).
   OUTGOING collects data written (simulates network send)."
  (incoming (make-array 0 :element-type '(unsigned-byte 8))
            :type (simple-array (unsigned-byte 8) (*)))
  (incoming-pos 0 :type fixnum)
  (outgoing (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))

(defmethod tls-transport-read ((transport memory-transport) buffer start end)
  (let* ((avail (- (length (memory-transport-incoming transport))
                   (memory-transport-incoming-pos transport)))
         (n (min (- end start) avail)))
    (when (plusp n)
      (replace buffer (memory-transport-incoming transport)
               :start1 start :end1 (+ start n)
               :start2 (memory-transport-incoming-pos transport))
      (incf (memory-transport-incoming-pos transport) n))
    n))

(defmethod tls-transport-write ((transport memory-transport) buffer start end)
  (let ((out (memory-transport-outgoing transport)))
    (loop for i from start below end
          do (vector-push-extend (aref buffer i) out))
    (- end start)))

(defmethod tls-transport-close ((transport memory-transport))
  nil)

(defun memory-transport-get-output (transport)
  "Return all bytes written to a memory-transport as a simple-array."
  (let ((out (memory-transport-outgoing transport)))
    (make-array (length out) :element-type '(unsigned-byte 8)
                :initial-contents out)))

(defun memory-transport-feed (transport data)
  "Feed bytes into a memory-transport's incoming buffer for reading."
  (let* ((old (memory-transport-incoming transport))
         (pos (memory-transport-incoming-pos transport))
         (remaining (- (length old) pos))
         (new-len (+ remaining (length data)))
         (new-buf (make-array new-len :element-type '(unsigned-byte 8))))
    (when (plusp remaining)
      (replace new-buf old :start2 pos))
    (replace new-buf data :start1 remaining)
    (setf (memory-transport-incoming transport) new-buf)
    (setf (memory-transport-incoming-pos transport) 0)))

;;; ---------------------------------------------------------------------------
;;; TLS Stream
;;; ---------------------------------------------------------------------------

(defstruct tls-stream
  "A TLS 1.3 stream wrapping a transport and a TLS connection.

   The stream drives the TLS record protocol over an arbitrary transport,
   providing tls-read/tls-write for application data exchange.

   Slots:
     CONNECTION: The TLS-CONNECTION state machine
     TRANSPORT: The underlying byte transport
     READ-BUFFER: Buffered decrypted application data
     READ-POS: Current position in read buffer
     CLOSED-P: Whether the stream has been closed"
  (connection nil)
  (transport nil)
  (read-buffer (make-array 0 :element-type '(unsigned-byte 8))
               :type (simple-array (unsigned-byte 8) (*)))
  (read-pos 0 :type fixnum)
  (closed-p nil :type boolean))

;;; ---------------------------------------------------------------------------
;;; Record I/O helpers
;;; ---------------------------------------------------------------------------

(defun transport-read-exact (transport n)
  "Read exactly N bytes from TRANSPORT. Returns byte vector or NIL on EOF."
  (let ((buf (make-array n :element-type '(unsigned-byte 8)))
        (pos 0))
    (loop while (< pos n)
          do (let ((got (tls-transport-read transport buf pos n)))
               (when (zerop got)
                 (if (zerop pos)
                     (return-from transport-read-exact nil)
                     (error "Unexpected EOF after ~D of ~D bytes" pos n)))
               (incf pos got)))
    buf))

(defun read-tls-record-from-transport (transport)
  "Read one complete TLS record from the transport.
   Returns the raw record bytes (header + payload) or NIL at EOF."
  ;; Read the 5-byte header: content-type (1) + version (2) + length (2)
  (let ((header (transport-read-exact transport 5)))
    (unless header (return-from read-tls-record-from-transport nil))
    (let ((payload-len (+ (ash (aref header 3) 8) (aref header 4))))
      (when (> payload-len 16641)
        (error "TLS record too large: ~D bytes" payload-len))
      (let ((payload (transport-read-exact transport payload-len)))
        (unless payload (error "Unexpected EOF reading TLS record payload"))
        (let ((record (make-array (+ 5 payload-len) :element-type '(unsigned-byte 8))))
          (replace record header)
          (replace record payload :start1 5)
          record)))))

(defun write-bytes-to-transport (transport bytes)
  "Write all of BYTES to the transport."
  (let ((pos 0)
        (total (length bytes)))
    (loop while (< pos total)
          do (let ((n (tls-transport-write transport bytes pos total)))
               (when (zerop n) (error "Transport write returned 0"))
               (incf pos n)))))

;;; ---------------------------------------------------------------------------
;;; Client handshake: tls-connect
;;; ---------------------------------------------------------------------------

(defun tls-connect (transport &key hostname trust-store alpn-protocols
                                   session-ticket
                                   new-session-ticket-callback)
  "Establish a TLS 1.3 connection as a client.

   Performs the full handshake over TRANSPORT and returns a TLS-STREAM
   ready for application data exchange.

   Parameters:
     TRANSPORT: Object implementing TLS-TRANSPORT-READ/WRITE/CLOSE
     HOSTNAME (string): Server hostname for SNI and certificate verification
     TRUST-STORE (list): List of trusted CA certificates (DER byte arrays)
     ALPN-PROTOCOLS (list): ALPN protocol strings to advertise
     SESSION-TICKET (tls-session-ticket): Ticket from a previous connection
                    for PSK resumption (1-RTT handshake)
     NEW-SESSION-TICKET-CALLBACK (function): Optional (conn ticket) hook
                    invoked once per NewSessionTicket received from the
                    server. Lets a caller-supplied cache populate
                    itself without this layer knowing about it.

   Returns:
     TLS-STREAM with established connection

   Example:
     (let ((stream (tls-connect transport :hostname \"example.com\")))
       (tls-write stream (string-to-bytes \"GET / HTTP/1.1\\r\\n\"))
       (tls-read stream buffer))"
  (let ((conn (make-tls-connection
               :hostname hostname
               :trust-store trust-store
               :alpn-protocols alpn-protocols
               :new-session-ticket-callback new-session-ticket-callback)))
    ;; 1. Send ClientHello (PSK or fresh)
    (let ((ch-record (if session-ticket
                         (build-psk-client-hello conn session-ticket
                                                 :alpn alpn-protocols)
                         (tls-start-handshake conn))))
      (write-bytes-to-transport transport ch-record))
    ;; 2. Read records and drive handshake until connected
    (loop until (eq (tls-connection-state conn) :connected)
          do (let ((record-bytes (read-tls-record-from-transport transport)))
               (unless record-bytes
                 (error "Server closed connection during handshake"))
               (let ((response (tls-handshake-step conn record-bytes)))
                 ;; Send any response data (client Finished, etc.)
                 (when (and response (typep response '(simple-array (unsigned-byte 8) (*))))
                   (write-bytes-to-transport transport response)))))
    ;; 3. Return stream
    (make-tls-stream :connection conn :transport transport)))

;;; ---------------------------------------------------------------------------
;;; Server handshake: tls-accept
;;; ---------------------------------------------------------------------------

(defun tls-accept (transport config)
  "Accept a TLS 1.3 connection as a server.

   Reads the ClientHello from TRANSPORT, performs the server-side handshake,
   and returns a TLS-STREAM ready for application data exchange.

   Parameters:
     TRANSPORT: Object implementing TLS-TRANSPORT-READ/WRITE/CLOSE
     CONFIG (tls-server-config): Server configuration with certificate chain
                                  and private key

   Returns:
     TLS-STREAM with established connection"
  (let ((conn (make-tls-connection :role :server)))
    (setf (tls-connection-server-config conn) config)
    ;; 1. Read ClientHello
    (let* ((record-bytes (read-tls-record-from-transport transport))
           (record (parse-tls-record record-bytes))
           (ch-data (tls-record-data record))
           ;; Skip 4-byte handshake header to get payload
           (ch-payload (subseq ch-data 4)))
      ;; 2. Process ClientHello and generate server response.
      ;;    Wrap in handler-case so negotiation/parsing failures send the
      ;;    RFC-mandated TLS alert before propagating the error. Without
      ;;    this the peer sees a silent connection drop and hangs.
      (let ((response (handler-case (tls-server-start-handshake conn ch-payload)
                        (tls-decode-error (e)
                          (%send-alert-and-error transport +alert-decode-error+
                            "~A" e))
                        (error (e)
                          (let ((msg (princ-to-string e)))
                            (%send-alert-and-error transport
                              (cond ((search "decrypt_error" msg) +alert-decrypt-error+)
                                    (t +alert-handshake-failure+))
                              "~A" e))))))
        (write-bytes-to-transport transport response)))
    ;; 3. Read client Finished. When a STEK store is configured,
    ;;    tls-server-process-finished returns the wire bytes of a
    ;;    NewSessionTicket record that must be written to the peer
    ;;    before we hand back the stream. Dropping the return value
    ;;    was a real bug: the function has already encrypted the NST
    ;;    under the server application key at server_seq=0 and
    ;;    bumped server_seq to 1, so forgetting to emit it leaves
    ;;    the peer's expected server-write-sequence one step behind
    ;;    and every subsequent app-data record fails AEAD with
    ;;    "bad record mac" on the client side.
    (loop until (eq (tls-connection-state conn) :connected)
          do (let ((record-bytes (read-tls-record-from-transport transport)))
               (unless record-bytes
                 (error "Client closed connection during handshake"))
               (let ((record (parse-tls-record record-bytes)))
                 (cond
                   ((= (tls-record-content-type record) +content-change-cipher-spec+)
                    nil)  ; ignore CCS
                   ((= (tls-record-content-type record) +content-application-data+)
                    (let ((nst-bytes
                            (tls-server-process-finished
                             conn (tls-record-data record))))
                      (when nst-bytes
                        (write-bytes-to-transport transport nst-bytes))))
                   ((= (tls-record-content-type record) +content-alert+)
                    ;; Client sent a plaintext alert during the handshake
                    ;; (e.g. after rejecting our Certificate). Surface
                    ;; the structured `tls-alert-error' so callers see
                    ;; the actual level + description.
                    (let* ((data (tls-record-data record))
                           (level (if (> (length data) 0) (aref data 0) 0))
                           (desc (if (> (length data) 1) (aref data 1) 0)))
                      (error 'tls-alert-error
                             :level level :description desc
                             :state :server-handshake)))
                   (t
                    (error "Unexpected record type ~D during server handshake"
                           (tls-record-content-type record)))))))
    ;; 4. Return stream
    (make-tls-stream :connection conn :transport transport)))

;;; ---------------------------------------------------------------------------
;;; Data transfer: tls-read / tls-write
;;; ---------------------------------------------------------------------------

(defun tls-read (stream buffer &key (start 0) (end (length buffer)))
  "Read decrypted data from a TLS stream into BUFFER[START..END).

   Returns the number of bytes read, or 0 at EOF/closed."
  (when (tls-stream-closed-p stream)
    (return-from tls-read 0))
  ;; First, serve from buffered data
  (let* ((rb (tls-stream-read-buffer stream))
         (rp (tls-stream-read-pos stream))
         (avail (- (length rb) rp))
         (wanted (- end start)))
    (when (plusp avail)
      (let ((n (min avail wanted)))
        (replace buffer rb :start1 start :end1 (+ start n) :start2 rp)
        (incf (tls-stream-read-pos stream) n)
        (return-from tls-read n))))
  ;; Buffer empty - read from transport
  (let* ((conn (tls-stream-connection stream))
         (transport (tls-stream-transport stream))
         (record-bytes (read-tls-record-from-transport transport)))
    (unless record-bytes
      (return-from tls-read 0))
    (let ((record (parse-tls-record record-bytes)))
      (cond
        ;; Application data
        ((= (tls-record-content-type record) +content-application-data+)
         (let ((plaintext (tls-receive-application-data conn (tls-record-data record))))
           (cond
             ((eq plaintext :close-notify)
              ;; Server sent encrypted close_notify -- clean EOF
              (setf (tls-stream-closed-p stream) t)
              0)
             ((null plaintext)
              ;; Post-handshake message was processed, try again
              (tls-read stream buffer :start start :end end))
             (t
              (let* ((n (min (- end start) (length plaintext))))
                ;; Copy what fits into buffer
                (replace buffer plaintext :start1 start :end1 (+ start n))
                ;; Buffer the rest
                (when (> (length plaintext) n)
                  (setf (tls-stream-read-buffer stream)
                        (subseq plaintext n))
                  (setf (tls-stream-read-pos stream) 0))
                n)))))
        ;; Alert
        ((= (tls-record-content-type record) +content-alert+)
         (let ((alert (parse-alert (tls-record-data record))))
           (when (= (tls-alert-description alert) +alert-close-notify+)
             (setf (tls-stream-closed-p stream) t))
           0))
        (t 0)))))

(defun tls-write (stream buffer &key (start 0) (end (length buffer)))
  "Write data from BUFFER[START..END) to the TLS stream.

   Returns the number of bytes written."
  (when (tls-stream-closed-p stream)
    (error "Cannot write to closed TLS stream"))
  (let* ((conn (tls-stream-connection stream))
         (transport (tls-stream-transport stream))
         (data (if (and (zerop start) (= end (length buffer)))
                   buffer
                   (subseq buffer start end)))
         ;; Fragment into 16KB max record payloads
         (pos 0)
         (total (length data)))
    (loop while (< pos total)
          do (let* ((chunk-end (min total (+ pos 16384)))
                    (chunk (subseq data pos chunk-end))
                    (record-bytes (tls-send-application-data conn chunk)))
               (write-bytes-to-transport transport record-bytes)
               (setf pos chunk-end)))
    total))

;;; ---------------------------------------------------------------------------
;;; Connection management
;;; ---------------------------------------------------------------------------

(defun tls-shutdown (stream)
  "Send a close_notify alert to the peer.
   The stream can still read remaining data after shutdown."
  (unless (tls-stream-closed-p stream)
    (let* ((conn (tls-stream-connection stream))
           (transport (tls-stream-transport stream))
           (alert-bytes (tls-close-notify conn)))
      (ignore-errors
        (write-bytes-to-transport transport alert-bytes)))))

(defun tls-close (stream)
  "Shut down and close the TLS stream and its transport."
  (unless (tls-stream-closed-p stream)
    (tls-shutdown stream)
    (setf (tls-stream-closed-p stream) t)
    (tls-transport-close (tls-stream-transport stream)))
  t)

;;; ---------------------------------------------------------------------------
;;; Stream accessors
;;; ---------------------------------------------------------------------------

(defun tls-stream-peer-certificates (stream)
  "Return the list of peer certificates (DER byte arrays)."
  (tls-connection-server-certificates (tls-stream-connection stream)))

(defun tls-stream-cipher-suite (stream)
  "Return the negotiated cipher suite constant."
  (tls-connection-cipher-suite (tls-stream-connection stream)))

;;; Already exported via tls-connection-alpn-protocol on the connection,
;;; but provide a convenience accessor on the stream.
(defun tls-stream-alpn-protocol (stream)
  "Return the negotiated ALPN protocol string, or NIL."
  (tls-connection-alpn-protocol (tls-stream-connection stream)))

;;; ---------------------------------------------------------------------------
;;; Convenience I/O helpers
;;; ---------------------------------------------------------------------------

(defun string-to-bytes (string)
  "Convert a string to a byte vector (ASCII/Latin-1)."
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun bytes-to-string (bytes &key (start 0) (end (length bytes)))
  "Convert a byte vector to a string."
  (let ((result (make-string (- end start))))
    (loop for i from start below end
          for j from 0
          do (setf (char result j) (code-char (aref bytes i))))
    result))

(defun tls-write-string (stream string)
  "Write a string to the TLS stream as bytes."
  (tls-write stream (string-to-bytes string)))

(defun tls-write-line (stream string)
  "Write a string followed by CRLF to the TLS stream."
  (tls-write-string stream (concatenate 'string string (string #\Return) (string #\Newline))))

(defun tls-read-line (stream &key (max-length 8192))
  "Read a line (terminated by LF or CRLF) from the TLS stream.
   Returns the line as a string (without terminator), or NIL at EOF."
  (let ((buf (make-array max-length :element-type '(unsigned-byte 8)
                                     :fill-pointer 0))
        (one-byte (make-array 1 :element-type '(unsigned-byte 8))))
    (loop
      (let ((n (tls-read stream one-byte)))
        (cond
          ((zerop n)
           (if (zerop (length buf))
               (return nil)
               (return (bytes-to-string buf))))
          (t
           (let ((byte (aref one-byte 0)))
             (cond
               ((= byte 10)  ; LF
                ;; Strip trailing CR
                (when (and (plusp (length buf))
                           (= (aref buf (1- (length buf))) 13))
                  (decf (fill-pointer buf)))
                (return (bytes-to-string buf)))
               (t
                (when (>= (length buf) max-length)
                  (error "Line exceeds maximum length ~D" max-length))
                (vector-push byte buf))))))))))

;;; ---------------------------------------------------------------------------
;;; JA3 / JA4 ClientHello fingerprinting
;;;
;;; These are diagnostic only -- they let us see what shape we present
;;; to the network so a CDN that filters by client fingerprint can be
;;; debugged without speculating. Both formats discard GREASE values
;;; per their respective specs (RFC 8701 §3 / JA4 §2.4).
;;; ---------------------------------------------------------------------------

(defun %grease-codepoint-p (cp)
  "T iff CP is one of the 16 RFC 8701 GREASE values."
  (declare (type (unsigned-byte 16) cp))
  (and (= (ldb (byte 4 0) cp) 10)
       (= (ldb (byte 4 8) cp) 10)
       (= (ldb (byte 8 0) cp) (ldb (byte 8 8) cp))))

(defun %ch-skip-prefix (ch pos)
  "Advance POS past the legacy_version, random, and session_id fields
   of a ClientHello payload (no handshake header). Returns the offset
   of the first byte of cipher_suites."
  (incf pos 2)                          ; legacy_version
  (incf pos 32)                         ; random
  (incf pos (1+ (aref ch pos)))         ; session_id (u8 length + data)
  pos)

(defun %read-u16-be (ch pos)
  (logior (ash (aref ch pos) 8) (aref ch (1+ pos))))

(defun %ch-extract-fingerprint-slots (ch-bytes)
  "Walk a ClientHello handshake message (with the 4-byte handshake
   header) and return a plist with :legacy-version, :cipher-suites,
   :extensions (as a list of (type . body)), :groups (from
   supported_groups extension), :ec-point-formats, :alpn-protocols,
   :sni-present-p, :supported-versions, :signature-algorithms.
   Cipher_suites and extension types are returned in advertised
   order; GREASE filtering is performed by callers."
  (let* ((body-len (logior (ash (aref ch-bytes 1) 16)
                           (ash (aref ch-bytes 2) 8)
                           (aref ch-bytes 3)))
         (ch (subseq ch-bytes 4 (+ 4 body-len)))
         (legacy-version (%read-u16-be ch 0))
         (pos (%ch-skip-prefix ch 0))
         ;; cipher_suites
         (cs-len (%read-u16-be ch pos))
         (_ (incf pos 2))
         (cs-end (+ pos cs-len))
         (ciphers (loop while (< pos cs-end)
                        collect (%read-u16-be ch pos)
                        do (incf pos 2)))
         ;; legacy_compression_methods
         (cm-len (aref ch pos))
         (__ (incf pos (1+ cm-len)))
         ;; extensions
         (exts-len (%read-u16-be ch pos))
         (___ (incf pos 2))
         (exts-end (+ pos exts-len))
         (extensions (loop while (< pos exts-end)
                           for type = (%read-u16-be ch pos)
                           for elen = (%read-u16-be ch (+ pos 2))
                           for body = (subseq ch (+ pos 4) (+ pos 4 elen))
                           collect (cons type body)
                           do (setf pos (+ pos 4 elen)))))
    (declare (ignore _ __ ___))
    (let ((groups nil)
          (point-formats nil)
          (alpn nil)
          (sni-present nil)
          (supported-versions nil)
          (signature-algorithms nil))
      (dolist (ext extensions)
        (let ((type (car ext))
              (body (cdr ext)))
          (cond
            ((= type +ext-server-name+)
             (setf sni-present (plusp (length body))))
            ((= type +ext-supported-groups+)
             (let ((bp 0)
                   (n (logior (ash (aref body 0) 8) (aref body 1))))
               (declare (ignore n))
               (incf bp 2)
               (loop while (< bp (length body))
                     do (push (logior (ash (aref body bp) 8)
                                      (aref body (1+ bp))) groups)
                        (incf bp 2))
               (setf groups (nreverse groups))))
            ((= type +ext-ec-point-formats+)
             (let ((n (aref body 0)))
               (loop for i from 1 to n
                     do (push (aref body i) point-formats))
               (setf point-formats (nreverse point-formats))))
            ((= type +ext-alpn+)
             (let ((bp 2)) ; skip the u16 outer length
               (loop while (< bp (length body))
                     for plen = (aref body bp)
                     do (push (map 'string #'code-char
                                   (subseq body (1+ bp) (+ 1 bp plen)))
                              alpn)
                        (incf bp (1+ plen)))
               (setf alpn (nreverse alpn))))
            ((= type +ext-supported-versions+)
             (let ((bp 1)) ; skip the u8 list length
               (loop while (< bp (length body))
                     do (push (logior (ash (aref body bp) 8)
                                      (aref body (1+ bp))) supported-versions)
                        (incf bp 2))
               (setf supported-versions (nreverse supported-versions))))
            ((= type +ext-signature-algorithms+)
             (let ((bp 2)) ; skip u16 outer length
               (loop while (< bp (length body))
                     do (push (logior (ash (aref body bp) 8)
                                      (aref body (1+ bp))) signature-algorithms)
                        (incf bp 2))
               (setf signature-algorithms
                     (nreverse signature-algorithms)))))))
      (list :legacy-version legacy-version
            :cipher-suites ciphers
            :extensions extensions
            :groups groups
            :ec-point-formats point-formats
            :alpn-protocols alpn
            :sni-present-p sni-present
            :supported-versions supported-versions
            :signature-algorithms signature-algorithms))))

(defun %csv (xs)
  (with-output-to-string (s)
    (loop for first = t then nil
          for x in xs
          unless first do (write-char #\- s)
          do (princ x s))))

(defun client-hello-ja3-fingerprint (ch-bytes)
  "Return the JA3 fingerprint (32-char hex MD5) of a ClientHello
   handshake message (4-byte handshake header included).

   JA3 string format (Salesforce, 2017):
     VERSION,CIPHERS,EXTENSIONS,GROUPS,EC_POINT_FORMATS

   GREASE codepoints are filtered out before formatting."
  (let* ((slots (%ch-extract-fingerprint-slots ch-bytes))
         (ver (getf slots :legacy-version))
         (ciphers (remove-if #'%grease-codepoint-p (getf slots :cipher-suites)))
         (ext-types (remove-if #'%grease-codepoint-p
                                (mapcar #'car (getf slots :extensions))))
         (groups (remove-if #'%grease-codepoint-p (getf slots :groups)))
         (formats (getf slots :ec-point-formats))
         (ja3-string (format nil "~A,~A,~A,~A,~A"
                             ver (%csv ciphers) (%csv ext-types)
                             (%csv groups) (%csv formats))))
    (md5:md5-hex (sb-ext:string-to-octets ja3-string :external-format :ascii))))

(defun %ja4-version-tag (supported-versions)
  "Return the 2-digit version tag for JA4 ja4_a, drawn from
   supported_versions: \"13\" for 1.3, \"12\" for 1.2, etc."
  (let ((versions (sort (copy-list supported-versions) #'>)))
    (dolist (v versions "00")
      (cond
        ((= v #x0304) (return "13"))
        ((= v #x0303) (return "12"))
        ((= v #x0302) (return "11"))
        ((= v #x0301) (return "10"))))))

(defun %ja4-alpn-grease-p (s)
  "True for the 2-byte ALPN value whose bytes encode a GREASE
   codepoint (RFC 8701 §3.5). The two bytes are equal and have
   low nibble = 0xA."
  (and (= (length s) 2)
       (let ((b0 (char-code (char s 0))))
         (and (= b0 (char-code (char s 1)))
              (= (ldb (byte 4 0) b0) 10)))))

(defun %ja4-alpn-tag (alpn)
  "Return the 2-char ALPN tag for JA4 ja4_a: first and last char of
   the first non-GREASE ALPN value, or \"00\" if no ALPN."
  (let ((real (find-if-not #'%ja4-alpn-grease-p alpn)))
    (if (and real (plusp (length real)))
        (format nil "~C~C" (char real 0) (char real (1- (length real))))
        "00")))

(defun %ja4-hex2 (n)
  (format nil "~(~4,'0x~)" n))

(defun %ja4-truncated-sha256 (s)
  "First 12 hex chars of SHA-256(S) where S is an ASCII string."
  (let* ((bytes (sb-ext:string-to-octets s :external-format :ascii))
         (digest (sha256:sha256 bytes)))
    (with-output-to-string (out)
      (loop for i from 0 below 6
            do (format out "~(~2,'0x~)" (aref digest i))))))

(defun client-hello-ja4-fingerprint (ch-bytes)
  "Return the JA4 fingerprint (FoxIO 2024) of a ClientHello.

   Format: ja4_a + \"_\" + ja4_b + \"_\" + ja4_c

     ja4_a = \"t\" + version2 + sni-flag + #ciphers(2) + #exts(2) + alpn2
     ja4_b = first 12 hex chars of SHA-256(sorted-ciphers, comma-CSV)
     ja4_c = first 12 hex chars of SHA-256(sorted-exts-sans-sni-alpn,
                                            comma-CSV +
                                            \"_\" + sigalgs-original-order)

   GREASE codepoints are filtered out before counting and hashing."
  (let* ((slots (%ch-extract-fingerprint-slots ch-bytes))
         (ciphers (remove-if #'%grease-codepoint-p
                             (getf slots :cipher-suites)))
         (ext-pairs (remove-if (lambda (p) (%grease-codepoint-p (car p)))
                                (getf slots :extensions)))
         (ext-types (mapcar #'car ext-pairs))
         (alpn (getf slots :alpn-protocols))
         (sni-flag (if (getf slots :sni-present-p) #\d #\i))
         (ja4-a (format nil "t~A~C~2,'0D~2,'0D~A"
                        (%ja4-version-tag (getf slots :supported-versions))
                        sni-flag
                        (min 99 (length ciphers))
                        (min 99 (length ext-types))
                        (%ja4-alpn-tag alpn)))
         (sorted-ciphers (sort (copy-list ciphers) #'<))
         (cipher-csv (with-output-to-string (s)
                       (loop for first = t then nil
                             for c in sorted-ciphers
                             unless first do (write-char #\, s)
                             do (write-string (%ja4-hex2 c) s))))
         (sigalgs (remove-if #'%grease-codepoint-p
                             (getf slots :signature-algorithms)))
         (filtered-exts (remove-if (lambda (e)
                                     (or (= e +ext-server-name+)
                                         (= e +ext-alpn+)))
                                   ext-types))
         (sorted-exts (sort (copy-list filtered-exts) #'<))
         (ext-csv (with-output-to-string (s)
                    (loop for first = t then nil
                          for e in sorted-exts
                          unless first do (write-char #\, s)
                          do (write-string (%ja4-hex2 e) s))))
         (sigalgs-csv (with-output-to-string (s)
                        (loop for first = t then nil
                              for sa in sigalgs
                              unless first do (write-char #\, s)
                              do (write-string (%ja4-hex2 sa) s))))
         (ja4-b (%ja4-truncated-sha256 cipher-csv))
         (ja4-c (%ja4-truncated-sha256
                 (concatenate 'string ext-csv "_" sigalgs-csv))))
    (concatenate 'string ja4-a "_" ja4-b "_" ja4-c)))
