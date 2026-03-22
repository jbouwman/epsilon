;;;; TLS 1.3 Protocol (RFC 8446)
;;;;
;;;; Implements the TLS 1.3 record layer, handshake protocol, key schedule,
;;;; and state machine. TLS 1.3 only -- no support for legacy versions.

(defpackage epsilon.ssl.tls13
  (:use :cl)
  (:local-nicknames
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:hmac #:epsilon.ssl.hmac)
   (#:hkdf #:epsilon.ssl.hkdf)
   (#:aes-gcm #:epsilon.ssl.aes-gcm)
   (#:chacha #:epsilon.ssl.chacha20-poly1305)
   (#:x25519 #:epsilon.ssl.curve25519)
   (#:ec-p256 #:epsilon.ssl.ec-p256)
   (#:ecdh #:epsilon.ssl.ecdh)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:rsa #:epsilon.ssl.rsa)
   (#:drbg #:epsilon.ssl.drbg)
   (#:x509 #:epsilon.ssl.x509)
   (#:asn1 #:epsilon.ssl.asn1)
   (#:pkcs #:epsilon.ssl.pkcs))
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
   ;; 0-RTT early data
   #:tls-send-early-data
   ;; HelloRetryRequest
   #:+hello-retry-request-magic+
   ;; KeyUpdate
   #:build-key-update
   #:process-key-update
   ;; Server-side handshake
   #:tls-server-config
   #:make-tls-server-config
   #:tls-server-start-handshake
   #:tls-server-process-finished
   #:parse-client-hello
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
   #:parsed-client-hello-quic-transport-params))

(in-package :epsilon.ssl.tls13)

;;; ---------------------------------------------------------------------------
;;; TLS 1.3 Constants (RFC 8446)
;;; ---------------------------------------------------------------------------

;; Protocol version
(defconstant +tls-1.3+ #x0303)  ; legacy version for record layer
(defconstant +tls-1.3-supported-version+ #x0304)  ; real version in extension

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
(defconstant +handshake-finished+ 20)
(defconstant +handshake-key-update+ 24)
(defconstant +handshake-end-of-early-data+ 5)

;; Cipher suites (RFC 8446 Section B.4)
(defconstant +tls-aes-128-gcm-sha256+ #x1301)
(defconstant +tls-aes-256-gcm-sha384+ #x1302)
(defconstant +tls-chacha20-poly1305-sha256+ #x1303)

;; Extension types (RFC 8446 Section 4.2)
(defconstant +ext-server-name+ 0)
(defconstant +ext-supported-groups+ 10)
(defconstant +ext-signature-algorithms+ 13)
(defconstant +ext-alpn+ 16)
(defconstant +ext-pre-shared-key+ 41)
(defconstant +ext-supported-versions+ 43)
(defconstant +ext-key-share+ 51)

;; QUIC transport parameters (RFC 9001 Section 8.2)
(defconstant +ext-quic-transport-parameters+ #x39)

;; Named groups
(defconstant +group-x25519+ #x001D)
(defconstant +group-secp256r1+ #x0017)

;; Signature schemes
(defconstant +sig-ecdsa-secp256r1-sha256+ #x0403)
(defconstant +sig-ed25519+ #x0807)
(defconstant +sig-rsa-pss-rsae-sha256+ #x0804)
(defconstant +sig-rsa-pss-rsae-sha384+ #x0805)
(defconstant +sig-rsa-pss-rsae-sha512+ #x0806)

;; Alert descriptions
(defconstant +alert-close-notify+ 0)
(defconstant +alert-unexpected-message+ 10)
(defconstant +alert-bad-record-mac+ 20)
(defconstant +alert-handshake-failure+ 40)
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

(defun read-u8 (data pos)
  (values (aref data pos) (1+ pos)))

(defun read-u16 (data pos)
  (values (logior (ash (aref data pos) 8) (aref data (1+ pos)))
          (+ pos 2)))

(defun read-u24 (data pos)
  (values (logior (ash (aref data pos) 16)
                  (ash (aref data (1+ pos)) 8)
                  (aref data (+ pos 2)))
          (+ pos 3)))

(defun read-bytes (data pos len)
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
  (transcript-hash nil))

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
  "Update the transcript hash with handshake data."
  (ecase (tls13-key-schedule-hash-fn ks)
    (:sha256 (sha256:sha256-update (tls13-key-schedule-transcript-hash ks) data))
    (:sha384 (sha512:sha384-update (tls13-key-schedule-transcript-hash ks) data))))

(defun transcript-current-hash (ks)
  "Get the current transcript hash value (without modifying the state)."
  (ecase (tls13-key-schedule-hash-fn ks)
    (:sha256
     (let ((copy (sha256:sha256-copy (tls13-key-schedule-transcript-hash ks))))
       (sha256:sha256-finalize copy)))
    (:sha384
     (let ((copy (sha512:sha384-copy (tls13-key-schedule-transcript-hash ks))))
       (sha512:sha384-finalize copy)))))

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

(defun make-supported-versions-ext-client ()
  "Build supported_versions extension for ClientHello."
  (let ((buf (make-buffer)))
    ;; List of supported versions (1 entry: TLS 1.3)
    (buf-append-byte buf 2)  ; length of version list
    (buf-append-u16 buf +tls-1.3-supported-version+)
    (make-extension +ext-supported-versions+ (buf-freeze buf))))

(defun make-key-share-ext-client (groups)
  "Build key_share extension for ClientHello.
   GROUPS is a list of (:x25519 key-bytes) or (:secp256r1 key-bytes)."
  (let ((entries-buf (make-buffer)))
    (dolist (group groups)
      (let ((group-id (ecase (first group) (:x25519 +group-x25519+) (:secp256r1 +group-secp256r1+)))
            (key-data (second group)))
        (buf-append-u16 entries-buf group-id)
        (buf-append-u16-prefixed entries-buf key-data)))
    (let ((buf (make-buffer)))
      (buf-append-u16-prefixed buf (buf-freeze entries-buf))
      (make-extension +ext-key-share+ (buf-freeze buf)))))

(defun make-supported-groups-ext ()
  "Build supported_groups extension."
  (let ((buf (make-buffer)))
    ;; Named group list
    (buf-append-u16 buf 4)  ; 2 groups * 2 bytes each
    (buf-append-u16 buf +group-x25519+)
    (buf-append-u16 buf +group-secp256r1+)
    (make-extension +ext-supported-groups+ (buf-freeze buf))))

(defun make-signature-algorithms-ext ()
  "Build signature_algorithms extension."
  (let ((buf (make-buffer)))
    (buf-append-u16 buf 10)  ; 5 algorithms * 2 bytes
    (buf-append-u16 buf +sig-ecdsa-secp256r1-sha256+)
    (buf-append-u16 buf +sig-ed25519+)
    (buf-append-u16 buf +sig-rsa-pss-rsae-sha256+)
    (buf-append-u16 buf +sig-rsa-pss-rsae-sha384+)
    (buf-append-u16 buf +sig-rsa-pss-rsae-sha512+)
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

(defun make-alpn-ext (protocols)
  "Build ALPN extension."
  (let ((protos-buf (make-buffer)))
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
                                (cipher-suites (list +tls-aes-128-gcm-sha256+
                                                     +tls-chacha20-poly1305-sha256+
                                                     +tls-aes-256-gcm-sha384+))
                                key-shares
                                (alpn nil)
                                (extra-extensions nil))
  "Build a ClientHello handshake message.
   KEY-SHARES: list of (:x25519 pub-bytes) or (:secp256r1 pub-bytes).
   EXTRA-EXTENSIONS: list of pre-encoded extension byte vectors (e.g. QUIC transport params).
   Returns the full handshake message bytes."
  (let ((buf (make-buffer)))
    ;; legacy_version
    (buf-append-u16 buf +tls-1.3+)
    ;; random (32 bytes)
    (let ((random (drbg:random-bytes 32)))
      (buf-append-bytes buf random))
    ;; legacy_session_id (for middlebox compatibility)
    (let ((session-id (drbg:random-bytes 32)))
      (buf-append-u8-prefixed buf session-id))
    ;; cipher_suites
    (buf-append-u16 buf (* 2 (length cipher-suites)))
    (dolist (cs cipher-suites)
      (buf-append-u16 buf cs))
    ;; legacy_compression_methods
    (buf-append-byte buf 1)   ; length
    (buf-append-byte buf 0)   ; null compression
    ;; extensions
    (let ((exts-buf (make-buffer)))
      ;; supported_versions (mandatory)
      (buf-append-bytes exts-buf (make-supported-versions-ext-client))
      ;; supported_groups
      (buf-append-bytes exts-buf (make-supported-groups-ext))
      ;; key_share
      (when key-shares
        (buf-append-bytes exts-buf (make-key-share-ext-client key-shares)))
      ;; signature_algorithms
      (buf-append-bytes exts-buf (make-signature-algorithms-ext))
      ;; server_name
      (when hostname
        (buf-append-bytes exts-buf (make-server-name-ext hostname)))
      ;; ALPN
      (when alpn
        (buf-append-bytes exts-buf (make-alpn-ext alpn)))
      ;; Extra extensions (e.g. QUIC transport parameters)
      (dolist (ext extra-extensions)
        (buf-append-bytes exts-buf ext))
      ;; Write extensions with length prefix
      (buf-append-u16-prefixed buf (buf-freeze exts-buf)))
    ;; Wrap in handshake header
    (make-handshake-message +handshake-client-hello+ (buf-freeze buf))))

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

(defun parse-certificate-message (payload)
  "Parse Certificate handshake message.
   Returns list of DER-encoded certificate byte arrays."
  (let ((pos 0)
        (certs nil))
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
                       (setf pos (+ new-p3 ext-len))
                       (push cert-data certs)))))))
    (nreverse certs)))

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
  ;; Negotiated parameters
  (cipher-suite 0 :type fixnum)
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
  ;; Certificate chain
  (server-certificates nil :type list)
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
  ;; Transcript hash snapshots (for CertificateVerify)
  (transcript-before-cv nil)      ; hash before CertificateVerify was added
  ;; Server config (for server role)
  (server-config nil)
  ;; Received session tickets
  (received-tickets nil :type list))

(defun make-tls-connection (&key (role :client) hostname trust-store alpn-protocols)
  "Create a new TLS 1.3 connection."
  (%make-tls-connection
   :role role
   :hostname hostname
   :trust-store trust-store
   :alpn-protocols alpn-protocols
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
         ;; ECDSA signature is DER-encoded SEQUENCE { INTEGER r, INTEGER s }
         (let* ((sig-seq (asn1:der-decode signature))
                (sig-tlvs (asn1:der-decode-sequence-contents sig-seq))
                (r (asn1:decode-der-integer (asn1:asn1-tlv-value (first sig-tlvs))))
                (s (asn1:decode-der-integer (asn1:asn1-tlv-value (second sig-tlvs)))))
           (unless (ecdsa:ecdsa-verify point content r s)
             (error "CertificateVerify: ECDSA signature verification failed")))))
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
  "Generate ephemeral key pairs for key exchange."
  ;; X25519
  (let ((x25519-sk (drbg:random-bytes 32)))
    (setf (tls-connection-client-x25519-private conn) x25519-sk)
    (setf (tls-connection-client-x25519-public conn) (x25519:x25519-base x25519-sk)))
  ;; P-256
  (multiple-value-bind (p256-sk p256-pk) (ecdh:ecdh-p256-generate-keypair)
    (setf (tls-connection-client-p256-private conn) p256-sk)
    (setf (tls-connection-client-p256-public conn)
          (ec-p256:p256-point-encode-uncompressed p256-pk)))
  ;; Return key share entries for ClientHello (X25519 preferred, P-256 as fallback)
  (list (list :x25519 (tls-connection-client-x25519-public conn))
        (list :secp256r1 (tls-connection-client-p256-public conn))))

(defun tls-start-handshake (conn)
  "Begin a TLS 1.3 handshake. Returns the ClientHello record bytes to send."
  (let* ((key-shares (tls-generate-key-shares conn))
         (ch (build-client-hello
              :hostname (tls-connection-hostname conn)
              :key-shares key-shares
              :alpn (tls-connection-alpn-protocols conn))))
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
    ;; Verify supported version
    (let ((version (server-hello-get-supported-version sh)))
      (unless (and version (= version +tls-1.3-supported-version+))
        (error "Server does not support TLS 1.3")))
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
    ;; Process key_share
    (multiple-value-bind (group-id server-key) (server-hello-get-key-share sh)
      (let ((shared-secret
              (cond
                ((= group-id +group-x25519+)
                 (x25519:x25519 (tls-connection-client-x25519-private conn) server-key))
                ((= group-id +group-secp256r1+)
                 ;; Decode server public key, compute ECDH shared secret (x-coordinate)
                 (let ((server-point (ec-p256:p256-point-decode server-key)))
                   (ecdh:ecdh-p256-shared-secret
                    (tls-connection-client-p256-private conn)
                    server-point)))
                (t (error "Unsupported key exchange group: ~X" group-id)))))
        ;; Update transcript with ServerHello
        (let ((sh-msg (make-handshake-message +handshake-server-hello+ payload)))
          (transcript-update ks sh-msg))
        ;; Derive handshake secret
        (derive-early-secret ks)
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
       (let ((cert-data-list (parse-certificate-message payload)))
         (setf (tls-connection-server-certificates conn)
               (mapcar #'x509:parse-x509-certificate cert-data-list)))
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

      ;; Finished
      ((and (eq state :wait-finished)
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
           (tls-process-server-hello conn payload))
         ;; After ServerHello, server sends encrypted handshake records
         nil)
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
  "Increment the write sequence number."
  (if (eq (tls-connection-role conn) :server)
      (incf (tls-connection-server-seq conn))
      (incf (tls-connection-client-seq conn))))

(defun bump-read-seq (conn)
  "Increment the read sequence number."
  (if (eq (tls-connection-role conn) :server)
      (incf (tls-connection-client-seq conn))
      (incf (tls-connection-server-seq conn))))

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
         (error "TLS alert received: ~D" (if (> (length plaintext) 1) (aref plaintext 1) 0)))
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

(defun alert-description-name (code)
  "Return a human-readable name for an alert description code."
  (case code
    (0 "close_notify") (10 "unexpected_message") (20 "bad_record_mac")
    (40 "handshake_failure") (42 "bad_certificate") (45 "certificate_expired")
    (48 "unknown_ca") (50 "decode_error") (51 "decrypt_error")
    (70 "protocol_version") (80 "internal_error") (109 "missing_extension")
    (116 "certificate_required")
    (t (format nil "unknown(~D)" code))))

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

(defun read-u32 (data pos)
  "Read a 4-byte big-endian unsigned integer."
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
   RFC 8446 Section 4.2.11.2."
  (let* ((early-secret (ks-hkdf-extract ks
                         (make-array (tls13-key-schedule-hash-len ks)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         psk))
         (binder-key (ks-hkdf-expand-label ks early-secret
                                           "res binder" +empty-context+
                                           (tls13-key-schedule-hash-len ks)))
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
;;; ---------------------------------------------------------------------------

(defun tls-send-early-data (conn data)
  "Encrypt and send early data (0-RTT) after sending a PSK ClientHello.
   Must be called after build-psk-client-hello and before processing ServerHello.
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
   Identified by the special random value."
  (and (>= (length server-hello-payload) 34)
       (let ((random (subseq server-hello-payload 2 34)))
         (equalp random +hello-retry-request-magic+))))

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
                    (push ticket (tls-connection-received-tickets conn))))
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
  (cipher-suites (list +tls-aes-128-gcm-sha256+
                       +tls-aes-256-gcm-sha384+
                       +tls-chacha20-poly1305-sha256+)
                 :type list))

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
  (early-data-p nil)
  (quic-transport-params nil))

(defun parse-client-hello (payload)
  "Parse a ClientHello handshake message. Returns a parsed-client-hello struct."
  (let ((pos 0)
        (result (make-parsed-client-hello)))
    ;; client_version (2 bytes, legacy)
    (incf pos 2)
    ;; random (32 bytes)
    (setf (parsed-client-hello-random result) (subseq payload pos (+ pos 32)))
    (incf pos 32)
    ;; session_id
    (multiple-value-bind (sid-len new-pos) (read-u8 payload pos)
      (setf pos new-pos)
      (setf (parsed-client-hello-session-id result) (subseq payload pos (+ pos sid-len)))
      (incf pos sid-len))
    ;; cipher_suites
    (multiple-value-bind (suites-len new-pos) (read-u16 payload pos)
      (setf pos new-pos)
      (let ((end (+ pos suites-len)))
        (loop while (< pos end)
              do (multiple-value-bind (suite new-pos2) (read-u16 payload pos)
                   (setf pos new-pos2)
                   (push suite (parsed-client-hello-cipher-suites result))))
        (setf (parsed-client-hello-cipher-suites result)
              (nreverse (parsed-client-hello-cipher-suites result)))))
    ;; compression_methods (skip)
    (multiple-value-bind (cm-len new-pos) (read-u8 payload pos)
      (setf pos new-pos)
      (incf pos cm-len))
    ;; Extensions
    (when (< pos (length payload))
      (multiple-value-bind (ext-len new-pos) (read-u16 payload pos)
        (setf pos new-pos)
        (let ((ext-end (+ pos ext-len)))
          (loop while (< pos ext-end)
                do (multiple-value-bind (etype new-pos2) (read-u16 payload pos)
                     (setf pos new-pos2)
                     (multiple-value-bind (elen new-pos3) (read-u16 payload pos)
                       (setf pos new-pos3)
                       (let ((ext-data (subseq payload pos (+ pos elen))))
                         (parse-client-hello-extension result etype ext-data))
                       (incf pos elen)))))))
    result))

(defun parse-client-hello-extension (ch ext-type data)
  "Parse a single ClientHello extension into the parsed-client-hello struct."
  (cond
    ;; SNI
    ((= ext-type +ext-server-name+)
     (let ((pos 0))
       (multiple-value-bind (list-len new-pos) (read-u16 data pos)
         (declare (ignore list-len))
         (setf pos new-pos)
         (multiple-value-bind (name-type new-pos2) (read-u8 data pos)
           (setf pos new-pos2)
           (when (zerop name-type) ; host_name
             (multiple-value-bind (name-len new-pos3) (read-u16 data pos)
               (setf pos new-pos3)
               (setf (parsed-client-hello-hostname ch)
                     (map 'string #'code-char (subseq data pos (+ pos name-len))))))))))
    ;; supported_versions
    ((= ext-type +ext-supported-versions+)
     (let ((pos 0))
       (multiple-value-bind (list-len new-pos) (read-u8 data pos)
         (declare (ignore list-len))
         (setf pos new-pos)
         (loop while (< pos (length data))
               do (multiple-value-bind (ver new-pos2) (read-u16 data pos)
                    (setf pos new-pos2)
                    (push ver (parsed-client-hello-supported-versions ch))))
         (setf (parsed-client-hello-supported-versions ch)
               (nreverse (parsed-client-hello-supported-versions ch))))))
    ;; key_share
    ((= ext-type +ext-key-share+)
     (let ((pos 0))
       (multiple-value-bind (list-len new-pos) (read-u16 data pos)
         (declare (ignore list-len))
         (setf pos new-pos)
         (loop while (< pos (length data))
               do (multiple-value-bind (group new-pos2) (read-u16 data pos)
                    (setf pos new-pos2)
                    (multiple-value-bind (klen new-pos3) (read-u16 data pos)
                      (setf pos new-pos3)
                      (push (cons group (subseq data pos (+ pos klen)))
                            (parsed-client-hello-key-shares ch))
                      (incf pos klen))))
         (setf (parsed-client-hello-key-shares ch)
               (nreverse (parsed-client-hello-key-shares ch))))))
    ;; signature_algorithms
    ((= ext-type +ext-signature-algorithms+)
     (let ((pos 0))
       (multiple-value-bind (list-len new-pos) (read-u16 data pos)
         (declare (ignore list-len))
         (setf pos new-pos)
         (loop while (< pos (length data))
               do (multiple-value-bind (sig new-pos2) (read-u16 data pos)
                    (setf pos new-pos2)
                    (push sig (parsed-client-hello-signature-algorithms ch))))
         (setf (parsed-client-hello-signature-algorithms ch)
               (nreverse (parsed-client-hello-signature-algorithms ch))))))
    ;; ALPN
    ((= ext-type +ext-alpn+)
     (let ((pos 0))
       (multiple-value-bind (list-len new-pos) (read-u16 data pos)
         (declare (ignore list-len))
         (setf pos new-pos)
         (loop while (< pos (length data))
               do (multiple-value-bind (plen new-pos2) (read-u8 data pos)
                    (setf pos new-pos2)
                    (push (map 'string #'code-char (subseq data pos (+ pos plen)))
                          (parsed-client-hello-alpn-protocols ch))
                    (incf pos plen)))
         (setf (parsed-client-hello-alpn-protocols ch)
               (nreverse (parsed-client-hello-alpn-protocols ch))))))
    ;; early_data
    ((= ext-type 42)
     (setf (parsed-client-hello-early-data-p ch) t))
    ;; QUIC transport parameters
    ((= ext-type +ext-quic-transport-parameters+)
     (setf (parsed-client-hello-quic-transport-params ch) data))
    ;; pre_shared_key
    ((= ext-type +ext-pre-shared-key+)
     (let ((pos 0))
       ;; identities
       (multiple-value-bind (id-len new-pos) (read-u16 data pos)
         (setf pos new-pos)
         (let ((id-end (+ pos id-len)))
           (loop while (< pos id-end)
                 do (multiple-value-bind (tlen new-pos2) (read-u16 data pos)
                      (setf pos new-pos2)
                      (let ((ticket (subseq data pos (+ pos tlen))))
                        (incf pos tlen)
                        (multiple-value-bind (age new-pos3) (read-u32 data pos)
                          (setf pos new-pos3)
                          (push (cons ticket age)
                                (parsed-client-hello-psk-identities ch)))))))
         (setf (parsed-client-hello-psk-identities ch)
               (nreverse (parsed-client-hello-psk-identities ch))))
       ;; binders
       (multiple-value-bind (bind-len new-pos) (read-u16 data pos)
         (declare (ignore bind-len))
         (setf pos new-pos)
         (loop while (< pos (length data))
               do (multiple-value-bind (blen new-pos2) (read-u8 data pos)
                    (setf pos new-pos2)
                    (push (subseq data pos (+ pos blen))
                          (parsed-client-hello-psk-binders ch))
                    (incf pos blen)))
         (setf (parsed-client-hello-psk-binders ch)
               (nreverse (parsed-client-hello-psk-binders ch))))))
    ;; Other extensions - ignore
    (t nil)))

(defun tls-server-start-handshake (conn client-hello-data)
  "Process a ClientHello and generate the server's response messages.
   CONN must have role :server and a server-config set.
   Returns the concatenated record bytes to send (ServerHello + encrypted handshake)."
  (let* ((config (tls-connection-server-config conn))
         (ch (parse-client-hello client-hello-data))
         ;; Negotiate cipher suite
         (suite (negotiate-cipher-suite (parsed-client-hello-cipher-suites ch)
                                        (tls-server-config-cipher-suites config)))
         ;; Select key share group
         (client-key-shares (parsed-client-hello-key-shares ch))
         (x25519-share (find +group-x25519+ client-key-shares :key #'car))
         ;; Initialize key schedule
         (ks (make-tls13-key-schedule :cipher-suite suite)))
    (unless suite
      (error "No common cipher suite"))
    (unless x25519-share
      (error "No supported key share group"))
    (setf (tls-connection-key-schedule conn) ks)
    (setf (tls-connection-cipher-suite conn) suite)
    ;; Add ClientHello to transcript
    (let ((ch-msg (make-handshake-message +handshake-client-hello+ client-hello-data)))
      (transcript-update ks ch-msg))
    ;; Generate server X25519 key pair
    (let* ((server-sk (drbg:random-bytes 32))
           (server-pk (x25519:x25519-base server-sk))
           (client-pk (cdr x25519-share))
           (shared-secret (x25519:x25519 server-sk client-pk)))
      ;; Derive secrets
      (derive-early-secret ks)
      (derive-handshake-secret ks shared-secret)
      ;; Build ServerHello
      (let* ((server-random (drbg:random-bytes 32))
             (session-id (parsed-client-hello-session-id ch))
             (sh-payload (build-server-hello-payload server-random session-id
                                                     suite server-pk))
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
          ;; Certificate
          (let ((cert-msg (build-certificate-message
                           (tls-server-config-certificate-chain config))))
            (transcript-update ks cert-msg)
            (buf-append-bytes encrypted-msgs cert-msg))
          ;; CertificateVerify
          (let ((cv-msg (build-certificate-verify conn config)))
            (transcript-update ks cv-msg)
            (buf-append-bytes encrypted-msgs cv-msg))
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
            (setf (tls-connection-state conn) :wait-finished)
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
                result))))))))

(defun negotiate-cipher-suite (client-suites server-suites)
  "Select the best cipher suite supported by both client and server."
  (dolist (suite server-suites)
    (when (member suite client-suites)
      (return suite))))

(defun build-server-hello-payload (server-random session-id cipher-suite server-pk)
  "Build the ServerHello payload (without handshake header)."
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
        (buf-append-u16 ks-buf +group-x25519+)
        (buf-append-u16-prefixed ks-buf server-pk)
        (buf-append-u16-prefixed ext-buf
                                 (buf-freeze ks-buf)))
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

(defun build-certificate-message (cert-chain)
  "Build a Certificate handshake message from a list of DER-encoded certificates."
  (let ((buf (make-buffer)))
    ;; certificate_request_context (empty for server)
    (buf-append-byte buf 0)
    ;; certificate_list
    (let ((list-buf (make-buffer)))
      (dolist (cert-der cert-chain)
        ;; cert_data
        (buf-append-u24 list-buf (length cert-der))
        (buf-append-bytes list-buf cert-der)
        ;; extensions (empty)
        (buf-append-u16 list-buf 0))
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

(defun tls-server-process-finished (conn ciphertext)
  "Process the client's Finished message on the server side.
   Returns T if successful."
  (multiple-value-bind (plaintext inner-ct)
      (decrypt-record ciphertext
                      (tls-connection-client-handshake-key conn)
                      (tls-connection-client-handshake-iv conn)
                      (tls-connection-client-seq conn)
                      (tls-connection-cipher-suite conn))
    (incf (tls-connection-client-seq conn))
    (unless (= inner-ct +content-handshake+)
      (error "Expected handshake, got content-type ~D" inner-ct))
    ;; Parse the Finished message
    (multiple-value-bind (type payload) (parse-handshake-header plaintext)
      (unless (= type +handshake-finished+)
        (error "Expected Finished message, got type ~D" type))
      (let* ((verify-data (parse-finished payload))
             (ks (tls-connection-key-schedule conn))
             (transcript-hash (transcript-current-hash ks))
             (finished-key (derive-finished-key ks (tls-connection-client-hs-secret conn)))
             (expected (hmac:hmac (tls13-key-schedule-hash-fn ks)
                                  finished-key transcript-hash)))
        (unless (equalp verify-data expected)
          (error "Client Finished verification failed"))
        ;; Reset sequence numbers for application traffic
        (setf (tls-connection-client-seq conn) 0)
        (setf (tls-connection-server-seq conn) 0)
        ;; Connection established
        (setf (tls-connection-state conn) :connected)
        t))))

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
  (write-sequence buffer (fd-transport-stream transport)
                  :start start :end end)
  (force-output (fd-transport-stream transport))
  (- end start))

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

(defun tls-connect (transport &key hostname trust-store alpn-protocols)
  "Establish a TLS 1.3 connection as a client.

   Performs the full handshake over TRANSPORT and returns a TLS-STREAM
   ready for application data exchange.

   Parameters:
     TRANSPORT: Object implementing TLS-TRANSPORT-READ/WRITE/CLOSE
     HOSTNAME (string): Server hostname for SNI and certificate verification
     TRUST-STORE (list): List of trusted CA certificates (DER byte arrays)
     ALPN-PROTOCOLS (list): ALPN protocol strings to advertise

   Returns:
     TLS-STREAM with established connection

   Example:
     (let ((stream (tls-connect transport :hostname \"example.com\")))
       (tls-write stream (string-to-bytes \"GET / HTTP/1.1\\r\\n\"))
       (tls-read stream buffer))"
  (let ((conn (make-tls-connection :hostname hostname
                                    :trust-store trust-store
                                    :alpn-protocols alpn-protocols)))
    ;; 1. Send ClientHello
    (let ((ch-record (tls-start-handshake conn)))
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
      ;; 2. Process ClientHello and generate server response
      (let ((response (tls-server-start-handshake conn ch-payload)))
        (write-bytes-to-transport transport response)))
    ;; 3. Read client Finished
    (loop until (eq (tls-connection-state conn) :connected)
          do (let ((record-bytes (read-tls-record-from-transport transport)))
               (unless record-bytes
                 (error "Client closed connection during handshake"))
               ;; The client sends its Finished encrypted
               (let ((record (parse-tls-record record-bytes)))
                 (cond
                   ((= (tls-record-content-type record) +content-change-cipher-spec+)
                    nil)  ; ignore CCS
                   ((= (tls-record-content-type record) +content-application-data+)
                    (tls-server-process-finished conn (tls-record-data record)))
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
