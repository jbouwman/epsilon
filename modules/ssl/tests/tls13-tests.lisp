;;;; Tests for TLS 1.3 Protocol

(defpackage epsilon.ssl.tls13-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:tls #:epsilon.ssl.tls13)
   (#:sha256 #:epsilon.ssl.sha256)
   (#:hkdf #:epsilon.ssl.hkdf)
   (#:aes-gcm #:epsilon.ssl.aes-gcm)
   (#:chacha #:epsilon.ssl.chacha20-poly1305)
   (#:x25519 #:epsilon.ssl.curve25519)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:net #:epsilon.net)
   (#:x509 #:epsilon.ssl.x509)
   (#:drbg #:epsilon.ssl.drbg))
  (:enter t))

(in-package :epsilon.ssl.tls13-tests)

;;; ---------------------------------------------------------------------------
;;; Key schedule tests
;;; ---------------------------------------------------------------------------

(deftest test-tls13-key-schedule-creation
  "TLS 1.3 key schedule creation for each cipher suite"
  (let ((ks256 (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-128-gcm-sha256+)))
    (assert-= (tls::tls13-key-schedule-hash-len ks256) 32))
  (let ((ks384 (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-256-gcm-sha384+)))
    (assert-= (tls::tls13-key-schedule-hash-len ks384) 48))
  (let ((kschacha (tls:make-tls13-key-schedule :cipher-suite tls:+tls-chacha20-poly1305-sha256+)))
    (assert-= (tls::tls13-key-schedule-hash-len kschacha) 32)))

(deftest test-tls13-early-secret
  "TLS 1.3 Early Secret derivation (no PSK)"
  (let ((ks (tls:make-tls13-key-schedule)))
    (let ((early (tls:derive-early-secret ks)))
      (assert-= (length early) 32)
      ;; Early secret should be deterministic with no PSK
      (let ((ks2 (tls:make-tls13-key-schedule)))
        (assert-equalp (tls:derive-early-secret ks2) early)))))

(deftest test-tls13-handshake-secret
  "TLS 1.3 Handshake Secret derivation"
  (let ((ks (tls:make-tls13-key-schedule))
        (shared-secret (drbg:random-bytes 32)))
    (tls:derive-early-secret ks)
    (let ((hs-secret (tls:derive-handshake-secret ks shared-secret)))
      (assert-= (length hs-secret) 32)
      ;; Should be non-zero
      (assert-true (some #'plusp hs-secret)))))

(deftest test-tls13-master-secret
  "TLS 1.3 Master Secret derivation"
  (let ((ks (tls:make-tls13-key-schedule))
        (shared-secret (drbg:random-bytes 32)))
    (tls:derive-early-secret ks)
    (tls:derive-handshake-secret ks shared-secret)
    (let ((master (tls:derive-master-secret ks)))
      (assert-= (length master) 32)
      (assert-true (some #'plusp master)))))

(deftest test-tls13-traffic-keys
  "TLS 1.3 traffic key derivation"
  (let ((ks (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-128-gcm-sha256+))
        (shared-secret (drbg:random-bytes 32)))
    (tls:derive-early-secret ks)
    (tls:derive-handshake-secret ks shared-secret)
    (multiple-value-bind (c-key c-iv s-key s-iv) (tls:derive-handshake-traffic-keys ks)
      ;; AES-128 key = 16 bytes, IV = 12 bytes
      (assert-= (length c-key) 16)
      (assert-= (length c-iv) 12)
      (assert-= (length s-key) 16)
      (assert-= (length s-iv) 12)
      ;; Client and server keys should be different
      (assert-not (equalp c-key s-key))
      (assert-not (equalp c-iv s-iv)))))

(deftest test-tls13-traffic-keys-aes256
  "TLS 1.3 traffic keys for AES-256-GCM"
  (let ((ks (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-256-gcm-sha384+))
        (shared-secret (drbg:random-bytes 48)))
    (tls:derive-early-secret ks)
    (tls:derive-handshake-secret ks shared-secret)
    (multiple-value-bind (c-key c-iv s-key s-iv) (tls:derive-handshake-traffic-keys ks)
      ;; AES-256 key = 32 bytes, IV = 12 bytes
      (assert-= (length c-key) 32)
      (assert-= (length c-iv) 12)
      (assert-= (length s-key) 32)
      (assert-= (length s-iv) 12))))

;;; ---------------------------------------------------------------------------
;;; Record layer tests
;;; ---------------------------------------------------------------------------

(deftest test-tls-record-serialize-parse-roundtrip
  "TLS record serialize/parse round-trip"
  (let* ((data (make-array 10 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
         (record (tls:make-tls-record :content-type tls:+content-handshake+
                                     :data data))
         (bytes (tls::serialize-tls-record record)))
    ;; Header: 1 byte content-type + 2 bytes version + 2 bytes length = 5
    (assert-= (length bytes) 15)
    (assert-= (aref bytes 0) tls:+content-handshake+)
    ;; Parse back
    (multiple-value-bind (parsed next-pos) (tls:parse-tls-record bytes)
      (assert-true parsed)
      (assert-= next-pos 15)
      (assert-= (tls:tls-record-content-type parsed) tls:+content-handshake+)
      (assert-equalp (tls:tls-record-data parsed) data))))

(deftest test-tls-record-parse-insufficient-data
  "TLS record parse returns NIL for insufficient data"
  (let ((short (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-nil (tls:parse-tls-record short))))

;;; ---------------------------------------------------------------------------
;;; Record encryption/decryption tests
;;; ---------------------------------------------------------------------------

(deftest test-record-encrypt-decrypt-aes128
  "Record encryption/decryption with AES-128-GCM"
  (let ((key (drbg:random-bytes 16))
        (iv (drbg:random-bytes 12))
        (plaintext (make-array 20 :element-type '(unsigned-byte 8) :initial-element #xAA)))
    (let ((ciphertext (tls:encrypt-record plaintext tls:+content-application-data+
                                          key iv 0 tls:+tls-aes-128-gcm-sha256+)))
      (assert-true (> (length ciphertext) (length plaintext)))
      ;; Decrypt
      (multiple-value-bind (decrypted inner-ct)
          (tls:decrypt-record ciphertext key iv 0 tls:+tls-aes-128-gcm-sha256+)
        (assert-= inner-ct tls:+content-application-data+)
        (assert-equalp decrypted plaintext)))))

(deftest test-record-encrypt-decrypt-chacha20
  "Record encryption/decryption with ChaCha20-Poly1305"
  (let ((key (drbg:random-bytes 32))
        (iv (drbg:random-bytes 12))
        (plaintext (make-array 50 :element-type '(unsigned-byte 8) :initial-element #xBB)))
    (let ((ciphertext (tls:encrypt-record plaintext tls:+content-handshake+
                                          key iv 42 tls:+tls-chacha20-poly1305-sha256+)))
      (multiple-value-bind (decrypted inner-ct)
          (tls:decrypt-record ciphertext key iv 42 tls:+tls-chacha20-poly1305-sha256+)
        (assert-= inner-ct tls:+content-handshake+)
        (assert-equalp decrypted plaintext)))))

(deftest test-record-encrypt-decrypt-seq-num
  "Record encryption with different sequence numbers produces different ciphertext"
  (let ((key (drbg:random-bytes 16))
        (iv (drbg:random-bytes 12))
        (plaintext (make-array 10 :element-type '(unsigned-byte 8) :initial-element #xCC)))
    (let ((ct0 (tls:encrypt-record plaintext tls:+content-application-data+
                                   key iv 0 tls:+tls-aes-128-gcm-sha256+))
          (ct1 (tls:encrypt-record plaintext tls:+content-application-data+
                                   key iv 1 tls:+tls-aes-128-gcm-sha256+)))
      (assert-not (equalp ct0 ct1)))))

(deftest test-record-decrypt-wrong-key-fails
  "Record decryption with wrong key fails"
  (let ((key1 (drbg:random-bytes 16))
        (key2 (drbg:random-bytes 16))
        (iv (drbg:random-bytes 12))
        (plaintext (make-array 10 :element-type '(unsigned-byte 8) :initial-element #xDD)))
    (let ((ciphertext (tls:encrypt-record plaintext tls:+content-application-data+
                                          key1 iv 0 tls:+tls-aes-128-gcm-sha256+)))
      ;; Should error on decryption with wrong key
      (let ((failed nil))
        (handler-case
            (tls:decrypt-record ciphertext key2 iv 0 tls:+tls-aes-128-gcm-sha256+)
          (error () (setf failed t)))
        (assert-true failed)))))

;;; ---------------------------------------------------------------------------
;;; Nonce computation tests
;;; ---------------------------------------------------------------------------

(deftest test-nonce-computation
  "Per-record nonce computation"
  (let ((iv (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; seq=0 should return iv unchanged
    (let ((nonce0 (tls::compute-nonce iv 0)))
      (assert-equalp nonce0 iv))
    ;; seq=1 should XOR 1 into the last byte
    (let ((nonce1 (tls::compute-nonce iv 1)))
      (assert-= (aref nonce1 11) 1))
    ;; seq=256 should XOR into second-to-last byte
    (let ((nonce256 (tls::compute-nonce iv 256)))
      (assert-= (aref nonce256 10) 1)
      (assert-= (aref nonce256 11) 0))))

;;; ---------------------------------------------------------------------------
;;; Handshake message construction
;;; ---------------------------------------------------------------------------

(deftest test-handshake-message-header
  "Handshake message construction/parsing"
  (let* ((payload (make-array 5 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4 5)))
         (msg (tls::make-handshake-message tls:+handshake-client-hello+ payload)))
    ;; 4 bytes header + 5 bytes payload
    (assert-= (length msg) 9)
    (assert-= (aref msg 0) tls:+handshake-client-hello+)
    ;; Parse back
    (multiple-value-bind (type parsed-payload next-pos) (tls::parse-handshake-header msg)
      (assert-= type tls:+handshake-client-hello+)
      (assert-equalp parsed-payload payload)
      (assert-= next-pos 9))))

;;; ---------------------------------------------------------------------------
;;; ClientHello construction
;;; ---------------------------------------------------------------------------

(deftest test-build-client-hello
  "ClientHello construction"
  (let* ((sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch (tls:build-client-hello
              :hostname "example.com"
              :key-shares (list (list :x25519 pk)))))
    ;; Should be a handshake message with type 1
    (assert-= (aref ch 0) tls:+handshake-client-hello+)
    ;; Should be > 100 bytes (has extensions)
    (assert-true (> (length ch) 100))))

(deftest test-build-client-hello-with-alpn
  "ClientHello with ALPN extension"
  (let* ((sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch (tls:build-client-hello
              :hostname "example.com"
              :key-shares (list (list :x25519 pk))
              :alpn '("h2" "http/1.1"))))
    (assert-= (aref ch 0) tls:+handshake-client-hello+)
    (assert-true (> (length ch) 100))))

;;; ---------------------------------------------------------------------------
;;; TLS Connection state machine
;;; ---------------------------------------------------------------------------

(deftest test-tls-connection-creation
  "TLS connection creation"
  (let ((conn (tls:make-tls-connection :hostname "example.com")))
    (assert-eq (tls:tls-connection-state conn) :start)))

(deftest test-tls-start-handshake
  "TLS handshake start produces ClientHello record"
  (let* ((conn (tls:make-tls-connection :hostname "test.example.com"))
         (record-bytes (tls::tls-start-handshake conn)))
    ;; Should be a TLS record
    (assert-= (aref record-bytes 0) tls:+content-handshake+)
    ;; State should advance
    (assert-eq (tls:tls-connection-state conn) :wait-server-hello)
    ;; Should have generated key shares
    (assert-true (tls::tls-connection-client-x25519-public conn))))

;;; ---------------------------------------------------------------------------
;;; Finished message
;;; ---------------------------------------------------------------------------

(deftest test-finished-verify-data
  "Finished message verify_data computation"
  (let ((ks (tls:make-tls13-key-schedule))
        (shared-secret (drbg:random-bytes 32)))
    ;; Set up key schedule
    (tls:derive-early-secret ks)
    (tls:derive-handshake-secret ks shared-secret)
    (let ((hs-secret (tls::tls13-key-schedule-handshake-secret ks)))
      ;; Build a Finished message
      (let* ((c-hs-secret (tls::ks-derive-secret ks hs-secret "c hs traffic"))
             (finished-msg (tls:build-finished ks c-hs-secret)))
        ;; Should be a handshake message type 20
        (assert-= (aref finished-msg 0) tls:+handshake-finished+)
        ;; Payload should be hash-length bytes
        (let ((payload-len (logior (ash (aref finished-msg 1) 16)
                                   (ash (aref finished-msg 2) 8)
                                   (aref finished-msg 3))))
          (assert-= payload-len 32))))))

;;; ---------------------------------------------------------------------------
;;; Alert protocol tests
;;; ---------------------------------------------------------------------------

(deftest test-alert-record-construction
  "Alert record construction"
  (let ((record (tls:make-alert-record 2 tls:+alert-handshake-failure+)))
    ;; Should be a TLS record with content type alert
    (assert-= (aref record 0) tls:+content-alert+)
    ;; Parse the record
    (let ((parsed (tls:parse-tls-record record)))
      (assert-true parsed)
      (assert-= (tls:tls-record-content-type parsed) tls:+content-alert+)
      ;; Alert data: 2 bytes (level + description)
      (let ((data (tls:tls-record-data parsed)))
        (assert-= (length data) 2)
        (assert-= (aref data 0) 2)
        (assert-= (aref data 1) tls:+alert-handshake-failure+)))))

(deftest test-parse-alert
  "Alert message parsing"
  (let* ((data (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents (list 1 tls:+alert-close-notify+)))
         (alert (tls:parse-alert data)))
    (assert-= (tls:tls-alert-level alert) 1)
    (assert-= (tls:tls-alert-description alert) tls:+alert-close-notify+)))

(deftest test-close-notify
  "close_notify transitions to :closed state"
  (let ((conn (tls:make-tls-connection :hostname "example.com")))
    ;; close_notify from :start state (unencrypted)
    (let ((record (tls:tls-close-notify conn)))
      (assert-true (> (length record) 0))
      (assert-eq (tls:tls-connection-state conn) :closed))))

;;; ---------------------------------------------------------------------------
;;; PSK / session ticket tests
;;; ---------------------------------------------------------------------------

(deftest test-session-ticket-creation
  "Session ticket struct creation"
  (let ((ticket (tls:make-tls-session-ticket
                 :ticket (drbg:random-bytes 16)
                 :lifetime 3600
                 :age-add 12345
                 :nonce (drbg:random-bytes 8)
                 :cipher-suite tls:+tls-aes-128-gcm-sha256+
                 :resumption-secret (drbg:random-bytes 32)
                 :creation-time (get-universal-time))))
    (assert-= (tls:tls-session-ticket-lifetime ticket) 3600)
    (assert-= (tls:tls-session-ticket-age-add ticket) 12345)
    (assert-= (tls:tls-session-ticket-cipher-suite ticket) tls:+tls-aes-128-gcm-sha256+)))

(deftest test-psk-binder-computation
  "PSK binder is deterministic and correct length"
  (let* ((ks (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-128-gcm-sha256+))
         (psk (drbg:random-bytes 32))
         (partial-ch (drbg:random-bytes 200))
         (binder (tls::compute-psk-binder ks psk partial-ch)))
    ;; Binder should be hash-length bytes (32 for SHA-256)
    (assert-= (length binder) 32)
    ;; Same inputs should give same binder
    (let ((binder2 (tls::compute-psk-binder ks psk partial-ch)))
      (assert-equalp binder binder2))))

(deftest test-psk-resumption-key-derivation
  "PSK derivation from session ticket"
  (let* ((ks (tls:make-tls13-key-schedule :cipher-suite tls:+tls-aes-128-gcm-sha256+))
         (ticket (tls:make-tls-session-ticket
                  :ticket (drbg:random-bytes 16)
                  :nonce (drbg:random-bytes 8)
                  :cipher-suite tls:+tls-aes-128-gcm-sha256+
                  :resumption-secret (drbg:random-bytes 32)))
         (psk (tls::derive-resumption-psk ks ticket)))
    ;; PSK should be hash-length bytes
    (assert-= (length psk) 32)
    ;; Should be deterministic
    (let ((psk2 (tls::derive-resumption-psk ks ticket)))
      (assert-equalp psk psk2))))

;;; ---------------------------------------------------------------------------
;;; ClientHello parsing (server side) tests
;;; ---------------------------------------------------------------------------

(deftest test-parse-client-hello
  "Parse a ClientHello message"
  ;; Build a ClientHello first, then parse it
  (let* ((sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch-msg (tls:build-client-hello
                  :hostname "test.example.com"
                  :key-shares (list (list :x25519 pk))
                  :alpn '("h2" "http/1.1"))))
    ;; The handshake message has a 4-byte header
    (let* ((payload (subseq ch-msg 4))
           (parsed (tls:parse-client-hello payload)))
      ;; Should have parsed cipher suites
      (assert-true (member tls:+tls-aes-128-gcm-sha256+
                           (tls:parsed-client-hello-cipher-suites parsed)))
      ;; Should have SNI
      (assert-equal (tls:parsed-client-hello-hostname parsed) "test.example.com")
      ;; Should have key shares
      (assert-true (tls:parsed-client-hello-key-shares parsed))
      ;; Should have ALPN
      (assert-true (member "h2" (tls:parsed-client-hello-alpn-protocols parsed)
                           :test #'string=))
      ;; Should have random
      (assert-= (length (tls:parsed-client-hello-random parsed)) 32))))

;;; ---------------------------------------------------------------------------
;;; Server-side handshake tests
;;; ---------------------------------------------------------------------------

(deftest test-server-handshake-basic
  "Server-side TLS handshake produces valid response"
  ;; Generate Ed25519 key pair for server
  (let* ((server-sk (drbg:random-bytes 32))
         (server-pk (ed-sign:ed25519-public-key-from-private server-sk))
         ;; Create a self-signed certificate (returns DER bytes directly)
         (cert-der (x509:make-self-signed-certificate
                    :key-type :ed25519
                    :private-key server-sk
                    :public-key-bytes server-pk
                    :subject "localhost"
                    :dns-names '("localhost")))
         ;; Server config
         (config (tls:make-tls-server-config
                  :certificate-chain (list cert-der)
                  :private-key server-sk
                  :key-type :ed25519
                  :alpn-protocols '("h2" "http/1.1")))
         ;; Create server connection
         (server-conn (tls:make-tls-connection :role :server))
         (_ (setf (tls::tls-connection-server-config server-conn) config))
         ;; Create client connection and get ClientHello
         (client-conn (tls:make-tls-connection :hostname "localhost"))
         (ch-record (tls::tls-start-handshake client-conn)))
    (declare (ignore _))
    ;; Parse the ClientHello record to get the handshake payload
    (let* ((record (tls:parse-tls-record ch-record))
           (ch-data (tls:tls-record-data record))
           ;; Extract handshake payload (skip 4-byte handshake header)
           (ch-payload (subseq ch-data 4)))
      ;; Process on server side
      (let ((response (tls:tls-server-start-handshake server-conn ch-payload)))
        ;; Should produce response bytes
        (assert-true (> (length response) 50))
        ;; Server should be waiting for client Finished
        (assert-eq (tls:tls-connection-state server-conn) :wait-finished)))))

;;; ---------------------------------------------------------------------------
;;; HRR detection test
;;; ---------------------------------------------------------------------------

(deftest test-hello-retry-request-detection
  "HelloRetryRequest is detected by magic random value"
  ;; Build a fake ServerHello payload with HRR magic random
  (let ((payload (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Version at offset 0-1
    (setf (aref payload 0) #x03)
    (setf (aref payload 1) #x03)
    ;; Copy HRR magic at offset 2-33
    (replace payload tls:+hello-retry-request-magic+ :start1 2)
    (assert-true (tls::hello-retry-request-p payload)))
  ;; Normal ServerHello should not be detected
  (let ((payload (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref payload 0) #x03)
    (setf (aref payload 1) #x03)
    ;; Random data
    (replace payload (drbg:random-bytes 32) :start1 2)
    (assert-not (tls::hello-retry-request-p payload))))

;;; ---------------------------------------------------------------------------
;;; Cipher suite negotiation
;;; ---------------------------------------------------------------------------

(deftest test-cipher-suite-negotiation
  "Cipher suite negotiation selects server preference"
  ;; Server prefers ChaCha20, client offers AES-128 first
  (let ((result (tls::negotiate-cipher-suite
                 (list tls:+tls-aes-128-gcm-sha256+ tls:+tls-chacha20-poly1305-sha256+)
                 (list tls:+tls-chacha20-poly1305-sha256+ tls:+tls-aes-128-gcm-sha256+))))
    ;; Should pick server's preference (ChaCha20)
    (assert-= result tls:+tls-chacha20-poly1305-sha256+))
  ;; No overlap returns nil
  (let ((result (tls::negotiate-cipher-suite
                 (list tls:+tls-aes-128-gcm-sha256+)
                 (list tls:+tls-aes-256-gcm-sha384+))))
    (assert-nil result)))

;;; ---------------------------------------------------------------------------
;;; Early secret with PSK
;;; ---------------------------------------------------------------------------

(deftest test-early-secret-with-psk
  "Early secret derivation with PSK differs from no-PSK"
  (let* ((ks1 (tls:make-tls13-key-schedule))
         (ks2 (tls:make-tls13-key-schedule))
         (psk (drbg:random-bytes 32))
         (early1 (tls:derive-early-secret ks1))
         (early2 (tls:derive-early-secret ks2 psk)))
    ;; Both should be 32 bytes
    (assert-= (length early1) 32)
    (assert-= (length early2) 32)
    ;; But they should differ
    (assert-not (equalp early1 early2))))

;;; ---------------------------------------------------------------------------
;;; Memory transport tests
;;; ---------------------------------------------------------------------------

(deftest test-memory-transport-basic
  "Memory transport reads and writes correctly"
  (let ((mt (tls::make-memory-transport)))
    ;; Feed data
    (tls::memory-transport-feed mt #(1 2 3 4 5))
    ;; Read partial
    (let ((buf (make-array 3 :element-type '(unsigned-byte 8))))
      (let ((n (tls:tls-transport-read mt buf 0 3)))
        (assert-= n 3)
        (assert-equalp buf #(1 2 3))))
    ;; Read rest
    (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
      (let ((n (tls:tls-transport-read mt buf 0 5)))
        (assert-= n 2)
        (assert-= (aref buf 0) 4)
        (assert-= (aref buf 1) 5)))
    ;; Write
    (let ((data #(10 20 30)))
      (let ((n (tls:tls-transport-write mt data 0 3)))
        (assert-= n 3)))
    (let ((out (tls::memory-transport-get-output mt)))
      (assert-= (length out) 3)
      (assert-equalp out #(10 20 30)))))

(deftest test-memory-transport-feed-appends
  "Memory transport feed appends to remaining unread data"
  (let ((mt (tls::make-memory-transport)))
    (tls::memory-transport-feed mt #(1 2 3))
    ;; Read 1 byte
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
      (tls:tls-transport-read mt buf 0 1))
    ;; Feed more -- should keep the remaining 2,3 and add 4,5
    (tls::memory-transport-feed mt #(4 5))
    (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
      (let ((n (tls:tls-transport-read mt buf 0 4)))
        (assert-= n 4)
        (assert-equalp buf #(2 3 4 5))))))

;;; ---------------------------------------------------------------------------
;;; TLS Stream structure tests
;;; ---------------------------------------------------------------------------

(deftest test-tls-stream-creation
  "TLS stream struct creation and accessors"
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         (mt (tls::make-memory-transport))
         (stream (tls:make-tls-stream :connection conn :transport mt)))
    (assert-true (tls:tls-stream-p stream))
    (assert-eq (tls:tls-stream-connection stream) conn)
    (assert-not (tls:tls-stream-closed-p stream))))

(deftest test-transport-read-exact
  "transport-read-exact reads the specified number of bytes"
  (let ((mt (tls::make-memory-transport)))
    (tls::memory-transport-feed mt #(10 20 30 40 50))
    (let ((result (tls::transport-read-exact mt 3)))
      (assert-= (length result) 3)
      (assert-equalp result #(10 20 30)))
    (let ((result (tls::transport-read-exact mt 2)))
      (assert-= (length result) 2)
      (assert-equalp result #(40 50)))
    ;; EOF returns nil
    (assert-nil (tls::transport-read-exact mt 1))))

(deftest test-read-tls-record-from-transport
  "Reading a TLS record from transport parses the header correctly"
  (let ((mt (tls::make-memory-transport)))
    ;; Build a minimal TLS record: type=23 (app data), version=0x0303, length=3, data=abc
    (let ((record (make-array 8 :element-type '(unsigned-byte 8)
                              :initial-contents '(23 3 3 0 3 97 98 99))))
      (tls::memory-transport-feed mt record))
    (let ((result (tls::read-tls-record-from-transport mt)))
      (assert-= (length result) 8)
      (assert-= (aref result 0) 23)
      (assert-= (aref result 5) 97))))

;;; ---------------------------------------------------------------------------
;;; String I/O helpers
;;; ---------------------------------------------------------------------------

(deftest test-string-to-bytes-roundtrip
  "String/bytes conversion round-trips correctly"
  (let* ((str "Hello, TLS!")
         (bytes (tls::string-to-bytes str))
         (back (tls::bytes-to-string bytes)))
    (assert-equal str back)
    (assert-= (length bytes) (length str))
    (assert-= (aref bytes 0) 72)))   ; H

;;; ---------------------------------------------------------------------------
;;; Role-aware key selection tests
;;; ---------------------------------------------------------------------------

(deftest test-connection-role-key-selection
  "Write/read keys swap based on connection role"
  (let ((client-conn (tls:make-tls-connection :role :client))
        (server-conn (tls:make-tls-connection :role :server)))
    ;; Set up dummy keys
    (let ((ck (make-array 16 :element-type '(unsigned-byte 8) :initial-element 1))
          (ci (make-array 12 :element-type '(unsigned-byte 8) :initial-element 2))
          (sk (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3))
          (si (make-array 12 :element-type '(unsigned-byte 8) :initial-element 4)))
      (dolist (conn (list client-conn server-conn))
        (setf (tls::tls-connection-client-app-key conn) ck)
        (setf (tls::tls-connection-client-app-iv conn) ci)
        (setf (tls::tls-connection-server-app-key conn) sk)
        (setf (tls::tls-connection-server-app-iv conn) si)))
    ;; Client writes with client keys
    (multiple-value-bind (key iv seq) (tls::connection-write-keys client-conn)
      (declare (ignore seq))
      (assert-= (aref key 0) 1)
      (assert-= (aref iv 0) 2))
    ;; Client reads with server keys
    (multiple-value-bind (key iv seq) (tls::connection-read-keys client-conn)
      (declare (ignore seq))
      (assert-= (aref key 0) 3)
      (assert-= (aref iv 0) 4))
    ;; Server writes with server keys
    (multiple-value-bind (key iv seq) (tls::connection-write-keys server-conn)
      (declare (ignore seq))
      (assert-= (aref key 0) 3)
      (assert-= (aref iv 0) 4))
    ;; Server reads with client keys
    (multiple-value-bind (key iv seq) (tls::connection-read-keys server-conn)
      (declare (ignore seq))
      (assert-= (aref key 0) 1)
      (assert-= (aref iv 0) 2))))

;;; ===========================================================================
;;; Full end-to-end handshake tests
;;; ===========================================================================

(defun make-test-server-config ()
  "Create a server config with Ed25519 self-signed cert for testing."
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
     :alpn-protocols '("h2" "http/1.1"))))

(deftest test-handshake-key-agreement
  "Client and server derive matching handshake traffic keys"
  (let* ((config (make-test-server-config))
         (client (tls:make-tls-connection :hostname "localhost"))
         (server (tls:make-tls-connection :role :server)))
    (setf (tls::tls-connection-server-config server) config)
    ;; Client sends ClientHello
    (let* ((ch-record-bytes (tls::tls-start-handshake client))
           (ch-record (tls:parse-tls-record ch-record-bytes))
           (ch-data (tls:tls-record-data ch-record))
           (ch-payload (subseq ch-data 4)))
      ;; Server processes ClientHello and returns response
      (let ((server-response (tls:tls-server-start-handshake server ch-payload)))
        ;; Server has keys
        (assert-true (tls::tls-connection-server-handshake-key server))
        ;; Feed ServerHello record to client
        (let ((pos 0))
          ;; Parse first record (ServerHello)
          (let* ((record-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                                  (aref server-response (+ pos 4))))
                 (record-bytes (subseq server-response pos (+ pos record-len))))
            (tls:tls-handshake-step client record-bytes)
            ;; After processing ServerHello, client should have handshake keys
            (assert-true (tls::tls-connection-server-handshake-key client))
            ;; Compare server handshake keys
            (assert-equalp (tls::tls-connection-server-handshake-key client)
                           (tls::tls-connection-server-handshake-key server))
            (assert-equalp (tls::tls-connection-server-handshake-iv client)
                           (tls::tls-connection-server-handshake-iv server))
            (assert-equalp (tls::tls-connection-client-handshake-key client)
                           (tls::tls-connection-client-handshake-key server))
            (assert-equalp (tls::tls-connection-client-handshake-iv client)
                           (tls::tls-connection-client-handshake-iv server))))))))

(deftest test-handshake-decrypt-server-flight
  "Client can decrypt the server's encrypted handshake flight"
  (let* ((config (make-test-server-config))
         (client (tls:make-tls-connection :hostname "localhost"))
         (server (tls:make-tls-connection :role :server)))
    (setf (tls::tls-connection-server-config server) config)
    ;; Client sends ClientHello
    (let* ((ch-record-bytes (tls::tls-start-handshake client))
           (ch-record (tls:parse-tls-record ch-record-bytes))
           (ch-data (tls:tls-record-data ch-record))
           (ch-payload (subseq ch-data 4)))
      ;; Server processes ClientHello and returns response
      (let ((server-response (tls:tls-server-start-handshake server ch-payload)))
        ;; Feed ServerHello to client via tls-handshake-step
        (let ((pos 0))
          ;; First record: ServerHello
          (let* ((r1-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r1-bytes (subseq server-response pos (+ pos r1-len))))
            (tls:tls-handshake-step client r1-bytes)
            (setf pos (+ pos r1-len)))
          ;; Second record: CCS
          (let* ((r2-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r2-bytes (subseq server-response pos (+ pos r2-len))))
            (tls:tls-handshake-step client r2-bytes)
            (setf pos (+ pos r2-len)))
          ;; Verify seq number is still 0 (no encrypted records processed yet)
          (assert-= (tls::tls-connection-server-seq client) 0)
          ;; Third record: encrypted handshake
          (let* ((r3-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r3-bytes (subseq server-response pos (+ pos r3-len))))
            ;; This should decrypt and process the full encrypted flight
            (tls:tls-handshake-step client r3-bytes)
            ;; Client should now be connected
            (assert-eq (tls:tls-connection-state client) :connected)))))))

(deftest test-full-handshake-step-by-step
  "Full TLS 1.3 handshake: client <-> server record exchange"
  (let* ((config (make-test-server-config))
         ;; Create connections
         (client (tls:make-tls-connection :hostname "localhost"))
         (server (tls:make-tls-connection :role :server)))
    (setf (tls::tls-connection-server-config server) config)

    ;; Step 1: Client sends ClientHello
    (let ((ch-record-bytes (tls::tls-start-handshake client)))
      (assert-true (> (length ch-record-bytes) 50))
      (assert-eq (tls:tls-connection-state client) :wait-server-hello)

      ;; Step 2: Server processes ClientHello and produces response
      (let* ((ch-record (tls:parse-tls-record ch-record-bytes))
             (ch-data (tls:tls-record-data ch-record))
             (ch-payload (subseq ch-data 4))
             (server-response (tls:tls-server-start-handshake server ch-payload)))
        (assert-true (> (length server-response) 100))
        (assert-eq (tls:tls-connection-state server) :wait-finished)

        ;; Step 3: Client processes server response record-by-record
        ;; Use manual extraction like the passing test
        (let ((pos 0))
          ;; Record 1: ServerHello
          (let* ((r1-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r1-bytes (subseq server-response pos (+ pos r1-len))))
            (tls:tls-handshake-step client r1-bytes)
            (setf pos (+ pos r1-len)))
          ;; Record 2: CCS
          (let* ((r2-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r2-bytes (subseq server-response pos (+ pos r2-len))))
            (tls:tls-handshake-step client r2-bytes)
            (setf pos (+ pos r2-len)))
          ;; Record 3: Encrypted handshake
          (let* ((r3-len (+ 5 (ash (aref server-response (+ pos 3)) 8)
                              (aref server-response (+ pos 4))))
                 (r3-bytes (subseq server-response pos (+ pos r3-len)))
                 (client-finished-bytes (tls:tls-handshake-step client r3-bytes)))
            ;; Client should now be connected
            (assert-eq (tls:tls-connection-state client) :connected)
            ;; Client should have produced Finished bytes
            (assert-true client-finished-bytes)
            (assert-true (typep client-finished-bytes '(simple-array (unsigned-byte 8) (*))))
            (assert-true (> (length client-finished-bytes) 10))

            ;; Step 4: Server processes client Finished
            ;; client-finished-bytes contains CCS + encrypted Finished
            (let ((fpos 0))
              (loop while (< fpos (length client-finished-bytes))
                    do (let* ((record-len (+ 5 (ash (aref client-finished-bytes (+ fpos 3)) 8)
                                                (aref client-finished-bytes (+ fpos 4))))
                              (record-bytes (subseq client-finished-bytes fpos (+ fpos record-len)))
                              (record (tls:parse-tls-record record-bytes)))
                         (cond
                           ((= (tls:tls-record-content-type record)
                               tls:+content-change-cipher-spec+)
                            nil)  ; ignore CCS
                           ((= (tls:tls-record-content-type record)
                               tls:+content-application-data+)
                            (tls:tls-server-process-finished server (tls:tls-record-data record))))
                         (setf fpos (+ fpos record-len)))))
            ;; Both sides connected!
            (assert-eq (tls:tls-connection-state server) :connected)

            ;; Step 5: Exchange application data
            ;; Client sends to server
            (let* ((plaintext (tls::string-to-bytes "Hello from client"))
                   (app-record (tls:tls-send-application-data client plaintext))
                   (app-rec (tls:parse-tls-record app-record))
                   (decrypted (tls:tls-receive-application-data server (tls:tls-record-data app-rec))))
              (assert-equalp decrypted plaintext))

            ;; Server sends to client
            (let* ((plaintext (tls::string-to-bytes "Hello from server"))
                   (app-record (tls:tls-send-application-data server plaintext))
                   (app-rec (tls:parse-tls-record app-record))
                   (decrypted (tls:tls-receive-application-data client (tls:tls-record-data app-rec))))
              (assert-equalp decrypted plaintext))))))))

;;; ---------------------------------------------------------------------------
;;; Pipe transport for tls-connect/tls-accept testing
;;; ---------------------------------------------------------------------------

(defstruct tls-pipe
  "A bidirectional pipe connecting two memory transports.
   Writing to side-a feeds into side-b's incoming, and vice versa."
  (a-to-b (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
  (b-to-a (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
  (a-read-pos 0 :type fixnum)
  (b-read-pos 0 :type fixnum))

(defstruct pipe-transport-side
  "One side of a pipe transport."
  (pipe nil :type (or null tls-pipe))
  (side :a :type symbol))  ; :a or :b

(defmethod tls:tls-transport-read ((transport pipe-transport-side) buffer start end)
  (let* ((pipe (pipe-transport-side-pipe transport))
         (side (pipe-transport-side-side transport))
         ;; Side :a reads from b-to-a, side :b reads from a-to-b
         (src (if (eq side :a) (tls-pipe-b-to-a pipe) (tls-pipe-a-to-b pipe)))
         (pos-ref (if (eq side :a) (tls-pipe-a-read-pos pipe) (tls-pipe-b-read-pos pipe)))
         (avail (- (length src) pos-ref))
         (wanted (- end start))
         (n (min wanted avail)))
    (when (plusp n)
      (loop for i from 0 below n
            do (setf (aref buffer (+ start i))
                     (aref src (+ pos-ref i))))
      (if (eq side :a)
          (incf (tls-pipe-a-read-pos pipe) n)
          (incf (tls-pipe-b-read-pos pipe) n)))
    n))

(defmethod tls:tls-transport-write ((transport pipe-transport-side) buffer start end)
  (let* ((pipe (pipe-transport-side-pipe transport))
         (side (pipe-transport-side-side transport))
         ;; Side :a writes to a-to-b, side :b writes to b-to-a
         (dst (if (eq side :a) (tls-pipe-a-to-b pipe) (tls-pipe-b-to-a pipe))))
    (loop for i from start below end
          do (vector-push-extend (aref buffer i) dst))
    (- end start)))

(defmethod tls:tls-transport-close ((transport pipe-transport-side))
  nil)

(defun make-pipe-transports ()
  "Create a pair of connected transports. Returns (values side-a side-b)."
  (let ((pipe (make-tls-pipe)))
    (values
     (make-pipe-transport-side :pipe pipe :side :a)
     (make-pipe-transport-side :pipe pipe :side :b))))

(deftest test-full-handshake-with-tls-connect-accept
  "Full TLS 1.3 handshake using tls-connect and tls-accept over pipe"
  (let ((config (make-test-server-config)))
    (multiple-value-bind (client-transport server-transport) (make-pipe-transports)
      ;; We can't run both sides concurrently (single-threaded), so we
      ;; do the handshake step-by-step manually using lower-level API,
      ;; then verify the stream works.
      ;; This test uses the step-by-step approach and then wraps in streams.
      (let* ((client-conn (tls:make-tls-connection :hostname "localhost"))
             (server-conn (tls:make-tls-connection :role :server)))
        (setf (tls::tls-connection-server-config server-conn) config)

        ;; Client -> ClientHello -> pipe -> Server
        (let ((ch-record (tls::tls-start-handshake client-conn)))
          (tls:tls-transport-write client-transport ch-record 0 (length ch-record)))

        ;; Server reads ClientHello from pipe
        (let* ((ch-bytes (tls::read-tls-record-from-transport server-transport))
               (record (tls:parse-tls-record ch-bytes))
               (ch-data (tls:tls-record-data record))
               (ch-payload (subseq ch-data 4))
               (server-response (tls:tls-server-start-handshake server-conn ch-payload)))
          ;; Server -> response -> pipe -> Client
          (tls:tls-transport-write server-transport server-response 0 (length server-response)))

        ;; Client reads and processes server records
        (let ((client-finished nil))
          (loop until (eq (tls:tls-connection-state client-conn) :connected)
                do (let* ((record-bytes (tls::read-tls-record-from-transport client-transport))
                          (result (tls:tls-handshake-step client-conn record-bytes)))
                     (when (and result (typep result '(simple-array (unsigned-byte 8) (*))))
                       (setf client-finished result))))
          ;; Client sends Finished back through pipe
          (when client-finished
            (tls:tls-transport-write client-transport client-finished 0 (length client-finished))))

        ;; Server reads client Finished
        (loop until (eq (tls:tls-connection-state server-conn) :connected)
              do (let* ((record-bytes (tls::read-tls-record-from-transport server-transport))
                        (record (tls:parse-tls-record record-bytes)))
                   (cond
                     ((= (tls:tls-record-content-type record) tls:+content-change-cipher-spec+)
                      nil)
                     ((= (tls:tls-record-content-type record) tls:+content-application-data+)
                      (tls:tls-server-process-finished server-conn (tls:tls-record-data record))))))

        ;; Both connected!
        (assert-eq (tls:tls-connection-state client-conn) :connected)
        (assert-eq (tls:tls-connection-state server-conn) :connected)

        ;; Wrap in streams
        (let ((client-stream (tls:make-tls-stream :connection client-conn :transport client-transport))
              (server-stream (tls:make-tls-stream :connection server-conn :transport server-transport)))

          ;; Client writes, server reads
          (tls:tls-write client-stream (tls::string-to-bytes "ping") :start 0 :end 4)
          (let* ((record-bytes (tls::read-tls-record-from-transport server-transport))
                 (record (tls:parse-tls-record record-bytes))
                 (plaintext (tls:tls-receive-application-data server-conn (tls:tls-record-data record))))
            (assert-equal (tls::bytes-to-string plaintext) "ping"))

          ;; Server writes, client reads
          (tls:tls-write server-stream (tls::string-to-bytes "pong") :start 0 :end 4)
          (let* ((record-bytes (tls::read-tls-record-from-transport client-transport))
                 (record (tls:parse-tls-record record-bytes))
                 (plaintext (tls:tls-receive-application-data client-conn (tls:tls-record-data record))))
            (assert-equal (tls::bytes-to-string plaintext) "pong")))))))

;;; ---------------------------------------------------------------------------
;;; Real-world integration test: connect to an actual HTTPS server
;;; ---------------------------------------------------------------------------

(defun make-tcp-transport (host port)
  "Open a TCP connection and return an fd-transport."
  (let* ((addrs (net:resolve-address host port))
         (tcp (net:tcp-connect (first addrs)))
         (stream (make-two-way-stream
                  (net:tcp-stream-byte-reader tcp)
                  (net:tcp-stream-byte-writer tcp))))
    (tls::make-fd-transport :stream stream)))

(deftest test-real-tls-handshake
  "Connect to example.com:443 and perform a TLS 1.3 handshake"
  (let ((transport (make-tcp-transport "example.com" 443)))
    (unwind-protect
         (let ((stream (tls:tls-connect transport :hostname "example.com")))
           ;; Handshake succeeded!
           (assert-true (tls:tls-stream-p stream))
           (assert-eq (tls:tls-connection-state (tls:tls-stream-connection stream)) :connected)
           ;; Send an HTTP GET request
           (let ((request (tls::string-to-bytes
                           (format nil "GET / HTTP/1.1~C~CHost: example.com~C~CConnection: close~C~C~C~C"
                                   #\Return #\Linefeed #\Return #\Linefeed
                                   #\Return #\Linefeed #\Return #\Linefeed))))
             (tls:tls-write stream request :start 0 :end (length request)))
           ;; Read the response
           (let* ((buf (make-array 4096 :element-type '(unsigned-byte 8)))
                  (n (tls:tls-read stream buf :start 0 :end 4096))
                  (response (tls::bytes-to-string (subseq buf 0 n))))
             ;; Should start with HTTP/1.1 200
             (assert-true (> n 0))
             (assert-true (search "HTTP/1.1" response))
             (assert-true (search "200" response)))
           ;; Clean shutdown
           (tls:tls-shutdown stream))
      (tls:tls-transport-close transport))))
