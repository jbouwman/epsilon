;;;; Tests for TLS 1.3 protocol error cases
;;;;
;;;; Exercises error paths that are reachable in adversarial/malformed input
;;;; scenarios: truncated handshake payloads, negotiation failures, state
;;;; machine violations, record layer edge cases, and early data misuse.
;;;; These complement tls13-tests.lisp which covers the happy path.

(defpackage epsilon.crypto.tls13-error-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.tls13 tls)
   (epsilon.crypto.curve25519 x25519)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.tls-session-ticket-store stek)))

(in-package :epsilon.crypto.tls13-error-tests)

;;; ===========================================================================
;;; Helpers
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

(defun make-server-conn (config)
  "Create a server-role connection with CONFIG attached."
  (let ((conn (tls:make-tls-connection :role :server)))
    (setf (tls::tls-connection-server-config conn) config)
    conn))

(defun %drive-to-connected ()
  "Drive a client+server pair to :connected state. Returns (values client server)."
  (let* ((config (make-test-server-config))
         (client (tls:make-tls-connection :hostname "localhost"))
         (server (make-server-conn config)))
    (let* ((ch-record-bytes (tls::tls-start-handshake client))
           (ch-record (tls:parse-tls-record ch-record-bytes))
           (ch-payload (subseq (tls:tls-record-data ch-record) 4))
           (server-response (tls:tls-server-start-handshake server ch-payload))
           (client-finished nil))
      ;; Feed server flight to client
      (let ((pos 0))
        (loop while (< pos (length server-response))
              do (let* ((rlen (+ 5 (ash (aref server-response (+ pos 3)) 8)
                                   (aref server-response (+ pos 4))))
                        (rbytes (subseq server-response pos (+ pos rlen)))
                        (resp (tls:tls-handshake-step client rbytes)))
                   (when (typep resp '(simple-array (unsigned-byte 8) (*)))
                     (setf client-finished resp))
                   (incf pos rlen))))
      ;; Feed client Finished to server
      (let ((fpos 0))
        (loop while (< fpos (length client-finished))
              do (let* ((rlen (+ 5 (ash (aref client-finished (+ fpos 3)) 8)
                                   (aref client-finished (+ fpos 4))))
                        (rbytes (subseq client-finished fpos (+ fpos rlen)))
                        (record (tls:parse-tls-record rbytes)))
                   (when (= (tls:tls-record-content-type record)
                            tls:+content-application-data+)
                     (tls:tls-server-process-finished server (tls:tls-record-data record)))
                   (incf fpos rlen)))))
    (values client server)))

;;; ===========================================================================
;;; 1. Malformed ClientHello parsing (tls-decode-error)
;;; ===========================================================================

(deftest test-parse-client-hello-empty-payload
  "Empty payload signals tls-decode-error"
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello empty))))

(deftest test-parse-client-hello-truncated-at-version
  "Payload with only 1 byte (need 2 for legacy_version) signals tls-decode-error"
  (let ((short (make-array 1 :element-type '(unsigned-byte 8) :initial-element 3)))
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello short))))

(deftest test-parse-client-hello-truncated-at-random
  "Payload with version but truncated random (< 34 bytes total) signals tls-decode-error"
  (let ((short (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; 2 bytes version + only 8 bytes of random (need 32)
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello short))))

(deftest test-parse-client-hello-truncated-at-session-id
  "Payload with version + random but truncated session_id length signals tls-decode-error"
  ;; 2 (version) + 32 (random) = 34 bytes, but no session_id length byte
  (let ((short (make-array 34 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello short))))

(deftest test-parse-client-hello-session-id-overflows
  "session_id length field pointing past end of payload signals tls-decode-error"
  ;; 2 (version) + 32 (random) + 1 (session_id_len=255) = 35 bytes
  (let ((bad (make-array 35 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref bad 34) 255)  ; session_id_len = 255, but no data follows
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello bad))))

(deftest test-parse-client-hello-truncated-at-cipher-suites
  "Payload that ends after session_id but before cipher_suites length signals tls-decode-error"
  ;; 2 (version) + 32 (random) + 1 (sid_len=0) = 35 bytes, no cipher_suites
  (let ((bad (make-array 35 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello bad))))

(deftest test-parse-client-hello-cipher-suites-overflow
  "cipher_suites length pointing past payload signals tls-decode-error"
  ;; 2 + 32 + 1(sid=0) + 2(suites_len) = 37 bytes
  (let ((bad (make-array 37 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; cipher_suites_len = 1000 at offset 35
    (setf (aref bad 35) #x03)
    (setf (aref bad 36) #xE8)
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello bad))))

(deftest test-parse-client-hello-truncated-at-compression
  "Payload that ends after cipher_suites but before compression_methods signals tls-decode-error"
  ;; 2 + 32 + 1(sid=0) + 2(suites_len=2) + 2(one suite) = 39 bytes, no compression
  (let ((bad (make-array 39 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; suites_len = 2
    (setf (aref bad 35) 0)
    (setf (aref bad 36) 2)
    ;; one cipher suite (AES-128-GCM)
    (setf (aref bad 37) #x13)
    (setf (aref bad 38) #x01)
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello bad))))

(deftest test-parse-client-hello-extension-data-overflow
  "Extension with length pointing past end signals tls-decode-error"
  ;; Build a minimal valid ClientHello up to extensions, then add a bad extension
  (let* ((sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch-msg (tls:build-client-hello
                  :hostname "localhost"
                  :key-shares (list (list :x25519 pk))))
         ;; Get the payload (strip 4-byte handshake header)
         (payload (subseq ch-msg 4))
         ;; Corrupt: truncate last 10 bytes to break an extension's data
         (truncated (subseq payload 0 (- (length payload) 10))))
    (assert-condition (tls:tls-decode-error)
      (tls:parse-client-hello truncated))))

(deftest test-parse-client-hello-minimal-valid
  "A minimal well-formed payload (no extensions) parses without error"
  ;; 2(version) + 32(random) + 1(sid=0) + 2(suites_len=2) + 2(suite) + 1(comp_len=1) + 1(comp=0) = 41
  (let ((payload (make-array 41 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; version
    (setf (aref payload 0) #x03 (aref payload 1) #x03)
    ;; random: leave as zeros
    ;; session_id length = 0
    (setf (aref payload 34) 0)
    ;; cipher_suites length = 2
    (setf (aref payload 35) 0 (aref payload 36) 2)
    ;; TLS_AES_128_GCM_SHA256
    (setf (aref payload 37) #x13 (aref payload 38) #x01)
    ;; compression_methods length = 1, method = 0
    (setf (aref payload 39) 1 (aref payload 40) 0)
    ;; Should parse without error
    (let ((parsed (tls:parse-client-hello payload)))
      (assert-true parsed)
      (assert-true (member #x1301 (tls:parsed-client-hello-cipher-suites parsed))))))

;;; ===========================================================================
;;; 2. Record layer edge cases
;;; ===========================================================================

(deftest test-parse-tls-record-oversized
  "Record with length > 16641 signals error"
  ;; Build a record header claiming 20000 bytes
  (let ((header (make-array 5 :element-type '(unsigned-byte 8))))
    (setf (aref header 0) tls:+content-application-data+)
    (setf (aref header 1) #x03 (aref header 2) #x03)
    ;; length = 20000 = #x4E20
    (setf (aref header 3) #x4E (aref header 4) #x20)
    ;; Pad with enough bytes so the header+data length check won't return nil first
    (let ((data (make-array 20005 :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace data header)
      (assert-condition (error)
        (tls:parse-tls-record data)))))

(deftest test-decrypt-record-ciphertext-too-short
  "Ciphertext shorter than 16 bytes (AEAD tag size) signals error"
  (let ((key (drbg:random-bytes 16))
        (iv (drbg:random-bytes 12))
        ;; 10 bytes -- too short to extract 16-byte tag
        (short-ct (drbg:random-bytes 10)))
    (assert-condition (error)
      (tls:decrypt-record short-ct key iv 0 tls:+tls-aes-128-gcm-sha256+))))

(deftest test-decrypt-record-empty-ciphertext
  "Zero-length ciphertext signals error"
  (let ((key (drbg:random-bytes 16))
        (iv (drbg:random-bytes 12))
        (empty (make-array 0 :element-type '(unsigned-byte 8))))
    (assert-condition (error)
      (tls:decrypt-record empty key iv 0 tls:+tls-aes-128-gcm-sha256+))))

(deftest test-decrypt-record-all-padding-plaintext
  "Record that decrypts to all-zero padding (no content type byte) signals error"
  ;; Encrypt a payload of just the content-type byte (0) which after
  ;; decryption will be stripped as padding, leaving empty inner plaintext.
  (let* ((key (drbg:random-bytes 16))
         (iv (drbg:random-bytes 12))
         ;; Encrypt empty data with content-type 0 -- the inner plaintext
         ;; will be a single 0x00 byte, which the padding stripper treats
         ;; as padding, yielding an empty result.
         (ciphertext (tls:encrypt-record
                      (make-array 0 :element-type '(unsigned-byte 8))
                      0  ; content-type byte = 0
                      key iv 0 tls:+tls-aes-128-gcm-sha256+)))
    (assert-condition (error)
      (tls:decrypt-record ciphertext key iv 0 tls:+tls-aes-128-gcm-sha256+))))

(deftest test-decrypt-record-wrong-sequence-number
  "Decryption with wrong sequence number fails (bad MAC)"
  (let* ((key (drbg:random-bytes 16))
         (iv (drbg:random-bytes 12))
         (plaintext (make-array 5 :element-type '(unsigned-byte 8) :initial-element #xAA))
         (ciphertext (tls:encrypt-record plaintext tls:+content-application-data+
                                         key iv 0 tls:+tls-aes-128-gcm-sha256+)))
    (assert-condition (error)
      (tls:decrypt-record ciphertext key iv 99 tls:+tls-aes-128-gcm-sha256+))))

;;; ===========================================================================
;;; 3. Cipher suite negotiation failures
;;; ===========================================================================

(deftest test-no-common-cipher-suite-errors
  "Server rejects ClientHello with no overlapping cipher suite"
  (let* ((config (make-test-server-config))
         (server (make-server-conn config))
         (sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         ;; Build ClientHello offering only unknown cipher suites
         (ch (tls:build-client-hello
              :hostname "localhost"
              :cipher-suites (list #xCAFE #xBABE)
              :key-shares (list (list :x25519 pk))))
         (ch-payload (subseq ch 4)))
    (assert-condition (error)
      (tls:tls-server-start-handshake server ch-payload))))

(deftest test-no-supported-key-share-group-errors
  "Server rejects ClientHello with no recognized key share groups"
  ;; Build a ClientHello that only offers an unknown key share group.
  ;; We build a valid CH then manually corrupt the group ID in the payload.
  (let* ((config (make-test-server-config))
         (server (make-server-conn config))
         (sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch (tls:build-client-hello
              :hostname "localhost"
              ;; Only offer x25519 so there's exactly one key share to corrupt
              :cipher-suites (list tls:+tls-aes-128-gcm-sha256+)
              :key-shares (list (list :x25519 pk))))
         (ch-payload (subseq ch 4)))
    ;; Find the x25519 group ID (0x001D) in the key_share extension and
    ;; replace with an unknown group (0xFFFF). The group ID appears as
    ;; two bytes: 0x00 0x1D inside the key_share extension data.
    (let ((found nil))
      (loop for i from 0 below (1- (length ch-payload))
            when (and (= (aref ch-payload i) #x00)
                      (= (aref ch-payload (1+ i)) #x1D))
            do (setf (aref ch-payload i) #xFF
                     (aref ch-payload (1+ i)) #xFF
                     found t)
               (return))
      (assert-true found)
      (assert-condition (error)
        (tls:tls-server-start-handshake server ch-payload)))))

(deftest test-unknown-cipher-suite-in-key-schedule
  "Unknown cipher suite ID signals error in key schedule creation"
  (assert-condition (error)
    (tls:make-tls13-key-schedule :cipher-suite #xDEAD)))

;;; ===========================================================================
;;; 4. State machine violations
;;; ===========================================================================

(deftest test-send-application-data-before-connected
  "Sending app data in :start state signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost"))
        (data (make-array 5 :element-type '(unsigned-byte 8) :initial-element 1)))
    (assert-eq (tls:tls-connection-state conn) :start)
    (assert-condition (error)
      (tls:tls-send-application-data conn data))))

(deftest test-send-application-data-in-wait-server-hello
  "Sending app data in :wait-server-hello state signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost"))
        (data (make-array 5 :element-type '(unsigned-byte 8) :initial-element 1)))
    (tls::tls-start-handshake conn)
    (assert-eq (tls:tls-connection-state conn) :wait-server-hello)
    (assert-condition (error)
      (tls:tls-send-application-data conn data))))

(deftest test-receive-application-data-before-connected
  "Receiving app data in :start state signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost"))
        (ct (drbg:random-bytes 50)))
    (assert-condition (error)
      (tls:tls-receive-application-data conn ct))))

(deftest test-unexpected-record-type-in-wait-server-hello
  "Alert record during :wait-server-hello signals error via tls-handshake-step"
  (let ((conn (tls:make-tls-connection :hostname "localhost")))
    (tls::tls-start-handshake conn)
    ;; Feed a handshake record with invalid handshake type where ServerHello expected
    ;; Actually feed an application_data record in wait-server-hello
    (let ((bad-record (tls::serialize-tls-record
                       (tls:make-tls-record
                        :content-type tls:+content-application-data+
                        :data (drbg:random-bytes 20)))))
      ;; In :wait-server-hello, only content-type handshake or CCS is expected
      ;; application_data should fall through to the error branch
      (assert-condition (error)
        (tls:tls-handshake-step conn bad-record)))))

(deftest test-unexpected-handshake-record-in-start-state
  "Handshake record in :start state (before client has sent CH) signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost")))
    ;; conn is in :start, feed it a handshake record
    (let ((fake-sh (tls::serialize-tls-record
                    (tls:make-tls-record
                     :content-type tls:+content-handshake+
                     :data (drbg:random-bytes 20)))))
      (assert-condition (error)
        (tls:tls-handshake-step conn fake-sh)))))

(deftest test-key-update-before-connected
  "KeyUpdate in non-:connected state signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost")))
    (assert-condition (error)
      (tls:build-key-update conn))))

(deftest test-close-notify-from-connected-state
  "close_notify from :connected state produces encrypted alert and transitions to :closed"
  (multiple-value-bind (client server) (%drive-to-connected)
    (declare (ignore server))
    (assert-eq (tls:tls-connection-state client) :connected)
    (let ((record (tls:tls-close-notify client)))
      (assert-true (> (length record) 0))
      (assert-eq (tls:tls-connection-state client) :closed))))

;;; ===========================================================================
;;; 5. Early data (0-RTT) error paths
;;; ===========================================================================

(deftest test-early-data-wrong-state
  "tls-send-early-data in :start state signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost"))
        (data (make-array 5 :element-type '(unsigned-byte 8) :initial-element 1)))
    (assert-condition (error)
      (tls:tls-send-early-data conn data))))

(deftest test-early-data-no-ticket
  "tls-send-early-data without session ticket signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost")))
    ;; Move to :wait-server-hello
    (tls::tls-start-handshake conn)
    (assert-eq (tls:tls-connection-state conn) :wait-server-hello)
    (let ((data (make-array 5 :element-type '(unsigned-byte 8) :initial-element 1)))
      (assert-condition (error)
        (tls:tls-send-early-data conn data)))))

;;; ===========================================================================
;;; 6. Version negotiation failures
;;; ===========================================================================

(deftest test-server-hello-wrong-version
  "ServerHello without TLS 1.3 supported_version signals error"
  (let ((conn (tls:make-tls-connection :hostname "localhost")))
    (tls::tls-start-handshake conn)
    ;; Build a fake ServerHello payload with TLS 1.2 version, no supported_versions ext
    ;; ServerHello: version(2) + random(32) + session_id_len(1) + session_id(0) +
    ;;   cipher_suite(2) + compression(1) = 38 bytes minimum
    (let ((payload (make-array 38 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref payload 0) #x03 (aref payload 1) #x03) ; version 0x0303
      ;; Random (non-HRR magic)
      (replace payload (drbg:random-bytes 32) :start1 2)
      ;; session_id_len = 0
      (setf (aref payload 34) 0)
      ;; cipher suite = TLS_AES_128_GCM_SHA256
      (setf (aref payload 35) #x13 (aref payload 36) #x01)
      ;; compression = 0
      (setf (aref payload 37) 0)
      ;; Wrap in handshake message
      (let* ((sh-msg (tls::make-handshake-message tls:+handshake-server-hello+ payload))
             (record (tls::serialize-tls-record
                      (tls:make-tls-record :content-type tls:+content-handshake+
                                           :data sh-msg))))
        ;; Should error because no supported_version extension with 0x0304
        (assert-condition (error)
          (tls:tls-handshake-step conn record))))))

;;; ===========================================================================
;;; 7. Alert protocol edge cases
;;; ===========================================================================

(deftest test-parse-alert-too-short
  "Parsing a 1-byte alert signals error"
  (let ((short (make-array 1 :element-type '(unsigned-byte 8) :initial-element 2)))
    (assert-condition (error)
      (tls:parse-alert short))))

(deftest test-parse-alert-zero-bytes
  "Parsing an empty alert signals error"
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    (assert-condition (error)
      (tls:parse-alert empty))))

(deftest test-parse-alert-unknown-description
  "Alert with unknown description code still parses"
  (let* ((data (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents '(2 255)))
         (alert (tls:parse-alert data)))
    (assert-= (tls:tls-alert-level alert) 2)
    (assert-= (tls:tls-alert-description alert) 255)))

;;; ===========================================================================
;;; 8. Application data exchange after connected
;;; ===========================================================================

(deftest test-receive-unexpected-inner-content-type
  "Decrypted record with unexpected inner content type signals error"
  (multiple-value-bind (client server) (%drive-to-connected)
    ;; Encrypt a record with an invalid inner content type (99)
    (let* ((key (tls::tls-connection-server-app-key server))
           (iv (tls::tls-connection-server-app-iv server))
           (seq (tls::tls-connection-server-seq server))
           (suite (tls::tls-connection-cipher-suite server))
           (data (make-array 5 :element-type '(unsigned-byte 8) :initial-element #xAA))
           (ciphertext (tls:encrypt-record data 99  ; content type 99
                                           key iv seq suite)))
      (assert-condition (error)
        (tls:tls-receive-application-data client ciphertext)))))

(deftest test-bidirectional-app-data-multiple-records
  "Multiple sequential records decrypt correctly with incrementing sequence numbers"
  (multiple-value-bind (client server) (%drive-to-connected)
    ;; Client -> Server: 3 messages
    (dotimes (i 3)
      (let* ((msg (tls::string-to-bytes (format nil "msg-~D" i)))
             (record (tls:tls-send-application-data client msg))
             (parsed (tls:parse-tls-record record))
             (decrypted (tls:tls-receive-application-data server (tls:tls-record-data parsed))))
        (assert-equalp decrypted msg)))
    ;; Server -> Client: 3 messages
    (dotimes (i 3)
      (let* ((msg (tls::string-to-bytes (format nil "reply-~D" i)))
             (record (tls:tls-send-application-data server msg))
             (parsed (tls:parse-tls-record record))
             (decrypted (tls:tls-receive-application-data client (tls:tls-record-data parsed))))
        (assert-equalp decrypted msg)))))

;;; ===========================================================================
;;; 9. Server rejects malformed key share data
;;; ===========================================================================

(deftest test-server-rejects-x25519-wrong-length
  "X25519 key share with wrong length causes handshake error"
  (let* ((config (make-test-server-config))
         (server (make-server-conn config))
         ;; 31 bytes instead of 32
         (bad-pk (drbg:random-bytes 31))
         (ch (tls:build-client-hello
              :hostname "localhost"
              :key-shares (list (list :x25519 bad-pk))))
         (ch-payload (subseq ch 4)))
    ;; Server should error during key exchange (X25519 needs exactly 32 bytes)
    (assert-condition (error)
      (tls:tls-server-start-handshake server ch-payload))))

(deftest test-server-rejects-p256-wrong-length
  "P-256 key share with wrong length causes handshake error"
  (let* ((config (make-test-server-config))
         (server (make-server-conn config))
         ;; P-256 uncompressed point is 65 bytes; send 30
         (bad-pk (drbg:random-bytes 30))
         (ch (tls:build-client-hello
              :hostname "localhost"
              :key-shares (list (list :secp256r1 bad-pk))))
         (ch-payload (subseq ch 4)))
    (assert-condition (error)
      (tls:tls-server-start-handshake server ch-payload))))

;;; ===========================================================================
;;; 10. Alert delivery: tls-accept sends alert before erroring
;;; ===========================================================================

(defun %make-ch-record (ch-msg)
  "Wrap a ClientHello handshake message in a TLS record for feeding to tls-accept."
  (tls::serialize-tls-record
   (tls:make-tls-record :content-type tls:+content-handshake+ :data ch-msg)))

(defun %extract-alert-from-output (output-bytes)
  "If OUTPUT-BYTES starts with a TLS alert record, return (values level description).
   Returns NIL if no alert record is found."
  (when (and (>= (length output-bytes) 7)
             (= (aref output-bytes 0) tls:+content-alert+))
    (values (aref output-bytes 5) (aref output-bytes 6))))

(deftest test-tls-accept-sends-alert-on-no-cipher-suite
  "tls-accept sends handshake_failure alert when no common cipher suite"
  (let* ((config (make-test-server-config))
         (sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch (tls:build-client-hello
              :hostname "localhost"
              :cipher-suites (list #xCAFE #xBABE)
              :key-shares (list (list :x25519 pk))))
         (record-bytes (%make-ch-record ch))
         (mt (tls::make-memory-transport)))
    (tls::memory-transport-feed mt record-bytes)
    (handler-case (tls:tls-accept mt config)
      (error ()
        (let ((output (tls::memory-transport-get-output mt)))
          (multiple-value-bind (level desc) (%extract-alert-from-output output)
            (assert-= level 2)   ; fatal
            (assert-= desc tls:+alert-handshake-failure+)))))))

(deftest test-tls-accept-sends-alert-on-decode-error
  "tls-accept sends decode_error alert on malformed ClientHello"
  (let* ((config (make-test-server-config))
         ;; Build a TLS record with truncated handshake payload
         (truncated-ch (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0))
         ;; Handshake header: type=1(ClientHello), length=6
         (_ (setf (aref truncated-ch 0) 1
                  (aref truncated-ch 1) 0 (aref truncated-ch 2) 0 (aref truncated-ch 3) 6))
         (record-bytes (tls::serialize-tls-record
                        (tls:make-tls-record :content-type tls:+content-handshake+
                                             :data truncated-ch)))
         (mt (tls::make-memory-transport)))
    (declare (ignore _))
    (tls::memory-transport-feed mt record-bytes)
    (handler-case (tls:tls-accept mt config)
      (error ()
        (let ((output (tls::memory-transport-get-output mt)))
          (multiple-value-bind (level desc) (%extract-alert-from-output output)
            (assert-= level 2)   ; fatal
            (assert-= desc tls:+alert-decode-error+)))))))

(deftest test-tls-accept-sends-alert-on-no-key-share
  "tls-accept sends handshake_failure when no recognized key share group"
  (let* ((config (make-test-server-config))
         (sk (drbg:random-bytes 32))
         (pk (x25519:x25519-base sk))
         (ch (tls:build-client-hello
              :hostname "localhost"
              :cipher-suites (list tls:+tls-aes-128-gcm-sha256+)
              :key-shares (list (list :x25519 pk))))
         (ch-payload (subseq ch 4)))
    ;; Corrupt ALL occurrences of the x25519 group ID (0x001D) to unknown
    ;; (0xFFFF). The ID appears in both supported_groups and key_share
    ;; extensions; both must be corrupted for the server to reject.
    (let ((found 0))
      (loop for i from 0 below (1- (length ch-payload))
            when (and (= (aref ch-payload i) #x00)
                      (= (aref ch-payload (1+ i)) #x1D))
            do (setf (aref ch-payload i) #xFF
                     (aref ch-payload (1+ i)) #xFF)
               (incf found))
      (assert-true (> found 0)))
    ;; Reassemble the handshake message with corrupted payload
    (let* ((ch-msg (tls::make-handshake-message tls:+handshake-client-hello+ ch-payload))
           (record-bytes (%make-ch-record ch-msg))
           (mt (tls::make-memory-transport)))
      (tls::memory-transport-feed mt record-bytes)
      (handler-case (tls:tls-accept mt config)
        (error ()
          (let ((output (tls::memory-transport-get-output mt)))
            (multiple-value-bind (level desc) (%extract-alert-from-output output)
              (assert-= level 2)
              (assert-= desc tls:+alert-handshake-failure+))))))))

(deftest test-tls-accept-sends-alert-on-psk-binder-failure
  "tls-accept sends decrypt_error alert on tampered PSK binder"
  (let* ((store (stek:make-stek-store))
         (sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (cert-der (x509:make-self-signed-certificate
                    :key-type :ed25519
                    :private-key sk
                    :public-key-bytes pk
                    :subject "localhost"
                    :dns-names '("localhost")))
         (config (tls:make-tls-server-config
                  :certificate-chain (list cert-der)
                  :private-key sk
                  :key-type :ed25519
                  :session-ticket-store store)))
    ;; Get a valid ticket via a fresh handshake
    (let* ((client1 (tls:make-tls-connection :hostname "localhost"))
           (server1 (tls:make-tls-connection :role :server)))
      (setf (tls::tls-connection-server-config server1) config)
      (let* ((ch-rec (tls::tls-start-handshake client1))
             (ch-record (tls:parse-tls-record ch-rec))
             (ch-payload (subseq (tls:tls-record-data ch-record) 4))
             (srv-resp (tls:tls-server-start-handshake server1 ch-payload))
             (client-finished nil))
        (let ((pos 0))
          (loop while (< pos (length srv-resp))
                do (let* ((rlen (+ 5 (ash (aref srv-resp (+ pos 3)) 8)
                                     (aref srv-resp (+ pos 4))))
                          (rbytes (subseq srv-resp pos (+ pos rlen)))
                          (resp (tls:tls-handshake-step client1 rbytes)))
                     (when (typep resp '(simple-array (unsigned-byte 8) (*)))
                       (setf client-finished resp))
                     (incf pos rlen))))
        (let ((fpos 0) (ticket-records nil))
          (loop while (< fpos (length client-finished))
                do (let* ((rlen (+ 5 (ash (aref client-finished (+ fpos 3)) 8)
                                     (aref client-finished (+ fpos 4))))
                          (rbytes (subseq client-finished fpos (+ fpos rlen)))
                          (record (tls:parse-tls-record rbytes)))
                     (when (= (tls:tls-record-content-type record) tls:+content-application-data+)
                       (setf ticket-records
                             (tls:tls-server-process-finished server1 (tls:tls-record-data record))))
                     (incf fpos rlen)))
          (when ticket-records
            (let ((tpos 0))
              (loop while (< tpos (length ticket-records))
                    do (let* ((rlen (+ 5 (ash (aref ticket-records (+ tpos 3)) 8)
                                         (aref ticket-records (+ tpos 4))))
                              (rbytes (subseq ticket-records tpos (+ tpos rlen)))
                              (record (tls:parse-tls-record rbytes)))
                         (tls:tls-receive-application-data
                          client1 (tls:tls-record-data record))
                         (incf tpos rlen))))))
        (let ((ticket (first (tls::tls-connection-received-tickets client1))))
          (assert-true ticket)
          ;; Build a PSK ClientHello with tampered binder, feed to tls-accept
          (let* ((client2 (tls:make-tls-connection :hostname "localhost"))
                 (psk-record (tls:build-psk-client-hello client2 ticket))
                 (psk-rec (tls:parse-tls-record psk-record))
                 (psk-data (tls:tls-record-data psk-rec)))
            ;; Flip a bit in the binder (last byte of the payload)
            (let ((idx (1- (length psk-data))))
              (setf (aref psk-data idx) (logxor (aref psk-data idx) 1)))
            (let* ((record-bytes (tls::serialize-tls-record
                                  (tls:make-tls-record :content-type tls:+content-handshake+
                                                       :data psk-data)))
                   (mt (tls::make-memory-transport)))
              (tls::memory-transport-feed mt record-bytes)
              (handler-case (tls:tls-accept mt config)
                (error ()
                  (let ((output (tls::memory-transport-get-output mt)))
                    (multiple-value-bind (level desc) (%extract-alert-from-output output)
                      (assert-= level 2)
                      (assert-= desc tls:+alert-decrypt-error+))))))))))))

(deftest test-tls-accept-no-alert-on-successful-handshake
  "Successful handshake does not produce spurious alert records"
  (let* ((config (make-test-server-config))
         (client (tls:make-tls-connection :hostname "localhost"))
         (ch-record-bytes (tls::tls-start-handshake client))
         (mt (tls::make-memory-transport)))
    (tls::memory-transport-feed mt ch-record-bytes)
    ;; tls-accept will read the CH, generate a response, then try to read
    ;; client Finished -- which won't be there, so it will error. That's OK;
    ;; we just need to check that the ServerHello flight was written (not an alert).
    (handler-case (tls:tls-accept mt config)
      (error ()
        (let ((output (tls::memory-transport-get-output mt)))
          ;; Output should start with ServerHello (content-type 22 = handshake), not alert (21)
          (assert-true (> (length output) 5))
          (assert-= (aref output 0) tls:+content-handshake+))))))

;;; ===========================================================================
;;; IMPL-380 Stage 1: structured tls-alert-error on incoming Alert records
;;;
;;; Before this change, an Alert (record type 21) arriving while the client
;;; was in :wait-server-hello surfaced as the opaque
;;; "Unexpected record type 21 in state :wait-server-hello" -- which is
;;; what we observed against scholarsjunction.msstate.edu and what hid
;;; the fact that the failure was a curve-mismatch handshake_failure.
;;; ===========================================================================

(deftest test-tls-alert-error-condition-shape
  "tls-alert-error carries level + description + state and reports both"
  (handler-case
      (error 'tls:tls-alert-error
             :level tls:+alert-level-fatal+
             :description tls:+alert-handshake-failure+
             :state :wait-server-hello)
    (tls:tls-alert-error (c)
      (assert-= (tls:tls-alert-error-level c) tls:+alert-level-fatal+)
      (assert-= (tls:tls-alert-error-description c) tls:+alert-handshake-failure+)
      (assert-eq (tls:tls-alert-error-state c) :wait-server-hello)
      (let ((report (princ-to-string c)))
        (assert-true (search "fatal" report))
        (assert-true (search "handshake_failure" report))
        ;; Keyword names print upper-case in the default ~A format.
        (assert-true (search "WAIT-SERVER-HELLO" report))))))

(deftest test-tls-handshake-step-surfaces-fatal-alert
  "Plaintext Alert during :wait-server-hello becomes a tls-alert-error,
   not the legacy 'Unexpected record type 21' string."
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         ;; Drive the connection into :wait-server-hello by starting the
         ;; handshake (the start function builds and emits ClientHello and
         ;; sets the state).
         (_ (tls::tls-start-handshake conn))
         (alert-bytes (tls:make-alert-record tls:+alert-level-fatal+
                                             tls:+alert-handshake-failure+)))
    (declare (ignore _))
    (handler-case (tls::tls-handshake-step conn alert-bytes)
      (tls:tls-alert-error (c)
        (assert-= (tls:tls-alert-error-level c) tls:+alert-level-fatal+)
        (assert-= (tls:tls-alert-error-description c)
                  tls:+alert-handshake-failure+)
        (assert-eq (tls:tls-alert-error-state c) :wait-server-hello))
      (error (e)
        (error "expected tls-alert-error, got ~A: ~A" (type-of e) e)))))

(deftest test-tls-handshake-step-surfaces-protocol-version-alert
  "protocol_version (70) surfaces as a structured alert, not opaque text"
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         (_ (tls::tls-start-handshake conn))
         (alert-bytes (tls:make-alert-record tls:+alert-level-fatal+
                                             tls:+alert-protocol-version+)))
    (declare (ignore _))
    (handler-case (tls::tls-handshake-step conn alert-bytes)
      (tls:tls-alert-error (c)
        (assert-= (tls:tls-alert-error-description c)
                  tls:+alert-protocol-version+)
        (assert-true (search "protocol_version" (princ-to-string c)))))))

(deftest test-alert-description-name-coverage
  "alert-description-name resolves the codes IMPL-380 names explicitly"
  (assert-equal "handshake_failure"
                (tls:alert-description-name tls:+alert-handshake-failure+))
  (assert-equal "protocol_version"
                (tls:alert-description-name tls:+alert-protocol-version+))
  (assert-equal "insufficient_security" (tls:alert-description-name 71))
  (assert-equal "unrecognized_name" (tls:alert-description-name 112))
  (assert-equal "no_application_protocol" (tls:alert-description-name 120))
  ;; Unknown codes get a descriptive fallback so callers can always log
  ;; something meaningful.
  (assert-true (search "unknown" (tls:alert-description-name 200))))

;;; ===========================================================================
;;; IMPL-380 Stage 2: P-384 / P-521 named groups in ClientHello
;;; ===========================================================================

(defun %client-hello-extensions-payload (ch-record-bytes)
  "Pull the raw extensions payload out of a ClientHello record. Used to
   exercise parse-client-hello on the bytes our client emits."
  (let* ((record (tls:parse-tls-record ch-record-bytes))
         (data (tls:tls-record-data record)))
    ;; data is [hs-type(1)][len(3)][CH body]
    (subseq data 4)))

(defun %u16-occurs-p (bytes value)
  "T iff the 16-bit BE value VALUE appears as a contiguous pair anywhere
   in BYTES. Used to spot named-group / signature-scheme codepoints in
   the raw ClientHello without re-parsing every extension."
  (let ((hi (logand #xFF (ash value -8)))
        (lo (logand #xFF value)))
    (loop for i from 0 below (1- (length bytes))
          when (and (= (aref bytes i) hi) (= (aref bytes (1+ i)) lo))
          return t)))

(deftest test-client-hello-advertises-fips-curves
  "ClientHello supported_groups now lists secp384r1 and secp521r1
   alongside the existing X25519 / hybrid / P-256 set. We assert the
   codepoints appear in the wire bytes directly because the local
   ClientHello parser doesn't extract supported_groups."
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         (ch-bytes (tls::tls-start-handshake conn)))
    (assert-true (%u16-occurs-p ch-bytes tls::+group-secp384r1+))
    (assert-true (%u16-occurs-p ch-bytes tls::+group-secp521r1+))
    ;; Existing curves still present.
    (assert-true (%u16-occurs-p ch-bytes tls::+group-x25519+))
    (assert-true (%u16-occurs-p ch-bytes tls::+group-secp256r1+))
    (assert-true (%u16-occurs-p ch-bytes tls::+group-x25519-mlkem768+))))

(deftest test-client-hello-key-shares-include-fips-curves
  "ClientHello key_share carries entries for secp384r1 and secp521r1
   so a server preferring those curves can finish in 1-RTT (no HRR)."
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         (ch-bytes (tls::tls-start-handshake conn))
         (payload (%client-hello-extensions-payload ch-bytes))
         (parsed (tls::%parse-client-hello payload)))
    (let* ((shares (tls::parsed-client-hello-key-shares parsed))
           (p384 (assoc tls::+group-secp384r1+ shares))
           (p521 (assoc tls::+group-secp521r1+ shares)))
      (assert-true p384)
      (assert-true p521)
      ;; Uncompressed SEC1: 04 || X || Y. P-384 uses 48-byte coords,
      ;; P-521 uses 66-byte coords.
      (assert-= 97 (length (cdr p384)))
      (assert-= 133 (length (cdr p521)))
      (assert-= #x04 (aref (cdr p384) 0))
      (assert-= #x04 (aref (cdr p521) 0)))))

(deftest test-client-stores-fips-curve-keys-on-connection
  "Generating key shares populates the per-curve private/public slots."
  (let ((conn (tls:make-tls-connection :hostname "example.com")))
    (tls::tls-start-handshake conn)
    (assert-true (tls::tls-connection-client-p384-private conn))
    (assert-true (tls::tls-connection-client-p384-public conn))
    (assert-true (tls::tls-connection-client-p521-private conn))
    (assert-true (tls::tls-connection-client-p521-public conn))))

;;; ===========================================================================
;;; IMPL-380 Stage 3: signature_algorithms covers FIPS curves + RSA-PSS
;;; ===========================================================================

(deftest test-client-hello-advertises-fips-signature-schemes
  "ClientHello signature_algorithms now includes ecdsa_secp384r1_sha384
   (#x0503) and ecdsa_secp521r1_sha512 (#x0603); without these the
   client would complete the key exchange and then abort the handshake
   on a CertificateVerify signed under a FIPS-curve leaf cert."
  (let* ((conn (tls:make-tls-connection :hostname "example.com"))
         (ch-bytes (tls::tls-start-handshake conn)))
    (assert-true (%u16-occurs-p ch-bytes tls::+sig-ecdsa-secp384r1-sha384+))
    (assert-true (%u16-occurs-p ch-bytes tls::+sig-ecdsa-secp521r1-sha512+))
    ;; Existing schemes still advertised.
    (assert-true (%u16-occurs-p ch-bytes tls::+sig-ecdsa-secp256r1-sha256+))
    (assert-true (%u16-occurs-p ch-bytes tls::+sig-ed25519+))
    (assert-true (%u16-occurs-p ch-bytes tls::+sig-rsa-pss-rsae-sha256+))))

;;; ===========================================================================
;;; IMPL-380 Stage 2/3 end-to-end: a full client+server handshake using a
;;; FIPS curve. This is the test the IMPL-380 doc calls out as Stage 2's
;;; primary verification (handshake against an in-process server
;;; configured to require P-384).
;;; ===========================================================================

(defun %force-p384-only-client ()
  "Build a TLS connection whose ClientHello will only carry a P-384
   key_share. We rebuild make-key-share-ext-client manually rather than
   reach into tls-generate-key-shares' return value, because the helper
   is what the real client uses end-to-end and we want to keep this
   test exercising production code paths.

   Strategy: drive tls-start-handshake, then re-emit the ClientHello
   with a one-entry key_shares list. All other connection state stays
   intact so the server's response routes back through the live client
   correctly."
  (let* ((conn (tls:make-tls-connection :hostname "localhost")))
    ;; Run the normal handshake initiator to populate per-curve keys
    ;; and compute the random / session-id we want to preserve.
    (tls::tls-start-handshake conn)
    ;; Rewrite the ClientHello to advertise only a P-384 key_share.
    ;; build-client-hello is a fresh ClientHello -- we feed it the same
    ;; hostname and a single (:secp384r1 ...) key_share entry.
    (let* ((p384-pub (tls::tls-connection-client-p384-public conn))
           (ch (tls::build-client-hello
                :hostname "localhost"
                :key-shares (list (list :secp384r1 p384-pub))
                :alpn (tls::tls-connection-alpn-protocols conn))))
      ;; Reset the transcript hash to match the new ClientHello bytes.
      (setf (tls::tls-connection-client-hello-msg conn) ch)
      (let ((ks (tls::make-tls13-key-schedule
                 :cipher-suite tls:+tls-aes-128-gcm-sha256+)))
        (setf (tls::tls-connection-key-schedule conn) ks)
        (tls::transcript-update ks ch))
      (setf (tls::tls-connection-state conn) :wait-server-hello)
      (values conn
              (tls::serialize-tls-record
               (tls:make-tls-record :content-type tls:+content-handshake+
                                    :data ch))))))

;;; ===========================================================================
;;; IMPL-380 Stage 4: HelloRetryRequest on the client side
;;; ===========================================================================

(defun %make-hrr-bytes (selected-group &key cookie
                                            (cipher-suite tls:+tls-aes-128-gcm-sha256+))
  "Synthesise a wire-form HelloRetryRequest record. Body is a normal
   ServerHello whose random is the HRR magic and whose key_share carries
   only the 2-byte selected_group (RFC 8446 §4.2.8.2)."
  (let* ((session-id (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
         (legacy-version #x0303)
         ;; Extensions: supported_versions(0x002b), key_share(0x0033),
         ;; optional cookie(0x002c). Encoded back-to-back with u16
         ;; type+u16 length+data each.
         (sv-ext
           (let ((b (make-array 6 :element-type '(unsigned-byte 8))))
             ;; ext_type
             (setf (aref b 0) 0) (setf (aref b 1) 43)
             ;; ext_length
             (setf (aref b 2) 0) (setf (aref b 3) 2)
             ;; selected_version = 0x0304
             (setf (aref b 4) #x03) (setf (aref b 5) #x04)
             b))
         (ks-ext
           (let ((b (make-array 6 :element-type '(unsigned-byte 8))))
             (setf (aref b 0) 0) (setf (aref b 1) 51)
             (setf (aref b 2) 0) (setf (aref b 3) 2)
             (setf (aref b 4) (ash selected-group -8))
             (setf (aref b 5) (logand selected-group #xFF))
             b))
         (cookie-ext
           (when cookie
             (let* ((cookie-len (length cookie))
                    (b (make-array (+ 4 2 cookie-len)
                                   :element-type '(unsigned-byte 8))))
               (setf (aref b 0) 0) (setf (aref b 1) 44)        ; ext_type
               (setf (aref b 2) (ash (+ 2 cookie-len) -8))      ; ext_length
               (setf (aref b 3) (logand (+ 2 cookie-len) #xFF))
               (setf (aref b 4) (ash cookie-len -8))            ; cookie length
               (setf (aref b 5) (logand cookie-len #xFF))
               (replace b cookie :start1 6)
               b)))
         (exts (concatenate '(simple-array (unsigned-byte 8) (*))
                            sv-ext ks-ext (or cookie-ext #())))
         (body
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector (ash legacy-version -8) (logand legacy-version #xFF))
                        ;; HRR magic (32 bytes)
                        tls:+hello-retry-request-magic+
                        ;; legacy_session_id length(1) + value(32)
                        (vector 32) session-id
                        ;; cipher_suite (2)
                        (vector (ash cipher-suite -8)
                                (logand cipher-suite #xFF))
                        ;; legacy_compression_method (1)
                        (vector 0)
                        ;; extensions length(2) + extensions
                        (vector (ash (length exts) -8)
                                (logand (length exts) #xFF))
                        exts))
         (hs-msg
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        ;; handshake_type=2 (server_hello), uint24 length
                        (vector 2
                                (ash (length body) -16)
                                (logand (ash (length body) -8) #xFF)
                                (logand (length body) #xFF))
                        body)))
    (tls::serialize-tls-record
     (tls:make-tls-record :content-type tls:+content-handshake+
                          :data hs-msg))))

(deftest test-hrr-emits-retry-clienthello-with-saved-random
  "On HRR, the client emits a fresh ClientHello with the *same*
   client_random (RFC 8446 §4.1.4) and only a key_share for the
   server-selected group."
  (let ((client (tls:make-tls-connection :hostname "example.com")))
    (tls::tls-start-handshake client)
    (let* ((orig-random (tls::tls-connection-client-random client))
           (orig-session (tls::tls-connection-client-session-id client))
           (hrr-bytes (%make-hrr-bytes tls::+group-secp384r1+))
           (response (tls:tls-handshake-step client hrr-bytes)))
      ;; Response must be a fresh ClientHello record.
      (assert-true (typep response '(simple-array (unsigned-byte 8) (*))))
      ;; State stays :wait-server-hello because we still owe the real SH.
      (assert-eq :wait-server-hello (tls::tls-connection-state client))
      ;; The retry CH carries the same client_random as the original.
      (let* ((rec (tls:parse-tls-record response))
             (hs (tls:tls-record-data rec))
             ;; hs = [hs-type(1)][len(3)][CH body]
             (ch-body (subseq hs 4))
             ;; CH body: [legacy_version(2)][random(32)][session_id...]
             (retry-random (subseq ch-body 2 34))
             (sid-len (aref ch-body 34))
             (retry-session (subseq ch-body 35 (+ 35 sid-len))))
        (assert-= 32 sid-len)
        (assert-true (equalp orig-random retry-random))
        (assert-true (equalp orig-session retry-session)))
      ;; The retry CH key_share lists the requested group (secp384r1).
      (let ((retry-ch-bytes response))
        (assert-true (%u16-occurs-p retry-ch-bytes
                                    tls::+group-secp384r1+))))))

(deftest test-hrr-rejects-second-hrr
  "RFC 8446 §4.1.4: a server MUST NOT send a second HRR. The client
   surfaces this as a structured tls-alert-error with
   unexpected_message."
  (let ((client (tls:make-tls-connection :hostname "example.com")))
    (tls::tls-start-handshake client)
    (let ((hrr-bytes (%make-hrr-bytes tls::+group-secp384r1+)))
      ;; First HRR: accepted.
      (tls:tls-handshake-step client hrr-bytes)
      ;; Second HRR: rejected.
      (handler-case (tls:tls-handshake-step client hrr-bytes)
        (tls:tls-alert-error (c)
          (assert-= tls:+alert-unexpected-message+
                    (tls:tls-alert-error-description c)))
        (error (e)
          (error "Expected tls-alert-error, got ~A: ~A" (type-of e) e))))))

(deftest test-hrr-cookie-is-echoed-in-retry-clienthello
  "When HRR carries a cookie, the retry ClientHello must echo the
   cookie unchanged (RFC 8446 §4.2.2)."
  (let ((client (tls:make-tls-connection :hostname "example.com"))
        (cookie (map '(simple-array (unsigned-byte 8) (*)) #'identity
                     #(#xc0 #xff #xee #x42 #x01 #x02 #x03 #x04))))
    (tls::tls-start-handshake client)
    (let* ((hrr-bytes (%make-hrr-bytes tls::+group-secp384r1+ :cookie cookie))
           (response (tls:tls-handshake-step client hrr-bytes)))
      ;; Search for the cookie content (8 bytes) in the retry CH.
      ;; Bound is (length response) - (length cookie) inclusive so that
      ;; cookies appearing at the very end of the buffer are caught.
      (let ((found nil))
        (loop for i from 0 to (- (length response) (length cookie))
              when (loop for j from 0 below (length cookie)
                         always (= (aref response (+ i j)) (aref cookie j)))
              do (setf found t) (return))
        (assert-true found)))))

(deftest test-hrr-rejects-unknown-selected-group
  "A server selecting a group we never advertised is a protocol error;
   surfaces as an illegal_parameter alert."
  (let ((client (tls:make-tls-connection :hostname "example.com")))
    (tls::tls-start-handshake client)
    ;; Use the codepoint for ffdhe2048 (#x0100) which we don't advertise.
    (let ((hrr-bytes (%make-hrr-bytes #x0100)))
      (handler-case (tls:tls-handshake-step client hrr-bytes)
        (tls:tls-alert-error (c)
          (assert-= tls:+alert-illegal-parameter+
                    (tls:tls-alert-error-description c)))
        (error (e)
          (error "Expected illegal_parameter alert, got ~A: ~A"
                 (type-of e) e))))))

;;; ===========================================================================
;;; IMPL-380 Stage 5: TLS 1.2 fallback detection
;;; ===========================================================================

(defun %make-tls12-server-hello-bytes ()
  "Synthesise a wire-form ServerHello that selects TLS 1.2: legacy
   version 0x0303 in the body and NO supported_versions extension. A
   real TLS 1.2 server replies this way to a TLS 1.3 ClientHello when
   it doesn't support 1.3."
  (let* ((legacy-version #x0303)
         (random (make-array 32 :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (session-id (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
         (cipher-suite tls:+tls-aes-128-gcm-sha256+)
         ;; Empty extensions block.
         (exts #())
         (body
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector (ash legacy-version -8) (logand legacy-version #xFF))
                        random
                        (vector 32) session-id
                        (vector (ash cipher-suite -8)
                                (logand cipher-suite #xFF))
                        (vector 0)            ; legacy_compression_method
                        (vector (ash (length exts) -8)
                                (logand (length exts) #xFF))
                        exts))
         (hs-msg
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector 2
                                (ash (length body) -16)
                                (logand (ash (length body) -8) #xFF)
                                (logand (length body) #xFF))
                        body)))
    (tls::serialize-tls-record
     (tls:make-tls-record :content-type tls:+content-handshake+
                          :data hs-msg))))

(deftest test-tls12-only-server-surfaces-version-mismatch
  "A ServerHello that doesn't list TLS 1.3 in supported_versions yields
   a structured `tls-version-mismatch-error', not the legacy bare error."
  (let ((client (tls:make-tls-connection :hostname "example.com")))
    (tls::tls-start-handshake client)
    (let ((sh-bytes (%make-tls12-server-hello-bytes)))
      (handler-case (tls:tls-handshake-step client sh-bytes)
        (tls:tls-version-mismatch-error (c)
          ;; The client never gets a positive version signal -- the ext
          ;; is missing -- so negotiated-version is NIL.
          (assert-true (null (tls:tls-version-mismatch-negotiated-version c)))
          (assert-true (search "non-1.3 ServerHello" (princ-to-string c))))
        (error (e)
          (error "Expected tls-version-mismatch-error, got ~A: ~A"
                 (type-of e) e))))))

(deftest test-tls-connect-with-fallback-symbol-exposed
  "tls-connect-with-fallback is exported from epsilon.crypto and
   tls-version-mismatch-error is reachable via the umbrella package."
  (assert-true (fboundp 'epsilon.crypto:tls-connect-with-fallback))
  (assert-true (find-class 'epsilon.crypto:tls-version-mismatch-error nil)))

(deftest test-end-to-end-p384-handshake
  "Driving a full ClientHello -> ServerHello -> Finished round-trip
   where the client offers only a secp384r1 key_share. Exercises:
   - Stage 2 client supported_groups + key_share emission for P-384
   - Server-side P-384 key exchange (ecdh-p384-shared-secret + ecdh-p384
     keypair generation), which now needs to be wired symmetrically for
     this test to drive
   - Stage 3 client-side CertificateVerify acceptance regardless of
     curve
   The test cert is Ed25519, so CertificateVerify lands on the existing
   Ed25519 path; the value here is proving the *key-exchange* path on
   P-384 actually completes."
  (let ((config (make-test-server-config))
        (server (make-server-conn (make-test-server-config))))
    (declare (ignore server))
    (let ((server (make-server-conn config)))
      (multiple-value-bind (client ch-bytes) (%force-p384-only-client)
        (let* ((ch-record (tls:parse-tls-record ch-bytes))
               (ch-payload (subseq (tls:tls-record-data ch-record) 4))
               (server-response (tls:tls-server-start-handshake server ch-payload))
               (client-finished nil))
          ;; Walk the server's flight through the client's handshake step.
          (let ((pos 0))
            (loop while (< pos (length server-response))
                  do (let* ((rlen (+ 5 (ash (aref server-response (+ pos 3)) 8)
                                       (aref server-response (+ pos 4))))
                            (rbytes (subseq server-response pos (+ pos rlen)))
                            (resp (tls:tls-handshake-step client rbytes)))
                       (when (typep resp '(simple-array (unsigned-byte 8) (*)))
                         (setf client-finished resp))
                       (incf pos rlen))))
          ;; Client must have transitioned out of :wait-server-hello and
          ;; produced a Finished flight.
          (assert-true client-finished)
          ;; Drive client-Finished into the server.
          (let ((fpos 0))
            (loop while (< fpos (length client-finished))
                  do (let* ((rlen (+ 5 (ash (aref client-finished (+ fpos 3)) 8)
                                       (aref client-finished (+ fpos 4))))
                            (rbytes (subseq client-finished fpos (+ fpos rlen)))
                            (record (tls:parse-tls-record rbytes)))
                       (when (= (tls:tls-record-content-type record)
                                tls:+content-application-data+)
                         (tls:tls-server-process-finished
                          server (tls:tls-record-data record)))
                       (incf fpos rlen))))
          ;; Both sides should be :connected.
          (assert-eq :connected (tls::tls-connection-state client))
          (assert-eq :connected (tls::tls-connection-state server)))))))
