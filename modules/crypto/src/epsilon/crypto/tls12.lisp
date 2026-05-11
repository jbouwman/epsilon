;;;; TLS 1.2 Protocol (RFC 5246, RFC 5288, RFC 7627, RFC 7905, RFC 8422)
;;;;
;;;; A deliberately minimal TLS 1.2 handshake sufficient for modern
;;;; clients. See manual/implement/333_tls-stack-completeness.md Phase 4.
;;;;
;;;;   * ECDHE only (secp256r1, x25519); no DHE, no static RSA
;;;;   * AEAD only: AES-128-GCM, AES-256-GCM, ChaCha20-Poly1305
;;;;   * Extended Master Secret (RFC 7627) REQUIRED
;;;;   * Renegotiation not supported: renegotiation_info sent empty
;;;;   * No compression
;;;;   * Server side only (edge proxy)
;;;;
;;;; The state machine is explicit and staged: a caller feeds a
;;;; ClientHello to TLS12-SERVER-START-HANDSHAKE, then feeds the next
;;;; client flight (ClientKeyExchange + ChangeCipherSpec + Finished)
;;;; to TLS12-SERVER-PROCESS-CLIENT-FLIGHT2, which returns the server
;;;; ChangeCipherSpec + Finished bytes to write to the peer. After that
;;;; the connection is in the :connected state and application data
;;;; uses TLS12-SEND-APPLICATION-DATA / TLS12-RECEIVE-APPLICATION-DATA.
;;;;
;;;; Session tickets (RFC 5077): when the server config carries a
;;;; STEK store, the server echoes an empty session_ticket extension
;;;; in ServerHello, then issues a NewSessionTicket handshake message
;;;; between its ChangeCipherSpec and Finished, sealed with the same
;;;; STEK store used by the TLS 1.3 ticket path.
;;;;
;;;; OCSP stapling (RFC 6066 §8): when the client sends a status_request
;;;; extension AND the server config carries a cached staple, the server
;;;; echoes an empty status_request extension in ServerHello and sends
;;;; a CertificateStatus handshake message (type 22) right after
;;;; Certificate. The staple bytes are opaque to this layer -- they are
;;;; produced by epsilon.proxy.ocsp-fetcher and cached on the tls-context.

(defpackage epsilon.crypto.tls12
  (:use :cl)
  (:import
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.sha512 sha512)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.aes-gcm aes-gcm)
   (epsilon.crypto.chacha20-poly1305 chacha)
   (epsilon.crypto.curve25519 x25519)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.tls-session-ticket-store stek)
   ;; Reuse the transport abstraction from tls13 so tls12 and tls13
   ;; can share fd/memory transports without duplication.
   (epsilon.crypto.tls13 tls13))
  (:export
   ;; Version
   #:+tls-1.2+
   ;; Content types
   #:+content-change-cipher-spec+
   #:+content-alert+
   #:+content-handshake+
   #:+content-application-data+
   ;; Handshake types
   #:+handshake-client-hello+
   #:+handshake-server-hello+
   #:+handshake-certificate+
   #:+handshake-server-key-exchange+
   #:+handshake-server-hello-done+
   #:+handshake-client-key-exchange+
   #:+handshake-finished+
   #:+handshake-new-session-ticket+
   #:+handshake-certificate-status+
   ;; Cipher suites
   #:+tls-ecdhe-ecdsa-aes-128-gcm-sha256+
   #:+tls-ecdhe-ecdsa-aes-256-gcm-sha384+
   #:+tls-ecdhe-ecdsa-chacha20-poly1305-sha256+
   #:+tls-ecdhe-rsa-aes-128-gcm-sha256+
   #:+tls-ecdhe-rsa-aes-256-gcm-sha384+
   #:+tls-ecdhe-rsa-chacha20-poly1305-sha256+
   ;; PRF
   #:tls12-prf
   ;; Record layer
   #:parse-tls-record
   #:serialize-tls-record
   #:encrypt-record
   #:decrypt-record
   ;; ClientHello
   #:parsed-client-hello
   #:parsed-client-hello-random
   #:parsed-client-hello-session-id
   #:parsed-client-hello-cipher-suites
   #:parsed-client-hello-hostname
   #:parsed-client-hello-alpn-protocols
   #:parsed-client-hello-signature-algorithms
   #:parsed-client-hello-supported-groups
   #:parsed-client-hello-extended-master-secret-p
   #:parsed-client-hello-renegotiation-info
   #:parsed-client-hello-status-request-p
   #:parsed-client-hello-session-ticket
   #:parse-client-hello
   ;; State machine
   #:tls12-server-config
   #:make-tls12-server-config
   #:tls12-server-config-cipher-suites
   #:tls12-server-config-certificate-chain
   #:tls12-server-config-private-key
   #:tls12-server-config-key-type
   #:tls12-server-config-alpn-protocols
   #:tls12-server-config-session-ticket-store
   #:tls12-server-config-session-ticket-lifetime
   #:tls12-server-config-ocsp-staple
   #:tls12-connection
   #:make-tls12-connection
   #:tls12-connection-state
   #:tls12-connection-cipher-suite
   #:tls12-connection-alpn-protocol
   #:tls12-connection-hostname
   #:tls12-server-start-handshake
   #:tls12-server-process-client-flight2
   #:tls12-accept
   ;; Client side
   #:tls12-client-config
   #:make-tls12-client-config
   #:tls12-client-config-hostname
   #:tls12-client-config-alpn-protocols
   #:tls12-client-config-cipher-suites
   #:tls12-client-config-trust-store
   #:tls12-connect
   #:build-client-hello
   #:tls12-client-start-handshake
   #:tls12-client-process-server-flight1
   #:tls12-client-finish-handshake
   #:tls12-stream
   #:tls12-stream-p
   #:make-tls12-stream
   #:tls12-stream-connection
   #:tls12-stream-closed-p
   #:tls12-read
   #:tls12-write
   #:tls12-close
   #:tls12-shutdown
   #:tls12-stream-alpn-protocol
   #:tls12-stream-cipher-suite
   #:tls12-stream-peer-certificates
   #:tls12-send-application-data
   #:tls12-receive-application-data
   ;; Alerts
   #:+alert-level-warning+
   #:+alert-level-fatal+
   #:+alert-close-notify+
   #:+alert-handshake-failure+
   #:+alert-illegal-parameter+
   #:+alert-decode-error+
   #:+alert-decrypt-error+
   #:+alert-protocol-version+
   #:+alert-internal-error+
   #:+alert-missing-extension+
   ;; Decode error condition
   #:tls12-decode-error
   #:tls12-decode-error-reason
   ;; Client-side parsers
   #:parse-server-hello
   #:parse-certificate-message
   #:parse-server-key-exchange
   #:parse-client-key-exchange
   ;; ServerHello struct (client-side)
   #:parsed-server-hello
   #:parsed-server-hello-random
   ;; ServerKeyExchange struct (client-side)
   #:parsed-ske
   #:parsed-ske-server-public
   ;; Handshake header
   #:parse-handshake-header))

(in-package :epsilon.crypto.tls12)

;;; ---------------------------------------------------------------------------
;;; Decode error condition
;;; ---------------------------------------------------------------------------

(define-condition tls12-decode-error (error)
  ((reason :initarg :reason :reader tls12-decode-error-reason))
  (:report (lambda (c s)
             (format s "TLS 1.2 decode_error: ~A" (tls12-decode-error-reason c))))
  (:documentation
   "Signaled when a TLS 1.2 parser reads past the end of a handshake payload
or otherwise detects malformed wire format. Matches the behavior RFC 5246
section 7.2.2 requires (the peer must respond with decode_error(50))."))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +tls-1.0+ #x0301)
(defconstant +tls-1.1+ #x0302)
(defconstant +tls-1.2+ #x0303)
(defconstant +tls-1.3-supported-version+ #x0304)

;; Record content types (shared with TLS 1.3)
(defconstant +content-change-cipher-spec+ 20)
(defconstant +content-alert+ 21)
(defconstant +content-handshake+ 22)
(defconstant +content-application-data+ 23)

;; Handshake message types (RFC 5246 §7.4)
(defconstant +handshake-client-hello+        1)
(defconstant +handshake-server-hello+        2)
(defconstant +handshake-new-session-ticket+  4)
(defconstant +handshake-certificate+         11)
(defconstant +handshake-server-key-exchange+ 12)
(defconstant +handshake-certificate-request+ 13)
(defconstant +handshake-server-hello-done+   14)
(defconstant +handshake-certificate-verify+  15)
(defconstant +handshake-client-key-exchange+ 16)
(defconstant +handshake-finished+            20)
(defconstant +handshake-certificate-status+  22)

;; Cipher suites (RFC 5289, RFC 7905)
(defconstant +tls-ecdhe-ecdsa-aes-128-gcm-sha256+        #xC02B)
(defconstant +tls-ecdhe-ecdsa-aes-256-gcm-sha384+        #xC02C)
(defconstant +tls-ecdhe-ecdsa-chacha20-poly1305-sha256+  #xCCA9)
(defconstant +tls-ecdhe-rsa-aes-128-gcm-sha256+          #xC02F)
(defconstant +tls-ecdhe-rsa-aes-256-gcm-sha384+          #xC030)
(defconstant +tls-ecdhe-rsa-chacha20-poly1305-sha256+    #xCCA8)

;; Extensions
(defconstant +ext-server-name+            0)
(defconstant +ext-status-request+         5)
(defconstant +ext-supported-groups+       10)
(defconstant +ext-ec-point-formats+       11)
(defconstant +ext-signature-algorithms+   13)
(defconstant +ext-alpn+                   16)
(defconstant +ext-extended-master-secret+ 23)
(defconstant +ext-session-ticket+         35)
(defconstant +ext-supported-versions+     43)
(defconstant +ext-renegotiation-info+     #xFF01)

;; Named curves / supported groups
(defconstant +group-secp256r1+      #x0017)
(defconstant +group-secp384r1+      #x0018)
(defconstant +group-x25519+         #x001D)

;; EC curve types (ServerKeyExchange)
(defconstant +ec-curve-type-named-curve+ 3)

;; EC point formats
(defconstant +ec-point-format-uncompressed+ 0)

;; SignatureAndHashAlgorithm (RFC 5246 §7.4.1.4.1 and TLS 1.3 values)
(defconstant +sig-rsa-pkcs1-sha256+         #x0401)
(defconstant +sig-rsa-pkcs1-sha384+         #x0501)
(defconstant +sig-ecdsa-secp256r1-sha256+   #x0403)
(defconstant +sig-rsa-pss-rsae-sha256+      #x0804)
(defconstant +sig-rsa-pss-rsae-sha384+      #x0805)

;; Alerts
(defconstant +alert-level-warning+ 1)
(defconstant +alert-level-fatal+ 2)
(defconstant +alert-close-notify+ 0)
(defconstant +alert-unexpected-message+ 10)
(defconstant +alert-bad-record-mac+ 20)
(defconstant +alert-handshake-failure+ 40)
(defconstant +alert-bad-certificate+ 42)
(defconstant +alert-illegal-parameter+ 47)
(defconstant +alert-decode-error+ 50)
(defconstant +alert-decrypt-error+ 51)
(defconstant +alert-protocol-version+ 70)
(defconstant +alert-internal-error+ 80)
(defconstant +alert-missing-extension+ 109)

;;; ---------------------------------------------------------------------------
;;; Byte buffer / reader helpers
;;; ---------------------------------------------------------------------------

(defun make-buffer (&optional (initial-size 256))
  (make-array initial-size :element-type '(unsigned-byte 8)
              :adjustable t :fill-pointer 0))

(defun buf-u8 (buf v)
  (vector-push-extend (logand v #xFF) buf))

(defun buf-u16 (buf v)
  (vector-push-extend (logand (ash v -8) #xFF) buf)
  (vector-push-extend (logand v #xFF) buf))

(defun buf-u24 (buf v)
  (vector-push-extend (logand (ash v -16) #xFF) buf)
  (vector-push-extend (logand (ash v -8) #xFF) buf)
  (vector-push-extend (logand v #xFF) buf))

(defun buf-u32 (buf v)
  (vector-push-extend (logand (ash v -24) #xFF) buf)
  (vector-push-extend (logand (ash v -16) #xFF) buf)
  (vector-push-extend (logand (ash v -8) #xFF) buf)
  (vector-push-extend (logand v #xFF) buf))

(defun buf-bytes (buf bytes)
  (loop for b across bytes do (vector-push-extend b buf)))

(defun buf-u8-prefixed (buf bytes)
  (buf-u8 buf (length bytes))
  (buf-bytes buf bytes))

(defun buf-u16-prefixed (buf bytes)
  (buf-u16 buf (length bytes))
  (buf-bytes buf bytes))

(defun buf-freeze (buf)
  (let ((r (make-array (length buf) :element-type '(unsigned-byte 8))))
    (replace r buf)
    r))

(defun rd-need (data pos n what)
  "Signal TLS12-DECODE-ERROR unless N bytes are available at POS in DATA."
  (unless (<= (+ pos n) (length data))
    (error 'tls12-decode-error
           :reason (format nil "~A: need ~D byte~:P at offset ~D, have ~D"
                           what n pos (- (length data) pos)))))

(defun rd-u8 (data pos &optional (what "u8"))
  (rd-need data pos 1 what)
  (values (aref data pos) (1+ pos)))

(defun rd-u16 (data pos &optional (what "u16"))
  (rd-need data pos 2 what)
  (values (logior (ash (aref data pos) 8) (aref data (1+ pos))) (+ pos 2)))

(defun rd-u24 (data pos &optional (what "u24"))
  (rd-need data pos 3 what)
  (values (logior (ash (aref data pos) 16)
                  (ash (aref data (1+ pos)) 8)
                  (aref data (+ pos 2)))
          (+ pos 3)))

(defun rd-bytes (data pos n &optional (what "bytes"))
  (rd-need data pos n what)
  (values (subseq data pos (+ pos n)) (+ pos n)))

(defun u16->bytes (v)
  (let ((out (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (logand (ash v -8) #xFF))
    (setf (aref out 1) (logand v #xFF))
    out))

(defun u64-be->bytes (v)
  (let ((out (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for i from 7 downto 0
          for x = v then (ash x -8)
          do (setf (aref out i) (logand x #xFF)))
    out))

(defun concat-bytes (&rest parts)
  (let* ((total (reduce #'+ parts :key #'length))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (p parts)
      (replace out p :start1 pos)
      (incf pos (length p)))
    out))

(defun zero-bytes (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---------------------------------------------------------------------------
;;; PRF (RFC 5246 §5)
;;;
;;;   P_hash(secret, seed) = HMAC(secret, A(1)||seed)
;;;                       || HMAC(secret, A(2)||seed)
;;;                       || ...
;;;   A(0) = seed
;;;   A(i) = HMAC(secret, A(i-1))
;;;
;;;   PRF(secret, label, seed) = P_hash(secret, label || seed)
;;; ---------------------------------------------------------------------------

(defun %hmac (hash-alg key data)
  (hmac:hmac hash-alg key data))

(defun p-hash (hash-alg secret seed length)
  "P_hash expansion. Returns LENGTH bytes."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (pos 0)
        (a seed))
    (loop while (< pos length)
          do (setf a (%hmac hash-alg secret a))
             (let* ((chunk (%hmac hash-alg secret (concat-bytes a seed)))
                    (take (min (length chunk) (- length pos))))
               (replace out chunk :start1 pos :end2 take)
               (incf pos take)))
    out))

(defun tls12-prf (hash-alg secret label seed length)
  "TLS 1.2 PRF. LABEL is a string literal, converted to ASCII bytes."
  (let* ((label-bytes (map '(vector (unsigned-byte 8)) #'char-code label))
         (full-seed (concat-bytes label-bytes seed)))
    (p-hash hash-alg secret full-seed length)))

;;; ---------------------------------------------------------------------------
;;; Cipher suite metadata
;;; ---------------------------------------------------------------------------

(defstruct cipher-suite-info
  (hash-alg :sha256)        ; for PRF and Finished
  (key-len 16)              ; write key length
  (fixed-iv-len 4)          ; salt / implicit IV length
  (explicit-nonce-len 8)    ; explicit nonce on wire (0 for chacha)
  (aead :aes-gcm)           ; :aes-gcm or :chacha20-poly1305
  (auth :ecdsa))            ; :ecdsa or :rsa

(defun cipher-suite-meta (suite)
  (cond
    ((= suite +tls-ecdhe-ecdsa-aes-128-gcm-sha256+)
     (make-cipher-suite-info :hash-alg :sha256 :key-len 16
                             :aead :aes-gcm :auth :ecdsa))
    ((= suite +tls-ecdhe-ecdsa-aes-256-gcm-sha384+)
     (make-cipher-suite-info :hash-alg :sha384 :key-len 32
                             :aead :aes-gcm :auth :ecdsa))
    ((= suite +tls-ecdhe-ecdsa-chacha20-poly1305-sha256+)
     (make-cipher-suite-info :hash-alg :sha256 :key-len 32
                             :fixed-iv-len 12 :explicit-nonce-len 0
                             :aead :chacha20-poly1305 :auth :ecdsa))
    ((= suite +tls-ecdhe-rsa-aes-128-gcm-sha256+)
     (make-cipher-suite-info :hash-alg :sha256 :key-len 16
                             :aead :aes-gcm :auth :rsa))
    ((= suite +tls-ecdhe-rsa-aes-256-gcm-sha384+)
     (make-cipher-suite-info :hash-alg :sha384 :key-len 32
                             :aead :aes-gcm :auth :rsa))
    ((= suite +tls-ecdhe-rsa-chacha20-poly1305-sha256+)
     (make-cipher-suite-info :hash-alg :sha256 :key-len 32
                             :fixed-iv-len 12 :explicit-nonce-len 0
                             :aead :chacha20-poly1305 :auth :rsa))
    (t nil)))

(defun cipher-suite-supported-p (suite)
  (not (null (cipher-suite-meta suite))))

;;; ---------------------------------------------------------------------------
;;; Transcript hash
;;;
;;; TLS 1.2 uses the cipher-suite PRF hash for both the Finished
;;; verify_data and (via EMS) the session_hash.
;;; ---------------------------------------------------------------------------

(defun %make-hash-state (hash-alg)
  (ecase hash-alg
    (:sha256 (sha256:make-sha256-state))
    (:sha384 (sha512:make-sha384-state))))

(defun %hash-update (hash-alg state data)
  (ecase hash-alg
    (:sha256 (sha256:sha256-update state data))
    (:sha384 (sha512:sha384-update state data))))

(defun %hash-snapshot (hash-alg state)
  "Finalize a *copy* of STATE and return the digest bytes."
  (ecase hash-alg
    (:sha256
     (let ((c (sha256:sha256-copy state)))
       (sha256:sha256-finalize c)))
    (:sha384
     (let ((c (sha512:sha384-copy state)))
       (sha512:sha384-finalize c)))))

;;; ---------------------------------------------------------------------------
;;; Record layer
;;; ---------------------------------------------------------------------------

(defstruct tls-record
  (content-type 0 :type (unsigned-byte 8))
  (version +tls-1.2+ :type fixnum)
  (data (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*))))

(defun serialize-tls-record (record)
  (let* ((data (tls-record-data record))
         (len (length data))
         (out (make-array (+ 5 len) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (tls-record-content-type record))
    (setf (aref out 1) (ash (tls-record-version record) -8))
    (setf (aref out 2) (logand (tls-record-version record) #xFF))
    (setf (aref out 3) (ash len -8))
    (setf (aref out 4) (logand len #xFF))
    (replace out data :start1 5)
    out))

(defun parse-tls-record (data &optional (pos 0))
  (when (< (- (length data) pos) 5)
    (return-from parse-tls-record nil))
  (let ((ct (aref data pos))
        (ver (logior (ash (aref data (+ pos 1)) 8) (aref data (+ pos 2))))
        (len (logior (ash (aref data (+ pos 3)) 8) (aref data (+ pos 4)))))
    (when (> len 18432)   ; 2^14 + 2048 ceiling per RFC 5246 §6.2.3
      (error "TLS record too large: ~D bytes" len))
    (when (< (- (length data) pos) (+ 5 len))
      (return-from parse-tls-record nil))
    (values
     (make-tls-record :content-type ct :version ver
                      :data (subseq data (+ pos 5) (+ pos 5 len)))
     (+ pos 5 len))))

;;; AEAD record layer (RFC 5246 §6.2.3.3, RFC 5288 for GCM, RFC 7905 for
;;; ChaCha20). For GCM the wire format is:
;;;
;;;     explicit_nonce(8) || ciphertext || tag(16)
;;;
;;; with the 12-byte AEAD nonce = fixed_iv(4) || explicit_nonce(8).
;;; For ChaCha20-Poly1305 there is no explicit nonce on the wire; the
;;; 12-byte nonce is the fixed_iv XORed with the 8-byte sequence number
;;; (right-aligned).
;;;
;;; AAD = seq_num(8) || type(1) || legacy_version(2) || plaintext_len(2)

(defun make-aad (seq-num content-type version plaintext-len)
  (let ((out (make-array 13 :element-type '(unsigned-byte 8))))
    (replace out (u64-be->bytes seq-num))
    (setf (aref out 8) content-type)
    (setf (aref out 9) (logand (ash version -8) #xFF))
    (setf (aref out 10) (logand version #xFF))
    (setf (aref out 11) (logand (ash plaintext-len -8) #xFF))
    (setf (aref out 12) (logand plaintext-len #xFF))
    out))

(defun xor-iv-with-seq (fixed-iv seq-num)
  "RFC 7905-style nonce: 12-byte IV XORed with 8-byte right-aligned seq."
  (let ((n (copy-seq fixed-iv)))
    (loop for i from 11 downto 4
          for shift from 0 by 8
          do (setf (aref n i)
                   (logxor (aref n i)
                           (logand (ash seq-num (- shift)) #xFF))))
    n))

(defun encrypt-record (plaintext content-type write-key fixed-iv seq-num info)
  "Return the record body (explicit_nonce || ciphertext || tag) for AEAD."
  (let* ((aead (cipher-suite-info-aead info))
         (plen (length plaintext))
         (aad (make-aad seq-num content-type +tls-1.2+ plen)))
    (ecase aead
      (:aes-gcm
       (let* ((explicit (u64-be->bytes seq-num))
              (nonce (concat-bytes fixed-iv explicit)))
         (multiple-value-bind (ct tag)
             (aes-gcm:aes-gcm-encrypt plaintext write-key nonce :aad aad)
           (concat-bytes explicit ct tag))))
      (:chacha20-poly1305
       (let ((nonce (xor-iv-with-seq fixed-iv seq-num)))
         (multiple-value-bind (ct tag)
             (chacha:chacha20-poly1305-encrypt plaintext write-key nonce :aad aad)
           (concat-bytes ct tag)))))))

(defun decrypt-record (body content-type read-key fixed-iv seq-num info)
  "Decrypt a TLS 1.2 AEAD record body. Returns the plaintext or errors."
  (let ((aead (cipher-suite-info-aead info)))
    (ecase aead
      (:aes-gcm
       (when (< (length body) (+ 8 16))
         (error "AES-GCM record too short"))
       (let* ((explicit (subseq body 0 8))
              (ct-end (- (length body) 16))
              (ct (subseq body 8 ct-end))
              (tag (subseq body ct-end))
              (nonce (concat-bytes fixed-iv explicit))
              (aad (make-aad seq-num content-type +tls-1.2+ (length ct)))
              (pt (aes-gcm:aes-gcm-decrypt ct read-key nonce tag :aad aad)))
         (unless pt (error "AEAD decryption failed"))
         pt))
      (:chacha20-poly1305
       (when (< (length body) 16)
         (error "ChaCha20-Poly1305 record too short"))
       (let* ((ct-end (- (length body) 16))
              (ct (subseq body 0 ct-end))
              (tag (subseq body ct-end))
              (nonce (xor-iv-with-seq fixed-iv seq-num))
              (aad (make-aad seq-num content-type +tls-1.2+ (length ct)))
              (pt (chacha:chacha20-poly1305-decrypt ct read-key nonce tag :aad aad)))
         (unless pt (error "AEAD decryption failed"))
         pt)))))

;;; ---------------------------------------------------------------------------
;;; Handshake message framing
;;; ---------------------------------------------------------------------------

(defun make-handshake-message (type payload)
  (let* ((len (length payload))
         (out (make-array (+ 4 len) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) type)
    (setf (aref out 1) (logand (ash len -16) #xFF))
    (setf (aref out 2) (logand (ash len -8) #xFF))
    (setf (aref out 3) (logand len #xFF))
    (replace out payload :start1 4)
    out))

(defun parse-handshake-header (data &optional (pos 0))
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
;;; ClientHello parser
;;; ---------------------------------------------------------------------------

(defstruct parsed-client-hello
  (legacy-version 0 :type fixnum)
  (random nil)
  (session-id nil)
  (cipher-suites nil :type list)
  (compression-methods nil :type list)
  (hostname nil)
  (alpn-protocols nil :type list)
  (signature-algorithms nil :type list)
  (supported-groups nil :type list)
  (key-share-groups nil :type list)    ; if TLS 1.3 supported_versions present
  (extended-master-secret-p nil)
  (renegotiation-info nil)             ; the raw extension data (may be #())
  (status-request-p nil)
  (session-ticket nil)                 ; raw ticket bytes or #()
  (supported-versions nil :type list)
  (ec-point-formats nil :type list))

(defun parse-client-hello (payload)
  "Parse a ClientHello handshake payload (without the 4-byte header).
All readers signal TLS12-DECODE-ERROR on truncated input, matching the
behavior RFC 5246 section 7.2.2 requires (decode_error(50))."
  (let ((pos 0)
        (ch (make-parsed-client-hello)))
    ;; legacy_version
    (multiple-value-bind (v p) (rd-u16 payload pos "legacy_version")
      (setf (parsed-client-hello-legacy-version ch) v pos p))
    ;; random
    (multiple-value-bind (r p) (rd-bytes payload pos 32 "random")
      (setf (parsed-client-hello-random ch) r pos p))
    ;; session_id
    (multiple-value-bind (sid-len p) (rd-u8 payload pos "session_id length")
      (setf pos p)
      (multiple-value-bind (sid p2) (rd-bytes payload pos sid-len "session_id")
        (setf (parsed-client-hello-session-id ch) sid pos p2)))
    ;; cipher_suites
    (multiple-value-bind (cs-len p) (rd-u16 payload pos "cipher_suites length")
      (setf pos p)
      (rd-need payload pos cs-len "cipher_suites")
      (let ((end (+ pos cs-len)))
        (loop while (< pos end)
              do (multiple-value-bind (cs p2) (rd-u16 payload pos "cipher_suite")
                   (push cs (parsed-client-hello-cipher-suites ch))
                   (setf pos p2)))
        (setf (parsed-client-hello-cipher-suites ch)
              (nreverse (parsed-client-hello-cipher-suites ch)))))
    ;; compression_methods
    (multiple-value-bind (cm-len p) (rd-u8 payload pos "compression_methods length")
      (setf pos p)
      (rd-need payload pos cm-len "compression_methods")
      (let ((end (+ pos cm-len)))
        (loop while (< pos end)
              do (multiple-value-bind (cm p2) (rd-u8 payload pos "compression_method")
                   (push cm (parsed-client-hello-compression-methods ch))
                   (setf pos p2)))
        (setf (parsed-client-hello-compression-methods ch)
              (nreverse (parsed-client-hello-compression-methods ch)))))
    ;; extensions (may be absent if end of payload)
    (when (< pos (length payload))
      (multiple-value-bind (ext-len p) (rd-u16 payload pos "extensions length")
        (setf pos p)
        (rd-need payload pos ext-len "extensions")
        (let ((end (+ pos ext-len)))
          (loop while (< pos end)
                do (multiple-value-bind (etype p2) (rd-u16 payload pos "extension type")
                     (setf pos p2)
                     (multiple-value-bind (elen p3) (rd-u16 payload pos "extension length")
                       (setf pos p3)
                       (multiple-value-bind (edata p4)
                           (rd-bytes payload pos elen "extension data")
                         (parse-extension ch etype edata)
                         (setf pos p4))))))))
    ch))

(defun parse-extension (ch etype data)
  (let ((dlen (length data)))
    (cond
      ;; SNI
      ((= etype +ext-server-name+)
       (when (>= dlen 5)
         ;; list-length(2) name-type(1) name-length(2) name...
         (let* ((name-type (aref data 2))
                (name-len (logior (ash (aref data 3) 8) (aref data 4))))
           (when (and (zerop name-type) (<= (+ 5 name-len) dlen))
             (setf (parsed-client-hello-hostname ch)
                   (map 'string #'code-char
                        (subseq data 5 (+ 5 name-len))))))))
      ;; supported_groups
      ((= etype +ext-supported-groups+)
       (when (>= dlen 2)
         (let ((pos 2))
           (loop while (<= (+ pos 2) dlen)
                 do (push (logior (ash (aref data pos) 8) (aref data (1+ pos)))
                          (parsed-client-hello-supported-groups ch))
                    (incf pos 2))
           (setf (parsed-client-hello-supported-groups ch)
                 (nreverse (parsed-client-hello-supported-groups ch))))))
      ;; ec_point_formats
      ((= etype +ext-ec-point-formats+)
       (when (>= dlen 1)
         (let ((n (min (aref data 0) (1- dlen))))
           (loop for i from 0 below n
                 do (push (aref data (+ 1 i))
                          (parsed-client-hello-ec-point-formats ch)))
           (setf (parsed-client-hello-ec-point-formats ch)
                 (nreverse (parsed-client-hello-ec-point-formats ch))))))
      ;; signature_algorithms
      ((= etype +ext-signature-algorithms+)
       (when (>= dlen 2)
         (let ((pos 2))
           (loop while (<= (+ pos 2) dlen)
                 do (push (logior (ash (aref data pos) 8) (aref data (1+ pos)))
                          (parsed-client-hello-signature-algorithms ch))
                    (incf pos 2))
           (setf (parsed-client-hello-signature-algorithms ch)
                 (nreverse (parsed-client-hello-signature-algorithms ch))))))
      ;; ALPN
      ((= etype +ext-alpn+)
       (when (>= dlen 2)
         (let ((pos 2))
           (loop while (< pos dlen)
                 do (let ((plen (aref data pos)))
                      (incf pos)
                      (when (> (+ pos plen) dlen)
                        (error 'tls12-decode-error
                               :reason "ALPN protocol name overruns extension"))
                      (push (map 'string #'code-char
                                 (subseq data pos (+ pos plen)))
                            (parsed-client-hello-alpn-protocols ch))
                      (incf pos plen)))
           (setf (parsed-client-hello-alpn-protocols ch)
                 (nreverse (parsed-client-hello-alpn-protocols ch))))))
      ;; extended_master_secret (empty extension body)
      ((= etype +ext-extended-master-secret+)
       (setf (parsed-client-hello-extended-master-secret-p ch) t))
      ;; renegotiation_info
      ((= etype +ext-renegotiation-info+)
       (setf (parsed-client-hello-renegotiation-info ch) data))
      ;; status_request (OCSP)
      ((= etype +ext-status-request+)
       (setf (parsed-client-hello-status-request-p ch) t))
      ;; session_ticket (RFC 5077)
      ((= etype +ext-session-ticket+)
       (setf (parsed-client-hello-session-ticket ch) data))
      ;; supported_versions (RFC 8446) - present if client also offers 1.3
      ((= etype +ext-supported-versions+)
       (when (>= dlen 1)
         (let ((pos 1))
           (loop while (<= (+ pos 2) dlen)
                 do (push (logior (ash (aref data pos) 8) (aref data (1+ pos)))
                          (parsed-client-hello-supported-versions ch))
                    (incf pos 2))
           (setf (parsed-client-hello-supported-versions ch)
                 (nreverse (parsed-client-hello-supported-versions ch))))))
      ;; Other extensions: ignore per RFC 5246.
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; ServerHello builder
;;; ---------------------------------------------------------------------------

(defun build-server-hello-payload (server-random session-id cipher-suite
                                   &key alpn extended-master-secret-p
                                        session-ticket-advertised-p
                                        status-request-advertised-p)
  "Build a ServerHello payload. When session_ticket or status_request
   is advertised, an empty extension body is emitted here and the
   associated payload handshake messages (NewSessionTicket /
   CertificateStatus) are emitted elsewhere in the flight."
  (let ((buf (make-buffer)))
    (buf-u16 buf +tls-1.2+)
    (buf-bytes buf server-random)
    (buf-u8-prefixed buf session-id)
    (buf-u16 buf cipher-suite)
    (buf-u8 buf 0)                      ; compression_method = null
    ;; Extensions
    (let ((ext-buf (make-buffer)))
      ;; renegotiation_info: empty ri (we don't support renegotiation)
      (buf-u16 ext-buf +ext-renegotiation-info+)
      (buf-u16 ext-buf 1)               ; ext data length
      (buf-u8  ext-buf 0)               ; renegotiated_connection length
      ;; extended_master_secret (echo; required)
      (when extended-master-secret-p
        (buf-u16 ext-buf +ext-extended-master-secret+)
        (buf-u16 ext-buf 0))
      ;; ec_point_formats (uncompressed only)
      (buf-u16 ext-buf +ext-ec-point-formats+)
      (buf-u16 ext-buf 2)
      (buf-u8  ext-buf 1)
      (buf-u8  ext-buf +ec-point-format-uncompressed+)
      ;; ALPN
      (when alpn
        (buf-u16 ext-buf +ext-alpn+)
        (let ((proto-bytes (map '(vector (unsigned-byte 8)) #'char-code alpn)))
          (buf-u16 ext-buf (+ 3 (length proto-bytes))) ; extension body length
          (buf-u16 ext-buf (+ 1 (length proto-bytes))) ; list length
          (buf-u8-prefixed ext-buf proto-bytes)))
      ;; session_ticket (empty, advertises willingness) -- STUB for Phase 2
      (when session-ticket-advertised-p
        (buf-u16 ext-buf +ext-session-ticket+)
        (buf-u16 ext-buf 0))
      ;; status_request echo (empty) -- STUB for Phase 3
      (when status-request-advertised-p
        (buf-u16 ext-buf +ext-status-request+)
        (buf-u16 ext-buf 0))
      (buf-u16-prefixed buf (buf-freeze ext-buf)))
    (buf-freeze buf)))

;;; ---------------------------------------------------------------------------
;;; Certificate message (TLS 1.2 form: no context, no per-entry extensions)
;;; ---------------------------------------------------------------------------

(defun build-certificate-message (cert-chain)
  (let ((buf (make-buffer))
        (list-buf (make-buffer)))
    (dolist (cert-der cert-chain)
      (buf-u24 list-buf (length cert-der))
      (buf-bytes list-buf cert-der))
    (let ((list-bytes (buf-freeze list-buf)))
      (buf-u24 buf (length list-bytes))
      (buf-bytes buf list-bytes))
    (make-handshake-message +handshake-certificate+ (buf-freeze buf))))

;;; ---------------------------------------------------------------------------
;;; ServerKeyExchange (ECDHE)
;;;
;;;   struct {
;;;     ECParameters curve_params;     -- curve_type(1) + namedcurve(2)
;;;     ECPoint      public;           -- opaque point<1..255>
;;;     SignatureAndHashAlgorithm alg; -- 2 bytes (TLS 1.2)
;;;     opaque signature<0..2^16-1>;
;;;   }
;;;
;;; The signature is over client_random || server_random || curve_params
;;; || public (where curve_params/public are the wire bytes above).
;;; ---------------------------------------------------------------------------

(defun select-ecdhe-group (client-groups)
  "Choose an ECDHE group we support from the client's advertised list."
  (cond ((member +group-x25519+ client-groups) +group-x25519+)
        ((member +group-secp256r1+ client-groups) +group-secp256r1+)
        (t nil)))

(defun ecdhe-generate (group)
  "Generate an ephemeral keypair for GROUP.
   Returns (values private-key wire-public-bytes)."
  (cond
    ((= group +group-x25519+)
     (let* ((sk (drbg:random-bytes 32))
            (pk (x25519:x25519-base sk)))
       (values sk pk)))
    ((= group +group-secp256r1+)
     (multiple-value-bind (sk pk-pt) (ecdh:ecdh-p256-generate-keypair)
       (values sk (ec-p256:p256-point-encode-uncompressed pk-pt))))
    (t (error "Unsupported ECDHE group: ~X" group))))

(defun ecdhe-derive (group server-sk client-pk-bytes)
  "Derive the pre-master secret from the peer's public key."
  (cond
    ((= group +group-x25519+)
     (x25519:x25519 server-sk client-pk-bytes))
    ((= group +group-secp256r1+)
     (let ((pt (ec-p256:p256-point-decode client-pk-bytes)))
       (ecdh:ecdh-p256-shared-secret server-sk pt)))
    (t (error "Unsupported ECDHE group: ~X" group))))

(defun choose-signature-scheme (key-type client-sigs)
  "Pick a signature_and_hash_algorithm we can use for ServerKeyExchange."
  (ecase key-type
    (:ecdsa-p256
     (if (or (null client-sigs)
             (member +sig-ecdsa-secp256r1-sha256+ client-sigs))
         +sig-ecdsa-secp256r1-sha256+
         (error "Client does not accept ecdsa_secp256r1_sha256")))
    (:rsa
     (cond ((or (null client-sigs)
                (member +sig-rsa-pss-rsae-sha256+ client-sigs))
            +sig-rsa-pss-rsae-sha256+)
           ((member +sig-rsa-pkcs1-sha256+ client-sigs)
            +sig-rsa-pkcs1-sha256+)
           (t (error "Client does not accept an RSA scheme we support"))))))

(defun sign-ske (scheme private-key to-sign)
  (cond
    ((= scheme +sig-ecdsa-secp256r1-sha256+)
     (multiple-value-bind (r s) (ecdsa:ecdsa-sign private-key to-sign :hash :sha256)
       (asn1:der-encode-sequence
        (asn1:der-encode-integer r)
        (asn1:der-encode-integer s))))
    ((= scheme +sig-rsa-pss-rsae-sha256+)
     (rsa:rsa-pss-sign private-key to-sign :hash :sha256))
    ((= scheme +sig-rsa-pkcs1-sha256+)
     (rsa:pkcs1-v15-sign private-key to-sign :hash :sha256))
    (t (error "Unsupported signature scheme: ~X" scheme))))

(defun build-server-key-exchange (client-random server-random
                                  group server-pk-bytes
                                  key-type private-key client-sigs)
  "Build ServerKeyExchange for an ECDHE_{ECDSA,RSA} cipher suite."
  (let* ((params-buf (make-buffer)))
    (buf-u8  params-buf +ec-curve-type-named-curve+)
    (buf-u16 params-buf group)
    (buf-u8-prefixed params-buf server-pk-bytes)
    (let* ((params-bytes (buf-freeze params-buf))
           (to-sign (concat-bytes client-random server-random params-bytes))
           (scheme (choose-signature-scheme key-type client-sigs))
           (signature (sign-ske scheme private-key to-sign))
           (buf (make-buffer)))
      (buf-bytes buf params-bytes)
      (buf-u16 buf scheme)
      (buf-u16-prefixed buf signature)
      (make-handshake-message +handshake-server-key-exchange+ (buf-freeze buf)))))

;;; ---------------------------------------------------------------------------
;;; CertificateRequest (RFC 5246 §7.4.4 + RFC 5246 sigalg-ext §7.4.1.4.1)
;;; ---------------------------------------------------------------------------

;; OpenSSL-compatible verify-mode bits (mirrored across tls12 / tls13 /
;; epsilon.crypto so the native bridge can forward the integer).
(defconstant +tls12-verify-peer+ 1)
(defconstant +tls12-verify-fail-if-no-peer-cert+ 2)

(declaim (inline %verify-peer-p-tls12 %verify-fail-if-missing-p-tls12))
(defun %verify-peer-p-tls12 (mode)
  (logtest mode +tls12-verify-peer+))
(defun %verify-fail-if-missing-p-tls12 (mode)
  (logtest mode +tls12-verify-fail-if-no-peer-cert+))

;; ClientCertificateType codepoints (RFC 5246 §7.4.4 + RFC 4492 §5.5).
(defconstant +client-cert-type-rsa-sign+   1)
(defconstant +client-cert-type-ecdsa-sign+ 64)

(defun build-tls12-certificate-request-message ()
  "Build a CertificateRequest message announcing rsa_sign + ecdsa_sign
   client cert types and the same SignatureAndHashAlgorithm set the
   server selects ServerKeyExchange schemes from. Empty
   certificate_authorities list -- the application authorises off the
   cert fingerprint, not the chain."
  (let ((buf (make-buffer)))
    ;; certificate_types<1..2^8-1>
    (let ((types (make-buffer)))
      (buf-u8 types +client-cert-type-rsa-sign+)
      (buf-u8 types +client-cert-type-ecdsa-sign+)
      (buf-u8-prefixed buf (buf-freeze types)))
    ;; supported_signature_algorithms<2^16-1>: each scheme is a 2-byte
    ;; SignatureAndHashAlgorithm. We mirror the same set the server
    ;; advertises in its ClientHello on the client side.
    (let ((sigalgs (make-buffer)))
      (buf-u16 sigalgs +sig-ecdsa-secp256r1-sha256+)
      (buf-u16 sigalgs +sig-rsa-pss-rsae-sha256+)
      (buf-u16 sigalgs +sig-rsa-pkcs1-sha256+)
      (buf-u16-prefixed buf (buf-freeze sigalgs)))
    ;; certificate_authorities<0..2^16-1>: empty.
    (buf-u16 buf 0)
    (make-handshake-message +handshake-certificate-request+ (buf-freeze buf))))

(defun build-server-hello-done ()
  (make-handshake-message +handshake-server-hello-done+
                          (make-array 0 :element-type '(unsigned-byte 8))))

;;; ---------------------------------------------------------------------------
;;; CertificateStatus (RFC 6066 §8)
;;;
;;;   struct {
;;;     CertificateStatusType status_type;   -- u8, 1 = ocsp
;;;     select (status_type) {
;;;       case ocsp: OCSPResponse;           -- opaque<1..2^24-1>
;;;     } response;
;;;   } CertificateStatus;
;;; ---------------------------------------------------------------------------

(defun build-certificate-status (ocsp-response)
  "Wrap OCSP-RESPONSE bytes in a CertificateStatus handshake message."
  (let* ((rlen (length ocsp-response))
         (payload (make-array (+ 1 3 rlen) :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) 1)                          ; status_type = ocsp
    (setf (aref payload 1) (logand (ash rlen -16) #xFF))
    (setf (aref payload 2) (logand (ash rlen -8) #xFF))
    (setf (aref payload 3) (logand rlen #xFF))
    (replace payload ocsp-response :start1 4)
    (make-handshake-message +handshake-certificate-status+ payload)))

;;; ---------------------------------------------------------------------------
;;; NewSessionTicket (RFC 5077 §3.3, TLS 1.2 shape)
;;;
;;;   struct {
;;;     uint32 ticket_lifetime_hint;
;;;     opaque ticket<0..2^16-1>;
;;;   } NewSessionTicket;
;;;
;;; Unlike TLS 1.3, the TLS 1.2 message has no nonce, no age_add, and no
;;; extensions. The handshake type constant (4) is shared with TLS 1.3.
;;; ---------------------------------------------------------------------------

(defun build-new-session-ticket-tls12 (lifetime sealed-ticket)
  (let ((buf (make-buffer)))
    (buf-u32 buf lifetime)
    (buf-u16-prefixed buf sealed-ticket)
    (make-handshake-message +handshake-new-session-ticket+ (buf-freeze buf))))

;;; ---------------------------------------------------------------------------
;;; ClientKeyExchange parsing (ECDHE): a single ECPoint<1..255>
;;; ---------------------------------------------------------------------------

(defun parse-client-key-exchange (payload)
  "Return the client's ephemeral public-key bytes."
  (multiple-value-bind (len p) (rd-u8 payload 0 "cke point length")
    (multiple-value-bind (point p2) (rd-bytes payload p len "cke point")
      (declare (ignore p2))
      point)))

;;; ---------------------------------------------------------------------------
;;; Key derivation: master secret + key block
;;; ---------------------------------------------------------------------------

(defun derive-master-secret-standard (info pre-master-secret client-random server-random)
  "RFC 5246 Section 8.1:
     master_secret = PRF(pms, \"master secret\", ClientHello.random + ServerHello.random)[0..47]"
  (let ((seed (make-array 64 :element-type '(unsigned-byte 8))))
    (replace seed client-random)
    (replace seed server-random :start1 32)
    (tls12-prf (cipher-suite-info-hash-alg info)
               pre-master-secret
               "master secret"
               seed
               48)))

(defun derive-master-secret-ems (info pre-master-secret session-hash)
  "Extended Master Secret (RFC 7627):
     master_secret = PRF(pms, \"extended master secret\", session_hash, 48)"
  (tls12-prf (cipher-suite-info-hash-alg info)
             pre-master-secret
             "extended master secret"
             session-hash
             48))

(defstruct key-material
  client-write-key
  server-write-key
  client-write-iv    ; fixed IV / salt
  server-write-iv)

(defun derive-key-block (info master-secret client-random server-random)
  "key_block = PRF(master, \"key expansion\", server_random || client_random, N)
   where N = 2 * (key_len + fixed_iv_len). No MAC keys for AEAD."
  (let* ((hash-alg (cipher-suite-info-hash-alg info))
         (key-len (cipher-suite-info-key-len info))
         (iv-len (cipher-suite-info-fixed-iv-len info))
         (total (* 2 (+ key-len iv-len)))
         (seed (concat-bytes server-random client-random))
         (block (tls12-prf hash-alg master-secret "key expansion" seed total))
         (km (make-key-material))
         (pos 0))
    (setf (key-material-client-write-key km) (subseq block pos (+ pos key-len)))
    (incf pos key-len)
    (setf (key-material-server-write-key km) (subseq block pos (+ pos key-len)))
    (incf pos key-len)
    (setf (key-material-client-write-iv km) (subseq block pos (+ pos iv-len)))
    (incf pos iv-len)
    (setf (key-material-server-write-iv km) (subseq block pos (+ pos iv-len)))
    km))

;;; ---------------------------------------------------------------------------
;;; Finished
;;; ---------------------------------------------------------------------------

(defun build-finished-verify-data (info master-secret label handshake-hash)
  "verify_data = PRF(master_secret, finished_label, Hash(handshake_messages), 12)"
  (tls12-prf (cipher-suite-info-hash-alg info)
             master-secret label handshake-hash 12))

(defun build-finished-message (info master-secret handshake-hash)
  (let ((vd (build-finished-verify-data info master-secret "server finished"
                                        handshake-hash)))
    (make-handshake-message +handshake-finished+ vd)))

(defun verify-client-finished (info master-secret handshake-hash payload)
  (let ((expected (build-finished-verify-data info master-secret "client finished"
                                              handshake-hash)))
    (unless (and (= (length payload) (length expected))
                 (equalp payload expected))
      (error "Client Finished verification failed"))))

;;; ---------------------------------------------------------------------------
;;; Connection state
;;; ---------------------------------------------------------------------------

(defstruct tls12-server-config
  (cipher-suites
   (list +tls-ecdhe-ecdsa-aes-128-gcm-sha256+
         +tls-ecdhe-ecdsa-chacha20-poly1305-sha256+
         +tls-ecdhe-ecdsa-aes-256-gcm-sha384+
         +tls-ecdhe-rsa-aes-128-gcm-sha256+
         +tls-ecdhe-rsa-chacha20-poly1305-sha256+
         +tls-ecdhe-rsa-aes-256-gcm-sha384+)
   :type list)
  (certificate-chain nil :type list)   ; list of DER bytes
  (private-key nil)
  (key-type :ecdsa-p256)               ; :ecdsa-p256 or :rsa
  (alpn-protocols nil :type list)
  ;; When non-NIL, the server echoes session_ticket in ServerHello and
  ;; issues a RFC 5077 NewSessionTicket between its CCS and Finished,
  ;; sealed with this STEK store. Matches the slot name on the TLS 1.3
  ;; tls-server-config so the crypto bridge can hand both paths the
  ;; same store.
  (session-ticket-store nil)
  (session-ticket-lifetime 86400 :type integer)
  ;; Cached OCSP response bytes (opaque DER). When non-NIL and the
  ;; client sent a status_request extension, the server echoes the
  ;; extension in ServerHello and sends a CertificateStatus handshake
  ;; message (RFC 6066 §8) immediately after Certificate. Populated by
  ;; the proxy's OCSP fetcher via the tls-context.
  (ocsp-staple nil)
  ;; mTLS verify mode. 0 = no client cert request (default).
  ;; 1 (+verify-peer+) = send CertificateRequest; accept handshakes
  ;; where the client returns an empty Certificate (the application
  ;; authorises off the leaf cert fingerprint).
  ;; 1|2 (+verify-peer+ | +verify-fail-if-no-peer-cert+) = require a
  ;; cert; abort with a TLS alert when the client returns empty.
  ;; Bit values match epsilon.crypto's OpenSSL-compatible flags so the
  ;; native bridge can forward the integer verbatim.
  (verify-mode 0 :type integer))

(defstruct tls12-connection
  (role :server)
  (config nil)
  (state :init)
  (cipher-suite 0 :type fixnum)
  (info nil)
  (hostname nil)
  (alpn-protocol nil)
  (client-random nil)
  (server-random nil)
  ;; ephemeral ECDHE
  (ecdhe-group 0 :type fixnum)
  (ecdhe-private nil)
  (ecdhe-server-public nil)
  ;; Transcript hash state (cipher-suite PRF hash)
  (transcript-state nil)
  (use-ems-p nil :type boolean)
  (ems-session-hash nil)
  ;; Secrets and keys
  (master-secret nil)
  (key-material nil)
  ;; Record layer sequence numbers (reset at each CCS).
  ;; Role-neutral: READ-SEQ counts records we read from the peer,
  ;; WRITE-SEQ counts records we write to the peer.
  (read-seq 0 :type integer)
  (write-seq 0 :type integer)
  ;; True when we committed to issuing a NewSessionTicket in flight 2.
  ;; Decided at flight 1 time (the decision is encoded in the echoed
  ;; session_ticket ServerHello extension, so flight 2 must honor it).
  (issue-session-ticket-p nil)
  ;; mTLS server-side bookkeeping: cert-requested-p flips when the
  ;; server emitted a CertificateRequest in flight 1; server-peer-certs
  ;; holds the leaf cert chain (DER bytes, matching the TLS 1.2 client
  ;; convention) received in flight 2 or NIL on the soft-mode-empty
  ;; path; transcript-bytes-pre-cv captures the concatenated raw
  ;; handshake_messages bytes up to but not including the client's
  ;; CertificateVerify, since the CV signature is over RAW bytes (the
  ;; digital-signature primitive hashes internally per the scheme's
  ;; hash alg) -- not over the rolling transcript hash itself.
  (cert-requested-p nil :type boolean)
  (server-peer-certs nil :type list)
  ;; Mirror of the rolling transcript hash, but as raw bytes. Only
  ;; populated when cert-requested-p is true (the only path that
  ;; needs the raw handshake_messages -- TLS 1.2 client
  ;; CertificateVerify signs the raw bytes, not the digest, per RFC
  ;; 5246 §7.4.8 + §4.7).
  (transcript-raw-buf nil))

(defun transcript-init (conn)
  (setf (tls12-connection-transcript-state conn)
        (%make-hash-state (cipher-suite-info-hash-alg (tls12-connection-info conn)))))

(defun transcript-update (conn data)
  (%hash-update (cipher-suite-info-hash-alg (tls12-connection-info conn))
                (tls12-connection-transcript-state conn)
                data)
  ;; Mirror into the raw byte buffer for server-side connections so
  ;; we can verify a client CertificateVerify if mTLS turns out to be
  ;; in play (the cert-requested-p flag flips after some transcript
  ;; updates have already happened, so we always record on the
  ;; server side and just drop the buffer when it isn't needed).
  ;; Handshakes are a few KB; cost is negligible.
  (when (eq (tls12-connection-role conn) :server)
    (let ((buf (or (tls12-connection-transcript-raw-buf conn)
                   (let ((b (make-buffer)))
                     (setf (tls12-connection-transcript-raw-buf conn) b)
                     b))))
      (buf-bytes buf data))))

(defun transcript-snapshot (conn)
  (%hash-snapshot (cipher-suite-info-hash-alg (tls12-connection-info conn))
                  (tls12-connection-transcript-state conn)))

;;; ---------------------------------------------------------------------------
;;; Cipher suite negotiation
;;; ---------------------------------------------------------------------------

(defun negotiate-cipher-suite (client-suites server-suites key-type)
  "Pick the server's first cipher suite that the client offered and that
   matches the server key type."
  (dolist (s server-suites)
    (when (and (member s client-suites)
               (let ((info (cipher-suite-meta s)))
                 (and info
                      (case key-type
                        (:rsa (eq (cipher-suite-info-auth info) :rsa))
                        (:ecdsa-p256 (eq (cipher-suite-info-auth info) :ecdsa))))))
      (return s))))

;;; ---------------------------------------------------------------------------
;;; Alert helper (plaintext alert for pre-encryption failures)
;;; ---------------------------------------------------------------------------

(defun make-plain-alert-record (level description)
  (serialize-tls-record
   (make-tls-record
    :content-type +content-alert+
    :version +tls-1.2+
    :data (let ((a (make-array 2 :element-type '(unsigned-byte 8))))
            (setf (aref a 0) level)
            (setf (aref a 1) description)
            a))))

;;; ---------------------------------------------------------------------------
;;; Server flight 1: ServerHello .. ServerHelloDone
;;; ---------------------------------------------------------------------------

(defun tls12-server-start-handshake (conn client-hello-data)
  "Accept a ClientHello (handshake payload, without the 4-byte header)
   and return the raw TLS record bytes to write: ServerHello,
   Certificate, ServerKeyExchange, ServerHelloDone."
  (let* ((config (tls12-connection-config conn))
         (ch (parse-client-hello client-hello-data)))
    (setf (tls12-connection-use-ems-p conn)
          (parsed-client-hello-extended-master-secret-p ch))
    ;; Verify null compression is offered (we always select null).
    (let ((cms (parsed-client-hello-compression-methods ch)))
      (unless (and cms (find 0 cms))
        (error "Client did not offer null compression")))
    ;; Negotiate cipher suite.
    (let ((suite (negotiate-cipher-suite
                  (parsed-client-hello-cipher-suites ch)
                  (tls12-server-config-cipher-suites config)
                  (tls12-server-config-key-type config))))
      (unless suite (error "No common cipher suite"))
      (setf (tls12-connection-cipher-suite conn) suite)
      (setf (tls12-connection-info conn) (cipher-suite-meta suite)))
    ;; Negotiate ECDHE group.
    (let ((group (select-ecdhe-group
                  (parsed-client-hello-supported-groups ch))))
      (unless group (error "No common ECDHE group"))
      (setf (tls12-connection-ecdhe-group conn) group)
      (multiple-value-bind (sk pk) (ecdhe-generate group)
        (setf (tls12-connection-ecdhe-private conn) sk)
        (setf (tls12-connection-ecdhe-server-public conn) pk)))
    ;; Initialize transcript and fold in the ClientHello.
    (transcript-init conn)
    (let ((ch-msg (make-handshake-message +handshake-client-hello+ client-hello-data)))
      (transcript-update conn ch-msg))
    ;; Save client random.
    (setf (tls12-connection-client-random conn) (parsed-client-hello-random ch))
    (setf (tls12-connection-hostname conn) (parsed-client-hello-hostname ch))
    ;; Negotiate ALPN.
    (let ((client-alpn (parsed-client-hello-alpn-protocols ch))
          (server-alpn (tls12-server-config-alpn-protocols config)))
      (when (and client-alpn server-alpn)
        (setf (tls12-connection-alpn-protocol conn)
              (find-if (lambda (p) (member p client-alpn :test #'string=))
                       server-alpn))))
    ;; Decide whether to issue a session ticket and whether to staple.
    (let* ((want-ticket (and (tls12-server-config-session-ticket-store config)
                             ;; Client offered the session_ticket extension.
                             (parsed-client-hello-session-ticket ch)))
           (staple-bytes (and (parsed-client-hello-status-request-p ch)
                              (tls12-server-config-ocsp-staple config))))
      (setf (tls12-connection-issue-session-ticket-p conn) (and want-ticket t))
      ;; Build server random.
      (let ((server-random (drbg:random-bytes 32)))
        (setf (tls12-connection-server-random conn) server-random)
        ;; ServerHello
        (let* ((sh-payload (build-server-hello-payload
                            server-random
                            (parsed-client-hello-session-id ch)
                            (tls12-connection-cipher-suite conn)
                            :alpn (tls12-connection-alpn-protocol conn)
                            :extended-master-secret-p (tls12-connection-use-ems-p conn)
                            :session-ticket-advertised-p want-ticket
                            :status-request-advertised-p (and staple-bytes t)))
               (sh-msg (make-handshake-message +handshake-server-hello+ sh-payload)))
          (transcript-update conn sh-msg)
          ;; Certificate
          (let ((cert-msg (build-certificate-message
                           (tls12-server-config-certificate-chain config))))
            (transcript-update conn cert-msg)
            ;; CertificateStatus (only when we're actually stapling).
            (let ((cert-status-msg
                    (when staple-bytes
                      (let ((m (build-certificate-status staple-bytes)))
                        (transcript-update conn m)
                        m))))
              ;; ServerKeyExchange
              (let* ((ch-sigs (parsed-client-hello-signature-algorithms ch))
                     (ske-msg (build-server-key-exchange
                               (tls12-connection-client-random conn)
                               (tls12-connection-server-random conn)
                               (tls12-connection-ecdhe-group conn)
                               (tls12-connection-ecdhe-server-public conn)
                               (tls12-server-config-key-type config)
                               (tls12-server-config-private-key config)
                               ch-sigs)))
                (transcript-update conn ske-msg)
                ;; CertificateRequest (mTLS, RFC 5246 §7.4.4): inserted
                ;; between ServerKeyExchange and ServerHelloDone when
                ;; the server config asks for a client cert. Captures
                ;; the flag on the connection so flight 2 expects a
                ;; client Certificate before ClientKeyExchange.
                (let ((cr-msg
                        (when (%verify-peer-p-tls12
                               (tls12-server-config-verify-mode config))
                          (let ((m (build-tls12-certificate-request-message)))
                            (transcript-update conn m)
                            (setf (tls12-connection-cert-requested-p conn) t)
                            m))))
                  ;; ServerHelloDone
                  (let ((shd-msg (build-server-hello-done)))
                    (transcript-update conn shd-msg)
                    (setf (tls12-connection-state conn) :wait-client-key-exchange)
                    (let* ((parts (remove nil (list sh-msg cert-msg cert-status-msg
                                                    ske-msg cr-msg shd-msg)))
                           (hs (apply #'concat-bytes parts))
                           (rec (make-tls-record :content-type +content-handshake+
                                                 :version +tls-1.2+
                                                 :data hs)))
                      (serialize-tls-record rec))))))))))))

;;; ---------------------------------------------------------------------------
;;; Server flight 2: process ClientKeyExchange + CCS + Finished, emit
;;; server CCS + Finished.
;;;
;;; The caller hands us the raw bytes of the client's second flight as
;;; one contiguous record stream. We parse record-by-record: the CKE is
;;; in a handshake record, CCS is its own record (a single byte 0x01),
;;; and the Finished is in the first AEAD-encrypted handshake record.
;;; ---------------------------------------------------------------------------

(defun %read-record (bytes pos)
  (parse-tls-record bytes pos))

(defun %tls12-verify-client-certificate-verify
    (conn payload-bytes transcript-hash)
  "Verify the client's CertificateVerify signature in TLS 1.2.
   PAYLOAD-BYTES is the message body (without the 4-byte handshake
   header). TRANSCRIPT-HASH is the prf-hash digest of every handshake
   message up to but not including this CV.

   The slot stores DER bytes (matches the TLS 1.2 client-side
   convention); we parse the leaf on demand here so the slot stays
   uniform across roles."
  (let ((pos 0))
    (multiple-value-bind (scheme p) (rd-u16 payload-bytes pos "client-cv sig scheme")
      (setf pos p)
      (multiple-value-bind (sig-len p2) (rd-u16 payload-bytes pos "client-cv sig length")
        (multiple-value-bind (sig p3) (rd-bytes payload-bytes p2 sig-len "client-cv signature")
          (declare (ignore p3))
          (let* ((leaf-der (first (tls12-connection-server-peer-certs conn)))
                 (leaf (and leaf-der (x509:parse-x509-certificate leaf-der))))
            (unless leaf
              (error "tls12 server: CertificateVerify with no leaf cert"))
            (%tls12-verify-cv-against-leaf scheme leaf transcript-hash sig)))))))

(defun %tls12-verify-cv-against-leaf (scheme leaf transcript-hash signature)
  "Verify a TLS 1.2 client CertificateVerify signature against the
   peer's leaf cert. Unlike `verify-ske-signature' which expects a
   pre-extracted public-key object, this resolves the cert's public
   key on demand from the leaf x509 struct, using the same dispatch
   the TLS 1.3 client-CV verifier uses."
  (let ((pk-bytes (x509:x509-cert-public-key-bytes leaf)))
    (cond
      ((= scheme +sig-ecdsa-secp256r1-sha256+)
       (let ((point (ec-p256:p256-point-decode pk-bytes)))
         (unless point
           (error "tls12 server CV: invalid P-256 public key"))
         (multiple-value-bind (r s) (decode-ecdsa-der signature)
           (unless (ecdsa:ecdsa-verify point transcript-hash r s)
             (error "tls12 server CV: ECDSA signature invalid")))))
      ((= scheme +sig-rsa-pss-rsae-sha256+)
       (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
         (let ((pub (rsa:make-rsa-public-key n e)))
           (unless (rsa:rsa-pss-verify pub transcript-hash signature :hash :sha256)
             (error "tls12 server CV: RSA-PSS signature invalid")))))
      ((= scheme +sig-rsa-pkcs1-sha256+)
       (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
         (let ((pub (rsa:make-rsa-public-key n e)))
           (unless (rsa:pkcs1-v15-verify pub transcript-hash signature :hash :sha256)
             (error "tls12 server CV: RSA-PKCS1 signature invalid")))))
      (t (error "tls12 server CV: unsupported signature scheme #x~4,'0X" scheme)))
    t))

(defun %tls12-process-client-cert-flight (conn payload pos)
  "Parse the client's Certificate handshake message at PAYLOAD[POS..],
   fold it into the transcript, store the leaf chain (DER bytes,
   matching the existing TLS 1.2 client-side `peer-certs' shape) on
   the connection, and return the next position. Signals an error --
   caller maps to a TLS `certificate_required(116)' alert -- when
   require-cert mode is configured and the client returned empty."
  (multiple-value-bind (mtype mpayload next-pos)
      (parse-handshake-header payload pos)
    (unless (and mtype (= mtype +handshake-certificate+))
      (error "tls12 server: expected client Certificate, got handshake type ~A"
             mtype))
    (let ((message-bytes (subseq payload pos next-pos)))
      (transcript-update conn message-bytes))
    (let ((cert-data-list (parse-certificate-message mpayload))
          (config (tls12-connection-config conn)))
      (setf (tls12-connection-server-peer-certs conn) cert-data-list)
      (when (and (null cert-data-list)
                 config
                 (%verify-fail-if-missing-p-tls12
                  (tls12-server-config-verify-mode config)))
        (error "tls12 server: client certificate required but Certificate was empty"))
      next-pos)))

(defun %tls12-collect-client-flight2-records (bytes)
  "Walk the wire bytes of the client's flight 2 and split into
     (values plaintext-handshake-payload ccs-seen-p finished-record)
   where plaintext-handshake-payload is the concatenation of every
   Handshake-content-type record before ChangeCipherSpec (clients are
   free to fragment Certificate / CKE / CV across records or to batch
   them; we accept either). FINISHED-RECORD is the first handshake
   record after CCS (the encrypted Finished)."
  (let ((pos 0)
        (hs-buf (make-buffer))
        ccs-seen finished-record)
    (loop
      (when (>= pos (length bytes))
        (return))
      (multiple-value-bind (rec next) (%read-record bytes pos)
        (unless rec (error "Truncated client flight 2 record"))
        (cond
          ((and (not ccs-seen)
                (= (tls-record-content-type rec) +content-handshake+))
           (buf-bytes hs-buf (tls-record-data rec)))
          ((= (tls-record-content-type rec) +content-change-cipher-spec+)
           (unless (and (= (length (tls-record-data rec)) 1)
                        (= (aref (tls-record-data rec) 0) 1))
             (error "Malformed ChangeCipherSpec body"))
           (setf ccs-seen t))
          ((and ccs-seen
                (= (tls-record-content-type rec) +content-handshake+))
           (setf finished-record rec)
           (setf pos next)
           (return))
          (t
           (error "Unexpected record content-type ~D in client flight 2"
                  (tls-record-content-type rec))))
        (setf pos next)))
    (values (buf-freeze hs-buf) ccs-seen finished-record)))

(defun tls12-server-process-client-flight2 (conn bytes)
  "Consume the client's second handshake flight bytes and return the
   server's CCS + Finished bytes to write back.

   On a non-mTLS handshake the flight is ClientKeyExchange + CCS +
   encrypted Finished (one record each). With mTLS the plaintext
   handshake portion carries Certificate + ClientKeyExchange +
   CertificateVerify in any combination of records. We collect the
   plaintext handshake bytes across all pre-CCS records, then walk
   the concatenated payload message-by-message."
  (multiple-value-bind (hs-payload ccs-seen finished-record)
      (%tls12-collect-client-flight2-records bytes)
    (unless ccs-seen (error "Missing ChangeCipherSpec"))
    (unless finished-record (error "Missing client Finished record"))
    (let ((rec-pos 0)
          (cke-payload nil))
      ;; Optional Certificate (mTLS path).
      (when (tls12-connection-cert-requested-p conn)
        (setf rec-pos (%tls12-process-client-cert-flight conn hs-payload rec-pos)))
      ;; ClientKeyExchange (always present).
      (multiple-value-bind (type payload next-pos)
          (parse-handshake-header hs-payload rec-pos)
        (unless type
          (error "Truncated handshake message in client flight 2"))
        (unless (= type +handshake-client-key-exchange+)
          (error "Expected ClientKeyExchange, got handshake type ~D" type))
        (transcript-update conn (subseq hs-payload rec-pos next-pos))
        (setf cke-payload payload)
        (setf rec-pos next-pos))
      ;; Compute master secret + derive key material from the CKE.
      ;; The EMS session_hash (RFC 7627 §4) is the digest over every
      ;; handshake message UP TO AND INCLUDING ClientKeyExchange --
      ;; CertificateVerify hasn't been folded in yet, and won't be
      ;; until after the master secret is derived. (The client
      ;; computes its own session_hash at the same point in its own
      ;; flight; the two must agree.)
      (let* ((peer-pk (parse-client-key-exchange cke-payload))
             (pms (ecdhe-derive (tls12-connection-ecdhe-group conn)
                                (tls12-connection-ecdhe-private conn)
                                peer-pk))
             (info (tls12-connection-info conn))
             (master (if (tls12-connection-use-ems-p conn)
                         (let ((session-hash (transcript-snapshot conn)))
                           (setf (tls12-connection-ems-session-hash conn) session-hash)
                           (derive-master-secret-ems info pms session-hash))
                         (derive-master-secret-standard
                          info pms
                          (tls12-connection-client-random conn)
                          (tls12-connection-server-random conn)))))
        (setf (tls12-connection-master-secret conn) master)
        (setf (tls12-connection-key-material conn)
              (derive-key-block info master
                                (tls12-connection-client-random conn)
                                (tls12-connection-server-random conn)))
        ;; Reset the record-layer sequence numbers for the new cipher state.
        (setf (tls12-connection-read-seq conn) 0)
        (setf (tls12-connection-write-seq conn) 0))
      ;; Optional CertificateVerify (RFC 5246 §7.4.8). Comes AFTER
      ;; ClientKeyExchange on the wire and AFTER master-secret
      ;; derivation in our processing -- the EMS session_hash and the
      ;; CV transcript snapshot are taken at different points (post-
      ;; CKE for EMS, post-CKE-pre-CV for CV). Signed content is the
      ;; raw bytes of every prior handshake message; the digital-
      ;; signature primitive hashes internally per the chosen scheme.
      (when (and (tls12-connection-cert-requested-p conn)
                 (tls12-connection-server-peer-certs conn))
        (let ((raw-pre-cv (and (tls12-connection-transcript-raw-buf conn)
                               (buf-freeze
                                (tls12-connection-transcript-raw-buf conn)))))
          (multiple-value-bind (type payload next-pos)
              (parse-handshake-header hs-payload rec-pos)
            (unless type
              (error "Truncated handshake message at expected CertificateVerify"))
            (unless (= type +handshake-certificate-verify+)
              (error "Expected CertificateVerify, got handshake type ~D" type))
            (%tls12-verify-client-certificate-verify conn payload raw-pre-cv)
            (transcript-update conn (subseq hs-payload rec-pos next-pos))
            (setf rec-pos next-pos)))))
    ;; Decrypt the client Finished.
    (let* ((info (tls12-connection-info conn))
           (km (tls12-connection-key-material conn))
           (pt (decrypt-record (tls-record-data finished-record)
                               +content-handshake+
                               (key-material-client-write-key km)
                               (key-material-client-write-iv km)
                               (tls12-connection-read-seq conn)
                               info)))
      (incf (tls12-connection-read-seq conn))
      (multiple-value-bind (type payload) (parse-handshake-header pt)
        (unless (= type +handshake-finished+)
          (error "Expected Finished inside encrypted handshake, got ~D" type))
        ;; verify_data is over Hash(handshake_messages) UP TO BUT NOT
        ;; INCLUDING this client Finished.
        (let ((transcript-hash (transcript-snapshot conn)))
          (verify-client-finished info
                                  (tls12-connection-master-secret conn)
                                  transcript-hash payload))
        ;; Then fold the full client Finished into the transcript so the
        ;; server Finished hashes the complete history.
        (transcript-update conn pt)))
    ;; Build optional NewSessionTicket (plaintext), CCS, and encrypted Finished.
    ;; RFC 5077 Section 3.3: the server sends NewSessionTicket as a
    ;; plaintext handshake message BEFORE ChangeCipherSpec, then the
    ;; encrypted Finished. The NST is part of the Finished transcript.
    (let* ((info (tls12-connection-info conn))
           (km (tls12-connection-key-material conn))
           (config (tls12-connection-config conn))
           (nst-record
             (when (tls12-connection-issue-session-ticket-p conn)
               (let* ((store (tls12-server-config-session-ticket-store config))
                      (lifetime (tls12-server-config-session-ticket-lifetime config))
                      (cs (tls12-connection-cipher-suite conn))
                      (master (tls12-connection-master-secret conn))
                      (plain (let ((b (make-buffer)))
                               (buf-u16 b cs)
                               (buf-bytes b (u64-be->bytes (get-universal-time)))
                               (buf-u8 b (length master))
                               (buf-bytes b master)
                               (buf-freeze b)))
                      (sealed (stek:stek-seal store plain))
                      (msg (build-new-session-ticket-tls12 lifetime sealed)))
                 (transcript-update conn msg)
                 ;; Wrap in a plaintext handshake record (sent before CCS)
                 (serialize-tls-record
                  (make-tls-record
                   :content-type +content-handshake+
                   :version +tls-1.2+
                   :data msg)))))
           (server-finished (build-finished-message
                             info
                             (tls12-connection-master-secret conn)
                             (transcript-snapshot conn))))
      (transcript-update conn server-finished)
      (let* ((ccs-record
               (serialize-tls-record
                (make-tls-record
                 :content-type +content-change-cipher-spec+
                 :version +tls-1.2+
                 :data (let ((a (make-array 1 :element-type '(unsigned-byte 8))))
                         (setf (aref a 0) 1) a))))
             (enc (encrypt-record server-finished +content-handshake+
                                  (key-material-server-write-key km)
                                  (key-material-server-write-iv km)
                                  (tls12-connection-write-seq conn)
                                  info))
             (fin-record
               (serialize-tls-record
                (make-tls-record
                 :content-type +content-handshake+
                 :version +tls-1.2+
                 :data enc))))
        (incf (tls12-connection-write-seq conn))
        (setf (tls12-connection-state conn) :connected)
        ;; Wire order: [NST] + CCS + encrypted Finished
        (if nst-record
            (concat-bytes nst-record ccs-record fin-record)
            (concat-bytes ccs-record fin-record))))))

;;; ---------------------------------------------------------------------------
;;; Application data
;;; ---------------------------------------------------------------------------

;;; Role-aware key/IV selectors. RFC 5246 labels the two halves of the
;;; key block as client_write_* and server_write_*: the client writes
;;; records under client_write_*, the server writes under
;;; server_write_*. So "my write" material depends on which role I am.

(defun %own-write-key (conn)
  (let ((km (tls12-connection-key-material conn)))
    (ecase (tls12-connection-role conn)
      (:server (key-material-server-write-key km))
      (:client (key-material-client-write-key km)))))

(defun %own-write-iv (conn)
  (let ((km (tls12-connection-key-material conn)))
    (ecase (tls12-connection-role conn)
      (:server (key-material-server-write-iv km))
      (:client (key-material-client-write-iv km)))))

(defun %peer-write-key (conn)
  (let ((km (tls12-connection-key-material conn)))
    (ecase (tls12-connection-role conn)
      (:server (key-material-client-write-key km))
      (:client (key-material-server-write-key km)))))

(defun %peer-write-iv (conn)
  (let ((km (tls12-connection-key-material conn)))
    (ecase (tls12-connection-role conn)
      (:server (key-material-client-write-iv km))
      (:client (key-material-server-write-iv km)))))

(defun tls12-send-application-data (conn data)
  "Encrypt DATA as a single application_data record. Returns wire bytes."
  (let* ((info (tls12-connection-info conn))
         (enc (encrypt-record data +content-application-data+
                              (%own-write-key conn)
                              (%own-write-iv conn)
                              (tls12-connection-write-seq conn)
                              info)))
    (incf (tls12-connection-write-seq conn))
    (serialize-tls-record
     (make-tls-record :content-type +content-application-data+
                      :version +tls-1.2+
                      :data enc))))

(defun tls12-receive-application-data (conn record-body)
  "Decrypt RECORD-BODY as an application_data record. Returns plaintext."
  (let* ((info (tls12-connection-info conn))
         (pt (decrypt-record record-body +content-application-data+
                             (%peer-write-key conn)
                             (%peer-write-iv conn)
                             (tls12-connection-read-seq conn)
                             info)))
    (incf (tls12-connection-read-seq conn))
    pt))

;;; ===========================================================================
;;; Transport-driven accept and stream adapter
;;; ===========================================================================
;;;
;;; Mirrors epsilon.crypto.tls13 tls-accept / tls-read / tls-write. The
;;; stream wraps an underlying transport (any object implementing the
;;; tls13:tls-transport-read/write/close generics, e.g. fd-transport)
;;; and buffers decrypted application data between record reads.
;;;
;;; The crypto bridge and proxy edge hand these stream objects back to
;;; callers via the version-agnostic dispatch in epsilon.crypto.tls-accept.

(defun %transport-read-exact (transport n)
  "Read exactly N bytes from a transport, or return NIL on EOF."
  (let ((buf (make-array n :element-type '(unsigned-byte 8)))
        (pos 0))
    (loop while (< pos n) do
      (let ((got (tls13:tls-transport-read transport buf pos n)))
        (when (or (null got) (zerop got))
          (return-from %transport-read-exact nil))
        (incf pos got)))
    buf))

(defun %read-next-record (transport)
  "Read a single TLS record from TRANSPORT and return its raw wire bytes
   (header + body) or NIL on EOF."
  (let ((header (%transport-read-exact transport 5)))
    (unless header (return-from %read-next-record nil))
    (let* ((len (logior (ash (aref header 3) 8) (aref header 4)))
           (body (%transport-read-exact transport len)))
      (unless body (return-from %read-next-record nil))
      (concat-bytes header body))))

(defun %write-all (transport bytes)
  (tls13:tls-transport-write transport bytes 0 (length bytes)))

(defun %send-alert-and-error (transport alert-description format-string &rest format-args)
  "Send a fatal TLS alert to TRANSPORT (unencrypted), then signal an error.
   Used during the pre-encryption handshake phase to ensure peers receive a
   proper rejection instead of a silent connection drop."
  (ignore-errors
    (%write-all transport
      (tls13:make-alert-record tls13:+alert-level-fatal+ alert-description)))
  (apply #'error format-string format-args))

(defstruct tls12-stream
  (connection nil)
  (transport nil)
  ;; Decrypted application-data bytes not yet consumed by tls12-read.
  (read-buffer (make-array 0 :element-type '(unsigned-byte 8)))
  (read-pos 0 :type fixnum)
  (closed-p nil))

(defun tls12-accept (transport config &key client-hello-bytes)
  "Accept a TLS 1.2 connection on TRANSPORT using CONFIG.

   CLIENT-HELLO-BYTES, if provided, is the raw wire bytes of the
   initial ClientHello record (5-byte header + body) that a dispatch
   wrapper already peeked off the transport to decide the version.
   When NIL we read the first record from the transport ourselves.

   Returns a TLS12-STREAM ready for application-data IO."
  (let* ((conn (make-tls12-connection :role :server :config config))
         (first-record-bytes (or client-hello-bytes
                                 (%read-next-record transport))))
    (unless first-record-bytes
      (error "tls12-accept: client closed before ClientHello"))
    (multiple-value-bind (record next) (parse-tls-record first-record-bytes)
      (declare (ignore next))
      (unless (and record (= (tls-record-content-type record) +content-handshake+))
        (error "tls12-accept: first record is not a handshake"))
      ;; Parse the handshake header and extract the ClientHello payload.
      (let* ((hs (tls-record-data record)))
        (multiple-value-bind (type payload end) (parse-handshake-header hs)
          (declare (ignore end))
          (unless (= type +handshake-client-hello+)
            (error "tls12-accept: expected ClientHello, got handshake type ~D"
                   type))
          ;; Flight 1: server -> client. Wrap so negotiation failures
          ;; (no common suite, missing EMS, etc.) send an alert first.
          (let ((flight1 (handler-case (tls12-server-start-handshake conn payload)
                           (tls13:tls-decode-error (e)
                             (%send-alert-and-error transport tls13:+alert-decode-error+
                               "~A" e))
                           (error (e)
                             (%send-alert-and-error transport tls13:+alert-handshake-failure+
                               "~A" e)))))
            (%write-all transport flight1)))))
    ;; Flight 2: client -> server. Without mTLS the client sends three
    ;; records (ClientKeyExchange + ChangeCipherSpec + encrypted
    ;; Finished). With mTLS it can send up to five (Certificate,
    ;; ClientKeyExchange, CertificateVerify, ChangeCipherSpec, encrypted
    ;; Finished); curl sends each handshake message as its own record.
    ;; Read records until we've seen ChangeCipherSpec followed by one
    ;; more (the encrypted Finished), accumulating an alert into a
    ;; useful error message if one arrives instead.
    (let ((records nil)
          (saw-ccs nil))
      (loop
        (let ((rb (%read-next-record transport)))
          (unless rb (error "tls12-accept: client closed during flight 2"))
          (let ((ct (aref rb 0)))
            (cond
              ((= ct +content-alert+)
               (let ((alert-body (subseq rb 5 (min (length rb) 7))))
                 (error "tls12-accept: client sent alert ~A~A during flight 2"
                        (if (and (>= (length alert-body) 1)
                                 (= (aref alert-body 0) +alert-level-fatal+))
                            "fatal " "")
                        (if (>= (length alert-body) 2) (aref alert-body 1) "?"))))
              ((= ct +content-change-cipher-spec+)
               (push rb records)
               (setf saw-ccs t))
              (saw-ccs
               ;; First record after CCS is the encrypted Finished.
               (push rb records)
               (return))
              (t
               ;; Pre-CCS handshake record (Certificate / CKE / CV / etc).
               (push rb records))))))
      (let* ((flight2-bytes (apply #'concat-bytes (nreverse records)))
             (server-flight (handler-case
                              (tls12-server-process-client-flight2
                               conn flight2-bytes)
                            (error (e)
                              (let ((msg (princ-to-string e)))
                                (%send-alert-and-error transport
                                  (cond ((search "Finished verification" msg)
                                         tls13:+alert-decrypt-error+)
                                        ((search "decryption" msg)
                                         tls13:+alert-bad-record-mac+)
                                        ((search "client certificate required" msg)
                                         tls13:+alert-certificate-required+)
                                        (t tls13:+alert-handshake-failure+))
                                  "~A" e))))))
        (%write-all transport server-flight)))
    (make-tls12-stream :connection conn :transport transport)))

(defun tls12-read (stream buffer &key (start 0) (end (length buffer)))
  "Read decrypted data from STREAM into BUFFER[START..END). Returns the
   number of bytes read, or 0 at EOF."
  (when (tls12-stream-closed-p stream)
    (return-from tls12-read 0))
  ;; Serve from buffered plaintext first.
  (let* ((rb (tls12-stream-read-buffer stream))
         (rp (tls12-stream-read-pos stream))
         (avail (- (length rb) rp))
         (wanted (- end start)))
    (when (plusp avail)
      (let ((n (min avail wanted)))
        (replace buffer rb :start1 start :end1 (+ start n) :start2 rp)
        (incf (tls12-stream-read-pos stream) n)
        (return-from tls12-read n))))
  ;; Otherwise pull the next record from the transport.
  (let* ((conn (tls12-stream-connection stream))
         (transport (tls12-stream-transport stream))
         (record-bytes (%read-next-record transport)))
    (unless record-bytes
      (setf (tls12-stream-closed-p stream) t)
      (return-from tls12-read 0))
    (multiple-value-bind (record next) (parse-tls-record record-bytes)
      (declare (ignore next))
      (let ((ct (tls-record-content-type record)))
        (cond
          ((= ct +content-application-data+)
           (let ((pt (tls12-receive-application-data conn (tls-record-data record))))
             (let* ((n (min (- end start) (length pt))))
               (replace buffer pt :start1 start :end1 (+ start n))
               (when (> (length pt) n)
                 (setf (tls12-stream-read-buffer stream) (subseq pt n))
                 (setf (tls12-stream-read-pos stream) 0))
               n)))
          ((= ct +content-alert+)
           ;; Encrypted alert: decrypt and inspect. At minimum
           ;; treat close_notify and any fatal alert as EOF.
           (let* ((info (tls12-connection-info conn))
                  (pt (decrypt-record (tls-record-data record) +content-alert+
                                      (%peer-write-key conn)
                                      (%peer-write-iv conn)
                                      (tls12-connection-read-seq conn)
                                      info)))
             (incf (tls12-connection-read-seq conn))
             (when (and (>= (length pt) 2)
                        (or (= (aref pt 0) +alert-level-fatal+)
                            (= (aref pt 1) +alert-close-notify+)))
               (setf (tls12-stream-closed-p stream) t))
             0))
          ((= ct +content-handshake+)
           ;; Renegotiation attempt (encrypted ClientHello after CCS).
           ;; We don't support renegotiation; skip the record and read
           ;; the next one. The peer will time out or close.
           (tls12-read stream buffer :start start :end end))
          (t
           (error "tls12-read: unexpected content type ~D" ct)))))))

(defun tls12-write (stream buffer &key (start 0) (end (length buffer)))
  "Encrypt BUFFER[START..END) and write it as a single record. Returns
   the number of bytes written."
  (when (tls12-stream-closed-p stream)
    (error "tls12-write: stream is closed"))
  (let* ((n (- end start))
         (slice (if (and (zerop start) (= end (length buffer)))
                    buffer
                    (subseq buffer start end)))
         (conn (tls12-stream-connection stream))
         (record-bytes (tls12-send-application-data conn slice)))
    (%write-all (tls12-stream-transport stream) record-bytes)
    n))

(defun tls12-shutdown (stream)
  "Send a close_notify alert and mark the stream closed."
  (unless (tls12-stream-closed-p stream)
    (let* ((conn (tls12-stream-connection stream))
           (info (tls12-connection-info conn))
           (body (let ((a (make-array 2 :element-type '(unsigned-byte 8))))
                   (setf (aref a 0) +alert-level-warning+)
                   (setf (aref a 1) +alert-close-notify+)
                   a))
           (enc (encrypt-record body +content-alert+
                                (%own-write-key conn)
                                (%own-write-iv conn)
                                (tls12-connection-write-seq conn)
                                info)))
      (incf (tls12-connection-write-seq conn))
      (%write-all (tls12-stream-transport stream)
                  (serialize-tls-record
                   (make-tls-record :content-type +content-alert+
                                    :version +tls-1.2+
                                    :data enc)))
      (setf (tls12-stream-closed-p stream) t))))

(defun tls12-close (stream)
  "Shut down and close the underlying transport."
  (handler-case (tls12-shutdown stream)
    (error () nil))
  (tls13:tls-transport-close (tls12-stream-transport stream)))

(defun tls12-stream-alpn-protocol (stream)
  (tls12-connection-alpn-protocol (tls12-stream-connection stream)))

(defun tls12-stream-cipher-suite (stream)
  (tls12-connection-cipher-suite (tls12-stream-connection stream)))

(defun tls12-stream-peer-certificates (stream)
  "Return the peer's certificate chain as a list of DER byte vectors.
   On a client stream this is the server's chain (the only direction
   that ran before mTLS support landed); on a server stream this is
   the client cert chain captured during the mTLS flight, or NIL when
   the server never requested a client cert / the client returned
   empty."
  (let ((conn (tls12-stream-connection stream)))
    (case (tls12-connection-role conn)
      (:client (tls12-connection-peer-certs conn))
      (:server (tls12-connection-server-peer-certs conn)))))

;;; ===========================================================================
;;; TLS 1.2 Client
;;; ===========================================================================
;;;
;;; Mirrors the server-side state machine but inverted: we send a
;;; ClientHello, parse the server's flight (ServerHello, Certificate,
;;; optional CertificateStatus, ServerKeyExchange, ServerHelloDone),
;;; then send our ClientKeyExchange + CCS + Finished and finally read
;;; the server's CCS + optional NewSessionTicket + Finished.
;;;
;;; Certificate chain verification is delegated to an optional
;;; trust-store callback supplied via tls12-client-config. When the
;;; trust store is NIL the client accepts any server and SKIPS
;;; signature verification -- this is only acceptable for in-process
;;; tests.
;;;
;;; TODO(IMPL-333 phase 4 followup): add real server certificate
;;; verification. The trust-store callback is a stopgap. A production
;;; client must: (1) parse the server's leaf cert with
;;; epsilon.crypto.x509:parse-x509-certificate; (2) extract the public
;;; key via a shared helper that understands ECDSA P-256 and RSA keys
;;; and returns the right shape for ecdsa-verify / rsa-pss-verify /
;;; pkcs1-v15-verify; (3) verify the ServerKeyExchange signature
;;; over (client_random || server_random || ECParameters || ECPoint)
;;; with the chosen sig_and_hash_algorithm; (4) walk the chain and
;;; validate each issuer signature against the trust store (same
;;; logic used by the TLS 1.3 client path in tls13.lisp); (5) check
;;; hostname against the leaf's SAN / CN; (6) check notBefore /
;;; notAfter; (7) when the server sent a CertificateStatus, verify
;;; the OCSP response signature and match it to the leaf. Until
;;; those land, callers MUST pass a trust-store callback explicitly
;;; and understand they are the ones enforcing trust.

(defstruct tls12-client-config
  (hostname nil)
  (alpn-protocols nil :type list)
  (cipher-suites
   (list +tls-ecdhe-ecdsa-aes-128-gcm-sha256+
         +tls-ecdhe-ecdsa-chacha20-poly1305-sha256+
         +tls-ecdhe-ecdsa-aes-256-gcm-sha384+
         +tls-ecdhe-rsa-aes-128-gcm-sha256+
         +tls-ecdhe-rsa-chacha20-poly1305-sha256+
         +tls-ecdhe-rsa-aes-256-gcm-sha384+)
   :type list)
  (supported-groups (list +group-x25519+ +group-secp256r1+) :type list)
  (signature-algorithms
   (list +sig-ecdsa-secp256r1-sha256+
         +sig-rsa-pss-rsae-sha256+
         +sig-rsa-pkcs1-sha256+)
   :type list)
  ;; A function of (leaf-x509 chain hostname) -> T/NIL or NIL to skip.
  (trust-store nil)
  ;; Client offers status_request when T.
  (request-ocsp-p nil)
  ;; Client offers an empty session_ticket extension when T.
  (request-session-ticket-p nil))

;;; Extend tls12-connection with slots the client path needs. These
;;; are added on the same struct to avoid duplicating the record-layer
;;; machinery; they are all NIL on server connections.
;;;
;;; (We edit the defstruct by redefining the slots, but CL does not
;;; support "adding slots to an existing struct" -- so instead of
;;; redefining, we piggy-back on a second struct reachable from the
;;; connection. That keeps the server defstruct binary compatible.)

(defstruct tls12-client-extras
  (config nil)
  ;; Server material from ServerHello..ServerHelloDone
  (server-ecdhe-group 0 :type fixnum)
  (server-ecdhe-public nil)
  (server-certs nil :type list)      ; list of DER byte vectors
  (chosen-cipher-suite 0 :type fixnum)
  ;; Client ephemeral keypair (set at ClientKeyExchange build time)
  (ephemeral-private nil)
  (ephemeral-public nil)
  (pre-master-secret nil))

;; Reuse the tls12-connection struct by stashing client extras in a
;; per-connection slot indirectly. We add one slot via a parallel
;; property hash table keyed on the connection object.

(defparameter *client-extras-table* (make-hash-table :test 'eq :weakness :key)
  "Weak map from tls12-connection -> tls12-client-extras, so client-
   side state can coexist with the server defstruct shape without
   breaking existing serialized fasls.")

(defun %client-extras (conn &key create-p)
  (or (gethash conn *client-extras-table*)
      (and create-p
           (setf (gethash conn *client-extras-table*)
                 (make-tls12-client-extras)))))

;; Store the peer cert chain where both the server-side (NIL) and
;; client-side tls12-stream-peer-certificates can find it. We use a
;; dynamic closure over the connection: the accessor below checks the
;; client extras first.

(defun tls12-connection-peer-certs (conn)
  (let ((extras (%client-extras conn)))
    (and extras (tls12-client-extras-server-certs extras))))

;; Note: tls12-connection-role is the defstruct accessor for the role
;; slot; no separate definition needed here.

;;; ---------------------------------------------------------------------------
;;; Client extension builders
;;; ---------------------------------------------------------------------------

(defun %build-ext (type body-bytes)
  (let ((buf (make-buffer)))
    (buf-u16 buf type)
    (buf-u16-prefixed buf body-bytes)
    (buf-freeze buf)))

(defun %build-sni-ext (hostname)
  (let ((name-bytes (map '(vector (unsigned-byte 8)) #'char-code hostname))
        (inner (make-buffer)))
    (buf-u16 inner (+ 3 (length name-bytes)))   ; list length
    (buf-u8 inner 0)                             ; host_name
    (buf-u16-prefixed inner name-bytes)
    (%build-ext +ext-server-name+ (buf-freeze inner))))

(defun %build-supported-groups-ext (groups)
  (let ((inner (make-buffer)))
    (buf-u16 inner (* 2 (length groups)))
    (dolist (g groups) (buf-u16 inner g))
    (%build-ext +ext-supported-groups+ (buf-freeze inner))))

(defun %build-ec-point-formats-ext ()
  (let ((inner (make-buffer)))
    (buf-u8 inner 1)
    (buf-u8 inner +ec-point-format-uncompressed+)
    (%build-ext +ext-ec-point-formats+ (buf-freeze inner))))

(defun %build-sigalgs-ext (sigalgs)
  (let ((inner (make-buffer)))
    (buf-u16 inner (* 2 (length sigalgs)))
    (dolist (s sigalgs) (buf-u16 inner s))
    (%build-ext +ext-signature-algorithms+ (buf-freeze inner))))

(defun %build-alpn-ext (protocols)
  (let ((list-buf (make-buffer)))
    (dolist (p protocols)
      (let ((pb (map '(vector (unsigned-byte 8)) #'char-code p)))
        (buf-u8-prefixed list-buf pb)))
    (let ((inner (make-buffer)))
      (buf-u16-prefixed inner (buf-freeze list-buf))
      (%build-ext +ext-alpn+ (buf-freeze inner)))))

(defun %build-ems-ext ()
  (%build-ext +ext-extended-master-secret+
              (make-array 0 :element-type '(unsigned-byte 8))))

(defun %build-renegotiation-info-ext ()
  "Empty renegotiation_info: signals the client has never renegotiated."
  (let ((inner (make-buffer)))
    (buf-u8 inner 0)
    (%build-ext +ext-renegotiation-info+ (buf-freeze inner))))

(defun %build-status-request-ext ()
  "Empty OCSP status_request: status_type=1, responder_id_list empty,
   request_extensions empty."
  (let ((inner (make-buffer)))
    (buf-u8 inner 1)            ; status_type = ocsp
    (buf-u16 inner 0)           ; responder_id_list<0..2^16-1>
    (buf-u16 inner 0)           ; request_extensions<0..2^16-1>
    (%build-ext +ext-status-request+ (buf-freeze inner))))

(defun %build-empty-session-ticket-ext ()
  (%build-ext +ext-session-ticket+ (make-array 0 :element-type '(unsigned-byte 8))))

(defun build-client-hello (config client-random &key key-share-group key-share-public)
  "Build a ClientHello handshake message (with the 4-byte handshake
   header) offering the cipher suites / groups / extensions in CONFIG.
   CLIENT-RANDOM must be 32 bytes. Unlike TLS 1.3, TLS 1.2
   ClientHello does not carry a key_share -- the arguments are
   accepted for symmetry and ignored here."
  (declare (ignore key-share-group key-share-public))
  (let ((buf (make-buffer)))
    ;; legacy_version
    (buf-u16 buf +tls-1.2+)
    ;; random
    (buf-bytes buf client-random)
    ;; session_id -- empty (no resumption yet)
    (buf-u8 buf 0)
    ;; cipher_suites
    (let ((suites (tls12-client-config-cipher-suites config)))
      (buf-u16 buf (* 2 (length suites)))
      (dolist (s suites) (buf-u16 buf s)))
    ;; compression_methods -- null only
    (buf-u8 buf 1) (buf-u8 buf 0)
    ;; extensions
    (let ((ext-buf (make-buffer)))
      (when (tls12-client-config-hostname config)
        (buf-bytes ext-buf (%build-sni-ext (tls12-client-config-hostname config))))
      (buf-bytes ext-buf (%build-supported-groups-ext
                          (tls12-client-config-supported-groups config)))
      (buf-bytes ext-buf (%build-ec-point-formats-ext))
      (buf-bytes ext-buf (%build-sigalgs-ext
                          (tls12-client-config-signature-algorithms config)))
      (when (tls12-client-config-alpn-protocols config)
        (buf-bytes ext-buf (%build-alpn-ext
                            (tls12-client-config-alpn-protocols config))))
      (buf-bytes ext-buf (%build-ems-ext))
      (buf-bytes ext-buf (%build-renegotiation-info-ext))
      (when (tls12-client-config-request-ocsp-p config)
        (buf-bytes ext-buf (%build-status-request-ext)))
      (when (tls12-client-config-request-session-ticket-p config)
        (buf-bytes ext-buf (%build-empty-session-ticket-ext)))
      (buf-u16-prefixed buf (buf-freeze ext-buf)))
    (make-handshake-message +handshake-client-hello+ (buf-freeze buf))))

;;; ---------------------------------------------------------------------------
;;; Client-side parsers for the server's flight 1
;;; ---------------------------------------------------------------------------

(defstruct parsed-server-hello
  (legacy-version 0 :type fixnum)
  (random nil)
  (session-id nil)
  (cipher-suite 0 :type fixnum)
  (alpn-protocol nil)
  (extended-master-secret-p nil)
  (session-ticket-advertised-p nil)
  (status-request-advertised-p nil))

(defun parse-server-hello (payload)
  "Parse a ServerHello handshake payload."
  (let ((pos 0) (sh (make-parsed-server-hello)))
    (multiple-value-bind (v p) (rd-u16 payload pos "sh version")
      (setf (parsed-server-hello-legacy-version sh) v pos p))
    (multiple-value-bind (r p) (rd-bytes payload pos 32 "sh random")
      (setf (parsed-server-hello-random sh) r pos p))
    (multiple-value-bind (sid-len p) (rd-u8 payload pos "sh session_id length")
      (setf pos p)
      (multiple-value-bind (sid p2) (rd-bytes payload pos sid-len "sh session_id")
        (setf (parsed-server-hello-session-id sh) sid pos p2)))
    (multiple-value-bind (cs p) (rd-u16 payload pos "sh cipher_suite")
      (setf (parsed-server-hello-cipher-suite sh) cs pos p))
    ;; skip compression method
    (rd-need payload pos 1 "sh compression_method")
    (incf pos 1)
    ;; Extensions (optional)
    (when (< pos (length payload))
      (multiple-value-bind (ext-len p) (rd-u16 payload pos "sh extensions length")
        (setf pos p)
        (rd-need payload pos ext-len "sh extensions")
        (let ((end (+ pos ext-len)))
          (loop while (< pos end)
                do (multiple-value-bind (etype p2) (rd-u16 payload pos "sh ext type")
                     (setf pos p2)
                     (multiple-value-bind (elen p3) (rd-u16 payload pos "sh ext length")
                       (setf pos p3)
                       (multiple-value-bind (edata p4)
                           (rd-bytes payload pos elen "sh ext data")
                         (let ((edlen (length edata)))
                           (cond
                             ((= etype +ext-extended-master-secret+)
                              (setf (parsed-server-hello-extended-master-secret-p sh) t))
                             ((= etype +ext-alpn+)
                              ;; list-len(2) plen(1) proto-bytes
                              (when (>= edlen 3)
                                (let ((plen (aref edata 2)))
                                  (when (<= (+ 3 plen) edlen)
                                    (setf (parsed-server-hello-alpn-protocol sh)
                                          (map 'string #'code-char
                                               (subseq edata 3 (+ 3 plen))))))))
                             ((= etype +ext-session-ticket+)
                              (setf (parsed-server-hello-session-ticket-advertised-p sh) t))
                             ((= etype +ext-status-request+)
                              (setf (parsed-server-hello-status-request-advertised-p sh) t))))
                         (setf pos p4))))))))
    sh))

(defun parse-certificate-message (payload)
  "Return a list of DER-encoded certificates (leaf first)."
  (let ((pos 0) (certs nil))
    (multiple-value-bind (total p) (rd-u24 payload pos "cert total length")
      (setf pos p)
      (rd-need payload pos total "certificates")
      (let ((end (+ pos total)))
        (loop while (< pos end)
              do (multiple-value-bind (clen p2) (rd-u24 payload pos "cert length")
                   (setf pos p2)
                   (multiple-value-bind (cert p3)
                       (rd-bytes payload pos clen "cert data")
                     (push cert certs)
                     (setf pos p3))))))
    (nreverse certs)))

(defstruct parsed-ske
  (curve-type 0)
  (named-curve 0)
  (server-public nil)
  (sig-scheme 0)
  (signature nil)
  (signed-params nil))   ; bytes covered by the signature (ECParameters || ECPoint)

(defun parse-server-key-exchange (payload)
  "Parse an ECDHE ServerKeyExchange handshake payload."
  (let ((sk (make-parsed-ske)) (pos 0))
    (multiple-value-bind (ct p) (rd-u8 payload pos "ske curve_type")
      (setf (parsed-ske-curve-type sk) ct pos p))
    (unless (= (parsed-ske-curve-type sk) +ec-curve-type-named-curve+)
      (error "tls12 client: unsupported curve_type ~D" (parsed-ske-curve-type sk)))
    (multiple-value-bind (nc p) (rd-u16 payload pos "ske named_curve")
      (setf (parsed-ske-named-curve sk) nc pos p))
    (multiple-value-bind (plen p) (rd-u8 payload pos "ske point length")
      (setf pos p)
      (multiple-value-bind (point p2) (rd-bytes payload pos plen "ske ec_point")
        (setf (parsed-ske-server-public sk) point)
        ;; Signed params = curve_type(1) + namedcurve(2) + u8_len + point
        (setf (parsed-ske-signed-params sk) (subseq payload 0 p2))
        (setf pos p2)))
    (multiple-value-bind (sch p) (rd-u16 payload pos "ske sig_scheme")
      (setf (parsed-ske-sig-scheme sk) sch pos p))
    (multiple-value-bind (slen p) (rd-u16 payload pos "ske sig length")
      (setf pos p)
      (multiple-value-bind (sig p2) (rd-bytes payload pos slen "ske signature")
        (declare (ignore p2))
        (setf (parsed-ske-signature sk) sig)))
    sk))

(defun verify-ske-signature (scheme leaf-cert signed-bytes signature)
  "Verify the ServerKeyExchange signature against the leaf cert's
   public key. Returns T on success, signals error on failure."
  (declare (type (or null symbol) leaf-cert))
  (let* ((cert (if (and leaf-cert (not (integerp leaf-cert)))
                   leaf-cert
                   nil))
         (pubkey (and cert (get-cert-public-key cert))))
    (unless pubkey
      (error "tls12 client: cannot extract public key from leaf certificate"))
    (cond
      ((= scheme +sig-ecdsa-secp256r1-sha256+)
       ;; signature is an ASN.1 DER SEQUENCE { r INTEGER, s INTEGER }.
       (multiple-value-bind (r s) (decode-ecdsa-der signature)
         (unless (ecdsa:ecdsa-verify pubkey signed-bytes r s)
           (error "tls12 client: ECDSA SKE signature invalid"))))
      ((= scheme +sig-rsa-pss-rsae-sha256+)
       (unless (rsa:rsa-pss-verify pubkey signed-bytes signature :hash :sha256)
         (error "tls12 client: RSA-PSS SKE signature invalid")))
      ((= scheme +sig-rsa-pkcs1-sha256+)
       (unless (rsa:pkcs1-v15-verify pubkey signed-bytes signature :hash :sha256)
         (error "tls12 client: RSA-PKCS1 SKE signature invalid")))
      (t (error "tls12 client: unsupported SKE signature scheme ~X" scheme))))
  t)

(defun get-cert-public-key (cert)
  "Return a public-key object suitable for the verify functions.
   CERT is either:
     * a DER byte vector (preferred): we parse on demand, or
     * a parsed x509 cert object with a public key accessor.
   This is a thin shim so the test path can drop in a raw public key."
  (cond
    ((or (arrayp cert) (vectorp cert))
     ;; Minimal path: asn1 + rsa helpers do not currently expose a
     ;; full leaf -> pubkey extractor shared by ECDSA/RSA at this
     ;; layer. Callers doing real verification should pass a parsed
     ;; pubkey via the trust-store callback (see tls12-connect).
     nil)
    (t cert)))

(defun decode-ecdsa-der (der)
  "Decode a DER SEQUENCE { r INTEGER, s INTEGER } into (values r s)."
  (multiple-value-bind (obj rest)
      (asn1:der-decode der)
    (declare (ignore rest))
    (unless (and (listp obj) (= (length obj) 2))
      (error "tls12 client: malformed ECDSA signature DER"))
    (values (first obj) (second obj))))

;;; ---------------------------------------------------------------------------
;;; Client flight 1: ClientHello
;;; ---------------------------------------------------------------------------

(defun tls12-client-start-handshake (conn)
  "Return the ClientHello record bytes to write. Initializes the
   transcript; the connection's info is set to a provisional value
   (AES-128-GCM-SHA256) and updated once the server picks a suite."
  (let* ((extras (%client-extras conn :create-p t))
         (cfg (tls12-client-extras-config extras))
         (client-random (drbg:random-bytes 32)))
    (setf (tls12-connection-client-random conn) client-random)
    (setf (tls12-connection-hostname conn) (tls12-client-config-hostname cfg))
    ;; Provisional hash for transcript; we rehash when the ServerHello
    ;; settles the cipher suite.
    (setf (tls12-connection-info conn)
          (cipher-suite-meta +tls-ecdhe-ecdsa-aes-128-gcm-sha256+))
    (transcript-init conn)
    (let ((ch-msg (build-client-hello cfg client-random)))
      (transcript-update conn ch-msg)
      (setf (tls12-connection-state conn) :wait-server-hello)
      (serialize-tls-record
       (make-tls-record :content-type +content-handshake+
                        :version +tls-1.0+  ; legacy compat
                        :data ch-msg)))))

(defun %rehash-transcript-for-suite (conn new-info prior-messages)
  "Switch the transcript hash to the new cipher suite's hash function
   and replay the prior handshake messages through it."
  (setf (tls12-connection-info conn) new-info)
  (transcript-init conn)
  (dolist (m prior-messages)
    (transcript-update conn m)))

(defun tls12-client-process-server-flight1 (conn record-bytes)
  "Consume the server's flight-1 records (ServerHello, Certificate,
   optional CertificateStatus, ServerKeyExchange, ServerHelloDone)
   and return the client's flight bytes (CKE + CCS + Finished)
   concatenated ready to write."
  (let ((extras (%client-extras conn :create-p t))
        (cfg nil)
        (client-hello-msg nil)
        (server-hello-msg nil)
        (certs-msg nil)
        (cert-status-msg nil)
        (ske-msg nil)
        (shd-msg nil)
        (pos 0))
    (setf cfg (tls12-client-extras-config extras))
    (setf client-hello-msg
          ;; We need the raw ClientHello to replay into the rehashed
          ;; transcript once the server picks a suite whose hash may
          ;; differ from our provisional :sha256. Since we built it
          ;; from config, reconstruct it by snapshotting the current
          ;; transcript *before* touching it -- but we only need the
          ;; bytes, which we derive from config and the stored
          ;; client_random.
          (build-client-hello cfg (tls12-connection-client-random conn)))
    ;; Pull every record we see until we have collected the full
    ;; ServerHelloDone. The server may pack everything into one record
    ;; or fragment across several.
    (let ((hs-stream (make-buffer)))
      (loop while (< pos (length record-bytes)) do
        (multiple-value-bind (rec next) (parse-tls-record record-bytes pos)
          (unless rec (return))
          (unless (= (tls-record-content-type rec) +content-handshake+)
            (error "tls12 client: expected handshake record during flight 1"))
          (buf-bytes hs-stream (tls-record-data rec))
          (setf pos next)))
      (let* ((hs (buf-freeze hs-stream))
             (hspos 0))
        (loop while (< hspos (length hs)) do
          (multiple-value-bind (type payload next)
              (parse-handshake-header hs hspos)
            (unless type (return))
            (setf hspos next)
            (let ((msg (subseq hs (- next (+ 4 (length payload))) next)))
              (cond
                ((= type +handshake-server-hello+)
                 (setf server-hello-msg msg)
                 (let* ((sh (parse-server-hello payload))
                        (cs (parsed-server-hello-cipher-suite sh))
                        (info (cipher-suite-meta cs)))
                   (unless info
                     (error "tls12 client: server selected unsupported suite #x~X" cs))
                   (unless (parsed-server-hello-extended-master-secret-p sh)
                     (error "tls12 client: server did not echo extended_master_secret"))
                   (setf (tls12-connection-cipher-suite conn) cs)
                   (setf (tls12-connection-server-random conn)
                         (parsed-server-hello-random sh))
                   (setf (tls12-connection-alpn-protocol conn)
                         (parsed-server-hello-alpn-protocol sh))
                   (setf (tls12-client-extras-chosen-cipher-suite extras) cs)
                   ;; Rehash the transcript under the new hash function.
                   (%rehash-transcript-for-suite conn info
                                                 (list client-hello-msg msg))))
                ((= type +handshake-certificate+)
                 (setf certs-msg msg)
                 (transcript-update conn msg)
                 (setf (tls12-client-extras-server-certs extras)
                       (parse-certificate-message payload)))
                ((= type +handshake-certificate-status+)
                 (setf cert-status-msg msg)
                 (transcript-update conn msg))
                ((= type +handshake-server-key-exchange+)
                 (setf ske-msg msg)
                 (transcript-update conn msg)
                 (let ((ske (parse-server-key-exchange payload)))
                   (setf (tls12-client-extras-server-ecdhe-group extras)
                         (parsed-ske-named-curve ske))
                   (setf (tls12-client-extras-server-ecdhe-public extras)
                         (parsed-ske-server-public ske))
                   ;; Signature verification requires a parsed pubkey.
                   ;; Delegate to the trust-store callback, which is
                   ;; responsible for both chain validation and
                   ;; producing the pubkey used to verify SKE.
                   (let ((ts (tls12-client-config-trust-store cfg)))
                     (when ts
                       (let ((pubkey (funcall ts
                                              (tls12-client-extras-server-certs extras)
                                              (tls12-client-config-hostname cfg))))
                         (when pubkey
                           (let ((signed (concat-bytes
                                          (tls12-connection-client-random conn)
                                          (tls12-connection-server-random conn)
                                          (parsed-ske-signed-params ske))))
                             (verify-ske-signature (parsed-ske-sig-scheme ske)
                                                   pubkey signed
                                                   (parsed-ske-signature ske)))))))))
                ((= type +handshake-server-hello-done+)
                 (setf shd-msg msg)
                 (transcript-update conn msg)
                 (return))
                (t
                 (error "tls12 client: unexpected handshake type ~D in flight 1"
                        type))))))))
    (unless (and server-hello-msg certs-msg ske-msg shd-msg)
      (error "tls12 client: incomplete server flight 1"))
    ;; cert-status-msg is optional; it has already been folded into
    ;; the transcript above when present and is otherwise unused here.
    (when cert-status-msg cert-status-msg)
    ;; Build ClientKeyExchange, derive secrets, build CCS + Finished.
    (let* ((group (tls12-client-extras-server-ecdhe-group extras)))
      (multiple-value-bind (sk pk) (ecdhe-generate group)
        (setf (tls12-client-extras-ephemeral-private extras) sk)
        (setf (tls12-client-extras-ephemeral-public extras) pk))
      (let* ((cke-body (let ((b (make-buffer)))
                         (buf-u8-prefixed
                          b (tls12-client-extras-ephemeral-public extras))
                         (buf-freeze b)))
             (cke-msg (make-handshake-message
                       +handshake-client-key-exchange+ cke-body))
             (info (tls12-connection-info conn))
             (pms (ecdhe-derive group
                                (tls12-client-extras-ephemeral-private extras)
                                (tls12-client-extras-server-ecdhe-public extras))))
        (setf (tls12-client-extras-pre-master-secret extras) pms)
        (transcript-update conn cke-msg)
        (let* ((session-hash (transcript-snapshot conn))
               (master (derive-master-secret-ems info pms session-hash))
               (km (derive-key-block info master
                                     (tls12-connection-client-random conn)
                                     (tls12-connection-server-random conn))))
          (setf (tls12-connection-ems-session-hash conn) session-hash)
          (setf (tls12-connection-master-secret conn) master)
          (setf (tls12-connection-key-material conn) km)
          (setf (tls12-connection-read-seq conn) 0)
          (setf (tls12-connection-write-seq conn) 0))
        ;; Finished
        (let* ((transcript-hash (transcript-snapshot conn))
               (verify-data (build-finished-verify-data
                             info (tls12-connection-master-secret conn)
                             "client finished" transcript-hash))
               (finished-msg (make-handshake-message
                              +handshake-finished+ verify-data)))
          (transcript-update conn finished-msg)
          (let* ((enc (encrypt-record finished-msg +content-handshake+
                                      (%own-write-key conn)
                                      (%own-write-iv conn)
                                      (tls12-connection-write-seq conn)
                                      info))
                 (ccs-record (serialize-tls-record
                              (make-tls-record
                               :content-type +content-change-cipher-spec+
                               :version +tls-1.2+
                               :data (let ((a (make-array 1 :element-type '(unsigned-byte 8))))
                                       (setf (aref a 0) 1) a))))
                 (cke-record (serialize-tls-record
                              (make-tls-record :content-type +content-handshake+
                                               :version +tls-1.2+
                                               :data cke-msg)))
                 (fin-record (serialize-tls-record
                              (make-tls-record :content-type +content-handshake+
                                               :version +tls-1.2+
                                               :data enc))))
            (incf (tls12-connection-write-seq conn))
            (setf (tls12-connection-state conn) :wait-server-finished)
            (concat-bytes cke-record ccs-record fin-record)))))))

(defun tls12-client-finish-handshake (conn bytes)
  "Consume the server's optional plaintext NewSessionTicket, CCS, and
   encrypted Finished, and leave CONN in :connected state. BYTES is the
   raw wire stream from the server side.

   Per RFC 5077 Section 3.3: NewSessionTicket is a plaintext handshake
   message sent BEFORE ChangeCipherSpec."
  (let ((pos 0)
        (ccs-seen nil)
        (encrypted-hs nil))
    ;; Optionally: plaintext NewSessionTicket record(s) before CCS.
    (loop
      (multiple-value-bind (rec next) (parse-tls-record bytes pos)
        (unless rec (error "tls12 client: truncated server flight"))
        (cond
          ;; CCS: transition to encrypted mode
          ((= (tls-record-content-type rec) +content-change-cipher-spec+)
           (setf ccs-seen t pos next)
           (return))
          ;; Plaintext handshake: should be NewSessionTicket
          ((= (tls-record-content-type rec) +content-handshake+)
           (let ((data (tls-record-data rec)))
             (multiple-value-bind (type payload hs-next)
                 (parse-handshake-header data)
               (declare (ignore payload hs-next))
               (unless (= type +handshake-new-session-ticket+)
                 (error "tls12 client: expected NST or CCS, got handshake type ~D" type))
               ;; Fold into transcript so server Finished hash aligns
               (transcript-update conn data)))
           (setf pos next))
          (t
           (error "tls12 client: expected NST or CCS, got record type ~D"
                  (tls-record-content-type rec))))))
    ;; Record after CCS: encrypted Finished
    (multiple-value-bind (rec next) (parse-tls-record bytes pos)
      (declare (ignore next))
      (unless rec (error "tls12 client: truncated encrypted server Finished"))
      (unless (= (tls-record-content-type rec) +content-handshake+)
        (error "tls12 client: expected encrypted handshake, got ~D"
               (tls-record-content-type rec)))
      (setf encrypted-hs (tls-record-data rec)))
    (unless ccs-seen (error "tls12 client: missing server CCS"))
    (let* ((info (tls12-connection-info conn))
           (plaintext (decrypt-record encrypted-hs +content-handshake+
                                      (%peer-write-key conn)
                                      (%peer-write-iv conn)
                                      (tls12-connection-read-seq conn)
                                      info)))
      (incf (tls12-connection-read-seq conn))
      ;; The plaintext should contain exactly the server Finished.
      (multiple-value-bind (type payload next)
          (parse-handshake-header plaintext)
        (declare (ignore next))
        (unless (= type +handshake-finished+)
          (error "tls12 client: expected Finished, got handshake type ~D" type))
        (let ((expected (build-finished-verify-data
                         info
                         (tls12-connection-master-secret conn)
                         "server finished"
                         (transcript-snapshot conn))))
          (unless (and (= (length payload) (length expected))
                       (equalp payload expected))
            (error "tls12 client: server Finished mismatch")))
        (transcript-update conn plaintext))
      (setf (tls12-connection-state conn) :connected))))

;;; ---------------------------------------------------------------------------
;;; Transport-driven client connect
;;; ---------------------------------------------------------------------------

(defun tls12-connect (transport config)
  "Initiate a TLS 1.2 handshake as a client over TRANSPORT. Returns a
   TLS12-STREAM on success."
  (let* ((conn (make-tls12-connection :role :client)))
    (let ((extras (%client-extras conn :create-p t)))
      (setf (tls12-client-extras-config extras) config))
    ;; Flight 1: ClientHello
    (let ((ch-record (tls12-client-start-handshake conn)))
      (%write-all transport ch-record))
    ;; Read the server's flight until ServerHelloDone.
    (let ((server-bytes (make-buffer))
          (done nil))
      (loop until done do
        (let ((rec-bytes (%read-next-record transport)))
          (unless rec-bytes (error "tls12-connect: server closed during flight 1"))
          (buf-bytes server-bytes rec-bytes)
          ;; Heuristic: parse what we have, stop when we see a
          ;; ServerHelloDone handshake message. Keep reading otherwise.
          (let ((frozen (buf-freeze server-bytes)))
            (when (%contains-server-hello-done-p frozen)
              (let ((flight2 (tls12-client-process-server-flight1 conn frozen)))
                (%write-all transport flight2))
              (setf done t)))))
      ;; Read the server's CCS + encrypted Finished.
      (let* ((r1 (%read-next-record transport))
             (r2 (%read-next-record transport)))
        (unless (and r1 r2) (error "tls12-connect: server closed before Finished"))
        (tls12-client-finish-handshake conn (concat-bytes r1 r2))))
    (make-tls12-stream :connection conn :transport transport)))

(defun %contains-server-hello-done-p (bytes)
  "Scan a plaintext handshake record stream and return T if it
   contains a ServerHelloDone handshake message. Used to decide when
   the server's flight 1 has been fully received."
  (let ((pos 0))
    (loop while (< pos (length bytes)) do
      (multiple-value-bind (rec next) (parse-tls-record bytes pos)
        (unless rec (return-from %contains-server-hello-done-p nil))
        (unless (= (tls-record-content-type rec) +content-handshake+)
          (return-from %contains-server-hello-done-p nil))
        (let ((hs (tls-record-data rec))
              (hspos 0))
          (loop while (< hspos (length hs)) do
            (multiple-value-bind (type payload next2)
                (parse-handshake-header hs hspos)
              (declare (ignore payload))
              (unless type (return))
              (when (= type +handshake-server-hello-done+)
                (return-from %contains-server-hello-done-p t))
              (setf hspos next2))))
        (setf pos next)))
    nil))
