;;;; FFI Bindings for OpenSSL (libssl and libcrypto)
;;;;
;;;; This file contains all Foreign Function Interface bindings for both
;;;; SSL/TLS operations and cryptographic functions.

(defpackage :epsilon.crypto.ffi
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.foreign)
   (#:lib #:epsilon.library))
  (:export
   ;; Re-export all FFI functions with % prefix
   ;; Modern OpenSSL 3.0 functions
   #:%evp-pkey-ctx-new-from-name
   #:%evp-pkey-ctx-free
   #:%evp-pkey-keygen-init
   #:%evp-pkey-keygen
   #:%evp-pkey-generate
   #:%evp-pkey-ctx-set-rsa-keygen-bits
   #:%evp-pkey-ctx-set-ec-paramgen-curve-nid
   #:%obj-sn2nid
   #:%tls-client-method
   #:%tls-server-method
   #:%ssl-ctx-new
   #:%ssl-ctx-free
   #:%ssl-ctx-use-certificate-file
   #:%ssl-ctx-use-privatekey-file
   #:%ssl-ctx-check-private-key
   #:%ssl-ctx-set-verify
   #:%ssl-ctx-ctrl
   #:%ssl-ctx-set-cipher-list
   #:%ssl-ctx-use-certificate
   #:%ssl-ctx-use-privatekey
   #:%ssl-ctx-load-verify-locations
   #:%ssl-ctx-set-client-ca-list
   #:%ssl-ctx-use-certificate-chain-file
   #:%ssl-ctx-set-alpn-protos
   #:%ssl-ctx-set-alpn-select-cb
   #:%ssl-set-alpn-protos
   #:%ssl-get0-alpn-selected
   #:%ssl-set-tlsext-host-name
   #:%ssl-ctx-set-session-cache-mode
   #:%ssl-get-verify-result
   #:%x509-name-stack-new
   #:%x509-name-stack-push
   #:%ssl-new
   #:%ssl-free
   #:%ssl-set-fd
   #:%ssl-connect
   #:%ssl-accept
   #:%ssl-shutdown
   #:%ssl-read
   #:%ssl-write
   #:%ssl-pending
   #:%ssl-get-error
   #:%ssl-get-version
   #:%ssl-get-cipher
   #:%ssl-get-peer-certificate
   #:%ssl-load-client-ca-file
   #:%evp-pkey-new
   #:%evp-pkey-free
   #:%evp-pkey-size
   #:%evp-pkey-bits
   #:%evp-pkey-id
   ;; #:%rsa-new  ; Deprecated in OpenSSL 3.0
   ;; #:%rsa-free  ; Deprecated in OpenSSL 3.0
   ;; #:%rsa-generate-key-ex  ; Deprecated in OpenSSL 3.0
   ;; #:%evp-pkey-assign-rsa  ; Deprecated in OpenSSL 3.0
   ;; #:%evp-pkey-get1-rsa  ; Deprecated in OpenSSL 3.0
   #:%bn-new
   #:%bn-free
   #:%bn-set-word
   ;; #:%ec-key-new-by-curve-name  ; Deprecated in OpenSSL 3.0
   ;; #:%ec-key-free  ; Deprecated in OpenSSL 3.0
   ;; #:%ec-key-generate-key  ; Deprecated in OpenSSL 3.0
   ;; #:%evp-pkey-assign-ec-key  ; Deprecated in OpenSSL 3.0
   ;; #:%evp-pkey-get1-ec-key  ; Deprecated in OpenSSL 3.0
   #:%obj-txt2nid
   #:%evp-pkey-ctx-new-id
   #:%evp-pkey-ctx-new
   #:%pem-read-bio-pubkey
   #:%pem-read-bio-privatekey
   #:%pem-write-bio-pubkey
   #:%pem-write-bio-privatekey
   #:%bio-new-mem-buf
   #:%bio-new
   #:%bio-s-mem
   #:%bio-free
   #:%bio-read
   #:%bio-write
   #:%bio-ctrl
   #:%evp-md-ctx-new
   #:%evp-md-ctx-free
   #:%evp-get-digestbyname
   #:%evp-sha256
   #:%evp-digestinit-ex
   #:%evp-digestupdate
   #:%evp-digestfinal-ex
   #:%evp-digestsigninit
   #:%evp-digestsignupdate
   #:%evp-digestsignfinal
   #:%evp-digestverifyinit
   #:%evp-digestverifyupdate
   #:%evp-digestverifyfinal
   #:%evp-pkey-encrypt-init
   #:%evp-pkey-encrypt
   #:%evp-pkey-decrypt-init
   #:%evp-pkey-decrypt
   #:%x509-new
   #:%x509-free
   #:%x509-set-version
   #:%x509-set-serialnumber
   #:%x509-get-serialnumber
   #:%x509-set-notbefore
   #:%x509-set-notafter
   #:%x509-set-pubkey
   #:%x509-get-pubkey
   #:%x509-sign
   #:%x509-verify
   #:%x509-check-private-key
   #:%x509-extension-create-by-nid
   #:%x509-add-ext
   #:%x509v3-ext-conf-nid
   #:%x509-extension-free
   #:%pem-read-bio-x509
   #:%pem-write-bio-x509
   #:%x509-get-subject-name
   #:%x509-get-issuer-name
   #:%x509-name-oneline
   #:%x509-name-add-entry-by-txt
   #:%x509-name-new
   #:%x509-name-free
   #:%x509-set-subject-name
   #:%x509-set-issuer-name
   #:%asn1-integer-new
   #:%asn1-integer-set
   #:%asn1-integer-get
   #:%asn1-integer-free
   #:%asn1-time-new
   #:%asn1-time-free
   #:%x509-time-adj-ex
   #:%x509-req-new
   #:%x509-req-free
   #:%x509-req-set-pubkey
   #:%x509-req-sign
   #:%x509-req-verify
   #:%x509-req-get-subject-name
   #:%x509-req-get-pubkey
   #:%x509-req-set-subject-name
   #:%x509-req-set-version
   #:%pem-read-bio-x509-req
   #:%pem-write-bio-x509-req
   #:%rand-bytes
   #:%rand-status
   #:%rand-seed
   #:%err-get-error
   #:%err-error-string
   ;; Security functions
   #:%openssl-init-crypto
   #:%openssl-cleanse
   #:%crypto-memcmp
   #:%openssl-version
   ;; KDF functions
   #:%pkcs5-pbkdf2-hmac
   #:%evp-pbe-scrypt
   #:%evp-pkey-ctx-set-hkdf-md
   #:%evp-pkey-ctx-set1-hkdf-salt
   #:%evp-pkey-ctx-set1-hkdf-key
   #:%evp-pkey-ctx-add1-hkdf-info
   #:%evp-pkey-derive-init
   #:%evp-pkey-derive
   #:+evp-pkey-hkdf+
   ;; Cipher functions for AEAD
   #:%evp-cipher-ctx-new
   #:%evp-cipher-ctx-free
   #:%evp-get-cipherbyname
   #:%evp-encryptinit-ex
   #:%evp-encryptupdate
   #:%evp-encryptfinal-ex
   #:%evp-decryptinit-ex
   #:%evp-decryptupdate
   #:%evp-decryptfinal-ex
   #:%evp-cipher-ctx-ctrl
   ;; Error condition and accessors
   #:crypto-error
   #:crypto-error-code
   #:crypto-error-string
   ;; X509 Certificate functions
   #:%x509-new
   #:%x509-free
   #:%x509-set-version
   #:%x509-set-serialnumber
   #:%x509-set1-notbefore
   #:%x509-set1-notafter
   #:%x509-set-pubkey
   #:%x509-set-issuer-name
   #:%x509-set-subject-name
   #:%x509-get-subject-name
   #:%x509-get-issuer-name
   #:%x509-get-pubkey
   #:%x509-sign
   #:%x509-verify
   #:%x509-check-private-key
   #:%x509-name-oneline
   #:%x509-name-entry-create-by-txt
   #:%x509-name-add-entry
   #:%x509-name-entry-free
   #:%x509-req-new
   #:%x509-req-free
   #:%x509-req-set-version
   #:%x509-req-set-subject-name
   #:%x509-req-set-pubkey
   #:%x509-req-sign
   #:%x509-req-get-subject-name
   #:%x509-req-get-pubkey
   #:%pem-write-bio-x509
   #:%pem-read-bio-x509
   #:%pem-write-bio-x509-req
   #:%pem-read-bio-x509-req
   #:%asn1-integer-new
   #:%asn1-integer-free
   #:%asn1-integer-set
   #:%asn1-time-new
   #:%asn1-time-free
   #:%asn1-time-set-string))

(in-package :epsilon.crypto.ffi)

;;;; Error Handling

(define-condition crypto-error (error)
  ((code :initarg :code :reader crypto-error-code :type integer)
   (message :initarg :message :reader crypto-error-string :type string))
  (:documentation "Cryptographic error condition signaled when cryptographic operations fail.")
  (:report (lambda (condition stream)
             (format stream "Crypto error ~A: ~A"
                     (crypto-error-code condition)
                     (crypto-error-string condition)))))

;;;; SSL/TLS Library FFI Bindings (libssl)

;; Find OpenSSL libraries using epsilon.library
(defparameter *ssl-library*
  (handler-case
      (lib:find-library :openssl)
    (error ()
      "libssl")))

(defparameter *crypto-library*
  (handler-case
      (lib:find-library :crypto)
    (error ()
      "libcrypto")))

;; SSL Context management
(ffi:defshared %tls-client-method "TLS_client_method" *ssl-library* :pointer ()
  :documentation "Get TLS client method")

(ffi:defshared %tls-server-method "TLS_server_method" *ssl-library* :pointer ()
  :documentation "Get TLS server method")

(ffi:defshared %ssl-ctx-new "SSL_CTX_new" *ssl-library* :pointer (method :pointer)
  :documentation "Create new SSL context")

(ffi:defshared %ssl-ctx-free "SSL_CTX_free" *ssl-library* :void (ctx :pointer)
  :documentation "Free SSL context")

(ffi:defshared %ssl-ctx-use-certificate-file "SSL_CTX_use_certificate_file" *ssl-library* :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load certificate file")

(ffi:defshared %ssl-ctx-use-privatekey-file "SSL_CTX_use_PrivateKey_file" *ssl-library* :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load private key file")

(ffi:defshared %ssl-ctx-check-private-key "SSL_CTX_check_private_key" *ssl-library* :int
  (ctx :pointer)
  :documentation "Check private key matches certificate")

(ffi:defshared %ssl-ctx-set-verify "SSL_CTX_set_verify" *ssl-library* :void
  (ctx :pointer) (mode :int) (callback :pointer)
  :documentation "Set verification mode")

(ffi:defshared %ssl-ctx-ctrl "SSL_CTX_ctrl" *ssl-library* :long
  (ctx :pointer) (cmd :int) (larg :long) (parg :pointer)
  :documentation "Generic SSL context control function")

(ffi:defshared %ssl-ctx-set-cipher-list "SSL_CTX_set_cipher_list" "libssl" :int
  (ctx :pointer) (str :string)
  :documentation "Set cipher list")

(ffi:defshared %ssl-ctx-use-certificate "SSL_CTX_use_certificate" "libssl" :int
  (ctx :pointer) (x509 :pointer)
  :documentation "Set certificate in SSL context")

(ffi:defshared %ssl-ctx-use-privatekey "SSL_CTX_use_PrivateKey" "libssl" :int
  (ctx :pointer) (pkey :pointer)
  :documentation "Set private key in SSL context")

(ffi:defshared %ssl-ctx-load-verify-locations "SSL_CTX_load_verify_locations" "libssl" :int
  (ctx :pointer) (ca-file :string) (ca-path :string)
  :documentation "Load CA certificates for verification")

(ffi:defshared %ssl-ctx-set-client-ca-list "SSL_CTX_set_client_CA_list" "libssl" :void
  (ctx :pointer) (list :pointer)
  :documentation "Set list of acceptable client CAs")

(ffi:defshared %ssl-ctx-use-certificate-chain-file "SSL_CTX_use_certificate_chain_file" "libssl" :int
  (ctx :pointer) (file :string)
  :documentation "Load certificate chain from file")

(ffi:defshared %ssl-ctx-set-alpn-protos "SSL_CTX_set_alpn_protos" "libssl" :int
  (ctx :pointer) (protos :pointer) (protos-len :unsigned-int)
  :documentation "Set ALPN protocols for context")

(ffi:defshared %ssl-ctx-set-alpn-select-cb "SSL_CTX_set_alpn_select_cb" "libssl" :void
  (ctx :pointer) (cb :pointer) (arg :pointer)
  :documentation "Set ALPN selection callback")

(ffi:defshared %ssl-set-alpn-protos "SSL_set_alpn_protos" "libssl" :int
  (ssl :pointer) (protos :pointer) (protos-len :unsigned-int)
  :documentation "Set ALPN protocols for SSL connection")

(ffi:defshared %ssl-get0-alpn-selected "SSL_get0_alpn_selected" "libssl" :void
  (ssl :pointer) (data :pointer) (len :pointer)
  :documentation "Get selected ALPN protocol")

(ffi:defshared %ssl-set-tlsext-host-name "SSL_set_tlsext_host_name" "libssl" :int
  (ssl :pointer) (name :string)
  :documentation "Set SNI hostname")

(ffi:defshared %ssl-ctx-set-session-cache-mode "SSL_CTX_set_session_cache_mode" "libssl" :long
  (ctx :pointer) (mode :long)
  :documentation "Set session cache mode")

(ffi:defshared %ssl-get-peer-certificate "SSL_get_peer_certificate" "libssl" :pointer
  (ssl :pointer)
  :documentation "Get peer certificate from SSL connection")

(ffi:defshared %ssl-get-verify-result "SSL_get_verify_result" "libssl" :long
  (ssl :pointer)
  :documentation "Get verification result")

(ffi:defshared %x509-name-stack-new "sk_X509_NAME_new_null" "libcrypto" :pointer
  ()
  :documentation "Create new X509_NAME stack")

(ffi:defshared %x509-name-stack-push "sk_X509_NAME_push" "libcrypto" :int
  (stack :pointer) (name :pointer)
  :documentation "Push X509_NAME to stack")

(ffi:defshared %ssl-load-client-ca-file "SSL_load_client_CA_file" "libssl" :pointer
  (file :string)
  :documentation "Load client CA list from file")

;; SSL Connection management
(ffi:defshared %ssl-new "SSL_new" "libssl" :pointer (ctx :pointer)
  :documentation "Create new SSL connection")

(ffi:defshared %ssl-free "SSL_free" "libssl" :void (ssl :pointer)
  :documentation "Free SSL connection")

(ffi:defshared %ssl-set-fd "SSL_set_fd" "libssl" :int
  (ssl :pointer) (fd :int)
  :documentation "Set file descriptor for SSL connection")

(ffi:defshared %ssl-connect "SSL_connect" "libssl" :int (ssl :pointer)
  :documentation "Initiate SSL handshake as client")

(ffi:defshared %ssl-accept "SSL_accept" "libssl" :int (ssl :pointer)
  :documentation "Accept SSL handshake as server")

(ffi:defshared %ssl-shutdown "SSL_shutdown" "libssl" :int (ssl :pointer)
  :documentation "Shutdown SSL connection")

;; SSL I/O operations
(ffi:defshared %ssl-read "SSL_read" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Read from SSL connection")

(ffi:defshared %ssl-write "SSL_write" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Write to SSL connection")

(ffi:defshared %ssl-pending "SSL_pending" "libssl" :int (ssl :pointer)
  :documentation "Get number of bytes pending in SSL buffer")

;; SSL State and Info
(ffi:defshared %ssl-get-error "SSL_get_error" "libssl" :int
  (ssl :pointer) (ret :int)
  :documentation "Get SSL error code")

(ffi:defshared %ssl-get-version "SSL_get_version" "libssl" :pointer (ssl :pointer)
  :documentation "Get SSL/TLS version string")

(ffi:defshared %ssl-get-cipher "SSL_get_cipher" "libssl" :pointer (ssl :pointer)
  :documentation "Get current cipher name")

;;;; Cryptography Library FFI Bindings (libcrypto)

;; Modern OpenSSL 3.0 EVP API for key generation
(ffi:defshared %evp-pkey-ctx-new-from-name "EVP_PKEY_CTX_new_from_name" "libcrypto" :pointer
  (libctx :pointer) (name :string) (propquery :pointer)
  :documentation "Create EVP_PKEY_CTX from algorithm name (OpenSSL 3.0)")

(ffi:defshared %evp-pkey-ctx-free "EVP_PKEY_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free EVP_PKEY_CTX")

(ffi:defshared %evp-pkey-keygen-init "EVP_PKEY_keygen_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize key generation")

(ffi:defshared %evp-pkey-keygen "EVP_PKEY_keygen" "libcrypto" :int
  (ctx :pointer) (ppkey :pointer)
  :documentation "Generate key pair")

(ffi:defshared %evp-pkey-generate "EVP_PKEY_generate" "libcrypto" :int
  (ctx :pointer) (ppkey :pointer)
  :documentation "Generate key pair")

(ffi:defshared %evp-pkey-ctx-set-rsa-keygen-bits "EVP_PKEY_CTX_set_rsa_keygen_bits" "libcrypto" :int
  (ctx :pointer) (bits :int)
  :documentation "Set RSA key size in bits")

(ffi:defshared %evp-pkey-ctx-set-ec-paramgen-curve-nid "EVP_PKEY_CTX_set_ec_paramgen_curve_nid" "libcrypto" :int
  (ctx :pointer) (nid :int)
  :documentation "Set EC curve by NID for key generation")

(ffi:defshared %obj-sn2nid "OBJ_sn2nid" "libcrypto" :int
  (sn :string)
  :documentation "Convert short name to NID")

;; EVP (Envelope) API - High-level cryptographic functions
(ffi:defshared %evp-pkey-new "EVP_PKEY_new" "libcrypto" :pointer ()
  :documentation "Create new EVP_PKEY structure")

(ffi:defshared %evp-pkey-free "EVP_PKEY_free" "libcrypto" :void (pkey :pointer)
  :documentation "Free EVP_PKEY structure")

(ffi:defshared %evp-pkey-size "EVP_PKEY_size" "libcrypto" :int (pkey :pointer)
  :documentation "Get key size in bytes")

(ffi:defshared %evp-pkey-bits "EVP_PKEY_get_bits" "libcrypto" :int (pkey :pointer)
  :documentation "Get key size in bits")

(ffi:defshared %evp-pkey-id "EVP_PKEY_get_id" "libcrypto" :int (pkey :pointer)
  :documentation "Get key type identifier")

;; Hash/Digest Functions
(ffi:defshared %evp-sha256 "EVP_sha256" "libcrypto" :pointer ()
  :documentation "Get EVP_MD for SHA-256")

;; RSA key generation - DEPRECATED in OpenSSL 3.0
;; (ffi:defshared %rsa-new "RSA_new" "libcrypto" :pointer ()
;;   :documentation "Create new RSA structure")
;;
;; (ffi:defshared %rsa-free "RSA_free" "libcrypto" :void (rsa :pointer)
;;   :documentation "Free RSA structure")
;;
;; (ffi:defshared %rsa-generate-key-ex "RSA_generate_key_ex" "libcrypto" :int
;;   (rsa :pointer) (bits :int) (e :pointer) (cb :pointer)
;;   :documentation "Generate RSA key pair")
;;
;; (ffi:defshared %evp-pkey-assign-rsa "EVP_PKEY_assign_RSA" "libcrypto" :int
;;   (pkey :pointer) (rsa :pointer)
;;   :documentation "Assign RSA key to EVP_PKEY")

(ffi:defshared %evp-pkey-get1-rsa "EVP_PKEY_get1_RSA" "libcrypto" :pointer
  (pkey :pointer)
  :documentation "Get RSA key from EVP_PKEY")

;; BIGNUM operations for RSA exponent
(ffi:defshared %bn-new "BN_new" "libcrypto" :pointer ()
  :documentation "Create new BIGNUM")

(ffi:defshared %bn-free "BN_free" "libcrypto" :void (bn :pointer)
  :documentation "Free BIGNUM")

(ffi:defshared %bn-set-word "BN_set_word" "libcrypto" :int
  (bn :pointer) (w :unsigned-long)
  :documentation "Set BIGNUM from word")

;; EC key generation - DEPRECATED in OpenSSL 3.0
;; (ffi:defshared %ec-key-new-by-curve-name "EC_KEY_new_by_curve_name" "libcrypto" :pointer
;;   (nid :int)
;;   :documentation "Create EC key for named curve")
;;
;; (ffi:defshared %ec-key-free "EC_KEY_free" "libcrypto" :void (key :pointer)
;;   :documentation "Free EC key")
;;
;; (ffi:defshared %ec-key-generate-key "EC_KEY_generate_key" "libcrypto" :int
;;   (key :pointer)
;;   :documentation "Generate EC key pair")
;;
;; (ffi:defshared %evp-pkey-assign-ec-key "EVP_PKEY_assign_EC_KEY" "libcrypto" :int
;;   (pkey :pointer) (ec :pointer)
;;   :documentation "Assign EC key to EVP_PKEY")

(ffi:defshared %evp-pkey-get1-ec-key "EVP_PKEY_get1_EC_KEY" "libcrypto" :pointer
  (pkey :pointer)
  :documentation "Get EC key from EVP_PKEY")

(ffi:defshared %obj-txt2nid "OBJ_txt2nid" "libcrypto" :int
  (s :string)
  :documentation "Convert text to NID")

;; Ed25519 key generation (EVP only)
(ffi:defshared %evp-pkey-ctx-new-id "EVP_PKEY_CTX_new_id" "libcrypto" :pointer
  (id :int) (e :pointer)
  :documentation "Create EVP_PKEY_CTX for algorithm")

;; Removed duplicates - these are defined in the OpenSSL 3.0 section above

(ffi:defshared %evp-pkey-ctx-new "EVP_PKEY_CTX_new" "libcrypto" :pointer
  (pkey :pointer) (e :pointer)
  :documentation "Create EVP_PKEY_CTX from key")

;; PEM I/O
(ffi:defshared %pem-read-bio-pubkey "PEM_read_bio_PUBKEY" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read public key from BIO in PEM format")

(ffi:defshared %pem-read-bio-privatekey "PEM_read_bio_PrivateKey" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read private key from BIO in PEM format")

(ffi:defshared %pem-write-bio-pubkey "PEM_write_bio_PUBKEY" "libcrypto" :int
  (bio :pointer) (x :pointer)
  :documentation "Write public key to BIO in PEM format")

(ffi:defshared %pem-write-bio-privatekey "PEM_write_bio_PrivateKey" "libcrypto" :int
  (bio :pointer) (x :pointer) (enc :pointer) (kstr :pointer) 
  (klen :int) (cb :pointer) (u :pointer)
  :documentation "Write private key to BIO in PEM format")

;; BIO operations
(ffi:defshared %bio-new-mem-buf "BIO_new_mem_buf" "libcrypto" :pointer
  (buf :pointer) (len :int)
  :documentation "Create memory BIO from buffer")

(ffi:defshared %bio-new "BIO_new" "libcrypto" :pointer (type :pointer)
  :documentation "Create new BIO")

(ffi:defshared %bio-s-mem "BIO_s_mem" "libcrypto" :pointer ()
  :documentation "Get memory BIO method")

(ffi:defshared %bio-free "BIO_free" "libcrypto" :int (bio :pointer)
  :documentation "Free BIO")

(ffi:defshared %bio-read "BIO_read" "libcrypto" :int
  (bio :pointer) (data :pointer) (dlen :int)
  :documentation "Read from BIO")

(ffi:defshared %bio-write "BIO_write" "libcrypto" :int
  (bio :pointer) (data :pointer) (dlen :int)
  :documentation "Write to BIO")

(ffi:defshared %bio-ctrl "BIO_ctrl" "libcrypto" :long
  (bio :pointer) (cmd :int) (larg :long) (parg :pointer)
  :documentation "BIO control operation")

;; Digest operations
(ffi:defshared %evp-md-ctx-new "EVP_MD_CTX_new" "libcrypto" :pointer ()
  :documentation "Create new digest context")

(ffi:defshared %evp-md-ctx-free "EVP_MD_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free digest context")

(ffi:defshared %evp-get-digestbyname "EVP_get_digestbyname" "libcrypto" :pointer
  (name :string)
  :documentation "Get digest algorithm by name")

(ffi:defshared %evp-digestinit-ex "EVP_DigestInit_ex" "libcrypto" :int
  (ctx :pointer) (type :pointer) (impl :pointer)
  :documentation "Initialize digest context")

(ffi:defshared %evp-digestupdate "EVP_DigestUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update digest with data")

(ffi:defshared %evp-digestfinal-ex "EVP_DigestFinal_ex" "libcrypto" :int
  (ctx :pointer) (md :pointer) (s :pointer)
  :documentation "Finalize digest")

;; Signing operations
(ffi:defshared %evp-digestsigninit "EVP_DigestSignInit" "libcrypto" :int
  (ctx :pointer) (pctx :pointer) (type :pointer) (e :pointer) (pkey :pointer)
  :documentation "Initialize signing operation")

(ffi:defshared %evp-digestsignupdate "EVP_DigestSignUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update data to be signed")

(ffi:defshared %evp-digestsignfinal "EVP_DigestSignFinal" "libcrypto" :int
  (ctx :pointer) (sig :pointer) (siglen :pointer)
  :documentation "Finalize signing operation")

;; Verification operations
(ffi:defshared %evp-digestverifyinit "EVP_DigestVerifyInit" "libcrypto" :int
  (ctx :pointer) (pctx :pointer) (type :pointer) (e :pointer) (pkey :pointer)
  :documentation "Initialize verification operation")

(ffi:defshared %evp-digestverifyupdate "EVP_DigestVerifyUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update data to be verified")

(ffi:defshared %evp-digestverifyfinal "EVP_DigestVerifyFinal" "libcrypto" :int
  (ctx :pointer) (sig :pointer) (siglen :unsigned-long)
  :documentation "Finalize verification operation")

;; Public key encryption
(ffi:defshared %evp-pkey-encrypt-init "EVP_PKEY_encrypt_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize public key encryption")

(ffi:defshared %evp-pkey-encrypt "EVP_PKEY_encrypt" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outlen :pointer) (in :pointer) (inlen :unsigned-long)
  :documentation "Encrypt with public key")

(ffi:defshared %evp-pkey-decrypt-init "EVP_PKEY_decrypt_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize public key decryption")

(ffi:defshared %evp-pkey-decrypt "EVP_PKEY_decrypt" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outlen :pointer) (in :pointer) (inlen :unsigned-long)
  :documentation "Decrypt with private key")

;; X.509 Certificate operations
(ffi:defshared %x509-new "X509_new" "libcrypto" :pointer ()
  :documentation "Create new X509 certificate")

(ffi:defshared %x509-free "X509_free" "libcrypto" :void (x509 :pointer)
  :documentation "Free X509 certificate")

(ffi:defshared %x509-set-version "X509_set_version" "libcrypto" :int
  (x509 :pointer) (version :long)
  :documentation "Set certificate version")

(ffi:defshared %x509-set-serialnumber "X509_set_serialNumber" "libcrypto" :int
  (x509 :pointer) (serial :pointer)
  :documentation "Set certificate serial number")

(ffi:defshared %x509-get-serialnumber "X509_get_serialNumber" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate serial number")

(ffi:defshared %x509-set-notbefore "X509_set1_notBefore" "libcrypto" :int
  (x509 :pointer) (tm :pointer)
  :documentation "Set certificate not-before time")

(ffi:defshared %x509-set-notafter "X509_set1_notAfter" "libcrypto" :int
  (x509 :pointer) (tm :pointer)
  :documentation "Set certificate not-after time")

(ffi:defshared %x509-set-pubkey "X509_set_pubkey" "libcrypto" :int
  (x509 :pointer) (pkey :pointer)
  :documentation "Set certificate public key")

(ffi:defshared %x509-get-pubkey "X509_get_pubkey" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate public key")

(ffi:defshared %x509-sign "X509_sign" "libcrypto" :int
  (x509 :pointer) (pkey :pointer) (md :pointer)
  :documentation "Sign certificate")

(ffi:defshared %x509-verify "X509_verify" "libcrypto" :int
  (x509 :pointer) (pkey :pointer)
  :documentation "Verify certificate signature")

(ffi:defshared %x509-check-private-key "X509_check_private_key" "libcrypto" :int
  (x509 :pointer) (pkey :pointer)
  :documentation "Check if private key matches certificate")

(ffi:defshared %x509-extension-create-by-nid "X509_EXTENSION_create_by_NID" "libcrypto" :pointer
  (ex :pointer) (nid :int) (crit :int) (data :pointer)
  :documentation "Create X509 extension by NID")

(ffi:defshared %x509-add-ext "X509_add_ext" "libcrypto" :int
  (x509 :pointer) (ex :pointer) (loc :int)
  :documentation "Add extension to certificate")

(ffi:defshared %x509v3-ext-conf-nid "X509V3_EXT_conf_nid" "libcrypto" :pointer
  (conf :pointer) (ctx :pointer) (ext-nid :int) (value :string)
  :documentation "Configure X509v3 extension")

(ffi:defshared %x509-extension-free "X509_EXTENSION_free" "libcrypto" :void
  (ex :pointer)
  :documentation "Free X509 extension")

(ffi:defshared %pem-read-bio-x509 "PEM_read_bio_X509" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read X509 certificate from BIO in PEM format")

(ffi:defshared %pem-write-bio-x509 "PEM_write_bio_X509" "libcrypto" :int
  (bio :pointer) (x :pointer)
  :documentation "Write X509 certificate to BIO in PEM format")

(ffi:defshared %x509-get-subject-name "X509_get_subject_name" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate subject name")

(ffi:defshared %x509-get-issuer-name "X509_get_issuer_name" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate issuer name")

(ffi:defshared %x509-name-oneline "X509_NAME_oneline" "libcrypto" :pointer
  (name :pointer) (buf :pointer) (size :int)
  :documentation "Convert X509 name to string")

(ffi:defshared %x509-name-new "X509_NAME_new" "libcrypto" :pointer
  :documentation "Create new X509_NAME")

(ffi:defshared %x509-name-free "X509_NAME_free" "libcrypto" :void
  (name :pointer)
  :documentation "Free X509_NAME")

(ffi:defshared %x509-name-entry-create-by-txt "X509_NAME_ENTRY_create_by_txt" "libcrypto" :pointer
  (ne :pointer) (field :string) (type :int) (bytes :string) (len :int)
  :documentation "Create X509_NAME_ENTRY from text")

(ffi:defshared %x509-name-add-entry "X509_NAME_add_entry" "libcrypto" :int
  (name :pointer) (ne :pointer) (loc :int) (set :int)
  :documentation "Add entry to X509_NAME")

(ffi:defshared %x509-name-entry-free "X509_NAME_ENTRY_free" "libcrypto" :void
  (ne :pointer)
  :documentation "Free X509_NAME_ENTRY")

;;;; X509_REQ (Certificate Request) Functions

(ffi:defshared %x509-req-new "X509_REQ_new" "libcrypto" :pointer
  :documentation "Create new X509_REQ")

(ffi:defshared %x509-req-free "X509_REQ_free" "libcrypto" :void
  (req :pointer)
  :documentation "Free X509_REQ")

(ffi:defshared %x509-req-set-version "X509_REQ_set_version" "libcrypto" :int
  (req :pointer) (version :long)
  :documentation "Set certificate request version")

(ffi:defshared %x509-req-set-subject-name "X509_REQ_set_subject_name" "libcrypto" :int
  (req :pointer) (name :pointer)
  :documentation "Set certificate request subject name")

(ffi:defshared %x509-req-set-pubkey "X509_REQ_set_pubkey" "libcrypto" :int
  (req :pointer) (pkey :pointer)
  :documentation "Set certificate request public key")

(ffi:defshared %x509-req-sign "X509_REQ_sign" "libcrypto" :int
  (req :pointer) (pkey :pointer) (md :pointer)
  :documentation "Sign certificate request")

(ffi:defshared %x509-req-get-subject-name "X509_REQ_get_subject_name" "libcrypto" :pointer
  (req :pointer)
  :documentation "Get certificate request subject name")

(ffi:defshared %x509-req-get-pubkey "X509_REQ_get_pubkey" "libcrypto" :pointer
  (req :pointer)
  :documentation "Get certificate request public key")

(ffi:defshared %pem-write-bio-x509-req "PEM_write_bio_X509_REQ" "libcrypto" :int
  (bio :pointer) (req :pointer)
  :documentation "Write X509_REQ to BIO in PEM format")

(ffi:defshared %pem-read-bio-x509-req "PEM_read_bio_X509_REQ" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read X509_REQ from BIO in PEM format")

(ffi:defshared %x509-name-add-entry-by-txt "X509_NAME_add_entry_by_txt" "libcrypto" :int
  (name :pointer) (field :string) (type :int) (bytes :string) (len :int) (loc :int) (set :int)
  :documentation "Add entry to X509 name")

(ffi:defshared %x509-set-subject-name "X509_set_subject_name" "libcrypto" :int
  (x509 :pointer) (name :pointer)
  :documentation "Set certificate subject name")

(ffi:defshared %x509-set-issuer-name "X509_set_issuer_name" "libcrypto" :int
  (x509 :pointer) (name :pointer)
  :documentation "Set certificate issuer name")

;; ASN.1 Integer operations
(ffi:defshared %asn1-integer-new "ASN1_INTEGER_new" "libcrypto" :pointer ()
  :documentation "Create new ASN1 integer")

(ffi:defshared %asn1-integer-set "ASN1_INTEGER_set" "libcrypto" :int
  (a :pointer) (v :long)
  :documentation "Set ASN1 integer value")

(ffi:defshared %asn1-integer-get "ASN1_INTEGER_get" "libcrypto" :long
  (a :pointer)
  :documentation "Get ASN1 integer value")

(ffi:defshared %asn1-integer-free "ASN1_INTEGER_free" "libcrypto" :void
  (a :pointer)
  :documentation "Free ASN1 integer")

(ffi:defshared %asn1-time-new "ASN1_TIME_new" "libcrypto" :pointer ()
  :documentation "Create new ASN1 time")

(ffi:defshared %asn1-time-free "ASN1_TIME_free" "libcrypto" :void
  (time :pointer)
  :documentation "Free ASN1 time")

(ffi:defshared %x509-time-adj-ex "X509_time_adj_ex" "libcrypto" :pointer
  (s :pointer) (offset-day :int) (offset-sec :long) (tm :pointer)
  :documentation "Adjust X509 time")

;; Random number generation
(ffi:defshared %rand-bytes "RAND_bytes" "libcrypto" :int
  (buf :pointer) (num :int)
  :documentation "Generate random bytes")

;; Random number status and seeding
(ffi:defshared %rand-status "RAND_status" "libcrypto" :int ()
  :documentation "Check if PRNG is seeded")

(ffi:defshared %rand-seed "RAND_seed" "libcrypto" :void
  (buf :pointer) (num :int)
  :documentation "Seed the PRNG")

;; Security functions
(ffi:defshared %openssl-init-crypto "OPENSSL_init_crypto" "libcrypto" :int
  (opts :unsigned-long) (settings :pointer)
  :documentation "Initialize OpenSSL crypto library")

(ffi:defshared %openssl-cleanse "OPENSSL_cleanse" "libcrypto" :void
  (ptr :pointer) (len :unsigned-long)
  :documentation "Securely clear memory")

(ffi:defshared %crypto-memcmp "CRYPTO_memcmp" "libcrypto" :int
  (a :pointer) (b :pointer) (len :unsigned-long)
  :documentation "Constant-time memory comparison")

(ffi:defshared %openssl-version "OpenSSL_version" "libcrypto" :pointer
  (type :int)
  :documentation "Get OpenSSL version string")

;; Error handling
(ffi:defshared %err-get-error "ERR_get_error" "libcrypto" :unsigned-long ()
  :documentation "Get error code")

;; Might not be available in all versions
;; (ffi:defshared %err-load-crypto-strings "ERR_load_crypto_strings" "libcrypto" :void ()
;;   :documentation "Load crypto error strings")

(ffi:defshared %err-error-string "ERR_error_string" "libcrypto" :pointer
  (e :unsigned-long) (buf :pointer)
  :documentation "Get error string")

;;;; Key Derivation Functions (KDF) FFI Bindings

;; PBKDF2
(ffi:defshared %pkcs5-pbkdf2-hmac "PKCS5_PBKDF2_HMAC" "libcrypto" :int
  (pass :pointer) (passlen :int)
  (salt :pointer) (saltlen :int)
  (iter :int)
  (digest :pointer)
  (keylen :int)
  (out :pointer)
  :documentation "PBKDF2 key derivation function")

;; Scrypt
(ffi:defshared %evp-pbe-scrypt "EVP_PBE_scrypt" "libcrypto" :int
  (pass :pointer) (passlen :unsigned-long)
  (salt :pointer) (saltlen :unsigned-long)
  (n :unsigned-long) (r :unsigned-long) (p :unsigned-long)
  (maxmem :unsigned-long)
  (key :pointer) (keylen :unsigned-long)
  :documentation "Scrypt key derivation function")

;; HKDF constants
(defconstant +evp-pkey-hkdf+ 1036
  "EVP_PKEY type for HKDF")

;; HKDF functions
(ffi:defshared %evp-pkey-ctx-set-hkdf-md "EVP_PKEY_CTX_set_hkdf_md" "libcrypto" :int
  (ctx :pointer) (md :pointer)
  :documentation "Set HKDF message digest")

(ffi:defshared %evp-pkey-ctx-set1-hkdf-salt "EVP_PKEY_CTX_set1_hkdf_salt" "libcrypto" :int
  (ctx :pointer) (salt :pointer) (saltlen :int)
  :documentation "Set HKDF salt")

(ffi:defshared %evp-pkey-ctx-set1-hkdf-key "EVP_PKEY_CTX_set1_hkdf_key" "libcrypto" :int
  (ctx :pointer) (key :pointer) (keylen :int)
  :documentation "Set HKDF key material")

(ffi:defshared %evp-pkey-ctx-add1-hkdf-info "EVP_PKEY_CTX_add1_hkdf_info" "libcrypto" :int
  (ctx :pointer) (info :pointer) (infolen :int)
  :documentation "Add HKDF info")

(ffi:defshared %evp-pkey-derive-init "EVP_PKEY_derive_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize key derivation")

(ffi:defshared %evp-pkey-derive "EVP_PKEY_derive" "libcrypto" :int
  (ctx :pointer) (key :pointer) (keylen :pointer)
  :documentation "Perform key derivation")

;;;; Symmetric Cipher Operations for AEAD

(ffi:defshared %evp-cipher-ctx-new "EVP_CIPHER_CTX_new" "libcrypto" :pointer ()
  :documentation "Create new cipher context")

(ffi:defshared %evp-cipher-ctx-free "EVP_CIPHER_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free cipher context")

(ffi:defshared %evp-get-cipherbyname "EVP_get_cipherbyname" "libcrypto" :pointer
  (name :string)
  :documentation "Get cipher by name")

(ffi:defshared %evp-encryptinit-ex "EVP_EncryptInit_ex" "libcrypto" :int
  (ctx :pointer) (cipher :pointer) (impl :pointer) (key :pointer) (iv :pointer)
  :documentation "Initialize encryption operation")

(ffi:defshared %evp-encryptupdate "EVP_EncryptUpdate" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer) (in :pointer) (inl :int)
  :documentation "Encrypt data")

(ffi:defshared %evp-encryptfinal-ex "EVP_EncryptFinal_ex" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer)
  :documentation "Finalize encryption")

(ffi:defshared %evp-decryptinit-ex "EVP_DecryptInit_ex" "libcrypto" :int
  (ctx :pointer) (cipher :pointer) (impl :pointer) (key :pointer) (iv :pointer)
  :documentation "Initialize decryption operation")

(ffi:defshared %evp-decryptupdate "EVP_DecryptUpdate" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer) (in :pointer) (inl :int)
  :documentation "Decrypt data")

(ffi:defshared %evp-decryptfinal-ex "EVP_DecryptFinal_ex" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer)
  :documentation "Finalize decryption")

(ffi:defshared %evp-cipher-ctx-ctrl "EVP_CIPHER_CTX_ctrl" "libcrypto" :int
  (ctx :pointer) (type :int) (arg :int) (ptr :pointer)
  :documentation "Cipher context control operations")

;;;; X.509 Certificate Functions - duplicates removed

(defconstant +evp-pkey-rsa+ 6)
(defconstant +mbstring-utf8+ #x1000)
(defconstant +bio-ctrl-info+ 3)
