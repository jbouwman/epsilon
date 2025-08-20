;;;; FFI Bindings for OpenSSL (libssl and libcrypto)
;;;;
;;;; This file contains all Foreign Function Interface bindings for both
;;;; SSL/TLS operations and cryptographic functions.

(defpackage :epsilon.crypto.ffi
  (:use :cl)
  (:local-nicknames
   (#:lib #:epsilon.foreign))
  (:export
   ;; Re-export all FFI functions with % prefix
   ;; Modern OpenSSL 3.0 functions
   #:%evp-pkey-ctx-new-from-name
   #:%evp-pkey-ctx-free
   #:%evp-pkey-keygen-init
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
   #:%ssl-ctx-set-cipher-list
   #:%ssl-ctx-use-certificate
   #:%ssl-ctx-use-privatekey
   #:%ssl-ctx-load-verify-locations
   #:%ssl-ctx-set-client-ca-list
   #:%ssl-ctx-use-certificate-chain-file
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
   #:%pem-read-bio-x509
   #:%pem-write-bio-x509
   #:%x509-get-subject-name
   #:%x509-get-issuer-name
   #:%x509-name-oneline
   #:%x509-name-add-entry-by-txt
   #:%x509-set-subject-name
   #:%x509-set-issuer-name
   #:%asn1-integer-new
   #:%asn1-integer-set
   #:%asn1-integer-get
   #:%asn1-integer-free
   #:%x509-time-adj-ex
   #:%x509-req-new
   #:%x509-req-free
   #:%x509-req-set-pubkey
   #:%x509-req-sign
   #:%x509-req-verify
   #:%x509-req-get-subject-name
   #:%x509-req-get-pubkey
   #:%pem-read-bio-x509-req
   #:%pem-write-bio-x509-req
   #:%rand-bytes
   #:%err-get-error
   #:%err-error-string
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
   #:crypto-error-string))

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

;; SSL Context management
(lib:defshared %tls-client-method "TLS_client_method" "libssl" :pointer ()
  :documentation "Get TLS client method")

(lib:defshared %tls-server-method "TLS_server_method" "libssl" :pointer ()
  :documentation "Get TLS server method")

(lib:defshared %ssl-ctx-new "SSL_CTX_new" "libssl" :pointer (method :pointer)
  :documentation "Create new SSL context")

(lib:defshared %ssl-ctx-free "SSL_CTX_free" "libssl" :void (ctx :pointer)
  :documentation "Free SSL context")

(lib:defshared %ssl-ctx-use-certificate-file "SSL_CTX_use_certificate_file" "libssl" :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load certificate file")

(lib:defshared %ssl-ctx-use-privatekey-file "SSL_CTX_use_PrivateKey_file" "libssl" :int
  (ctx :pointer) (file :string) (type :int)
  :documentation "Load private key file")

(lib:defshared %ssl-ctx-check-private-key "SSL_CTX_check_private_key" "libssl" :int
  (ctx :pointer)
  :documentation "Check private key matches certificate")

(lib:defshared %ssl-ctx-set-verify "SSL_CTX_set_verify" "libssl" :void
  (ctx :pointer) (mode :int) (callback :pointer)
  :documentation "Set verification mode")

(lib:defshared %ssl-ctx-set-cipher-list "SSL_CTX_set_cipher_list" "libssl" :int
  (ctx :pointer) (str :string)
  :documentation "Set cipher list")

(lib:defshared %ssl-ctx-use-certificate "SSL_CTX_use_certificate" "libssl" :int
  (ctx :pointer) (x509 :pointer)
  :documentation "Set certificate in SSL context")

(lib:defshared %ssl-ctx-use-privatekey "SSL_CTX_use_PrivateKey" "libssl" :int
  (ctx :pointer) (pkey :pointer)
  :documentation "Set private key in SSL context")

(lib:defshared %ssl-ctx-load-verify-locations "SSL_CTX_load_verify_locations" "libssl" :int
  (ctx :pointer) (ca-file :string) (ca-path :string)
  :documentation "Load CA certificates for verification")

(lib:defshared %ssl-ctx-set-client-ca-list "SSL_CTX_set_client_CA_list" "libssl" :void
  (ctx :pointer) (list :pointer)
  :documentation "Set list of acceptable client CAs")

(lib:defshared %ssl-ctx-use-certificate-chain-file "SSL_CTX_use_certificate_chain_file" "libssl" :int
  (ctx :pointer) (file :string)
  :documentation "Load certificate chain from file")

(lib:defshared %ssl-get-peer-certificate "SSL_get_peer_certificate" "libssl" :pointer
  (ssl :pointer)
  :documentation "Get peer certificate from SSL connection")

(lib:defshared %ssl-get-verify-result "SSL_get_verify_result" "libssl" :long
  (ssl :pointer)
  :documentation "Get verification result")

(lib:defshared %x509-name-stack-new "sk_X509_NAME_new_null" "libcrypto" :pointer
  ()
  :documentation "Create new X509_NAME stack")

(lib:defshared %x509-name-stack-push "sk_X509_NAME_push" "libcrypto" :int
  (stack :pointer) (name :pointer)
  :documentation "Push X509_NAME to stack")

(lib:defshared %ssl-load-client-ca-file "SSL_load_client_CA_file" "libssl" :pointer
  (file :string)
  :documentation "Load client CA list from file")

;; SSL Connection management
(lib:defshared %ssl-new "SSL_new" "libssl" :pointer (ctx :pointer)
  :documentation "Create new SSL connection")

(lib:defshared %ssl-free "SSL_free" "libssl" :void (ssl :pointer)
  :documentation "Free SSL connection")

(lib:defshared %ssl-set-fd "SSL_set_fd" "libssl" :int
  (ssl :pointer) (fd :int)
  :documentation "Set file descriptor for SSL connection")

(lib:defshared %ssl-connect "SSL_connect" "libssl" :int (ssl :pointer)
  :documentation "Initiate SSL handshake as client")

(lib:defshared %ssl-accept "SSL_accept" "libssl" :int (ssl :pointer)
  :documentation "Accept SSL handshake as server")

(lib:defshared %ssl-shutdown "SSL_shutdown" "libssl" :int (ssl :pointer)
  :documentation "Shutdown SSL connection")

;; SSL I/O operations
(lib:defshared %ssl-read "SSL_read" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Read from SSL connection")

(lib:defshared %ssl-write "SSL_write" "libssl" :int
  (ssl :pointer) (buf :pointer) (num :int)
  :documentation "Write to SSL connection")

(lib:defshared %ssl-pending "SSL_pending" "libssl" :int (ssl :pointer)
  :documentation "Get number of bytes pending in SSL buffer")

;; SSL State and Info
(lib:defshared %ssl-get-error "SSL_get_error" "libssl" :int
  (ssl :pointer) (ret :int)
  :documentation "Get SSL error code")

(lib:defshared %ssl-get-version "SSL_get_version" "libssl" :pointer (ssl :pointer)
  :documentation "Get SSL/TLS version string")

(lib:defshared %ssl-get-cipher "SSL_get_cipher" "libssl" :pointer (ssl :pointer)
  :documentation "Get current cipher name")

;;;; Cryptography Library FFI Bindings (libcrypto)

;; Modern OpenSSL 3.0 EVP API for key generation
(lib:defshared %evp-pkey-ctx-new-from-name "EVP_PKEY_CTX_new_from_name" "libcrypto" :pointer
  (libctx :pointer) (name :string) (propquery :pointer)
  :documentation "Create EVP_PKEY_CTX from algorithm name (OpenSSL 3.0)")

(lib:defshared %evp-pkey-ctx-free "EVP_PKEY_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free EVP_PKEY_CTX")

(lib:defshared %evp-pkey-keygen-init "EVP_PKEY_keygen_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize key generation")

(lib:defshared %evp-pkey-generate "EVP_PKEY_generate" "libcrypto" :int
  (ctx :pointer) (ppkey :pointer)
  :documentation "Generate key pair")

(lib:defshared %evp-pkey-ctx-set-rsa-keygen-bits "EVP_PKEY_CTX_set_rsa_keygen_bits" "libcrypto" :int
  (ctx :pointer) (bits :int)
  :documentation "Set RSA key size in bits")

(lib:defshared %evp-pkey-ctx-set-ec-paramgen-curve-nid "EVP_PKEY_CTX_set_ec_paramgen_curve_nid" "libcrypto" :int
  (ctx :pointer) (nid :int)
  :documentation "Set EC curve by NID for key generation")

(lib:defshared %obj-sn2nid "OBJ_sn2nid" "libcrypto" :int
  (sn :string)
  :documentation "Convert short name to NID")

;; EVP (Envelope) API - High-level cryptographic functions
(lib:defshared %evp-pkey-new "EVP_PKEY_new" "libcrypto" :pointer ()
  :documentation "Create new EVP_PKEY structure")

(lib:defshared %evp-pkey-free "EVP_PKEY_free" "libcrypto" :void (pkey :pointer)
  :documentation "Free EVP_PKEY structure")

(lib:defshared %evp-pkey-size "EVP_PKEY_size" "libcrypto" :int (pkey :pointer)
  :documentation "Get key size in bytes")

(lib:defshared %evp-pkey-bits "EVP_PKEY_get_bits" "libcrypto" :int (pkey :pointer)
  :documentation "Get key size in bits")

(lib:defshared %evp-pkey-id "EVP_PKEY_get_id" "libcrypto" :int (pkey :pointer)
  :documentation "Get key type identifier")

;; RSA key generation - DEPRECATED in OpenSSL 3.0
;; (lib:defshared %rsa-new "RSA_new" "libcrypto" :pointer ()
;;   :documentation "Create new RSA structure")
;;
;; (lib:defshared %rsa-free "RSA_free" "libcrypto" :void (rsa :pointer)
;;   :documentation "Free RSA structure")
;;
;; (lib:defshared %rsa-generate-key-ex "RSA_generate_key_ex" "libcrypto" :int
;;   (rsa :pointer) (bits :int) (e :pointer) (cb :pointer)
;;   :documentation "Generate RSA key pair")
;;
;; (lib:defshared %evp-pkey-assign-rsa "EVP_PKEY_assign_RSA" "libcrypto" :int
;;   (pkey :pointer) (rsa :pointer)
;;   :documentation "Assign RSA key to EVP_PKEY")

(lib:defshared %evp-pkey-get1-rsa "EVP_PKEY_get1_RSA" "libcrypto" :pointer
  (pkey :pointer)
  :documentation "Get RSA key from EVP_PKEY")

;; BIGNUM operations for RSA exponent
(lib:defshared %bn-new "BN_new" "libcrypto" :pointer ()
  :documentation "Create new BIGNUM")

(lib:defshared %bn-free "BN_free" "libcrypto" :void (bn :pointer)
  :documentation "Free BIGNUM")

(lib:defshared %bn-set-word "BN_set_word" "libcrypto" :int
  (bn :pointer) (w :unsigned-long)
  :documentation "Set BIGNUM from word")

;; EC key generation - DEPRECATED in OpenSSL 3.0
;; (lib:defshared %ec-key-new-by-curve-name "EC_KEY_new_by_curve_name" "libcrypto" :pointer
;;   (nid :int)
;;   :documentation "Create EC key for named curve")
;;
;; (lib:defshared %ec-key-free "EC_KEY_free" "libcrypto" :void (key :pointer)
;;   :documentation "Free EC key")
;;
;; (lib:defshared %ec-key-generate-key "EC_KEY_generate_key" "libcrypto" :int
;;   (key :pointer)
;;   :documentation "Generate EC key pair")
;;
;; (lib:defshared %evp-pkey-assign-ec-key "EVP_PKEY_assign_EC_KEY" "libcrypto" :int
;;   (pkey :pointer) (ec :pointer)
;;   :documentation "Assign EC key to EVP_PKEY")

(lib:defshared %evp-pkey-get1-ec-key "EVP_PKEY_get1_EC_KEY" "libcrypto" :pointer
  (pkey :pointer)
  :documentation "Get EC key from EVP_PKEY")

(lib:defshared %obj-txt2nid "OBJ_txt2nid" "libcrypto" :int
  (s :string)
  :documentation "Convert text to NID")

;; Ed25519 key generation (EVP only)
(lib:defshared %evp-pkey-ctx-new-id "EVP_PKEY_CTX_new_id" "libcrypto" :pointer
  (id :int) (e :pointer)
  :documentation "Create EVP_PKEY_CTX for algorithm")

;; Removed duplicates - these are defined in the OpenSSL 3.0 section above

(lib:defshared %evp-pkey-ctx-new "EVP_PKEY_CTX_new" "libcrypto" :pointer
  (pkey :pointer) (e :pointer)
  :documentation "Create EVP_PKEY_CTX from key")

;; PEM I/O
(lib:defshared %pem-read-bio-pubkey "PEM_read_bio_PUBKEY" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read public key from BIO in PEM format")

(lib:defshared %pem-read-bio-privatekey "PEM_read_bio_PrivateKey" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read private key from BIO in PEM format")

(lib:defshared %pem-write-bio-pubkey "PEM_write_bio_PUBKEY" "libcrypto" :int
  (bio :pointer) (x :pointer)
  :documentation "Write public key to BIO in PEM format")

(lib:defshared %pem-write-bio-privatekey "PEM_write_bio_PrivateKey" "libcrypto" :int
  (bio :pointer) (x :pointer) (enc :pointer) (kstr :pointer) 
  (klen :int) (cb :pointer) (u :pointer)
  :documentation "Write private key to BIO in PEM format")

;; BIO operations
(lib:defshared %bio-new-mem-buf "BIO_new_mem_buf" "libcrypto" :pointer
  (buf :pointer) (len :int)
  :documentation "Create memory BIO from buffer")

(lib:defshared %bio-new "BIO_new" "libcrypto" :pointer (type :pointer)
  :documentation "Create new BIO")

(lib:defshared %bio-s-mem "BIO_s_mem" "libcrypto" :pointer ()
  :documentation "Get memory BIO method")

(lib:defshared %bio-free "BIO_free" "libcrypto" :int (bio :pointer)
  :documentation "Free BIO")

(lib:defshared %bio-read "BIO_read" "libcrypto" :int
  (bio :pointer) (data :pointer) (dlen :int)
  :documentation "Read from BIO")

(lib:defshared %bio-write "BIO_write" "libcrypto" :int
  (bio :pointer) (data :pointer) (dlen :int)
  :documentation "Write to BIO")

(lib:defshared %bio-ctrl "BIO_ctrl" "libcrypto" :long
  (bio :pointer) (cmd :int) (larg :long) (parg :pointer)
  :documentation "BIO control operation")

;; Digest operations
(lib:defshared %evp-md-ctx-new "EVP_MD_CTX_new" "libcrypto" :pointer ()
  :documentation "Create new digest context")

(lib:defshared %evp-md-ctx-free "EVP_MD_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free digest context")

(lib:defshared %evp-get-digestbyname "EVP_get_digestbyname" "libcrypto" :pointer
  (name :string)
  :documentation "Get digest algorithm by name")

(lib:defshared %evp-digestinit-ex "EVP_DigestInit_ex" "libcrypto" :int
  (ctx :pointer) (type :pointer) (impl :pointer)
  :documentation "Initialize digest context")

(lib:defshared %evp-digestupdate "EVP_DigestUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update digest with data")

(lib:defshared %evp-digestfinal-ex "EVP_DigestFinal_ex" "libcrypto" :int
  (ctx :pointer) (md :pointer) (s :pointer)
  :documentation "Finalize digest")

;; Signing operations
(lib:defshared %evp-digestsigninit "EVP_DigestSignInit" "libcrypto" :int
  (ctx :pointer) (pctx :pointer) (type :pointer) (e :pointer) (pkey :pointer)
  :documentation "Initialize signing operation")

(lib:defshared %evp-digestsignupdate "EVP_DigestSignUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update data to be signed")

(lib:defshared %evp-digestsignfinal "EVP_DigestSignFinal" "libcrypto" :int
  (ctx :pointer) (sig :pointer) (siglen :pointer)
  :documentation "Finalize signing operation")

;; Verification operations
(lib:defshared %evp-digestverifyinit "EVP_DigestVerifyInit" "libcrypto" :int
  (ctx :pointer) (pctx :pointer) (type :pointer) (e :pointer) (pkey :pointer)
  :documentation "Initialize verification operation")

(lib:defshared %evp-digestverifyupdate "EVP_DigestVerifyUpdate" "libcrypto" :int
  (ctx :pointer) (d :pointer) (cnt :unsigned-long)
  :documentation "Update data to be verified")

(lib:defshared %evp-digestverifyfinal "EVP_DigestVerifyFinal" "libcrypto" :int
  (ctx :pointer) (sig :pointer) (siglen :unsigned-long)
  :documentation "Finalize verification operation")

;; Public key encryption
(lib:defshared %evp-pkey-encrypt-init "EVP_PKEY_encrypt_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize public key encryption")

(lib:defshared %evp-pkey-encrypt "EVP_PKEY_encrypt" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outlen :pointer) (in :pointer) (inlen :unsigned-long)
  :documentation "Encrypt with public key")

(lib:defshared %evp-pkey-decrypt-init "EVP_PKEY_decrypt_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize public key decryption")

(lib:defshared %evp-pkey-decrypt "EVP_PKEY_decrypt" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outlen :pointer) (in :pointer) (inlen :unsigned-long)
  :documentation "Decrypt with private key")

;; X.509 Certificate operations
(lib:defshared %x509-new "X509_new" "libcrypto" :pointer ()
  :documentation "Create new X509 certificate")

(lib:defshared %x509-free "X509_free" "libcrypto" :void (x509 :pointer)
  :documentation "Free X509 certificate")

(lib:defshared %x509-set-version "X509_set_version" "libcrypto" :int
  (x509 :pointer) (version :long)
  :documentation "Set certificate version")

(lib:defshared %x509-set-serialnumber "X509_set_serialNumber" "libcrypto" :int
  (x509 :pointer) (serial :pointer)
  :documentation "Set certificate serial number")

(lib:defshared %x509-get-serialnumber "X509_get_serialNumber" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate serial number")

(lib:defshared %x509-set-notbefore "X509_set1_notBefore" "libcrypto" :int
  (x509 :pointer) (tm :pointer)
  :documentation "Set certificate not-before time")

(lib:defshared %x509-set-notafter "X509_set1_notAfter" "libcrypto" :int
  (x509 :pointer) (tm :pointer)
  :documentation "Set certificate not-after time")

(lib:defshared %x509-set-pubkey "X509_set_pubkey" "libcrypto" :int
  (x509 :pointer) (pkey :pointer)
  :documentation "Set certificate public key")

(lib:defshared %x509-get-pubkey "X509_get_pubkey" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate public key")

(lib:defshared %x509-sign "X509_sign" "libcrypto" :int
  (x509 :pointer) (pkey :pointer) (md :pointer)
  :documentation "Sign certificate")

(lib:defshared %x509-verify "X509_verify" "libcrypto" :int
  (x509 :pointer) (pkey :pointer)
  :documentation "Verify certificate signature")

(lib:defshared %pem-read-bio-x509 "PEM_read_bio_X509" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read X509 certificate from BIO in PEM format")

(lib:defshared %pem-write-bio-x509 "PEM_write_bio_X509" "libcrypto" :int
  (bio :pointer) (x :pointer)
  :documentation "Write X509 certificate to BIO in PEM format")

(lib:defshared %x509-get-subject-name "X509_get_subject_name" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate subject name")

(lib:defshared %x509-get-issuer-name "X509_get_issuer_name" "libcrypto" :pointer
  (x509 :pointer)
  :documentation "Get certificate issuer name")

(lib:defshared %x509-name-oneline "X509_NAME_oneline" "libcrypto" :pointer
  (name :pointer) (buf :pointer) (size :int)
  :documentation "Convert X509 name to string")

(lib:defshared %x509-name-add-entry-by-txt "X509_NAME_add_entry_by_txt" "libcrypto" :int
  (name :pointer) (field :string) (type :int) (bytes :string) (len :int) (loc :int) (set :int)
  :documentation "Add entry to X509 name")

(lib:defshared %x509-set-subject-name "X509_set_subject_name" "libcrypto" :int
  (x509 :pointer) (name :pointer)
  :documentation "Set certificate subject name")

(lib:defshared %x509-set-issuer-name "X509_set_issuer_name" "libcrypto" :int
  (x509 :pointer) (name :pointer)
  :documentation "Set certificate issuer name")

;; ASN.1 Integer operations
(lib:defshared %asn1-integer-new "ASN1_INTEGER_new" "libcrypto" :pointer ()
  :documentation "Create new ASN1 integer")

(lib:defshared %asn1-integer-set "ASN1_INTEGER_set" "libcrypto" :int
  (a :pointer) (v :long)
  :documentation "Set ASN1 integer value")

(lib:defshared %asn1-integer-get "ASN1_INTEGER_get" "libcrypto" :long
  (a :pointer)
  :documentation "Get ASN1 integer value")

(lib:defshared %asn1-integer-free "ASN1_INTEGER_free" "libcrypto" :void
  (a :pointer)
  :documentation "Free ASN1 integer")

(lib:defshared %x509-time-adj-ex "X509_time_adj_ex" "libcrypto" :pointer
  (s :pointer) (offset-day :int) (offset-sec :long) (tm :pointer)
  :documentation "Adjust X509 time")

;; Certificate Signing Request (CSR) operations
(lib:defshared %x509-req-new "X509_REQ_new" "libcrypto" :pointer ()
  :documentation "Create new CSR")

(lib:defshared %x509-req-free "X509_REQ_free" "libcrypto" :void (req :pointer)
  :documentation "Free CSR")

(lib:defshared %x509-req-set-pubkey "X509_REQ_set_pubkey" "libcrypto" :int
  (req :pointer) (pkey :pointer)
  :documentation "Set CSR public key")

(lib:defshared %x509-req-sign "X509_REQ_sign" "libcrypto" :int
  (req :pointer) (pkey :pointer) (md :pointer)
  :documentation "Sign CSR")

(lib:defshared %x509-req-verify "X509_REQ_verify" "libcrypto" :int
  (req :pointer) (pkey :pointer)
  :documentation "Verify CSR signature")

(lib:defshared %x509-req-get-subject-name "X509_REQ_get_subject_name" "libcrypto" :pointer
  (req :pointer)
  :documentation "Get CSR subject name")

(lib:defshared %x509-req-get-pubkey "X509_REQ_get_pubkey" "libcrypto" :pointer
  (req :pointer)
  :documentation "Get public key from CSR")

(lib:defshared %pem-read-bio-x509-req "PEM_read_bio_X509_REQ" "libcrypto" :pointer
  (bio :pointer) (x :pointer) (cb :pointer) (u :pointer)
  :documentation "Read CSR from BIO in PEM format")

(lib:defshared %pem-write-bio-x509-req "PEM_write_bio_X509_REQ" "libcrypto" :int
  (bio :pointer) (x :pointer)
  :documentation "Write CSR to BIO in PEM format")

;; Random number generation
(lib:defshared %rand-bytes "RAND_bytes" "libcrypto" :int
  (buf :pointer) (num :int)
  :documentation "Generate random bytes")

;; Error handling
(lib:defshared %err-get-error "ERR_get_error" "libcrypto" :unsigned-long ()
  :documentation "Get error code")

(lib:defshared %err-error-string "ERR_error_string" "libcrypto" :pointer
  (e :unsigned-long) (buf :pointer)
  :documentation "Get error string")

;;;; Key Derivation Functions (KDF) FFI Bindings

;; PBKDF2
(lib:defshared %pkcs5-pbkdf2-hmac "PKCS5_PBKDF2_HMAC" "libcrypto" :int
  (pass :pointer) (passlen :int)
  (salt :pointer) (saltlen :int)
  (iter :int)
  (digest :pointer)
  (keylen :int)
  (out :pointer)
  :documentation "PBKDF2 key derivation function")

;; Scrypt
(lib:defshared %evp-pbe-scrypt "EVP_PBE_scrypt" "libcrypto" :int
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
(lib:defshared %evp-pkey-ctx-set-hkdf-md "EVP_PKEY_CTX_set_hkdf_md" "libcrypto" :int
  (ctx :pointer) (md :pointer)
  :documentation "Set HKDF message digest")

(lib:defshared %evp-pkey-ctx-set1-hkdf-salt "EVP_PKEY_CTX_set1_hkdf_salt" "libcrypto" :int
  (ctx :pointer) (salt :pointer) (saltlen :int)
  :documentation "Set HKDF salt")

(lib:defshared %evp-pkey-ctx-set1-hkdf-key "EVP_PKEY_CTX_set1_hkdf_key" "libcrypto" :int
  (ctx :pointer) (key :pointer) (keylen :int)
  :documentation "Set HKDF key material")

(lib:defshared %evp-pkey-ctx-add1-hkdf-info "EVP_PKEY_CTX_add1_hkdf_info" "libcrypto" :int
  (ctx :pointer) (info :pointer) (infolen :int)
  :documentation "Add HKDF info")

(lib:defshared %evp-pkey-derive-init "EVP_PKEY_derive_init" "libcrypto" :int
  (ctx :pointer)
  :documentation "Initialize key derivation")

(lib:defshared %evp-pkey-derive "EVP_PKEY_derive" "libcrypto" :int
  (ctx :pointer) (key :pointer) (keylen :pointer)
  :documentation "Perform key derivation")

;;;; Symmetric Cipher Operations for AEAD

(lib:defshared %evp-cipher-ctx-new "EVP_CIPHER_CTX_new" "libcrypto" :pointer ()
  :documentation "Create new cipher context")

(lib:defshared %evp-cipher-ctx-free "EVP_CIPHER_CTX_free" "libcrypto" :void
  (ctx :pointer)
  :documentation "Free cipher context")

(lib:defshared %evp-get-cipherbyname "EVP_get_cipherbyname" "libcrypto" :pointer
  (name :string)
  :documentation "Get cipher by name")

(lib:defshared %evp-encryptinit-ex "EVP_EncryptInit_ex" "libcrypto" :int
  (ctx :pointer) (cipher :pointer) (impl :pointer) (key :pointer) (iv :pointer)
  :documentation "Initialize encryption operation")

(lib:defshared %evp-encryptupdate "EVP_EncryptUpdate" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer) (in :pointer) (inl :int)
  :documentation "Encrypt data")

(lib:defshared %evp-encryptfinal-ex "EVP_EncryptFinal_ex" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer)
  :documentation "Finalize encryption")

(lib:defshared %evp-decryptinit-ex "EVP_DecryptInit_ex" "libcrypto" :int
  (ctx :pointer) (cipher :pointer) (impl :pointer) (key :pointer) (iv :pointer)
  :documentation "Initialize decryption operation")

(lib:defshared %evp-decryptupdate "EVP_DecryptUpdate" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer) (in :pointer) (inl :int)
  :documentation "Decrypt data")

(lib:defshared %evp-decryptfinal-ex "EVP_DecryptFinal_ex" "libcrypto" :int
  (ctx :pointer) (out :pointer) (outl :pointer)
  :documentation "Finalize decryption")

(lib:defshared %evp-cipher-ctx-ctrl "EVP_CIPHER_CTX_ctrl" "libcrypto" :int
  (ctx :pointer) (type :int) (arg :int) (ptr :pointer)
  :documentation "Cipher context control operations")