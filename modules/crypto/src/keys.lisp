;;;; Cryptographic Operations Implementation
;;;;
;;;; This file provides public key cryptography operations using OpenSSL's libcrypto

(defpackage :epsilon.crypto.keys
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi)
   (#:utils #:epsilon.crypto.utils))
  (:import-from :epsilon.crypto
		;; Import all needed types and constants
		#:crypto-key
		#:crypto-key-p
		#:crypto-key-handle
		#:crypto-key-type
		#:crypto-key-bits
		#:crypto-key-public-p
		#:crypto-key-private-p
		#:make-crypto-key
		#:crypto-error
		#:+evp-pkey-rsa+
		#:+evp-pkey-ec+
		#:+evp-pkey-ed25519+
		#:+rsa-f4+
		#:+digest-sha256+
		#:+digest-sha384+
		#:+digest-sha512+
		#:+digest-sha3-256+
		#:+digest-sha3-512+))

(in-package :epsilon.crypto.keys)

;;;; Key Generation Functions

(defun generate-rsa-key (&key (bits 2048))
  "Generate an RSA key pair with specified bit size.
   
   Creates a new RSA key pair suitable for digital signatures, encryption,
   and key exchange. The generated key contains both public and private
   components and uses secure random number generation.
   
   Parameters:
     bits (integer): RSA modulus size in bits. Must be 2048, 3072, or 4096.
                    Defaults to 2048 bits.
   
   Returns:
     CRYPTO-KEY structure containing the generated RSA key pair.
   
   Security Notes:
     - 2048-bit keys provide ~112 bits of security (acceptable until ~2030)
     - 3072-bit keys provide ~128 bits of security (recommended for new systems)
     - 4096-bit keys provide ~152 bits of security (future-proof but slower)
     - Key generation uses cryptographically secure random number generation
     - Private key material should be protected and freed promptly after use
   
   Performance:
     - 2048-bit: ~50-200ms generation time
     - 3072-bit: ~200-800ms generation time  
     - 4096-bit: ~500-2000ms generation time
   
   Errors:
     Signals CRYPTO-ERROR if key generation fails or invalid bit size specified."
  ;; Don't declare type here so we can handle invalid sizes gracefully
  (unless (member bits '(2048 3072 4096))
    (error 'crypto-error :code -1 
           :message (format nil "RSA key size must be 2048, 3072, or 4096 bits, got ~A" bits)))
  
  ;; Use modern EVP_PKEY_CTX_new_from_name API (OpenSSL 3.0)
  (let* ((ctx (ffi:%evp-pkey-ctx-new-from-name 
               (sb-sys:int-sap 0) "RSA" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    (when (zerop (sb-sys:sap-int ctx))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create RSA context"))
    
    (unwind-protect
        (progn
          ;; Initialize key generation
          (when (zerop (ffi:%evp-pkey-keygen-init ctx))
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "Failed to initialize RSA key generation"))
          
          ;; Set RSA key size
          (when (zerop (ffi:%evp-pkey-ctx-set-rsa-keygen-bits ctx bits))
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "Failed to set RSA key size"))
          
          ;; Generate the key
          (let ((pkey-holder (sb-alien:make-alien sb-sys:system-area-pointer)))
		(unwind-protect
		     (progn
		       (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
		       (let ((result (ffi:%evp-pkey-generate ctx 
							     (sb-alien:alien-sap pkey-holder))))
			 (when (zerop result)
			   (error 'crypto-error :code (ffi:%err-get-error)
				  :message "Failed to generate RSA key"))
			 (setf pkey-ptr (sb-alien:deref pkey-holder))))
		  (sb-alien:free-alien pkey-holder)))
          
          ;; Create key structure
          (make-crypto-key :handle pkey-ptr
                           :type :rsa
                           :bits bits
                           :public-p t
                           :private-p t))
      ;; Cleanup
      (when ctx
        (ffi:%evp-pkey-ctx-free ctx)))))

(defun generate-ec-key (&key (curve :p256))
  "Generate an Elliptic Curve key pair for the specified curve.
   
   Creates a new EC key pair suitable for digital signatures (ECDSA) and
   key exchange (ECDH). EC keys provide equivalent security to RSA with
   significantly smaller key sizes and faster operations.
   
   Parameters:
     curve (keyword): Elliptic curve to use. Defaults to :P256.
                     Supported curves:
                     - :P256 (prime256v1) - 256-bit, ~128-bit security
                     - :P384 (secp384r1) - 384-bit, ~192-bit security  
                     - :P521 (secp521r1) - 521-bit, ~256-bit security
                     - :SECP256K1 - Bitcoin curve, 256-bit
   
   Returns:
     CRYPTO-KEY structure containing the generated EC key pair.
   
   Security Notes:
     - P-256 provides ~128 bits of security (recommended for most uses)
     - P-384 provides ~192 bits of security (high security applications)
     - P-521 provides ~256 bits of security (maximum security)
     - SECP256K1 used by Bitcoin/Ethereum (specialized applications only)
     - All curves use cryptographically secure random number generation
     - EC keys are vulnerable to quantum attacks (use post-quantum crypto for long-term)
   
   Performance:
     - P-256: ~1-5ms generation, fast signing/verification
     - P-384: ~2-8ms generation, moderate speed
     - P-521: ~5-15ms generation, slower operations
     - SECP256K1: Similar to P-256
   
   Standards Compliance:
     - P-256/P-384/P-521: NIST standard curves (FIPS 186-4)
     - SECP256K1: SEC standard curve (SEC 2)
   
   Errors:
     Signals CRYPTO-ERROR if key generation fails or unsupported curve specified."
  ;; Don't declare type here so we can handle invalid curves gracefully
  (let* ((curve-name (case curve
			   (:p256 "prime256v1")
			   (:p384 "secp384r1")
			   (:p521 "secp521r1")
			   (:secp256k1 "secp256k1")
			   (t (error 'crypto-error :code -1 
                                          :message (format nil "Unsupported EC curve: ~A" curve)))))
         ;; Get NID for the curve
         (nid (ffi:%obj-sn2nid curve-name))
         ;; Use modern EVP_PKEY_CTX_new_from_name API (OpenSSL 3.0)
         (ctx (ffi:%evp-pkey-ctx-new-from-name 
               (sb-sys:int-sap 0) "EC" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    
    (when (zerop nid)
      (error 'crypto-error :code -1
             :message (format nil "Unknown EC curve: ~A" curve-name)))
    
    (when (zerop (sb-sys:sap-int ctx))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create EC context"))
    
    (unwind-protect
        (progn
          ;; Initialize key generation
          (when (zerop (ffi:%evp-pkey-keygen-init ctx))
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "Failed to initialize EC key generation"))
          
          ;; Set curve parameter using NID
          (when (zerop (ffi:%evp-pkey-ctx-set-ec-paramgen-curve-nid ctx nid))
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message (format nil "Failed to set EC curve: ~A" curve-name)))
          
          ;; Generate the key
          (let ((pkey-holder (sb-alien:make-alien sb-sys:system-area-pointer)))
		(unwind-protect
		     (progn
		       (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
		       (let ((result (ffi:%evp-pkey-generate ctx 
							     (sb-alien:alien-sap pkey-holder))))
			 (when (zerop result)
			   (error 'crypto-error :code (ffi:%err-get-error)
				  :message "Failed to generate EC key"))
			 (setf pkey-ptr (sb-alien:deref pkey-holder))))
		  (sb-alien:free-alien pkey-holder)))
          
          ;; Create key structure
          (make-crypto-key :handle pkey-ptr
                           :type :ec
                           :bits (ffi:%evp-pkey-bits pkey-ptr)
                           :public-p t
                           :private-p t))
      ;; Cleanup
      (when ctx
        (ffi:%evp-pkey-ctx-free ctx)))))

(defun generate-ed25519-key ()
  "Generate an Ed25519 key pair for digital signatures.
   
   Creates a new Ed25519 key pair optimized for digital signatures.
   Ed25519 is a modern elliptic curve signature scheme offering
   high security, fast operations, and deterministic signatures.
   
   Returns:
     CRYPTO-KEY structure containing the generated Ed25519 key pair.
   
   Security Notes:
     - Provides ~128 bits of security (equivalent to 3072-bit RSA)
     - Uses Edwards25519 curve (RFC 7748) with twisted Edwards coordinates
     - Deterministic signatures (same message + key = same signature)
     - Built-in protection against side-channel attacks
     - Resistant to many implementation pitfalls of other signature schemes
   
   Performance:
     - Very fast key generation (~0.1-1ms)
     - Extremely fast signing (~10-50μs)
     - Fast verification (~20-100μs)  
     - Small signatures (64 bytes) and public keys (32 bytes)
   
   Use Cases:
     - Digital signatures (recommended for new applications)
     - Authentication protocols (SSH, TLS 1.3)
     - Cryptocurrency and blockchain applications
     - IoT and embedded systems (low resource usage)
   
   Standards Compliance:
     - RFC 8032 (EdDSA signature algorithms)
     - RFC 7748 (Elliptic curves for security)
     - Widely supported in modern cryptographic libraries
   
   Limitations:
     - Signature-only algorithm (cannot be used for encryption)
     - No key exchange capability (use X25519 for ECDH)
     - Fixed parameters (no configuration options)
   
   Errors:
     Signals CRYPTO-ERROR if key generation fails."
  ;; Use modern EVP_PKEY_CTX_new_from_name API (OpenSSL 3.0)
  (let* ((ctx (ffi:%evp-pkey-ctx-new-from-name 
               (sb-sys:int-sap 0) "ED25519" (sb-sys:int-sap 0)))
         (pkey-ptr nil))
    (when (zerop (sb-sys:sap-int ctx))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create Ed25519 context"))
    
    (unwind-protect
        (progn
          ;; Initialize key generation
          (when (zerop (ffi:%evp-pkey-keygen-init ctx))
            (error 'crypto-error :code (ffi:%err-get-error)
                   :message "Failed to initialize Ed25519 key generation"))
          
          ;; Ed25519 doesn't need parameters
          
          ;; Generate the key
          (let ((pkey-holder (sb-alien:make-alien sb-sys:system-area-pointer)))
		(unwind-protect
		     (progn
		       (setf (sb-alien:deref pkey-holder) (sb-sys:int-sap 0))
		       (let ((result (ffi:%evp-pkey-generate ctx 
							     (sb-alien:alien-sap pkey-holder))))
			 (when (zerop result)
			   (error 'crypto-error :code (ffi:%err-get-error)
				  :message "Failed to generate Ed25519 key"))
			 (setf pkey-ptr (sb-alien:deref pkey-holder))))
		  (sb-alien:free-alien pkey-holder)))
          
          ;; Create key structure
          (make-crypto-key :handle pkey-ptr
                           :type :ed25519
                           :bits 256
                           :public-p t
                           :private-p t))
      ;; Cleanup
      (when ctx
        (ffi:%evp-pkey-ctx-free ctx)))))

;;;; Key I/O Functions

(defun key-to-pem (key &key private-p)
  "Export cryptographic key to PEM format string.
   
   Converts a CRYPTO-KEY structure to PEM (Privacy-Enhanced Mail) format,
   which is a standard text-based encoding for cryptographic keys and
   certificates widely supported by cryptographic software.
   
   Parameters:
     key (crypto-key): Key to export
     private-p (boolean): If T, export private key; if NIL, export public key only
   
   Returns:
     String containing PEM-formatted key
   
   PEM Format:
     - Base64-encoded DER with BEGIN/END markers
     - Public keys: '-----BEGIN PUBLIC KEY-----'  
     - Private keys: '-----BEGIN PRIVATE KEY-----'
     - Line breaks every 64 characters
     - Human-readable and widely compatible
   
   Security Notes:
     - Private keys contain sensitive material - handle carefully
     - PEM files should have restricted permissions (0600 for private keys)
     - Consider encrypting PEM private keys for storage
     - Public keys are safe to share freely
   
   Example Output:
     -----BEGIN PUBLIC KEY-----
     MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA...
     -----END PUBLIC KEY-----
   
   Errors:
     Signals error if key is invalid or doesn't contain requested key material."
  (declare (type crypto-key key)
           (type boolean private-p))
  (unless (crypto-key-p key)
    (error 'crypto-error :code -1 :message "Invalid key object"))
  
  (when (and private-p (not (crypto-key-private-p key)))
    (error 'crypto-error :code -1 :message "Key does not contain private key material"))
  
  (let* ((bio (ffi:%bio-new (ffi:%bio-s-mem)))
         (result nil))
    (unwind-protect
        (progn
          ;; Write key to BIO
          (if private-p
              (when (zerop (ffi:%pem-write-bio-privatekey bio (crypto-key-handle key)
							  (sb-sys:int-sap 0) (sb-sys:int-sap 0)
							  0 (sb-sys:int-sap 0) (sb-sys:int-sap 0)))
                (error 'crypto-error :code (ffi:%err-get-error)
                       :message "Failed to write private key"))
            (when (zerop (ffi:%pem-write-bio-pubkey bio (crypto-key-handle key)))
              (error 'crypto-error :code (ffi:%err-get-error)
                     :message "Failed to write public key")))
          
          ;; Get PEM string from BIO
          (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0)))) ; BIO_CTRL_PENDING = 3
            (when (plusp len)
              (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char #.(expt 2 16))))
				   (let ((bytes-read (ffi:%bio-read bio (sb-alien:alien-sap buf) len)))
				     (when (plusp bytes-read)
				       (setf result (make-string bytes-read))
				       (loop for i from 0 below bytes-read
					     do (setf (char result i) 
						      (code-char (sb-alien:deref buf i))))))))))
      ;; Cleanup
      (ffi:%bio-free bio))
    result))

(defun key-from-pem (pem-string &key private-p)
  "Import key from PEM format string"
  (let* ((len (length pem-string))
	 (pem-bytes (make-array len :element-type '(unsigned-byte 8))))
    ;; Convert string to bytes
    (loop for i from 0 below len
	  do (setf (aref pem-bytes i) (char-code (char pem-string i))))
    (sb-alien:with-alien ((pem-buf (sb-alien:array sb-alien:unsigned-char #.(* 64 1024))))
			 ;; Copy to alien buffer
			 (loop for i from 0 below (min len #.(* 64 1024))
			       do (setf (sb-alien:deref pem-buf i) (aref pem-bytes i)))
			 (let* ((bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-buf) len))
			      (pkey nil))
			 (unwind-protect
			     (progn
			       ;; Read key from BIO
			       (setf pkey
				     (if private-p
					 (ffi:%pem-read-bio-privatekey bio (sb-sys:int-sap 0) 
								       (sb-sys:int-sap 0) (sb-sys:int-sap 0))
				       (ffi:%pem-read-bio-pubkey bio (sb-sys:int-sap 0)
								 (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
			       
			       (when (sb-sys:sap= pkey (sb-sys:int-sap 0))
				 (error 'crypto-error :code (ffi:%err-get-error)
					:message "Failed to read key from PEM"))
			       
			       ;; Determine key type
			       (let ((key-id (ffi:%evp-pkey-id pkey)))
				 (make-crypto-key 
				  :handle pkey
				  :type (cond ((= key-id +evp-pkey-rsa+) :rsa)
					      ((= key-id +evp-pkey-ec+) :ec)
					      ((= key-id +evp-pkey-ed25519+) :ed25519)
					      (t :unknown))
				  :bits (ffi:%evp-pkey-bits pkey)
				  :public-p t
				  :private-p private-p)))
			   ;; Cleanup on error
			   (progn
			     (ffi:%bio-free bio)
			     (when (and pkey (sb-sys:sap= pkey (sb-sys:int-sap 0)))
			       nil)))))))

;;;; Signing and Verification

(defun sign-message (key data &key (digest-algo nil))
  "Sign data with private key using specified digest algorithm"
  (unless (crypto-key-private-p key)
    (error "Private key required for signing"))
  
  (let* ((md-ctx (ffi:%evp-md-ctx-new))
         ;; Ed25519 uses NULL digest, others default to SHA256
         (md (if (eq (crypto-key-type key) :ed25519)
                 (sb-sys:int-sap 0)
                 (ffi:%evp-get-digestbyname (or digest-algo +digest-sha256+))))
         ;; Convert string to UTF-8 bytes if needed
         (data-bytes (if (stringp data)
                         (sb-ext:string-to-octets data :external-format :utf-8)
                         data))
         (signature nil))
    
    (when (and (not (eq (crypto-key-type key) :ed25519))
               (sb-sys:sap= md (sb-sys:int-sap 0)))
      (error 'crypto-error :code -1 :message (format nil "Unknown digest: ~A" digest-algo)))
    
    (unwind-protect
        (sb-alien:with-alien ((data-buf (sb-alien:array sb-alien:unsigned-char #.(expt 2 16)))
                              (sig-buf (sb-alien:array sb-alien:unsigned-char 4096))
                              (sig-len sb-alien:size-t :local 4096))
			     ;; Copy data bytes to alien buffer
			     (let ((data-len (min (length data-bytes) #.(expt 2 16))))
			       (loop for i from 0 below data-len
				     do (setf (sb-alien:deref data-buf i) (aref data-bytes i)))
			       
			       ;; Initialize signing
			       (when (zerop (ffi:%evp-digestsigninit md-ctx (sb-sys:int-sap 0) md 
								     (sb-sys:int-sap 0) (crypto-key-handle key)))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to init signing"))
			       
			       ;; Update with data
			       (when (zerop (ffi:%evp-digestsignupdate md-ctx (sb-alien:alien-sap data-buf) data-len))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to update signing")))
			     
			     ;; Finalize signature
			     (when (zerop (ffi:%evp-digestsignfinal md-ctx (sb-alien:alien-sap sig-buf) 
								    (sb-alien:alien-sap (sb-alien:addr sig-len))))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to finalize signing"))
			     
			     ;; Convert signature to byte vector
			     (let ((sig-bytes (make-array sig-len :element-type '(unsigned-byte 8))))
			       (loop for i from 0 below sig-len
				     do (setf (aref sig-bytes i) (sb-alien:deref sig-buf i)))
			       (setf signature sig-bytes)))
      ;; Cleanup
      (ffi:%evp-md-ctx-free md-ctx))
    signature))

(defun verify-message (key data signature &key (digest-algo nil))
  "Verify signature of data with public key"
  (unless (crypto-key-public-p key)
    (error "Public key required for verification"))
  
  (let* ((md-ctx (ffi:%evp-md-ctx-new))
         ;; Ed25519 uses NULL digest, others default to SHA256
         (md (if (eq (crypto-key-type key) :ed25519)
                 (sb-sys:int-sap 0)
                 (ffi:%evp-get-digestbyname (or digest-algo +digest-sha256+))))
         ;; Convert string to UTF-8 bytes if needed
         (data-bytes (if (stringp data)
                         (sb-ext:string-to-octets data :external-format :utf-8)
                         data))
         (valid nil))
    
    (when (and (not (eq (crypto-key-type key) :ed25519))
               (sb-sys:sap= md (sb-sys:int-sap 0)))
      (error 'crypto-error :code -1 :message (format nil "Unknown digest: ~A" digest-algo)))
    
    (unwind-protect
        (sb-alien:with-alien ((data-buf (sb-alien:array sb-alien:unsigned-char #.(expt 2 16)))
                              (sig-buf (sb-alien:array sb-alien:unsigned-char 4096)))
			     ;; Copy data bytes to alien buffer
			     (let ((data-len (min (length data-bytes) #.(expt 2 16))))
			       (loop for i from 0 below data-len
				     do (setf (sb-alien:deref data-buf i) (aref data-bytes i)))
			       
			       ;; Copy signature to alien buffer
			     (loop for i from 0 below (length signature)
				   do (setf (sb-alien:deref sig-buf i) (aref signature i)))
			     
			     ;; Initialize verification
			     (when (zerop (ffi:%evp-digestverifyinit md-ctx (sb-sys:int-sap 0) md
								     (sb-sys:int-sap 0) (crypto-key-handle key)))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to init verification"))
			     
			       ;; Update with data
			       (when (zerop (ffi:%evp-digestverifyupdate md-ctx (sb-alien:alien-sap data-buf) data-len))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to update verification"))
			     
			       ;; Verify signature
			       (setf valid (= 1 (ffi:%evp-digestverifyfinal md-ctx (sb-alien:alien-sap sig-buf) 
									    (length signature))))))
      ;; Cleanup
      (ffi:%evp-md-ctx-free md-ctx))
    valid))

;; Aliases for compatibility
(defun sign (key data &rest args)
  (apply #'sign-message key data args))

(defun verify (key data signature &rest args)
  (apply #'verify-message key data signature args))

;;;; Public Key Encryption

(defun encrypt (key data)
  "Encrypt data with public key (RSA only)"
  (unless (eq (crypto-key-type key) :rsa)
    (error "Only RSA keys support direct encryption"))
  
  (unless (crypto-key-public-p key)
    (error "Public key required for encryption"))
  
  (let* ((ctx (ffi:%evp-pkey-ctx-new (crypto-key-handle key) (sb-sys:int-sap 0)))
         ;; Convert string to UTF-8 bytes if needed
         (data-bytes (if (stringp data)
                         (sb-ext:string-to-octets data :external-format :utf-8)
                         data))
         (encrypted nil))
    
    (unwind-protect
        (sb-alien:with-alien ((in-buf (sb-alien:array sb-alien:unsigned-char #.(expt 2 16)))
                              (out-buf (sb-alien:array sb-alien:unsigned-char 4096))
                              (out-len sb-alien:size-t :local 4096))
			     ;; Copy data bytes to alien buffer
			     (let ((data-len (min (length data-bytes) #.(expt 2 16))))
			       (loop for i from 0 below data-len
				     do (setf (sb-alien:deref in-buf i) (aref data-bytes i)))
			       
			       ;; Initialize encryption
			       (when (zerop (ffi:%evp-pkey-encrypt-init ctx))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to init encryption"))
			       
			       ;; Encrypt data
			       (when (zerop (ffi:%evp-pkey-encrypt ctx (sb-alien:alien-sap out-buf) 
								   (sb-alien:alien-sap (sb-alien:addr out-len))
								   (sb-alien:alien-sap in-buf) data-len))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to encrypt")))
			     
			     ;; Convert to byte vector
			     (let ((enc-bytes (make-array out-len :element-type '(unsigned-byte 8))))
			       (loop for i from 0 below out-len
				     do (setf (aref enc-bytes i) (sb-alien:deref out-buf i)))
			       (setf encrypted enc-bytes)))
      ;; Cleanup
      (ffi:%evp-pkey-ctx-free ctx))
    encrypted))

(defun decrypt (key encrypted-data)
  "Decrypt data with private key (RSA only)"
  (unless (eq (crypto-key-type key) :rsa)
    (error "Only RSA keys support direct decryption"))
  
  (unless (crypto-key-private-p key)
    (error "Private key required for decryption"))
  
  (let* ((ctx (ffi:%evp-pkey-ctx-new (crypto-key-handle key) (sb-sys:int-sap 0)))
         (decrypted nil))
    
    (unwind-protect
        (sb-alien:with-alien ((in-buf (sb-alien:array sb-alien:unsigned-char 4096))
                              (out-buf (sb-alien:array sb-alien:unsigned-char #.(expt 2 16)))
                              (out-len sb-alien:size-t :local #.(expt 2 16)))
			     ;; Copy encrypted data to alien buffer
			     (loop for i from 0 below (length encrypted-data)
				   do (setf (sb-alien:deref in-buf i) (aref encrypted-data i)))
			     
			     ;; Initialize decryption
			     (when (zerop (ffi:%evp-pkey-decrypt-init ctx))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to init decryption"))
			     
			     ;; Decrypt data
			     (when (zerop (ffi:%evp-pkey-decrypt ctx (sb-alien:alien-sap out-buf) 
								 (sb-alien:alien-sap (sb-alien:addr out-len))
								 (sb-alien:alien-sap in-buf) (length encrypted-data)))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to decrypt"))
			     
			     ;; Convert to string
			     (setf decrypted (make-string out-len))
			     (loop for i from 0 below out-len
				   do (setf (char decrypted i) (code-char (sb-alien:deref out-buf i)))))
      ;; Cleanup
      (ffi:%evp-pkey-ctx-free ctx))
    decrypted))

;;;; Random Number Generation

;; crypto-random-integer defined in package.lisp

;;;; Digest Functions

(defun digest (data algorithm)
  "Compute message digest of data using specified algorithm"
  (let* ((md-ctx (ffi:%evp-md-ctx-new))
         (md (ffi:%evp-get-digestbyname algorithm))
         ;; Convert string to UTF-8 bytes if needed
         (data-bytes (if (stringp data)
                         (sb-ext:string-to-octets data :external-format :utf-8)
                         data))
         (digest-bytes nil))
    
    (when (sb-sys:sap= md (sb-sys:int-sap 0))
      (error 'crypto-error :code -1 :message (format nil "Unknown digest: ~A" algorithm)))
    
    (unwind-protect
        (sb-alien:with-alien ((data-buf (sb-alien:array sb-alien:unsigned-char #.(expt 2 16)))
                              (md-buf (sb-alien:array sb-alien:unsigned-char 64))
                              (md-len sb-alien:unsigned-int :local 0))
			     ;; Copy data bytes to alien buffer
			     (let ((data-len (min (length data-bytes) #.(expt 2 16))))
			       (loop for i from 0 below data-len
				     do (setf (sb-alien:deref data-buf i) (aref data-bytes i)))
			       
			       ;; Initialize digest
			       (when (zerop (ffi:%evp-digestinit-ex md-ctx md (sb-sys:int-sap 0)))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to init digest"))
			       
			       ;; Update with data
			       (when (zerop (ffi:%evp-digestupdate md-ctx (sb-alien:alien-sap data-buf) data-len))
				 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to update digest")))
			     
			     ;; Finalize digest
			     (when (zerop (ffi:%evp-digestfinal-ex md-ctx (sb-alien:alien-sap md-buf) 
								   (sb-alien:alien-sap (sb-alien:addr md-len))))
			       (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to finalize digest"))
			     
			     ;; Convert to byte vector
			     (let ((bytes (make-array md-len :element-type '(unsigned-byte 8))))
			       (loop for i from 0 below md-len
				     do (setf (aref bytes i) (sb-alien:deref md-buf i)))
			       (setf digest-bytes bytes)))
      ;; Cleanup
      (ffi:%evp-md-ctx-free md-ctx))
    digest-bytes))

(defun digest-algorithm (name)
  "Get digest algorithm identifier"
  name)

;;;; Error Handling
;; get-crypto-errors defined in package.lisp

;;;; Cleanup

(defun cleanup-key (key)
  "Free resources associated with a key"
  (when (and (crypto-key-p key) (crypto-key-handle key))
    (ffi:%evp-pkey-free (crypto-key-handle key))
    (setf (crypto-key-handle key) nil)))
