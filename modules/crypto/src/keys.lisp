;;;; Cryptographic Operations Implementation
;;;;
;;;; This file provides public key cryptography operations using OpenSSL's libcrypto

(defpackage :epsilon.crypto.keys
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
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

(defun generate-rsa-key (bits)
  "Generate an RSA key pair with specified bit size"
  (unless (member bits '(2048 3072 4096))
    (error 'crypto-error :code -1 
           :message "RSA key size must be 2048, 3072, or 4096 bits"))
  
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

(defun generate-ec-key (curve)
  "Generate an EC key pair for specified curve.
   Supported curves: :p256, :p384, :p521, :secp256k1"
  (let* ((curve-name (case curve
			   (:p256 "prime256v1")
			   (:p384 "secp384r1")
			   (:p521 "secp521r1")
			   (:secp256k1 "secp256k1")
			   (t (error "Unsupported EC curve: ~A" curve))))
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
  "Generate an Ed25519 key pair"
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
  "Export key to PEM format string"
  (unless (crypto-key-p key)
    (error "Invalid key object"))
  
  (when (and private-p (not (crypto-key-private-p key)))
    (error "Key does not contain private key"))
  
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

(defun crypto-random-bytes (n)
  "Generate n random bytes"
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 4096)))
		       (when (> n 4096)
			 (error "Maximum 4096 random bytes at once"))
		       
		       (when (zerop (ffi:%rand-bytes (sb-alien:alien-sap buf) n))
			 (error 'crypto-error :code (ffi:%err-get-error) :message "Failed to generate random bytes"))
		       
		       (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
			 (loop for i from 0 below n
			       do (setf (aref bytes i) (sb-alien:deref buf i)))
			 bytes)))

(defun crypto-random-integer (max)
  "Generate random integer from 0 to max-1"
  (let* ((bytes-needed (ceiling (log max 256)))
         (bytes (crypto-random-bytes bytes-needed))
         (value 0))
    (loop for i from 0 below bytes-needed
          do (setf value (+ (* value 256) (aref bytes i))))
    (mod value max)))

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

(defun get-crypto-errors ()
  "Get all pending OpenSSL errors as a list"
  (loop for err = (ffi:%err-get-error)
        while (plusp err)
        collect (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
				     (let ((str-ptr (ffi:%err-error-string err (sb-alien:alien-sap buf))))
				       (unless (sb-sys:sap= str-ptr (sb-sys:int-sap 0))
					 (sb-alien:cast str-ptr sb-alien:c-string))))))

;;;; Cleanup

(defun cleanup-key (key)
  "Free resources associated with a key"
  (when (and (crypto-key-p key) (crypto-key-handle key))
    (ffi:%evp-pkey-free (crypto-key-handle key))
    (setf (crypto-key-handle key) nil)))
