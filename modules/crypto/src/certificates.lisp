;;;; Certificate Generation and Management
;;;;
;;;; Utilities for creating and managing X.509 certificates,
;;;; including self-signed certificates for development and testing

(defpackage :epsilon.crypto.certificates
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi)
   (#:lib #:epsilon.foreign))
  (:export
   #:generate-self-signed-certificate
   #:generate-certificate-request
   #:sign-certificate-request
   #:generate-ca-certificate
   #:load-certificate
   #:save-certificate
   #:load-private-key
   #:save-private-key
   #:verify-certificate-chain
   #:certificate-info
   #:make-certificate-pair
   #:evp-pkey-to-pem))

(in-package :epsilon.crypto.certificates)

;;;; Helper Functions for Real Implementation

(defun create-x509-name ()
  "Create a new X509 name structure"
  (let ((name (ffi:%x509-name-new)))
    (when (sb-sys:sap= name (sb-sys:int-sap 0))
      (error "Failed to create X509 name"))
    name))

(defun add-name-field (name field value)
  "Add a field to an X509 name"
  (let ((result (ffi:%x509-name-add-entry-by-txt
                 name field #x1000  ; MBSTRING_UTF8
                 value -1 -1 0)))
    (when (zerop result)
      (error "Failed to add name field ~A=~A" field value))))

(defun build-x509-name (&key common-name organization country state locality email)
  "Build an X509 name with the given fields"
  (let ((name (create-x509-name)))
    (handler-case
        (progn
          (when country (add-name-field name "C" country))
          (when state (add-name-field name "ST" state))
          (when locality (add-name-field name "L" locality))
          (when organization (add-name-field name "O" organization))
          (when common-name (add-name-field name "CN" common-name))
          (when email (add-name-field name "emailAddress" email))
          name)
      (error (e)
        (ffi:%x509-name-free name)
        (error e)))))

(defun create-rsa-key (bits)
  "Generate an RSA key pair using direct C call"
  (let* ((ctx (ffi:%evp-pkey-ctx-new-id 6 (sb-sys:int-sap 0))))  ; EVP_PKEY_RSA = 6
    (when (sb-sys:sap= ctx (sb-sys:int-sap 0))
      (error "Failed to create key generation context"))
    (unwind-protect
         (progn
           (when (zerop (ffi:%evp-pkey-keygen-init ctx))
             (error "Failed to initialize key generation"))
           (when (zerop (ffi:%evp-pkey-ctx-set-rsa-keygen-bits ctx bits))
             (error "Failed to set key size"))
           ;; Use raw memory allocation for the pointer
           (let ((ptr-size 8)  ; Size of a pointer on 64-bit
                 (pkey-ptr-addr nil)
                 (result-ptr nil))
             (unwind-protect
                  (progn
                    ;; Allocate memory for the pointer
                    (setf pkey-ptr-addr (sb-sys:allocate-system-memory ptr-size))
                    ;; Initialize to NULL
                    (setf (sb-sys:sap-ref-sap pkey-ptr-addr 0) (sb-sys:int-sap 0))
                    ;; Call the function
                    (when (zerop (ffi:%evp-pkey-keygen ctx pkey-ptr-addr))
                      (error "Failed to generate key"))
                    ;; Get the result pointer
                    (setf result-ptr (sb-sys:sap-ref-sap pkey-ptr-addr 0))
                    (when (sb-sys:sap= result-ptr (sb-sys:int-sap 0))
                      (error "Generated key is NULL"))
                    result-ptr)
                ;; Clean up allocated memory
                (when pkey-ptr-addr
                  (sb-sys:deallocate-system-memory pkey-ptr-addr ptr-size)))))
      (ffi:%evp-pkey-ctx-free ctx))))

(defun x509-to-pem (x509)
  "Convert X509 certificate to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (when (sb-sys:sap= bio (sb-sys:int-sap 0))
      (error "Failed to create BIO"))
    (unwind-protect
         (progn
           (when (zerop (ffi:%pem-write-bio-x509 bio x509))
             (error "Failed to write certificate to BIO"))
           ;; Get length first
           (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0))))  ; BIO_CTRL_INFO = 3
             (when (<= len 0)
               (error "No certificate data in BIO"))
             ;; Read the data
             (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
               (sb-sys:with-pinned-objects (buffer)
                 (let ((bytes-read (ffi:%bio-read bio (sb-sys:vector-sap buffer) len)))
                   (when (<= bytes-read 0)
                     (error "Failed to read certificate data"))
                   ;; Convert to string
                   (map 'string #'code-char buffer))))))
      (ffi:%bio-free bio))))

(defun evp-pkey-to-pem (pkey)
  "Convert EVP_PKEY to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (when (sb-sys:sap= bio (sb-sys:int-sap 0))
      (error "Failed to create BIO"))
    (unwind-protect
         (progn
           (when (zerop (ffi:%pem-write-bio-privatekey bio pkey
                                                       (sb-sys:int-sap 0)  ; cipher (none)
                                                       (sb-sys:int-sap 0)  ; kstr (none)
                                                       0                    ; klen (0)
                                                       (sb-sys:int-sap 0)  ; cb (none)
                                                       (sb-sys:int-sap 0))) ; u (none)
             (error "Failed to write private key to BIO"))
           ;; Get length first
           (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0))))  ; BIO_CTRL_INFO = 3
             (when (<= len 0)
               (error "No private key data in BIO"))
             ;; Read the data
             (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
               (sb-sys:with-pinned-objects (buffer)
                 (let ((bytes-read (ffi:%bio-read bio (sb-sys:vector-sap buffer) len)))
                   (when (<= bytes-read 0)
                     (error "Failed to read private key data"))
                   ;; Convert to string
                   (map 'string #'code-char buffer))))))
      (ffi:%bio-free bio))))

(defun generate-self-signed-certificate-real (common-name &key
                                              (days 365)
                                              (key-bits 2048)
                                              organization
                                              country
                                              state
                                              locality
                                              email)
  "Generate a real self-signed certificate using OpenSSL"
  (let ((x509 (ffi:%x509-new))
        (pkey nil)
        (name nil))
    (when (sb-sys:sap= x509 (sb-sys:int-sap 0))
      (error "Failed to create X509 certificate"))
    
    (unwind-protect
         (handler-case
             (progn
               ;; Generate key pair
               (setf pkey (create-rsa-key key-bits))
               
               ;; Set certificate version (v3)
               (when (zerop (ffi:%x509-set-version x509 2))
                 (error "Failed to set certificate version"))
               
               ;; Set serial number
               (let ((serial (ffi:%asn1-integer-new)))
                 (when (sb-sys:sap= serial (sb-sys:int-sap 0))
                   (error "Failed to create serial number"))
                 (unwind-protect
                      (progn
                        (when (zerop (ffi:%asn1-integer-set serial (random 1000000)))
                          (error "Failed to set serial number"))
                        (when (zerop (ffi:%x509-set-serialnumber x509 serial))
                          (error "Failed to set certificate serial number")))
                   (ffi:%asn1-integer-free serial)))
               
               ;; Set validity period
               (let ((not-before (ffi:%asn1-time-new))
                     (not-after (ffi:%asn1-time-new)))
                 (when (or (sb-sys:sap= not-before (sb-sys:int-sap 0))
                           (sb-sys:sap= not-after (sb-sys:int-sap 0)))
                   (error "Failed to create time structures"))
                 (ffi:%x509-time-adj-ex not-before 0 0 (sb-sys:int-sap 0))
                 (ffi:%x509-time-adj-ex not-after days 0 (sb-sys:int-sap 0))
                 (when (zerop (ffi:%x509-set-notbefore x509 not-before))
                   (error "Failed to set not-before"))
                 (when (zerop (ffi:%x509-set-notafter x509 not-after))
                   (error "Failed to set not-after")))
               
               ;; Create and set subject name
               (setf name (build-x509-name :common-name common-name
                                           :organization organization
                                           :country country
                                           :state state
                                           :locality locality))
               (when (zerop (ffi:%x509-set-subject-name x509 name))
                 (error "Failed to set subject name"))
               
               ;; For self-signed, issuer = subject
               (when (zerop (ffi:%x509-set-issuer-name x509 name))
                 (error "Failed to set issuer name"))
               
               ;; Set public key
               (when (zerop (ffi:%x509-set-pubkey x509 pkey))
                 (error "Failed to set public key"))
               
               ;; Sign the certificate
               (let ((digest (ffi:%evp-sha256)))
                 (when (zerop (ffi:%x509-sign x509 pkey digest))
                   (error "Failed to sign certificate")))
               
               ;; Convert to PEM format
               (values (x509-to-pem x509)
                       (evp-pkey-to-pem pkey)))
           
           (error (e)
             ;; Clean up and re-throw
             (error "Certificate generation failed: ~A" e)))
      
      ;; Cleanup
      (when name (ffi:%x509-name-free name))
      (when pkey (ffi:%evp-pkey-free pkey))
      (when x509 (ffi:%x509-free x509)))))

;;;; Additional Real Implementation Functions

(defun generate-certificate-request-real (common-name private-key &key
                                          organization
                                          country
                                          state
                                          locality
                                          email)
  "Generate a real certificate signing request"
  (let ((req (ffi:%x509-req-new))
        (name nil)
        (need-key-gen (not private-key))
        (local-pkey private-key))
    (when (sb-sys:sap= req (sb-sys:int-sap 0))
      (error "Failed to create X509 request"))
    
    (unwind-protect
         (handler-case
             (progn
               ;; Generate key if not provided
               (when need-key-gen
                 (setf local-pkey (create-rsa-key 2048)))
               
               ;; Set version
               (when (zerop (ffi:%x509-req-set-version req 0))
                 (error "Failed to set request version"))
               
               ;; Create and set subject name
               (setf name (build-x509-name :common-name common-name
                                           :organization organization
                                           :country country
                                           :state state
                                           :locality locality
                                           :email email))
               (when (zerop (ffi:%x509-req-set-subject-name req name))
                 (error "Failed to set subject name"))
               
               ;; Set public key
               (when (zerop (ffi:%x509-req-set-pubkey req local-pkey))
                 (error "Failed to set public key"))
               
               ;; Sign the request
               (let ((digest (ffi:%evp-sha256)))
                 (when (zerop (ffi:%x509-req-sign req local-pkey digest))
                   (error "Failed to sign request")))
               
               ;; Convert to PEM
               (csr-to-pem req))
           
           (error (e)
             (error "CSR generation failed: ~A" e)))
      
      ;; Cleanup
      (when name (ffi:%x509-name-free name))
      (when req (ffi:%x509-req-free req))
      (when (and need-key-gen local-pkey)
        (ffi:%evp-pkey-free local-pkey)))))

(defun csr-to-pem (req)
  "Convert CSR to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (when (sb-sys:sap= bio (sb-sys:int-sap 0))
      (error "Failed to create BIO"))
    (unwind-protect
         (progn
           (when (zerop (ffi:%pem-write-bio-x509-req bio req))
             (error "Failed to write CSR to BIO"))
           (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0))))
             (when (<= len 0)
               (error "No data in BIO"))
             (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
               (sb-sys:with-pinned-objects (buffer)
                 (let ((bytes-read (ffi:%bio-read bio (sb-sys:vector-sap buffer) len)))
                   (when (<= bytes-read 0)
                     (error "Failed to read CSR data"))
                   (map 'string #'code-char buffer))))))
      (ffi:%bio-free bio))))

(defun verify-certificate-chain-real (cert-pem ca-cert-pem)
  "Verify that a certificate was signed by a CA"
  (let* ((cert (pem-to-certificate cert-pem))
         (ca-cert (pem-to-certificate ca-cert-pem))
         (ca-pubkey nil))
    (unwind-protect
         (handler-case
             (progn
               ;; Get CA's public key
               (setf ca-pubkey (ffi:%x509-get-pubkey ca-cert))
               (when (sb-sys:sap= ca-pubkey (sb-sys:int-sap 0))
                 (error "Failed to get CA public key"))
               
               ;; Verify certificate signature with CA's public key
               (let ((result (ffi:%x509-verify cert ca-pubkey)))
                 (case result
                   (1 t)  ; Valid
                   (0 nil) ; Invalid
                   (otherwise
                    (error "Verification error: ~A" result)))))
           
           (error (e)
             (warn "Certificate verification failed: ~A" e)
             nil))
      
      ;; Cleanup
      (when ca-pubkey (ffi:%evp-pkey-free ca-pubkey))
      (when cert (ffi:%x509-free cert))
      (when ca-cert (ffi:%x509-free ca-cert)))))

(defun pem-to-certificate (pem-string)
  "Convert PEM string to X509 certificate"
  ;; Convert string to byte array and pin it
  (let ((byte-array (map 'vector #'char-code pem-string)))
    (sb-sys:with-pinned-objects (byte-array)
      (let ((bio (ffi:%bio-new-mem-buf (sb-sys:vector-sap byte-array) (length byte-array))))
        (when (sb-sys:sap= bio (sb-sys:int-sap 0))
          (error "Failed to create BIO"))
        (unwind-protect
             (let ((cert (ffi:%pem-read-bio-x509 bio (sb-sys:int-sap 0)
                                                 (sb-sys:int-sap 0)
                                                 (sb-sys:int-sap 0))))
               (when (sb-sys:sap= cert (sb-sys:int-sap 0))
                 (error "Failed to read certificate from PEM"))
               cert)
          (ffi:%bio-free bio))))))

(defun sign-certificate-request-real (csr-pem ca-cert-pem ca-key-pem &key (days 365))
  "Sign a CSR with a CA certificate to produce a signed certificate"
  (let* ((csr (pem-to-csr csr-pem))
         (ca-cert (pem-to-certificate ca-cert-pem))
         (ca-key (pem-to-private-key ca-key-pem))
         (x509 (ffi:%x509-new))
         (serial nil)
         (not-before nil)
         (not-after nil))
    (when (sb-sys:sap= x509 (sb-sys:int-sap 0))
      (error "Failed to create X509 certificate"))
    
    (unwind-protect
         (handler-case
             (progn
               ;; Set version (v3)
               (when (zerop (ffi:%x509-set-version x509 2))
                 (error "Failed to set certificate version"))
               
               ;; Set serial number
               (setf serial (ffi:%asn1-integer-new))
               (when (sb-sys:sap= serial (sb-sys:int-sap 0))
                 (error "Failed to create serial number"))
               (when (zerop (ffi:%asn1-integer-set serial (random 1000000)))
                 (error "Failed to set serial number value"))
               (when (zerop (ffi:%x509-set-serialnumber x509 serial))
                 (error "Failed to set certificate serial number"))
               
               ;; Set validity period
               (setf not-before (ffi:%asn1-time-new))
               (setf not-after (ffi:%asn1-time-new))
               (when (or (sb-sys:sap= not-before (sb-sys:int-sap 0))
                         (sb-sys:sap= not-after (sb-sys:int-sap 0)))
                 (error "Failed to create time structures"))
               (ffi:%x509-time-adj-ex not-before 0 0 (sb-sys:int-sap 0))
               (ffi:%x509-time-adj-ex not-after days 0 (sb-sys:int-sap 0))
               (when (zerop (ffi:%x509-set-notbefore x509 not-before))
                 (error "Failed to set not-before"))
               (when (zerop (ffi:%x509-set-notafter x509 not-after))
                 (error "Failed to set not-after"))
               
               ;; Copy subject from CSR
               (let ((subject (ffi:%x509-req-get-subject-name csr)))
                 (when (zerop (ffi:%x509-set-subject-name x509 subject))
                   (error "Failed to set subject name")))
               
               ;; Set issuer from CA certificate
               (let ((issuer (ffi:%x509-get-issuer-name ca-cert)))
                 (when (zerop (ffi:%x509-set-issuer-name x509 issuer))
                   (error "Failed to set issuer name")))
               
               ;; Get and set public key from CSR
               (let ((req-pubkey (ffi:%x509-req-get-pubkey csr)))
                 (when (sb-sys:sap= req-pubkey (sb-sys:int-sap 0))
                   (error "Failed to get public key from CSR"))
                 (when (zerop (ffi:%x509-set-pubkey x509 req-pubkey))
                   (error "Failed to set public key"))
                 (ffi:%evp-pkey-free req-pubkey))
               
               ;; Sign with CA key
               (let ((digest (ffi:%evp-sha256)))
                 (when (zerop (ffi:%x509-sign x509 ca-key digest))
                   (error "Failed to sign certificate")))
               
               ;; Convert to PEM
               (x509-to-pem x509))
           
           (error (e)
             (error "Certificate signing failed: ~A" e)))
      
      ;; Cleanup
      (when serial (ffi:%asn1-integer-free serial))
      (when not-before (ffi:%asn1-time-free not-before))
      (when not-after (ffi:%asn1-time-free not-after))
      (when csr (ffi:%x509-req-free csr))
      (when ca-cert (ffi:%x509-free ca-cert))
      (when ca-key (ffi:%evp-pkey-free ca-key))
      (when x509 (ffi:%x509-free x509)))))

(defun pem-to-csr (pem-string)
  "Convert PEM string to CSR"
  (let ((byte-array (map 'vector #'char-code pem-string)))
    (sb-sys:with-pinned-objects (byte-array)
      (let ((bio (ffi:%bio-new-mem-buf (sb-sys:vector-sap byte-array) (length byte-array))))
        (when (sb-sys:sap= bio (sb-sys:int-sap 0))
          (error "Failed to create BIO"))
        (unwind-protect
             (let ((req (ffi:%pem-read-bio-x509-req bio (sb-sys:int-sap 0)
                                                    (sb-sys:int-sap 0)
                                                    (sb-sys:int-sap 0))))
               (when (sb-sys:sap= req (sb-sys:int-sap 0))
                 (error "Failed to read CSR from PEM"))
               req)
          (ffi:%bio-free bio))))))

(defun pem-to-private-key (pem-string)
  "Convert PEM string to private key"
  (let ((byte-array (map 'vector #'char-code pem-string)))
    (sb-sys:with-pinned-objects (byte-array)
      (let ((bio (ffi:%bio-new-mem-buf (sb-sys:vector-sap byte-array) (length byte-array))))
        (when (sb-sys:sap= bio (sb-sys:int-sap 0))
          (error "Failed to create BIO"))
        (unwind-protect
             (let ((pkey (ffi:%pem-read-bio-privatekey bio (sb-sys:int-sap 0)
                                                       (sb-sys:int-sap 0)
                                                       (sb-sys:int-sap 0))))
               (when (sb-sys:sap= pkey (sb-sys:int-sap 0))
                 (error "Failed to read private key from PEM"))
               pkey)
          (ffi:%bio-free bio))))))

;;;; Public API Functions

(defun generate-self-signed-certificate (common-name &key
                                         (days 365)
                                         (key-bits 2048)
                                         organization
                                         country
                                         state
                                         locality
                                         email)
  "Generate a self-signed certificate with private key.
   Returns (values certificate-pem private-key-pem)"
  ;; Generate a key pair
  (let ((key (epsilon.crypto:generate-rsa-key :bits key-bits)))
    ;; Build subject DN
    (let ((subject-dn (list "CN" common-name)))
      (when organization
        (setf subject-dn (append subject-dn (list "O" organization))))
      (when country
        (setf subject-dn (append subject-dn (list "C" country))))
      (when state
        (setf subject-dn (append subject-dn (list "ST" state))))
      (when locality
        (setf subject-dn (append subject-dn (list "L" locality))))
      (when email
        (setf subject-dn (append subject-dn (list "emailAddress" email))))
      
      ;; Create self-signed certificate using x509 module
      (let ((cert (epsilon.crypto.x509:create-certificate
                   :subject subject-dn
                   :issuer subject-dn  ; Self-signed = same issuer and subject
                   :public-key key
                   :signing-key key
                   :days days
                   :serial (random 1000000))))
        (values (epsilon.crypto.x509:save-certificate cert)
                (epsilon.crypto:key-to-pem key :private-p t))))))

(defun generate-certificate-request (common-name private-key &key
                                    organization
                                    country
                                    state
                                    locality
                                    email)
  "Generate a Certificate Signing Request (CSR)"
  ;; Build subject DN
  (let ((subject-dn (list "CN" common-name)))
    (when organization
      (setf subject-dn (append subject-dn (list "O" organization))))
    (when country
      (setf subject-dn (append subject-dn (list "C" country))))
    (when state
      (setf subject-dn (append subject-dn (list "ST" state))))
    (when locality
      (setf subject-dn (append subject-dn (list "L" locality))))
    (when email
      (setf subject-dn (append subject-dn (list "emailAddress" email))))
    
    ;; Create CSR using x509 module
    (epsilon.crypto.x509:create-csr private-key subject-dn)))

(defun sign-certificate-request (csr-pem ca-cert-pem ca-key-pem &key (days 365))
  "Sign a CSR with a CA certificate to produce a signed certificate"
  ;; Load components
  (let* ((csr-handle (epsilon.crypto.x509:load-csr csr-pem))
         (ca-cert (epsilon.crypto.x509:load-certificate ca-cert-pem))
         (ca-key (epsilon.crypto:key-from-pem ca-key-pem :private-p t))
         (issuer-dn (list "CN" (epsilon.crypto.x509:x509-certificate-subject ca-cert)))
         (cert (epsilon.crypto.x509:sign-csr csr-handle issuer-dn nil ca-key
                                             :days days
                                             :serial (random 1000000))))
    (epsilon.crypto.x509:save-certificate cert)))

(defun generate-ca-certificate (common-name &key
                               (days 3650)
                               (key-bits 4096)
                               organization
                               country
                               state
                               locality)
  "Generate a self-signed CA certificate"
  (generate-self-signed-certificate
   common-name
   :days days
   :key-bits key-bits
   :organization (or organization "Certificate Authority")
   :country country
   :state state
   :locality locality))

(defun save-certificate (certificate-pem filepath)
  "Save certificate PEM to file"
  (with-open-file (stream filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (write-string certificate-pem stream)))

(defun load-certificate (filepath)
  "Load certificate PEM from file"
  (with-open-file (stream filepath :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun save-private-key (key-pem filepath)
  "Save private key PEM to file with restrictive permissions"
  (with-open-file (stream filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (write-string key-pem stream))
  ;; Set restrictive permissions (owner read/write only)
  #+unix
  (sb-posix:chmod filepath #o600))

(defun load-private-key (filepath)
  "Load private key PEM from file"
  (with-open-file (stream filepath :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun verify-certificate-chain (cert-pem ca-cert-pem)
  "Verify that a certificate was signed by a CA"
  (let* ((cert (epsilon.crypto.x509:load-certificate cert-pem))
         (ca-cert (epsilon.crypto.x509:load-certificate ca-cert-pem))
         (ca-public-key (epsilon.crypto.x509:certificate-public-key ca-cert)))
    (epsilon.crypto.x509:verify-certificate cert ca-public-key)))

(defun certificate-info (cert-pem)
  "Extract information from a certificate"
  (let ((cert (epsilon.crypto.x509:load-certificate cert-pem)))
    (list :subject (epsilon.crypto.x509:x509-certificate-subject cert)
          :issuer (epsilon.crypto.x509:x509-certificate-issuer cert)
          :serial (epsilon.crypto.x509:x509-certificate-serial cert)
          :not-before (epsilon.crypto.x509:x509-certificate-not-before cert)
          :not-after (epsilon.crypto.x509:x509-certificate-not-after cert))))


(defun make-certificate-pair (common-name &key (output-dir "/tmp"))
  "Generate a certificate and key pair, saving to files.
   Returns (values cert-file key-file)"
  (multiple-value-bind (cert-pem key-pem)
      (generate-self-signed-certificate common-name)
    (let ((cert-file (format nil "~A/~A-cert.pem" output-dir common-name))
          (key-file (format nil "~A/~A-key.pem" output-dir common-name)))
      (save-certificate cert-pem cert-file)
      (save-private-key key-pem key-file)
      (values cert-file key-file))))