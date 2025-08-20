;;;; X.509 Certificate and CSR Operations
;;;;
;;;; This file provides certificate and CSR functionality

(defpackage :epsilon.crypto.x509
  (:use :cl :epsilon.crypto)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
  (:import-from :epsilon.crypto
		;; Import needed types and functions
		#:x509-certificate
		#:x509-certificate-p
		#:x509-certificate-handle
		#:x509-certificate-subject
		#:x509-certificate-issuer
		#:x509-certificate-serial
		#:x509-certificate-not-before
		#:x509-certificate-not-after
		#:make-x509-certificate
		#:crypto-key
		#:crypto-key-p
		#:crypto-key-handle
		#:crypto-key-type
		#:crypto-error
		#:+digest-sha256+))

(in-package :epsilon.crypto.x509)

;;;; Helper Functions for BIO Operations

(defun call-with-bio-mem (fn)
  "Create a memory BIO, call FN with it, and ensure cleanup"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (when (sb-sys:sap= bio (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to create BIO"))
    (unwind-protect
        (funcall fn bio)
      (ffi:%bio-free bio))))

(defun call-with-bio-mem-buf (string fn)
  "Create a BIO from string, call FN with it, and ensure cleanup"
  (let* ((len (length string))
         (buffer (sb-ext:string-to-octets string :external-format :utf-8)))
    (sb-sys:with-pinned-objects (buffer)
      (let* ((buffer-sap (sb-sys:vector-sap buffer))
             (bio (ffi:%bio-new-mem-buf buffer-sap len)))
        (when (sb-sys:sap= bio (sb-sys:int-sap 0))
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Failed to create BIO from buffer"))
        (unwind-protect
            (funcall fn bio)
          (ffi:%bio-free bio))))))

(defun bio-to-string (bio)
  "Extract string contents from a BIO"
  (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0)))) ; BIO_CTRL_PENDING = 3
    (when (plusp len)
      ;; Use a byte array to read the data
      (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
        (sb-sys:with-pinned-objects (buffer)
          (let* ((buffer-sap (sb-sys:vector-sap buffer))
                 (bytes-read (ffi:%bio-read bio buffer-sap len)))
            (when (plusp bytes-read)
              ;; Convert bytes to string
              (sb-ext:octets-to-string buffer :end bytes-read :external-format :utf-8))))))))

(defun get-x509-name-string (name-ptr)
  "Convert X509_NAME pointer to string using X509_NAME_oneline"
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
    ;; X509_NAME_oneline writes to the buffer and returns a pointer to it
    (ffi:%x509-name-oneline name-ptr (sb-alien:alien-sap buf) 256)
    ;; Convert the buffer to a string
    (sb-alien:cast buf sb-alien:c-string)))

(defun get-x509-serial-number (x509)
  "Extract serial number from X509 certificate"
  (let ((serial-asn1 (ffi:%x509-get-serialnumber x509)))
    (if (sb-sys:sap= serial-asn1 (sb-sys:int-sap 0))
        "0"
        (format nil "~D" (ffi:%asn1-integer-get serial-asn1)))))

;;;; Helper Functions for X509 Operations

(defmacro with-x509 ((var) &body body)
  "Create an X509 certificate, execute body, and clean up on error"
  (let ((success (gensym "SUCCESS")))
    `(let ((,var (ffi:%x509-new))
           (,success nil))
       (when (sb-sys:sap= ,var (sb-sys:int-sap 0))
         (error 'crypto-error :code (ffi:%err-get-error)
                :message "Failed to create X509 structure"))
       (unwind-protect
           (prog1
               (progn ,@body)
             (setf ,success t))
         (unless ,success
           (when ,var
             (ffi:%x509-free ,var)))))))

(defmacro with-asn1-integer ((var value) &body body)
  "Create an ASN1 integer, set its value, execute body, and clean up"
  `(let ((,var (ffi:%asn1-integer-new)))
     (when (sb-sys:sap= ,var (sb-sys:int-sap 0))
       (error 'crypto-error :code (ffi:%err-get-error)
              :message "Failed to create ASN1 integer"))
     (unwind-protect
         (progn
           (ffi:%asn1-integer-set ,var ,value)
           ,@body)
       (ffi:%asn1-integer-free ,var))))

(defun set-x509-version (x509 version)
  "Set the X509 certificate version"
  (when (zerop (ffi:%x509-set-version x509 version))
    (error 'crypto-error :code (ffi:%err-get-error)
           :message "Failed to set certificate version")))

(defun set-x509-serial (x509 serial)
  "Set the X509 certificate serial number"
  (with-asn1-integer (serial-asn1 serial)
    (when (zerop (ffi:%x509-set-serialnumber x509 serial-asn1))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to set serial number"))))

(defun set-x509-validity (x509 days)
  "Set the X509 certificate validity period"
  (let ((not-before (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) 0 0 (sb-sys:int-sap 0)))
        (not-after (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) days 0 (sb-sys:int-sap 0))))
    (when (zerop (ffi:%x509-set-notbefore x509 not-before))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to set not-before time"))
    (when (zerop (ffi:%x509-set-notafter x509 not-after))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to set not-after time"))))

(defun add-x509-name-entries (name-ptr entries)
  "Add entries to an X509 name"
  (loop for (field value) on entries by #'cddr
        do (when (zerop (ffi:%x509-name-add-entry-by-txt 
                         name-ptr (string field) 4097 ; MBSTRING_ASC
                         (string value) -1 -1 0))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message (format nil "Failed to add name field: ~A" field)))))

(defun set-x509-subject (x509 subject)
  "Set the X509 certificate subject"
  (when subject
    (let ((name (ffi:%x509-get-subject-name x509)))
      (add-x509-name-entries name subject))))

(defun set-x509-issuer (x509 issuer)
  "Set the X509 certificate issuer"
  (when issuer
    (let ((name (ffi:%x509-get-issuer-name x509)))
      (add-x509-name-entries name issuer))))

(defun set-x509-public-key (x509 public-key)
  "Set the X509 certificate public key"
  (when (zerop (ffi:%x509-set-pubkey x509 (crypto-key-handle public-key)))
    (error 'crypto-error :code (ffi:%err-get-error)
           :message "Failed to set public key")))

(defun sign-x509 (x509 signing-key)
  "Sign the X509 certificate"
  (let ((md (if (eq (crypto-key-type signing-key) :ed25519)
                (sb-sys:int-sap 0)  ; NULL for Ed25519
                (ffi:%evp-get-digestbyname +digest-sha256+))))
    (when (and (not (eq (crypto-key-type signing-key) :ed25519))
               (sb-sys:sap= md (sb-sys:int-sap 0)))
      (error 'crypto-error :code -1 :message "Unknown digest algorithm"))
    (when (zerop (ffi:%x509-sign x509 (crypto-key-handle signing-key) md))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to sign certificate"))))

;;;; Main Certificate Functions

(defun create-certificate (&key
                           subject
                           issuer
                           (serial 1)
                           (days 365)
                           public-key
                           signing-key
                           ;; Compatibility arguments
                           key
                           subject-cn
                           issuer-cn)
  "Create a new X.509 certificate with specified attributes.
   
   Creates an X.509 v3 certificate suitable for TLS, code signing, or other PKI uses.
   Can create self-signed certificates (when issuer equals subject) or certificates
   signed by a CA (when issuer differs from subject).
   
   Parameters:
     subject (list): Subject DN as property list (\"CN\" \"example.com\" \"O\" \"Corp\")
     issuer (list): Issuer DN as property list (defaults to subject for self-signed)
     serial (integer): Certificate serial number (default: 1)
     days (integer): Validity period in days from now (default: 365)
     public-key (crypto-key): Public key to embed in certificate
     signing-key (crypto-key): Private key to sign certificate (issuer's key)
     
     Compatibility parameters (deprecated):
     key: Same key for both public-key and signing-key (self-signed)
     subject-cn: Simple common name for subject
     issuer-cn: Simple common name for issuer
   
   Returns:
     X509-CERTIFICATE structure containing the created certificate
   
   Distinguished Name (DN) Fields:
     \"CN\" - Common Name (e.g., domain name)
     \"O\" - Organization
     \"OU\" - Organizational Unit
     \"C\" - Country (2-letter code)
     \"ST\" - State/Province
     \"L\" - Locality/City
     \"emailAddress\" - Email address
   
   Example - Self-signed certificate:
     (create-certificate
       :subject (list \"CN\" \"example.com\" \"O\" \"Example Corp\" \"C\" \"US\")
       :public-key my-key
       :signing-key my-key
       :days 365)
   
   Example - CA-signed certificate:
     (create-certificate
       :subject (list \"CN\" \"server.example.com\")
       :issuer (list \"CN\" \"Example CA\" \"O\" \"Example Corp\")
       :public-key server-key
       :signing-key ca-key
       :serial 12345
       :days 90)
   
   Security Notes:
     - Serial numbers must be unique per issuer
     - Use appropriate validity periods (shorter is more secure)
     - For production, use proper CA infrastructure
     - Self-signed certificates require explicit trust
   
   Errors:
     Signals CRYPTO-ERROR if certificate creation or signing fails"
  ;; Handle compatibility arguments
  (when key
    (setf public-key (or public-key key)
          signing-key (or signing-key key)))
  (when subject-cn
    (setf subject (or subject (list "CN" subject-cn))))
  (when issuer-cn
    (setf issuer (or issuer (list "CN" issuer-cn))))
  
  (unless (crypto-key-p public-key)
    (error "Invalid public key"))
  
  (unless (crypto-key-p signing-key)
    (error "Invalid signing key"))
  
  (with-x509 (x509)
    ;; Set all certificate fields
    (set-x509-version x509 2)  ; X509 v3
    (set-x509-serial x509 serial)
    (set-x509-validity x509 days)
    (set-x509-subject x509 subject)
    (set-x509-issuer x509 issuer)
    (set-x509-public-key x509 public-key)
    (sign-x509 x509 signing-key)
    
    ;; Extract certificate info for the structure
    (let ((subject-str (get-x509-name-string (ffi:%x509-get-subject-name x509)))
          (issuer-str (get-x509-name-string (ffi:%x509-get-issuer-name x509))))
      (make-x509-certificate :handle x509
                            :subject subject-str
                            :issuer issuer-str
                            :serial (format nil "~D" serial)
                            :not-before 0
                            :not-after (* days 86400)))))

(defun load-certificate (pem-string)
  "Load X.509 certificate from PEM string"
  (call-with-bio-mem-buf pem-string
    (lambda (bio)
      (let ((x509 (ffi:%pem-read-bio-x509 bio (sb-sys:int-sap 0)
                                          (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
        (when (sb-sys:sap= x509 (sb-sys:int-sap 0))
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Failed to read certificate from PEM"))
        
        ;; Extract certificate info
        (let ((subject (get-x509-name-string (ffi:%x509-get-subject-name x509)))
              (issuer (get-x509-name-string (ffi:%x509-get-issuer-name x509)))
              (serial (get-x509-serial-number x509)))
          ;; TODO: Extract actual not-before, not-after values
          (make-x509-certificate :handle x509
                                :subject subject
                                :issuer issuer
                                :serial serial
                                :not-before 0
                                :not-after 0))))))

(defun save-certificate (certificate)
  "Export X.509 certificate to PEM format string"
  (unless (x509-certificate-p certificate)
    (error "Invalid certificate object"))
  
  (call-with-bio-mem
   (lambda (bio)
     (when (zerop (ffi:%pem-write-bio-x509 bio (x509-certificate-handle certificate)))
       (error 'crypto-error :code (ffi:%err-get-error)
              :message "Failed to write certificate"))
     (bio-to-string bio))))

(defun certificate-public-key (certificate)
  "Extract public key from X.509 certificate"
  (unless (x509-certificate-p certificate)
    (error "Invalid certificate object"))
  
  (let ((pkey (ffi:%x509-get-pubkey (x509-certificate-handle certificate))))
    (when (sb-sys:sap= pkey (sb-sys:int-sap 0))
      (error 'crypto-error :code (ffi:%err-get-error)
             :message "Failed to extract public key from certificate"))
    
    ;; Determine key type
    (let ((key-id (ffi:%evp-pkey-id pkey)))
      (make-crypto-key 
       :handle pkey
       :type (cond ((= key-id 6) :rsa)  ; EVP_PKEY_RSA
                   ((= key-id 408) :ec)   ; EVP_PKEY_EC
                   ((= key-id 1087) :ed25519) ; EVP_PKEY_ED25519
                   (t :unknown))
       :bits (ffi:%evp-pkey-bits pkey)
       :public-p t
       :private-p nil))))

(defun verify-certificate (certificate issuer-key)
  "Verify certificate signature with issuer's public key"
  (unless (x509-certificate-p certificate)
    (error "Invalid certificate object"))
  
  (unless (crypto-key-p issuer-key)
    (error "Invalid issuer key"))
  
  (= 1 (ffi:%x509-verify (x509-certificate-handle certificate)
                        (crypto-key-handle issuer-key))))

;;;; Certificate Signing Request (CSR) Operations

(defmacro with-x509-req ((var) &body body)
  "Create an X509 REQ, execute body, and clean up on error"
  (let ((success (gensym "SUCCESS")))
    `(let ((,var (ffi:%x509-req-new))
           (,success nil))
       (when (sb-sys:sap= ,var (sb-sys:int-sap 0))
         (error 'crypto-error :code (ffi:%err-get-error)
                :message "Failed to create X509 REQ structure"))
       (unwind-protect
           (prog1
               (progn ,@body)
             (setf ,success t))
         (unless ,success
           (when ,var
             (ffi:%x509-req-free ,var)))))))

(defun create-csr (key &optional subject-or-keyword cn-value)
  "Create a Certificate Signing Request"
  (let ((subject
         (cond
          ;; Handle (create-csr key :subject-cn "value") format
          ((eq subject-or-keyword :subject-cn)
           (list "CN" cn-value))
          ;; Handle (create-csr key subject) format
          (subject-or-keyword
           subject-or-keyword)
          ;; Default to empty subject
          (t nil))))
    
    (unless (crypto-key-p key)
      (error "Invalid key object"))
    
    (with-x509-req (req)
      ;; Set public key
      (when (zerop (ffi:%x509-req-set-pubkey req (crypto-key-handle key)))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set CSR public key"))
      
      ;; Set subject name
      (when subject
        (let ((name (ffi:%x509-req-get-subject-name req)))
          (add-x509-name-entries name subject)))
      
      ;; Sign CSR - Ed25519 uses NULL digest
      (let ((md (if (eq (crypto-key-type key) :ed25519)
                    (sb-sys:int-sap 0)  ; NULL for Ed25519
                    (ffi:%evp-get-digestbyname +digest-sha256+))))
        (when (and (not (eq (crypto-key-type key) :ed25519))
                   (sb-sys:sap= md (sb-sys:int-sap 0)))
          (error 'crypto-error :code -1 :message "Unknown digest algorithm"))
        
        (when (zerop (ffi:%x509-req-sign req (crypto-key-handle key) md))
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Failed to sign CSR")))
      
      ;; Convert CSR to PEM string before returning
      (save-csr req))))

(defun sign-csr (csr-handle issuer subject signing-key &key (days 365) (serial 1))
  "Sign a CSR to create a certificate"
  (with-x509 (x509)
    ;; Set certificate fields
    (set-x509-version x509 2)  ; X509 v3
    (set-x509-serial x509 serial)
    (set-x509-validity x509 days)
    
    ;; Copy subject from CSR
    (let ((csr-subject (ffi:%x509-req-get-subject-name csr-handle)))
      (when (zerop (ffi:%x509-set-subject-name x509 csr-subject))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set subject name from CSR")))
    
    ;; Set issuer name
    (set-x509-issuer x509 issuer)
    
    ;; Get public key from CSR (this creates a new reference)
    (let ((pubkey (ffi:%x509-req-get-pubkey csr-handle)))
      (when (sb-sys:sap= pubkey (sb-sys:int-sap 0))
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to get public key from CSR"))
      
      ;; Set public key in certificate
      (when (zerop (ffi:%x509-set-pubkey x509 pubkey))
        (ffi:%evp-pkey-free pubkey)
        (error 'crypto-error :code (ffi:%err-get-error)
               :message "Failed to set public key"))
      
      (ffi:%evp-pkey-free pubkey)) ; Free the reference
    
    ;; Sign certificate
    (sign-x509 x509 signing-key)
    
    ;; Create certificate structure
    (let ((subject-str (get-x509-name-string (ffi:%x509-get-subject-name x509)))
          (issuer-str (get-x509-name-string (ffi:%x509-get-issuer-name x509))))
      (make-x509-certificate :handle x509
                            :subject subject-str
                            :issuer issuer-str
                            :serial (format nil "~D" serial)
                            :not-before 0
                            :not-after (* days 86400)))))

(defun load-csr (pem-string)
  "Load CSR from PEM string"
  (call-with-bio-mem-buf pem-string
    (lambda (bio)
      (let ((req (ffi:%pem-read-bio-x509-req bio (sb-sys:int-sap 0)
                                             (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
        (when (sb-sys:sap= req (sb-sys:int-sap 0))
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Failed to read CSR from PEM"))
        req))))

(defun save-csr (csr-handle)
  "Export CSR to PEM format string"
  (call-with-bio-mem
   (lambda (bio)
     (when (zerop (ffi:%pem-write-bio-x509-req bio csr-handle))
       (error 'crypto-error :code (ffi:%err-get-error)
              :message "Failed to write CSR"))
     (bio-to-string bio))))

;;;; Utility Functions for Key/Certificate Pairs

(defun save-key-and-cert-pair (key cert key-file cert-file)
  "Save a key and certificate pair to files"
  (unless (crypto-key-p key)
    (error "Invalid key object"))
  (unless (x509-certificate-p cert)
    (error "Invalid certificate object"))
  
  ;; Save the private key
  (with-open-file (stream key-file :direction :output :if-exists :supersede)
    (write-string (key-to-pem key :private-p t) stream))
  
  ;; Save the certificate
  (with-open-file (stream cert-file :direction :output :if-exists :supersede)
    (write-string (save-certificate cert) stream)))

(defun load-key-and-cert-pair (key-file cert-file)
  "Load a key and certificate pair from files"
  ;; Load the private key
  (let ((key-pem (with-open-file (stream key-file :direction :input)
                   (let ((content (make-string (file-length stream))))
                     (read-sequence content stream)
                     content)))
        (cert-pem (with-open-file (stream cert-file :direction :input)
                    (let ((content (make-string (file-length stream))))
                      (read-sequence content stream)
                      content))))
    (values (key-from-pem key-pem :private-p t)
            (load-certificate cert-pem))))