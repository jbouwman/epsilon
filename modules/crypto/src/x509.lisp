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
   #:crypto-error
   #:+digest-sha256+))

(in-package :epsilon.crypto.x509)

;;;; X.509 Certificate Operations

(defun create-certificate (&key
                          subject
                          issuer
                          (serial 1)
                          (days 365)
                          public-key
                          signing-key)
  "Create a new X.509 certificate"
  (unless (crypto-key-p public-key)
    (error "Invalid public key"))
  
  (unless (crypto-key-p signing-key)
    (error "Invalid signing key"))
  
  (let ((x509 (ffi:%x509-new))
        (success nil))
    (unwind-protect
         (progn
           ;; Set version to X509 v3
           (when (zerop (ffi:%x509-set-version x509 2))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to set certificate version"))
           
           ;; Set serial number
           (let ((serial-asn1 (ffi:%asn1-integer-new)))
             (unwind-protect
                  (progn
                    (ffi:%asn1-integer-set serial-asn1 serial)
                    (when (zerop (ffi:%x509-set-serialnumber x509 serial-asn1))
                      (error 'crypto-error :code (ffi:%err-get-error)
                             :message "Failed to set serial number")))
               (ffi:%asn1-integer-free serial-asn1)))
           
           ;; Set validity period
           (let ((not-before (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) 0 0 (sb-sys:int-sap 0)))
                 (not-after (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) days 0 (sb-sys:int-sap 0))))
             (when (zerop (ffi:%x509-set-notbefore x509 not-before))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to set not-before time"))
             (when (zerop (ffi:%x509-set-notafter x509 not-after))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to set not-after time")))
           
           ;; Set subject name
           (when subject
             (let ((name (ffi:%x509-get-subject-name x509)))
               (loop for (field value) on subject by #'cddr
                     do (sb-alien:with-alien ((value-str sb-alien:c-string :local (string value)))
                          (when (zerop (ffi:%x509-name-add-entry-by-txt 
                                       name (string field) 4097 ; MBSTRING_ASC
                                       (sb-alien:alien-sap value-str) -1 -1 0))
                            (error 'crypto-error :code (ffi:%err-get-error)
                                   :message (format nil "Failed to add subject field: ~A" field)))))))
           
           ;; Set issuer name
           (when issuer
             (let ((name (ffi:%x509-get-issuer-name x509)))
               (loop for (field value) on issuer by #'cddr
                     do (sb-alien:with-alien ((value-str sb-alien:c-string :local (string value)))
                          (when (zerop (ffi:%x509-name-add-entry-by-txt 
                                       name (string field) 4097 ; MBSTRING_ASC
                                       (sb-alien:alien-sap value-str) -1 -1 0))
                            (error 'crypto-error :code (ffi:%err-get-error)
                                   :message (format nil "Failed to add issuer field: ~A" field)))))))
           
           ;; Set public key
           (when (zerop (ffi:%x509-set-pubkey x509 (crypto-key-handle public-key)))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to set public key"))
           
           ;; Sign certificate
           (let ((md (ffi:%evp-get-digestbyname +digest-sha256+)))
             (when (sb-sys:sap= md (sb-sys:int-sap 0))
               (error 'crypto-error :code -1 :message "Unknown digest algorithm"))
             
             (when (zerop (ffi:%x509-sign x509 (crypto-key-handle signing-key) md))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to sign certificate")))
           
           (setf success t)
           
           ;; Extract certificate info
           (let ((subject-name (ffi:%x509-get-subject-name x509))
                 (issuer-name (ffi:%x509-get-issuer-name x509)))
             
             (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
               (let ((subject-str (ffi:%x509-name-oneline subject-name 
                                                      (sb-alien:alien-sap buf) 256))
                     (issuer-str nil))
                 (setf subject-str (sb-alien:cast subject-str sb-alien:c-string))
                 
                 (setf issuer-str (ffi:%x509-name-oneline issuer-name
                                                      (sb-alien:alien-sap buf) 256))
                 (setf issuer-str (sb-alien:cast issuer-str sb-alien:c-string))
                 
                 (make-x509-certificate :handle x509
                                       :subject subject-str
                                       :issuer issuer-str
                                       :serial (format nil "~D" serial)
                                       :not-before 0
                                       :not-after (* days 86400))))))
      ;; Cleanup on error
      (unless success
        (when x509
          (ffi:%x509-free x509))))))

(defun load-certificate (pem-string)
  "Load X.509 certificate from PEM string"
  (sb-alien:with-alien ((pem-cstr sb-alien:c-string :local pem-string))
    (let* ((len (length pem-string))
           (bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-cstr) len))
           (x509 nil))
      (unwind-protect
           (progn
             ;; Read certificate from BIO
             (setf x509 (ffi:%pem-read-bio-x509 bio (sb-sys:int-sap 0)
                                            (sb-sys:int-sap 0) (sb-sys:int-sap 0)))
             
             (when (sb-sys:sap= x509 (sb-sys:int-sap 0))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to read certificate from PEM"))
             
             ;; Extract certificate info
             (let ((subject-name (ffi:%x509-get-subject-name x509))
                   (issuer-name (ffi:%x509-get-issuer-name x509)))
               
               (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
                 (let ((subject-str (ffi:%x509-name-oneline subject-name 
                                                        (sb-alien:alien-sap buf) 256))
                       (issuer-str nil))
                   (setf subject-str (sb-alien:cast subject-str sb-alien:c-string))
                   
                   (setf issuer-str (ffi:%x509-name-oneline issuer-name
                                                        (sb-alien:alien-sap buf) 256))
                   (setf issuer-str (sb-alien:cast issuer-str sb-alien:c-string))
                   
                   (make-x509-certificate :handle x509
                                         :subject subject-str
                                         :issuer issuer-str)))))
        ;; Cleanup
        (progn
          (ffi:%bio-free bio)
          (when (and x509 (sb-sys:sap= x509 (sb-sys:int-sap 0)))
            nil))))))

(defun save-certificate (certificate)
  "Export X.509 certificate to PEM format string"
  (unless (x509-certificate-p certificate)
    (error "Invalid certificate object"))
  
  (let* ((bio (ffi:%bio-new (ffi:%bio-s-mem)))
         (result nil))
    (unwind-protect
         (progn
           ;; Write certificate to BIO
           (when (zerop (ffi:%pem-write-bio-x509 bio (x509-certificate-handle certificate)))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to write certificate"))
           
           ;; Get PEM string from BIO
           (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0)))) ; BIO_CTRL_PENDING = 3
             (when (plusp len)
               (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char #.(expt 2 16))))
                 (let ((bytes-read (ffi:%bio-read bio (sb-alien:alien-sap buf) len)))
                   (when (plusp bytes-read)
                     (setf result (sb-alien:cast 
                                  (sb-alien:alien-sap buf)
                                  (sb-alien:c-string)))))))))
      ;; Cleanup
      (ffi:%bio-free bio))
    result))

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

(defun create-csr (key subject)
  "Create a Certificate Signing Request"
  (unless (crypto-key-p key)
    (error "Invalid key object"))
  
  (let ((req (ffi:%x509-req-new))
        (success nil))
    (unwind-protect
         (progn
           ;; Set public key
           (when (zerop (ffi:%x509-req-set-pubkey req (crypto-key-handle key)))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to set CSR public key"))
           
           ;; Set subject name
           (when subject
             (let ((name (ffi:%x509-req-get-subject-name req)))
               (loop for (field value) on subject by #'cddr
                     do (sb-alien:with-alien ((value-str sb-alien:c-string :local (string value)))
                          (when (zerop (ffi:%x509-name-add-entry-by-txt 
                                       name (string field) 4097 ; MBSTRING_ASC
                                       (sb-alien:alien-sap value-str) -1 -1 0))
                            (error 'crypto-error :code (ffi:%err-get-error)
                                   :message (format nil "Failed to add subject field: ~A" field)))))))
           
           ;; Sign CSR
           (let ((md (ffi:%evp-get-digestbyname +digest-sha256+)))
             (when (sb-sys:sap= md (sb-sys:int-sap 0))
               (error 'crypto-error :code -1 :message "Unknown digest algorithm"))
             
             (when (zerop (ffi:%x509-req-sign req (crypto-key-handle key) md))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to sign CSR")))
           
           (setf success t)
           req)
      ;; Cleanup on error
      (unless success
        (when req
          (ffi:%x509-req-free req))))))

(defun sign-csr (csr-handle issuer subject signing-key &key (days 365) (serial 1))
  "Sign a CSR to create a certificate"
  (let ((x509 (ffi:%x509-new))
        (success nil))
    (unwind-protect
         (progn
           ;; Set version to X509 v3
           (when (zerop (ffi:%x509-set-version x509 2))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to set certificate version"))
           
           ;; Set serial number
           (let ((serial-asn1 (ffi:%asn1-integer-new)))
             (unwind-protect
                  (progn
                    (ffi:%asn1-integer-set serial-asn1 serial)
                    (when (zerop (ffi:%x509-set-serialnumber x509 serial-asn1))
                      (error 'crypto-error :code (ffi:%err-get-error)
                             :message "Failed to set serial number")))
               (ffi:%asn1-integer-free serial-asn1)))
           
           ;; Set validity period
           (let ((not-before (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) 0 0 (sb-sys:int-sap 0)))
                 (not-after (ffi:%x509-time-adj-ex (sb-sys:int-sap 0) days 0 (sb-sys:int-sap 0))))
             (when (zerop (ffi:%x509-set-notbefore x509 not-before))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to set not-before time"))
             (when (zerop (ffi:%x509-set-notafter x509 not-after))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to set not-after time")))
           
           ;; Copy subject from CSR
           (let ((csr-subject (ffi:%x509-req-get-subject-name csr-handle)))
             (when (zerop (ffi:%x509-set-subject-name x509 csr-subject))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to set subject name from CSR")))
           
           ;; Set issuer name
           (when issuer
             (let ((name (ffi:%x509-get-issuer-name x509)))
               (loop for (field value) on issuer by #'cddr
                     do (sb-alien:with-alien ((value-str sb-alien:c-string :local (string value)))
                          (when (zerop (ffi:%x509-name-add-entry-by-txt 
                                       name (string field) 4097 ; MBSTRING_ASC
                                       (sb-alien:alien-sap value-str) -1 -1 0))
                            (error 'crypto-error :code (ffi:%err-get-error)
                                   :message (format nil "Failed to add issuer field: ~A" field)))))))
           
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
           (let ((md (ffi:%evp-get-digestbyname +digest-sha256+)))
             (when (sb-sys:sap= md (sb-sys:int-sap 0))
               (error 'crypto-error :code -1 :message "Unknown digest algorithm"))
             
             (when (zerop (ffi:%x509-sign x509 (crypto-key-handle signing-key) md))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to sign certificate")))
           
           (setf success t)
           
           ;; Create certificate structure
           (let ((subject-name (ffi:%x509-get-subject-name x509))
                 (issuer-name (ffi:%x509-get-issuer-name x509)))
             
             (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
               (let ((subject-str (ffi:%x509-name-oneline subject-name 
                                                      (sb-alien:alien-sap buf) 256))
                     (issuer-str nil))
                 (setf subject-str (sb-alien:cast subject-str sb-alien:c-string))
                 
                 (setf issuer-str (ffi:%x509-name-oneline issuer-name
                                                      (sb-alien:alien-sap buf) 256))
                 (setf issuer-str (sb-alien:cast issuer-str sb-alien:c-string))
                 
                 (make-x509-certificate :handle x509
                                       :subject subject-str
                                       :issuer issuer-str
                                       :serial (format nil "~D" serial)
                                       :not-before 0
                                       :not-after (* days 86400))))))
      ;; Cleanup on error
      (unless success
        (when x509
          (ffi:%x509-free x509))))))

(defun load-csr (pem-string)
  "Load CSR from PEM string"
  (sb-alien:with-alien ((pem-cstr sb-alien:c-string :local pem-string))
    (let* ((len (length pem-string))
           (bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-cstr) len))
           (req nil))
      (unwind-protect
           (progn
             ;; Read CSR from BIO
             (setf req (ffi:%pem-read-bio-x509-req bio (sb-sys:int-sap 0)
                                               (sb-sys:int-sap 0) (sb-sys:int-sap 0)))
             
             (when (sb-sys:sap= req (sb-sys:int-sap 0))
               (error 'crypto-error :code (ffi:%err-get-error)
                      :message "Failed to read CSR from PEM"))
             
             req)
        ;; Cleanup
        (progn
          (ffi:%bio-free bio)
          (when (and req (sb-sys:sap= req (sb-sys:int-sap 0)))
            nil))))))

(defun save-csr (csr-handle)
  "Export CSR to PEM format string"
  (let* ((bio (ffi:%bio-new (ffi:%bio-s-mem)))
         (result nil))
    (unwind-protect
         (progn
           ;; Write CSR to BIO
           (when (zerop (ffi:%pem-write-bio-x509-req bio csr-handle))
             (error 'crypto-error :code (ffi:%err-get-error)
                    :message "Failed to write CSR"))
           
           ;; Get PEM string from BIO
           (let ((len (ffi:%bio-ctrl bio 3 0 (sb-sys:int-sap 0)))) ; BIO_CTRL_PENDING = 3
             (when (plusp len)
               (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char #.(expt 2 16))))
                 (let ((bytes-read (ffi:%bio-read bio (sb-alien:alien-sap buf) len)))
                   (when (plusp bytes-read)
                     (setf result (sb-alien:cast 
                                  (sb-alien:alien-sap buf)
                                  (sb-alien:c-string)))))))))
      ;; Cleanup
      (ffi:%bio-free bio))
    result))

