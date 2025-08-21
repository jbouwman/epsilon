;;;; Certificate Generation and Management
;;;;
;;;; Utilities for creating and managing X.509 certificates,
;;;; including self-signed certificates for development and testing

(defpackage :epsilon.crypto.certificates
  (:use :cl)
  (:local-nicknames
   (#:openssl3 #:epsilon.crypto.openssl3)
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
   #:make-certificate-pair))

(in-package :epsilon.crypto.certificates)

;;;; FFI Bindings for Certificate Operations
;;;; NOTE: These are now defined in ffi.lisp

#| Moved to ffi.lisp
(lib:defshared %x509-set-version "X509_set_version" "libcrypto" :int
  (x509 :pointer) (version :long))

(lib:defshared %x509-set-serialnumber "X509_set_serialNumber" "libcrypto" :int
  (x509 :pointer) (serial :pointer))

(lib:defshared %x509-set-notbefore "X509_set1_notBefore" "libcrypto" :int
  (x509 :pointer) (tm :pointer))

(lib:defshared %x509-set-notafter "X509_set1_notAfter" "libcrypto" :int
  (x509 :pointer) (tm :pointer))

(lib:defshared %x509-set-pubkey "X509_set_pubkey" "libcrypto" :int
  (x509 :pointer) (pkey :pointer))

(lib:defshared %x509-get-subject-name "X509_get_subject_name" "libcrypto" :pointer
  (x509 :pointer))

(lib:defshared %x509-get-issuer-name "X509_get_issuer_name" "libcrypto" :pointer
  (x509 :pointer))

(lib:defshared %x509-set-subject-name "X509_set_subject_name" "libcrypto" :int
  (x509 :pointer) (name :pointer))

(lib:defshared %x509-set-issuer-name "X509_set_issuer_name" "libcrypto" :int
  (x509 :pointer) (name :pointer))

(lib:defshared %x509-name-add-entry-by-txt "X509_NAME_add_entry_by_txt" "libcrypto" :int
  (name :pointer) (field :string) (type :int) (bytes :pointer) (len :int) (loc :int) (set :int))

(lib:defshared %x509-sign "X509_sign" "libcrypto" :int
  (x509 :pointer) (pkey :pointer) (md :pointer))

(lib:defshared %x509-req-new "X509_REQ_new" "libcrypto" :pointer ())

(lib:defshared %x509-req-free "X509_REQ_free" "libcrypto" :void
  (req :pointer))

(lib:defshared %x509-req-set-version "X509_REQ_set_version" "libcrypto" :int
  (req :pointer) (version :long))

(lib:defshared %x509-req-set-subject-name "X509_REQ_set_subject_name" "libcrypto" :int
  (req :pointer) (name :pointer))

(lib:defshared %x509-req-set-pubkey "X509_REQ_set_pubkey" "libcrypto" :int
  (req :pointer) (pkey :pointer))

(lib:defshared %x509-req-sign "X509_REQ_sign" "libcrypto" :int
  (req :pointer) (pkey :pointer) (md :pointer))

(lib:defshared %x509-req-get-subject-name "X509_REQ_get_subject_name" "libcrypto" :pointer
  (req :pointer))

(lib:defshared %asn1-integer-new "ASN1_INTEGER_new" "libcrypto" :pointer ())

(lib:defshared %asn1-integer-set "ASN1_INTEGER_set" "libcrypto" :int
  (a :pointer) (v :long))

(lib:defshared %asn1-time-new "ASN1_TIME_new" "libcrypto" :pointer ())

(lib:defshared %x509-time-adj-ex "X509_time_adj_ex" "libcrypto" :pointer
  (s :pointer) (offset-day :int) (offset-sec :long) (t_ :pointer))

(lib:defshared %pem-write-bio-x509 "PEM_write_bio_X509" "libcrypto" :int
  (bp :pointer) (x509 :pointer))

(lib:defshared %pem-read-bio-x509 "PEM_read_bio_X509" "libcrypto" :pointer
  (bp :pointer) (x509 :pointer) (cb :pointer) (u :pointer))

(lib:defshared %pem-write-bio-x509-req "PEM_write_bio_X509_REQ" "libcrypto" :int
  (bp :pointer) (req :pointer))

(lib:defshared %pem-read-bio-x509-req "PEM_read_bio_X509_REQ" "libcrypto" :pointer
  (bp :pointer) (req :pointer) (cb :pointer) (u :pointer))

(lib:defshared %x509-verify "X509_verify" "libcrypto" :int
  (x509 :pointer) (pkey :pointer))

(lib:defshared %x509-req-verify "X509_REQ_verify" "libcrypto" :int
  (req :pointer) (pkey :pointer))

(lib:defshared %x509-check-private-key "X509_check_private_key" "libcrypto" :int
  (x509 :pointer) (pkey :pointer))

(lib:defshared %x509-extension-create-by-nid "X509_EXTENSION_create_by_NID" "libcrypto" :pointer
  (ex :pointer) (nid :int) (crit :int) (data :pointer))

(lib:defshared %x509-add-ext "X509_add_ext" "libcrypto" :int
  (x509 :pointer) (ex :pointer) (loc :int))

(lib:defshared %x509v3-ext-conf-nid "X509V3_EXT_conf_nid" "libcrypto" :pointer
  (conf :pointer) (ctx :pointer) (ext-nid :int) (value :string))

(lib:defshared %x509-extension-free "X509_EXTENSION_free" "libcrypto" :void
  (ex :pointer))
|#

;;;; Helper Constants

(defconstant +mbstring-asc+ #x1000)  ; ASCII string type for X509_NAME entries
(defconstant +nid-basic-constraints+ 87)
(defconstant +nid-key-usage+ 83)
(defconstant +nid-ext-key-usage+ 126)
(defconstant +nid-subject-alt-name+ 85)
(defconstant +nid-subject-key-identifier+ 82)
(defconstant +nid-authority-key-identifier+ 90)

;;;; Certificate Generation Functions

(defun generate-self-signed-certificate (common-name &key 
                                         (key-bits 2048)
                                         (days 365)
                                         (country "US")
                                         (state "CA")
                                         (locality "San Francisco")
                                         (organization "Test Organization")
                                         (organizational-unit "IT")
                                         (email nil)
                                         (dns-names nil)
                                         (ip-addresses nil))
  "Generate a self-signed X.509 certificate with private key.
   Returns (values certificate-pem private-key-pem)"
  
  ;; Generate RSA key pair
  (let* ((pkey (openssl3:generate-rsa-key key-bits))
         (x509 (ffi:%x509-new))
         (name (ffi:%x509-name-new)))
    
    (unwind-protect
         (progn
           ;; Set certificate version (X509v3)
           (ffi:%x509-set-version x509 2)
           
           ;; Set serial number
           (let ((serial (ffi:%asn1-integer-new)))
             (ffi:%asn1-integer-set serial (random 1000000))
             (ffi:%x509-set-serialnumber x509 serial))
           
           ;; Set validity period
           (let ((not-before (ffi:%asn1-time-new))
                 (not-after (ffi:%asn1-time-new)))
             (ffi:%x509-time-adj-ex not-before 0 0 (sb-sys:int-sap 0))
             (ffi:%x509-time-adj-ex not-after days 0 (sb-sys:int-sap 0))
             (ffi:%x509-set-notbefore x509 not-before)
             (ffi:%x509-set-notafter x509 not-after))
           
           ;; Set subject name
           (add-name-entry name "C" country)
           (add-name-entry name "ST" state)
           (add-name-entry name "L" locality)
           (add-name-entry name "O" organization)
           (add-name-entry name "OU" organizational-unit)
           (add-name-entry name "CN" common-name)
           (when email
             (add-name-entry name "emailAddress" email))
           
           (ffi:%x509-set-subject-name x509 name)
           (ffi:%x509-set-issuer-name x509 name)  ; Self-signed
           
           ;; Set public key
           (ffi:%x509-set-pubkey x509 pkey)
           
           ;; Add extensions for CA certificate if needed
           (add-basic-constraints-extension x509 t)  ; CA:TRUE for self-signed
           (add-key-usage-extension x509 '(:key-cert-sign :crl-sign :digital-signature))
           
           ;; Add Subject Alternative Names if provided
           (when (or dns-names ip-addresses)
             (add-subject-alt-name-extension x509 dns-names ip-addresses))
           
           ;; Sign the certificate
           (let ((md (ffi:%evp-get-digestbyname "SHA256")))
             (when (zerop (ffi:%x509-sign x509 pkey md))
               (error "Failed to sign certificate")))
           
           ;; Convert to PEM format
           (values (certificate-to-pem x509)
                   (private-key-to-pem pkey)))
      
      ;; Cleanup
      (when x509 (ffi:%x509-free x509))
      (when pkey (ffi:%evp-pkey-free pkey))
      (when name (ffi:%x509-name-free name)))))

(defun generate-certificate-request (common-name private-key &key
                                     (country "US")
                                     (state "CA")
                                     (locality "San Francisco")
                                     (organization "Test Organization")
                                     (organizational-unit "IT")
                                     (email nil))
  "Generate a Certificate Signing Request (CSR)"
  (let* ((req (ffi:%x509-req-new))
         (name (ffi:%x509-name-new)))
    
    (unwind-protect
         (progn
           ;; Set version
           (ffi:%x509-req-set-version req 0)
           
           ;; Set subject name
           (add-name-entry name "C" country)
           (add-name-entry name "ST" state)
           (add-name-entry name "L" locality)
           (add-name-entry name "O" organization)
           (add-name-entry name "OU" organizational-unit)
           (add-name-entry name "CN" common-name)
           (when email
             (add-name-entry name "emailAddress" email))
           
           (ffi:%x509-req-set-subject-name req name)
           
           ;; Set public key
           (ffi:%x509-req-set-pubkey req private-key)
           
           ;; Sign the request
           (let ((md (ffi:%evp-get-digestbyname "SHA256")))
             (when (zerop (ffi:%x509-req-sign req private-key md))
               (error "Failed to sign certificate request")))
           
           ;; Convert to PEM
           (csr-to-pem req))
      
      ;; Cleanup
      (when req (ffi:%x509-req-free req))
      (when name (ffi:%x509-name-free name)))))

(defun sign-certificate-request (csr-pem ca-cert-pem ca-key-pem &key (days 365))
  "Sign a CSR with a CA certificate to produce a signed certificate"
  (let* ((csr (pem-to-csr csr-pem))
         (ca-cert (pem-to-certificate ca-cert-pem))
         (ca-key (pem-to-private-key ca-key-pem))
         (x509 (ffi:%x509-new)))
    
    (unwind-protect
         (progn
           ;; Set version
           (ffi:%x509-set-version x509 2)
           
           ;; Set serial number
           (let ((serial (ffi:%asn1-integer-new)))
             (ffi:%asn1-integer-set serial (random 1000000))
             (ffi:%x509-set-serialnumber x509 serial))
           
           ;; Set validity period
           (let ((not-before (ffi:%asn1-time-new))
                 (not-after (ffi:%asn1-time-new)))
             (ffi:%x509-time-adj-ex not-before 0 0 (sb-sys:int-sap 0))
             (ffi:%x509-time-adj-ex not-after days 0 (sb-sys:int-sap 0))
             (ffi:%x509-set-notbefore x509 not-before)
             (ffi:%x509-set-notafter x509 not-after))
           
           ;; Copy subject from CSR
           (let ((subject (ffi:%x509-req-get-subject-name csr)))
             (ffi:%x509-set-subject-name x509 subject))
           
           ;; Set issuer from CA certificate
           (let ((issuer (ffi:%x509-get-issuer-name ca-cert)))
             (ffi:%x509-set-issuer-name x509 issuer))
           
           ;; Get public key from CSR
           (let ((req-pubkey (ffi:%x509-req-get-pubkey csr)))
             (ffi:%x509-set-pubkey x509 req-pubkey))
           
           ;; Add extensions
           (add-basic-constraints-extension x509 nil)  ; CA:FALSE for end-entity
           (add-key-usage-extension x509 '(:digital-signature :key-encipherment))
           (add-ext-key-usage-extension x509 '(:server-auth :client-auth))
           
           ;; Sign with CA key
           (let ((md (ffi:%evp-get-digestbyname "SHA256")))
             (when (zerop (ffi:%x509-sign x509 ca-key md))
               (error "Failed to sign certificate")))
           
           ;; Convert to PEM
           (certificate-to-pem x509))
      
      ;; Cleanup
      (when csr (ffi:%x509-req-free csr))
      (when ca-cert (ffi:%x509-free ca-cert))
      (when ca-key (ffi:%evp-pkey-free ca-key))
      (when x509 (ffi:%x509-free x509)))))

(defun generate-ca-certificate (common-name &key
                               (key-bits 4096)
                               (days 3650)
                               (country "US")
                               (state "CA")
                               (locality "San Francisco")
                               (organization "Test CA"))
  "Generate a CA certificate suitable for signing other certificates"
  (generate-self-signed-certificate common-name
                                    :key-bits key-bits
                                    :days days
                                    :country country
                                    :state state
                                    :locality locality
                                    :organization organization
                                    :organizational-unit "Certificate Authority"))

;;;; Helper Functions

(defun add-name-entry (name field value)
  "Add an entry to an X509_NAME structure"
  (sb-alien:with-alien ((value-buf (sb-alien:array sb-alien:char 256)))
    (loop for i from 0 below (min (length value) 255)
          do (setf (sb-alien:deref value-buf i) (char-code (char value i))))
    (setf (sb-alien:deref value-buf (min (length value) 255)) 0)
    (ffi:%x509-name-add-entry-by-txt name field +mbstring-asc+ 
                                     (sb-alien:alien-sap value-buf)
                                     -1 -1 0)))

(defun add-basic-constraints-extension (x509 is-ca)
  "Add basicConstraints extension to certificate"
  (let* ((value (if is-ca "CA:TRUE" "CA:FALSE"))
         (ext (ffi:%x509v3-ext-conf-nid (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                        +nid-basic-constraints+ value)))
    (when ext
      (ffi:%x509-add-ext x509 ext -1)
      (ffi:%x509-extension-free ext))))

(defun add-key-usage-extension (x509 usages)
  "Add keyUsage extension to certificate"
  (let* ((usage-strings (mapcar (lambda (u)
                                  (case u
                                    (:digital-signature "digitalSignature")
                                    (:key-encipherment "keyEncipherment")
                                    (:key-cert-sign "keyCertSign")
                                    (:crl-sign "cRLSign")
                                    (t (string-downcase (string u)))))
                                usages))
         (value (format nil "~{~A~^,~}" usage-strings))
         (ext (ffi:%x509v3-ext-conf-nid (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                        +nid-key-usage+ value)))
    (when ext
      (ffi:%x509-add-ext x509 ext -1)
      (ffi:%x509-extension-free ext))))

(defun add-ext-key-usage-extension (x509 usages)
  "Add extendedKeyUsage extension to certificate"
  (let* ((usage-strings (mapcar (lambda (u)
                                  (case u
                                    (:server-auth "serverAuth")
                                    (:client-auth "clientAuth")
                                    (:code-signing "codeSigning")
                                    (:email-protection "emailProtection")
                                    (t (string-downcase (string u)))))
                                usages))
         (value (format nil "~{~A~^,~}" usage-strings))
         (ext (ffi:%x509v3-ext-conf-nid (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                        +nid-ext-key-usage+ value)))
    (when ext
      (ffi:%x509-add-ext x509 ext -1)
      (ffi:%x509-extension-free ext))))

(defun add-subject-alt-name-extension (x509 dns-names ip-addresses)
  "Add subjectAltName extension with DNS names and IP addresses"
  (let* ((alt-names '())
         (dns-entries (mapcar (lambda (dns) (format nil "DNS:~A" dns)) dns-names))
         (ip-entries (mapcar (lambda (ip) (format nil "IP:~A" ip)) ip-addresses))
         (value (format nil "~{~A~^,~}" (append dns-entries ip-entries)))
         (ext (when (> (length value) 0)
                (ffi:%x509v3-ext-conf-nid (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                          +nid-subject-alt-name+ value))))
    (when ext
      (ffi:%x509-add-ext x509 ext -1)
      (ffi:%x509-extension-free ext))))

;;;; PEM Conversion Functions

(defun certificate-to-pem (x509)
  "Convert X509 certificate to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (unwind-protect
         (progn
           (ffi:%pem-write-bio-x509 bio x509)
           (bio-to-string bio))
      (ffi:%bio-free bio))))

(defun private-key-to-pem (pkey)
  "Convert private key to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (unwind-protect
         (progn
           (ffi:%pem-write-bio-privatekey bio pkey 
                                         (sb-sys:int-sap 0) (sb-sys:int-sap 0) 
                                         0 (sb-sys:int-sap 0) (sb-sys:int-sap 0))
           (bio-to-string bio))
      (ffi:%bio-free bio))))

(defun csr-to-pem (req)
  "Convert CSR to PEM string"
  (let ((bio (ffi:%bio-new (ffi:%bio-s-mem))))
    (unwind-protect
         (progn
           (ffi:%pem-write-bio-x509-req bio req)
           (bio-to-string bio))
      (ffi:%bio-free bio))))

(defun pem-to-certificate (pem-string)
  "Convert PEM string to X509 certificate"
  (sb-alien:with-alien ((pem-buf (sb-alien:array sb-alien:char 65536)))
    (loop for i from 0 below (min (length pem-string) 65535)
          do (setf (sb-alien:deref pem-buf i) (char-code (char pem-string i))))
    (setf (sb-alien:deref pem-buf (min (length pem-string) 65535)) 0)
    
    (let ((bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-buf) 
                                     (length pem-string))))
      (unwind-protect
           (ffi:%pem-read-bio-x509 bio (sb-sys:int-sap 0) 
                                   (sb-sys:int-sap 0) (sb-sys:int-sap 0))
        (ffi:%bio-free bio)))))

(defun pem-to-private-key (pem-string)
  "Convert PEM string to private key"
  (sb-alien:with-alien ((pem-buf (sb-alien:array sb-alien:char 65536)))
    (loop for i from 0 below (min (length pem-string) 65535)
          do (setf (sb-alien:deref pem-buf i) (char-code (char pem-string i))))
    (setf (sb-alien:deref pem-buf (min (length pem-string) 65535)) 0)
    
    (let ((bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-buf) 
                                     (length pem-string))))
      (unwind-protect
           (ffi:%pem-read-bio-privatekey bio (sb-sys:int-sap 0) 
                                        (sb-sys:int-sap 0) (sb-sys:int-sap 0))
        (ffi:%bio-free bio)))))

(defun pem-to-csr (pem-string)
  "Convert PEM string to CSR"
  (sb-alien:with-alien ((pem-buf (sb-alien:array sb-alien:char 65536)))
    (loop for i from 0 below (min (length pem-string) 65535)
          do (setf (sb-alien:deref pem-buf i) (char-code (char pem-string i))))
    (setf (sb-alien:deref pem-buf (min (length pem-string) 65535)) 0)
    
    (let ((bio (ffi:%bio-new-mem-buf (sb-alien:alien-sap pem-buf) 
                                     (length pem-string))))
      (unwind-protect
           (ffi:%pem-read-bio-x509-req bio (sb-sys:int-sap 0) 
                                       (sb-sys:int-sap 0) (sb-sys:int-sap 0))
        (ffi:%bio-free bio)))))

(defun bio-to-string (bio)
  "Read string data from BIO"
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 65536)))
    (let ((len (ffi:%bio-ctrl bio 3 0 (sb-alien:alien-sap buf))))  ; BIO_CTRL_INFO
      (let ((result (make-string len)))
        (ffi:%bio-read bio (sb-alien:alien-sap buf) len)
        (loop for i from 0 below len
              do (setf (char result i) (code-char (sb-alien:deref buf i))))
        result))))

;;;; File I/O Functions

(defun save-certificate (certificate-pem filename)
  "Save certificate PEM to file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string certificate-pem stream)))

(defun load-certificate (filename)
  "Load certificate PEM from file"
  (with-open-file (stream filename :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun save-private-key (key-pem filename)
  "Save private key PEM to file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string key-pem stream))
  ;; Set restrictive permissions (owner read/write only)
  #+unix
  (sb-posix:chmod filename #o600))

(defun load-private-key (filename)
  "Load private key PEM from file"
  (with-open-file (stream filename :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

;;;; Utility Functions

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

(defun certificate-info (certificate-pem)
  "Extract information from a certificate"
  (let ((x509 (pem-to-certificate certificate-pem)))
    (unwind-protect
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
               
               (list :subject subject-str
                     :issuer issuer-str))))
      (when x509 (ffi:%x509-free x509)))))

(defun verify-certificate-chain (cert-pem ca-cert-pem)
  "Verify that a certificate was signed by a CA"
  (let ((cert (pem-to-certificate cert-pem))
        (ca-cert (pem-to-certificate ca-cert-pem)))
    (unwind-protect
         (let ((ca-pubkey (ffi:%x509-get-pubkey ca-cert)))
           (= 1 (ffi:%x509-verify cert ca-pubkey)))
      (when cert (ffi:%x509-free cert))
      (when ca-cert (ffi:%x509-free ca-cert)))))
