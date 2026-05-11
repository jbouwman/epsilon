;;;; X.509 Certificate Parsing and Validation (RFC 5280)
;;;;
;;;; Implements X.509v3 certificate parsing, chain building, path validation,
;;;; hostname verification, and certificate generation. Uses the ASN.1 DER
;;;; encoder/decoder and PKCS key encoding modules.

(defpackage epsilon.crypto.x509
  (:use :cl)
  (:import
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.pem pem)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.sha1 sha1)
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.sha512 sha384)
   (epsilon.crypto.sha512 sha512)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ec-p256 ec-p256))
  (:export
   ;; Certificate structure
   #:x509-certificate
   #:x509-cert-version
   #:x509-cert-serial
   #:x509-cert-signature-algorithm
   #:x509-cert-issuer
   #:x509-cert-not-before
   #:x509-cert-not-after
   #:x509-cert-subject
   #:x509-cert-public-key-algorithm
   #:x509-cert-public-key-bytes
   #:x509-cert-public-key-params
   #:x509-cert-extensions
   #:x509-cert-signature
   #:x509-cert-tbs-bytes
   #:x509-cert-raw-issuer-bytes
   #:x509-cert-raw-subject-bytes
   #:x509-cert-ocsp-url
   #:x509-cert-raw-bytes
   ;; Parsing
   #:parse-x509-certificate
   #:parse-x509-pem
   ;; Distinguished name
   #:x509-name
   #:x509-name-entries
   #:x509-name-common-name
   #:x509-name-to-string
   ;; Extensions
   #:x509-extension
   #:x509-ext-oid
   #:x509-ext-critical
   #:x509-ext-value
   #:x509-get-extension
   #:x509-basic-constraints
   #:x509-basic-constraints-ca
   #:x509-basic-constraints-path-len
   #:x509-key-usage
   #:x509-subject-alt-names
   #:parse-extended-key-usage
   #:parse-subject-key-identifier
   #:parse-authority-key-identifier
   #:+oid-extended-key-usage+
   #:+oid-subject-key-identifier+
   #:+oid-authority-key-identifier+
   #:+oid-server-auth+
   #:+oid-ocsp-signing+
   ;; Validation
   #:verify-certificate-signature
   #:verify-certificate-chain
   #:hostname-matches-p
   ;; Certificate generation
   #:make-self-signed-certificate
   #:make-ca-signed-certificate
   #:make-csr
   #:certificate-from-csr
   ;; CSR parsing
   #:parse-csr
   ;; CRL (RFC 5280 §5)
   #:x509-crl
   #:x509-crl-p
   #:x509-crl-version
   #:x509-crl-signature-algorithm
   #:x509-crl-issuer
   #:x509-crl-this-update
   #:x509-crl-next-update
   #:x509-crl-revoked-entries
   #:x509-crl-extensions
   #:x509-crl-signature
   #:x509-crl-tbs-bytes
   #:x509-crl-raw-bytes
   #:x509-crl-entry
   #:x509-crl-entry-p
   #:x509-crl-entry-serial
   #:x509-crl-entry-revocation-date
   #:x509-crl-entry-extensions
   #:make-x509-crl-entry
   #:parse-x509-crl
   #:parse-x509-crl-pem
   #:verify-crl-signature
   #:crl-revokes-p
   #:make-crl
   #:x509-csr
   #:x509-csr-p
   #:x509-csr-version
   #:x509-csr-subject
   #:x509-csr-subject-cn
   #:x509-csr-public-key-algorithm
   #:x509-csr-public-key-params
   #:x509-csr-public-key-bytes
   #:x509-csr-dns-names
   #:x509-csr-signature-algorithm
   #:x509-csr-signature
   #:x509-csr-cri-bytes
   #:x509-csr-raw-bytes
   #:verify-csr-signature
   ;; Trust store
   #:make-trust-store
   #:trust-store-add
   #:trust-store-certificates
   #:load-pem-trust-store
   ;; Time
   #:x509-time
   #:make-x509-time
   #:x509-time-year #:x509-time-month #:x509-time-day
   #:x509-time-hour #:x509-time-minute #:x509-time-second
   #:x509-time-before-p
   #:x509-time-now
   ;; Name
   #:make-x509-name
   ;; Decode error condition
   #:x509-decode-error
   #:x509-decode-error-reason))

(in-package :epsilon.crypto.x509)

;;; ---------------------------------------------------------------------------
;;; Decode error condition
;;; ---------------------------------------------------------------------------

(define-condition x509-decode-error (error)
  ((reason :initarg :reason :reader x509-decode-error-reason))
  (:report (lambda (c s)
             (format s "X.509 decode error: ~A" (x509-decode-error-reason c))))
  (:documentation
   "Signaled when the X.509 certificate parser encounters structurally
invalid certificate encoding. Wraps ASN.1 decode errors and structural
validation failures."))

;;; ---------------------------------------------------------------------------
;;; Well-known OIDs for X.509
;;; ---------------------------------------------------------------------------

(defparameter +oid-common-name+ '(2 5 4 3))
(defparameter +oid-country+ '(2 5 4 6))
(defparameter +oid-locality+ '(2 5 4 7))
(defparameter +oid-state+ '(2 5 4 8))
(defparameter +oid-organization+ '(2 5 4 10))
(defparameter +oid-organizational-unit+ '(2 5 4 11))

;; Extension OIDs
(defparameter +oid-basic-constraints+ '(2 5 29 19))
(defparameter +oid-key-usage+ '(2 5 29 15))
(defparameter +oid-subject-alt-name+ '(2 5 29 17))
(defparameter +oid-authority-key-identifier+ '(2 5 29 35))
(defparameter +oid-subject-key-identifier+ '(2 5 29 14))
(defparameter +oid-extended-key-usage+ '(2 5 29 37))
(defparameter +oid-name-constraints+ '(2 5 29 30))

;; Extended Key Usage OIDs
(defparameter +oid-server-auth+ '(1 3 6 1 5 5 7 3 1))
(defparameter +oid-client-auth+ '(1 3 6 1 5 5 7 3 2))
(defparameter +oid-ocsp-signing+ '(1 3 6 1 5 5 7 3 9))

;; Signature algorithm OIDs (shared OIDs imported from pkcs)
(defparameter +oid-sha384-with-rsa+ '(1 2 840 113549 1 1 12))
(defparameter +oid-sha512-with-rsa+ '(1 2 840 113549 1 1 13))
(defparameter +oid-rsa-pss+ '(1 2 840 113549 1 1 10))
(defparameter +oid-ecdsa-with-sha384+ '(1 2 840 10045 4 3 3))

;;; ---------------------------------------------------------------------------
;;; Time representation
;;; ---------------------------------------------------------------------------

(defstruct (x509-time (:constructor %make-x509-time))
  (year 0 :type fixnum)
  (month 1 :type fixnum)
  (day 1 :type fixnum)
  (hour 0 :type fixnum)
  (minute 0 :type fixnum)
  (second 0 :type fixnum))

(defun make-x509-time (&key (year 0) (month 1) (day 1) (hour 0) (minute 0) (second 0))
  (%make-x509-time :year year :month month :day day
                   :hour hour :minute minute :second second))

(defun x509-time-before-p (a b)
  "Return T if time A is strictly before time B."
  (or (< (x509-time-year a) (x509-time-year b))
      (and (= (x509-time-year a) (x509-time-year b))
           (or (< (x509-time-month a) (x509-time-month b))
               (and (= (x509-time-month a) (x509-time-month b))
                    (or (< (x509-time-day a) (x509-time-day b))
                        (and (= (x509-time-day a) (x509-time-day b))
                             (or (< (x509-time-hour a) (x509-time-hour b))
                                 (and (= (x509-time-hour a) (x509-time-hour b))
                                      (or (< (x509-time-minute a) (x509-time-minute b))
                                          (and (= (x509-time-minute a) (x509-time-minute b))
                                               (< (x509-time-second a) (x509-time-second b)))))))))))))

(defun x509-time-now ()
  "Return the current UTC time as an x509-time."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (make-x509-time :year year :month month :day day
                    :hour hour :minute min :second sec)))

(defun parse-utc-time (bytes)
  "Parse UTCTime (YYMMDDHHMMSSZ) from raw bytes."
  (let ((s (map 'string #'code-char bytes)))
    (let ((yy (parse-integer s :start 0 :end 2))
          (mm (parse-integer s :start 2 :end 4))
          (dd (parse-integer s :start 4 :end 6))
          (hh (parse-integer s :start 6 :end 8))
          (mn (parse-integer s :start 8 :end 10))
          (ss (if (>= (length s) 14)
                  (parse-integer s :start 10 :end 12)
                  0)))
      ;; RFC 5280: YY >= 50 means 19YY, YY < 50 means 20YY
      (make-x509-time :year (if (>= yy 50) (+ 1900 yy) (+ 2000 yy))
                      :month mm :day dd :hour hh :minute mn :second ss))))

(defun parse-generalized-time (bytes)
  "Parse GeneralizedTime (YYYYMMDDHHMMSSZ) from raw bytes."
  (let ((s (map 'string #'code-char bytes)))
    (make-x509-time :year (parse-integer s :start 0 :end 4)
                    :month (parse-integer s :start 4 :end 6)
                    :day (parse-integer s :start 6 :end 8)
                    :hour (parse-integer s :start 8 :end 10)
                    :minute (parse-integer s :start 10 :end 12)
                    :second (if (>= (length s) 16)
                                (parse-integer s :start 12 :end 14)
                                0))))

(defun parse-x509-time-tlv (tlv)
  "Parse a time TLV (UTCTime or GeneralizedTime)."
  (cond
    ((= (asn1:asn1-tlv-tag tlv) asn1:+tag-utc-time+)
     (parse-utc-time (asn1:asn1-tlv-value tlv)))
    ((= (asn1:asn1-tlv-tag tlv) asn1:+tag-generalized-time+)
     (parse-generalized-time (asn1:asn1-tlv-value tlv)))
    (t (error "Unknown time type: tag ~A" (asn1:asn1-tlv-tag tlv)))))

(defun encode-utc-time (time)
  "Encode an x509-time as UTCTime DER bytes."
  (let* ((yy (mod (x509-time-year time) 100))
         (s (format nil "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0DZ"
                    yy (x509-time-month time) (x509-time-day time)
                    (x509-time-hour time) (x509-time-minute time)
                    (x509-time-second time)))
         (bytes (map '(vector (unsigned-byte 8)) #'char-code s))
         (len (length bytes))
         (result (make-array (+ 2 len) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) asn1:+tag-utc-time+)
    (setf (aref result 1) len)
    (replace result bytes :start1 2)
    result))

(defun encode-generalized-time (time)
  "Encode an x509-time as GeneralizedTime DER bytes."
  (let* ((s (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0DZ"
                    (x509-time-year time) (x509-time-month time)
                    (x509-time-day time) (x509-time-hour time)
                    (x509-time-minute time) (x509-time-second time)))
         (bytes (map '(vector (unsigned-byte 8)) #'char-code s))
         (len (length bytes))
         (result (make-array (+ 2 len) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) asn1:+tag-generalized-time+)
    (setf (aref result 1) len)
    (replace result bytes :start1 2)
    result))

(defun encode-x509-time (time)
  "Encode time as UTCTime (for 1950-2049) or GeneralizedTime."
  (if (and (>= (x509-time-year time) 1950) (<= (x509-time-year time) 2049))
      (encode-utc-time time)
      (encode-generalized-time time)))

;;; ---------------------------------------------------------------------------
;;; Distinguished Name (DN)
;;; ---------------------------------------------------------------------------

(defstruct (x509-name (:constructor %make-x509-name))
  "An X.509 distinguished name, a sequence of attribute type-value pairs."
  (entries nil :type list))  ; list of (oid . string) pairs

(defun make-x509-name (&key entries)
  (%make-x509-name :entries entries))

(defun x509-name-common-name (name)
  "Get the Common Name (CN) from a DN, or NIL."
  (let ((entry (assoc +oid-common-name+ (x509-name-entries name) :test #'equal)))
    (when entry (cdr entry))))

(defun oid-short-name (oid)
  "Return a short name for well-known OIDs."
  (cond
    ((equal oid +oid-common-name+) "CN")
    ((equal oid +oid-country+) "C")
    ((equal oid +oid-locality+) "L")
    ((equal oid +oid-state+) "ST")
    ((equal oid +oid-organization+) "O")
    ((equal oid +oid-organizational-unit+) "OU")
    (t (asn1:oid-to-string oid))))

(defun x509-name-to-string (name)
  "Format DN as a readable string (e.g. CN=example.com, O=Example Inc)."
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (entry)
                    (format nil "~A=~A" (oid-short-name (car entry)) (cdr entry)))
                  (x509-name-entries name))))

(defun parse-rdn-sequence (tlv)
  "Parse an RDNSequence (SEQUENCE OF SET OF AttributeTypeAndValue)."
  (let ((entries nil)
        (rdns (asn1:der-decode-sequence-contents tlv)))
    (dolist (rdn rdns)
      ;; Each RDN is a SET OF AttributeTypeAndValue
      (let ((atvs (asn1:der-decode-sequence-contents rdn)))
        (dolist (atv atvs)
          (let* ((atv-children (asn1:der-decode-sequence-contents atv))
                 (oid-tlv (first atv-children))
                 (value-tlv (second atv-children))
                 (oid (asn1:decode-oid-value (asn1:asn1-tlv-value oid-tlv)))
                 (value-str (map 'string #'code-char (asn1:asn1-tlv-value value-tlv))))
            (push (cons oid value-str) entries)))))
    (make-x509-name :entries (nreverse entries))))

(defun encode-rdn-sequence (name)
  "Encode an x509-name as a DER RDNSequence."
  (apply #'asn1:der-encode-sequence
         (mapcar (lambda (entry)
                   ;; Each entry becomes a SET { SEQUENCE { OID, UTF8String } }
                   (asn1:der-encode-set
                    (asn1:der-encode-sequence
                     (asn1:der-encode-oid (car entry))
                     (encode-utf8-string (cdr entry)))))
                 (x509-name-entries name))))

(defun encode-utf8-string (string)
  "Encode a string as DER UTF8String."
  (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code string)))
    (asn1:der-encode-tagged asn1:+tag-utf8-string+ bytes)))

;;; ---------------------------------------------------------------------------
;;; X.509 Extensions
;;; ---------------------------------------------------------------------------

(defstruct (x509-extension (:constructor %make-x509-extension))
  (oid nil :type list)
  (critical nil :type boolean)
  (value #() :type (simple-array (unsigned-byte 8) (*))))

(defun make-x509-extension (&key oid critical value)
  (%make-x509-extension :oid oid :critical critical :value value))

(defun x509-ext-oid (ext) (x509-extension-oid ext))
(defun x509-ext-critical (ext) (x509-extension-critical ext))
(defun x509-ext-value (ext) (x509-extension-value ext))

(defun %parse-extension-list (seq-tlv)
  "Parse a `SEQUENCE OF Extension' TLV (no outer wrapper) into a list
   of `x509-extension'. Used for both TBSCertificate extensions (after
   the [3] EXPLICIT wrapper is stripped) and CRL entry extensions
   (which never have a wrapper)."
  (let ((ext-list (asn1:der-decode-sequence-contents seq-tlv))
        (extensions nil))
    (dolist (ext-tlv ext-list)
      (let* ((children (asn1:der-decode-sequence-contents ext-tlv))
             (oid (asn1:decode-oid-value (asn1:asn1-tlv-value (first children))))
             (critical (and (> (length children) 2)
                           (= (asn1:asn1-tlv-tag (second children)) asn1:+tag-boolean+)
                           (plusp (aref (asn1:asn1-tlv-value (second children)) 0))))
             (value-tlv (if (> (length children) 2)
                           (third children)
                           (second children)))
             (value (asn1:asn1-tlv-value value-tlv)))
        (push (make-x509-extension :oid oid :critical critical :value value)
              extensions)))
    (nreverse extensions)))

(defun parse-extensions (extensions-tlv)
  "Parse the extensions from a TBSCertificate.
   EXTENSIONS-TLV is the context-specific [3] wrapper."
  (let ((inner-seq (first (asn1:der-decode-sequence-contents extensions-tlv))))
    (%parse-extension-list inner-seq)))

(defstruct (x509-certificate (:constructor %make-x509-certificate)
                             (:conc-name x509-cert-))
  (version 3 :type fixnum)               ; v3 = 3
  (serial 0 :type integer)               ; certificate serial number
  (signature-algorithm nil :type list)    ; OID
  (signature-params nil)                  ; algorithm parameters TLV
  (issuer nil)                            ; x509-name
  (not-before nil)                        ; x509-time
  (not-after nil)                         ; x509-time
  (subject nil)                           ; x509-name
  (public-key-algorithm nil :type list)   ; OID
  (public-key-params nil)                 ; algorithm parameters (e.g., curve OID)
  (public-key-bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (extensions nil :type list)             ; list of x509-extension
  (signature #() :type (simple-array (unsigned-byte 8) (*)))
  (tbs-bytes #() :type (simple-array (unsigned-byte 8) (*)))  ; raw TBSCertificate
  ;; DER bytes of the issuer Name SEQUENCE and subject Name SEQUENCE
  ;; exactly as they appear in the certificate. Needed by OCSP to
  ;; compute issuerNameHash (RFC 6960 4.1.1) without re-canonicalizing.
  (raw-issuer-bytes  #() :type (simple-array (unsigned-byte 8) (*)))
  (raw-subject-bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (raw-bytes #() :type (simple-array (unsigned-byte 8) (*))))  ; full certificate DER

(defun x509-get-extension (cert oid)
  "Get an extension from a certificate by OID, or NIL."
  (find oid (x509-cert-extensions cert) :key #'x509-extension-oid :test #'equal))

;; AuthorityInfoAccess extension OID (RFC 5280 4.2.2.1).
(defparameter +oid-authority-info-access+ '(1 3 6 1 5 5 7 1 1))
;; OCSP accessMethod OID (RFC 5280 4.2.2.1).
(defparameter +oid-ad-ocsp+ '(1 3 6 1 5 5 7 48 1))

(defun x509-cert-ocsp-url (cert)
  "Return the OCSP responder URL declared by CERT via its
   AuthorityInfoAccess extension, or NIL if the cert does not carry
   one. Picks the first AccessDescription whose accessMethod is
   id-ad-ocsp and whose accessLocation is a uniformResourceIdentifier
   (GeneralName CHOICE tag [6] IMPLICIT IA5String)."
  (let ((ext (x509-get-extension cert +oid-authority-info-access+)))
    (when ext
      (handler-case
          (let* ((seq (asn1:der-decode (x509-extension-value ext)))
                 (descriptions (asn1:der-decode-sequence-contents seq)))
            (dolist (ad descriptions nil)
              (let* ((fields (asn1:der-decode-sequence-contents ad))
                     (method-tlv (first fields))
                     (location-tlv (second fields))
                     (oid (asn1:decode-oid-value
                           (asn1:asn1-tlv-value method-tlv))))
                (when (and (equal oid +oid-ad-ocsp+)
                           (= (asn1:asn1-tlv-class location-tlv)
                              asn1:+context-specific+)
                           (= (asn1:asn1-tlv-tag location-tlv) 6))
                  (return-from x509-cert-ocsp-url
                    (map 'string #'code-char
                         (asn1:asn1-tlv-value location-tlv)))))))
        (error () nil)))))

;;; ---------------------------------------------------------------------------
;;; Basic Constraints extension parsing
;;; ---------------------------------------------------------------------------

(defstruct (x509-basic-constraints (:constructor %make-x509-basic-constraints))
  (ca nil :type boolean)
  (path-len nil))  ; nil = no limit

(defun parse-basic-constraints (ext)
  "Parse a BasicConstraints extension value."
  (let* ((tlv (asn1:der-decode (x509-extension-value ext)))
         (children (asn1:der-decode-sequence-contents tlv))
         (ca nil)
         (path-len nil))
    (when children
      (let ((first-child (first children)))
        (when (= (asn1:asn1-tlv-tag first-child) asn1:+tag-boolean+)
          (setf ca (plusp (aref (asn1:asn1-tlv-value first-child) 0)))
          (when (> (length children) 1)
            (setf path-len (asn1:decode-der-integer
                           (asn1:asn1-tlv-value (second children)))))))
      (when (and (not ca) (= (asn1:asn1-tlv-tag (first children)) asn1:+tag-integer+))
        (setf path-len (asn1:decode-der-integer
                       (asn1:asn1-tlv-value (first children))))))
    (%make-x509-basic-constraints :ca ca :path-len path-len)))

;;; ---------------------------------------------------------------------------
;;; Key Usage extension parsing
;;; ---------------------------------------------------------------------------

(defun parse-key-usage (ext)
  "Parse a KeyUsage extension value. Returns a list of keyword symbols."
  (let* ((tlv (asn1:der-decode (x509-extension-value ext)))
         (bits (asn1:asn1-tlv-value tlv))
         (unused-bits (aref bits 0))
         (byte0 (if (> (length bits) 1) (aref bits 1) 0))
         (byte1 (if (> (length bits) 2) (aref bits 2) 0))
         (usage nil))
    (declare (ignore unused-bits))
    (when (logbitp 7 byte0) (push :digital-signature usage))
    (when (logbitp 6 byte0) (push :content-commitment usage))
    (when (logbitp 5 byte0) (push :key-encipherment usage))
    (when (logbitp 4 byte0) (push :data-encipherment usage))
    (when (logbitp 3 byte0) (push :key-agreement usage))
    (when (logbitp 2 byte0) (push :key-cert-sign usage))
    (when (logbitp 1 byte0) (push :crl-sign usage))
    (when (logbitp 0 byte0) (push :encipher-only usage))
    (when (logbitp 7 byte1) (push :decipher-only usage))
    (nreverse usage)))

;;; ---------------------------------------------------------------------------
;;; Extended Key Usage extension parsing
;;; ---------------------------------------------------------------------------

(defun parse-extended-key-usage (ext)
  "Parse an ExtendedKeyUsage extension. Returns a list of OID lists."
  (let* ((tlv (asn1:der-decode (x509-extension-value ext)))
         (children (asn1:der-decode-sequence-contents tlv)))
    (mapcar (lambda (child)
              (asn1:decode-oid-value (asn1:asn1-tlv-value child)))
            children)))

;;; ---------------------------------------------------------------------------
;;; Subject Alternative Name (SAN) extension parsing
;;; ---------------------------------------------------------------------------

(defun parse-subject-alt-names (ext)
  "Parse SubjectAlternativeName extension. Returns list of (type . value) pairs.
   Types: :dns, :ip, :email, :uri."
  (let* ((tlv (asn1:der-decode (x509-extension-value ext)))
         (names (asn1:der-decode-sequence-contents tlv))
         (result nil))
    (dolist (name-tlv names)
      (let ((tag (asn1:asn1-tlv-tag name-tlv))
            (value (asn1:asn1-tlv-value name-tlv)))
        (cond
          ;; rfc822Name [1]
          ((= tag 1)
           (push (cons :email (map 'string #'code-char value)) result))
          ;; dNSName [2]
          ((= tag 2)
           (push (cons :dns (map 'string #'code-char value)) result))
          ;; uniformResourceIdentifier [6]
          ((= tag 6)
           (push (cons :uri (map 'string #'code-char value)) result))
          ;; iPAddress [7]
          ((= tag 7)
           (push (cons :ip (format-ip-address value)) result)))))
    (nreverse result)))

(defun format-ip-address (bytes)
  "Format IP address bytes as a string."
  (cond
    ((= (length bytes) 4)  ; IPv4
     (format nil "~D.~D.~D.~D" (aref bytes 0) (aref bytes 1)
             (aref bytes 2) (aref bytes 3)))
    ((= (length bytes) 16) ; IPv6
     (format nil "~{~4,'0X~^:~}"
             (loop for i from 0 below 16 by 2
                   collect (logior (ash (aref bytes i) 8) (aref bytes (1+ i))))))
    (t (format nil "~{~2,'0X~}" (coerce bytes 'list)))))

;;; ---------------------------------------------------------------------------
;;; Certificate parsing
;;; ---------------------------------------------------------------------------

(defun extract-raw-tbs (cert-der)
  "Extract the raw TBSCertificate bytes (including tag+length) from certificate DER.
   The TBSCertificate is the first element of the outer SEQUENCE."
  (let ((pos 0)
        (len (length cert-der)))
    ;; Skip the outer SEQUENCE tag+length
    (when (< pos len)
      (incf pos)  ; skip tag byte
      ;; Read length
      (let ((first-len-byte (aref cert-der pos)))
        (incf pos)
        (if (< first-len-byte 128)
            ;; Short form
            nil  ; pos is already past the outer header
            ;; Long form
            (let ((num-len-bytes (logand first-len-byte #x7F)))
              (incf pos num-len-bytes)))))
    ;; Now pos points to the start of TBSCertificate
    ;; Read TBSCertificate tag+length to determine its end
    (let ((tbs-start pos))
      (incf pos)  ; skip tag
      (let ((first-len-byte (aref cert-der pos)))
        (incf pos)
        (let ((tbs-content-len
                (if (< first-len-byte 128)
                    first-len-byte
                    (let ((num-len-bytes (logand first-len-byte #x7F)))
                      (let ((content-len 0))
                        (loop for i from 0 below num-len-bytes
                              do (setf content-len (logior (ash content-len 8)
                                                          (aref cert-der pos)))
                                 (incf pos))
                        content-len)))))
          (subseq cert-der tbs-start (+ pos tbs-content-len)))))))

(defun parse-x509-certificate (der)
  "Parse a DER-encoded X.509 certificate.
   Returns an x509-certificate structure.
   Signals X509-DECODE-ERROR on malformed input."
  (handler-case (%parse-x509-certificate der)
    (asn1:asn1-decode-error (c)
      (error 'x509-decode-error
             :reason (format nil "ASN.1: ~A" (asn1:asn1-decode-error-reason c))))
    (type-error (c)
      (error 'x509-decode-error
             :reason (format nil "structural: ~A" c)))
    (simple-error (c)
      (error 'x509-decode-error
             :reason (format nil "~A" c)))))

(defun %parse-x509-certificate (der)
  "Inner X.509 parser."
  (let* ((cert-tlv (asn1:der-decode der))
         (cert-children (asn1:der-decode-sequence-contents cert-tlv))
         ;; TBSCertificate
         (tbs-tlv (first cert-children))
         (tbs-children (asn1:der-decode-sequence-contents tbs-tlv))
         ;; signatureAlgorithm
         (sig-alg-tlv (second cert-children))
         (sig-alg-children (asn1:der-decode-sequence-contents sig-alg-tlv))
         (sig-oid (asn1:decode-oid-value (asn1:asn1-tlv-value (first sig-alg-children))))
         (sig-params (when (> (length sig-alg-children) 1) (second sig-alg-children)))
         ;; signatureValue
         (sig-bs (asn1:asn1-tlv-value (third cert-children)))
         (sig-bytes (subseq sig-bs 1))  ; strip unused-bits byte
         ;; Parse TBS fields
         (idx 0)
         ;; Version: explicit [0] tag
         (version 1))
    ;; Check for explicit version tag [0]
    (let ((first-tbs (nth idx tbs-children)))
      (when (and (= (asn1:asn1-tlv-class first-tbs) asn1:+context-specific+)
                 (= (asn1:asn1-tlv-tag first-tbs) 0))
        ;; version is explicitly tagged
        (let* ((inner (if (asn1:asn1-tlv-constructed-p first-tbs)
                         (first (asn1:asn1-tlv-value first-tbs))
                         (asn1:der-decode (asn1:asn1-tlv-value first-tbs)))))
          (setf version (1+ (asn1:decode-der-integer (asn1:asn1-tlv-value inner)))))
        (incf idx)))
    ;; serialNumber
    (let ((serial (asn1:decode-der-integer (asn1:asn1-tlv-value (nth idx tbs-children)))))
      (incf idx)
      ;; signature (AlgorithmIdentifier in TBS, should match outer)
      (incf idx)
      ;; issuer
      (let* ((issuer-tlv (nth idx tbs-children))
             (issuer (parse-rdn-sequence issuer-tlv))
             (raw-issuer (asn1:der-encode issuer-tlv)))
        (incf idx)
        ;; validity
        (let* ((validity-children (asn1:der-decode-sequence-contents (nth idx tbs-children)))
               (not-before (parse-x509-time-tlv (first validity-children)))
               (not-after (parse-x509-time-tlv (second validity-children))))
          (incf idx)
          ;; subject
          (let* ((subject-tlv (nth idx tbs-children))
                 (subject (parse-rdn-sequence subject-tlv))
                 (raw-subject (asn1:der-encode subject-tlv)))
            (incf idx)
            ;; subjectPublicKeyInfo
            (let* ((spki-tlv (nth idx tbs-children))
                   (spki-children (asn1:der-decode-sequence-contents spki-tlv))
                   (pk-alg-children (asn1:der-decode-sequence-contents (first spki-children)))
                   (pk-oid (asn1:decode-oid-value (asn1:asn1-tlv-value (first pk-alg-children))))
                   (pk-params (when (> (length pk-alg-children) 1)
                               (second pk-alg-children)))
                   (pk-bs (asn1:asn1-tlv-value (second spki-children)))
                   (pk-bytes (subseq pk-bs 1)))  ; strip unused-bits byte
              (incf idx)
              ;; extensions: explicit [3] tag
              (let ((extensions nil))
                (loop while (< idx (length tbs-children))
                      for field = (nth idx tbs-children)
                      do (when (and (= (asn1:asn1-tlv-class field) asn1:+context-specific+)
                                    (= (asn1:asn1-tlv-tag field) 3))
                           (setf extensions (parse-extensions field)))
                         (incf idx))
                ;; Extract raw TBS bytes
                (let ((tbs-bytes (extract-raw-tbs der)))
                  (%make-x509-certificate
                   :version version
                   :serial serial
                   :signature-algorithm sig-oid
                   :signature-params sig-params
                   :issuer issuer
                   :not-before not-before
                   :not-after not-after
                   :subject subject
                   :public-key-algorithm pk-oid
                   :public-key-params pk-params
                   :public-key-bytes pk-bytes
                   :extensions extensions
                   :signature sig-bytes
                   :tbs-bytes tbs-bytes
                   :raw-issuer-bytes raw-issuer
                   :raw-subject-bytes raw-subject
                   :raw-bytes der))))))))))

(defun parse-x509-pem (pem-text)
  "Parse a PEM-encoded X.509 certificate."
  (let ((block (pem:pem-decode pem-text)))
    (when (and block (string= (pem:pem-block-label block) "CERTIFICATE"))
      (parse-x509-certificate (pem:pem-block-data block)))))

;;; ---------------------------------------------------------------------------
;;; Certificate signature verification
;;; ---------------------------------------------------------------------------

(defun verify-certificate-signature (cert issuer-cert)
  "Verify that CERT was signed by ISSUER-CERT.
   Returns T if valid, NIL if invalid."
  (let ((sig-alg (x509-cert-signature-algorithm cert))
        (tbs (x509-cert-tbs-bytes cert))
        (sig (x509-cert-signature cert))
        (pk-alg (x509-cert-public-key-algorithm issuer-cert))
        (pk-bytes (x509-cert-public-key-bytes issuer-cert)))
    (cond
      ;; RSA with SHA-256
      ((equal sig-alg pkcs:+oid-sha256-with-rsa+)
       (when (equal pk-alg pkcs:+oid-rsa-encryption+)
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (rsa:rsa-pss-verify pub-key tbs sig :hash :sha256)))))

      ;; RSA with SHA-384
      ((equal sig-alg +oid-sha384-with-rsa+)
       (when (equal pk-alg pkcs:+oid-rsa-encryption+)
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (rsa:rsa-pss-verify pub-key tbs sig :hash :sha384)))))

      ;; RSA with SHA-512
      ((equal sig-alg +oid-sha512-with-rsa+)
       (when (equal pk-alg pkcs:+oid-rsa-encryption+)
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (rsa:rsa-pss-verify pub-key tbs sig :hash :sha512)))))

      ;; ECDSA with SHA-256
      ((equal sig-alg pkcs:+oid-ecdsa-with-sha256+)
       (when (equal pk-alg pkcs:+oid-ec-public-key+)
         ;; Decode public key bytes to P256 point
         (let ((pk-point (ec-p256:p256-point-decode pk-bytes)))
           (when pk-point
             ;; Decode DER signature: SEQUENCE { INTEGER r, INTEGER s }
             (let* ((sig-tlv (asn1:der-decode sig))
                    (sig-children (asn1:der-decode-sequence-contents sig-tlv))
                    (r (asn1:decode-der-integer (asn1:asn1-tlv-value (first sig-children))))
                    (s (asn1:decode-der-integer (asn1:asn1-tlv-value (second sig-children)))))
               (ecdsa:ecdsa-verify pk-point tbs r s))))))

      ;; Ed25519
      ((equal sig-alg pkcs:+oid-ed25519+)
       (when (equal pk-alg pkcs:+oid-ed25519+)
         (ed-sign:ed25519-verify pk-bytes tbs sig)))

      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Certificate chain validation (RFC 5280 Section 6)
;;; ---------------------------------------------------------------------------

(defun %crl-applies-to-issuer-p (crl issuer-cert)
  "Test whether CRL is issued by the holder of ISSUER-CERT and is
   currently signed by that issuer. Only CRLs that pass both checks
   are consulted during chain validation."
  (declare (notinline x509-crl-issuer))
  (and (equal (x509-name-entries (x509-crl-issuer crl))
              (x509-name-entries (x509-cert-subject issuer-cert)))
       (verify-crl-signature crl issuer-cert)))

(defun %cert-revoked-p (cert issuer crls revocation-callback)
  "Decide whether CERT is revoked under ISSUER. Returns
   (values revoked-p reason). A :revoked answer from either CRLS or
   REVOCATION-CALLBACK is sufficient. Other answers, missing data, or
   no sources at all yield (values nil ...)."
  (dolist (crl crls)
    (when (%crl-applies-to-issuer-p crl issuer)
      (when (crl-revokes-p crl (x509-cert-serial cert))
        (return-from %cert-revoked-p
          (values t (format nil "serial ~A in CRL"
                            (x509-cert-serial cert)))))))
  (when revocation-callback
    (let ((status (funcall revocation-callback cert issuer)))
      (when (eq status :revoked)
        (return-from %cert-revoked-p
          (values t "revocation callback returned :revoked")))))
  (values nil nil))

(defun verify-certificate-chain (chain &key trust-store (time (x509-time-now))
                                         (verify-time t) (purpose nil)
                                         (crls nil)
                                         (revocation-callback nil))
  "Verify a certificate chain. CHAIN is a list of certificates from
   leaf to root. TRUST-STORE is a trust-store of root CA certificates.
   PURPOSE, when :tls-server, enforces serverAuth EKU on the leaf.

   Revocation: when CRLS is supplied (a list of `x509-crl' structs),
   each non-root cert is checked against any CRL whose issuer matches
   the cert's issuer; the CRL signature is verified against the
   issuer cert in the chain before the revocation list is consulted.
   When REVOCATION-CALLBACK is supplied, it is called for each
   non-root cert as `(funcall callback cert issuer)' and is expected
   to return :good, :revoked, :unknown, or NIL (no information).
   `:revoked' from either source rejects the chain. `:unknown' and
   NIL are accepted: callers wanting strict revocation enforcement
   should reject those at the call site or pre-filter the
   callback. The OCSP variant is built by
   `epsilon.crypto.ocsp:make-revocation-callback'.

   Returns (values valid-p reason)."
  (when (null chain)
    (return-from verify-certificate-chain (values nil "empty chain")))

  ;; Check time validity for each certificate
  (when verify-time
    (dolist (cert chain)
      (when (x509-time-before-p time (x509-cert-not-before cert))
        (return-from verify-certificate-chain
          (values nil (format nil "certificate not yet valid: ~A"
                             (x509-name-to-string (x509-cert-subject cert))))))
      (when (x509-time-before-p (x509-cert-not-after cert) time)
        (return-from verify-certificate-chain
          (values nil (format nil "certificate expired: ~A"
                             (x509-name-to-string (x509-cert-subject cert))))))))

  ;; Check leaf EKU for purpose
  (when (eq purpose :tls-server)
    (let* ((leaf (first chain))
           (eku-ext (x509-get-extension leaf +oid-extended-key-usage+)))
      (when eku-ext
        (let ((ekus (parse-extended-key-usage eku-ext)))
          (unless (member +oid-server-auth+ ekus :test #'equal)
            (return-from verify-certificate-chain
              (values nil "leaf missing serverAuth EKU")))))))

  ;; Verify signature chain
  (loop for i from 0 below (1- (length chain))
        for cert = (nth i chain)
        for issuer = (nth (1+ i) chain)
        do (unless (verify-certificate-signature cert issuer)
             (return-from verify-certificate-chain
               (values nil (format nil "signature verification failed at depth ~D" i))))
           ;; Check that issuer is a CA (RFC 5280 S6.1.4)
           (let ((bc-ext (x509-get-extension issuer +oid-basic-constraints+)))
             (unless bc-ext
               (return-from verify-certificate-chain
                 (values nil (format nil
                              "issuer missing basicConstraints at depth ~D"
                              (1+ i)))))
             (let ((bc (parse-basic-constraints bc-ext)))
               (unless (x509-basic-constraints-ca bc)
                 (return-from verify-certificate-chain
                   (values nil (format nil
                                "non-CA issuer at depth ~D" (1+ i)))))
               ;; Check path length constraint
               (when (and (x509-basic-constraints-path-len bc)
                         (> i (x509-basic-constraints-path-len bc)))
                 (return-from verify-certificate-chain
                   (values nil (format nil
                                "pathLen exceeded at depth ~D"
                                (1+ i)))))))
           ;; Check keyUsage includes keyCertSign when present
           (let ((ku-ext (x509-get-extension issuer +oid-key-usage+)))
             (when ku-ext
               (unless (member :key-cert-sign (parse-key-usage ku-ext))
                 (return-from verify-certificate-chain
                   (values nil (format nil
                                "issuer missing keyCertSign at depth ~D"
                                (1+ i))))))))

  ;; Revocation check (optional). Each non-root cert is consulted
  ;; against the supplied CRLs and/or the OCSP-style callback. A
  ;; :revoked answer rejects the chain. Missing/unknown answers are
  ;; accepted -- strict policies should pre-filter.
  (when (or crls revocation-callback)
    (loop for i from 0 below (1- (length chain))
          for cert = (nth i chain)
          for issuer = (nth (1+ i) chain)
          do (multiple-value-bind (revoked reason)
                 (%cert-revoked-p cert issuer crls revocation-callback)
               (when revoked
                 (return-from verify-certificate-chain
                   (values nil
                           (format nil
                                   "certificate revoked at depth ~D: ~A"
                                   i reason)))))))

  ;; Verify root against trust store (if provided)
  (when trust-store
    (let ((root (car (last chain))))
      (unless (find-trusted-issuer root trust-store)
        (return-from verify-certificate-chain
          (values nil "root certificate not in trust store")))))

  (values t nil))

(defstruct (trust-store (:constructor %make-trust-store))
  (certificates nil :type list))

(defun make-trust-store ()
  (%make-trust-store))

(defun parse-authority-key-identifier (ext)
  "Parse AuthorityKeyIdentifier. Returns key identifier bytes or NIL."
  (let* ((tlv (asn1:der-decode (x509-extension-value ext)))
         (children (asn1:der-decode-sequence-contents tlv)))
    (dolist (child children)
      (when (and (= (asn1:asn1-tlv-class child) asn1:+context-specific+)
                 (= (asn1:asn1-tlv-tag child) 0))
        (return (asn1:asn1-tlv-value child))))))

(defun parse-subject-key-identifier (ext)
  "Parse SubjectKeyIdentifier. Returns the key identifier bytes."
  (asn1:asn1-tlv-value (asn1:der-decode (x509-extension-value ext))))

(defun issuer-matches-p (cert issuer)
  "Check if ISSUER could be the issuer of CERT.
   Uses AKI/SKI matching when both extensions are present,
   falls back to DN matching otherwise."
  (let ((aki-ext (x509-get-extension cert +oid-authority-key-identifier+))
        (ski-ext (x509-get-extension issuer +oid-subject-key-identifier+)))
    (if (and aki-ext ski-ext)
        (let ((aki (parse-authority-key-identifier aki-ext))
              (ski (parse-subject-key-identifier ski-ext)))
          (and aki ski (equalp aki ski)))
        ;; Fallback: DN matching
        (equal (x509-name-entries (x509-cert-subject issuer))
               (x509-name-entries (x509-cert-issuer cert))))))

(defun find-trusted-issuer (cert trust-store)
  "Find a trusted issuer for CERT in the trust store."
  (dolist (trusted (trust-store-certificates trust-store))
    (when (and (issuer-matches-p cert trusted)
              (verify-certificate-signature cert trusted))
      (return trusted))))

;;; ---------------------------------------------------------------------------
;;; Hostname verification (RFC 6125)
;;; ---------------------------------------------------------------------------

(defun hostname-matches-p (cert hostname)
  "Check if HOSTNAME matches the certificate's SANs or CN.
   Implements RFC 6125 wildcard matching."
  (let ((san-ext (x509-get-extension cert +oid-subject-alt-name+)))
    (if san-ext
        ;; If SANs exist, check only SANs (RFC 6125 Section 6.4.4)
        (let ((sans (parse-subject-alt-names san-ext)))
          (dolist (san sans nil)
            (when (and (eq (car san) :dns)
                      (hostname-matches-pattern-p hostname (cdr san)))
              (return t))))
        ;; Fallback to CN (deprecated but still used)
        (let ((cn (x509-name-common-name (x509-cert-subject cert))))
          (when cn
            (hostname-matches-pattern-p hostname cn))))))

(defun hostname-matches-pattern-p (hostname pattern)
  "Check if HOSTNAME matches PATTERN, supporting wildcard matching.
   Only left-most label wildcards are supported. Rejects embedded NUL
   bytes and wildcards at public-suffix boundaries."
  ;; Reject embedded NUL bytes (SAN NUL injection)
  (when (or (find (code-char 0) hostname)
            (find (code-char 0) pattern))
    (return-from hostname-matches-pattern-p nil))
  (let ((hostname (string-downcase hostname))
        (pattern (string-downcase pattern)))
    (cond
      ;; Exact match
      ((string= hostname pattern) t)
      ;; Wildcard: *.example.com
      ((and (>= (length pattern) 3)
            (char= (char pattern 0) #\*)
            (char= (char pattern 1) #\.))
       (let ((suffix (subseq pattern 1)))  ; .example.com
         (let ((dot-pos (position #\. hostname)))
           (and dot-pos
                (string= (subseq hostname dot-pos) suffix)
                ;; Wildcard must not span dots (RFC 6125 S6.4.3)
                (not (find #\. hostname :end dot-pos))
                ;; At least 2 labels in suffix
                (find #\. suffix :start 1)
                ;; Reject wildcards at public-suffix level
                ;; (require >= 2 dots in suffix, e.g. *.x.y)
                (>= (count #\. suffix) 2)))))
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Subject Alternative Name convenience
;;; ---------------------------------------------------------------------------

(defun x509-subject-alt-names (cert)
  "Get all Subject Alternative Names from a certificate.
   Returns list of (type . value) pairs, or NIL."
  (let ((ext (x509-get-extension cert +oid-subject-alt-name+)))
    (when ext (parse-subject-alt-names ext))))

(defun x509-basic-constraints (cert)
  "Get BasicConstraints from a certificate, or NIL."
  (let ((ext (x509-get-extension cert +oid-basic-constraints+)))
    (when ext (parse-basic-constraints ext))))

(defun x509-key-usage (cert)
  "Get KeyUsage from a certificate as a list of keywords, or NIL."
  (let ((ext (x509-get-extension cert +oid-key-usage+)))
    (when ext (parse-key-usage ext))))

(defun trust-store-add (store cert)
  "Add a certificate to the trust store."
  (push cert (trust-store-certificates store))
  store)

(defun load-pem-trust-store (pem-text)
  "Load a trust store from PEM text containing multiple certificates."
  (let ((store (make-trust-store))
        (blocks (pem:pem-decode-all pem-text)))
    (dolist (block blocks)
      (when (string= (pem:pem-block-label block) "CERTIFICATE")
        (let ((cert (parse-x509-certificate (pem:pem-block-data block))))
          (trust-store-add store cert))))
    store))

;;; ---------------------------------------------------------------------------
;;; Certificate generation
;;; ---------------------------------------------------------------------------

(defun %public-key-sha1 (public-key-bytes)
  "Compute the RFC 5280 §4.2.1.2 method (1) keyIdentifier: 160-bit
SHA-1 of the SubjectPublicKey BIT STRING value, NOT including the
leading `unused bits' byte. Our emitters wrap PUBLIC-KEY-BYTES into a
BIT STRING with unused-bits=0, so the input here is exactly the bytes
we want to hash."
  (sha1:sha1 public-key-bytes))

(defun %encode-subject-key-identifier (key-id-bytes)
  "Build a SubjectKeyIdentifier extension TLV (non-critical)."
  (asn1:der-encode-sequence
   (asn1:der-encode-oid +oid-subject-key-identifier+)
   (asn1:der-encode-octet-string
    (asn1:der-encode-octet-string key-id-bytes))))

(defun %encode-authority-key-identifier (key-id-bytes)
  "Build an AuthorityKeyIdentifier extension TLV (non-critical) carrying
just the keyIdentifier [0] field. We omit authorityCertIssuer/SerialNumber
because RFC 5280 §4.2.1.1 says the keyIdentifier-only form is
sufficient and recommended for chain construction."
  (let* ((inner (asn1:der-encode-context 0 key-id-bytes))
         (seq   (asn1:der-encode-sequence inner)))
    (asn1:der-encode-sequence
     (asn1:der-encode-oid +oid-authority-key-identifier+)
     (asn1:der-encode-octet-string seq))))

(defun %issuer-key-identifier (ca-cert)
  "Return the issuer's keyIdentifier bytes for use in an
AuthorityKeyIdentifier extension on a child cert. Prefers the issuer's
own SubjectKeyIdentifier if present (the canonical path); falls back to
SHA-1 of the issuer's public key bytes when the issuer was minted by
an older code path that did not emit SKI. The fallback keeps a chain
from a sparse legacy root from breaking the moment we start emitting
AKI on children."
  (let ((ext (x509-get-extension ca-cert +oid-subject-key-identifier+)))
    (if ext
        (parse-subject-key-identifier ext)
        (%public-key-sha1 (x509-cert-public-key-bytes ca-cert)))))

(defun make-self-signed-certificate (&key subject
                                          (serial 1)
                                          (not-before (x509-time-now))
                                          (not-after (make-x509-time :year (+ (x509-time-year (x509-time-now)) 1)
                                                                     :month (x509-time-month (x509-time-now))
                                                                     :day (x509-time-day (x509-time-now))))
                                          key-type
                                          private-key
                                          public-key-bytes
                                          (is-ca nil)
                                          (dns-names nil)
                                          (key-usage nil))
  "Generate a self-signed X.509v3 certificate.
   KEY-TYPE: :ed25519, :ecdsa-p256, or :rsa
   KEY-USAGE: when non-NIL, a list of keywords selecting KeyUsage
   bits to assert in the certificate (as a critical extension).
   Supported: :digital-signature, :key-encipherment, :key-cert-sign,
   :crl-sign. Modern OpenSSL refuses to use an ECDHE_ECDSA server
   certificate for signing unless keyUsage.digitalSignature is
   explicitly set, so leaf server certs should pass at least
   (:digital-signature). CA certs SHOULD include :key-cert-sign
   (and :crl-sign if they issue CRLs); Apple's SecTrust returns
   CSSMERR_TP_NOT_TRUSTED for a CA cert lacking keyCertSign even
   when the chain is otherwise valid.
   Returns DER-encoded certificate bytes."
  (let* ((subject-name (if (typep subject 'x509-name)
                          subject
                          (make-x509-name :entries (list (cons +oid-common-name+ subject)))))
         ;; Determine algorithm OID and encode public key
         (alg-oid (ecase key-type
                    (:ed25519 pkcs:+oid-ed25519+)
                    (:ecdsa-p256 pkcs:+oid-ecdsa-with-sha256+)
                    (:rsa pkcs:+oid-sha256-with-rsa+)))
         (pk-alg-oid (ecase key-type
                       (:ed25519 pkcs:+oid-ed25519+)
                       (:ecdsa-p256 pkcs:+oid-ec-public-key+)
                       (:rsa pkcs:+oid-rsa-encryption+)))
         ;; Build SubjectPublicKeyInfo
         (spki-alg-id (ecase key-type
                        (:ed25519
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)))
                        (:ecdsa-p256
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)
                          (asn1:der-encode-oid pkcs:+oid-prime256v1+)))
                        (:rsa
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)
                          (asn1:der-encode-null)))))
         ;; Build extensions
         (extensions
          (let ((ext-list nil))
            ;; Basic Constraints (RFC 5280 §4.2.1.9). MUST be present
            ;; and critical on CA certs.  For end-entity certs RFC 5280
            ;; makes the extension optional, so we omit it entirely:
            ;; the previous "critical=FALSE explicit + empty inner
            ;; SEQUENCE" encoding is non-canonical DER and triggers
            ;; OpenSSL 3.x's `ossl_x509v3_cache_extensions' to mark
            ;; the cert EXFLAG_INVALID, which surfaces as opaque
            ;; "invalid certificate" rejections in strict consumers
            ;; like Twisted/pyOpenSSL's `use_certificate_chain_file'
            ;; (synapse hit this on 2026-04-26, see /var/lib/matrix-tls).
            (when is-ca
              (push (asn1:der-encode-sequence
                     (asn1:der-encode-oid +oid-basic-constraints+)
                     (asn1:der-encode-boolean t)
                     (asn1:der-encode-octet-string
                      (asn1:der-encode-sequence
                       (asn1:der-encode-boolean t))))
                    ext-list))
            ;; Key Usage (critical) when requested. RFC 5280 §4.2.1.3
            ;; numbers bits from the MSB of the first byte:
            ;; 0=digitalSignature, 2=keyEncipherment, 5=keyCertSign,
            ;; 6=cRLSign. We compose a single byte and emit a BIT
            ;; STRING whose `unused-bits' count equals the position
            ;; (from the LSB) of the lowest set bit, so the highest
            ;; meaningful bit lines up with its RFC slot.
            (when key-usage
              (let* ((ds (member :digital-signature key-usage))
                     (ke (member :key-encipherment key-usage))
                     (cs (member :key-cert-sign key-usage))
                     (crl (member :crl-sign key-usage))
                     (byte (logior (if ds #x80 0)
                                   (if ke #x20 0)
                                   (if cs #x04 0)
                                   (if crl #x02 0)))
                     (unused (if (zerop byte)
                                 8
                                 (loop for i from 0 below 8
                                       when (logbitp i byte)
                                         return i)))
                     (ku-bitstring (let ((b (make-array 2
                                             :element-type '(unsigned-byte 8))))
                                     (setf (aref b 0) unused)
                                     (setf (aref b 1) byte)
                                     b)))
                (push (asn1:der-encode-sequence
                       (asn1:der-encode-oid +oid-key-usage+)
                       (asn1:der-encode-boolean t)   ; critical
                       (asn1:der-encode-octet-string
                        (let ((bs (make-array (+ 2 (length ku-bitstring))
                                              :element-type '(unsigned-byte 8))))
                          (setf (aref bs 0) #x03)    ; BIT STRING tag
                          (setf (aref bs 1) (length ku-bitstring))
                          (replace bs ku-bitstring :start1 2)
                          bs)))
                      ext-list)))
            ;; SubjectKeyIdentifier on CA certs (RFC 5280 §4.2.1.2:
            ;; "MUST appear in all conforming CA certificates"). Apple's
            ;; SecTrust returns CSSMERR_TP_NOT_TRUSTED when the chain's
            ;; intermediate lacks SKI -- it cannot match a child's
            ;; AuthorityKeyIdentifier and falls off the trusted path.
            (when is-ca
              (push (%encode-subject-key-identifier
                     (%public-key-sha1 public-key-bytes))
                    ext-list))
            ;; Subject Alternative Names
            (when dns-names
              (push (asn1:der-encode-sequence
                     (asn1:der-encode-oid +oid-subject-alt-name+)
                     (asn1:der-encode-octet-string
                      (apply #'asn1:der-encode-sequence
                             (mapcar (lambda (name)
                                       (asn1:der-encode-context
                                        2 (map '(vector (unsigned-byte 8))
                                               #'char-code name)))
                                     dns-names))))
                    ext-list))
            (nreverse ext-list)))
         ;; Build TBSCertificate
         (tbs (apply #'asn1:der-encode-sequence
                     ;; version [0] EXPLICIT INTEGER (v3 = 2)
                     (asn1:der-encode-context 0 (asn1:der-encode-integer 2)
                                              :constructed t)
                     ;; serialNumber
                     (asn1:der-encode-integer serial)
                     ;; signature AlgorithmIdentifier
                     (ecase key-type
                       (:ed25519
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:ecdsa-p256
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:rsa
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)
                         (asn1:der-encode-null))))
                     ;; issuer (self-signed: same as subject)
                     (encode-rdn-sequence subject-name)
                     ;; validity
                     (asn1:der-encode-sequence
                      (encode-x509-time not-before)
                      (encode-x509-time not-after))
                     ;; subject
                     (encode-rdn-sequence subject-name)
                     ;; subjectPublicKeyInfo
                     (asn1:der-encode-sequence
                      spki-alg-id
                      (asn1:der-encode-bit-string public-key-bytes))
                     ;; extensions [3] EXPLICIT
                     (when extensions
                       (list (asn1:der-encode-context
                              3 (apply #'asn1:der-encode-sequence extensions)
                              :constructed t)))))
         ;; Sign TBS
         (signature (sign-tbs tbs key-type private-key))
         ;; Build full certificate
         (sig-alg-id (ecase key-type
                       (:ed25519
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:ecdsa-p256
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:rsa
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)
                         (asn1:der-encode-null))))))
    (asn1:der-encode-sequence
     tbs
     sig-alg-id
     (asn1:der-encode-bit-string signature))))

(defun sign-tbs (tbs key-type private-key)
  "Sign the TBSCertificate bytes with the given key.
   Returns the signature as a byte array."
  (ecase key-type
    (:ed25519
     (ed-sign:ed25519-sign private-key tbs))
    (:ecdsa-p256
     ;; ECDSA returns (values r s) -- encode as DER SEQUENCE { INTEGER r, INTEGER s }
     (multiple-value-bind (r s) (ecdsa:ecdsa-sign private-key tbs)
       (asn1:der-encode-sequence
        (asn1:der-encode-integer r)
        (asn1:der-encode-integer s))))
    (:rsa
     (rsa:rsa-pss-sign private-key tbs :hash :sha256))))

(defun make-ca-signed-certificate (&key subject serial
                                        (not-before (x509-time-now))
                                        not-after
                                        key-type
                                        public-key-bytes
                                        ca-private-key
                                        ca-key-type
                                        ca-cert
                                        (dns-names nil)
                                        (is-ca nil)
                                        (key-usage nil)
                                        (extended-key-usage nil))
  "Generate a CA-signed X.509v3 certificate.
   IS-CA, when true, emits a basicConstraints extension with cA=TRUE
   (the right setting for issuing an intermediate CA). Leaf certificates
   keep the default cA=FALSE.
   KEY-USAGE: list of keywords selecting Key Usage bits (RFC 5280 §4.2.1.3),
   marked critical. Supported: :digital-signature, :key-encipherment,
   :key-cert-sign, :crl-sign. TLS server leaf certs MUST include
   :digital-signature for ECDSA cipher suites to negotiate, per RFC
   8446 §4.4.3. Intermediate CA certs SHOULD include :key-cert-sign --
   Apple's SecTrust returns CSSMERR_TP_NOT_TRUSTED for a chain whose
   intermediate lacks keyCertSign.
   EXTENDED-KEY-USAGE: list of purpose keywords (RFC 5280 §4.2.1.12),
   non-critical. Supported: :server-auth. Firefox 90+ and Chrome 110+
   refuse to use a TLS leaf cert that lacks `id-kp-serverAuth' EKU even
   when the chain is otherwise trusted; this is the visible symptom when
   the browser reports `Peer's Certificate issuer is not recognized'
   despite the root being installed.
   Returns DER-encoded certificate bytes."
  (let* ((subject-name (if (typep subject 'x509-name)
                          subject
                          (make-x509-name :entries (list (cons +oid-common-name+ subject)))))
         ;; Pass the issuer's raw DN bytes through verbatim instead of
         ;; re-encoding the parsed structure. RFC 5280 §4.1.2.4 lets
         ;; CAs use either PrintableString or UTF8String for X.500
         ;; attribute values, but Apple's SecTrust path-builder does
         ;; byte-exact Issuer/Subject DN comparison and rejects a chain
         ;; whose intermediate's Issuer (UTF8String, our emitter's
         ;; default for new strings) differs from the root's Subject
         ;; (PrintableString, the existing root cert's encoding) -- even though the
         ;; canonical names are equal and openssl accepts the chain.
         ;; The raw bytes captured at parse time are the canonical
         ;; source: using them keeps the encoding stable up the chain.
         (issuer-bytes (x509-cert-raw-subject-bytes ca-cert))
         (alg-oid (ecase ca-key-type
                    (:ed25519 pkcs:+oid-ed25519+)
                    (:ecdsa-p256 pkcs:+oid-ecdsa-with-sha256+)
                    (:rsa pkcs:+oid-sha256-with-rsa+)))
         (pk-alg-oid (ecase key-type
                       (:ed25519 pkcs:+oid-ed25519+)
                       (:ecdsa-p256 pkcs:+oid-ec-public-key+)
                       (:rsa pkcs:+oid-rsa-encryption+)))
         (spki-alg-id (ecase key-type
                        (:ed25519
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)))
                        (:ecdsa-p256
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)
                          (asn1:der-encode-oid pkcs:+oid-prime256v1+)))
                        (:rsa
                         (asn1:der-encode-sequence
                          (asn1:der-encode-oid pk-alg-oid)
                          (asn1:der-encode-null)))))
         ;; Extensions.  See `make-self-signed-certificate' for the
         ;; full rationale -- end-entity certs MUST NOT emit a
         ;; BasicConstraints with `critical=TRUE + cA absent', because
         ;; OpenSSL 3.x (`ossl_x509v3_cache_extensions') flags the cert
         ;; EXFLAG_INVALID and strict consumers (synapse via
         ;; `use_certificate_chain_file', 2026-04-26) reject it.
         ;; Issued leaf certs were silently malformed for months
         ;; because this emitter and the self-signed one diverged.
         (extensions
          (let ((ext-list nil))
            (when is-ca
              (push (asn1:der-encode-sequence
                     (asn1:der-encode-oid +oid-basic-constraints+)
                     (asn1:der-encode-boolean t)  ; critical
                     (asn1:der-encode-octet-string
                      (asn1:der-encode-sequence
                       (asn1:der-encode-boolean t))))
                    ext-list))
            ;; Key Usage (critical) when requested. Mirrors the bit
            ;; layout in `make-self-signed-certificate' so the two
            ;; emitters stay byte-for-byte interchangeable.
            (when key-usage
              (let* ((ds (member :digital-signature key-usage))
                     (ke (member :key-encipherment key-usage))
                     (cs (member :key-cert-sign key-usage))
                     (crl (member :crl-sign key-usage))
                     (byte (logior (if ds #x80 0)
                                   (if ke #x20 0)
                                   (if cs #x04 0)
                                   (if crl #x02 0)))
                     (unused (if (zerop byte)
                                 8
                                 (loop for i from 0 below 8
                                       when (logbitp i byte)
                                         return i)))
                     (ku-bitstring (let ((b (make-array 2
                                             :element-type '(unsigned-byte 8))))
                                     (setf (aref b 0) unused)
                                     (setf (aref b 1) byte)
                                     b)))
                (push (asn1:der-encode-sequence
                       (asn1:der-encode-oid +oid-key-usage+)
                       (asn1:der-encode-boolean t)
                       (asn1:der-encode-octet-string
                        (let ((bs (make-array (+ 2 (length ku-bitstring))
                                              :element-type '(unsigned-byte 8))))
                          (setf (aref bs 0) #x03)
                          (setf (aref bs 1) (length ku-bitstring))
                          (replace bs ku-bitstring :start1 2)
                          bs)))
                      ext-list)))
            ;; Extended Key Usage (non-critical) for TLS server leaves.
            ;; Without serverAuth here, Firefox and Chrome reject the
            ;; cert with "issuer not recognized" even though the chain
            ;; is otherwise trusted.
            (when extended-key-usage
              (push (asn1:der-encode-sequence
                     (asn1:der-encode-oid +oid-extended-key-usage+)
                     (asn1:der-encode-octet-string
                      (apply #'asn1:der-encode-sequence
                             (mapcar (lambda (purpose)
                                       (asn1:der-encode-oid
                                        (ecase purpose
                                          (:server-auth +oid-server-auth+))))
                                     extended-key-usage))))
                    ext-list))
            ;; SubjectKeyIdentifier on CA-signed CA certs (intermediates).
            ;; Mirrors the self-signed branch -- see comment there.
            (when is-ca
              (push (%encode-subject-key-identifier
                     (%public-key-sha1 public-key-bytes))
                    ext-list))
            ;; AuthorityKeyIdentifier on every CA-signed cert. Lets
            ;; Apple SecTrust and mozilla::pkix link the child to a
            ;; specific issuer key (not just the issuer DN), which is
            ;; required when an issuer rolls its key but keeps the
            ;; same DN (intermediate rotation is the obvious case).
            ;; Without AKI, Apple returns CSSMERR_TP_NOT_TRUSTED on
            ;; chains that look fine to openssl but fail strict
            ;; SecTrust path-building.
            (push (%encode-authority-key-identifier
                   (%issuer-key-identifier ca-cert))
                  ext-list)
            (when dns-names
              (push (asn1:der-encode-sequence
                     (asn1:der-encode-oid +oid-subject-alt-name+)
                     (asn1:der-encode-octet-string
                      (apply #'asn1:der-encode-sequence
                             (mapcar (lambda (name)
                                       (asn1:der-encode-context
                                        2 (map '(vector (unsigned-byte 8))
                                               #'char-code name)))
                                     dns-names))))
                    ext-list))
            (nreverse ext-list)))
         ;; TBSCertificate
         (tbs (apply #'asn1:der-encode-sequence
                     (asn1:der-encode-context 0 (asn1:der-encode-integer 2)
                                              :constructed t)
                     (asn1:der-encode-integer serial)
                     (ecase ca-key-type
                       (:ed25519 (asn1:der-encode-sequence
                                  (asn1:der-encode-oid alg-oid)))
                       (:ecdsa-p256 (asn1:der-encode-sequence
                                     (asn1:der-encode-oid alg-oid)))
                       (:rsa (asn1:der-encode-sequence
                              (asn1:der-encode-oid alg-oid)
                              (asn1:der-encode-null))))
                     issuer-bytes
                     (asn1:der-encode-sequence
                      (encode-x509-time not-before)
                      (encode-x509-time not-after))
                     (encode-rdn-sequence subject-name)
                     (asn1:der-encode-sequence
                      spki-alg-id
                      (asn1:der-encode-bit-string public-key-bytes))
                     (when extensions
                       (list (asn1:der-encode-context
                              3 (apply #'asn1:der-encode-sequence extensions)
                              :constructed t)))))
         (signature (sign-tbs tbs ca-key-type ca-private-key))
         (sig-alg-id (ecase ca-key-type
                       (:ed25519 (asn1:der-encode-sequence
                                  (asn1:der-encode-oid alg-oid)))
                       (:ecdsa-p256 (asn1:der-encode-sequence
                                     (asn1:der-encode-oid alg-oid)))
                       (:rsa (asn1:der-encode-sequence
                              (asn1:der-encode-oid alg-oid)
                              (asn1:der-encode-null))))))
    (asn1:der-encode-sequence tbs sig-alg-id (asn1:der-encode-bit-string signature))))

;;; ---------------------------------------------------------------------------
;;; PKCS#10 Certificate Signing Request (RFC 2986)
;;; ---------------------------------------------------------------------------

(defun %csr-encode-san-extension (domains)
  "Encode a subjectAltName extension value as DER bytes (GeneralNames SEQUENCE).
Each domain is encoded as a dNSName [2] IMPLICIT IA5String."
  (apply #'asn1:der-encode-sequence
         (mapcar (lambda (domain)
                   (let ((name-bytes (map '(vector (unsigned-byte 8))
                                          #'char-code domain)))
                     (asn1:der-encode-context 2 name-bytes)))
                 domains)))

(defun %csr-encode-extension-request (domains)
  "Encode a PKCS#9 extensionRequest attribute containing subjectAltName.
Returns the DER-encoded Attribute ready for placement in a CRI attributes SET."
  (let* ((ext-request-oid '(1 2 840 113549 1 9 14))  ; PKCS#9 extensionRequest
         (san-oid '(2 5 29 17))                       ; subjectAltName
         (san-der (%csr-encode-san-extension domains))
         (extension (asn1:der-encode-sequence
                     (asn1:der-encode-oid san-oid)
                     (asn1:der-encode-octet-string san-der)))
         (extensions (asn1:der-encode-sequence extension)))
    (asn1:der-encode-sequence
     (asn1:der-encode-oid ext-request-oid)
     (asn1:der-encode-set extensions))))

(defun make-csr (&key key-type private-key subject-cn domains)
  "Build a DER-encoded PKCS#10 CertificationRequest (RFC 2986).

KEY-TYPE is :rsa or :ec-p256. PRIVATE-KEY is the corresponding private-key
material: an `rsa:rsa-private-key` struct for :rsa, or an integer private
scalar (mod n) for :ec-p256. SUBJECT-CN is the Common Name string for the
subject DN. DOMAINS is an optional list of dNSNames for the subjectAltName
extension attribute.

Signatures use RSASSA-PKCS1-v1_5 with SHA-256 for RSA (as required by every
conformant CA), and ECDSA with SHA-256 (DER encoded) for EC P-256."
  (let* (;; SubjectPublicKeyInfo
         (spki (ecase key-type
                 (:rsa
                  (asn1:der-encode-sequence
                   (asn1:der-encode-sequence
                    (asn1:der-encode-oid pkcs:+oid-rsa-encryption+)
                    (asn1:der-encode-null))
                   (asn1:der-encode-bit-string
                    (asn1:der-encode-sequence
                     (asn1:der-encode-integer (rsa:rsa-private-key-n private-key))
                     (asn1:der-encode-integer (rsa:rsa-private-key-e private-key))))))
                 (:ec-p256
                  (let ((pub-bytes (ec-p256:p256-point-encode-uncompressed
                                    (ecdsa:ecdsa-public-key-from-private private-key))))
                    (asn1:der-encode-sequence
                     (asn1:der-encode-sequence
                      (asn1:der-encode-oid pkcs:+oid-ec-public-key+)
                      (asn1:der-encode-oid pkcs:+oid-prime256v1+))
                     (asn1:der-encode-bit-string pub-bytes))))))
         ;; Subject: SEQUENCE { SET { SEQUENCE { OID CN, UTF8String value } } }
         (cn-bytes (map '(vector (unsigned-byte 8)) #'char-code subject-cn))
         (subject-name
           (asn1:der-encode-sequence
            (asn1:der-encode-set
             (asn1:der-encode-sequence
              (asn1:der-encode-oid +oid-common-name+)
              (asn1:der-encode-tagged asn1:+tag-utf8-string+ cn-bytes)))))
         ;; Attributes [0] IMPLICIT SET OF Attribute
         (attributes
           (if domains
               (asn1:der-encode-context 0 (%csr-encode-extension-request domains)
                                        :constructed t)
               (asn1:der-encode-context 0 #() :constructed t)))
         ;; CertificationRequestInfo
         (cri (asn1:der-encode-sequence
               (asn1:der-encode-integer 0)  ; version v1
               subject-name
               spki
               attributes))
         ;; Signature -- CSRs use PKCS#1 v1.5 for RSA (not PSS).
         (signature (ecase key-type
                      (:rsa
                       (rsa:pkcs1-v15-sign private-key cri))
                      (:ec-p256
                       (multiple-value-bind (r s) (ecdsa:ecdsa-sign private-key cri)
                         (asn1:der-encode-sequence
                          (asn1:der-encode-integer r)
                          (asn1:der-encode-integer s))))))
         (sig-alg (ecase key-type
                    (:rsa (asn1:der-encode-sequence
                           (asn1:der-encode-oid pkcs:+oid-sha256-with-rsa+)
                           (asn1:der-encode-null)))
                    (:ec-p256 (asn1:der-encode-sequence
                               (asn1:der-encode-oid pkcs:+oid-ecdsa-with-sha256+))))))
    (asn1:der-encode-sequence
     cri
     sig-alg
     (asn1:der-encode-bit-string signature))))

;;; ---------------------------------------------------------------------------
;;; PKCS#10 Certificate Signing Request parsing
;;; ---------------------------------------------------------------------------
;;;
;;; Parses a DER-encoded CertificationRequest, verifies its self-signature,
;;; and extracts the subject, public key, and any requested SubjectAlternative
;;; Names from a PKCS#9 extensionRequest attribute. The parsed form is the
;;; input to CA issuance -- the public key bytes and key type can be passed
;;; directly to `make-ca-signed-certificate'.

;; PKCS#9 extensionRequest attribute OID (RFC 2985 5.4.2).
(defparameter +oid-pkcs9-extension-request+ '(1 2 840 113549 1 9 14))

(defstruct (x509-csr (:constructor %make-x509-csr)
                     (:conc-name x509-csr-))
  "Parsed PKCS#10 CertificationRequest."
  (version 0 :type fixnum)
  (subject nil)                                             ; x509-name
  (public-key-algorithm nil :type list)                     ; OID components
  (public-key-params nil)                                   ; optional curve OID TLV
  (public-key-bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (dns-names nil :type list)                                ; list of DNS SANs
  (signature-algorithm nil :type list)                      ; OID components
  (signature #() :type (simple-array (unsigned-byte 8) (*)))
  ;; Raw CertificationRequestInfo bytes (tag+length+contents) exactly as they
  ;; appear inside the CSR. Needed to verify the signature without
  ;; re-canonicalising and to feed downstream auditing.
  (cri-bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (raw-bytes #() :type (simple-array (unsigned-byte 8) (*))))

(defun x509-csr-subject-cn (csr)
  "Return the Common Name string from the CSR subject, or NIL."
  (x509-name-common-name (x509-csr-subject csr)))

(defun %extract-raw-cri-bytes (csr-der)
  "Return the raw CertificationRequestInfo bytes (tag+length+content) from
CSR-DER. Mirrors `extract-raw-tbs' but for PKCS#10 where the CRI is the
first element of the outer SEQUENCE."
  (extract-raw-tbs csr-der))

(defun %parse-csr-spki (spki-tlv)
  "Destructure a SubjectPublicKeyInfo TLV into (values oid params key-bytes).
KEY-BYTES is the inner content of the BIT STRING (with the leading
`unused bits' byte stripped), in the form that `make-ca-signed-certificate'
expects for its :public-key-bytes argument."
  (let* ((children (asn1:der-decode-sequence-contents spki-tlv))
         (alg-children (asn1:der-decode-sequence-contents (first children)))
         (alg-oid (asn1:decode-oid-value (asn1:asn1-tlv-value (first alg-children))))
         (alg-params (when (> (length alg-children) 1) (second alg-children)))
         (bit-string-bytes (asn1:asn1-tlv-value (second children)))
         (key-bytes (subseq bit-string-bytes 1)))  ; strip unused-bits byte
    (values alg-oid alg-params key-bytes)))

(defun %parse-csr-dns-names (attributes-tlv)
  "Extract the list of dNSName SAN values from the CSR attributes context-tag.
Returns NIL if no extensionRequest / subjectAltName is present. Raises an
error on malformed extension encodings."
  (let ((inner (asn1:asn1-tlv-value attributes-tlv)))
    ;; The context [0] wrapper can be empty (no attributes).
    (when (and (listp inner) inner)
      (let ((dns-names nil))
        (dolist (attr inner)
          (let* ((attr-children (asn1:der-decode-sequence-contents attr))
                 (attr-oid (asn1:decode-oid-value
                            (asn1:asn1-tlv-value (first attr-children))))
                 (values-set (second attr-children)))
            (when (equal attr-oid +oid-pkcs9-extension-request+)
              ;; values is a SET containing a single Extensions SEQUENCE.
              (let* ((set-children (asn1:der-decode-sequence-contents values-set))
                     (extensions-seq (first set-children))
                     (extensions (asn1:der-decode-sequence-contents extensions-seq)))
                (dolist (ext extensions)
                  (let* ((ext-children (asn1:der-decode-sequence-contents ext))
                         (ext-oid (asn1:decode-oid-value
                                   (asn1:asn1-tlv-value (first ext-children))))
                         ;; The extnValue is the last element. If a critical
                         ;; BOOLEAN is present, it sits in the middle.
                         (extn-value-tlv (car (last ext-children)))
                         (extn-bytes (asn1:asn1-tlv-value extn-value-tlv)))
                    (when (equal ext-oid +oid-subject-alt-name+)
                      (let* ((san-seq (asn1:der-decode extn-bytes))
                             (san-names (asn1:der-decode-sequence-contents san-seq)))
                        (dolist (n san-names)
                          ;; dNSName [2] IMPLICIT IA5String -- context-specific tag 2.
                          (when (and (= (asn1:asn1-tlv-class n)
                                        asn1:+context-specific+)
                                     (= (asn1:asn1-tlv-tag n) 2))
                            (push (map 'string #'code-char
                                       (asn1:asn1-tlv-value n))
                                  dns-names)))))))))))
        (nreverse dns-names)))))

(defun %csr-key-type-for-oid (oid)
  "Map a SubjectPublicKeyInfo algorithm OID to the keyword the x509 cert
generation code uses. Returns NIL for unsupported algorithms."
  (cond
    ((equal oid pkcs:+oid-ed25519+)      :ed25519)
    ((equal oid pkcs:+oid-ec-public-key+) :ecdsa-p256)
    ((equal oid pkcs:+oid-rsa-encryption+) :rsa)
    (t nil)))

(defun verify-csr-signature (csr)
  "Verify the self-signature on a parsed x509-csr. Returns T on valid,
NIL on invalid. Only supports the signature algorithms that `make-csr'
emits: sha256WithRSAEncryption (PKCS#1 v1.5), ecdsa-with-SHA256, and
Ed25519."
  (let ((sig-alg (x509-csr-signature-algorithm csr))
        (pk-alg (x509-csr-public-key-algorithm csr))
        (pk-bytes (x509-csr-public-key-bytes csr))
        (cri (x509-csr-cri-bytes csr))
        (sig (x509-csr-signature csr)))
    (cond
      ;; RSA + SHA-256, PKCS#1 v1.5 (NOT PSS -- CSRs use v1.5).
      ((equal sig-alg pkcs:+oid-sha256-with-rsa+)
       (when (equal pk-alg pkcs:+oid-rsa-encryption+)
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (rsa:pkcs1-v15-verify pub-key cri sig :hash :sha256)))))
      ;; ECDSA P-256 + SHA-256.
      ((equal sig-alg pkcs:+oid-ecdsa-with-sha256+)
       (when (equal pk-alg pkcs:+oid-ec-public-key+)
         (let ((pk-point (ec-p256:p256-point-decode pk-bytes)))
           (when pk-point
             (let* ((sig-tlv (asn1:der-decode sig))
                    (sig-children (asn1:der-decode-sequence-contents sig-tlv))
                    (r (asn1:decode-der-integer
                        (asn1:asn1-tlv-value (first sig-children))))
                    (s (asn1:decode-der-integer
                        (asn1:asn1-tlv-value (second sig-children)))))
               (ecdsa:ecdsa-verify pk-point cri r s))))))
      ;; Ed25519.
      ((equal sig-alg pkcs:+oid-ed25519+)
       (when (equal pk-alg pkcs:+oid-ed25519+)
         (ed-sign:ed25519-verify pk-bytes cri sig)))
      (t nil))))

(defun parse-csr (der &key (verify t) (max-size 16384))
  "Parse a DER-encoded PKCS#10 CertificationRequest.

DER is a byte vector. Returns an x509-csr struct.

When VERIFY is true (default) the self-signature is checked and an error
is signalled on failure. MAX-SIZE caps the accepted input length to guard
against amplification attacks.

The returned struct exposes the inputs a CA needs to mint a cert:
  - `x509-csr-public-key-algorithm' (OID) and `x509-csr-public-key-bytes'
    are in the form `make-ca-signed-certificate' expects for its
    :key-type and :public-key-bytes arguments (use
    `%csr-key-type-for-oid' externally to map the OID to a keyword).
  - `x509-csr-subject-cn' returns the Common Name string.
  - `x509-csr-dns-names' is the list of dNSName SANs requested via the
    PKCS#9 extensionRequest attribute.
Only sha256WithRSAEncryption, ecdsa-with-SHA256, and Ed25519 signatures
are accepted -- these are what `make-csr' emits and what ACME clients in
practice use."
  (when (> (length der) max-size)
    (error "CSR exceeds maximum size (~D > ~D bytes)" (length der) max-size))
  (let* ((csr-tlv (asn1:der-decode der))
         (csr-children (asn1:der-decode-sequence-contents csr-tlv)))
    (unless (= (length csr-children) 3)
      (error "CSR: outer SEQUENCE must have 3 elements"))
    (let* ((cri-tlv (first csr-children))
           (sig-alg-tlv (second csr-children))
           (sig-bs-tlv (third csr-children))
           (cri-children (asn1:der-decode-sequence-contents cri-tlv)))
      (when (< (length cri-children) 4)
        (error "CSR: CertificationRequestInfo has fewer than 4 fields"))
      (let* ((version-tlv (nth 0 cri-children))
             (subject-tlv (nth 1 cri-children))
             (spki-tlv (nth 2 cri-children))
             (attrs-tlv (nth 3 cri-children))
             (version (asn1:decode-der-integer (asn1:asn1-tlv-value version-tlv))))
        (unless (zerop version)
      (error "CSR: unsupported version ~D (expected 0)" version))
    (multiple-value-bind (pk-oid pk-params pk-bytes) (%parse-csr-spki spki-tlv)
      (let* ((subject (parse-rdn-sequence subject-tlv))
             (dns-names
               (when (and (= (asn1:asn1-tlv-class attrs-tlv) asn1:+context-specific+)
                          (= (asn1:asn1-tlv-tag attrs-tlv) 0))
                 (%parse-csr-dns-names attrs-tlv)))
             (sig-alg-children (asn1:der-decode-sequence-contents sig-alg-tlv))
             (sig-alg-oid (asn1:decode-oid-value
                           (asn1:asn1-tlv-value (first sig-alg-children))))
             (sig-bs (asn1:asn1-tlv-value sig-bs-tlv))
             (sig (subseq sig-bs 1))
             (cri-bytes (%extract-raw-cri-bytes der))
             (csr (%make-x509-csr
                   :version version
                   :subject subject
                   :public-key-algorithm pk-oid
                   :public-key-params pk-params
                   :public-key-bytes pk-bytes
                   :dns-names dns-names
                   :signature-algorithm sig-alg-oid
                   :signature sig
                   :cri-bytes cri-bytes
                   :raw-bytes der)))
          (when verify
            (unless (verify-csr-signature csr)
              (error "CSR: signature verification failed")))
          csr))))))

(defun certificate-from-csr (csr &key serial
                                      (not-before (x509-time-now))
                                      not-after
                                      ca-cert
                                      ca-private-key
                                      ca-key-type
                                      (key-usage '(:digital-signature))
                                      (extended-key-usage '(:server-auth)))
  "Sign a parsed CSR with a CA key, producing a DER-encoded leaf certificate.

CSR is an `x509-csr' as returned by `parse-csr'. Subject, public-key, and
dNSName SANs are taken from the CSR; the CA decides serial number, validity,
and key/extended-key usages.

KEY-USAGE defaults to (:digital-signature) and EXTENDED-KEY-USAGE defaults
to (:server-auth) so that the resulting cert is acceptable to current
TLS clients (Firefox 90+/Chrome 110+ refuse leaf certs without serverAuth)."
  (let ((key-type (%csr-key-type-for-oid (x509-csr-public-key-algorithm csr))))
    (unless key-type
      (error "certificate-from-csr: unsupported CSR key algorithm OID ~A"
             (x509-csr-public-key-algorithm csr)))
    (make-ca-signed-certificate
     :subject (x509-csr-subject csr)
     :serial serial
     :not-before not-before
     :not-after not-after
     :key-type key-type
     :public-key-bytes (x509-csr-public-key-bytes csr)
     :ca-private-key ca-private-key
     :ca-key-type ca-key-type
     :ca-cert ca-cert
     :dns-names (x509-csr-dns-names csr)
     :key-usage key-usage
     :extended-key-usage extended-key-usage)))

;;; ---------------------------------------------------------------------------
;;; X.509 v2 Certificate Revocation Lists (RFC 5280 §5)
;;;
;;;   CertificateList ::= SEQUENCE {
;;;       tbsCertList         TBSCertList,
;;;       signatureAlgorithm  AlgorithmIdentifier,
;;;       signatureValue      BIT STRING }
;;;
;;;   TBSCertList ::= SEQUENCE {
;;;       version             INTEGER OPTIONAL (v2 := 1),
;;;       signature           AlgorithmIdentifier,
;;;       issuer              Name,
;;;       thisUpdate          Time,
;;;       nextUpdate          Time OPTIONAL,
;;;       revokedCertificates SEQUENCE OF
;;;           SEQUENCE { userCertificate    CertificateSerialNumber,
;;;                      revocationDate     Time,
;;;                      crlEntryExtensions Extensions OPTIONAL }
;;;           OPTIONAL,
;;;       crlExtensions [0] EXPLICIT Extensions OPTIONAL }
;;; ---------------------------------------------------------------------------

(defstruct x509-crl-entry
  "One revoked-certificate entry inside a CRL's revokedCertificates SEQUENCE."
  (serial 0 :type integer)
  (revocation-date nil)
  (extensions nil :type list))

(defstruct (x509-crl (:constructor %make-x509-crl))
  "Parsed RFC 5280 v2 CertificateList."
  (version 1 :type fixnum)              ; 1 = v1, 2 = v2
  (signature-algorithm nil :type list)  ; OID components
  (issuer nil)                          ; x509-name
  (this-update nil)                     ; x509-time
  (next-update nil)                     ; x509-time or nil
  (revoked-entries nil :type list)
  (extensions nil :type list)
  (signature #() :type (simple-array (unsigned-byte 8) (*)))
  (tbs-bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (raw-bytes #() :type (simple-array (unsigned-byte 8) (*))))

(defun %parse-revoked-entry (entry-tlv)
  (let* ((children (asn1:der-decode-sequence-contents entry-tlv))
         (serial (asn1:decode-der-integer (asn1:asn1-tlv-value (first children))))
         (rev-date (parse-x509-time-tlv (second children)))
         (extensions
           (when (> (length children) 2)
             ;; The third element is a `SEQUENCE OF Extension' (no [0]
             ;; EXPLICIT wrapper -- that wrapper only appears around
             ;; CRL-level extensions, not entry-level ones).
             (%parse-extension-list (third children)))))
    (make-x509-crl-entry :serial serial
                         :revocation-date rev-date
                         :extensions (or extensions nil))))

(defun parse-x509-crl (der &key (verify t) (issuer-cert nil))
  "Parse a DER-encoded X.509 CRL (RFC 5280 CertificateList).

When VERIFY is true and ISSUER-CERT is supplied, the CRL signature is
checked against ISSUER-CERT's public key and an error is raised on
mismatch. When ISSUER-CERT is NIL the signature is left for the caller
to verify with `verify-crl-signature'.

Returns an `x509-crl' struct."
  (declare (type (simple-array (unsigned-byte 8) (*)) der))
  (let* ((crl-tlv (asn1:der-decode der))
         (children (asn1:der-decode-sequence-contents crl-tlv))
         (tbs-tlv (first children))
         (sig-alg-tlv (second children))
         (sig-bs-tlv (third children))
         (tbs-children (asn1:der-decode-sequence-contents tbs-tlv))
         (idx 0)
         (version 1)
         (first-tbs (nth 0 tbs-children)))
    ;; Optional version INTEGER
    (when (= (asn1:asn1-tlv-tag first-tbs) asn1:+tag-integer+)
      (setf version (1+ (asn1:decode-der-integer
                         (asn1:asn1-tlv-value first-tbs))))
      (incf idx))
    (let* ((sig-tbs-tlv (nth idx tbs-children)) (_1 (incf idx))
           (issuer-tlv (nth idx tbs-children))  (_2 (incf idx))
           (issuer (parse-rdn-sequence issuer-tlv))
           (this-update (parse-x509-time-tlv (nth idx tbs-children)))
           (_3 (incf idx))
           (next-update nil)
           (revoked-entries nil)
           (extensions nil))
      (declare (ignore _1 _2 _3 sig-tbs-tlv))
      ;; Optional nextUpdate (UTCTime / GeneralizedTime tag)
      (when (and (< idx (length tbs-children))
                 (let ((tag (asn1:asn1-tlv-tag (nth idx tbs-children))))
                   (or (= tag asn1:+tag-utc-time+)
                       (= tag asn1:+tag-generalized-time+))))
        (setf next-update (parse-x509-time-tlv (nth idx tbs-children)))
        (incf idx))
      ;; Optional revokedCertificates SEQUENCE OF
      (when (and (< idx (length tbs-children))
                 (= (asn1:asn1-tlv-tag (nth idx tbs-children))
                    asn1:+tag-sequence+)
                 (= (asn1:asn1-tlv-class (nth idx tbs-children))
                    asn1:+universal+))
        (let* ((rc-tlv (nth idx tbs-children))
               (rc-children (asn1:der-decode-sequence-contents rc-tlv)))
          (setf revoked-entries
                (mapcar #'%parse-revoked-entry rc-children)))
        (incf idx))
      ;; Optional crlExtensions [0] EXPLICIT Extensions
      (when (and (< idx (length tbs-children))
                 (= (asn1:asn1-tlv-class (nth idx tbs-children))
                    asn1:+context-specific+)
                 (= (asn1:asn1-tlv-tag (nth idx tbs-children)) 0))
        (setf extensions (parse-extensions (nth idx tbs-children))))
      (let* ((sig-alg-children (asn1:der-decode-sequence-contents sig-alg-tlv))
             (sig-alg-oid (asn1:decode-oid-value
                           (asn1:asn1-tlv-value (first sig-alg-children))))
             (sig-bs (asn1:asn1-tlv-value sig-bs-tlv))
             (sig (subseq sig-bs 1))
             (tbs-bytes (extract-raw-tbs der))
             (crl (%make-x509-crl
                   :version version
                   :signature-algorithm sig-alg-oid
                   :issuer issuer
                   :this-update this-update
                   :next-update next-update
                   :revoked-entries revoked-entries
                   :extensions extensions
                   :signature sig
                   :tbs-bytes tbs-bytes
                   :raw-bytes der)))
        (when (and verify issuer-cert)
          (unless (verify-crl-signature crl issuer-cert)
            (error "CRL signature verification failed")))
        crl))))

(defun parse-x509-crl-pem (pem-text &key (verify t) (issuer-cert nil))
  "Parse a PEM-encoded `X509 CRL' block."
  (let ((block (pem:pem-decode pem-text)))
    (when (and block
               (or (string= (pem:pem-block-label block) "X509 CRL")
                   (string= (pem:pem-block-label block) "CRL")))
      (parse-x509-crl (pem:pem-block-data block)
                      :verify verify :issuer-cert issuer-cert))))

(defun verify-crl-signature (crl issuer-cert)
  "Verify the CRL signature against the issuer's public key.
   Returns T on valid, NIL on invalid. Mirrors the algorithms accepted
   by `verify-certificate-signature' (RSA-PSS, ECDSA P-256, Ed25519).

   NOTE: real-world CRLs are usually signed with `sha256WithRSAEncryption'
   (PKCS#1 v1.5), so that path is handled explicitly in addition to PSS."
  (let ((sig-alg (x509-crl-signature-algorithm crl))
        (tbs (x509-crl-tbs-bytes crl))
        (sig (x509-crl-signature crl))
        (pk-alg (x509-cert-public-key-algorithm issuer-cert))
        (pk-bytes (x509-cert-public-key-bytes issuer-cert)))
    (cond
      ;; RSA + SHA-256 -- PKCS#1 v1.5 first (typical CRL signing), then PSS
      ((equal sig-alg pkcs:+oid-sha256-with-rsa+)
       (when (equal pk-alg pkcs:+oid-rsa-encryption+)
         (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
           (let ((pub-key (rsa:make-rsa-public-key n e)))
             (or (rsa:pkcs1-v15-verify pub-key tbs sig :hash :sha256)
                 (rsa:rsa-pss-verify pub-key tbs sig :hash :sha256))))))
      ;; ECDSA P-256 + SHA-256
      ((equal sig-alg pkcs:+oid-ecdsa-with-sha256+)
       (when (equal pk-alg pkcs:+oid-ec-public-key+)
         (let ((pk-point (ec-p256:p256-point-decode pk-bytes)))
           (when pk-point
             (let* ((sig-tlv (asn1:der-decode sig))
                    (sig-children (asn1:der-decode-sequence-contents sig-tlv))
                    (r (asn1:decode-der-integer
                        (asn1:asn1-tlv-value (first sig-children))))
                    (s (asn1:decode-der-integer
                        (asn1:asn1-tlv-value (second sig-children)))))
               (ecdsa:ecdsa-verify pk-point tbs r s))))))
      ;; Ed25519
      ((equal sig-alg pkcs:+oid-ed25519+)
       (when (equal pk-alg pkcs:+oid-ed25519+)
         (ed-sign:ed25519-verify pk-bytes tbs sig)))
      (t nil))))

(defun crl-revokes-p (crl serial)
  "Test whether SERIAL is listed in CRL's revokedCertificates.
   Returns the matching `x509-crl-entry' on hit, NIL on miss."
  (find serial (x509-crl-revoked-entries crl)
        :key #'x509-crl-entry-serial))

(defun %encode-revoked-entry (entry)
  "DER-encode one revokedCertificates SEQUENCE element."
  (asn1:der-encode-sequence
   (asn1:der-encode-integer (x509-crl-entry-serial entry))
   (encode-x509-time (x509-crl-entry-revocation-date entry))))

(defun make-crl (&key issuer-cert
                      ca-key-type
                      ca-private-key
                      this-update
                      next-update
                      (revoked-entries nil)
                      (crl-number nil))
  "Build a DER-encoded X.509 v2 CertificateList (RFC 5280 §5).

ISSUER-CERT supplies the issuer Name (its subject) and is the
certificate whose CA key signs this CRL. CA-PRIVATE-KEY / CA-KEY-TYPE
are the CA's signing material. THIS-UPDATE / NEXT-UPDATE are the CRL
validity bounds (NEXT-UPDATE is optional but recommended).
REVOKED-ENTRIES is a list of `x509-crl-entry' structs; CRL-NUMBER, if
non-nil, populates the standard CRLNumber extension (OID 2.5.29.20).

Only the v2 CRL signature algorithms supported by `make-self-signed-
certificate' are emitted: Ed25519, ECDSA-with-SHA-256, and
RSA-PSS-with-SHA-256."
  (let* ((alg-oid (ecase ca-key-type
                    (:ed25519 pkcs:+oid-ed25519+)
                    (:ecdsa-p256 pkcs:+oid-ecdsa-with-sha256+)
                    (:rsa pkcs:+oid-sha256-with-rsa+)))
         (sig-alg-id (ecase ca-key-type
                       (:ed25519
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:ecdsa-p256
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)))
                       (:rsa
                        (asn1:der-encode-sequence
                         (asn1:der-encode-oid alg-oid)
                         (asn1:der-encode-null)))))
         (issuer-bytes (encode-rdn-sequence (x509-cert-subject issuer-cert)))
         (revoked-section
           (when revoked-entries
             (apply #'asn1:der-encode-sequence
                    (mapcar #'%encode-revoked-entry revoked-entries))))
         (crl-number-ext-bytes
           (when crl-number
             (asn1:der-encode-sequence
              (asn1:der-encode-oid '(2 5 29 20))      ; cRLNumber
              (asn1:der-encode-octet-string
               (asn1:der-encode-integer crl-number)))))
         (extensions-section
           (when crl-number-ext-bytes
             (asn1:der-encode-context
              0 (asn1:der-encode-sequence crl-number-ext-bytes)
              :constructed t)))
         (tbs (apply #'asn1:der-encode-sequence
                     (asn1:der-encode-integer 1) ; v2
                     sig-alg-id
                     issuer-bytes
                     (encode-x509-time this-update)
                     (append
                      (when next-update
                        (list (encode-x509-time next-update)))
                      (when revoked-section (list revoked-section))
                      (when extensions-section (list extensions-section)))))
         (signature (sign-tbs tbs ca-key-type ca-private-key)))
    (asn1:der-encode-sequence
     tbs
     sig-alg-id
     (asn1:der-encode-bit-string signature))))
