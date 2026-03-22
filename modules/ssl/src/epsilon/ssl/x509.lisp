;;;; X.509 Certificate Parsing and Validation (RFC 5280)
;;;;
;;;; Implements X.509v3 certificate parsing, chain building, path validation,
;;;; hostname verification, and certificate generation. Uses the ASN.1 DER
;;;; encoder/decoder and PKCS key encoding modules.

(defpackage epsilon.ssl.x509
  (:use :cl)
  (:local-nicknames
   (#:asn1 #:epsilon.ssl.asn1)
   (#:pem #:epsilon.ssl.pem)
   (#:pkcs #:epsilon.ssl.pkcs)
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha384 #:epsilon.ssl.sha512)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:rsa #:epsilon.ssl.rsa)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ec-p256 #:epsilon.ssl.ec-p256))
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
   ;; Validation
   #:verify-certificate-signature
   #:verify-certificate-chain
   #:hostname-matches-p
   ;; Certificate generation
   #:make-self-signed-certificate
   #:make-ca-signed-certificate
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
   #:make-x509-name))

(in-package :epsilon.ssl.x509)

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

(defun parse-extensions (extensions-tlv)
  "Parse the extensions from a TBSCertificate.
   EXTENSIONS-TLV is the context-specific [3] wrapper."
  (let* ((inner-seq (first (asn1:der-decode-sequence-contents extensions-tlv)))
         (ext-list (asn1:der-decode-sequence-contents inner-seq))
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
        (push (make-x509-extension :oid oid :critical critical :value value) extensions)))
    (nreverse extensions)))

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
  (raw-bytes #() :type (simple-array (unsigned-byte 8) (*))))  ; full certificate DER

(defun x509-get-extension (cert oid)
  "Get an extension from a certificate by OID, or NIL."
  (find oid (x509-cert-extensions cert) :key #'x509-extension-oid :test #'equal))

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
   Returns an x509-certificate structure."
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
      (let ((issuer (parse-rdn-sequence (nth idx tbs-children))))
        (incf idx)
        ;; validity
        (let* ((validity-children (asn1:der-decode-sequence-contents (nth idx tbs-children)))
               (not-before (parse-x509-time-tlv (first validity-children)))
               (not-after (parse-x509-time-tlv (second validity-children))))
          (incf idx)
          ;; subject
          (let ((subject (parse-rdn-sequence (nth idx tbs-children))))
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

(defun verify-certificate-chain (chain &key trust-store (time (x509-time-now))
                                         (verify-time t))
  "Verify a certificate chain. CHAIN is a list of certificates from
   leaf to root. TRUST-STORE is a trust-store of root CA certificates.
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

  ;; Verify signature chain
  (loop for i from 0 below (1- (length chain))
        for cert = (nth i chain)
        for issuer = (nth (1+ i) chain)
        do (unless (verify-certificate-signature cert issuer)
             (return-from verify-certificate-chain
               (values nil (format nil "signature verification failed at depth ~D" i))))
           ;; Check that issuer is a CA
           (let ((bc-ext (x509-get-extension issuer +oid-basic-constraints+)))
             (when bc-ext
               (let ((bc (parse-basic-constraints bc-ext)))
                 (unless (x509-basic-constraints-ca bc)
                   (return-from verify-certificate-chain
                     (values nil (format nil "non-CA certificate used as issuer at depth ~D" (1+ i)))))
                 ;; Check path length constraint
                 (when (and (x509-basic-constraints-path-len bc)
                           (> i (x509-basic-constraints-path-len bc)))
                   (return-from verify-certificate-chain
                     (values nil (format nil "path length constraint exceeded at depth ~D" (1+ i)))))))))

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

(defun find-trusted-issuer (cert trust-store)
  "Find a trusted issuer for CERT in the trust store."
  (dolist (trusted (trust-store-certificates trust-store))
    (when (and (equal (x509-name-entries (x509-cert-subject trusted))
                     (x509-name-entries (x509-cert-issuer cert)))
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
   Only left-most label wildcards are supported (e.g., *.example.com)."
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
         (and
          ;; Must have at least one label before the suffix
          (let ((dot-pos (position #\. hostname)))
            (and dot-pos
                 ;; The suffix must match
                 (string= (subseq hostname dot-pos) suffix)
                 ;; Wildcard must not span dots (RFC 6125 Section 6.4.3)
                 (not (find #\. hostname :end dot-pos))
                 ;; Must have at least 2 labels in suffix
                 (find #\. suffix :start 1))))))
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
                                          (dns-names nil))
  "Generate a self-signed X.509v3 certificate.
   KEY-TYPE: :ed25519, :ecdsa-p256, or :rsa
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
            ;; Basic Constraints
            (push (asn1:der-encode-sequence
                   (asn1:der-encode-oid +oid-basic-constraints+)
                   (asn1:der-encode-boolean is-ca)
                   (asn1:der-encode-octet-string
                    (asn1:der-encode-sequence
                     (if is-ca
                         (asn1:der-encode-boolean t)
                         (asn1:der-encode-sequence)))))
                  ext-list)
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
                                        (dns-names nil))
  "Generate a CA-signed X.509v3 certificate.
   Returns DER-encoded certificate bytes."
  (let* ((subject-name (if (typep subject 'x509-name)
                          subject
                          (make-x509-name :entries (list (cons +oid-common-name+ subject)))))
         (issuer-name (x509-cert-subject ca-cert))
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
         ;; Extensions
         (extensions
          (let ((ext-list nil))
            (push (asn1:der-encode-sequence
                   (asn1:der-encode-oid +oid-basic-constraints+)
                   (asn1:der-encode-boolean t)
                   (asn1:der-encode-octet-string
                    (asn1:der-encode-sequence)))
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
                     (encode-rdn-sequence issuer-name)
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
