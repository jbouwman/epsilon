;;;; ASN.1 DER Encoder/Decoder (ITU-T X.690)
;;;;
;;;; Implements Distinguished Encoding Rules (DER) for ASN.1 structures.
;;;; Supports the subset needed for X.509 certificates and PKCS encoding.
;;;;
;;;; The decoder is a pull parser that validates tag/length/value triples
;;;; without building a full ASN.1 tree. A depth limit prevents memory
;;;; amplification attacks from deeply nested constructions.

(defpackage epsilon.ssl.asn1
  (:use :cl)
  (:export
   ;; Tag classes and types
   #:+universal+ #:+application+ #:+context-specific+ #:+private+
   #:+primitive+ #:+constructed+
   ;; Universal tags
   #:+tag-boolean+
   #:+tag-integer+
   #:+tag-bit-string+
   #:+tag-octet-string+
   #:+tag-null+
   #:+tag-oid+
   #:+tag-utf8-string+
   #:+tag-sequence+
   #:+tag-set+
   #:+tag-printable-string+
   #:+tag-ia5-string+
   #:+tag-utc-time+
   #:+tag-generalized-time+
   ;; TLV structure
   #:asn1-tlv
   #:asn1-tlv-tag
   #:asn1-tlv-constructed-p
   #:asn1-tlv-class
   #:asn1-tlv-value
   ;; Decoding
   #:der-decode
   #:der-decode-all
   #:der-decode-sequence-contents
   ;; Encoding
   #:der-encode
   #:der-encode-integer
   #:der-encode-oid
   #:der-encode-octet-string
   #:der-encode-bit-string
   #:der-encode-null
   #:der-encode-boolean
   #:der-encode-sequence
   #:der-encode-set
   #:der-encode-context
   ;; OID utilities
   #:oid-to-string
   #:string-to-oid
   #:decode-oid-value
   ;; Integer utilities
   #:decode-der-integer
   #:integer-to-der-bytes
   ;; Generic tagged encoding
   #:der-encode-tagged))

(in-package :epsilon.ssl.asn1)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

;; Tag classes (bits 7-6)
(defconstant +universal+        #b00)
(defconstant +application+      #b01)
(defconstant +context-specific+  #b10)
(defconstant +private+          #b11)

;; Constructed vs primitive (bit 5)
(defconstant +primitive+    0)
(defconstant +constructed+  1)

;; Universal tag numbers
(defconstant +tag-boolean+           1)
(defconstant +tag-integer+           2)
(defconstant +tag-bit-string+        3)
(defconstant +tag-octet-string+      4)
(defconstant +tag-null+              5)
(defconstant +tag-oid+              6)
(defconstant +tag-utf8-string+      12)
(defconstant +tag-sequence+         16)
(defconstant +tag-set+              17)
(defconstant +tag-printable-string+ 19)
(defconstant +tag-ia5-string+       22)
(defconstant +tag-utc-time+         23)
(defconstant +tag-generalized-time+ 24)

;; Maximum nesting depth to prevent stack overflow / amplification attacks
(defconstant +max-depth+ 32)

;;; ---------------------------------------------------------------------------
;;; TLV structure
;;; ---------------------------------------------------------------------------

(defstruct (asn1-tlv (:constructor %make-asn1-tlv))
  (tag 0 :type (unsigned-byte 32))
  (class +universal+ :type (unsigned-byte 2))
  (constructed-p nil :type boolean)
  (value #() :type (or (simple-array (unsigned-byte 8) (*)) list)))

(defun make-asn1-tlv (tag class constructed-p value)
  (%make-asn1-tlv :tag tag :class class :constructed-p constructed-p :value value))

;;; ---------------------------------------------------------------------------
;;; DER Decoding
;;; ---------------------------------------------------------------------------

(defun read-tag (data pos)
  "Read a DER tag starting at POS. Returns (values tag class constructed-p new-pos)."
  (when (>= pos (length data))
    (error "ASN.1: unexpected end of data reading tag"))
  (let* ((byte (aref data pos))
         (class (ash byte -6))
         (constructed (logbitp 5 byte))
         (tag-number (logand byte #x1F)))
    (incf pos)
    (if (= tag-number #x1F)
        ;; Long form tag
        (let ((n 0))
          (loop
            (when (>= pos (length data))
              (error "ASN.1: unexpected end of data in long tag"))
            (let ((b (aref data pos)))
              (incf pos)
              (setf n (logior (ash n 7) (logand b #x7F)))
              (unless (logbitp 7 b)
                (return))))
          (values n class constructed pos))
        (values tag-number class constructed pos))))

(defun read-length (data pos)
  "Read a DER length starting at POS. Returns (values length new-pos)."
  (when (>= pos (length data))
    (error "ASN.1: unexpected end of data reading length"))
  (let ((byte (aref data pos)))
    (incf pos)
    (cond
      ((not (logbitp 7 byte))
       ;; Short form
       (values byte pos))
      ((zerop (logand byte #x7F))
       (error "ASN.1: indefinite length not allowed in DER"))
      (t
       ;; Long form
       (let ((num-bytes (logand byte #x7F)))
         (when (> num-bytes 4)
           (error "ASN.1: length too large (> 4 bytes)"))
         (let ((len 0))
           (loop for i from 0 below num-bytes
                 do (when (>= pos (length data))
                      (error "ASN.1: unexpected end of data in length"))
                    (setf len (logior (ash len 8) (aref data pos)))
                    (incf pos))
           ;; DER: must use shortest encoding
           (when (and (= num-bytes 1) (< len 128))
             (error "ASN.1: DER violation: non-minimal length encoding"))
           (values len pos)))))))

(defun der-decode (data &key (start 0) (depth 0))
  "Decode one DER TLV from DATA starting at START.
   Returns (values asn1-tlv new-position).
   Recursively decodes SEQUENCE/SET contents."
  (when (> depth +max-depth+)
    (error "ASN.1: maximum nesting depth exceeded"))
  (multiple-value-bind (tag class constructed new-pos) (read-tag data start)
    (multiple-value-bind (len val-start) (read-length data new-pos)
      (when (> (+ val-start len) (length data))
        (error "ASN.1: value extends beyond data (need ~d bytes at ~d, have ~d)"
               len val-start (length data)))
      (let ((val-end (+ val-start len)))
        (if constructed
            ;; Recursively decode contents
            (let ((children nil)
                  (pos val-start))
              (loop while (< pos val-end)
                    do (multiple-value-bind (child child-end)
                           (der-decode data :start pos :depth (1+ depth))
                         (push child children)
                         (setf pos child-end)))
              (unless (= pos val-end)
                (error "ASN.1: constructed value length mismatch"))
              (values (make-asn1-tlv tag class t (nreverse children))
                      val-end))
            ;; Primitive: extract raw bytes
            (values (make-asn1-tlv tag class nil
                                   (subseq data val-start val-end))
                    val-end))))))

(defun der-decode-all (data &key (start 0))
  "Decode all TLVs from DATA. Returns a list of asn1-tlv."
  (let ((result nil)
        (pos start))
    (loop while (< pos (length data))
          do (multiple-value-bind (tlv new-pos)
                 (der-decode data :start pos)
               (push tlv result)
               (setf pos new-pos)))
    (nreverse result)))

(defun der-decode-sequence-contents (tlv)
  "Get the children of a SEQUENCE or SET TLV."
  (unless (asn1-tlv-constructed-p tlv)
    (error "ASN.1: expected constructed type"))
  (asn1-tlv-value tlv))

;;; ---------------------------------------------------------------------------
;;; DER Encoding
;;; ---------------------------------------------------------------------------

(defun encode-tag (tag class constructed)
  "Encode a DER tag as bytes."
  (let ((first-byte (logior (ash class 6)
                            (if constructed #x20 0))))
    (if (< tag #x1F)
        (make-array 1 :element-type '(unsigned-byte 8)
                      :initial-element (logior first-byte tag))
        ;; Long form tag
        (let ((tag-bytes nil))
          (loop for n = tag then (ash n -7)
                while (plusp n)
                do (push (logand n #x7F) tag-bytes))
          ;; Set continuation bits
          (let ((result (make-array (1+ (length tag-bytes))
                                   :element-type '(unsigned-byte 8))))
            (setf (aref result 0) (logior first-byte #x1F))
            (loop for i from 1
                  for b in tag-bytes
                  for last-p = (= i (length tag-bytes))
                  do (setf (aref result i)
                           (if last-p b (logior b #x80))))
            result)))))

(defun encode-length (len)
  "Encode a DER length as bytes."
  (cond
    ((< len 128)
     (make-array 1 :element-type '(unsigned-byte 8) :initial-element len))
    (t
     (let ((num-bytes (ceiling (integer-length len) 8)))
       (let ((result (make-array (1+ num-bytes) :element-type '(unsigned-byte 8))))
         (setf (aref result 0) (logior #x80 num-bytes))
         (loop for i from num-bytes downto 1
               for shift from 0 by 8
               do (setf (aref result i) (logand #xFF (ash len (- shift)))))
         result)))))

(defun concat-arrays (&rest arrays)
  "Concatenate byte arrays."
  (let* ((total (reduce #'+ arrays :key #'length))
         (result (make-array total :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (arr arrays result)
      (replace result arr :start1 pos)
      (incf pos (length arr)))))

(defun der-encode-tlv (tag class constructed value-bytes)
  "Encode a complete TLV."
  (concat-arrays (encode-tag tag class constructed)
                 (encode-length (length value-bytes))
                 value-bytes))

(defun der-encode (tlv)
  "Encode an asn1-tlv back to DER bytes."
  (if (asn1-tlv-constructed-p tlv)
      ;; Constructed: encode children and concatenate
      (let ((child-bytes (apply #'concat-arrays
                                (mapcar #'der-encode (asn1-tlv-value tlv)))))
        (der-encode-tlv (asn1-tlv-tag tlv) (asn1-tlv-class tlv) t child-bytes))
      ;; Primitive: value is already bytes
      (der-encode-tlv (asn1-tlv-tag tlv) (asn1-tlv-class tlv) nil
                      (asn1-tlv-value tlv))))

;;; ---------------------------------------------------------------------------
;;; Convenience encoders
;;; ---------------------------------------------------------------------------

(defun integer-to-der-bytes (n)
  "Encode an integer as DER INTEGER content bytes (minimal two's complement)."
  (if (zerop n)
      (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
      ;; For any integer n, the minimal signed encoding needs
      ;; ceiling((1 + integer-length(n)) / 8) bytes.
      ;; CL's integer-length counts significant bits (excluding sign for negatives).
      (let* ((byte-count (ceiling (1+ (integer-length n)) 8))
             (bytes (make-array byte-count :element-type '(unsigned-byte 8) :initial-element 0)))
        (if (minusp n)
            ;; Two's complement: store the bits of n (CL handles negative integers natively)
            (loop for i from (1- byte-count) downto 0
                  for shift from 0 by 8
                  do (setf (aref bytes i) (logand #xFF (ash n (- shift)))))
            ;; Positive: big-endian with possible leading zero for sign bit
            (loop for i from (1- byte-count) downto 0
                  for shift from 0 by 8
                  do (setf (aref bytes i) (logand #xFF (ash n (- shift))))))
        bytes)))

(defun decode-der-integer (bytes)
  "Decode DER INTEGER content bytes to a CL integer."
  (if (zerop (length bytes))
      (error "ASN.1: empty INTEGER encoding")
      (let ((negative (logbitp 7 (aref bytes 0)))
            (n 0))
        (loop for b across bytes
              do (setf n (logior (ash n 8) b)))
        (if negative
            ;; Two's complement
            (- n (ash 1 (* 8 (length bytes))))
            n))))

(defun der-encode-integer (n)
  "Encode an integer as a DER INTEGER TLV."
  (der-encode-tlv +tag-integer+ +universal+ nil (integer-to-der-bytes n)))

(defun encode-oid-component (n)
  "Encode a single OID component in base-128."
  (if (< n 128)
      (list n)
      (let ((bytes nil))
        (loop for val = n then (ash val -7)
              while (plusp val)
              do (push (logand val #x7F) bytes))
        ;; Set continuation bits on all but last
        (loop for cell on bytes
              unless (null (cdr cell))
              do (setf (car cell) (logior (car cell) #x80)))
        bytes)))

(defun der-encode-oid (oid-components)
  "Encode an OID from a list of integer components."
  (when (< (length oid-components) 2)
    (error "ASN.1: OID must have at least 2 components"))
  (let* ((first (first oid-components))
         (second (second oid-components))
         (combined (+ (* first 40) second))
         (bytes nil))
    (setf bytes (encode-oid-component combined))
    (dolist (c (cddr oid-components))
      (setf bytes (append bytes (encode-oid-component c))))
    (der-encode-tlv +tag-oid+ +universal+ nil
                    (make-array (length bytes) :element-type '(unsigned-byte 8)
                                :initial-contents bytes))))

(defun decode-oid-value (bytes)
  "Decode OID value bytes to a list of integer components."
  (when (zerop (length bytes))
    (error "ASN.1: empty OID encoding"))
  (let ((first-byte (aref bytes 0))
        (components nil)
        (pos 1))
    ;; First byte encodes first two components
    (push (floor first-byte 40) components)
    (push (mod first-byte 40) components)
    ;; Remaining components
    (loop while (< pos (length bytes))
          do (let ((n 0))
               (loop
                 (when (>= pos (length bytes))
                   (error "ASN.1: truncated OID component"))
                 (let ((b (aref bytes pos)))
                   (incf pos)
                   (setf n (logior (ash n 7) (logand b #x7F)))
                   (unless (logbitp 7 b)
                     (return))))
               (push n components)))
    (nreverse components)))

(defun oid-to-string (components)
  "Convert OID component list to dotted string, e.g. \"1.2.840.113549\"."
  (format nil "~{~d~^.~}" components))

(defun string-to-oid (string)
  "Parse dotted OID string to component list."
  (let ((components nil)
        (start 0))
    (loop for i from 0 to (length string)
          when (or (= i (length string)) (char= (char string i) #\.))
          do (push (parse-integer string :start start :end i) components)
             (setf start (1+ i)))
    (nreverse components)))

(defun der-encode-tagged (tag bytes &key (class +universal+) constructed)
  "Encode bytes with the given tag and class. Generic TLV encoding."
  (der-encode-tlv tag class constructed bytes))

(defun der-encode-octet-string (bytes)
  "Encode a byte array as a DER OCTET STRING."
  (der-encode-tlv +tag-octet-string+ +universal+ nil bytes))

(defun der-encode-bit-string (bytes &optional (unused-bits 0))
  "Encode a byte array as a DER BIT STRING."
  (let ((content (make-array (1+ (length bytes)) :element-type '(unsigned-byte 8))))
    (setf (aref content 0) unused-bits)
    (replace content bytes :start1 1)
    (der-encode-tlv +tag-bit-string+ +universal+ nil content)))

(defun der-encode-null ()
  "Encode a DER NULL."
  (der-encode-tlv +tag-null+ +universal+ nil
                  (make-array 0 :element-type '(unsigned-byte 8))))

(defun der-encode-boolean (value)
  "Encode a DER BOOLEAN."
  (der-encode-tlv +tag-boolean+ +universal+ nil
                  (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-element (if value #xFF 0))))

(defun der-encode-sequence (&rest children)
  "Encode children as a DER SEQUENCE."
  (let ((content (apply #'concat-arrays children)))
    (der-encode-tlv +tag-sequence+ +universal+ t content)))

(defun der-encode-set (&rest children)
  "Encode children as a DER SET."
  (let ((content (apply #'concat-arrays children)))
    (der-encode-tlv +tag-set+ +universal+ t content)))

(defun der-encode-context (tag-number value &key (constructed nil))
  "Encode a context-specific tagged value.
   VALUE should be pre-encoded DER bytes."
  (der-encode-tlv tag-number +context-specific+ constructed value))
