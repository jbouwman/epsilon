(defpackage :epsilon.lib.char
  (:use
   :cl
   :epsilon.lib.type)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:vector :epsilon.lib.vector))
  (:export
   :code-point
   :code
   :octet
   :unicode-char
   :simple-unicode-string

   :*suppress-character-coding-errors*
   :+unicode-replacement-code-point+
   :+default-replacement-code-point+
   
   :enc-max-units-per-char
   :get-character-encoding
   :define-character-encoding
   :define-unibyte-methods
   :character-encoding-error
   :character-decoding-error
   :handle-encoding-error
   :handle-decoding-error
   
   :encode-to-octets
   :decode-from-octets
   :count-encoding-octets
   :count-decoding-characters

   :string-size-in-bytes
   :vector-size-in-chars
   
   :string-set
   :bytes-to-string
   :string-to-bytes
   ))

(in-package :epsilon.lib.char)

;;;; Core Types

(deftype code-point () '(mod #x110000))

(deftype unicode-char ()
  "This character type can hold any characters whose CHAR-CODEs
are less than UNICODE-CHAR-CODE-LIMIT."
  'character)

(deftype simple-unicode-string ()
  "Alias for (SIMPLE-ARRAY UNICODE-CHAR (*))."
  '(simple-array unicode-char (*)))

;;;; Character Encoding Metadata

(defclass character-encoding ()
  ((name :initarg :name :reader enc-name
         :initform (error "Must specify a NAME for this character encoding."))
   (documentation
    :initarg :documentation :reader enc-documentation :initform nil)
   (aliases :initarg :aliases :initform nil :reader enc-aliases)
   (code-unit-size
    :initarg :code-unit-size :reader enc-code-unit-size :initform 8)
   (max-units-per-char
    :initarg :max-units-per-char :reader enc-max-units-per-char :initform 1)
   (use-bom :initarg :use-bom :initform nil :reader enc-use-bom)
   (bom-encoding
    :initarg :bom-encoding :reader enc-bom-encoding :initform nil)
   (default-replacement
    :initarg :default-replacement :reader enc-default-replacement
    :initform #x1a)))

(defparameter *supported-character-encodings* nil)

(defun list-character-encodings ()
  "List of keyword symbols denoting supported character
encodings.  This list does not include aliases."
  *supported-character-encodings*)

(defparameter *character-encodings* map:+empty+)

(defparameter *default-character-encoding* :utf-8
  "Special variable used to determine the default character
encoding.")

(defun get-character-encoding (name)
  "Lookups the character encoding denoted by the keyword symbol
NAME.  Signals an error if one is not found.  If NAME is already
a CHARACTER-ENCONDING object, it is returned unmodified."
  (when (typep name 'character-encoding)
    (return-from get-character-encoding name))
  (when (eq name :default)
    (setq name *default-character-encoding*))
  (or (map:get *character-encodings* name)
      (error "Unknown character encoding: ~S" name)))

(defun notice-character-encoding (enc)
  (pushnew (enc-name enc) *supported-character-encodings*)
  (dolist (kw (cons (enc-name enc) (enc-aliases enc)))
    (setf *character-encodings*
          (map:assoc *character-encodings* kw enc)))
  (enc-name enc))

(defmacro define-character-encoding (name docstring &body options)
  `(notice-character-encoding
    (make-instance 'character-encoding :name ,name ,@options
                   :documentation ,docstring)))

;;;; Generic Functions for Encoding Operations

(defgeneric encode-to-octets (encoding string start end destination dest-start)
  (:documentation "Encode STRING from START to END into DESTINATION starting at DEST-START.
Returns the number of octets written."))

(defgeneric decode-from-octets (encoding octets start end destination dest-start)
  (:documentation "Decode OCTETS from START to END into DESTINATION starting at DEST-START.
Returns the number of characters written."))

(defgeneric count-encoding-octets (encoding string start end &optional max)
  (:documentation "Count how many octets would be needed to encode STRING from START to END.
If MAX is provided and positive, stop counting when MAX octets would be exceeded.
Returns (values octet-count characters-processed)."))

(defgeneric count-decoding-characters (encoding octets start end &optional max)
  (:documentation "Count how many characters would result from decoding OCTETS from START to END.
If MAX is provided and positive, stop counting when MAX characters would be produced.
Returns (values character-count octets-processed)."))

;;;; Error Handling

(defparameter *suppress-character-coding-errors* nil
  "If non-NIL, encoding/decoding errors are suppressed and replacement characters used.")

(defconstant +default-replacement-code-point+ #x1a
  "Default replacement character for encoding errors.")

(defconstant +unicode-replacement-code-point+ #xfffd
  "Unicode replacement character for decoding errors.")

(defun handle-encoding-error (code encoding position &optional (replacement +default-replacement-code-point+))
  "Handle an encoding error for CODE at POSITION."
  (unless *suppress-character-coding-errors*
    (error 'character-encoding-error 
           :code code :encoding encoding :position position))
  replacement)

(defun handle-decoding-error (octets encoding position &optional (replacement +unicode-replacement-code-point+) condition)
  "Handle a decoding error for OCTETS at POSITION with optional specific CONDITION."
  (unless *suppress-character-coding-errors*
    (error (or condition 'character-decoding-error)
           :octets octets :encoding encoding :position position))
  replacement)

;;;; High-Level Interface Functions

(defun string-to-octets (string &key (encoding :utf-8) (start 0) end
                                  (errorp (not *suppress-character-coding-errors*)))
  "Convert STRING to octets using ENCODING."
  (check-type string string)
  (let* ((string (coerce string 'simple-unicode-string))
         (end (or end (length string)))
         (*suppress-character-coding-errors* (not errorp))
         (octet-count (count-encoding-octets encoding string start end))
         (result (make-array octet-count :element-type 'u8)))
    (encode-to-octets encoding string start end result 0)
    result))

(defun octets-to-string (octets &key (encoding :utf-8) (start 0) end
                                  (errorp (not *suppress-character-coding-errors*)))
  "Convert OCTETS to string using ENCODING."
  (check-type octets (vector u8))
  (let* ((end (or end (length octets)))
         (*suppress-character-coding-errors* (not errorp))
         (char-count (count-decoding-characters encoding octets start end))
         (result (make-string char-count :element-type 'unicode-char)))
    (decode-from-octets encoding octets start end result 0)
    result))

(defun encoding-supported-p (encoding)
  "Check if ENCODING is supported."
  (member encoding *supported-character-encodings*))

;;;; Utility Functions for Implementing Encodings

(defun fixed-width-octet-count (string start end &optional max)
  "Count octets for fixed-width encodings (1 octet per character)."
  (declare (type simple-unicode-string string)
           (fixnum start end))
  (let ((char-count (- end start)))
    (if (and max (plusp max) (> char-count max))
        (values max (+ start max))
        (values char-count end))))

(defun fixed-width-character-count (octets start end &optional max)
  "Count characters for fixed-width encodings (1 character per octet)."
  (declare (type ->u8 octets)
           (fixnum start end))
  (let ((octet-count (- end start)))
    (if (and max (plusp max) (> octet-count max))
        (values max (+ start max))
        (values octet-count end))))

;;;; Utilities for Fixed-Width Encodings

(defmacro define-unibyte-methods (encoding char-limit &key encode-body decode-body)
  "Define encoding methods for a fixed-width 8-bit encoding."
  `(progn
     (defmethod count-encoding-octets ((encoding (eql ,encoding)) string start end &optional max)
       (fixed-width-octet-count string start end max))
     
     (defmethod count-decoding-characters ((encoding (eql ,encoding)) octets start end &optional max)
       (fixed-width-character-count octets start end max))
     
     (defmethod encode-to-octets ((encoding (eql ,encoding)) string start end destination dest-start)
       (declare (type simple-unicode-string string)
                (type ->u8 destination)
                (fixnum start end dest-start))
       (loop for i fixnum from start below end
             and di fixnum from dest-start
             for code of-type code-point = (char-code (schar string i))
             do (u8-set (let ((result ,encode-body))
                          (if result result
                              (handle-encoding-error code ,encoding i)))
                        destination di)
             finally (return (the fixnum (- di dest-start)))))
     
     (defmethod decode-from-octets ((encoding (eql ,encoding)) octets start end destination dest-start)
       (declare (type ->u8 octets)
                (type simple-unicode-string destination)
                (fixnum start end dest-start))
       (loop for i fixnum from start below end
             and di fixnum from dest-start
             for octet of-type u8 = (u8-get octets i)
             do (string-set (let ((result ,decode-body))
                              (if result result
                                  (handle-decoding-error (vector octet) ,encoding i)))
                            destination di)
             finally (return (the fixnum (- di dest-start)))))))

;;;; Error Conditions
;;;;
;;; For now, we don't define any actual restarts.  The only mechanism
;;; for "restarting" a coding error is the
;;; *SUPPRESS-CHARACTER-CODING-ERRORS* special variable which, when
;;; bound to T (the default), suppresses any error and uses a default
;;; replacement character instead.

;;; All error conditions are subtypes of
;;; CHARACTER-CODING-ERROR.  This error hierarchy is based on SBCL's.

(define-condition character-coding-error (error)
  ((buffer :initarg :buffer :reader character-coding-error-buffer)
   (position :initarg :position :reader character-coding-error-position)
   (encoding :initarg :encoding :reader character-coding-error-encoding)))

(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code))
  (:report (lambda (c s)
             (format s "Unable to encode character code point ~A as ~S."
                     (character-encoding-error-code c)
                     (character-coding-error-encoding c)))))

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets))
  (:report (lambda (c s)
             (format s "Illegal ~S character starting at position ~D."
                     (character-coding-error-encoding c)
                     (character-coding-error-position c)))))

(define-condition end-of-input-in-character (character-decoding-error)
  ()
  (:documentation "Signalled by DECODERs or CODE-POINT-COUNTERs
of variable-width character encodings."))

(define-condition character-out-of-range (character-decoding-error)
  ()
  (:documentation
   "Signalled when the character being decoded is out of range."))

(defvar *default-eol-style*
  #+windows :crlf
  #-windows :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(deftype eol-style ()
  "Possible end-of-line styles."
  '(member :cr :lf :crlf))

(defclass encoding ()
  ((encoding :initarg :encoding :reader encoding-encoding
             :type character-encoding)
   (eol-style :initarg :eol-style :reader encoding-eol-style
              :type eol-style :initform *default-eol-style*))
  (:documentation
   "An ENCODING consists in a combination of a
   CHARACTER-ENCODING and an end-of-line style."))

(defmethod print-object ((ef encoding) stream)
  (print-unreadable-object (ef stream :type t :identity t)
    (format stream "~A ~A"
            (enc-name (encoding-encoding ef))
            (encoding-eol-style ef))))

(defun make-encoding (encoding &key (eol-style *default-eol-style*))
  (check-type eol-style eol-style)
  (make-instance 'encoding
                 :encoding (get-character-encoding encoding)
                 :eol-style eol-style))

(defun ensure-encoding (thing)
  (etypecase thing
    (encoding thing)
    (character-encoding (make-instance 'encoding :encoding thing))
    (symbol (make-encoding thing))
    (list (apply #'make-encoding thing))))

(defun encoding-equal (ef1 ef2)
  (and (eq (encoding-encoding ef1) (encoding-encoding ef2))
       (eq (encoding-eol-style ef1) (encoding-eol-style ef2))))

(defmacro string-get (string index)
  `(char-code (schar ,string ,index)))

(defmacro string-set (code string index)
  `(setf (schar ,string ,index) (code-char ,code)))

;;; SIMPLE-BASE-STRING would also be a subtype of SIMPLE-STRING so we
;;; don't use that because on SBCL BASE-CHARs can only hold ASCII.
;;; Also, with (> SPEED SAFETY) (setf (schar base-str n) big-char)
;;; will quietly work, sort of.

(defconstant unicode-char-code-limit
  char-code-limit
  "An alias for CL:CHAR-CODE-LIMIT which might be lower than
#x110000 on some Lisps.")

(deftype unicode-string ()
  "Alias for (VECTOR UNICODE-CHAR *)."
  '(vector unicode-char *))

;;;; New Interface Functions using simplified system

(defun bytes-to-string (vector &key (start 0) end
                             (errorp (not *suppress-character-coding-errors*))
                             (encoding *default-character-encoding*))
  "Convert byte VECTOR to string using ENCODING."
  (let ((coerced-vector (coerce vector '(simple-array u8 (*)))))
    (vector:with-checked-bounds ((coerced-vector coerced-vector) (start start) (end end))
      (declare (type ->u8 coerced-vector))
      (let ((*suppress-character-coding-errors* (not errorp)))
        (octets-to-string coerced-vector :encoding encoding :start start :end end :errorp errorp)))))

(defun string-to-bytes (string &key (encoding *default-character-encoding*)
                              (start 0) end (use-bom :default)
                              (errorp (not *suppress-character-coding-errors*)))
  "Convert string to U8 vector using ENCODING."
  (let ((*suppress-character-coding-errors* (not errorp))
        (bom (bom-vector encoding use-bom))
        (result (string-to-octets string :encoding encoding :start start :end end :errorp errorp)))
    (if (zerop (length bom))
        result
        (concatenate '(vector u8) bom result))))

(defun bom-vector (encoding use-bom)
  "Get BOM vector for encoding if needed."
  (check-type use-bom (member :default t nil))
  (the simple-vector
    (if (null use-bom)
        #()
        (let ((enc (typecase encoding
                     (encoding (encoding-encoding encoding))
                     (t (get-character-encoding encoding)))))
          (if (or (eq use-bom t)
                  (and (eq use-bom :default) (enc-use-bom enc)))
              ;; VALUES avoids a "type assertion too complex to check" note.
              (values (enc-bom-encoding enc))
              #())))))

(defun concatenate-strings-to-octets (encoding &rest strings)
  "Optimized equivalent of
\(string-to-bytes \(apply #'concatenate 'string strings)
                  :encoding encoding)"
  (declare (dynamic-extent strings))
  (let* ((total-string (apply #'concatenate 'string strings)))
    (string-to-octets total-string :encoding encoding)))

(defun string-size-in-bytes (string &key (start 0) end (max -1 maxp)
                              (errorp (not *suppress-character-coding-errors*))
                              (encoding *default-character-encoding*))
  "Get size in octets for string."
  (check-type string string)
  (vector:with-checked-bounds ((string (coerce string 'unicode-string))
                               (start start) (end end))
    (declare (type simple-unicode-string string))
    (let ((*suppress-character-coding-errors* (not errorp)))
      (when maxp (assert (plusp max)))
      (count-encoding-octets encoding string start end (when maxp max)))))

(defun vector-size-in-chars (vector &key (start 0) end (max -1 maxp)
                                      (errorp (not *suppress-character-coding-errors*))
                                      (encoding *default-character-encoding*))
  "Get size in characters for octet vector."
  (let ((vector (coerce vector '(simple-array u8 (*)))))
    (vector:with-checked-bounds ((vector vector) (start start) (end end))
      (declare (type ->u8 vector))
      (let ((*suppress-character-coding-errors* (not errorp)))
        (when maxp (assert (plusp max)))
        (count-decoding-characters encoding vector start end (when maxp max))))))

(defun standard-alpha-byte-p (byte)
  (declare (type u8 byte))
  (or (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\a) byte #.(char-code #\z))))

(defun standard-alpha-char-p (char)
  (declare (type character char))
  (standard-alpha-byte-p (char-code char)))

(defun standard-alphanumeric-p (char)
  (declare (type character char))
  (or (digit-char-p char)
      (standard-alpha-char-p char)))

(defun standard-alphanumeric-byte-p (byte)
  (declare (type u8 byte))
  (or (<= #.(char-code #\0) byte #.(char-code #\9))
      (standard-alpha-byte-p byte)))

;;; ASCII

(define-character-encoding :ascii
    "A 7-bit, fixed-width character encoding in which all
character codes map to their Unicode equivalents."
  :aliases '(:us-ascii))

;;;; ASCII Implementation using new simplified system

(define-unibyte-methods :ascii 128
  :encode-body (when (< code 128) code)
  :decode-body (when (< octet 128) octet))

;;; ISO-8859-1

(define-character-encoding :iso-8859-1
    "An 8-bit, fixed-width character encoding in which all
character codes map to their Unicode equivalents.  Intended to
support most characters used in most Western European languages."
  :aliases '(:latin-1 :latin1))

;;;; ISO-8859-1 Implementation using new simplified system
;;;; This is the simplest encoding - direct 1:1 mapping for all 256 codes

(define-unibyte-methods :iso-8859-1 256
  :encode-body (when (< code 256) code)
  :decode-body octet)
