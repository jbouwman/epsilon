(in-package #:epsilon.lib.char)

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

(declaim (inline lookup-mapping))
(defun lookup-mapping (ht encoding)
  "HT should be an hashtable created by
INSTANTIATE-CONCRETE-MAPPINGS. ENCODING should be either an
external format, an encoding object or a keyword symbol
denoting a character encoding name or one of its aliases."
  (or (etypecase encoding
        (keyword
         (gethash encoding ht))
        (epsilon.lib.char::concrete-mapping
         encoding)
        (character-encoding
         (gethash (enc-name encoding) ht))
        (encoding
         (gethash (enc-name (encoding-encoding encoding)) ht)))
      (error "~S is not a valid encoding designator" encoding)))
