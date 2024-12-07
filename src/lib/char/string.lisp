(in-package #:epsilon.lib.char)

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

(deftype unicode-char ()
  "This character type can hold any characters whose CHAR-CODEs
are less than UNICODE-CHAR-CODE-LIMIT."
  'character)

(deftype simple-unicode-string ()
  "Alias for (SIMPLE-ARRAY UNICODE-CHAR (*))."
  '(simple-array unicode-char (*)))

(deftype unicode-string ()
  "Alias for (VECTOR UNICODE-CHAR *)."
  '(vector unicode-char *))

(defparameter *string-vector-mappings*
  (instantiate-concrete-mappings
   :octet-seq-setter u8-set
   :octet-seq-getter u8-get
   :octet-seq-type ->u8
   :code-point-seq-setter string-set
   :code-point-seq-getter string-get
   :code-point-seq-type simple-unicode-string))

(defparameter *simple-base-string-vector-mappings*
  (instantiate-concrete-mappings
   :instantiate-decoders nil
   :octet-seq-setter u8-set
   :octet-seq-getter u8-get
   :octet-seq-type ->u8
   :code-point-seq-setter string-set
   :code-point-seq-getter string-get
   :code-point-seq-type simple-base-string))

;;; Future features these functions should have:
;;;
;;;   * null-terminate
;;;   * specify target vector/string + offset
;;;   * documentation :)

(declaim (inline u8-to-string string-to-u8 string-size-in-octets
                 vector-size-in-chars concatenate-strings-to-octets
                 bom-vector))

(defun u8-to-string (vector &key (start 0) end
                              (errorp (not *suppress-character-coding-errors*))
                              (encoding *default-character-encoding*))
  (check-type vector (vector u8))
  (with-checked-bounds ((vector vector) (start start) (end end))
    (declare (type ->u8 vector))
    (let ((*suppress-character-coding-errors* (not errorp))
          (mapping (lookup-mapping *string-vector-mappings* encoding)))
      (multiple-value-bind (size new-end)
          (funcall (code-point-counter mapping) vector start end -1)
        ;; TODO we could optimize ASCII here: the result should
        ;; be a simple-base-string filled using code-char...
        (let ((string (make-string size :element-type 'unicode-char)))
          (funcall (decoder mapping) vector start new-end string 0)
          string)))))

(defun bom-vector (encoding use-bom)
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

(defun string-to-u8 (string &key (encoding *default-character-encoding*)
                              (start 0) end (use-bom :default)
                              (errorp (not *suppress-character-coding-errors*)))
  (let ((*suppress-character-coding-errors* (not errorp)))
    (etypecase string
      (simple-base-string
       (unless end
         (setf end (length string)))
       (check-bounds string start end)
       (let* ((mapping (lookup-mapping *simple-base-string-vector-mappings*
                                       encoding))
              (bom (bom-vector encoding use-bom))
              (bom-length (length bom))
              ;; OPTIMIZE: we could use the (length string) information here
              ;; because it's a simple-base-string where each character <= 127
              (result (make-array
                       (+ (the array-index
                            (funcall (the function (octet-counter mapping))
                                     string start end -1))
                          bom-length)
                       :element-type 'u8)))
         (replace result bom)
         (funcall (the function (encoder mapping))
                  string start end result bom-length)
         result))
      (string
       ;; FIXME: we shouldn't really need that coercion to UNICODE-STRING
       ;; but we kind of because it's declared all over.  To avoid that,
       ;; we'd need different types for input and output strings.  Or maybe
       ;; this is not a problem; figure that out.
       (with-checked-bounds ((string (coerce string 'unicode-string))
                             (start start) (end end))
         (declare (type simple-unicode-string string))
         (let* ((mapping (lookup-mapping *string-vector-mappings* encoding))
                (bom (bom-vector encoding use-bom))
                (bom-length (length bom))
                (result (make-array
                         (+ (the array-index
                              (funcall (the function (octet-counter mapping))
                                       string start end -1))
                            bom-length)
                         :element-type 'u8)))
           (replace result bom)
           (funcall (the function (encoder mapping))
                    string start end result bom-length)
           result))))))

(defun concatenate-strings-to-octets (encoding &rest strings)
  "Optimized equivalent of
\(string-to-u8 \(apply #'concatenate 'string strings)
                  :encoding encoding)"
  (declare (dynamic-extent strings))
  (let* ((mapping (lookup-mapping *string-vector-mappings* encoding))
         (octet-counter (octet-counter mapping))
         (vector (make-array
                  (the array-index
                    (reduce #'+ strings
                            :key (lambda (string)
                                   (funcall octet-counter
                                            string 0 (length string) -1))))
                  :element-type 'u8))
         (current-index 0))
    (declare (type array-index current-index))
    (dolist (string strings)
      (check-type string string)
      (with-checked-bounds ((string (coerce string 'unicode-string))
                                   (start 0) (end (length string)))
        (declare (type simple-unicode-string string))
        (incf current-index
              (funcall (encoder mapping)
                       string start end vector current-index))))
    vector))

(defun string-size-in-octets (string &key (start 0) end (max -1 maxp)
                              (errorp (not *suppress-character-coding-errors*))
                              (encoding *default-character-encoding*))
  (check-type string string)
  (with-checked-bounds ((string (coerce string 'unicode-string))
                               (start start) (end end))
    (declare (type simple-unicode-string string))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (when maxp (assert (plusp max)))
      (funcall (octet-counter mapping) string start end max))))

(defun vector-size-in-chars (vector &key (start 0) end (max -1 maxp)
                             (errorp (not *suppress-character-coding-errors*))
                             (encoding *default-character-encoding*))
  (check-type vector (vector u8))
  (with-checked-bounds ((vector vector) (start start) (end end))
    (declare (type ->u8 vector))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (when maxp (assert (plusp max)))
      (funcall (code-point-counter mapping) vector start end max))))

(declaim (notinline u8-to-string string-to-u8 string-size-in-octets
                    vector-size-in-chars concatenate-strings-to-octets))

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
