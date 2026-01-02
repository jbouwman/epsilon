;;;; TypeID - Type-safe, K-sortable, globally unique identifiers
;;;;
;;;; TypeID combines a type prefix with a UUIDv7 encoded in Crockford's base32.
;;;; Format: {prefix}_{base32_suffix}
;;;; Example: user_2x4y6z8a0b1c2d3e4f5g6h7j8k
;;;;
;;;; Specification: https://github.com/jetify-com/typeid

(defpackage epsilon.typeid
  (:use cl)
  (:local-nicknames
   (uuid epsilon.uuid))
  (:export
   ;; TypeID type and predicates
   #:typeid
   #:typeid-p
   #:valid-typeid-p
   #:valid-prefix-p

   ;; TypeID creation
   #:make-typeid
   #:make-from-string
   #:make-from-uuid

   ;; TypeID accessors
   #:typeid-prefix
   #:typeid-suffix
   #:typeid-uuid

   ;; TypeID conversion
   #:to-string
   #:to-uuid

   ;; Timestamp extraction
   #:typeid-timestamp-ms

   ;; Base32 encoding/decoding (Crockford's)
   #:encode-base32
   #:decode-base32))

(in-package epsilon.typeid)

;;; Crockford's Base32 Encoding
;;; Alphabet: 0123456789abcdefghjkmnpqrstvwxyz (excludes i, l, o, u)

(defconstant +base32-alphabet+ "0123456789abcdefghjkmnpqrstvwxyz"
  "Crockford's base32 alphabet (lowercase).")

(defconstant +base32-decode-table+
  (let ((table (make-array 128 :element-type '(signed-byte 8) :initial-element -1)))
    ;; Map each character to its value
    (loop for i from 0 below 32
          for c = (char +base32-alphabet+ i)
          do (setf (aref table (char-code c)) i)
             ;; Also handle uppercase
             (setf (aref table (char-code (char-upcase c))) i))
    ;; Handle common confusions (i/I -> 1, l/L -> 1, o/O -> 0)
    (setf (aref table (char-code #\i)) 1)
    (setf (aref table (char-code #\I)) 1)
    (setf (aref table (char-code #\l)) 1)
    (setf (aref table (char-code #\L)) 1)
    (setf (aref table (char-code #\o)) 0)
    (setf (aref table (char-code #\O)) 0)
    table)
  "Decoding table for Crockford's base32.")

(defun encode-base32 (bytes)
  "Encode a 16-byte array to a 26-character Crockford's base32 string.
   The encoding treats the bytes as a big-endian 128-bit integer."
  (assert (= (length bytes) 16))
  ;; Convert bytes to a 128-bit integer (big-endian)
  (let ((value 0))
    (dotimes (i 16)
      (setf value (logior (ash value 8) (aref bytes i))))
    ;; Encode as base32 (26 characters for 128 bits, with 2 bits padding)
    ;; Each base32 character represents 5 bits
    ;; 26 * 5 = 130 bits, so the first character only uses 3 bits (values 0-7)
    (let ((result (make-string 26)))
      (loop for i from 25 downto 0
            do (setf (char result i) (char +base32-alphabet+ (logand value #x1F)))
               (setf value (ash value -5)))
      result)))

(defun decode-base32 (string)
  "Decode a 26-character Crockford's base32 string to a 16-byte array.
   Returns NIL if the string is invalid."
  (when (and (stringp string) (= (length string) 26))
    ;; Verify first character is valid (0-7 only, since we have 128 bits)
    (let ((first-val (aref +base32-decode-table+ (char-code (char string 0)))))
      (when (and (>= first-val 0) (<= first-val 7))
        ;; Decode all characters to a 128-bit integer
        (let ((value 0)
              (valid t))
          (loop for i from 0 below 26
                for c = (char string i)
                for code = (char-code c)
                while valid
                do (if (or (>= code 128)
                           (< (aref +base32-decode-table+ code) 0))
                       (setf valid nil)
                       (setf value (logior (ash value 5)
                                           (aref +base32-decode-table+ code)))))
          (when valid
            ;; Convert to 16-byte array (big-endian)
            (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
              (loop for i from 15 downto 0
                    do (setf (aref bytes i) (logand value #xFF))
                       (setf value (ash value -8)))
              bytes)))))))

;;; TypeID Structure

(defstruct (typeid (:constructor %make-typeid)
                   (:print-function print-typeid))
  "A TypeID consisting of a type prefix and a UUIDv7-based suffix."
  (prefix "" :type string :read-only t)
  (suffix "" :type string :read-only t))

(defun print-typeid (typeid stream depth)
  "Print a TypeID in the standard format: prefix_suffix"
  (declare (ignore depth))
  (if (string= (typeid-prefix typeid) "")
      (format stream "~A" (typeid-suffix typeid))
      (format stream "~A_~A" (typeid-prefix typeid) (typeid-suffix typeid))))

;;; Prefix Validation

(defun valid-prefix-p (prefix)
  "Check if a string is a valid TypeID prefix.
   Rules:
   - Must be 0-63 characters
   - Must contain only lowercase letters (a-z) and underscores
   - Cannot start or end with an underscore
   - Cannot be empty (use empty string for no prefix)"
  (and (stringp prefix)
       (<= (length prefix) 63)
       (or (= (length prefix) 0)
           (and (every (lambda (c)
                         (or (and (char>= c #\a) (char<= c #\z))
                             (char= c #\_)))
                       prefix)
                (char/= (char prefix 0) #\_)
                (char/= (char prefix (1- (length prefix))) #\_)))))

;;; TypeID Creation

(defun make-typeid (prefix &optional timestamp-ms)
  "Create a new TypeID with the given prefix and a fresh UUIDv7.
   Optional TIMESTAMP-MS allows specifying a custom timestamp."
  (assert (valid-prefix-p prefix) (prefix) "Invalid TypeID prefix: ~S" prefix)
  (let* ((uuid (uuid:make-v7 timestamp-ms))
         (bytes (uuid:to-bytes uuid))
         (suffix (encode-base32 bytes)))
    (%make-typeid :prefix prefix :suffix suffix)))

(defun make-from-uuid (prefix uuid)
  "Create a TypeID from a prefix and an existing UUID."
  (assert (valid-prefix-p prefix) (prefix) "Invalid TypeID prefix: ~S" prefix)
  (assert (uuid:uuid-p uuid) (uuid) "Expected a UUID, got: ~S" uuid)
  (let* ((bytes (uuid:to-bytes uuid))
         (suffix (encode-base32 bytes)))
    (%make-typeid :prefix prefix :suffix suffix)))

(defun make-from-string (string)
  "Parse a TypeID from its string representation.
   Returns NIL if the string is invalid."
  (when (stringp string)
    (let ((underscore-pos (position #\_ string :from-end t)))
      (cond
        ;; No underscore - entire string is the suffix (no prefix)
        ((null underscore-pos)
         (when (= (length string) 26)
           (let ((bytes (decode-base32 string)))
             (when bytes
               (%make-typeid :prefix "" :suffix string)))))
        ;; Has underscore - split into prefix and suffix
        (t
         (let ((prefix (subseq string 0 underscore-pos))
               (suffix (subseq string (1+ underscore-pos))))
           (when (and (valid-prefix-p prefix)
                      (= (length suffix) 26)
                      (decode-base32 suffix))
             (%make-typeid :prefix prefix :suffix suffix))))))))

;;; TypeID Conversion

(defun to-string (typeid)
  "Convert a TypeID to its string representation."
  (if (string= (typeid-prefix typeid) "")
      (typeid-suffix typeid)
      (format nil "~A_~A" (typeid-prefix typeid) (typeid-suffix typeid))))

(defun to-uuid (typeid)
  "Extract the UUID from a TypeID."
  (let ((bytes (decode-base32 (typeid-suffix typeid))))
    (uuid:make-from-bytes bytes)))

(defun typeid-uuid (typeid)
  "Get the UUID component of a TypeID."
  (to-uuid typeid))

;;; Timestamp Extraction

(defun typeid-timestamp-ms (typeid)
  "Extract the Unix timestamp in milliseconds from a TypeID.
   Returns NIL if the underlying UUID is not version 7."
  (uuid:uuid-v7-timestamp-ms (to-uuid typeid)))

;;; Validation

(defun valid-typeid-p (object)
  "Check if an object is a valid TypeID or TypeID string."
  (typecase object
    (typeid t)
    (string (not (null (make-from-string object))))
    (t nil)))
