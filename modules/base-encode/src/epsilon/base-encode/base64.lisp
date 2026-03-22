;;;; epsilon.base-encode.base64 - Base64 encoding (RFC 4648 + URL-safe)
;;;;
;;;; Single bytes->string encode and string->bytes decode functions
;;;; instead of the macro-generated 12-variant matrix in epsilon.base64.
;;;; Streaming is handled by streams.lisp via io.protocol.

(defpackage epsilon.base-encode.base64
  (:use :cl)
  (:require (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.conditions cond))
  (:export
   ;; Standard base64
   #:encode
   #:decode

   ;; URL-safe base64
   #:encode-url
   #:decode-url

   ;; String convenience
   #:encode-string
   #:decode-string)
  (:enter t))

;;; ============================================================================
;;; Encode
;;; ============================================================================

(defun encode (bytes &key (table tbl:+base64-encode+) (pad-char #\=) (columns 0))
  "Encode a byte vector to base64 string.
   TABLE selects the alphabet. PAD-CHAR is used for padding (nil to omit).
   COLUMNS > 0 inserts newlines at that width."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type simple-string table)
           (type fixnum columns))
  (let* ((input-length (length bytes))
         (complete-groups (truncate input-length 3))
         (remainder (mod input-length 3))
         (base-length (+ (* complete-groups 4)
                         (if (zerop remainder) 0
                             (if pad-char 4 (+ remainder 1)))))
         (num-breaks (if (plusp columns)
                         (max 0 (1- (ceiling base-length columns)))
                         0))
         (total-length (+ base-length num-breaks))
         (result (make-string total-length :element-type 'base-char))
         (iout 0)
         (col 0))
    (declare (type fixnum input-length complete-groups remainder
                         base-length num-breaks total-length iout col))
    (labels ((emit (ch)
               (when (and (plusp columns) (= col columns))
                 (setf (schar result iout) #\Newline)
                 (incf iout)
                 (setf col 0))
               (setf (schar result iout) ch)
               (incf iout)
               (incf col)))
      ;; Process complete 3-byte groups
      (loop for g fixnum from 0 below complete-groups
            for isrc fixnum = (* g 3)
            do (let ((b0 (aref bytes isrc))
                     (b1 (aref bytes (+ isrc 1)))
                     (b2 (aref bytes (+ isrc 2))))
                 (declare (type (unsigned-byte 8) b0 b1 b2))
                 (let ((triplet (the fixnum
                                     (logior (the fixnum (ash b0 16))
                                             (the fixnum (ash b1 8))
                                             b2))))
                   (declare (type fixnum triplet))
                   (emit (schar table (the fixnum (logand (ash triplet -18) #x3f))))
                   (emit (schar table (the fixnum (logand (ash triplet -12) #x3f))))
                   (emit (schar table (the fixnum (logand (ash triplet -6) #x3f))))
                   (emit (schar table (the fixnum (logand triplet #x3f)))))))
      ;; Process remainder
      (let ((isrc (* complete-groups 3)))
        (cond
          ((= remainder 2)
           (let* ((b0 (aref bytes isrc))
                  (b1 (aref bytes (+ isrc 1)))
                  (pair (the fixnum (logior (the fixnum (ash b0 8)) b1))))
             (declare (type (unsigned-byte 8) b0 b1)
                      (type fixnum pair))
             (emit (schar table (the fixnum (logand (ash pair -10) #x3f))))
             (emit (schar table (the fixnum (logand (ash pair -4) #x3f))))
             (emit (schar table (the fixnum (logand (ash pair 2) #x3f))))
             (when pad-char (emit pad-char))))
          ((= remainder 1)
           (let ((b0 (aref bytes isrc)))
             (declare (type (unsigned-byte 8) b0))
             (emit (schar table (the fixnum (logand (ash b0 -2) #x3f))))
             (emit (schar table (the fixnum (logand (ash b0 4) #x3f))))
             (when pad-char
               (emit pad-char)
               (emit pad-char)))))))
    result))

;;; ============================================================================
;;; Decode
;;; ============================================================================

(defun decode (string &key (table tbl:+base64-decode+) (whitespace :ignore))
  "Decode a base64 string to a byte vector.
   TABLE selects the decode table. WHITESPACE: :ignore or :error."
  (declare (type string string)
           (type tbl:decode-table table))
  (let* ((input-length (length string))
         ;; Estimate max output size (3 bytes per 4 chars)
         (max-output (the fixnum (* 3 (the fixnum (ceiling input-length 4)))))
         (result (make-array max-output :element-type '(unsigned-byte 8)))
         (bitstore 0)
         (bitcount 0)
         (rpos 0)
         (padcount 0))
    (declare (type fixnum input-length max-output bitstore bitcount rpos padcount))
    (loop for i fixnum from 0 below input-length
          for code fixnum = (char-code (char string i))
          do (let ((sval (if (< code 128) (aref table code) -1)))
               (declare (type (signed-byte 8) sval))
               (cond
                 ((>= sval 0)
                  ;; Valid base64 character
                  (when (plusp padcount)
                    (error 'cond:bad-character
                           :encoding-name "base64"
                           :input string
                           :position i
                           :character-code code))
                  (setf bitstore (the fixnum
                                      (logior (the fixnum (ash bitstore 6)) sval)))
                  (incf bitcount 6)
                  (when (>= bitcount 8)
                    (decf bitcount 8)
                    (setf (aref result rpos)
                          (the (unsigned-byte 8)
                               (logand (the fixnum (ash bitstore (- bitcount))) #xFF)))
                    (incf rpos)
                    (setf bitstore (logand bitstore
                                          (the fixnum (1- (the fixnum (ash 1 bitcount))))))))
                 ((= sval -2)
                  ;; Padding character
                  (when (<= (incf padcount) 2)
                    (when (< bitcount 2)
                      (error 'cond:bad-character
                             :encoding-name "base64"
                             :input string
                             :position i
                             :character-code code))
                    (decf bitcount 2)))
                 ((= sval -3)
                  ;; Whitespace
                  (ecase whitespace
                    (:ignore nil)
                    (:error
                     (error 'cond:bad-character
                            :encoding-name "base64"
                            :input string
                            :position i
                            :character-code code))))
                 (t
                  ;; Invalid character
                  (error 'cond:bad-character
                         :encoding-name "base64"
                         :input string
                         :position i
                         :character-code code)))))
    (if (= rpos max-output)
        result
        (subseq result 0 rpos))))

;;; ============================================================================
;;; URL-safe Variants
;;; ============================================================================

(defun encode-url (bytes &key (pad nil))
  "Encode bytes to URL-safe base64. No padding by default (JWT convention)."
  (encode bytes :table tbl:+base64url-encode+
               :pad-char (if pad #\= nil)
               :columns 0))

(defun decode-url (string)
  "Decode a URL-safe base64 string. Handles missing padding gracefully."
  (declare (type string string))
  ;; If no padding, add it for the decoder
  (let* ((len (length string))
         (rem (mod len 4))
         (padded (if (zerop rem)
                     string
                     (concatenate 'string string
                                  (make-string (- 4 rem) :initial-element #\=)))))
    (decode padded :table tbl:+base64url-decode+)))

;;; ============================================================================
;;; String Convenience
;;; ============================================================================

(defun encode-string (string &key (encoding :utf-8))
  "Encode a UTF-8 string to base64."
  (declare (type string string))
  (encode (ecase encoding
            (:utf-8 (sb-ext:string-to-octets string :external-format :utf-8))
            (:ascii (map '(simple-array (unsigned-byte 8) (*))
                         #'char-code string))
            (:latin-1 (sb-ext:string-to-octets string :external-format :latin-1)))))

(defun decode-string (b64-string &key (encoding :utf-8))
  "Decode a base64 string to a UTF-8 string."
  (declare (type string b64-string))
  (let ((bytes (decode b64-string)))
    (ecase encoding
      (:utf-8 (sb-ext:octets-to-string bytes :external-format :utf-8))
      (:ascii (map 'string #'code-char bytes))
      (:latin-1 (sb-ext:octets-to-string bytes :external-format :latin-1)))))
