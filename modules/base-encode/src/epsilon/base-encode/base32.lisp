;;;; epsilon.base-encode.base32 - Base32 encoding (RFC 4648 + Crockford)
;;;;
;;;; Provides general-purpose RFC 4648 base32 encoding/decoding with
;;;; configurable tables, plus specialized Crockford base32 functions
;;;; including the fixed 128-bit variant used by TypeID.

(defpackage epsilon.base-encode.base32
  (:use :cl)
  (:require (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.conditions cond))
  (:export
   ;; General base32
   #:encode
   #:decode

   ;; Crockford variable-length
   #:encode-crockford
   #:decode-crockford

   ;; Crockford fixed 128-bit (TypeID compatible)
   #:encode-crockford-128
   #:decode-crockford-128)
  (:enter t))

;;; ============================================================================
;;; RFC 4648 Base32
;;; ============================================================================

(defun encode (bytes &key (table tbl:+base32-encode+) (pad t))
  "Encode a byte vector to base32 string using TABLE.
   PAD controls whether = padding is appended."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type simple-string table))
  (let* ((input-length (length bytes))
         (complete-groups (truncate input-length 5))
         (remainder (mod input-length 5))
         ;; Each 5-byte group produces 8 base32 chars
         (unpadded-length (+ (* complete-groups 8)
                             (ecase remainder
                               (0 0) (1 2) (2 4) (3 5) (4 7))))
         (padded-length (if pad
                            (* 8 (ceiling input-length 5))
                            unpadded-length))
         (result (make-string padded-length :element-type 'base-char))
         (iout 0))
    (declare (type fixnum input-length complete-groups remainder
                         unpadded-length padded-length iout))
    ;; Process complete 5-byte groups
    (loop for g fixnum from 0 below complete-groups
          for isrc fixnum = (* g 5)
          do (let ((b0 (aref bytes isrc))
                   (b1 (aref bytes (+ isrc 1)))
                   (b2 (aref bytes (+ isrc 2)))
                   (b3 (aref bytes (+ isrc 3)))
                   (b4 (aref bytes (+ isrc 4))))
               (declare (type (unsigned-byte 8) b0 b1 b2 b3 b4))
               (setf (schar result iout)       (schar table (ash b0 -3)))
               (setf (schar result (+ iout 1)) (schar table (logand (logior (ash b0 2) (ash b1 -6)) #x1f)))
               (setf (schar result (+ iout 2)) (schar table (logand (ash b1 -1) #x1f)))
               (setf (schar result (+ iout 3)) (schar table (logand (logior (ash b1 4) (ash b2 -4)) #x1f)))
               (setf (schar result (+ iout 4)) (schar table (logand (logior (ash b2 1) (ash b3 -7)) #x1f)))
               (setf (schar result (+ iout 5)) (schar table (logand (ash b3 -2) #x1f)))
               (setf (schar result (+ iout 6)) (schar table (logand (logior (ash b3 3) (ash b4 -5)) #x1f)))
               (setf (schar result (+ iout 7)) (schar table (logand b4 #x1f)))
               (incf iout 8)))
    ;; Process remainder
    (let ((isrc (* complete-groups 5)))
      (when (plusp remainder)
        (let ((b0 (aref bytes isrc)))
          (declare (type (unsigned-byte 8) b0))
          (setf (schar result iout) (schar table (ash b0 -3)))
          (incf iout)
          (if (= remainder 1)
              (progn
                (setf (schar result iout) (schar table (logand (ash b0 2) #x1f)))
                (incf iout))
              (let ((b1 (aref bytes (+ isrc 1))))
                (declare (type (unsigned-byte 8) b1))
                (setf (schar result iout) (schar table (logand (logior (ash b0 2) (ash b1 -6)) #x1f)))
                (incf iout)
                (setf (schar result iout) (schar table (logand (ash b1 -1) #x1f)))
                (incf iout)
                (if (= remainder 2)
                    (progn
                      (setf (schar result iout) (schar table (logand (ash b1 4) #x1f)))
                      (incf iout))
                    (let ((b2 (aref bytes (+ isrc 2))))
                      (declare (type (unsigned-byte 8) b2))
                      (setf (schar result iout) (schar table (logand (logior (ash b1 4) (ash b2 -4)) #x1f)))
                      (incf iout)
                      (if (= remainder 3)
                          (progn
                            (setf (schar result iout) (schar table (logand (ash b2 1) #x1f)))
                            (incf iout))
                          (let ((b3 (aref bytes (+ isrc 3))))
                            (declare (type (unsigned-byte 8) b3))
                            (setf (schar result iout) (schar table (logand (logior (ash b2 1) (ash b3 -7)) #x1f)))
                            (incf iout)
                            (setf (schar result iout) (schar table (logand (ash b3 -2) #x1f)))
                            (incf iout)
                            (setf (schar result iout) (schar table (logand (ash b3 3) #x1f)))
                            (incf iout))))))))))
    ;; Add padding
    (when pad
      (loop while (< iout padded-length)
            do (setf (schar result iout) #\=)
               (incf iout)))
    result))

(defun decode (string &key (table tbl:+base32-decode+) (whitespace :ignore))
  "Decode a base32 string to a byte vector using TABLE.
   WHITESPACE can be :ignore (skip) or :error (signal)."
  (declare (type string string)
           (type tbl:decode-table table))
  ;; First pass: count valid characters to determine output size
  (let ((valid-count 0)
        (pad-count 0))
    (declare (type fixnum valid-count pad-count))
    (loop for i fixnum from 0 below (length string)
          for code fixnum = (char-code (char string i))
          for sval = (if (< code 128) (aref table code) -1)
          do (cond
               ((>= sval 0) (incf valid-count))
               ((= sval -2) (incf pad-count))
               ((= sval -3)
                (ecase whitespace
                  (:ignore nil)
                  (:error
                   (error 'cond:bad-character
                          :encoding-name "base32"
                          :input string
                          :position i
                          :character-code code))))
               (t
                (error 'cond:bad-character
                       :encoding-name "base32"
                       :input string
                       :position i
                       :character-code code))))
    (let* ((total-chars (+ valid-count pad-count))
           ;; 8 base32 chars = 5 bytes; compute output length accounting for padding
           (bit-count (* valid-count 5))
           (output-length (truncate bit-count 8))
           (result (make-array output-length :element-type '(unsigned-byte 8)))
           (bitstore 0)
           (bitcount 0)
           (rpos 0))
      (declare (type fixnum total-chars bit-count output-length bitstore bitcount rpos)
               (ignorable total-chars))
      ;; Second pass: decode
      (loop for i fixnum from 0 below (length string)
            for code fixnum = (char-code (char string i))
            for sval fixnum = (if (< code 128) (aref table code) -1)
            do (when (>= sval 0)
                 (setf bitstore (logior (the fixnum (ash bitstore 5)) sval))
                 (incf bitcount 5)
                 (when (>= bitcount 8)
                   (decf bitcount 8)
                   (setf (aref result rpos)
                         (the (unsigned-byte 8)
                              (logand (the fixnum (ash bitstore (- bitcount))) #xFF)))
                   (incf rpos)
                   (setf bitstore (logand bitstore (the fixnum (1- (ash 1 bitcount))))))))
      result)))

;;; ============================================================================
;;; Crockford Base32 (variable length)
;;; ============================================================================

(defun encode-crockford (bytes)
  "Encode a byte vector to variable-length Crockford base32.
   No padding is added."
  (encode bytes :table tbl:+base32-crockford-encode+ :pad nil))

(defun decode-crockford (string)
  "Decode a Crockford base32 string to a byte vector.
   Case-insensitive with confusion tolerance (i/l -> 1, o -> 0)."
  (decode string :table tbl:+base32-crockford-decode+ :whitespace :ignore))

;;; ============================================================================
;;; Crockford Base32 Fixed 128-bit (TypeID compatible)
;;; ============================================================================

(defun encode-crockford-128 (bytes)
  "Encode exactly 16 bytes to a 26-character Crockford base32 string.
   Uses the integer-shift approach for zero intermediate allocation.
   Compatible with epsilon.typeid:encode-base32."
  (declare (type (vector (unsigned-byte 8)) bytes))
  (assert (= (length bytes) 16))
  ;; Convert bytes to a 128-bit integer (big-endian)
  (let ((value 0))
    (dotimes (i 16)
      (setf value (logior (ash value 8) (aref bytes i))))
    ;; 26 chars * 5 bits = 130 bits, first char uses only 3 bits (values 0-7)
    (let ((result (make-string 26 :element-type 'base-char))
          (table tbl:+base32-crockford-encode+))
      (declare (type simple-string table result))
      (loop for i from 25 downto 0
            do (setf (schar result i)
                     (schar table (logand value #x1F)))
               (setf value (ash value -5)))
      result)))

(defun decode-crockford-128 (string)
  "Decode a 26-character Crockford base32 string to a 16-byte array.
   Returns NIL if the string is invalid.
   Compatible with epsilon.typeid:decode-base32."
  (when (and (stringp string) (= (length string) 26))
    (let ((table tbl:+base32-crockford-decode+))
      (declare (type tbl:decode-table table))
      ;; Verify first character is valid (0-7 only for 128 bits)
      (let ((first-code (char-code (char string 0))))
        (when (< first-code 128)
          (let ((first-val (aref table first-code)))
            (when (and (>= first-val 0) (<= first-val 7))
              ;; Decode all characters to a 128-bit integer
              (let ((value 0)
                    (valid t))
                (loop for i from 0 below 26
                      for c = (char string i)
                      for code = (char-code c)
                      while valid
                      do (if (or (>= code 128)
                                 (< (aref table code) 0))
                             (setf valid nil)
                             (setf value (logior (ash value 5)
                                                 (aref table code)))))
                (when valid
                  ;; Convert to 16-byte array (big-endian)
                  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
                    (loop for i from 15 downto 0
                          do (setf (aref bytes i) (logand value #xFF))
                             (setf value (ash value -8)))
                    bytes))))))))))
