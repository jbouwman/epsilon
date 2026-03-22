;;;; epsilon.base-encode.base16 - Hexadecimal encoding
;;;;
;;;; O(1) table-lookup encode and decode for base16 (hex).
;;;; Replaces epsilon.hex with proper decode tables instead of
;;;; O(16) position scans.

(defpackage epsilon.base-encode.base16
  (:use :cl)
  (:require (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.conditions cond))
  (:export
   #:encode
   #:decode
   #:encode-byte
   #:decode-byte)
  (:enter t))

;;; ============================================================================
;;; Single-byte Operations (inlineable)
;;; ============================================================================

(declaim (inline encode-byte decode-byte))

(defun encode-byte (byte &key (uppercase nil))
  "Encode a single byte to two hex characters.
   Returns (values high-char low-char)."
  (declare (type (unsigned-byte 8) byte))
  (let ((table (if uppercase "0123456789ABCDEF" tbl:+base16-encode+)))
    (declare (type simple-string table))
    (values (schar table (ldb (byte 4 4) byte))
            (schar table (ldb (byte 4 0) byte)))))

(defun decode-byte (hi lo)
  "Decode two hex characters to a single byte."
  (declare (type character hi lo))
  (let ((table tbl:+base16-decode+)
        (hi-code (char-code hi))
        (lo-code (char-code lo)))
    (declare (type tbl:decode-table table)
             (type fixnum hi-code lo-code))
    (let ((hi-val (if (< hi-code 128) (aref table hi-code) -1))
          (lo-val (if (< lo-code 128) (aref table lo-code) -1)))
      (declare (type (signed-byte 8) hi-val lo-val))
      (when (or (< hi-val 0) (< lo-val 0))
        (error 'cond:bad-character
               :encoding-name "base16"
               :character-code (if (< hi-val 0) hi-code lo-code)
               :position 0))
      (the (unsigned-byte 8)
           (logior (the fixnum (ash hi-val 4)) lo-val)))))

;;; ============================================================================
;;; Bulk Encode
;;; ============================================================================

(defun encode (bytes &key (start 0) (end nil) (uppercase nil))
  "Encode a byte vector to a hexadecimal string.
   Pre-allocates exact output size. O(1) per byte via table lookup."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type fixnum start))
  (let* ((end (or end (length bytes)))
         (length (- end start))
         (table (if uppercase "0123456789ABCDEF" tbl:+base16-encode+))
         (result (make-string (* length 2) :element-type 'base-char)))
    (declare (type fixnum end length)
             (type simple-string table result))
    (loop for i fixnum from start below end
          for j fixnum from 0 by 2
          do (let ((byte (aref bytes i)))
               (declare (type (unsigned-byte 8) byte)
                        (optimize (speed 3) (safety 0)))
               (setf (schar result j)
                     (schar table (ldb (byte 4 4) byte)))
               (setf (schar result (the fixnum (1+ j)))
                     (schar table (ldb (byte 4 0) byte)))))
    result))

;;; ============================================================================
;;; Bulk Decode
;;; ============================================================================

(defun decode (string &key (start 0) (end nil))
  "Decode a hexadecimal string to a byte vector.
   O(1) per character via decode table lookup."
  (declare (type string string)
           (type fixnum start))
  (let* ((end (or end (length string)))
         (input-length (- end start))
         (table tbl:+base16-decode+))
    (declare (type fixnum end input-length)
             (type tbl:decode-table table))
    (when (oddp input-length)
      (error 'cond:invalid-length
             :encoding-name "base16"
             :input string
             :position input-length
             :expected-length "even"))
    (let* ((output-length (ash input-length -1))
           (result (make-array output-length :element-type '(unsigned-byte 8))))
      (declare (type fixnum output-length))
      (loop for i fixnum from start below end by 2
            for j fixnum from 0
            do (let* ((hi-code (char-code (char string i)))
                      (lo-code (char-code (char string (the fixnum (1+ i)))))
                      (hi-val (if (< hi-code 128) (aref table hi-code) -1))
                      (lo-val (if (< lo-code 128) (aref table lo-code) -1)))
                 (declare (type fixnum hi-code lo-code)
                          (type (signed-byte 8) hi-val lo-val)
                          (optimize (speed 3) (safety 0)))
                 (when (< hi-val 0)
                   (error 'cond:bad-character
                          :encoding-name "base16"
                          :input string
                          :position i
                          :character-code hi-code))
                 (when (< lo-val 0)
                   (error 'cond:bad-character
                          :encoding-name "base16"
                          :input string
                          :position (1+ i)
                          :character-code lo-code))
                 (setf (aref result j)
                       (the (unsigned-byte 8)
                            (logior (the fixnum (ash hi-val 4)) lo-val)))))
      result)))
