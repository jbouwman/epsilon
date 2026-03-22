;;;; epsilon.base-encode.bech32 - BIP-173 Bech32 encoding
;;;;
;;;; Full encoder and decoder for Bech32, including BCH checksum
;;;; computation and verification. Extends the decode-only
;;;; implementation from epsilon.crypto.age.

(defpackage epsilon.base-encode.bech32
  (:use :cl)
  (:require (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.conditions cond))
  (:export
   #:encode
   #:decode
   #:polymod
   #:hrp-expand)
  (:enter t))

;;; ============================================================================
;;; BCH Checksum
;;; ============================================================================

(defun polymod (values)
  "Compute the Bech32 BCH checksum polymod over a list of integer values."
  (let ((chk 1))
    (dolist (v values chk)
      (let ((b (ash chk -25)))
        (setf chk (logxor (ash (logand chk #x1ffffff) 5) v))
        (when (logbitp 0 b) (setf chk (logxor chk #x3b6a57b2)))
        (when (logbitp 1 b) (setf chk (logxor chk #x26508e6d)))
        (when (logbitp 2 b) (setf chk (logxor chk #x1ea119fa)))
        (when (logbitp 3 b) (setf chk (logxor chk #x3d4233dd)))
        (when (logbitp 4 b) (setf chk (logxor chk #x2a1462b3)))))))

(defun hrp-expand (hrp)
  "Expand the human-readable part for checksum computation.
   Returns a list: high-bits of each char, 0 separator, low-bits of each char."
  (let ((result nil))
    (loop for ch across hrp
          do (push (ash (char-code ch) -5) result))
    (push 0 result)
    (loop for ch across hrp
          do (push (logand (char-code ch) 31) result))
    (nreverse result)))

(defun create-checksum (hrp data-values &key (variant :bech32))
  "Compute the 6-value Bech32 checksum for HRP and DATA-VALUES.
   VARIANT is :bech32 (constant 1) or :bech32m (constant #x2bc830a3)."
  (let* ((constant (ecase variant
                     (:bech32 1)
                     (:bech32m #x2bc830a3)))
         (values (append (hrp-expand hrp) data-values (list 0 0 0 0 0 0)))
         (polymod-val (logxor (polymod values) constant)))
    (loop for i from 0 below 6
          collect (logand (ash polymod-val (- (* (- 5 i) 5))) 31))))

(defun verify-checksum (hrp data-values &key (variant :bech32))
  "Verify a Bech32 checksum. Returns T if valid."
  (let ((constant (ecase variant
                    (:bech32 1)
                    (:bech32m #x2bc830a3))))
    (= constant (polymod (append (hrp-expand hrp) data-values)))))

;;; ============================================================================
;;; Bit Conversion
;;; ============================================================================

(defun convert-bits (data from-bits to-bits &optional (pad t))
  "Convert a list of values from FROM-BITS width to TO-BITS width.
   Returns a byte vector."
  (let ((acc 0)
        (bits 0)
        (result nil)
        (maxv (1- (ash 1 to-bits))))
    (dolist (value data)
      (setf acc (logior (ash acc from-bits) value))
      (incf bits from-bits)
      (loop while (>= bits to-bits)
            do (decf bits to-bits)
               (push (logand (ash acc (- bits)) maxv) result)))
    (when (and pad (plusp bits))
      (push (logand (ash acc (- to-bits bits)) maxv) result))
    (let ((bytes (nreverse result)))
      (make-array (length bytes)
                  :element-type '(unsigned-byte 8)
                  :initial-contents bytes))))

;;; ============================================================================
;;; Encode
;;; ============================================================================

(defun encode (hrp data-bytes &key (variant :bech32))
  "Encode HRP and DATA-BYTES into a Bech32 string.
   HRP is the human-readable part (lowercase string).
   DATA-BYTES is a byte vector.
   VARIANT is :bech32 or :bech32m."
  (declare (type string hrp)
           (type (vector (unsigned-byte 8)) data-bytes))
  (let* ((table tbl:+bech32-encode+)
         ;; Convert 8-bit bytes to 5-bit groups
         (data-5bit (let ((acc 0) (bits 0) (result nil))
                      (loop for byte across data-bytes
                            do (setf acc (logior (ash acc 8) byte))
                               (incf bits 8)
                               (loop while (>= bits 5)
                                     do (decf bits 5)
                                        (push (logand (ash acc (- bits)) 31) result)))
                      (when (plusp bits)
                        (push (logand (ash acc (- 5 bits)) 31) result))
                      (nreverse result)))
         ;; Compute checksum
         (checksum (create-checksum hrp data-5bit :variant variant))
         ;; Build result: hrp + "1" + data-chars + checksum-chars
         (data-and-checksum (append data-5bit checksum))
         (result-length (+ (length hrp) 1 (length data-and-checksum)))
         (result (make-string result-length :element-type 'base-char)))
    (declare (type simple-string table result))
    ;; Write HRP (lowercase)
    (loop for i from 0 below (length hrp)
          do (setf (schar result i) (char-downcase (char hrp i))))
    ;; Write separator
    (setf (schar result (length hrp)) #\1)
    ;; Write data + checksum
    (loop for val in data-and-checksum
          for i from (1+ (length hrp))
          do (setf (schar result i) (schar table val)))
    result))

;;; ============================================================================
;;; Decode
;;; ============================================================================

(defun decode (bech32-string)
  "Decode a Bech32-encoded string.
   Returns (values hrp data-bytes) where data-bytes is the decoded payload.
   Signals encoding-error on invalid input."
  (declare (type string bech32-string))
  (let* ((str (string-upcase bech32-string))
         (last-1 (position #\1 str :from-end t)))
    ;; Validate structure
    (unless last-1
      (error 'cond:encoding-error
             :encoding-name "bech32"
             :input bech32-string
             :position 0))
    (when (< last-1 1)
      (error 'cond:encoding-error
             :encoding-name "bech32"
             :input bech32-string
             :position 0))
    (when (< (- (length str) last-1 1) 6)
      (error 'cond:incomplete-input
             :encoding-name "bech32"
             :input bech32-string
             :position (length str)))

    (let* ((hrp (string-downcase (subseq str 0 last-1)))
           (data-part (subseq str (1+ last-1)))
           (table tbl:+bech32-decode+)
           (data-values nil))
      (declare (type tbl:decode-table table))
      ;; Decode the data characters to 5-bit values
      (loop for i from 0 below (length data-part)
            for ch = (char data-part i)
            for code = (char-code (char-downcase ch))
            do (when (or (>= code 128)
                         (= -1 (aref table code)))
                 (error 'cond:bad-character
                        :encoding-name "bech32"
                        :input bech32-string
                        :position (+ last-1 1 i)
                        :character-code code))
               (push (aref table code) data-values))
      (setf data-values (nreverse data-values))

      ;; Verify checksum
      (unless (verify-checksum hrp data-values)
        (error 'cond:checksum-error
               :encoding-name "bech32"
               :input bech32-string
               :position (length bech32-string)
               :expected 1
               :actual (polymod (append (hrp-expand hrp) data-values))))

      ;; Remove the 6-character checksum
      (let ((payload-5bit (butlast data-values 6)))
        ;; Convert from 5-bit groups to 8-bit bytes
        (values hrp (convert-bits payload-5bit 5 8 nil))))))
