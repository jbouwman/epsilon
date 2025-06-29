(defpackage :epsilon.lib.encoding.unicode
  (:use
   :cl
   :epsilon.lib.type
   :epsilon.lib.char))

(in-package #:epsilon.lib.encoding.unicode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +repl+ #xfffd "Unicode replacement character code point.")
  (defconstant +byte-order-mark-code+ #xfeff)
  (defconstant +swapped-byte-order-mark-code+ #xfffe)
  (defconstant +swapped-byte-order-mark-code-32+ #xfffe0000))

;;;; UTF-8

(define-character-encoding :utf-8
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 4 bytes."
  :max-units-per-char 4
  :bom-encoding #(#xef #xbb #xbf)
  :default-replacement #xfffd)

(define-condition invalid-utf8-starter-byte (character-decoding-error)
  ()
  (:documentation "Signalled when an invalid UTF-8 starter byte is found."))

(define-condition invalid-utf8-continuation-byte (character-decoding-error)
  ()
  (:documentation
   "Signalled when an invalid UTF-8 continuation byte is found."))

(define-condition overlong-utf8-sequence (character-decoding-error)
  ()
  (:documentation "Signalled upon overlong UTF-8 sequences."))

;;;; UTF-8 Implementation

(defmethod count-encoding-octets ((encoding (eql :utf-8)) string start end &optional max)
  "UTF-8: variable width encoding."
  (declare (type simple-unicode-string string)
           (fixnum start end))
  (loop with noctets fixnum = 0
        for i fixnum from start below end
        for code of-type code-point = (char-code (schar string i))
        for char-octets fixnum = (cond ((< code #x80) 1)
                                       ((< code #x800) 2)  
                                       ((< code #x10000) 3)
                                       (t 4))
        do (let ((new-total (+ noctets char-octets)))
             (when (and max (plusp max) (> new-total max))
               (return (values noctets i)))
             (setf noctets new-total))
        finally (return (values noctets i))))

(defmethod encode-to-octets ((encoding (eql :utf-8)) string start end destination dest-start)
  "UTF-8 encoder: variable-width encoding."
  (declare (type simple-unicode-string string)
           (type ->u8 destination)
           (fixnum start end dest-start))
  (loop with di fixnum = dest-start
        for i fixnum from start below end
        for code of-type code-point = (char-code (schar string i))
        do (cond
             ;; 1 octet: 0xxxxxxx
             ((< code #x80)
              (u8-set code destination di)
              (incf di))
             ;; 2 octets: 110xxxxx 10xxxxxx
             ((< code #x800)
              (u8-set (logior #xc0 (ash code -6)) destination di)
              (u8-set (logior #x80 (logand code #x3f)) destination (1+ di))
              (incf di 2))
             ;; 3 octets: 1110xxxx 10xxxxxx 10xxxxxx
             ((< code #x10000)
              (u8-set (logior #xe0 (ash code -12)) destination di)
              (u8-set (logior #x80 (logand #x3f (ash code -6))) destination (1+ di))
              (u8-set (logior #x80 (logand code #x3f)) destination (+ di 2))
              (incf di 3))
             ;; 4 octets: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             (t
              (u8-set (logior #xf0 (logand #x07 (ash code -18))) destination di)
              (u8-set (logior #x80 (logand #x3f (ash code -12))) destination (1+ di))
              (u8-set (logior #x80 (logand #x3f (ash code -6))) destination (+ di 2))
              (u8-set (logior #x80 (logand code #x3f)) destination (+ di 3))
              (incf di 4)))
        finally (return (the fixnum (- di dest-start)))))

(defmethod count-decoding-characters ((encoding (eql :utf-8)) octets start end &optional max)
  "Count characters in UTF-8 sequence."
  (declare (type ->u8 octets) (fixnum start end))
  (loop with nchars fixnum = 0
        with i fixnum = start
        while (< i end) do
        (let* ((octet (u8-get octets i))
               (next-i (+ i (cond ((< octet #xc0) 1)
                                  ((< octet #xe0) 2)
                                  ((< octet #xf0) 3)
                                  (t 4)))))
          (declare (type u8 octet) (fixnum next-i))
          (cond
            ((> next-i end)
             (handle-decoding-error (vector octet) :utf-8 i nil)
             (return (values (1+ nchars) end)))
            (t
             (setq nchars (1+ nchars)
                   i next-i)
             (when (and max (plusp max) (= nchars max))
               (return (values nchars i))))))
        finally (return (values nchars i))))

(defmethod decode-from-octets ((encoding (eql :utf-8)) octets start end destination dest-start)
  "UTF-8 decoder."
  (declare (type ->u8 octets)
           (type simple-unicode-string destination)
           (fixnum start end dest-start))
  (loop with i fixnum = start
        for di fixnum from dest-start
        while (< i end)
        do (let ((u1 (u8-get octets i)))
             (declare (type u8 u1))
             (macrolet ((consume-octet ()
                          `(if (>= i end)
                               (return-from decode-char
                                 (handle-decoding-error (vector u1) :utf-8 i +unicode-replacement-code-point+))
                               (prog1 (u8-get octets i) (incf i))))
                        (invalid-cb-p (octet)
                          `(not (< #x7f ,octet #xc0)))
                        (handle-error (octets-vec &optional (condition 'character-decoding-error))
                          `(handle-decoding-error ,octets-vec :utf-8 i +unicode-replacement-code-point+ ',condition)))
               (string-set
                (block decode-char
                  (incf i) ; consume u1
                  (cond
                    ((< u1 #x80) u1)    ; 1 octet
                    ((< u1 #xc0)
                     (handle-error (vector u1) invalid-utf8-starter-byte))
                    (t
                     (let ((u2 (consume-octet)))
                       (when (invalid-cb-p u2)
                         (return-from decode-char
                           (handle-error (vector u1 u2) invalid-utf8-continuation-byte)))
                       (cond
                         ((< u1 #xc2)
                          (handle-error (vector u1 u2) 'overlong-utf8-sequence))
                         ((< u1 #xe0)     ; 2 octets
                          (logior (ash (logand #x1f u1) 6)
                                  (logxor u2 #x80)))
                         (t
                          (let ((u3 (consume-octet)))
                            (when (invalid-cb-p u3)
                              (return-from decode-char
                                (handle-error (vector u1 u2 u3) invalid-utf8-continuation-byte)))
                            (cond
                              ((and (= u1 #xe0) (< u2 #xa0))
                               (handle-error (vector u1 u2 u3) 'overlong-utf8-sequence))
                              ((< u1 #xf0)  ; 3 octets
                               (let ((start (logior (ash (logand u1 #x0f) 12)
                                                      (ash (logand u2 #x3f) 6))))
                                 (if (<= #xd800 start #xdfc0)
                                     (handle-error (vector u1 u2 u3) character-out-of-range)
                                     (logior start (logand u3 #x3f)))))
                              (t            ; 4 octets
                               (let ((u4 (consume-octet)))
                                 (when (invalid-cb-p u4)
                                   (return-from decode-char
                                     (handle-error (vector u1 u2 u3 u4) invalid-utf8-continuation-byte)))
                                 (cond
                                   ((and (= u1 #xf0) (< u2 #x90))
                                    (handle-error (vector u1 u2 u3 u4) overlong-utf8-sequence))
                                   ((< u1 #xf8)
                                    (if (or (> u1 #xf4) (and (= u1 #xf4) (> u2 #x8f)))
                                        (handle-error (vector u1 u2 u3 u4) 'character-out-of-range)
                                        (logior (ash (logand u1 7) 18)
                                                  (ash (logxor u2 #x80) 12)
                                                  (ash (logxor u3 #x80) 6)
                                                  (logxor u4 #x80))))
                                   (t
                                    (handle-error (vector u1 u2 u3 u4) character-out-of-range)))))))))))))
                destination di)))
        finally (return (the fixnum (- di dest-start)))))

;;;; UTF-8B (Binary-safe UTF-8)

(define-character-encoding :utf-8b
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 4 bytes.  Invalid UTF-8 sequences are encoded with #xDCXX
code points for each invalid byte."
  :max-units-per-char 4
  :bom-encoding #(#xef #xbb #xbf)
  :default-replacement nil)

;; UTF-8B methods would go here - simplified for now but would follow
;; similar pattern with special handling for invalid sequences

;;;; Placeholder for other Unicode encodings
;;;; UTF-16, UTF-32, UCS-2 would be implemented here following
;;;; the same generic function pattern
