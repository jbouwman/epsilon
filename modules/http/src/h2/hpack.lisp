;;;; Full HPACK Implementation for HTTP/2
;;;;
;;;; Implements HPACK header compression per RFC 7541
;;;;
;;;; This file is part of the unified HTTP module (epsilon.http.h2)

(defpackage :epsilon.http.h2.hpack
  (:use :cl)
  (:import
   (epsilon.string str))
  (:export
   ;; Main API
   #:make-encoder
   #:make-decoder
   #:create-encoder    ; alias for make-encoder
   #:create-decoder    ; alias for make-decoder
   #:encode-header-list
   #:decode-header-block
   #:encode-headers    ; alias for encode-header-list
   #:decode-headers    ; alias for decode-header-block

   ;; String encoding
   #:encode-string
   #:decode-string

   ;; Static table
   #:find-in-static-table
   #:*static-table*

   ;; Configuration
   #:set-dynamic-table-size
   #:get-table-size

   ;; Encoder/decoder state accessors
   #:encoder-max-dynamic-table-size
   #:decoder-max-dynamic-table-size
   #:encoder-current-table-size
   #:decoder-current-table-size
   #:hpack-decoder-dynamic-table

   ;; Huffman coding
   #:huffman-encode
   #:huffman-decode
   ;; Decode error condition
   #:hpack-decode-error
   #:hpack-decode-error-reason))

;;;; Decode error condition

(define-condition hpack-decode-error (error)
  ((reason :initarg :reason :reader hpack-decode-error-reason))
  (:report (lambda (c s)
             (format s "HPACK decode error: ~A" (hpack-decode-error-reason c))))
  (:documentation
   "Signaled when an HPACK decoder reads past the end of a header block
or encounters structurally invalid encoding. Maps to HTTP/2
COMPRESSION_ERROR per RFC 7540 section 5.4.1."))

(defun hpack-need (bytes pos n what)
  "Signal HPACK-DECODE-ERROR unless N bytes are available at POS in BYTES."
  (unless (<= (+ pos n) (length bytes))
    (error 'hpack-decode-error
           :reason (format nil "~A: need ~D byte~:P at offset ~D, have ~D"
                           what n pos (- (length bytes) pos)))))

;;;; Constants

(defconstant +default-dynamic-table-size+ 4096)
(defconstant +max-dynamic-table-size+ 65536)

;;;; Huffman Coding Table (RFC 7541 Appendix B)

(defparameter *huffman-codes*
  #(#x1ff8     #x7fffd8   #xfffffe2  #xfffffe3  #xfffffe4  #xfffffe5  #xfffffe6  #xfffffe7  ; 0-7
    #xfffffe8  #xffffea   #x3ffffffc #xfffffe9  #xfffffea  #x3ffffffd #xfffffeb  #xfffffec  ; 8-15
    #xfffffed  #xfffffee  #xfffffef  #xffffff0  #xffffff1  #xffffff2  #x3ffffffe #xffffff3  ; 16-23
    #xffffff4  #xffffff5  #xffffff6  #xffffff7  #xffffff8  #xffffff9  #xffffffa  #xffffffb  ; 24-31
    #x14       #x3f8      #x3f9      #xffa      #x1ff9     #x15       #xf8       #x7fa      ; 32-39
    #x3fa      #x3fb      #xf9       #x7fb      #xfa       #x16       #x17       #x18       ; 40-47
    #x0        #x1        #x2        #x19       #x1a       #x1b       #x1c       #x1d       ; 48-55
    #x1e       #x1f       #x5c       #xfb       #x7ffc     #x20       #xffb      #x3fc      ; 56-63
    #x1ffa     #x21       #x5d       #x5e       #x5f       #x60       #x61       #x62       ; 64-71
    #x63       #x64       #x65       #x66       #x67       #x68       #x69       #x6a       ; 72-79
    #x6b       #x6c       #x6d       #x6e       #x6f       #x70       #x71       #x72       ; 80-87
    #xfc       #x73       #xfd       #x1ffb     #x7fff0    #x1ffc     #x3ffc     #x22       ; 88-95
    #x7ffd     #x3        #x23       #x4        #x24       #x5        #x25       #x26       ; 96-103
    #x27       #x6        #x74       #x75       #x28       #x29       #x2a       #x7        ; 104-111
    #x2b       #x76       #x2c       #x8        #x9        #x2d       #x77       #x78       ; 112-119
    #x79       #x7a       #x7b       #x7ffe     #x7fc      #x3ffd     #x1ffd     #xffffffc  ; 120-127
    #xfffe6    #x3fffd2   #xfffe7    #xfffe8    #x3fffd3   #x3fffd4   #x3fffd5   #x7fffd9   ; 128-135
    #x3fffd6   #x7fffda   #x7fffdb   #x7fffdc   #x7fffdd   #x7fffde   #xffffeb   #x7fffdf   ; 136-143
    #xffffec   #xffffed   #x3fffd7   #x7fffe0   #xffffee   #x7fffe1   #x7fffe2   #x7fffe3   ; 144-151
    #x7fffe4   #x1fffdc   #x3fffd8   #x7fffe5   #x3fffd9   #x7fffe6   #x7fffe7   #xffffef   ; 152-159
    #x3fffda   #x1fffdd   #xfffe9    #x3fffdb   #x3fffdc   #x7fffe8   #x7fffe9   #x1fffde   ; 160-167
    #x7fffea   #x3fffdd   #x3fffde   #xfffff0   #x1fffdf   #x3fffdf   #x7fffeb   #x7fffec   ; 168-175
    #x1fffe0   #x1fffe1   #x3fffe0   #x1fffe2   #x7fffed   #x3fffe1   #x7fffee   #x7fffef   ; 176-183
    #xfffea    #x3fffe2   #x3fffe3   #x3fffe4   #x7ffff0   #x3fffe5   #x3fffe6   #x7ffff1   ; 184-191
    #x3ffffe0  #x3ffffe1  #xfffeb    #x7fff1    #x3fffe7   #x7ffff2   #x3fffe8   #x1ffffec  ; 192-199
    #x3ffffe2  #x3ffffe3  #x3ffffe4  #x7ffffde  #x7ffffdf  #x3ffffe5  #xfffff1   #x1ffffed  ; 200-207
    #x7fff2    #x1fffe3   #x3ffffe6  #x7ffffe0  #x7ffffe1  #x3ffffe7  #x7ffffe2  #xfffff2   ; 208-215
    #x1fffe4   #x1fffe5   #x3ffffe8  #x3ffffe9  #xffffffd  #x7ffffe3  #x7ffffe4  #x7ffffe5  ; 216-223
    #xfffec    #xfffff3   #xfffed    #x1fffe6   #x3fffe9   #x1fffe7   #x1fffe8   #x7ffff3   ; 224-231
    #x3fffea   #x3fffeb   #x1ffffee  #x1ffffef  #xfffff4   #xfffff5   #x3ffffea  #x7ffff4   ; 232-239
    #x3ffffeb  #x7ffffe6  #x3ffffec  #x3ffffed  #x7ffffe7  #x7ffffe8  #x7ffffe9  #x7ffffea  ; 240-247
    #x7ffffeb  #xffffffe  #x7ffffec  #x7ffffed  #x7ffffee  #x7ffffef  #x7fffff0  #x3ffffee) ; 248-255
  "Huffman code table per RFC 7541 Appendix B (256 entries, symbols 0-255).")

(defparameter *huffman-lengths*
  #(13 23 28 28 28 28 28 28 28 24 30 28 28 30 28 28  ;   0-15
    28 28 28 28 28 28 30 28 28 28 28 28 28 28 28 28  ;  16-31
     6 10 10 12 13  6  8 11 10 10  8 11  8  6  6  6  ;  32-47
     5  5  5  6  6  6  6  6  6  6  7  8 15  6 12 10  ;  48-63
    13  6  7  7  7  7  7  7  7  7  7  7  7  7  7  7  ;  64-79
     7  7  7  7  7  7  7  7  8  7  8 13 19 13 14  6  ;  80-95
    15  5  6  5  6  5  6  6  6  5  7  7  6  6  6  5  ;  96-111
     6  7  6  5  5  6  7  7  7  7  7 15 11 14 13 28  ; 112-127
    20 22 20 20 22 22 22 23 22 23 23 23 23 23 24 23  ; 128-143
    24 24 22 23 24 23 23 23 23 21 22 23 22 23 23 24  ; 144-159
    22 21 20 22 22 23 23 21 23 22 22 24 21 22 23 23  ; 160-175
    21 21 22 21 23 22 23 23 20 22 22 22 23 22 22 23  ; 176-191
    26 26 20 19 22 23 22 25 26 26 26 27 27 26 24 25  ; 192-207
    19 21 26 27 27 26 27 24 21 21 26 26 28 27 27 27  ; 208-223
    20 24 20 21 22 21 21 23 22 22 25 25 24 24 26 23  ; 224-239
    26 27 26 26 27 27 27 27 27 28 27 27 27 27 27 26) ; 240-255
  "Huffman code length table per RFC 7541 Appendix B (256 entries, symbols 0-255).")

;;;; Static Table (RFC 7541 Appendix A)

(defparameter *static-table*
  #(nil  ; Index 0 unused
    (":authority" . nil)
    (":method" . "GET")
    (":method" . "POST")
    (":path" . "/")
    (":path" . "/index.html")
    (":scheme" . "http")
    (":scheme" . "https")
    (":status" . "200")
    (":status" . "204")
    (":status" . "206")
    (":status" . "304")
    (":status" . "400")
    (":status" . "404")
    (":status" . "500")
    ("accept-charset" . nil)
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . nil)
    ("accept-ranges" . nil)
    ("accept" . nil)
    ("access-control-allow-origin" . nil)
    ("age" . nil)
    ("allow" . nil)
    ("authorization" . nil)
    ("cache-control" . nil)
    ("content-disposition" . nil)
    ("content-encoding" . nil)
    ("content-language" . nil)
    ("content-length" . nil)
    ("content-location" . nil)
    ("content-range" . nil)
    ("content-type" . nil)
    ("cookie" . nil)
    ("date" . nil)
    ("etag" . nil)
    ("expect" . nil)
    ("expires" . nil)
    ("from" . nil)
    ("host" . nil)
    ("if-match" . nil)
    ("if-modified-since" . nil)
    ("if-none-match" . nil)
    ("if-range" . nil)
    ("if-unmodified-since" . nil)
    ("last-modified" . nil)
    ("link" . nil)
    ("location" . nil)
    ("max-forwards" . nil)
    ("proxy-authenticate" . nil)
    ("proxy-authorization" . nil)
    ("range" . nil)
    ("referer" . nil)
    ("refresh" . nil)
    ("retry-after" . nil)
    ("server" . nil)
    ("set-cookie" . nil)
    ("strict-transport-security" . nil)
    ("transfer-encoding" . nil)
    ("user-agent" . nil)
    ("vary" . nil)
    ("via" . nil)
    ("www-authenticate" . nil)))

(defconstant +static-table-size+ 61)

;;;; Dynamic Table Implementation

(defstruct dynamic-table
  "HPACK dynamic table"
  (entries (make-array 0 :adjustable t :fill-pointer 0))
  (size 0 :type fixnum)
  (max-size +default-dynamic-table-size+ :type fixnum))

(defun dynamic-table-add (table name value)
  "Add entry to dynamic table with eviction"
  (let* ((entry-size (+ 32 (length name) (length value)))
         (entry (cons name value)))

    ;; Evict entries if needed
    (loop while (and (> (length (dynamic-table-entries table)) 0)
                     (> (+ (dynamic-table-size table) entry-size)
                        (dynamic-table-max-size table)))
          do (let* ((oldest (vector-pop (dynamic-table-entries table)))
                    (oldest-size (+ 32 (length (car oldest)) (length (cdr oldest)))))
               (decf (dynamic-table-size table) oldest-size)))

    ;; Add new entry if it fits
    (when (<= entry-size (dynamic-table-max-size table))
      (vector-push-extend entry (dynamic-table-entries table) 32)
      ;; Rotate to maintain FIFO order (newest at index 0)
      (rotatef (aref (dynamic-table-entries table) 0)
               (aref (dynamic-table-entries table)
                     (1- (length (dynamic-table-entries table)))))
      (incf (dynamic-table-size table) entry-size))

    table))

(defun dynamic-table-get (table index)
  "Get entry from dynamic table by index"
  (when (and (>= index 0) (< index (length (dynamic-table-entries table))))
    (aref (dynamic-table-entries table) index)))

(defun dynamic-table-resize (table new-max-size)
  "Resize dynamic table, evicting entries if needed.  NEW-MAX-SIZE is
   bounds-checked against +max-dynamic-table-size+ (the operational
   ceiling the decoder is willing to accept from the peer); the
   HPACK varint on the wire is unbounded, so an unchecked size could
   crash the slot's :type fixnum declaration with a TYPE-ERROR.
   Out-of-range sizes are reported as HPACK-DECODE-ERROR -- HTTP/2
   maps this to COMPRESSION_ERROR."
  (unless (and (integerp new-max-size)
               (<= 0 new-max-size +max-dynamic-table-size+))
    (error 'hpack-decode-error
           :reason (format nil "dynamic-table size update out of range: ~A (must be 0..~A)"
                           new-max-size +max-dynamic-table-size+)))
  (setf (dynamic-table-max-size table) new-max-size)

  ;; Evict entries that don't fit
  (loop while (and (> (length (dynamic-table-entries table)) 0)
                   (> (dynamic-table-size table) new-max-size))
        do (let* ((oldest (vector-pop (dynamic-table-entries table)))
                  (oldest-size (+ 32 (length (car oldest)) (length (cdr oldest)))))
             (decf (dynamic-table-size table) oldest-size)))

  table)

;;;; Encoder/Decoder Structures

(defstruct hpack-encoder
  "HPACK encoder context"
  (dynamic-table (make-dynamic-table))
  (huffman-p t))

(defstruct hpack-decoder
  "HPACK decoder context"
  (dynamic-table (make-dynamic-table)))

;;;; Public API for table size management

(defun set-dynamic-table-size (table new-size)
  "Set the maximum size of a dynamic table"
  (dynamic-table-resize table new-size))

(defun get-table-size (table)
  "Get the current size of a dynamic table"
  (dynamic-table-size table))

;;;; Huffman Decode Trie
;;;;
;;;; Built at load time from the code/length tables. Each internal node is a
;;;; cons cell (left . right) where left = bit-0 child and right = bit-1 child.
;;;; Leaf nodes are integers (the decoded symbol 0-255).

(defun %build-huffman-tree ()
  "Build a binary trie from the Huffman code tables."
  (let ((root (cons nil nil)))
    (dotimes (sym 256)
      (let ((code (aref *huffman-codes* sym))
            (len (aref *huffman-lengths* sym))
            (node root))
        ;; Traverse all bits except the last to reach the parent of the leaf
        (loop for i from (1- len) downto 1
              for bit = (logand 1 (ash code (- i)))
              do (if (zerop bit)
                     (progn
                       (unless (consp (car node))
                         (setf (car node) (cons nil nil)))
                       (setf node (car node)))
                     (progn
                       (unless (consp (cdr node))
                         (setf (cdr node) (cons nil nil)))
                       (setf node (cdr node)))))
        ;; Set the leaf for the last bit
        (if (zerop (logand 1 code))
            (setf (car node) sym)
            (setf (cdr node) sym))))
    root))

(defparameter *huffman-decode-tree* (%build-huffman-tree)
  "Pre-built Huffman decode trie for O(code-length) per symbol decoding.")

;;;; Huffman Encoding/Decoding

(defun huffman-encode-byte (byte)
  "Get Huffman code and length for a byte"
  (values (aref *huffman-codes* byte)
          (aref *huffman-lengths* byte)))

(defun huffman-encode (string)
  "Encode string using Huffman coding per RFC 7541 Section 5.2"
  (let ((bits 0)
        (bit-count 0)
        (output (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))

    (loop for char across string
          for byte = (char-code char)
          do (multiple-value-bind (code len) (huffman-encode-byte byte)
               (setf bits (logior (ash bits len) code))
               (incf bit-count len)

               ;; Output complete bytes
               (loop while (>= bit-count 8)
                     do (vector-push-extend
                         (logand #xff (ash bits (- 8 bit-count)))
                         output)
                        (decf bit-count 8))))

    ;; Pad remaining bits with 1s (EOS prefix)
    (when (> bit-count 0)
      (let ((remaining (logand bits (1- (ash 1 bit-count)))))
        (vector-push-extend
         (logior (ash remaining (- 8 bit-count))
                 (1- (ash 1 (- 8 bit-count))))
         output)))

    output))

(defun huffman-decode (bytes)
  "Decode Huffman-encoded bytes using trie traversal per RFC 7541 Section 5.2"
  (let ((result (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0))
        (node *huffman-decode-tree*))
    (dotimes (byte-idx (length bytes))
      (let ((byte (aref bytes byte-idx)))
        (loop for bit-pos from 7 downto 0
              for bit = (logand 1 (ash byte (- bit-pos)))
              do (setf node (if (zerop bit) (car node) (cdr node)))
                 (when (integerp node)
                   (vector-push-extend (code-char node) result)
                   (setf node *huffman-decode-tree*)))))
    ;; Any remaining partial traversal is EOS padding (all-1s, at most 7 bits)
    (coerce result 'string)))

;;;; Integer Encoding/Decoding

(defun encode-integer (value prefix-bits output)
  "Encode integer with prefix into OUTPUT (array with fill-pointer)."
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (if (< value max-prefix)
        ;; Fits in prefix
        (vector-push-extend value output)
        ;; Needs continuation bytes
        (progn
          (vector-push-extend max-prefix output)
          (decf value max-prefix)
          (loop while (>= value 128)
                do (vector-push-extend (logior 128 (logand value 127)) output)
                   (setf value (ash value -7)))
          (vector-push-extend value output)))))

(defun decode-integer (bytes offset prefix-bits)
  "Decode integer from BYTES starting at OFFSET.
   Returns (values value new-offset)."
  (hpack-need bytes offset 1 "integer prefix")
  (let* ((max-prefix (1- (ash 1 prefix-bits)))
         (first-byte (aref bytes offset))
         (value (logand first-byte max-prefix))
         (pos (1+ offset)))
    (if (< value max-prefix)
        (values value pos)
        (let ((m 0))
          (loop
            (hpack-need bytes pos 1 "integer continuation")
            (let ((byte (aref bytes pos)))
              (incf value (* (logand byte 127) (ash 1 m)))
              (incf m 7)
              (incf pos)
              (when (< byte 128) (return))))
          (values value pos)))))

;;;; Main API

(defun make-encoder (&key (huffman-p t)
                          (table-size nil table-size-supplied-p)
                          (max-table-size nil max-table-size-supplied-p))
  "Create new HPACK encoder"
  (let ((size (cond (table-size-supplied-p table-size)
                    (max-table-size-supplied-p max-table-size)
                    (t +default-dynamic-table-size+))))
    (make-hpack-encoder
     :dynamic-table (make-dynamic-table :max-size size)
     :huffman-p huffman-p)))

(defun make-decoder (&key (table-size nil table-size-supplied-p)
                          (max-table-size nil max-table-size-supplied-p))
  "Create new HPACK decoder"
  (let ((size (cond (table-size-supplied-p table-size)
                    (max-table-size-supplied-p max-table-size)
                    (t +default-dynamic-table-size+))))
    (make-hpack-decoder
     :dynamic-table (make-dynamic-table :max-size size))))

;;; Accessor functions for encoder/decoder state

(defun encoder-max-dynamic-table-size (encoder)
  "Get maximum dynamic table size for encoder"
  (dynamic-table-max-size (hpack-encoder-dynamic-table encoder)))

(defun decoder-max-dynamic-table-size (decoder)
  "Get maximum dynamic table size for decoder"
  (dynamic-table-max-size (hpack-decoder-dynamic-table decoder)))

(defun encoder-current-table-size (encoder)
  "Get current dynamic table size for encoder"
  (dynamic-table-size (hpack-encoder-dynamic-table encoder)))

(defun decoder-current-table-size (decoder)
  "Get current dynamic table size for decoder"
  (dynamic-table-size (hpack-decoder-dynamic-table decoder)))

(defun encode-header-list (encoder headers)
  "Encode list of headers to bytes per RFC 7541"
  (let ((output (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 0))
        (huffman-p (hpack-encoder-huffman-p encoder)))
    (dolist (header headers)
      (let* ((name (car header))
             (value (cdr header))
             (static-idx (find-in-static-table name value)))
        (cond
          ;; Exact match in static table - use indexed header field
          ((and static-idx (plusp static-idx))
           (encode-indexed-header static-idx output))
          ;; Name match in static table - literal with name index
          ((and static-idx (minusp static-idx))
           (encode-literal-header-indexed-name (- static-idx) value output huffman-p))
          ;; No match - literal with new name
          (t
           (encode-literal-header-new-name name value output huffman-p)))))
    output))

(defun encode-indexed-header (index output)
  "Encode indexed header field (prefix 1, 7-bit index)"
  ;; RFC 7541 Section 6.1: Indexed Header Field
  ;; Format: 1xxxxxxx (7-bit prefix)
  (if (< index 127)
      (vector-push-extend (logior #x80 index) output)
      (progn
        (vector-push-extend #xff output)  ; 127 = max 7-bit value
        (encode-integer-continuation (- index 127) output))))

(defun encode-literal-header-indexed-name (name-index value output huffman-p)
  "Encode literal header with indexed name (prefix 01, 6-bit index)"
  ;; RFC 7541 Section 6.2.1: Literal Header Field with Incremental Indexing
  ;; Format: 01xxxxxx (6-bit prefix for name index)
  (if (< name-index 63)
      (vector-push-extend (logior #x40 name-index) output)
      (progn
        (vector-push-extend #x7f output)  ; 63 = max 6-bit value
        (encode-integer-continuation (- name-index 63) output)))
  ;; Encode value string
  (encode-hpack-string value output huffman-p))

(defun encode-literal-header-new-name (name value output huffman-p)
  "Encode literal header with new name (prefix 0000 0000)"
  ;; RFC 7541 Section 6.2.2: Literal Header Field without Indexing
  ;; Name index = 0 means new name
  (vector-push-extend #x00 output)
  ;; Encode name string
  (encode-hpack-string name output huffman-p)
  ;; Encode value string
  (encode-hpack-string value output huffman-p))

(defun encode-integer-continuation (value output)
  "Encode integer continuation bytes"
  (loop while (>= value 128)
        do (vector-push-extend (logior #x80 (logand value #x7f)) output)
           (setf value (ash value -7)))
  (vector-push-extend value output))

(defun encode-hpack-string (string output huffman-p)
  "Encode a string for HPACK with length prefix"
  (let* ((bytes (str:string-to-octets string))
         (encoded (if huffman-p (huffman-encode string) bytes))
         (len (length encoded)))
    ;; Encode length with Huffman flag and proper integer encoding
    (let ((first-byte (if huffman-p #x80 #x00)))
      (if (< len 127)
          (vector-push-extend (logior first-byte len) output)
          (progn
            (vector-push-extend (logior first-byte #x7f) output)
            (encode-integer-continuation (- len 127) output))))
    ;; Write string bytes
    (loop for byte across encoded
          do (vector-push-extend byte output))))

(defun decode-header-block (decoder bytes)
  "Decode header block from bytes per RFC 7541.
Signals HPACK-DECODE-ERROR on truncated or malformed input."
  (let ((headers nil)
        (pos 0)
        (len (length bytes))
        (dyn-table (hpack-decoder-dynamic-table decoder)))
    (loop while (< pos len)
          do (hpack-need bytes pos 1 "header field type")
             (let ((first-byte (aref bytes pos)))
               (cond
                 ;; Indexed Header Field (1xxxxxxx)
                 ((logtest first-byte #x80)
                  (multiple-value-bind (index new-pos)
                      (decode-integer bytes pos 7)
                    (setf pos new-pos)
                    (let ((entry (get-table-entry index dyn-table)))
                      (when entry
                        (push entry headers)))))

                 ;; Literal Header with Incremental Indexing (01xxxxxx)
                 ((logtest first-byte #x40)
                  (multiple-value-bind (name-index new-pos)
                      (decode-integer bytes pos 6)
                    (setf pos new-pos)
                    (let (name value)
                      ;; Get name
                      (if (zerop name-index)
                          ;; New name
                          (multiple-value-setq (name pos)
                            (decode-hpack-string bytes pos))
                          ;; Indexed name
                          (let ((entry (get-table-entry name-index dyn-table)))
                            (setf name (car entry))))
                      ;; Decode value
                      (multiple-value-setq (value pos)
                        (decode-hpack-string bytes pos))
                      ;; Add to dynamic table and headers
                      (let ((header (cons name value)))
                        (dynamic-table-add dyn-table name value)
                        (push header headers)))))

                 ;; Literal Header without Indexing (0000xxxx)
                 ((zerop (logand first-byte #xf0))
                  (multiple-value-bind (name-index new-pos)
                      (decode-integer bytes pos 4)
                    (setf pos new-pos)
                    (let (name value)
                      (if (zerop name-index)
                          (multiple-value-setq (name pos)
                            (decode-hpack-string bytes pos))
                          (let ((entry (get-table-entry name-index dyn-table)))
                            (setf name (car entry))))
                      (multiple-value-setq (value pos)
                        (decode-hpack-string bytes pos))
                      (push (cons name value) headers))))

                 ;; Literal Header Never Indexed (0001xxxx)
                 ((= (logand first-byte #xf0) #x10)
                  (multiple-value-bind (name-index new-pos)
                      (decode-integer bytes pos 4)
                    (setf pos new-pos)
                    (let (name value)
                      (if (zerop name-index)
                          (multiple-value-setq (name pos)
                            (decode-hpack-string bytes pos))
                          (let ((entry (get-table-entry name-index dyn-table)))
                            (setf name (car entry))))
                      (multiple-value-setq (value pos)
                        (decode-hpack-string bytes pos))
                      (push (cons name value) headers))))

                 ;; Dynamic Table Size Update (001xxxxx)
                 ((= (logand first-byte #xe0) #x20)
                  (multiple-value-bind (new-size new-pos)
                      (decode-integer bytes pos 5)
                    (setf pos new-pos)
                    (dynamic-table-resize dyn-table new-size)))

                 ;; Unknown - skip byte
                 (t (incf pos)))))
    (nreverse headers)))

(defun get-table-entry (index dyn-table)
  "Get entry from static or dynamic table by index"
  (cond
    ;; Index 0 is invalid
    ((zerop index) nil)
    ;; Static table (1-61)
    ((<= index +static-table-size+)
     (aref *static-table* index))
    ;; Dynamic table (62+)
    (t
     (let ((dyn-index (- index +static-table-size+ 1)))
       (dynamic-table-get dyn-table dyn-index)))))

(defun decode-hpack-string (bytes pos)
  "Decode HPACK string, returns (values string new-pos)"
  (hpack-need bytes pos 1 "string header")
  (let* ((first-byte (aref bytes pos))
         (huffman-p (logtest first-byte #x80)))
    (multiple-value-bind (len new-pos)
        (decode-integer bytes pos 7)
      (hpack-need bytes new-pos len "string body")
      (let* ((data (subseq bytes new-pos (+ new-pos len)))
             (string (handler-case
                         (if huffman-p
                             (huffman-decode data)
                             (str:octets-to-string data))
                       (error (c)
                         (error 'hpack-decode-error
                                :reason (format nil "string decode: ~A" c))))))
        (values string (+ new-pos len))))))

;;;; Aliases for compatibility
(defun create-encoder (&rest args)
  "Create new HPACK encoder (alias for make-encoder)"
  (apply #'make-encoder args))

(defun create-decoder (&rest args)
  "Create new HPACK decoder (alias for make-decoder)"
  (apply #'make-decoder args))

(defun encode-headers (encoder headers)
  "Encode headers (alias for encode-header-list)"
  (encode-header-list encoder headers))

(defun decode-headers (decoder bytes)
  "Decode headers (alias for decode-header-block)"
  (decode-header-block decoder bytes))

;;;; String encoding/decoding

(defun encode-string (string output &key huffman-p)
  "Encode a string for HPACK.
   If HUFFMAN-P is true, use Huffman encoding.
   OUTPUT should be an array with fill-pointer."
  (let* ((bytes (str:string-to-octets string))
         (encoded-bytes (if huffman-p
                           (huffman-encode bytes)
                           bytes))
         (len (length encoded-bytes)))
    ;; Write length with Huffman flag
    (if huffman-p
        (vector-push-extend (logior #x80 len) output)  ; High bit set for Huffman
        (vector-push-extend len output))
    ;; Write bytes
    (loop for byte across encoded-bytes
          do (vector-push-extend byte output))
    output))

(defun decode-string (bytes &optional (start 0))
  "Decode a string from HPACK encoding.
   Returns (values decoded-string end-position)."
  (hpack-need bytes start 1 "string prefix")
  (let* ((first-byte (aref bytes start))
         (huffman-p (logtest first-byte #x80))
         (len (logand first-byte #x7f))
         (data-start (1+ start))
         (data-end (+ data-start len)))
    (hpack-need bytes data-start len "string data")
    (let ((data (subseq bytes data-start data-end)))
      (values
       (handler-case
           (if huffman-p
               (str:octets-to-string (huffman-decode data))
               (str:octets-to-string data))
         (error (c)
           (error 'hpack-decode-error
                  :reason (format nil "string decode: ~A" c))))
       data-end))))

;;;; Static table lookup

(defun find-in-static-table (name &optional value)
  "Find an entry in the static table.
   Returns index (1-based) for exact match (name+value).
   Returns negative index if only name matches but value differs.
   Returns nil if no match at all."
  (let ((name-only-match nil))
    (loop for i from 1 below (length *static-table*)
          for entry = (aref *static-table* i)
          when entry
          do (let ((entry-name (car entry))
                   (entry-value (cdr entry)))
               ;; Check for name match
               (when (string= name entry-name)
                 ;; Exact match (name and value)
                 (when (and value (string= value entry-value))
                   (return-from find-in-static-table i))
                 ;; Name-only lookup (no value specified)
                 (when (null value)
                   (return-from find-in-static-table i))
                 ;; Track first name-only match for later
                 (unless name-only-match
                   (setf name-only-match i)))))
    ;; If we had a name match but no value match, return negative index
    (if name-only-match
        (- name-only-match)
        nil)))
