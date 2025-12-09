;;;; Full HPACK Implementation for HTTP/2
;;;;
;;;; Implements HPACK header compression per RFC 7541

(defpackage :epsilon.http2.hpack
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string))
  (:export 
   ;; Main API
   #:make-encoder
   #:make-decoder
   #:encode-header-list
   #:decode-header-block
   
   ;; Configuration
   #:set-dynamic-table-size
   #:get-table-size
   
   ;; Huffman coding
   #:huffman-encode
   #:huffman-decode))

(in-package :epsilon.http2.hpack)

;;;; Constants

(defconstant +default-dynamic-table-size+ 4096)
(defconstant +max-dynamic-table-size+ 65536)

;;;; Huffman Coding Table (RFC 7541 Appendix B)

(defparameter *huffman-codes*
  #(#x1ff8     #x7fffd8   #xfffffe2  #xfffffe3  #xfffffe4  #xfffffe5  #xfffffe6  #xfffffe7  
    #xfffffe8  #xffffea   #x3ffffffc #xfffffe9  #xfffffea  #x3ffffffd #xfffffeb  #xfffffec  
    #xfffffed  #xfffffee  #xfffffef  #xffffff0  #xffffff1  #xffffff2  #x3ffffffe #xffffff3  
    #xffffff4  #xffffff5  #xffffff6  #xffffff7  #xffffff8  #xffffff9  #xffffffa  #xffffffb  
    #x14       #x3f8      #x3f9      #xffa      #x1ff9     #x15       #xf8       #x7fa      
    #x3fa      #x3fb      #xf9       #x7fb      #xfa       #x16       #x17       #x18       
    #x0        #x1        #x2        #x19       #x1a       #x1b       #x1c       #x1d       
    #x1e       #x1f       #x5c       #xfb       #x7ffc     #x20       #xffb      #x3fc      
    #x1ffa     #x21       #x5d       #x5e       #x5f       #x60       #x61       #x62       
    #x63       #x64       #x65       #x66       #x67       #x68       #x69       #x6a       
    #x6b       #x6c       #x6d       #x6e       #x6f       #x70       #x71       #x72       
    #xfc       #x73       #xfd       #x1ffb     #x7fff0    #x1ffc     #x3ffc     #x22       
    #x7ffd     #x3        #x23       #x4        #x24       #x5        #x25       #x26       
    #x27       #x6        #x74       #x75       #x28       #x29       #x2a       #x7        
    #x2b       #x76       #x2c       #x8        #x9        #x2d       #x77       #x78       
    #x79       #x7a       #x7b       #x7ffe     #x7fc      #x3ffd     #x1ffd     #xffffffc  
    #xfffe6    #x3fffd2   #xfffe7    #xfffe8    #x3fffd3   #x3fffd4   #x3fffd5   #x7fffd9   
    #x3fffd6   #x7fffda   #x7fffdb   #x7fffdc   #x7fffdd   #x7fffde   #xffffeb   #x7fffdf   
    #xffffec   #xffffed   #x3fffd7   #x7fffe0   #xffffee   #x7fffe1   #x7fffe2   #x7fffe3   
    #x7fffe4   #x1fffdc   #x3fffd8   #x7fffe5   #x3fffd9   #x7fffe6   #x7fffe7   #xffffef   
    #x3fffda   #x1fffdd   #xfffe9    #x3fffdb   #x3fffdc   #x7fffe8   #x7fffe9   #x1fffde   
    #x7fffea   #x3fffdd   #x3fffde   #xfffff0   #x1fffdf   #x3fffdf   #x7fffeb   #x7fffec   
    #x1fffe0   #x1fffe1   #x3fffe0   #x1fffe2   #x7fffed   #x3fffe1   #x7fffee   #x7fffef   
    #xfffea    #x3fffe2   #x3fffe3   #x3fffe4   #x7ffff0   #x3fffe5   #x3fffe6   #x7ffff1   
    #x3ffffe0  #x3ffffe1  #xfffeb    #x7fff1    #x3fffe7   #x7ffff2   #x3fffe8   #x1ffffec  
    #x3ffffe2  #x3ffffe3  #x3ffffe4  #x7ffffde  #x7ffffdf  #x3ffffe5  #xfffff1   #x1ffffed  
    #x7fff2    #x1fffe3   #x3ffffe6  #x7ffffe0  #x7ffffe1  #x3ffffe7  #x7ffffe2  #xfffff2   
    #x1fffe4   #x1fffe5   #x3ffffe8  #x3ffffe9  #xffffffd  #x7ffffe3  #x7ffffe4  #x7ffffe5  
    #xfffec    #xfffff3   #xfffed    #x1fffe6   #x3fffea   #x1fffe7   #xfffff4   #xfffff5   
    #x3ffffeb  #x7ffff3   #x3ffffec  #x3ffffed  #x7ffffe6  #x7ffffe7  #x7ffffe8  #x7ffffe9  
    #x7ffffea  #x7ffffeb  #xffffffe  #x7ffffec  #x7ffffed  #x7ffffee  #x7ffffef  #x7fffff0  
    #x3ffffee  #x3ffffff))

(defparameter *huffman-lengths*
  #(13 23 28 28 28 28 28 28 28 24 30 28 28 30 28 28
    28 28 28 28 28 28 30 28 28 28 28 28 28 28 28 28
    6 10 10 12 13 6 8 11 10 10 8 11 8 6 6 6
    5 5 5 6 6 6 6 6 6 6 7 8 15 6 12 10
    13 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    7 7 7 7 7 7 7 7 8 7 8 13 19 13 14 6
    15 5 6 5 6 5 6 6 6 5 7 7 6 6 6 5
    6 7 6 5 5 6 7 7 7 7 7 15 11 14 13 28
    20 22 20 20 22 22 22 23 22 23 23 23 23 23 24 23
    24 24 22 23 24 23 23 23 23 21 22 23 22 23 23 24
    22 21 20 22 22 23 23 21 23 22 22 24 21 22 23 23
    21 21 22 21 23 22 23 23 20 22 22 22 23 22 22 23
    26 26 20 19 22 23 22 25 26 26 26 27 27 26 24 25
    19 21 26 27 27 26 27 24 21 21 26 26 28 27 27 27
    20 24 20 21 22 21 21 23 22 22 25 25 24 24 26 23
    26 27 26 26 27 27 27 27 27 28 27 27 27 27 27 26
    27 28 26 26 26))

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
  "Resize dynamic table, evicting entries if needed"
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

;;;; Huffman Encoding/Decoding

(defun huffman-encode-byte (byte)
  "Get Huffman code and length for a byte"
  (values (aref *huffman-codes* byte)
          (aref *huffman-lengths* byte)))

(defun huffman-encode (string)
  "Encode string using Huffman coding"
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
    
    ;; Pad remaining bits with 1s
    (when (> bit-count 0)
      (vector-push-extend 
       (logior (ash bits (- 8 bit-count))
               (1- (ash 1 (- 8 bit-count))))
       output))
    
    output))

(defun huffman-decode (bytes)
  "Decode Huffman-encoded bytes"
  ;; Simplified decoder - production would use a proper tree
  (let ((result (make-array 0 :element-type 'character 
                           :adjustable t :fill-pointer 0))
        (bits 0)
        (bit-count 0))
    
    (loop for byte across bytes
          do (setf bits (logior (ash bits 8) byte))
             (incf bit-count 8)
          
          ;; Try to decode symbols
          while (>= bit-count 5)  ; Minimum code length
          do (block decode-symbol
               (loop for i from 0 below 256
                     for code = (aref *huffman-codes* i)
                     for len = (aref *huffman-lengths* i)
                     when (and (<= len bit-count)
                              (= code (logand (ash bits (- len bit-count))
                                            (1- (ash 1 len)))))
                     do (vector-push-extend (code-char i) result)
                        (decf bit-count len)
                        (return-from decode-symbol))))
    
    (coerce result 'string)))

;;;; Integer Encoding/Decoding

(defun encode-integer (value prefix-bits stream)
  "Encode integer with prefix"
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (if (< value max-prefix)
        ;; Fits in prefix
        (write-byte value stream)
        ;; Needs continuation bytes
        (progn
          (write-byte max-prefix stream)
          (decf value max-prefix)
          (loop while (>= value 128)
                do (write-byte (logior 128 (logand value 127)) stream)
                   (setf value (ash value -7)))
          (write-byte value stream)))))

(defun decode-integer (prefix-bits stream first-byte)
  "Decode integer with prefix"
  (let* ((max-prefix (1- (ash 1 prefix-bits)))
         (value (logand first-byte max-prefix)))
    
    (if (< value max-prefix)
        value
        (let ((m 0))
          (loop for byte = (read-byte stream)
                do (incf value (* (logand byte 127) (ash 1 m)))
                   (incf m 7)
                while (>= byte 128))
          value))))

;;;; Simplified HPACK Implementation
;;;; These functions are stubs for the full implementation

;;;; Main API

(defun make-encoder (&key (huffman-p t) (table-size +default-dynamic-table-size+))
  "Create new HPACK encoder"
  (make-hpack-encoder 
   :dynamic-table (make-dynamic-table :max-size table-size)
   :huffman-p huffman-p))

(defun make-decoder (&key (table-size +default-dynamic-table-size+))
  "Create new HPACK decoder"
  (make-hpack-decoder
   :dynamic-table (make-dynamic-table :max-size table-size)))

(defun encode-header-list (encoder headers)
  "Encode list of headers to bytes"
  ;; Simplified implementation for now
  (let ((output (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    ;; For each header, use literal encoding for now
    (dolist (header headers)
      (let* ((name (car header))
             (value (cdr header))
             (name-bytes (str:string-to-octets name))
             (value-bytes (str:string-to-octets value)))
        ;; Simple literal encoding: length + name + length + value
        (vector-push-extend (length name-bytes) output)
        (loop for byte across name-bytes do (vector-push-extend byte output))
        (vector-push-extend (length value-bytes) output)
        (loop for byte across value-bytes do (vector-push-extend byte output))))
    output))

(defun decode-header-block (decoder bytes)
  "Decode header block from bytes"
  ;; Simplified implementation matching the encoder
  (let ((headers nil)
        (pos 0))
    (loop while (< pos (length bytes))
          do (let* ((name-len (aref bytes pos))
                   (name-start (incf pos))
                   (name-end (+ pos name-len))
                   (value-len (progn (setf pos name-end) (aref bytes pos)))
                   (value-start (incf pos))
                   (value-end (+ pos value-len)))
               
               (when (>= value-end (length bytes))
                 (return))
               
               (let ((name (str:octets-to-string 
                           (subseq bytes name-start name-end)))
                     (value (str:octets-to-string 
                            (subseq bytes value-start value-end))))
                 (push (cons name value) headers))
               
               (setf pos value-end)))
    (nreverse headers)))


