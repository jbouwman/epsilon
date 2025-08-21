;;;; Simple HPACK Implementation for HTTP/2
;;;;
;;;; A minimal HPACK implementation for testing. 
;;;; For production, consider using nghttp2 via FFI.

(defpackage :epsilon.http2.hpack
  (:use :cl)
  (:export 
   #:create-encoder
   #:create-decoder
   #:encode-headers
   #:decode-headers
   #:encoder
   #:decoder))

(in-package :epsilon.http2.hpack)

;;;; Static Table (Appendix A of RFC 7541)
;;;; Only including the most common entries for now

(defparameter *static-table*
  #(nil  ; Index 0 is not used
    (":authority" . "")
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
    ("accept-charset" . "")
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . "")
    ("accept-ranges" . "")
    ("accept" . "")
    ("access-control-allow-origin" . "")
    ("age" . "")
    ("allow" . "")
    ("authorization" . "")
    ("cache-control" . "")
    ("content-disposition" . "")
    ("content-encoding" . "")
    ("content-language" . "")
    ("content-length" . "")
    ("content-location" . "")
    ("content-range" . "")
    ("content-type" . "")
    ("cookie" . "")
    ("date" . "")
    ("etag" . "")
    ("expect" . "")
    ("expires" . "")
    ("from" . "")
    ("host" . "")
    ("if-match" . "")
    ("if-modified-since" . "")
    ("if-none-match" . "")
    ("if-range" . "")
    ("if-unmodified-since" . "")
    ("last-modified" . "")
    ("link" . "")
    ("location" . "")
    ("max-forwards" . "")
    ("proxy-authenticate" . "")
    ("proxy-authorization" . "")
    ("range" . "")
    ("referer" . "")
    ("refresh" . "")
    ("retry-after" . "")
    ("server" . "")
    ("set-cookie" . "")
    ("strict-transport-security" . "")
    ("transfer-encoding" . "")
    ("user-agent" . "")
    ("vary" . "")
    ("via" . "")
    ("www-authenticate" . "")))

(defparameter +static-table-size+ (length *static-table*))

;;;; Encoder/Decoder Structures

(defstruct encoder
  "HPACK encoder state"
  (dynamic-table (make-array 0 :adjustable t :fill-pointer 0))
  (max-dynamic-table-size 4096)
  (current-table-size 0))

(defstruct decoder
  "HPACK decoder state"
  (dynamic-table (make-array 0 :adjustable t :fill-pointer 0))
  (max-dynamic-table-size 4096)
  (current-table-size 0))

;;;; Integer Encoding (RFC 7541 Section 5.1)

(defun encode-integer (value prefix-bits output)
  "Encode an integer with the given prefix size"
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (if (< value max-prefix)
        ;; Fits in prefix
        value
        ;; Needs continuation bytes
        (progn
          (vector-push-extend max-prefix output)
          (let ((remaining (- value max-prefix)))
            (loop while (>= remaining 128)
                  do (vector-push-extend (logior 128 (logand remaining 127)) output)
                     (setf remaining (ash remaining -7)))
            (vector-push-extend remaining output)
            max-prefix)))))

(defun decode-integer (input offset prefix-bits)
  "Decode an integer from input starting at offset"
  (let* ((max-prefix (1- (ash 1 prefix-bits)))
         (value (logand (aref input offset) max-prefix)))
    (incf offset)
    (if (< value max-prefix)
        (values value offset)
        ;; Read continuation bytes
        (let ((m 0))
          (loop
            (when (>= offset (length input))
              (error "Truncated integer"))
            (let ((byte (aref input offset)))
              (incf offset)
              (incf value (* (logand byte 127) (ash 1 m)))
              (incf m 7)
              (when (zerop (logand byte 128))
                (return))))
          (values value offset)))))

;;;; String Encoding (RFC 7541 Section 5.2)

(defun encode-string (string output &key huffman-p)
  "Encode a string (without Huffman encoding for now)"
  (declare (ignore huffman-p))  ; TODO: Implement Huffman encoding
  (let ((bytes (epsilon.string:string-to-octets string)))
    ;; First bit is Huffman flag (0 for now), then 7-bit length
    (if (< (length bytes) 127)
        (vector-push-extend (length bytes) output)
        (progn
          ;; For longer strings, use continuation bytes
          (encode-integer (length bytes) 7 output)))
    ;; Then the actual string bytes
    (loop for byte across bytes
          do (vector-push-extend byte output))))

(defun decode-string (input offset)
  "Decode a string from input"
  (let* ((first-byte (aref input offset))
         (huffman-p (logtest first-byte #x80))
         (length-value (logand first-byte #x7f)))
    (incf offset)
    
    ;; Decode length if needed
    (when (= length-value 127)
      (multiple-value-bind (actual-length new-offset)
          (decode-integer input (1- offset) 7)
        (setf length-value actual-length
              offset new-offset)))
    
    ;; Extract string bytes
    (let ((bytes (subseq input offset (+ offset length-value))))
      (incf offset length-value)
      (if huffman-p
          (error "Huffman decoding not yet implemented")
          (values (epsilon.string:octets-to-string bytes) offset)))))

;;;; Static Table Lookup

(defun find-in-static-table (name value)
  "Find a header in the static table. Returns index or nil."
  (loop for i from 1 below +static-table-size+
        for entry = (aref *static-table* i)
        when (and (string-equal (car entry) name)
                  (string-equal (cdr entry) value))
          return i
        when (string-equal (car entry) name)
          do (return (- i))))  ; Negative means name-only match

;;;; Header Encoding

(defun encode-headers (encoder headers)
  "Encode a list of headers using HPACK"
  (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (dolist (header headers)
      (let* ((name (string-downcase (car header)))
             (value (cdr header))
             (static-index (find-in-static-table name value)))
        (cond
          ;; Indexed header field (Section 6.1)
          ((and static-index (plusp static-index))
           (vector-push-extend (logior #x80 static-index) output))
          
          ;; Literal with indexed name (Section 6.2.1)
          ((and static-index (minusp static-index))
           (let ((name-index (- static-index)))
             ;; Literal header field with incremental indexing - indexed name
             (vector-push-extend (logior #x40 name-index) output)
             (encode-string value output)))
          
          ;; Literal without indexing - new name (Section 6.2.2)
          (t
           (vector-push-extend #x00 output)  ; Literal without indexing
           (encode-string name output)
           (encode-string value output)))))
    output))

(defun decode-headers (decoder input)
  "Decode HPACK encoded headers"
  (let ((headers nil)
        (offset 0))
    (loop while (< offset (length input))
          do (let ((byte (aref input offset)))
               (cond
                 ;; Indexed header field (Section 6.1)
                 ((logtest byte #x80)
                  (multiple-value-bind (index new-offset)
                      (decode-integer input offset 7)
                    (setf offset new-offset)
                    (when (and (> index 0) (< index +static-table-size+))
                      (let ((entry (aref *static-table* index)))
                        (push (cons (car entry) (cdr entry)) headers)))))
                 
                 ;; Literal header field with incremental indexing (Section 6.2.1)
                 ((logtest byte #x40)
                  (multiple-value-bind (index new-offset)
                      (decode-integer input offset 6)
                    (setf offset new-offset)
                    (let ((name (if (zerop index)
                                   (multiple-value-bind (n o)
                                       (decode-string input offset)
                                     (setf offset o)
                                     n)
                                   (car (aref *static-table* index)))))
                      (multiple-value-bind (value new-offset)
                          (decode-string input offset)
                        (setf offset new-offset)
                        (push (cons name value) headers)))))
                 
                 ;; Literal header field without indexing (Section 6.2.2)
                 ((zerop (logand byte #xf0))
                  (multiple-value-bind (index new-offset)
                      (decode-integer input offset 4)
                    (setf offset new-offset)
                    (let ((name (if (zerop index)
                                   (multiple-value-bind (n o)
                                       (decode-string input offset)
                                     (setf offset o)
                                     n)
                                   (car (aref *static-table* index)))))
                      (multiple-value-bind (value new-offset)
                          (decode-string input offset)
                        (setf offset new-offset)
                        (push (cons name value) headers)))))
                 
                 ;; Dynamic table size update (Section 6.3)
                 ((logtest byte #x20)
                  (multiple-value-bind (size new-offset)
                      (decode-integer input offset 5)
                    (setf offset new-offset)
                    (setf (decoder-max-dynamic-table-size decoder) size)))
                 
                 (t
                  (error "Unknown HPACK instruction: ~X" byte)))))
    (nreverse headers)))

;;;; Public API

(defun create-encoder (&key (max-table-size 4096))
  "Create a new HPACK encoder"
  (make-encoder :max-dynamic-table-size max-table-size))

(defun create-decoder (&key (max-table-size 4096))
  "Create a new HPACK decoder"
  (make-decoder :max-dynamic-table-size max-table-size))

;;;; Testing

(defun test-hpack ()
  "Simple test of HPACK encoding/decoding"
  (let ((encoder (create-encoder))
        (decoder (create-decoder))
        (headers '((":method" . "GET")
                  (":path" . "/")
                  (":scheme" . "https")
                  ("host" . "example.com")
                  ("accept" . "text/html"))))
    (format t "Original headers: ~S~%" headers)
    (let* ((encoded (encode-headers encoder headers))
           (decoded (decode-headers decoder encoded)))
      (format t "Encoded size: ~D bytes~%" (length encoded))
      (format t "Decoded headers: ~S~%" decoded)
      (format t "Match: ~A~%" (equal headers decoded)))))

(export 'test-hpack)