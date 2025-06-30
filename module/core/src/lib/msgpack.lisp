(defpackage :epsilon.lib.msgpack
  (:use
   :epsilon.lib.type
   :cl)
  (:local-nicknames
   (:char :epsilon.lib.char)
   (:map :epsilon.lib.map)
   (:stream :epsilon.lib.stream)
   (:time :epsilon.lib.time))
  (:export
   :encode
   :decode
   
   ;; Constants for extension types
   :+timestamp-type+))

(in-package :epsilon.lib.msgpack)

;; MessagePack format codes

(defconstant +nil+ #xc0)
(defconstant +false+ #xc2)
(defconstant +true+ #xc3)
(defconstant +fixint+ #x00)      ; 0XXXXXXX
(defconstant +fixmap+ #x80)      ; 1000XXXX
(defconstant +fixarray+ #x90)    ; 1001XXXX
(defconstant +fixstr+ #xa0)      ; 101XXXXX
(defconstant +uint8+ #xcc)
(defconstant +uint16+ #xcd)
(defconstant +uint32+ #xce)
(defconstant +uint64+ #xcf)
(defconstant +int8+ #xd0)
(defconstant +int16+ #xd1)
(defconstant +int32+ #xd2)
(defconstant +int64+ #xd3)
(defconstant +float32+ #xca)
(defconstant +float64+ #xcb)
(defconstant +str8+ #xd9)
(defconstant +str16+ #xda)
(defconstant +str32+ #xdb)
(defconstant +bin8+ #xc4)
(defconstant +bin16+ #xc5)
(defconstant +bin32+ #xc6)
(defconstant +array16+ #xdc)
(defconstant +array32+ #xdd)
(defconstant +map16+ #xde)
(defconstant +map32+ #xdf)
(defconstant +fixext1+ #xd4)
(defconstant +fixext2+ #xd5)
(defconstant +fixext4+ #xd6)
(defconstant +fixext8+ #xd7)
(defconstant +fixext16+ #xd8)
(defconstant +ext8+ #xc7)
(defconstant +ext16+ #xc8)
(defconstant +ext32+ #xc9)

;; Extension type values
(defconstant +timestamp-type+ -1)

;;; MessagePack Encoding/Decoding

(defun encode (object)
  "Encode a Lisp object to MessagePack byte array"
  (let ((stream (stream:make-output-stream)))
    (encode-object stream object)
    (stream:buffer stream)))

(defun encode-object (stream object)
  "Encode a single object to a MessagePack stream"
  (cond
    ((null object)
     (write-byte +nil+ stream))
    ((eq object t)
     (write-byte +true+ stream))
    ((integerp object)
     (encode-integer stream object))
    ((floatp object)
     (encode-float stream object))
    ((stringp object)
     (encode-string stream object))
    ((symbolp object)
     (encode-string stream (symbol-name object)))
    ((typep object 'map::hamt)
     (encode-map stream object))
    ((typep object 'time:timestamp)
     (encode-timestamp stream object))
    ((vectorp object) 
     (if (subtypep (array-element-type object) 'u8)
         (encode-binary stream object)
         (encode-array stream object)))
    ((listp object) 
     (cond
       ((eq (car object) :ext)
         (encode-ext stream (second object) (third object)))
       (t (encode-array stream (coerce object 'vector)))))
    (t
     (error "Cannot encode object of type ~A" (type-of object)))))

(defun write-uint (stream value byte-count)
  "Write a VALUE of BYTE-COUNT bytes to STREAM in big-endian order."
  (loop for i from (1- byte-count) downto 0
        do (write-byte (ldb (byte 8 (* i 8)) value) stream)))

(defun write-size (stream size format-8 format-16 format-32 max-fixed &optional fixed-prefix)
  "Write a SIZE value using the appropriate format based on its magnitude."
  (cond
    ((<= size max-fixed)
     (write-byte (if fixed-prefix (logior fixed-prefix size) size) stream))
    ((<= size 255)
     (write-byte format-8 stream)
     (write-byte size stream))
    ((<= size 65535)
     (write-byte format-16 stream)
     (write-uint stream size 2))
    (t
     (write-byte format-32 stream)
     (write-uint stream size 4))))

(defun encode-integer (stream int)
  (cond
    ((<= 0 int 127)
     (write-byte int stream))
    ((<= 0 int 255)
     (write-byte +uint8+ stream)
     (write-byte int stream))
    ((<= 0 int 65535)
     (write-byte +uint16+ stream)
     (write-uint stream int 2))
    ((<= 0 int #xFFFFFFFF)
     (write-byte +uint32+ stream)
     (write-uint stream int 4))
    ((<= 0 int #xFFFFFFFFFFFFFFFF)
     (write-byte +uint64+ stream)
     (write-uint stream int 8))
    ((<= -32 int -1)
     (write-byte (logand int #xFF) stream))
    ((<= -128 int -1)
     (write-byte +int8+ stream)
     (write-byte (logand int #xFF) stream))
    ((<= -32768 int -1)
     (write-byte +int16+ stream)
     (write-uint stream (logand int #xFFFF) 2))
    ((<= #x-80000000 int -1)
     (write-byte +int32+ stream)
     (write-uint stream (logand int #xFFFFFFFF) 4))
    ((<= #x-8000000000000000 int -1)
     (write-byte +int64+ stream)
     (write-uint stream (logand int #xFFFFFFFFFFFFFFFF) 8))
    (t (error "Integer too large to encode: ~A" int))))

(defun encode-string (stream string)
  (let* ((bytes (char:string-to-bytes string))
         (length (length bytes)))
    (write-size stream length +str8+ +str16+ +str32+ 31 +fixstr+)
    (write-sequence bytes stream)))

(defun encode-binary (stream bytes)
  (let ((length (length bytes)))
    (write-size stream length +bin8+ +bin16+ +bin32+ -1)
    (write-sequence bytes stream)))

(defun encode-array (stream array)
  (let ((length (length array)))
    (cond
      ((<= length 15)
       (write-byte (logior +fixarray+ length) stream))
      ((<= length 65535)
       (write-byte +array16+ stream)
       (write-uint stream length 2))
      (t
       (write-byte +array32+ stream)
       (write-uint stream length 4)))
    (loop for element across array
          do (encode-object stream element))))

(defun encode-map (stream map)
  (let ((length (map:size map)))
    (cond
      ((<= length 15)
       (write-byte (logior +fixmap+ length) stream))
      ((<= length 65535)
       (write-byte +map16+ stream)
       (write-uint stream length 2))
      (t
       (write-byte +map32+ stream)
       (write-uint stream length 4)))
    (loop for (key . value) in (map:seq map)
          do (encode-object stream key)
             (encode-object stream value))))

;;; Additional Encoding Functions

(defun encode-float (stream float)
  "Encode a floating point number to a MessagePack stream"
  (if (typep float 'single-float)
      (encode-float32 stream float)
      (encode-float64 stream float)))

(defun single-float-to-bits (float)
  "Convert a single-precision float to its 32-bit integer representation."
  (declare (type single-float float))
  #+sbcl (sb-kernel:single-float-bits float)
  #-sbcl (error "Not implemented for this Lisp implementation"))

(defun bits-to-single-float (bits)
  "Convert a 32-bit integer representation to a single-precision float."
  (declare (type (unsigned-byte 32) bits))
  #+sbcl (sb-kernel:make-single-float bits)
  #-sbcl (error "Not implemented for this Lisp implementation"))

(defun double-float-to-bits (float)
  "Convert a double-precision float to its 64-bit integer representation."
  (declare (type double-float float))
  #+sbcl (logior (ash (sb-kernel:double-float-high-bits float) 32)
                (sb-kernel:double-float-low-bits float))
  #-sbcl (error "Not implemented for this Lisp implementation"))

(defun bits-to-double-float (bits)
  "Convert a 64-bit integer representation to a double-precision float."
  (declare (type (unsigned-byte 64) bits))
  #+sbcl (sb-kernel:make-double-float (ash bits -32) (logand bits #xFFFFFFFF))
  #-sbcl (error "Not implemented for this Lisp implementation"))

(defun encode-float32 (stream float)
  "Encode a 32-bit float to a MessagePack stream"
  (write-byte +float32+ stream)
  (let ((bits (single-float-to-bits float)))
    (write-uint stream bits 4)))

(defun encode-float64 (stream float)
  "Encode a 64-bit float to a MessagePack stream"
  (write-byte +float64+ stream)
  (let ((bits (double-float-to-bits float)))
    (write-uint stream bits 8)))

(defun encode-ext (stream type data)
  "Encode an extension type with TYPE and DATA bytes"
  (let ((length (length data)))
    (cond
      ((= length 1)
       (write-byte +fixext1+ stream))
      ((= length 2)
       (write-byte +fixext2+ stream))
      ((= length 4)
       (write-byte +fixext4+ stream))
      ((= length 8)
       (write-byte +fixext8+ stream))
      ((= length 16)
       (write-byte +fixext16+ stream))
      ((<= length 255)
       (write-byte +ext8+ stream)
       (write-byte length stream))
      ((<= length 65535)
       (write-byte +ext16+ stream)
       (write-uint stream length 2))
      (t
       (write-byte +ext32+ stream)
       (write-uint stream length 4))))
  (write-byte type stream)
  (write-sequence data stream))

;;; Decoding Functions

(defun decode (bytes)
  "Decode a MessagePack byte array to a Lisp object"
  (let ((stream (stream:make-input-stream bytes)))
    (decode-object stream)))

(defun decode-object (stream)
  "Decode a single MessagePack object from a stream"
  (let ((byte (read-byte stream)))
    (cond
      ;; nil, true, false
      ((= byte +nil+) nil)
      ((= byte +true+) t)
      ((= byte +false+) nil)
      
      ;; Integers
      ((< byte #x80) byte)                         ; positive fixint (0-127)
      ((>= byte #xe0) (- byte #x100))              ; negative fixint (-32 to -1)
      ((= byte +uint8+) (read-byte stream))        ; uint8
      ((= byte +uint16+) (read-uint stream 2))     ; uint16
      ((= byte +uint32+) (read-uint stream 4))     ; uint32
      ((= byte +uint64+) (read-uint stream 8))     ; uint64
      ((= byte +int8+)                             ; int8
       (let ((val (read-byte stream)))
         (if (> val 127) (- val 256) val)))
      ((= byte +int16+)                            ; int16
       (let ((val (read-uint stream 2)))
         (if (> val 32767) (- val 65536) val)))
      ((= byte +int32+)                            ; int32
       (let ((val (read-uint stream 4)))
         (if (> val 2147483647) (- val 4294967296) val)))
      ((= byte +int64+)                            ; int64
       (let ((val (read-uint stream 8)))
         (if (> val 9223372036854775807) 
             (- val 18446744073709551616) 
             val)))
      
      ;; Floats
      ((= byte +float32+)                          ; float32
       (bits-to-single-float (read-uint stream 4)))
      ((= byte +float64+)                          ; float64
       (bits-to-double-float (read-uint stream 8)))
      
      ;; Strings
      ((<= +fixstr+ byte (+ +fixstr+ #x1f))        ; fixstr
       (let ((len (logand byte #x1f)))
         (decode-raw-string stream len)))
      ((= byte +str8+)                             ; str8
       (let ((len (read-byte stream)))
         (decode-raw-string stream len)))
      ((= byte +str16+)                            ; str16
       (let ((len (read-uint stream 2)))
         (decode-raw-string stream len)))
      ((= byte +str32+)                            ; str32
       (let ((len (read-uint stream 4)))
         (decode-raw-string stream len)))
      
      ;; Binary
      ((= byte +bin8+)                             ; bin8
       (let ((len (read-byte stream)))
         (decode-binary stream len)))
      ((= byte +bin16+)                            ; bin16
       (let ((len (read-uint stream 2)))
         (decode-binary stream len)))
      ((= byte +bin32+)                            ; bin32
       (let ((len (read-uint stream 4)))
         (decode-binary stream len)))
      
      ;; Extension Types
      ((= byte +fixext1+) (decode-ext stream 1))
      ((= byte +fixext2+) (decode-ext stream 2))
      ((= byte +fixext4+) (decode-ext stream 4))
      ((= byte +fixext8+) (decode-ext stream 8))
      ((= byte +fixext16+) (decode-ext stream 16))
      ((= byte +ext8+)
       (let ((len (read-byte stream)))
         (decode-ext stream len)))
      ((= byte +ext16+)
       (let ((len (read-uint stream 2)))
         (decode-ext stream len)))
      ((= byte +ext32+)
       (let ((len (read-uint stream 4)))
         (decode-ext stream len)))
      
      ;; Arrays
      ((<= +fixarray+ byte (+ +fixarray+ #xf))     ; fixarray
       (let ((len (logand byte #xf)))
         (decode-array stream len)))
      ((= byte +array16+)                          ; array16
       (let ((len (read-uint stream 2)))
         (decode-array stream len)))
      ((= byte +array32+)                          ; array32
       (let ((len (read-uint stream 4)))
         (decode-array stream len)))
      
      ;; Maps
      ((<= +fixmap+ byte (+ +fixmap+ #xf))         ; fixmap
       (let ((len (logand byte #xf)))
         (decode-map stream len)))
      ((= byte +map16+)                            ; map16
       (let ((len (read-uint stream 2)))
         (decode-map stream len)))
      ((= byte +map32+)                            ; map32
       (let ((len (read-uint stream 4)))
         (decode-map stream len)))
      
      (t (error "Unsupported MessagePack format: ~X" byte)))))

(defun read-uint (stream byte-count)
  "Read an unsigned integer of BYTE-COUNT bytes from STREAM in big-endian order."
  (let ((result 0))
    (dotimes (i byte-count result)
      (setf result (logior (ash result 8) (read-byte stream))))))

(defun decode-raw-string (stream length)
  "Decode a raw string of LENGTH bytes from STREAM."
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream :end length)
    (char:bytes-to-string bytes)))

(defun decode-binary (stream length)
  "Decode a binary blob of LENGTH bytes from STREAM."
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

(defun decode-array (stream length)
  "Decode an array of LENGTH elements from STREAM."
  (let ((result (make-array length)))
    (dotimes (i length result)
      (setf (aref result i) (decode-object stream)))))

(defun decode-map (stream length)
  "Decode a map with LENGTH key-value pairs from STREAM."
  (let ((result map:+empty+))
    (dotimes (i length result)
      (let ((key (decode-object stream))
            (value (decode-object stream)))
        (map:assoc! result key value)))))

(defun decode-ext (stream length)
  "Decode an extension type of LENGTH bytes from STREAM."
  (let ((type (read-byte stream))
        (data (make-array length :element-type 'u8)))
    (read-sequence data stream)
    (cond
      ((= type +timestamp-type+) (decode-timestamp data length))
      (t (list :ext type data)))))

(defun get-uint32 (array offset)
  "Get a 32-bit unsigned integer from ARRAY starting at OFFSET in big-endian order"
  (+ (ash (aref array offset) 24)
     (ash (aref array (+ offset 1)) 16)
     (ash (aref array (+ offset 2)) 8)
     (aref array (+ offset 3))))

(defun get-uint64 (array offset)
  "Get a 64-bit unsigned integer from ARRAY starting at OFFSET in big-endian order"
  (+ (ash (get-uint32 array offset) 32)
     (get-uint32 array (+ offset 4))))

(defun set-uint32 (array offset value)
  "Set a 32-bit unsigned integer in ARRAY starting at OFFSET in big-endian order"
  (setf (aref array offset) (ldb (byte 8 24) value)
        (aref array (+ offset 1)) (ldb (byte 8 16) value)
        (aref array (+ offset 2)) (ldb (byte 8 8) value)
        (aref array (+ offset 3)) (ldb (byte 8 0) value)))

(defun set-uint64 (array offset value)
  "Set a 64-bit unsigned integer in ARRAY starting at OFFSET in big-endian order"
  (set-uint32 array offset (ldb (byte 32 32) value))
  (set-uint32 array (+ offset 4) (ldb (byte 32 0) value)))

(defun encode-timestamp (stream timestamp)
  "Encode a timestamp as MessagePack extension type -1"
  (let* ((encoded-timestamp (if (typep timestamp 'time:timestamp)
                               (time:encode-unix-timestamp timestamp)
                               (if (listp timestamp)
                                   timestamp
                                   (error "Expected timestamp format: time:timestamp or (:timestamp seconds nanoseconds)"))))
         (seconds (second encoded-timestamp))
         (nsec (if (>= (length encoded-timestamp) 3)
                  (third encoded-timestamp)
                  0)))
    (cond
      ;; 32-bit format for seconds only (no nanoseconds, seconds < 2^32)
      ((and (zerop nsec) (<= 0 seconds #xFFFFFFFF))
       (let ((data (make-array 4 :element-type 'u8)))
         (set-uint32 data 0 seconds)
         (encode-ext stream +timestamp-type+ data)))
      
      ;; 64-bit format when nanoseconds needed but still fits
      ((and (<= 0 nsec #x3FFFFFFF) ; 30 bits for nanoseconds
            (<= 0 seconds #x3FFFFFFFF)) ; 34 bits for seconds
       (let ((data (make-array 8 :element-type 'u8))
             (combined (logior (ash nsec 34) seconds)))
         (set-uint64 data 0 combined)
         (encode-ext stream +timestamp-type+ data)))
      
      ;; 96-bit format for larger values
      (t
       (let ((data (make-array 12 :element-type 'u8)))
         (set-uint32 data 0 nsec)
         (set-uint64 data 4 seconds)
         (encode-ext stream +timestamp-type+ data))))))

(defun decode-timestamp (data length)
  "Decode a timestamp extension type from DATA of LENGTH bytes."
  (let ((timestamp-list 
         (case length
           (4 (list :timestamp (get-uint32 data 0) 0))
           (8 (let ((combined (get-uint64 data 0))
                    (seconds (ldb (byte 34 0) (get-uint64 data 0)))
                    (nano-seconds (ldb (byte 30 34) (get-uint64 data 0))))
                (list :timestamp seconds nano-seconds)))
           (12 (let ((nano-seconds (get-uint32 data 0))
                     (seconds (get-uint64 data 4)))
                 (list :timestamp seconds nano-seconds)))
           (t (error "Invalid timestamp extension length: ~A" length)))))
    (time:decode-unix-timestamp timestamp-list)))
