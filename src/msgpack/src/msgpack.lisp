
;;;; https://github.com/msgpack/msgpack/blob/master/spec.md
;;;;
;;;; MessagePack is an object serialization specification like JSON.
;;;;
;;;; MessagePack has two concepts: type system and formats.
;;;;
;;;; Serialization is conversion from application objects into
;;;; MessagePack formats via MessagePack type system.
;;;;
;;;; Deserialization is conversion from MessagePack formats into
;;;; application objects via MessagePack type system.

(defpackage epsilon.msgpack
  (:use cl)
  (:local-nicknames
   (binary epsilon.binary)
   (char epsilon.char)
   (map epsilon.map)
   (time epsilon.time))
  (:export
   ;; Binary structure types
   msgpack-header
   msgpack-string
   msgpack-binary
   msgpack-array-header
   msgpack-map-header
   msgpack-ext
   msgpack-timestamp
   
   ;; Encoding/decoding functions
   encode
   decode
   
   ;; Format detection
   detect-msgpack-format))

(in-package epsilon.msgpack)

;;;; MessagePack binary structure definitions using epsilon.binary

;;; Format constants
(defconstant +nil+ #xc0)
(defconstant +false+ #xc2)
(defconstant +true+ #xc3)
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
(defconstant +fixstr+ #xa0)
(defconstant +fixarray+ #x90)
(defconstant +fixmap+ #x80)
(defconstant +timestamp-type+ -1)

;;; Basic MessagePack structures

(binary:define-binary-struct msgpack-uint8 (:endian :big-endian)
  (format :u8 :value +uint8+)
  (value :u8))

(binary:define-binary-struct msgpack-uint16 (:endian :big-endian)
  (format :u8 :value +uint16+)
  (value :u16))

(binary:define-binary-struct msgpack-uint32 (:endian :big-endian)
  (format :u8 :value +uint32+)
  (value :u32))

(binary:define-binary-struct msgpack-uint64 (:endian :big-endian)
  (format :u8 :value +uint64+)
  (value :u64))

(binary:define-binary-struct msgpack-int8 (:endian :big-endian)
  (format :u8 :value +int8+)
  (value :s8))

(binary:define-binary-struct msgpack-int16 (:endian :big-endian)
  (format :u8 :value +int16+)
  (value :s16))

(binary:define-binary-struct msgpack-int32 (:endian :big-endian)
  (format :u8 :value +int32+)
  (value :s32))

(binary:define-binary-struct msgpack-int64 (:endian :big-endian)
  (format :u8 :value +int64+)
  (value :s64))

(binary:define-binary-struct msgpack-float32 (:endian :big-endian)
  (format :u8 :value +float32+)
  (value :f32))

(binary:define-binary-struct msgpack-float64 (:endian :big-endian)
  (format :u8 :value +float64+)
  (value :f64))

;;; String structures

(binary:define-binary-struct msgpack-str8 (:endian :big-endian)
  (format :u8 :value +str8+)
  (length :u8)
  (data :string :size length))

(binary:define-binary-struct msgpack-str16 (:endian :big-endian)
  (format :u8 :value +str16+)
  (length :u16)
  (data :string :size length))

(binary:define-binary-struct msgpack-str32 (:endian :big-endian)
  (format :u8 :value +str32+)
  (length :u32)
  (data :string :size length))

;;; Binary data structures

(binary:define-binary-struct msgpack-bin8 (:endian :big-endian)
  (format :u8 :value +bin8+)
  (length :u8)
  (data :bytes :size length))

(binary:define-binary-struct msgpack-bin16 (:endian :big-endian)
  (format :u8 :value +bin16+)
  (length :u16)
  (data :bytes :size length))

(binary:define-binary-struct msgpack-bin32 (:endian :big-endian)
  (format :u8 :value +bin32+)
  (length :u32)
  (data :bytes :size length))

;;; Array structures

(binary:define-binary-struct msgpack-array16 (:endian :big-endian)
  (format :u8 :value +array16+)
  (length :u16))

(binary:define-binary-struct msgpack-array32 (:endian :big-endian)
  (format :u8 :value +array32+)
  (length :u32))

;;; Map structures

(binary:define-binary-struct msgpack-map16 (:endian :big-endian)
  (format :u8 :value +map16+)
  (length :u16))

(binary:define-binary-struct msgpack-map32 (:endian :big-endian)
  (format :u8 :value +map32+)
  (length :u32))

;;; Extension type structures

(binary:define-binary-struct msgpack-fixext1 (:endian :big-endian)
  (format :u8 :value +fixext1+)
  (type :s8)
  (data :bytes :size 1))

(binary:define-binary-struct msgpack-fixext2 (:endian :big-endian)
  (format :u8 :value +fixext2+)
  (type :s8)
  (data :bytes :size 2))

(binary:define-binary-struct msgpack-fixext4 (:endian :big-endian)
  (format :u8 :value +fixext4+)
  (type :s8)
  (data :bytes :size 4))

(binary:define-binary-struct msgpack-fixext8 (:endian :big-endian)
  (format :u8 :value +fixext8+)
  (type :s8)
  (data :bytes :size 8))

(binary:define-binary-struct msgpack-fixext16 (:endian :big-endian)
  (format :u8 :value +fixext16+)
  (type :s8)
  (data :bytes :size 16))

(binary:define-binary-struct msgpack-ext8 (:endian :big-endian)
  (format :u8 :value +ext8+)
  (length :u8)
  (type :s8)
  (data :bytes :size length))

(binary:define-binary-struct msgpack-ext16 (:endian :big-endian)
  (format :u8 :value +ext16+)
  (length :u16)
  (type :s8)
  (data :bytes :size length))

(binary:define-binary-struct msgpack-ext32 (:endian :big-endian)
  (format :u8 :value +ext32+)
  (length :u32)
  (type :s8)
  (data :bytes :size length))

;;; Timestamp structures (extension type -1)

(binary:define-binary-struct msgpack-timestamp32 (:endian :big-endian)
  (seconds :u32))

(binary:define-binary-struct msgpack-timestamp64 (:endian :big-endian)
  (combined :u64))

(binary:define-binary-struct msgpack-timestamp96 (:endian :big-endian)
  (nanoseconds :u32)
  (seconds :u64))

;;; Format detection utilities

(defun detect-msgpack-format (byte)
  "Detect MessagePack format from first byte"
  (cond
    ;; Fixed format detection
    ((= byte +nil+) :nil)
    ((= byte +true+) :true)
    ((= byte +false+) :false)
    
    ;; Integer formats
    ((< byte #x80) :positive-fixint)
    ((>= byte #xe0) :negative-fixint)
    ((= byte +uint8+) :uint8)
    ((= byte +uint16+) :uint16)
    ((= byte +uint32+) :uint32)
    ((= byte +uint64+) :uint64)
    ((= byte +int8+) :int8)
    ((= byte +int16+) :int16)
    ((= byte +int32+) :int32)
    ((= byte +int64+) :int64)
    
    ;; Float formats
    ((= byte +float32+) :float32)
    ((= byte +float64+) :float64)
    
    ;; String formats
    ((<= #xa0 byte #xbf) :fixstr)
    ((= byte +str8+) :str8)
    ((= byte +str16+) :str16)
    ((= byte +str32+) :str32)
    
    ;; Binary formats
    ((= byte +bin8+) :bin8)
    ((= byte +bin16+) :bin16)
    ((= byte +bin32+) :bin32)
    
    ;; Array formats
    ((<= #x90 byte #x9f) :fixarray)
    ((= byte +array16+) :array16)
    ((= byte +array32+) :array32)
    
    ;; Map formats
    ((<= #x80 byte #x8f) :fixmap)
    ((= byte +map16+) :map16)
    ((= byte +map32+) :map32)
    
    ;; Extension formats
    ((= byte +fixext1+) :fixext1)
    ((= byte +fixext2+) :fixext2)
    ((= byte +fixext4+) :fixext4)
    ((= byte +fixext8+) :fixext8)
    ((= byte +fixext16+) :fixext16)
    ((= byte +ext8+) :ext8)
    ((= byte +ext16+) :ext16)
    ((= byte +ext32+) :ext32)
    
    (t :unknown)))

;;; Optimized direct writers (avoiding struct allocation)

(defmacro define-direct-writer (name format-byte &rest fields)
  "Define a direct writer function that avoids struct allocation"
  (let ((stream-var (gensym "STREAM"))
        (field-vars (mapcar (lambda (field) 
                              (if (listp field) (first field) field)) 
                            fields)))
    `(defun ,name (,stream-var ,@field-vars)
       (declare (optimize (speed 3) (safety 0)))
       ,@(when format-byte
           `((binary:write ,stream-var ,format-byte :u8)))
       ,@(mapcar (lambda (field)
                   (if (listp field)
                       (let ((var (first field))
                             (type (second field)))
                         `(binary:write ,stream-var ,var ,type :big-endian))
                       `(binary:write ,stream-var ,field :u8)))
                 fields))))

;; Define optimized direct writers for common MessagePack formats
(define-direct-writer write-uint8-direct +uint8+ (value :u8))
(define-direct-writer write-uint16-direct +uint16+ (value :u16))
(define-direct-writer write-uint32-direct +uint32+ (value :u32))
(define-direct-writer write-uint64-direct +uint64+ (value :u64))
(define-direct-writer write-int8-direct +int8+ (value :s8))
(define-direct-writer write-int16-direct +int16+ (value :s16))
(define-direct-writer write-int32-direct +int32+ (value :s32))
(define-direct-writer write-int64-direct +int64+ (value :s64))
(define-direct-writer write-float32-direct +float32+ (value :f32))
(define-direct-writer write-float64-direct +float64+ (value :f64))


;;; Encoding helpers

(defun encode-integer-with-structs (stream value)
  "Encode integer using optimized direct writers"
  (declare (optimize (speed 3) (safety 0)))
  (cond
    ((<= 0 value 127)
     (binary:write stream value :u8))
    ((<= 0 value 255)
     (write-uint8-direct stream value))
    ((<= 0 value 65535)
     (write-uint16-direct stream value))
    ((<= 0 value #xFFFFFFFF)
     (write-uint32-direct stream value))
    ((<= 0 value #xFFFFFFFFFFFFFFFF)
     (write-uint64-direct stream value))
    ((<= -32 value -1)
     (binary:write stream (logand value #xFF) :u8))
    ((<= -128 value -1)
     (write-int8-direct stream value))
    ((<= -32768 value -1)
     (write-int16-direct stream value))
    ((<= #x-80000000 value -1)
     (write-int32-direct stream value))
    ((<= #x-8000000000000000 value -1)
     (write-int64-direct stream value))
    (t (error "Integer too large to encode: ~A" value))))

(defun encode-string-with-structs (stream string)
  "Encode string using binary structures"
  (let* ((bytes (char:string-to-bytes string))
         (length (length bytes)))
    (cond
      ((<= length 31)
       ;; fixstr format
       (binary:write stream (logior #xa0 length) :u8)
       (binary:write-bytes stream bytes))
      ((<= length 255)
       (let ((s (make-msgpack-str8-struct :format +str8+ :length length :data string)))
         (binary:write stream s 'msgpack-str8)))
      ((<= length 65535)
       (let ((s (make-msgpack-str16-struct :format +str16+ :length length :data string)))
         (binary:write stream s 'msgpack-str16)))
      (t
       (let ((s (make-msgpack-str32-struct :format +str32+ :length length :data string)))
         (binary:write stream s 'msgpack-str32))))))

(defun encode-binary-with-structs (stream bytes)
  "Encode binary data using binary structures"
  (let ((length (length bytes)))
    (cond
      ((<= length 255)
       (let ((s (make-msgpack-bin8-struct :format +bin8+ :length length :data bytes)))
         (binary:write stream s 'msgpack-bin8)))
      ((<= length 65535)
       (let ((s (make-msgpack-bin16-struct :format +bin16+ :length length :data bytes)))
         (binary:write stream s 'msgpack-bin16)))
      (t
       (let ((s (make-msgpack-bin32-struct :format +bin32+ :length length :data bytes)))
         (binary:write stream s 'msgpack-bin32))))))

(defun encode-array-header-with-structs (stream length)
  "Encode array header using binary structures"
  (cond
    ((<= length 15)
     ;; fixarray format
     (binary:write stream (logior #x90 length) :u8))
    ((<= length 65535)
     (let ((s (make-msgpack-array16-struct :format +array16+ :length length)))
       (binary:write stream s 'msgpack-array16)))
    (t
     (let ((s (make-msgpack-array32-struct :format +array32+ :length length)))
       (binary:write stream s 'msgpack-array32)))))

(defun encode-map-header-with-structs (stream length)
  "Encode map header using binary structures"
  (cond
    ((<= length 15)
     ;; fixmap format
     (binary:write stream (logior #x80 length) :u8))
    ((<= length 65535)
     (let ((s (make-msgpack-map16-struct :format +map16+ :length length)))
       (binary:write stream s 'msgpack-map16)))
    (t
     (let ((s (make-msgpack-map32-struct :format +map32+ :length length)))
       (binary:write stream s 'msgpack-map32)))))

(defun encode-extension-with-structs (stream type-code data)
  "Encode extension type using binary structures"
  (let ((length (length data)))
    (cond
      ((= length 1)
       (binary:write stream +fixext1+ :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((= length 2)
       (binary:write stream +fixext2+ :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((= length 4)
       (binary:write stream +fixext4+ :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((= length 8)
       (binary:write stream +fixext8+ :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((= length 16)
       (binary:write stream +fixext16+ :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((<= length 255)
       (binary:write stream +ext8+ :u8)
       (binary:write stream length :u8)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      ((<= length 65535)
       (binary:write stream +ext16+ :u8)
       (binary:write stream length :u16 :big-endian)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data))
      (t
       (binary:write stream +ext32+ :u8)
       (binary:write stream length :u32 :big-endian)
       (binary:write stream type-code :s8)
       (binary:write-bytes stream data)))))

;;; Main encoding function

(defun encode (object)
  "Encode object"
  (let ((stream (make-instance 'binary:binary-output-stream)))
    (encode-object-with-structs stream object)
    (binary:get-output-stream-bytes stream)))

(defun encode-object-with-structs (stream object)
  "Encode a single object using binary structures"
  (declare (optimize (speed 3) (safety 1)))
  (typecase object
    (null (binary:write stream +nil+ :u8))
    ((eql t) (binary:write stream +true+ :u8))
    (fixnum ; Most common integer type
     (encode-integer-with-structs stream object))
    (string
     (encode-string-with-structs stream object))
    (list
     (if (and (>= (length object) 3) (eq (first object) :ext))
         ;; Handle extension type: (:ext type-code data)
         (let ((type-code (second object))
               (data (third object)))
           (encode-extension-with-structs stream type-code data))
         ;; Regular array
         (progn
           (encode-array-header-with-structs stream (length object))
           (dolist (element object)
             (encode-object-with-structs stream element)))))
    (single-float
     (write-float32-direct stream object))
    (double-float
     (write-float64-direct stream object))
    (integer ; Non-fixnum integers
     (encode-integer-with-structs stream object))
    (symbol
     (encode-string-with-structs stream (symbol-name object)))
    ((simple-array (unsigned-byte 8) (*))
     (encode-binary-with-structs stream object))
    (vector
     (if (subtypep (array-element-type object) '(unsigned-byte 8))
         (encode-binary-with-structs stream object)
         (progn
           (encode-array-header-with-structs stream (length object))
           (loop for element across object
                 do (encode-object-with-structs stream element)))))
    ;; Handle HAMT maps from epsilon.map
    (structure-object
     (if (typep object 'epsilon.map::hamt)
         (let ((pairs (map:seq object)))
           (encode-map-header-with-structs stream (map:count object))
           (dolist (pair pairs)
             (encode-object-with-structs stream (car pair))
             (encode-object-with-structs stream (cdr pair))))
         (error "Cannot encode structure of type ~A" (type-of object))))
    (t
     (error "Cannot encode object of type ~A" (type-of object)))))

;;; Decoding using binary structures

(defun decode (bytes)
  "Decode MessagePack data"
  (let ((stream (make-instance 'binary:binary-input-stream :data bytes :position 0)))
    (decode-object-with-structs stream)))

;; TODO This should be a static dispatch table

(defun decode-object-with-structs (stream)
  "Decode a single MessagePack object using binary structures"
  (let ((format-byte (binary:read stream :u8)))
    (cond
      ;; nil, true, false
      ((= format-byte +nil+) nil)
      ((= format-byte +true+) t)
      ((= format-byte +false+) nil)
      
      ;; Integers
      ((< format-byte #x80) format-byte)               ; positive fixint (0-127)
      ((>= format-byte #xe0) (- format-byte #x100))    ; negative fixint (-32 to -1)
      ((= format-byte +uint8+)
       (binary:read stream :u8))
      ((= format-byte +uint16+)
       (binary:read stream :u16 :big-endian))
      ((= format-byte +uint32+)
       (binary:read stream :u32 :big-endian))
      ((= format-byte +uint64+)
       (binary:read stream :u64 :big-endian))
      ((= format-byte +int8+)
       (binary:read stream :s8))
      ((= format-byte +int16+)
       (binary:read stream :s16 :big-endian))
      ((= format-byte +int32+)
       (binary:read stream :s32 :big-endian))
      ((= format-byte +int64+)
       (binary:read stream :s64 :big-endian))
      
      ;; Floats
      ((= format-byte +float32+)
       (binary:read stream :f32 :big-endian))
      ((= format-byte +float64+)
       (binary:read stream :f64 :big-endian))
      
      ;; Strings
      ((<= +fixstr+ format-byte (+ +fixstr+ #x1f))     ; fixstr
       (let ((len (logand format-byte #x1f)))
         (decode-string-with-structs stream len)))
      ((= format-byte +str8+)
       (let ((len (binary:read stream :u8)))
         (decode-string-with-structs stream len)))
      ((= format-byte +str16+)
       (let ((len (binary:read stream :u16 :big-endian)))
         (decode-string-with-structs stream len)))
      ((= format-byte +str32+)
       (let ((len (binary:read stream :u32 :big-endian)))
         (decode-string-with-structs stream len)))
      
      ;; Binary
      ((= format-byte +bin8+)
       (let ((len (binary:read stream :u8)))
         (binary:read-bytes stream len)))
      ((= format-byte +bin16+)
       (let ((len (binary:read stream :u16 :big-endian)))
         (binary:read-bytes stream len)))
      ((= format-byte +bin32+)
       (let ((len (binary:read stream :u32 :big-endian)))
         (binary:read-bytes stream len)))
      
      ;; Arrays
      ((<= +fixarray+ format-byte (+ +fixarray+ #xf))  ; fixarray
       (let ((len (logand format-byte #xf)))
         (decode-array-with-structs stream len)))
      ((= format-byte +array16+)
       (let ((len (binary:read stream :u16 :big-endian)))
         (decode-array-with-structs stream len)))
      ((= format-byte +array32+)
       (let ((len (binary:read stream :u32 :big-endian)))
         (decode-array-with-structs stream len)))
      
      ;; Maps
      ((<= +fixmap+ format-byte (+ +fixmap+ #xf))      ; fixmap
       (let ((len (logand format-byte #xf)))
         (decode-map-with-structs stream len)))
      ((= format-byte +map16+)
       (let ((len (binary:read stream :u16 :big-endian)))
         (decode-map-with-structs stream len)))
      ((= format-byte +map32+)
       (let ((len (binary:read stream :u32 :big-endian)))
         (decode-map-with-structs stream len)))
      
      ;; Extension types
      ((= format-byte +fixext1+)
       (let ((type-code (binary:read stream :s8))
             (data (binary:read-bytes stream 1)))
         (list :ext type-code data)))
      ((= format-byte +fixext2+)
       (let ((type-code (binary:read stream :s8))
             (data (binary:read-bytes stream 2)))
         (list :ext type-code data)))
      ((= format-byte +fixext4+)
       (let ((type-code (binary:read stream :s8))
             (data (binary:read-bytes stream 4)))
         (list :ext type-code data)))
      ((= format-byte +fixext8+)
       (let ((type-code (binary:read stream :s8))
             (data (binary:read-bytes stream 8)))
         (list :ext type-code data)))
      ((= format-byte +fixext16+)
       (let ((type-code (binary:read stream :s8))
             (data (binary:read-bytes stream 16)))
         (list :ext type-code data)))
      ((= format-byte +ext8+)
       (let* ((len (binary:read stream :u8))
              (type-code (binary:read stream :s8))
              (data (binary:read-bytes stream len)))
         (list :ext type-code data)))
      ((= format-byte +ext16+)
       (let* ((len (binary:read stream :u16 :big-endian))
              (type-code (binary:read stream :s8))
              (data (binary:read-bytes stream len)))
         (list :ext type-code data)))
      ((= format-byte +ext32+)
       (let* ((len (binary:read stream :u32 :big-endian))
              (type-code (binary:read stream :s8))
              (data (binary:read-bytes stream len)))
         (list :ext type-code data)))
      
      (t (error "Unsupported MessagePack format: ~X" format-byte)))))

(defun decode-string-with-structs (stream length)
  "Decode a string of specified length"
  (let ((bytes (binary:read-bytes stream length)))
    (char:bytes-to-string bytes)))

(defun decode-array-with-structs (stream length)
  "Decode an array of specified length"
  (let ((result (make-array length)))
    (dotimes (i length result)
      (setf (aref result i) (decode-object-with-structs stream)))))

(defun decode-map-with-structs (stream length)
  "Decode a map with specified number of key-value pairs"
  (let ((result map:+empty+))
    (dotimes (i length result)
      (let ((key (decode-object-with-structs stream))
            (value (decode-object-with-structs stream)))
        (setf result (map:assoc result key value))))))
