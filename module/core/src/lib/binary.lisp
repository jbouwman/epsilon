(defpackage epsilon.lib.binary
  (:use
   cl)
  (:local-nicknames
   (map epsilon.lib.map))
  (:shadow
   read
   read-sequence
   write
   write-sequence
   stream-error
   stream-error-stream)
  (:export

   ;; main api
   read
   write
   read-sequence
   write-sequence
   read-bytes
   write-bytes
   to-bytes
   from-bytes
   size
   
   ;; type definition
   define-binary-type
   define-binary-struct
   
   ;; endianness
   *default-endian*
   with-endian
   
   ;; conditions
   binary-error
   unknown-type
   stream-error
   
   ;; structure support
   binary-input-stream
   binary-output-stream
   get-output-stream-bytes))

;;;; An API for reading and writing binary data with explicit
;;;; endianness control.

(in-package :epsilon.lib.binary)

;;; Configuration

(defparameter *default-endian* :native
  "Default endianness for binary operations. Can be :little-endian, :big-endian, or :native")

(defparameter *native-endian*
  #+little-endian :little-endian
  #+big-endian :big-endian
  #-(or little-endian big-endian) :little-endian
  "The native endianness of the current platform")

;;; Conditions

(define-condition binary-error (error)
  ()
  (:documentation "Base condition for binary I/O errors"))

(define-condition unknown-type (binary-error)
  ((type :initarg :type :reader unknown-type-type))
  (:report (lambda (condition stream)
             (format stream "Unknown binary type: ~A" (unknown-type-type condition)))))

(define-condition stream-error (binary-error)
  ((operation :initarg :operation :reader stream-error-operation)
   (stream :initarg :stream :reader stream-error-stream))
  (:report (lambda (condition stream)
             (format stream "Binary ~A error on stream ~A"
                     (stream-error-operation condition)
                     (stream-error-stream condition)))))

;;; Type registry

(defstruct binary-type
  "Describes how to read/write a binary type"
  name
  size          ; integer or function of (instance) -> integer
  alignment     ; byte alignment requirement
  reader        ; function of (stream endian) -> value
  writer        ; function (stream value endian) -> nil
  encoder       ; function of (value endian) -> bytes
  decoder)      ; function of (bytes offset endian) -> value

(defvar *binary-types* map:+empty+
  "Registry of known binary types")

(defun get-binary-type (type)
  "Look up a binary type, signaling error if not found"
  (let ((type-key (if (keywordp type)
                      type
                      (intern (symbol-name type) :keyword))))
    (or (map:get *binary-types* type-key)
        (error 'unknown-type :type type))))

(defun resolve-endian (endian)
  "Resolve endianness keyword"
  (ecase endian
    ((:native) *native-endian*)
    ((:little-endian :little :le) :little-endian)
    ((:big-endian :big :be :network) :big-endian)))

;;; Low-level byte operations

(declaim (inline get-u8 set-u8))

(defun get-u8 (bytes offset)
  "Get unsigned 8-bit value from bytes"
  (aref bytes offset))

(defun set-u8 (bytes offset value)
  "Set unsigned 8-bit value in bytes"
  (setf (aref bytes offset) (ldb (byte 8 0) value)))

(defun get-bytes-le (bytes offset size)
  "Get little-endian unsigned integer from bytes"
  (loop for i from 0 below size
        sum (ash (aref bytes (+ offset i)) (* i 8))))

(defun get-bytes-be (bytes offset size)
  "Get big-endian unsigned integer from bytes"
  (loop for i from 0 below size
        sum (ash (aref bytes (+ offset i)) (* (- size i 1) 8))))

(defun set-bytes-le (bytes offset value size)
  "Set little-endian unsigned integer in bytes"
  (loop for i from 0 below size
        do (setf (aref bytes (+ offset i))
                 (ldb (byte 8 (* i 8)) value))))

(defun set-bytes-be (bytes offset value size)
  "Set big-endian unsigned integer in bytes"
  (loop for i from 0 below size
        do (setf (aref bytes (+ offset i))
                 (ldb (byte 8 (* (- size i 1) 8)) value))))

(defun get-signed (value size)
  "Convert unsigned value to signed based on two's complement"
  (let ((sign-bit (ash 1 (1- (* size 8)))))
    (if (logbitp (1- (* size 8)) value)
        (- value (ash 1 (* size 8)))
        value)))

;;; Forward declarations and utility functions

;;; Forward declare binary stream classes
(defclass binary-input-stream ()
  ((data :initarg :data :reader stream-data)
   (position :initarg :position :initform 0 :accessor stream-position)))

(defclass binary-output-stream ()
  ((data :initform (make-array 0 :element-type '(unsigned-byte 8) 
                               :adjustable t :fill-pointer 0)
         :accessor stream-data)))

;;; Stream method declarations
(defgeneric stream-read-byte (stream &optional eof-error-p eof-value))
(defgeneric stream-write-byte (stream byte))
(defgeneric stream-read-sequence (stream sequence &key start end))
(defgeneric stream-write-sequence (stream sequence &key start end))

;;; Stream method implementations
(defmethod stream-read-byte ((stream binary-input-stream) &optional eof-error-p eof-value)
  (declare (ignore eof-error-p))
  (with-slots (data position) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        eof-value)))

(defmethod stream-write-byte ((stream binary-output-stream) byte)
  (vector-push-extend byte (stream-data stream)))

(defmethod stream-read-sequence ((stream binary-input-stream) sequence &key start end)
  (let ((start (or start 0))
        (end (or end (length sequence))))
    (with-slots (data position) stream
      (loop for i from start below end
            while (< position (length data))
            do (setf (elt sequence i) (aref data position))
               (incf position)
            finally (return i)))))

(defmethod stream-write-sequence ((stream binary-output-stream) sequence &key start end)
  (let ((start (or start 0))
        (end (or end (length sequence))))
    (loop for i from start below end
          do (vector-push-extend (elt sequence i) (stream-data stream)))
    sequence))

(defun infer-type (value)
  "Attempt to infer binary type from value"
  (typecase value
    ((unsigned-byte 8) :u8)
    ((signed-byte 8) :s8)
    ((unsigned-byte 16) :u16)
    ((signed-byte 16) :s16)
    ((unsigned-byte 32) :u32)
    ((signed-byte 32) :s32)
    ((unsigned-byte 64) :u64)
    ((signed-byte 64) :s64)
    (single-float :f32)
    (double-float :f64)
    (t (error "Cannot infer binary type for ~A" value))))

(defun binary-read-byte (stream)
  "Read a byte from stream, using appropriate method"
  (etypecase stream
    (binary-input-stream (stream-read-byte stream))
    (stream (cl:read-byte stream))))

(defun binary-write-byte (stream byte)
  "Write a byte to stream, using appropriate method"
  (etypecase stream
    (binary-output-stream (stream-write-byte stream byte))
    (stream (cl:write-byte byte stream))))

(defun binary-read-sequence (stream sequence &key start end)
  "Read a sequence from stream, using appropriate method"
  (etypecase stream
    (binary-input-stream (stream-read-sequence stream sequence :start start :end end))
    (stream (if (and start end)
                (cl:read-sequence sequence stream :start start :end end)
                (if start
                    (cl:read-sequence sequence stream :start start)
                    (if end
                        (cl:read-sequence sequence stream :end end)
                        (cl:read-sequence sequence stream)))))))

(defun binary-write-sequence (stream sequence &key start end)
  "Write a sequence to stream, using appropriate method"
  (etypecase stream
    (binary-output-stream (stream-write-sequence stream sequence :start start :end end))
    (stream (if (and start end)
                (cl:write-sequence sequence stream :start start :end end)
                (if start
                    (cl:write-sequence sequence stream :start start)
                    (if end
                        (cl:write-sequence sequence stream :end end)
                        (cl:write-sequence sequence stream)))))))

(defun get-output-stream-bytes (stream)
  "Get the bytes written to a binary output stream"
  (stream-data stream))

;;; Main API

(defun read (stream type &optional (endian *default-endian*))
  "Read a value of TYPE from STREAM with specified ENDIAN"
  (let ((type-info (get-binary-type type))
        (endian (resolve-endian endian)))
    (handler-case
        (funcall (binary-type-reader type-info) stream endian)
      (end-of-file ()
        (error 'stream-error :operation 'read :stream stream)))))

(defun write (stream value &optional type (endian *default-endian*))
  "Write VALUE to STREAM as TYPE with specified ENDIAN"
  (let* ((type (or type (infer-type value)))
         (type-info (get-binary-type type))
         (endian (resolve-endian endian)))
    (funcall (binary-type-writer type-info) stream value endian)
    value))

(defun to-bytes (value type &optional (endian *default-endian*))
  "Convert VALUE to byte vector representation"
  (let ((type-info (get-binary-type type))
        (endian (resolve-endian endian)))
    (funcall (binary-type-encoder type-info) value endian)))

(defun from-bytes (bytes type &optional (endian *default-endian*) (offset 0))
  "Parse value from BYTES at OFFSET"
  (let ((type-info (get-binary-type type))
        (endian (resolve-endian endian)))
    (funcall (binary-type-decoder type-info) bytes offset endian)))

(defun size (type-or-value &optional type)
  "Get size in bytes of a type or value"
  (cond
    ;; Called on a type symbol
    ((and (symbolp type-or-value) (null type))
     (let ((type-info (get-binary-type type-or-value)))
       (etypecase (binary-type-size type-info)
         (integer (binary-type-size type-info))
         (function (error "Type ~A has variable size" type-or-value)))))
    ;; Called on a value with explicit type
    (type
     (let ((type-info (get-binary-type type)))
       (etypecase (binary-type-size type-info)
         (integer (binary-type-size type-info))
         (function (funcall (binary-type-size type-info) type-or-value)))))
    ;; Called on a value, infer type
    (t
     (size type-or-value (infer-type type-or-value)))))

(defun read-bytes (stream count)
  "Read COUNT bytes from STREAM"
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (let ((read-count (binary-read-sequence stream bytes)))
      (unless (= read-count count)
        (error 'stream-error :operation 'read-bytes :stream stream)))
    bytes))

(defun write-bytes (stream bytes &key (start 0) end)
  "Write BYTES to STREAM"
  (binary-write-sequence stream bytes :start start :end end))

(defun read-sequence (stream type count &optional (endian *default-endian*))
  "Read COUNT elements of TYPE from STREAM with optimized bulk operations"
  (if (> count 0)
      (read-sequence-optimized stream type count endian)
      (make-array 0)))

(defun read-sequence-optimized (stream type count endian)
  "Optimized sequence reading using bulk byte array operations"
  (let* ((type-info (get-binary-type type))
         (element-size (binary-type-size type-info))
         (total-bytes (* element-size count))
         (buffer (read-bytes stream total-bytes))
         (result (make-array count)))
    
    ;; Decode all values from the buffer
    (loop for i from 0 below count
          for offset from 0 by element-size
          do (setf (aref result i)
                   (funcall (binary-type-decoder type-info) buffer offset endian)))
    
    result))

(defun write-sequence (stream sequence &optional type (endian *default-endian*))
  "Write SEQUENCE to STREAM with optimized bulk operations"
  (cond
    ((zerop (length sequence))
     ;; Empty sequence, nothing to do
     nil)
    (type
     ;; Type specified, use optimized bulk operations
     (write-sequence-optimized stream sequence type endian))
    (t
     ;; No type specified, write each value individually with type inference
     (map nil (lambda (value) (write stream value nil endian)) sequence))))

(defun write-sequence-optimized (stream sequence type endian)
  "Optimized sequence writing using bulk byte array operations"
  (let* ((type-info (get-binary-type type))
         (element-size (binary-type-size type-info))
         (sequence-length (length sequence))
         (total-bytes (* element-size sequence-length))
         (buffer (make-array total-bytes :element-type '(unsigned-byte 8))))
    
    ;; Encode all values into a single buffer
    (loop for value across sequence
          for offset from 0 by element-size
          do (let ((element-bytes (funcall (binary-type-encoder type-info) value endian)))
               (replace buffer element-bytes :start1 offset)))
    
    ;; Single bulk write operation
    (cl:write-sequence buffer stream)))

(defmacro with-endian ((endian) &body body)
  "Execute BODY with *default-endian* bound to ENDIAN"
  `(let ((*default-endian* ,endian))
     ,@body))

;;; Type definition

(defmacro define-binary-type (name size &body options)
  "Define a new binary type"
  (let ((alignment (getf options :alignment 1))
        (reader-body (cdr (assoc :reader options)))
        (writer-body (cdr (assoc :writer options)))
        (encoder-body (cdr (assoc :encoder options)))
        (decoder-body (cdr (assoc :decoder options))))
    `(map:assoc! *binary-types* ,(if (keywordp name) `',name `(intern (symbol-name ',name) :keyword))
                 (make-binary-type
                  :name ',name
                  :size ,size
                  :alignment ,alignment
                  :reader (lambda (stream endian)
                            (declare (ignorable endian))
                            ,@reader-body)
                  :writer (lambda (stream value endian)
                            (declare (ignorable endian))
                            ,@writer-body)
                  :encoder (lambda (value endian)
                             (declare (ignorable endian))
                             ,@encoder-body)
                  :decoder (lambda (bytes offset endian)
                             (declare (ignorable endian))
                             ,@decoder-body)))))

(defun register-binary-type (name size reader writer encoder decoder)
  "Register a binary type programmatically"
  (setf *binary-types*
        (map:assoc *binary-types* 
                   (if (keywordp name) name (intern (symbol-name name) :keyword))
                   (make-binary-type
                    :name name
                    :size size
                    :reader reader
                    :writer writer
                    :encoder encoder
                    :decoder decoder))))

;;; Primitive type definitions

(define-binary-type :u8 1
  (:reader (binary-read-byte stream))
  (:writer (binary-write-byte stream value))
  (:encoder (let ((bytes (make-array 1 :element-type '(unsigned-byte 8))))
              (set-u8 bytes 0 value)
              bytes))
  (:decoder (get-u8 bytes offset)))

(define-binary-type :s8 1
  (:reader (get-signed (binary-read-byte stream) 1))
  (:writer (binary-write-byte stream (ldb (byte 8 0) value)))
  (:encoder (let ((bytes (make-array 1 :element-type '(unsigned-byte 8))))
              (set-u8 bytes 0 (ldb (byte 8 0) value))
              bytes))
  (:decoder (get-signed (get-u8 bytes offset) 1)))

;; Macro to generate integer types
(defmacro define-integer-type (name signed-p bytes)
  `(define-binary-type ,name ,bytes
     (:reader
      (let ((bytes (read-bytes stream ,bytes)))
        (let ((value (ecase endian
                       (:little-endian (get-bytes-le bytes 0 ,bytes))
                       (:big-endian (get-bytes-be bytes 0 ,bytes)))))
          ,(if signed-p
               `(get-signed value ,bytes)
               'value))))
     (:writer
      (let ((bytes (make-array ,bytes :element-type '(unsigned-byte 8))))
        (ecase endian
          (:little-endian (set-bytes-le bytes 0 value ,bytes))
          (:big-endian (set-bytes-be bytes 0 value ,bytes)))
        (binary-write-sequence stream bytes)))
     (:encoder
      (let ((bytes (make-array ,bytes :element-type '(unsigned-byte 8))))
        (ecase endian
          (:little-endian (set-bytes-le bytes 0 value ,bytes))
          (:big-endian (set-bytes-be bytes 0 value ,bytes)))
        bytes))
     (:decoder
      (let ((value (ecase endian
                     (:little-endian (get-bytes-le bytes offset ,bytes))
                     (:big-endian (get-bytes-be bytes offset ,bytes)))))
        ,(if signed-p
             `(get-signed value ,bytes)
             'value)))))

;; standard integer types

(define-integer-type :u16 nil 2)
(define-integer-type :u32 nil 4)
(define-integer-type :u64 nil 8)
(define-integer-type :s16 t 2)
(define-integer-type :s32 t 4)
(define-integer-type :s64 t 8)

;; float types (platform-specific)

(define-binary-type :f32 4
  (:reader
   (let ((bits (read stream :u32 endian)))
     (sb-kernel:make-single-float bits)))
  (:writer
   (write stream (sb-kernel:single-float-bits value) :u32 endian))
  (:encoder
   (to-bytes (sb-kernel:single-float-bits value) :u32 endian))
  (:decoder
   (let ((bits (from-bytes bytes :u32 endian offset)))
     (sb-kernel:make-single-float bits))))

(define-binary-type :f64 8
  (:reader
   (let ((high (read stream :s32 endian))
         (low (read stream :u32 endian)))
     (if (eq endian :big-endian)
         (sb-kernel:make-double-float high low)
         (sb-kernel:make-double-float low high))))
  (:writer
   (let ((high (sb-kernel:double-float-high-bits value))
         (low (sb-kernel:double-float-low-bits value)))
     (if (eq endian :big-endian)
         (progn (write stream high :s32 endian)
                (write stream low :u32 endian))
         (progn (write stream low :u32 endian)
                (write stream high :s32 endian)))))
  (:encoder
   (let ((high (sb-kernel:double-float-high-bits value))
         (low (sb-kernel:double-float-low-bits value)))
     (concatenate '(vector (unsigned-byte 8))
                  (if (eq endian :big-endian)
                      (to-bytes high :s32 endian)
                      (to-bytes low :u32 endian))
                  (if (eq endian :big-endian)
                      (to-bytes low :u32 endian)
                      (to-bytes high :s32 endian)))))
  (:decoder
   (error "f64 decoder not implemented")))

;;; Type inference
;;; (moved earlier in file)

;;; Binary structure support

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct binary-struct-field
  name
  type
  size      ; nil, integer, or symbol referring to another field
  offset    ; computed offset in structure
  value     ; constant value for validation
  endian    ; field-specific endianness override
  condition ; predicate for conditional fields
  reader    ; custom reader function
  writer)   ; custom writer function

(defstruct binary-struct-type
  name
  fields
  size      ; total size if fixed, nil if variable
  options)
) ; end eval-when

(defvar *binary-struct-registry* (map:make-map)
  "Registry of defined binary structures")

(defun register-binary-struct (name struct-type)
  "Register a binary structure type"
  (setf *binary-struct-registry* 
        (map:assoc *binary-struct-registry* name struct-type)))

(defun get-binary-struct (name)
  "Get a registered binary structure type"
  (or (map:get *binary-struct-registry* name)
      (error "Unknown binary struct type: ~A" name)))

;;; Helper functions for define-binary-struct macro
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-struct-fields (fields)
  "Parse field definitions into binary-struct-field objects"
  (let ((offset 0)
        (result '()))
    (dolist (field fields (nreverse result))
      (let* ((parsed (parse-field-definition field))
             (field-obj (apply #'make-binary-struct-field parsed)))
        (setf (binary-struct-field-offset field-obj) offset)
        (when (integerp (binary-struct-field-size field-obj))
          (incf offset (binary-struct-field-size field-obj)))
        (push field-obj result)))))

(defun parse-field-definition (field)
  "Parse a single field definition"
  (destructuring-bind (name type &rest options) field
    (list* :name name :type type options)))

(defun field-to-plist (field)
  "Convert field object to plist for macro expansion"
  `(:name ',(binary-struct-field-name field)
    :type ',(binary-struct-field-type field)
    ,@(when (binary-struct-field-size field)
        `(:size ,(if (symbolp (binary-struct-field-size field))
                     `',(binary-struct-field-size field)
                     (binary-struct-field-size field))))
    ,@(when (binary-struct-field-value field)
        `(:value ,(binary-struct-field-value field)))
    ,@(when (binary-struct-field-endian field)
        `(:endian ,(binary-struct-field-endian field)))
    ,@(when (binary-struct-field-condition field)
        `(:condition ,(binary-struct-field-condition field)))))

(defun compute-struct-size (fields)
  "Compute total size of structure if fixed"
  (let ((total 0))
    (dolist (field fields)
      (let ((field-size (binary-struct-field-size field)))
        (cond
          ((null field-size)
           (return-from compute-struct-size nil)) ; Variable size
          ((integerp field-size)
           (incf total field-size))
          (t
           (return-from compute-struct-size nil))))) ; Variable size
    total))

(defun resolve-field-size (size struct)
  "Resolve field size which may reference another field"
  (cond
    ((integerp size) size)
    ((symbolp size) (slot-value struct size))
    (t (error "Invalid field size: ~A" size))))

(defun field-size (field value)
  "Calculate the size of a field value"
  (cond
    ((binary-struct-field-size field)
     (if (integerp (binary-struct-field-size field))
         (binary-struct-field-size field)
         (length value))) ; For variable-size fields
    ((member (binary-struct-field-type field) '(:bytes :string))
     (length value))
    (t
     (size (binary-struct-field-type field)))))

(defun write-struct-field (stream field struct endian)
  "Write a single field to stream"
  (let ((field-endian (or (binary-struct-field-endian field) endian))
        (value (slot-value struct (binary-struct-field-name field))))
    (cond
      ;; Custom writer
      ((binary-struct-field-writer field)
       (funcall (binary-struct-field-writer field) stream value struct field-endian))
      ;; Bytes type
      ((eq (binary-struct-field-type field) :bytes)
       (write-bytes stream value))
      ;; String type
      ((eq (binary-struct-field-type field) :string)
       (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code value)))
         (write-bytes stream bytes)))
      ;; Standard type
      (t
       (write stream value (binary-struct-field-type field) field-endian)))))

(defun read-struct-field (stream field struct endian)
  "Read a single field from stream"
  (let ((field-endian (or (binary-struct-field-endian field) endian)))
    (cond
      ;; Custom reader
      ((binary-struct-field-reader field)
       (funcall (binary-struct-field-reader field) stream struct field-endian))
      ;; Bytes type with size
      ((and (eq (binary-struct-field-type field) :bytes)
            (binary-struct-field-size field))
       (let ((size (resolve-field-size (binary-struct-field-size field) struct)))
         (read-bytes stream size)))
      ;; String type with size
      ((and (eq (binary-struct-field-type field) :string)
            (binary-struct-field-size field))
       (let* ((size (resolve-field-size (binary-struct-field-size field) struct))
              (bytes (read-bytes stream size)))
         (map 'string #'code-char bytes)))
      ;; Standard type
      (t
       (read stream (binary-struct-field-type field) field-endian)))))
) ; end eval-when

(defmacro define-binary-struct (name options &body fields)
  "Define a binary structure type
   
   Options:
     :endian - default endianness for all fields
     :size   - fixed size validation
   
   Field specifications:
     (name type &key size value endian condition reader writer)
   
   Examples:
     (define-binary-struct point ()
       (x :s32)
       (y :s32))
     
     (define-binary-struct pascal-string ()
       (length :u8)
       (data :bytes :size length))"
  (let ((struct-var (gensym "STRUCT"))
        (field-defs (parse-struct-fields fields))
        (struct-name (if (listp name) (first name) name))
        (parent (when (listp name) (second name)))
        (struct-symbol (intern (format nil "~A-STRUCT" (if (listp name) (first name) name)))))
    `(progn
       ;; Define the structure type
       (defstruct ,struct-symbol
         ,@(mapcar #'binary-struct-field-name field-defs))
       
       ;; Register the binary structure
       (let ((,struct-var (make-binary-struct-type
                           :name ',struct-name
                           :fields (list ,@(mapcar (lambda (field)
                                                     `(make-binary-struct-field
                                                       ,@(field-to-plist field)))
                                                   field-defs))
                           :size ,(compute-struct-size field-defs)
                           :options ',options)))
         (register-binary-struct ',struct-name ,struct-var)
         
         ;; Register as a binary type
         (register-binary-type ',struct-name
                              (or ,(compute-struct-size field-defs) 
                                  (lambda (value) (struct-size value ',struct-name)))
                              (make-struct-reader ',struct-name)
                              (make-struct-writer ',struct-name)
                              (make-struct-encoder ',struct-name)
                              (make-struct-decoder ',struct-name)))
       
       ',struct-name)))

;;; Helper functions (moved earlier in file)

(defun make-struct-reader (struct-name)
  "Create a reader function for a binary structure"
  (lambda (stream endian)
    (let* ((struct-type (get-binary-struct struct-name))
           (constructor-name (intern (format nil "MAKE-~A-STRUCT" struct-name) 
                                     (symbol-package struct-name)))
           (result (funcall constructor-name)))
      (dolist (field (binary-struct-type-fields struct-type))
        (let ((value (read-struct-field stream field result endian)))
          (when (binary-struct-field-value field)
            (unless (equalp value (binary-struct-field-value field))
              (error "Field ~A: expected ~A, got ~A" 
                     (binary-struct-field-name field)
                     (binary-struct-field-value field)
                     value)))
          (setf (slot-value result (binary-struct-field-name field)) value)))
      result)))

;;; read-struct-field (moved earlier in file)

;;; resolve-field-size (moved earlier in file)

(defun make-struct-writer (struct-name)
  "Create a writer function for a binary structure"
  (lambda (stream value endian)
    (let ((struct-type (get-binary-struct struct-name)))
      (dolist (field (binary-struct-type-fields struct-type))
        (write-struct-field stream field value endian)))))

;;; write-struct-field (moved earlier in file)

(defun make-struct-encoder (struct-name)
  "Create an encoder function for a binary structure"
  (lambda (value endian)
    (let ((stream (make-instance 'binary-output-stream)))
      (write stream value struct-name endian)
      (get-output-stream-bytes stream))))

(defun make-struct-decoder (struct-name)
  "Create a decoder function for a binary structure"
  (lambda (bytes offset endian)
    (let ((stream (make-instance 'binary-input-stream :data bytes :position offset)))
      (read stream struct-name endian))))

(defun struct-size (value struct-name)
  "Calculate the size of a structure instance"
  (let ((struct-type (get-binary-struct struct-name))
        (total 0))
    (dolist (field (binary-struct-type-fields struct-type))
      (let ((field-value (slot-value value (binary-struct-field-name field))))
        (incf total (field-size field field-value))))
    total))

;;; field-size (moved earlier in file)

;;; Binary stream classes for in-memory operations
;;; (moved earlier in file)

;;; Stream generics and methods (moved earlier in file)

;; Wrapper functions that use the appropriate stream type
;;; (moved earlier in file)
