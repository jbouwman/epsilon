;;;; struct-support.lisp - Struct-by-value support for JIT FFI
;;;;
;;;; Implements struct passing conventions for ARM64 and x86-64.
;;;; Since SBCL's alien-funcall doesn't fully support struct-by-value,
;;;; we handle it by packing small structs into integer registers.
;;;;
;;;; Strategy:
;;;; - Structs <= 8 bytes: Pack into single 64-bit word
;;;; - Structs 9-16 bytes: Pack into two 64-bit words
;;;; - Structs > 16 bytes: Pass by pointer (copy to temp, pass address)
;;;;
;;;; This matches the ARM64 AAPCS64 and x86-64 System V ABI for non-HFA structs.

(defpackage epsilon.foreign.jit.struct
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit))
  (:export
   ;; Struct definition
   #:define-jit-struct
   #:define-ffi-struct
   #:get-jit-struct
   #:jit-struct-size
   #:jit-struct-alignment
   #:jit-struct-fields

   ;; Struct operations (for serialization/manual packing)
   #:struct-to-words
   #:words-to-struct

   ;; Field accessors (pointer-based access)
   #:struct-field-ref

   ;; sb-alien type conversion
   #:jit-type-to-alien-field-type)
  (:enter t))

;;; ============================================================================
;;; Struct Definition
;;; ============================================================================

(defstruct jit-struct-def
  "Definition of a struct for JIT FFI"
  name
  size         ; Total size in bytes
  alignment    ; Required alignment
  fields       ; List of (name offset size type)
  pass-mode)   ; :registers-1, :registers-2, or :pointer

(defvar *jit-structs* (make-hash-table :test 'eq)
  "Registry of JIT struct definitions")

(defparameter *primitive-sizes*
  '((:char . 1) (:uchar . 1) (:unsigned-char . 1)
    (:short . 2) (:ushort . 2) (:unsigned-short . 2)
    (:int . 4) (:uint . 4) (:unsigned-int . 4)
    (:long . 8) (:ulong . 8) (:unsigned-long . 8)
    (:float . 4) (:double . 8)
    (:pointer . 8))
  "Size in bytes for primitive types")

(defparameter *primitive-alignments*
  '((:char . 1) (:uchar . 1) (:unsigned-char . 1)
    (:short . 2) (:ushort . 2) (:unsigned-short . 2)
    (:int . 4) (:uint . 4) (:unsigned-int . 4)
    (:long . 8) (:ulong . 8) (:unsigned-long . 8)
    (:float . 4) (:double . 8)
    (:pointer . 8))
  "Alignment in bytes for primitive types")

(defun type-size (type)
  "Get size of a type in bytes"
  (or (cdr (assoc type *primitive-sizes*))
      (let ((struct (get-jit-struct type)))
        (when struct (jit-struct-def-size struct)))
      (error "Unknown type: ~A" type)))

(defun type-alignment (type)
  "Get alignment of a type in bytes"
  (or (cdr (assoc type *primitive-alignments*))
      (let ((struct (get-jit-struct type)))
        (when struct (jit-struct-def-alignment struct)))
      (error "Unknown type: ~A" type)))

(defun align-offset (offset alignment)
  "Align offset to required alignment"
  (let ((rem (mod offset alignment)))
    (if (zerop rem)
        offset
        (+ offset (- alignment rem)))))

(defun calculate-layout (fields)
  "Calculate struct layout from field specs.
   FIELDS is a list of (name type) pairs.
   Returns (size alignment field-list) where field-list is (name offset size type)."
  (let ((offset 0)
        (max-align 1)
        (result-fields '()))
    (dolist (field fields)
      (destructuring-bind (name type) field
        (let ((size (type-size type))
              (align (type-alignment type)))
          (setf max-align (max max-align align))
          (setf offset (align-offset offset align))
          (push (list name offset size type) result-fields)
          (incf offset size))))
    ;; Align final size to struct alignment
    (setf offset (align-offset offset max-align))
    (values offset max-align (nreverse result-fields))))

(defun determine-pass-mode (size)
  "Determine how struct should be passed based on size"
  (cond
    ((<= size 8) :registers-1)
    ((<= size 16) :registers-2)
    (t :pointer)))

(defmacro define-jit-struct (name &rest fields)
  "Define a struct for use with JIT FFI.

   Example:
   (define-jit-struct point
     (x :double)
     (y :double))

   (define-jit-struct small-point
     (x :float)
     (y :float))"
  (multiple-value-bind (size alignment field-list)
      (calculate-layout fields)
    `(setf (gethash ',name *jit-structs*)
           (make-jit-struct-def
            :name ',name
            :size ,size
            :alignment ,alignment
            :fields ',field-list
            :pass-mode ,(determine-pass-mode size)))))

(defun get-jit-struct (name)
  "Get a JIT struct definition by name"
  (gethash name *jit-structs*))

(defun jit-struct-size (name)
  "Get size of a JIT struct"
  (let ((struct (get-jit-struct name)))
    (when struct (jit-struct-def-size struct))))

(defun jit-struct-alignment (name)
  "Get alignment of a JIT struct"
  (let ((struct (get-jit-struct name)))
    (when struct (jit-struct-def-alignment struct))))

(defun jit-struct-fields (name)
  "Get fields of a JIT struct"
  (let ((struct (get-jit-struct name)))
    (when struct (jit-struct-def-fields struct))))

;;; ============================================================================
;;; Struct Packing/Unpacking
;;; ============================================================================

(defun pack-bytes-to-word (bytes)
  "Pack up to 8 bytes into a 64-bit word (little-endian)"
  (let ((word 0))
    (loop for byte across bytes
          for i from 0
          do (setf word (logior word (ash byte (* i 8)))))
    word))

(defun unpack-word-to-bytes (word size)
  "Unpack a 64-bit word into SIZE bytes (little-endian)"
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (loop for i from 0 below size
          do (setf (aref bytes i) (logand (ash word (* i -8)) #xFF)))
    bytes))

(defun struct-to-words (struct-def values)
  "Convert struct field values to words for register passing.
   VALUES is a plist of field names to values.
   Returns a list of 64-bit words."
  (let* ((size (jit-struct-def-size struct-def))
         (fields (jit-struct-def-fields struct-def))
         (buffer (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Write each field value to buffer
    (dolist (field fields)
      (destructuring-bind (name offset field-size type) field
        (let ((value (getf values name 0)))
          (write-value-to-buffer buffer offset field-size type value))))
    ;; Convert buffer to words
    (let ((words '()))
      (loop for i from 0 below size by 8
            do (let ((end (min (+ i 8) size)))
                 (push (pack-bytes-to-word (subseq buffer i end)) words)))
      (nreverse words))))

(defun words-to-struct (struct-def words)
  "Convert words from registers back to struct field values.
   Returns a plist of field names to values."
  (let* ((size (jit-struct-def-size struct-def))
         (fields (jit-struct-def-fields struct-def))
         (buffer (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Unpack words into buffer
    (loop for word in words
          for i from 0 by 8
          do (let* ((end (min (+ i 8) size))
                    (bytes (unpack-word-to-bytes word (- end i))))
               (replace buffer bytes :start1 i)))
    ;; Read field values from buffer
    (let ((result '()))
      (dolist (field fields)
        (destructuring-bind (name offset field-size type) field
          (let ((value (read-value-from-buffer buffer offset field-size type)))
            ;; Build plist in reverse, then reverse at the end
            (push value result)
            (push name result))))
      result)))

(defun write-value-to-buffer (buffer offset size type value)
  "Write a typed value to buffer at offset"
  (declare (ignore size))
  (case type
    ((:char :uchar :unsigned-char)
     (setf (aref buffer offset) (logand value #xFF)))
    ((:short :ushort :unsigned-short)
     (setf (aref buffer offset) (logand value #xFF))
     (setf (aref buffer (+ offset 1)) (logand (ash value -8) #xFF)))
    ((:int :uint :unsigned-int)
     (loop for i from 0 below 4
           do (setf (aref buffer (+ offset i)) (logand (ash value (* i -8)) #xFF))))
    ((:long :ulong :unsigned-long :pointer)
     (loop for i from 0 below 8
           do (setf (aref buffer (+ offset i)) (logand (ash value (* i -8)) #xFF))))
    (:float
     (let ((bits (sb-kernel:single-float-bits (float value 1.0))))
       (loop for i from 0 below 4
             do (setf (aref buffer (+ offset i)) (logand (ash bits (* i -8)) #xFF)))))
    (:double
     (let ((bits (sb-kernel:double-float-bits (float value 1.0d0))))
       (loop for i from 0 below 8
             do (setf (aref buffer (+ offset i)) (logand (ash bits (* i -8)) #xFF)))))))

(defun read-value-from-buffer (buffer offset size type)
  "Read a typed value from buffer at offset"
  (declare (ignore size))
  (case type
    ((:char)
     (let ((v (aref buffer offset)))
       (if (> v 127) (- v 256) v)))
    ((:uchar :unsigned-char)
     (aref buffer offset))
    ((:short)
     (let ((v (logior (aref buffer offset)
                      (ash (aref buffer (+ offset 1)) 8))))
       (if (> v 32767) (- v 65536) v)))
    ((:ushort :unsigned-short)
     (logior (aref buffer offset)
             (ash (aref buffer (+ offset 1)) 8)))
    ((:int)
     (let ((v (logior (aref buffer offset)
                      (ash (aref buffer (+ offset 1)) 8)
                      (ash (aref buffer (+ offset 2)) 16)
                      (ash (aref buffer (+ offset 3)) 24))))
       (if (logbitp 31 v) (- v (ash 1 32)) v)))
    ((:uint :unsigned-int)
     (logior (aref buffer offset)
             (ash (aref buffer (+ offset 1)) 8)
             (ash (aref buffer (+ offset 2)) 16)
             (ash (aref buffer (+ offset 3)) 24)))
    ((:long :pointer)
     (let ((v (logior (aref buffer offset)
                      (ash (aref buffer (+ offset 1)) 8)
                      (ash (aref buffer (+ offset 2)) 16)
                      (ash (aref buffer (+ offset 3)) 24)
                      (ash (aref buffer (+ offset 4)) 32)
                      (ash (aref buffer (+ offset 5)) 40)
                      (ash (aref buffer (+ offset 6)) 48)
                      (ash (aref buffer (+ offset 7)) 56))))
       (if (logbitp 63 v) (- v (ash 1 64)) v)))
    ((:ulong :unsigned-long)
     (logior (aref buffer offset)
             (ash (aref buffer (+ offset 1)) 8)
             (ash (aref buffer (+ offset 2)) 16)
             (ash (aref buffer (+ offset 3)) 24)
             (ash (aref buffer (+ offset 4)) 32)
             (ash (aref buffer (+ offset 5)) 40)
             (ash (aref buffer (+ offset 6)) 48)
             (ash (aref buffer (+ offset 7)) 56)))
    (:float
     (let ((bits (logior (aref buffer offset)
                         (ash (aref buffer (+ offset 1)) 8)
                         (ash (aref buffer (+ offset 2)) 16)
                         (ash (aref buffer (+ offset 3)) 24))))
       (sb-kernel:make-single-float bits)))
    (:double
     (let ((bits (logior (aref buffer offset)
                         (ash (aref buffer (+ offset 1)) 8)
                         (ash (aref buffer (+ offset 2)) 16)
                         (ash (aref buffer (+ offset 3)) 24)
                         (ash (aref buffer (+ offset 4)) 32)
                         (ash (aref buffer (+ offset 5)) 40)
                         (ash (aref buffer (+ offset 6)) 48)
                         (ash (aref buffer (+ offset 7)) 56))))
       (sb-kernel:make-double-float (ash bits -32) (logand bits #xFFFFFFFF))))))




;;; ============================================================================
;;; Field Accessors
;;; ============================================================================

(defun struct-field-ref (ptr struct-name field-name)
  "Read a field from a struct pointer.

   PTR - Integer pointer to struct data
   STRUCT-NAME - Name of the struct type
   FIELD-NAME - Name of the field to read

   Example:
   (struct-field-ref ptr 'point 'x)"
  (let ((struct (get-jit-struct struct-name)))
    (unless struct
      (error "Unknown struct: ~A" struct-name))
    (let ((field (find field-name (jit-struct-def-fields struct) :key #'first)))
      (unless field
        (error "Unknown field ~A in struct ~A" field-name struct-name))
      (destructuring-bind (name offset size type) field
        (declare (ignore name size))
        (let ((sap (sb-sys:int-sap ptr)))
          (case type
            ((:char) (sb-sys:signed-sap-ref-8 sap offset))
            ((:uchar :unsigned-char) (sb-sys:sap-ref-8 sap offset))
            ((:short) (sb-sys:signed-sap-ref-16 sap offset))
            ((:ushort :unsigned-short) (sb-sys:sap-ref-16 sap offset))
            ((:int) (sb-sys:signed-sap-ref-32 sap offset))
            ((:uint :unsigned-int) (sb-sys:sap-ref-32 sap offset))
            ((:long) (sb-sys:signed-sap-ref-64 sap offset))
            ((:ulong :unsigned-long :pointer) (sb-sys:sap-ref-64 sap offset))
            ((:float) (sb-sys:sap-ref-single sap offset))
            ((:double) (sb-sys:sap-ref-double sap offset))))))))

(defun (setf struct-field-ref) (value ptr struct-name field-name)
  "Write a field to a struct pointer.

   VALUE - Value to write
   PTR - Integer pointer to struct data
   STRUCT-NAME - Name of the struct type
   FIELD-NAME - Name of the field to write

   Example:
   (setf (struct-field-ref ptr 'point 'x) 1.0d0)"
  (let ((struct (get-jit-struct struct-name)))
    (unless struct
      (error "Unknown struct: ~A" struct-name))
    (let ((field (find field-name (jit-struct-def-fields struct) :key #'first)))
      (unless field
        (error "Unknown field ~A in struct ~A" field-name struct-name))
      (destructuring-bind (name offset size type) field
        (declare (ignore name size))
        (let ((sap (sb-sys:int-sap ptr)))
          (case type
            ((:char) (setf (sb-sys:signed-sap-ref-8 sap offset) value))
            ((:uchar :unsigned-char) (setf (sb-sys:sap-ref-8 sap offset) value))
            ((:short) (setf (sb-sys:signed-sap-ref-16 sap offset) value))
            ((:ushort :unsigned-short) (setf (sb-sys:sap-ref-16 sap offset) value))
            ((:int) (setf (sb-sys:signed-sap-ref-32 sap offset) value))
            ((:uint :unsigned-int) (setf (sb-sys:sap-ref-32 sap offset) value))
            ((:long) (setf (sb-sys:signed-sap-ref-64 sap offset) value))
            ((:ulong :unsigned-long :pointer) (setf (sb-sys:sap-ref-64 sap offset) value))
            ((:float) (setf (sb-sys:sap-ref-single sap offset) (float value 1.0)))
            ((:double) (setf (sb-sys:sap-ref-double sap offset) (float value 1.0d0))))
          value)))))

;;; ============================================================================
;;; FFI Struct Definition (sb-alien Integration)
;;; ============================================================================
;;;
;;; define-ffi-struct creates both jit-struct metadata for layout queries
;;; AND sb-alien type definitions for correct struct-by-value calling convention.
;;;
;;; This is the recommended way to define structs for FFI when you need
;;; struct-by-value returns. Use sb-alien:define-alien-routine with
;;; (sb-alien:struct name) as the return type.

(defun jit-type-to-alien-field-type (type)
  "Convert a jit-struct field type to sb-alien struct field type.

   :char -> (sb-alien:signed 8)
   :unsigned-char -> (sb-alien:unsigned 8)
   :short -> (sb-alien:signed 16)
   :unsigned-short -> (sb-alien:unsigned 16)
   :int -> (sb-alien:signed 32)
   :unsigned-int -> (sb-alien:unsigned 32)
   :long -> (sb-alien:signed 64)
   :unsigned-long -> (sb-alien:unsigned 64)
   :float -> sb-alien:single-float
   :double -> sb-alien:double-float
   :pointer -> sb-alien:system-area-pointer"
  (case type
    ((:char) '(sb-alien:signed 8))
    ((:uchar :unsigned-char) '(sb-alien:unsigned 8))
    ((:short) '(sb-alien:signed 16))
    ((:ushort :unsigned-short) '(sb-alien:unsigned 16))
    ((:int) '(sb-alien:signed 32))
    ((:uint :unsigned-int) '(sb-alien:unsigned 32))
    ((:long) '(sb-alien:signed 64))
    ((:ulong :unsigned-long) '(sb-alien:unsigned 64))
    ((:float) 'sb-alien:single-float)
    ((:double) 'sb-alien:double-float)
    ((:pointer) 'sb-alien:system-area-pointer)
    (t (error "Unknown type for sb-alien conversion: ~A" type))))

(defun convert-field-to-alien (field)
  "Convert a jit-struct field spec (name type) to sb-alien field spec."
  (destructuring-bind (name type) field
    (list name (jit-type-to-alien-field-type type))))

(defmacro define-ffi-struct (name &body fields)
  "Define a struct for FFI use with both jit-struct metadata and sb-alien type.

   This creates:
   - jit-struct definition for layout queries and field access
   - sb-alien struct type for use in define-alien-routine

   Example:
   (define-ffi-struct xxh128-hash-t
     (low64 :unsigned-long)
     (high64 :unsigned-long))

   Then use with sb-alien:define-alien-routine:
   (sb-alien:define-alien-routine (\"XXH3_128bits\" %xxh3-128bits)
       (sb-alien:struct xxh128-hash-t)
     (input sb-alien:system-area-pointer)
     (length sb-alien:unsigned-long))"
  (let ((alien-fields (mapcar #'convert-field-to-alien fields)))
    `(progn
       ;; jit-struct for layout queries and field access
       (define-jit-struct ,name ,@fields)
       ;; sb-alien struct type for calling convention
       (sb-alien:define-alien-type nil
         (sb-alien:struct ,name ,@alien-fields))
       ',name)))
