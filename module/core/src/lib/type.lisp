;;;; Lean version of type.lisp - Essential type definitions only
;;;;
;;;; This module provides core type definitions and basic byte array operations.
;;;; More complex binary I/O operations have been moved to separate modules.

(defpackage #:epsilon.lib.type
  (:use #:cl)
  (:export
   ;; Array types
   #:array-index
   #:array-index+1
   
   ;; Basic unsigned types
   #:u8
   #:u16
   #:u32
   #:u64
   
   ;; Type conversion
   #:->u8
   #:->u16
   #:->u32
   #:->u64
   
   ;; Essential utilities
   #:random-u8
   
   ;; Essential binary I/O (only what's actually used)
   #:u16ref/le
   #:u32ref/le
   #:u32ref/be
   #:u64ref/le
   #:u64ref/be
   #:write-u8
   #:write-u16/le
   #:write-u32/le
   #:write-u64/le
   #:write-u16-msb
   #:write-u32-msb))

(in-package #:epsilon.lib.type)

;;; Array index types

(deftype array-index (&optional (length array-dimension-limit))
  "Type for valid array indices"
  `(mod ,length))

(deftype array-index+1 (&optional (length array-dimension-limit))
  "Type for array indices including the end position"
  `(mod ,(1+ length)))

;;; Basic unsigned integer types

(deftype u8 ()
  "Unsigned 8-bit integer"
  '(unsigned-byte 8))

(deftype u16 ()
  "Unsigned 16-bit integer"
  '(unsigned-byte 16))

(deftype u32 ()
  "Unsigned 32-bit integer"
  '(unsigned-byte 32))

(deftype u64 ()
  "Unsigned 64-bit integer"
  '(unsigned-byte 64))

;;; Array type constructors

(deftype ->u8 (&optional length)
  "Type for simple arrays of unsigned 8-bit integers"
  (let ((length (or length '*)))
    `(simple-array u8 (,length))))

(deftype ->u16 (&optional length)
  "Type for simple arrays of unsigned 16-bit integers"
  (let ((length (or length '*)))
    `(simple-array u16 (,length))))

(deftype ->u32 (&optional length)
  "Type for simple arrays of unsigned 32-bit integers"
  (let ((length (or length '*)))
    `(simple-array u32 (,length))))

(deftype ->u64 (&optional length)
  "Type for simple arrays of unsigned 64-bit integers"
  (let ((length (or length '*)))
    `(simple-array u64 (,length))))

;;; Array creation functions

(defun ->u8 (arg)
  "Create a simple array of unsigned 8-bit integers.
   ARG can be:
   - An integer specifying the array size
   - A sequence to convert to a u8 array"
  (etypecase arg
    (array-index 
     (make-array arg :element-type 'u8))
    (sequence 
     (make-array (length arg)
                 :element-type 'u8
                 :initial-contents arg))))

(defun ->u16 (arg)
  "Create a simple array of unsigned 16-bit integers"
  (etypecase arg
    (array-index 
     (make-array arg :element-type 'u16))
    (sequence 
     (make-array (length arg)
                 :element-type 'u16
                 :initial-contents arg))))

(defun ->u32 (arg)
  "Create a simple array of unsigned 32-bit integers"
  (etypecase arg
    (array-index 
     (make-array arg :element-type 'u32))
    (sequence 
     (make-array (length arg)
                 :element-type 'u32
                 :initial-contents arg))))

(defun ->u64 (arg)
  "Create a simple array of unsigned 64-bit integers"
  (etypecase arg
    (array-index 
     (make-array arg :element-type 'u64))
    (sequence 
     (make-array (length arg)
                 :element-type 'u64
                 :initial-contents arg))))

;;; Utility functions

(defun random-u8 (size)
  "Create an array of SIZE random unsigned 8-bit integers"
  (->u8 (loop :for i :from 1 :to size
              :collect (random 256))))

;;; Essential binary reference operations
;;; Only the most commonly used operations are included here.
;;; For complete binary I/O support, see epsilon.lib.binary-io module.

(declaim (inline u16ref/le u32ref/le u32ref/be))

(defun u16ref/le (vector index)
  "Read a 16-bit unsigned integer from VECTOR at INDEX in little-endian order"
  (declare (type ->u8 vector)
           (type array-index index))
  (logior (aref vector index)
          (ash (aref vector (1+ index)) 8)))

(defun u32ref/le (vector index)
  "Read a 32-bit unsigned integer from VECTOR at INDEX in little-endian order"
  (declare (type ->u8 vector)
           (type array-index index))
  (logior (aref vector index)
          (ash (aref vector (+ index 1)) 8)
          (ash (aref vector (+ index 2)) 16)
          (ash (aref vector (+ index 3)) 24)))

(defun u32ref/be (vector index)
  "Read a 32-bit unsigned integer from VECTOR at INDEX in big-endian order"
  (declare (type ->u8 vector)
           (type array-index index))
  (logior (ash (aref vector index) 24)
          (ash (aref vector (+ index 1)) 16)
          (ash (aref vector (+ index 2)) 8)
          (aref vector (+ index 3))))

(defun (setf u16ref/le) (value vector index)
  "Write a 16-bit unsigned integer to VECTOR at INDEX in little-endian order"
  (declare (type u16 value)
           (type ->u8 vector)
           (type array-index index))
  (setf (aref vector index) (ldb (byte 8 0) value)
        (aref vector (1+ index)) (ldb (byte 8 8) value))
  value)

(defun (setf u32ref/le) (value vector index)
  "Write a 32-bit unsigned integer to VECTOR at INDEX in little-endian order"
  (declare (type u32 value)
           (type ->u8 vector)
           (type array-index index))
  (setf (aref vector index) (ldb (byte 8 0) value)
        (aref vector (+ index 1)) (ldb (byte 8 8) value)
        (aref vector (+ index 2)) (ldb (byte 8 16) value)
        (aref vector (+ index 3)) (ldb (byte 8 24) value))
  value)

(defun (setf u32ref/be) (value vector index)
  "Write a 32-bit unsigned integer to VECTOR at INDEX in big-endian order"
  (declare (type u32 value)
           (type ->u8 vector)
           (type array-index index))
  (setf (aref vector index) (ldb (byte 8 24) value)
        (aref vector (+ index 1)) (ldb (byte 8 16) value)
        (aref vector (+ index 2)) (ldb (byte 8 8) value)
        (aref vector (+ index 3)) (ldb (byte 8 0) value))
  value)

(defun u64ref/le (vector index)
  "Read a 64-bit unsigned integer from VECTOR at INDEX in little-endian order"
  (declare (type ->u8 vector)
           (type array-index index))
  (logior (aref vector index)
          (ash (aref vector (+ index 1)) 8)
          (ash (aref vector (+ index 2)) 16)
          (ash (aref vector (+ index 3)) 24)
          (ash (aref vector (+ index 4)) 32)
          (ash (aref vector (+ index 5)) 40)
          (ash (aref vector (+ index 6)) 48)
          (ash (aref vector (+ index 7)) 56)))

(defun u64ref/be (vector index)
  "Read a 64-bit unsigned integer from VECTOR at INDEX in big-endian order"
  (declare (type ->u8 vector)
           (type array-index index))
  (logior (ash (aref vector index) 56)
          (ash (aref vector (+ index 1)) 48)
          (ash (aref vector (+ index 2)) 40)
          (ash (aref vector (+ index 3)) 32)
          (ash (aref vector (+ index 4)) 24)
          (ash (aref vector (+ index 5)) 16)
          (ash (aref vector (+ index 6)) 8)
          (aref vector (+ index 7))))

(defun (setf u64ref/le) (value vector index)
  "Write a 64-bit unsigned integer to VECTOR at INDEX in little-endian order"
  (declare (type u64 value)
           (type ->u8 vector)
           (type array-index index))
  (setf (aref vector index) (ldb (byte 8 0) value)
        (aref vector (+ index 1)) (ldb (byte 8 8) value)
        (aref vector (+ index 2)) (ldb (byte 8 16) value)
        (aref vector (+ index 3)) (ldb (byte 8 24) value)
        (aref vector (+ index 4)) (ldb (byte 8 32) value)
        (aref vector (+ index 5)) (ldb (byte 8 40) value)
        (aref vector (+ index 6)) (ldb (byte 8 48) value)
        (aref vector (+ index 7)) (ldb (byte 8 56) value))
  value)

(defun (setf u64ref/be) (value vector index)
  "Write a 64-bit unsigned integer to VECTOR at INDEX in big-endian order"
  (declare (type u64 value)
           (type ->u8 vector)
           (type array-index index))
  (setf (aref vector index) (ldb (byte 8 56) value)
        (aref vector (+ index 1)) (ldb (byte 8 48) value)
        (aref vector (+ index 2)) (ldb (byte 8 40) value)
        (aref vector (+ index 3)) (ldb (byte 8 32) value)
        (aref vector (+ index 4)) (ldb (byte 8 24) value)
        (aref vector (+ index 5)) (ldb (byte 8 16) value)
        (aref vector (+ index 6)) (ldb (byte 8 8) value)
        (aref vector (+ index 7)) (ldb (byte 8 0) value))
  value)

;;; Stream I/O operations for struct.lisp

(defgeneric write-u8 (stream u8)
  (:method ((stream stream) (u8 integer))
    (write-byte u8 stream)))

(defun write-u16/le (u16 stream)
  "Write a 16-bit unsigned integer to STREAM in little-endian order"
  (write-byte (ldb (byte 8 0) u16) stream)
  (write-byte (ldb (byte 8 8) u16) stream))

(defun write-u32/le (u32 stream)
  "Write a 32-bit unsigned integer to STREAM in little-endian order"
  (write-byte (ldb (byte 8 0) u32) stream)
  (write-byte (ldb (byte 8 8) u32) stream)
  (write-byte (ldb (byte 8 16) u32) stream)
  (write-byte (ldb (byte 8 24) u32) stream))

(defun write-u64/le (u64 stream)
  "Write a 64-bit unsigned integer to STREAM in little-endian order"
  (dotimes (i 8)
    (write-byte (ldb (byte 8 (* i 8)) u64) stream)))

(defun write-u16-msb (stream u16)
  "Write a 16-bit unsigned integer to STREAM in big-endian order (MSB first)"
  (write-byte (ldb (byte 8 8) u16) stream)
  (write-byte (ldb (byte 8 0) u16) stream))

(defun write-u32-msb (stream u32)
  "Write a 32-bit unsigned integer to STREAM in big-endian order (MSB first)"
  (write-byte (ldb (byte 8 24) u32) stream)
  (write-byte (ldb (byte 8 16) u32) stream)
  (write-byte (ldb (byte 8 8) u32) stream)
  (write-byte (ldb (byte 8 0) u32) stream))