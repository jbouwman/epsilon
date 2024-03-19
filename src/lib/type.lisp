(defpackage #:lib.type
  (:use
   #:cl
   #:lib.symbol)
  (:export

   #:array-index
   #:array-index+1
   
   ;; unsigned 8
   
   #:u8
   #:u8-get
   #:u8-set
   #:->u8
   #:random-u8

   ;; unsigned 16

   #:u16
   #:->u16
   #:write-u16-msb
   #:read-u16/be
   #:read-u16/be-into-sequence
   #:read-u16/be-sequence
   #:read-u16/le
   #:read-u16/le-into-sequence
   #:read-u16/le-sequence
   #:u16ref/be
   #:u16ref/le
   #:write-u16/be
   #:write-u16/be-sequence
   #:write-u16/le
   #:write-u16/le-sequence

   ;; signed 16
   
   #:read-s16/be
   #:read-s16/be-into-sequence
   #:read-s16/be-sequence
   #:read-s16/le
   #:read-s16/le-into-sequence
   #:read-s16/le-sequence
   #:write-s16/be
   #:write-s16/be-sequence
   #:write-s16/le
   #:write-s16/le-sequence
   #:s16ref/be
   #:s16ref/le

   ;; unsigned 32
   
   #:u32
   #:->u32
   #:write-u8
   #:write-u32
   #:write-u32-msb
   #:read-u32/be
   #:read-u32/be-into-sequence
   #:read-u32/be-sequence
   #:read-u32/le
   #:read-u32/le-into-sequence
   #:read-u32/le-sequence
   #:u32ref/be
   #:u32ref/le
   #:write-u32/be
   #:write-u32/be-sequence
   #:write-u32/le
   #:write-u32/le-sequence

   ;; signed 32

   #:read-s32/be
   #:read-s32/be-into-sequence
   #:read-s32/be-sequence
   #:read-s32/le
   #:read-s32/le-into-sequence
   #:read-s32/le-sequence
   #:s32ref/be
   #:s32ref/le
   #:write-s32/be
   #:write-s32/be-sequence
   #:write-s32/le
   #:write-s32/le-sequence

   ;; unsigned 64

   #:u64
   #:->u64
   #:read-u64/be
   #:read-u64/be-into-sequence
   #:read-u64/be-sequence
   #:read-u64/le
   #:read-u64/le-into-sequence
   #:read-u64/le-sequence
   #:write-u64/be
   #:write-u64/be-sequence
   #:write-u64/le
   #:write-u64/le-sequence
   #:u64ref/be
   #:u64ref/le

   ;; signed 64

   #:read-s64/be
   #:read-s64/be-into-sequence
   #:read-s64/be-sequence
   #:read-s64/le
   #:read-s64/le-into-sequence
   #:read-s64/le-sequence
   #:write-s64/be
   #:write-s64/be-sequence
   #:write-s64/le
   #:write-s64/le-sequence
   #:s64ref/be
   #:s64ref/le))

(in-package #:lib.type)

(deftype array-index (&optional (length array-dimension-limit))
  `(mod ,length))

(deftype array-index+1 (&optional (length array-dimension-limit))
  `(mod ,(1+ length)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-unsigned-type (bits)
    (let ((base (symbolicate '#:u (princ-to-string bits))))
      `(progn
         (deftype ,base ()
           '(unsigned-byte ,bits))
         (deftype ,(symbolicate '#:-> base) (&optional length)
           (let ((length (or length '*)))
             `(simple-array ,',base (,length))))
         (defun ,(symbolicate '#:-> base) (arg)
           (etypecase arg
             (array-index (make-array arg :element-type ',base))
             (sequence (make-array (length arg)
                                   :element-type ',base
                                   :initial-contents arg))))))))

;; u8

(define-unsigned-type 8)

(defgeneric write-u8 (stream u8))

(defun random-u8 (size)
  (->u8 (loop :for i :from 1 :to size
              :collect (random 256))))

;; u16

(define-unsigned-type 16)

(defun write-u16-msb (stream u16)
  (write-u8 stream (ldb (byte 8 8) u16))
  (write-u8 stream (ldb (byte 8 0) u16)))

;; u32

(define-unsigned-type 32)

(defun write-u32 (stream u32)
  (write-u8 stream (ldb (byte 8 0) u32))
  (write-u8 stream (ldb (byte 8 8) u32))
  (write-u8 stream (ldb (byte 8 16) u32))
  (write-u8 stream (ldb (byte 8 24) u32)))

(defun write-u32-msb (stream u32)
  (write-u8 stream (ldb (byte 8 24) u32))
  (write-u8 stream (ldb (byte 8 16) u32))
  (write-u8 stream (ldb (byte 8 8) u32))
  (write-u8 stream (ldb (byte 8 0) u32)))

;; u64

(define-unsigned-type 64)
