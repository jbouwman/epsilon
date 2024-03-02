(defpackage #:lib.type
  (:use
   #:cl
   #:lib.symbol)
  (:export
   #:array-index
   #:array-index+1
   #:u8
   #:u8-get
   #:u8-set
   #:->u8
   #:random-u8
   #:fixed-u8
   #:u16
   #:->u16
   #:write-u16-msb
   #:u32
   #:->u32
   #:write-u8
   #:write-u32
   #:write-u32-msb
   #:u64
   #:->u64))

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

(defun fixed-u8 (size)
  (->u8 (loop :for i :from 1 :to size
              :collect (mod i 256))))

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
