(defpackage #:sys.type
  (:use #:cl)
  (:export
   #:array-index
   #:u8
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
   #:write-u32-msb))

(in-package #:sys.type)

(deftype array-index ()
  `(mod ,array-dimension-limit))

;; 8 bit unsigned integer

(deftype u8 ()
  '(unsigned-byte 8))

(defgeneric write-u8 (stream u8))

(deftype ->u8 (&optional length)
  (let ((length (or length '*)))
    `(simple-array u8 (,length))))

(defun ->u8 (arg)
  (etypecase arg
    (array-index (make-array arg :element-type 'u8))
    (sequence (make-array (length arg)
                          :element-type 'u8
                          :initial-contents arg))))

(defun random-u8 (size)
  (->u8 (loop :for i :from 1 :to size
              :collect (random 256))))

(defun fixed-u8 (size)
  (->u8 (loop :for i :from 1 :to size
              :collect (mod i 256))))

;; 16 bit unsigned integer

(deftype u16 ()
  '(unsigned-byte 16))

(defun ->u16 (arg)
  (etypecase arg
    (array-index (make-array arg :element-type 'u16))
    (simple-array (make-array (length arg)
                              :element-type 'u16
                              :initial-contents arg))))

(defun write-u16-msb (stream u16)
  (write-u8 stream (ldb (byte 8 8) u16))
  (write-u8 stream (ldb (byte 8 0) u16)))

;; 32 bit unsigned integer

(deftype u32 ()
  '(unsigned-byte 32))

(deftype ->u32 (&optional length)
  (let ((length (or length '*)))
    `(simple-array u32 (,length))))

(defun ->u32 (arg)
  (etypecase arg
    (array-index (make-array arg :element-type 'u32))
    (simple-array (make-array (length arg)
                              :element-type 'u32
                              :initial-contents arg))))

;; LSB

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
