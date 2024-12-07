(defpackage #:epsilon.lib.checksum.crc-32
  (:use
   #:cl
   #:epsilon.lib.type
   #:epsilon.lib.checksum.generic)
  (:export
   #:crc-32))

(in-package #:epsilon.lib.checksum.crc-32)

(defun crc-32-table ()
  (let ((table (->u16 512)))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type u32 c))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor #xedb88320 (ash c -1)))
              (setf c (ash c -1)))
          (setf (aref table (ash n 1))
                (ldb (byte 16 16) c)
                (aref table (1+ (ash n 1)))
                (ldb (byte 16 0) c)))))))

(defvar *crc-32-table* (crc-32-table))

(defclass crc-32 ()
  ((low :initform #xffff)
   (high :initform #xffff)))

(defmethod update ((checksum crc-32) buffer start count)
  (declare (type ->u8 buffer)
           (type array-index start count))
  (with-slots (high low) checksum
    (declare (type u16 high low))
      (loop :with table := (the (->u16 512) *crc-32-table*)
            :for i :from start :below (+ start count)
            :do (let ((index (logxor (logand low #xff) (aref buffer i))))
                  (declare (type (integer 0 255) index))
                  (let ((high-index (ash index 1))
                        (low-index (1+ (ash index 1))))
                    (declare (type (integer 0 511) high-index low-index))
                    (let ((t-high (aref table high-index))
                          (t-low (aref table low-index)))
                      (declare (type u16 t-high t-low))
                      (setf low (logxor (ash (logand high #xFF) 8)
                                        (ash low -8)
                                        t-low))
                      (setf high (logxor (ash high -8) t-high))))))))

(defmethod checksum ((checksum crc-32))
  (with-slots (high low) checksum
    (+ (ash (logxor high #xffff) 16)
       (logxor low #xffff))))
