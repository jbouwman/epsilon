;;;; CRC-32 Implementation for Digest Package

(defpackage #:epsilon.digest.crc-32
  (:use
   #:cl
   #:epsilon.type
   #:epsilon.digest.generic)
  (:export
   #:crc-32
   #:make-crc32-digest
   #:crc32
   #:crc32-sequence))

(in-package #:epsilon.digest.crc-32)

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

;; Integrate with the digest framework using update-digest method
(defmethod update-digest ((checksum crc-32) (buffer vector) &key (start 0) (end (length buffer)))
  (declare (type ->u8 buffer)
           (type array-index start end))
  (with-slots (high low) checksum
    (declare (type u16 high low))
      (loop :with table := (the (->u16 512) *crc-32-table*)
            :for i :from start :below end
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

(defmethod copy-digest ((checksum crc-32) &optional copy)
  (let ((result (or copy (make-instance 'crc-32))))
    (with-slots ((src-high high) (src-low low)) checksum
      (with-slots ((dst-high high) (dst-low low)) result
        (setf dst-high src-high
              dst-low src-low)))
    result))

(defmethod produce-digest ((checksum crc-32) &key digest (digest-start 0))
  (with-slots (high low) checksum
    (let ((result (+ (ash (logxor high #xffff) 16)
                     (logxor low #xffff))))
      (if digest
          ;; Write to provided digest buffer
          (progn
            (setf (aref digest (+ digest-start 0)) (ldb (byte 8 24) result)
                  (aref digest (+ digest-start 1)) (ldb (byte 8 16) result)
                  (aref digest (+ digest-start 2)) (ldb (byte 8 8) result)
                  (aref digest (+ digest-start 3)) (ldb (byte 8 0) result))
            digest)
          ;; Return new 4-byte array in big-endian format
          (let ((digest-array (make-array 4 :element-type '(unsigned-byte 8))))
            (setf (aref digest-array 0) (ldb (byte 8 24) result)
                  (aref digest-array 1) (ldb (byte 8 16) result)
                  (aref digest-array 2) (ldb (byte 8 8) result)
                  (aref digest-array 3) (ldb (byte 8 0) result))
            digest-array)))))

(defmethod digest-length ((digest crc-32))
  4)

(defun make-crc32-digest ()
  "Create a new CRC-32 digest instance"
  (make-instance 'crc-32))

;; Convenience functions for direct calculation
(defun crc32 (octets)
  "Compute CRC-32 checksum of octets, return as integer"
  (let ((digest (make-crc32-digest)))
    (update-digest digest octets)
    (let ((result (produce-digest digest)))
      ;; Convert 4-byte array back to integer
      (+ (ash (aref result 0) 24)
         (ash (aref result 1) 16)
         (ash (aref result 2) 8)
         (aref result 3)))))

(defun crc32-sequence (sequence)
  "Compute CRC-32 checksum of sequence, return as integer"
  (let* ((octets (make-array (length sequence) :element-type '(unsigned-byte 8)))
         (i 0))
    (map nil (lambda (x) 
               (setf (aref octets i) x)
               (incf i)) 
         sequence)
    (crc32 octets)))