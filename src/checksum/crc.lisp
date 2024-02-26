(in-package :encode)

(defun crc32-table ()
  (let ((table (make-array 512 :element-type 'u16)))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type u32 c))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1)))
          (setf (aref table (ash n 1)) (ldb (byte 16 16) c)
                (aref table (1+ (ash n 1))) (ldb (byte 16 0) c)))))))

(defvar *crc32-table*
    (crc32-table))

(defun crc32 (high low buf start count)
  (declare (type u16 high low)
           (type array-index start count)
           (type ->u8 buf)
           (optimize speed))
  (let ((i start)
        (table *crc32-table*))
    (declare (type array-index i)
             (type (simple-array u16 (*)) table))
    (dotimes (j count (values high low))
      (let ((index (logxor (logand low #xFF) (aref buf i))))
        (declare (type (integer 0 255) index))
        (let ((high-index (ash index 1))
              (low-index (1+ (ash index 1))))
          (declare (type (integer 0 511) high-index low-index))
          (let ((t-high (aref table high-index))
                (t-low (aref table low-index)))
            (declare (type u16 t-high t-low))
            (incf i)
            (setf low (logxor (ash (logand high #xFF) 8)
                              (ash low -8)
                              t-low))
            (setf high (logxor (ash high -8) t-high))))))))

(defclass crc32-checksum ()
  ((low
    :initarg :low
    :initform #xffff
    :accessor low)
   (high
    :initarg :high
    :initform #xffff
    :accessor high)))

(defmethod update ((checksum crc32-checksum) input start count)
  (setf (values (high checksum)
                (low checksum))
        (crc32 (high checksum) (low checksum)
               input start count)))

(defmethod checksum ((checksum crc32-checksum))
  (+ (ash (logxor (high checksum) #xFFFF) 16)
     (logxor (low checksum) #xFFFF)))
