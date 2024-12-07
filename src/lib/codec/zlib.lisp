(in-package #:epsilon.lib.codec)

(defclass zlib-header ()
  ((flags :initarg :flags :accessor flags)
   (cmf :initarg :cmf :accessor cmf)
   (fdict :initarg :fdict :accessor fdict)
   (adler32 :initarg :adler32 :accessor adler32)))

(defconstant +zlib-compression-method+ 8)

(defun zlib-compression-method (cmf-byte)
  (declare (type u8 cmf-byte))
  (ldb (byte 4 0) cmf-byte))

(defun zlib-compression-info (cmf-byte)
  (declare (type u8 cmf-byte))
  (ldb (byte 4 4) cmf-byte))

(defconstant +zlib-flag-fdict+ 5)

(defun zlib-flag-fcheck (flag-byte)
  (declare (type u8 flag-byte))
  (ldb (byte 4 0) flag-byte))

(defconstant +zlib-flevel-fastest+ 0)
(defconstant +zlib-flevel-fast+ 1)
(defconstant +zlib-flevel-default+ 2)
(defconstant +zlib-flevel-maximum+ 3)

(defun zlib-flag-flevel (flag-byte)
  (declare (type u8 flag-byte))
  (ldb (byte 2 6) flag-byte))

(defclass zlib-compressor (deflate-compressor)
  ((checksum :initform (make-instance 'adler-32))))

(defconstant +zlib-magic+ #x789c)

(defmethod start-data-format :before ((compressor zlib-compressor))
  (write-u16-msb compressor +zlib-magic+))

(defmethod process-input :after ((compressor zlib-compressor) input start count)
  (with-slots (checksum) compressor
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor zlib-compressor))
  (with-slots (checksum) compressor
    (write-u32-msb compressor (checksum checksum))))

(defclass zlib-codec ()
  ())

(defmethod encode ((codec zlib-codec) in out)
  (compress-stream 'zlib-compressor in out))

(defmethod decode ((codec zlib-codec) in out)
  (decompress out (make-dstate :zlib) in))

(defclass deflate-codec ()
  ())

(defmethod encode ((codec deflate-codec) in out)
  (compress-stream 'deflate-compressor in out))

(defmethod decode ((codec deflate-codec) in out)
  (decompress out (make-dstate :deflate) in))
