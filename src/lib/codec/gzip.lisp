(in-package #:epsilon.lib.codec)

(defclass gzip-header ()
  ((flags :initarg :flags :accessor flags)
   (filename :initform nil :accessor filename)
   (write-date :initarg :write-date :accessor write-date)
   (mtime :initform 0 :accessor mtime)
   (comment :initform nil :accessor comment)
   (extra-flags :initarg :extra-flags :accessor extra-flags)
   (os :initarg :os :accessor os)
   (crc16 :initarg :crc16 :accessor crc16)
   (compression-method :initarg :compression-method :accessor compression-method)))

;;; individual bit meanings in the flag field

(defconstant +gzip-flag-text+ 0)
(defconstant +gzip-flag-crc+ 1)
(defconstant +gzip-flag-extra+ 2)
(defconstant +gzip-flag-name+ 3)
(defconstant +gzip-flag-comment+ 4)

;;; values of the compression method byte

(defconstant +gzip-deflate-method+ 8)

;;; values of the extra flag field

(defconstant +gzip-xfl-max-compression+ 2)

(defconstant +gzip-xfl-fast-compression+ 4)

(defvar *gzip-signature* (->u8 '(#x1F #x8B))
  "These two octets precede all data in the gzip format.")

(defconstant +gzip-fast-compression+ 4
  "Code for gzip compression level. This is present only to create valid
gzip data; it has no meaning to the compressor and is only a hint to
the decompressor.")

;;; These are all used to create valid files, not to control or modify
;;; the compression process.

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)
(defconstant +gzip-unix-os+ 3)
(defconstant +gzip-mtime+ 0)

(defclass gzip-compressor (deflate-compressor)
  ((checksum :initform (make-instance 'crc-32))
   (data-length :initform 0)))

(defmethod start-data-format :before ((compressor gzip-compressor))
  (write-u8-vector *gzip-signature* compressor)
  (write-u8 compressor +gzip-deflate-compression+)
  (write-u8 compressor +gzip-flags+)
  (write-u32 compressor +gzip-mtime+)
  (write-u8 compressor +gzip-fast-compression+)
  (write-u8 compressor +gzip-unix-os+))

(defmethod process-input :after ((compressor gzip-compressor) input start count)
  (with-slots (data-length checksum) compressor
    (incf data-length count)
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor gzip-compressor))
  (with-slots (data-length checksum) compressor
    (write-u32 compressor (checksum checksum))
    (write-u32 compressor data-length)))

(defclass gzip-codec ()
  ())

(defmethod encode ((codec gzip-codec) in out)
  (compress-stream 'gzip-compressor in out))

(defmethod decode ((codec gzip-codec) in out)
  (decompress out (make-dstate :gzip) in))
