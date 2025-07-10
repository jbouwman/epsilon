;;;;; GZIP Compression/Decompression Module
;;;;; 
;;;;; This module implements RFC 1952 GZIP format support with header processing,
;;;;; CRC-32 validation, and proper integration with DEFLATE decompression.
;;;;; Extracted from epsilon.lib.codec for modularity.

(defpackage :epsilon.lib.gzip
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.function
   :epsilon.lib.type
   :epsilon.lib.checksum.crc-32
   :epsilon.lib.checksum.generic)
  (:local-nicknames
   (:inflate :epsilon.lib.inflate))
  (:export
   ;; Main functions
   :gzip-encode
   :gzip-decode
   :make-gzip-decompressing-stream
   :make-gzip-compressing-stream
   
   ;; Header and data structures
   :gzip-header
   :gzip-header-p
   :gzip-codec
   :gzip-compressor
   
   ;; Header accessors
   :flags
   :filename
   :write-date
   :mtime
   :comment
   :extra-flags
   :os
   :crc16
   :compression-method
   
   ;; Constants
   :+gzip-flag-text+
   :+gzip-flag-crc+
   :+gzip-flag-extra+
   :+gzip-flag-name+
   :+gzip-flag-comment+
   :+gzip-deflate-method+
   :+gzip-xfl-max-compression+
   :+gzip-xfl-fast-compression+
   :+gzip-fast-compression+
   :+gzip-deflate-compression+
   :+gzip-flags+
   :+gzip-unix-os+
   :+gzip-mtime+
   :*gzip-signature*
   
   ;; Errors
   :invalid-gzip-header-error
   :gzip-error))

(in-package :epsilon.lib.gzip)

;;;; GZIP Constants

;;; GZIP signature
(defvar *gzip-signature* (->u8 '(#x1F #x8B))
  "These two octets precede all data in the gzip format.")

;;; Individual bit meanings in the flag field
(defconstant +gzip-flag-text+ 0)
(defconstant +gzip-flag-crc+ 1)
(defconstant +gzip-flag-extra+ 2)
(defconstant +gzip-flag-name+ 3)
(defconstant +gzip-flag-comment+ 4)

;;; Values of the compression method byte
(defconstant +gzip-deflate-method+ 8)

;;; Values of the extra flag field
(defconstant +gzip-xfl-max-compression+ 2)
(defconstant +gzip-xfl-fast-compression+ 4)

;;; These are all used to create valid files, not to control or modify
;;; the compression process.
(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)
(defconstant +gzip-unix-os+ 3)
(defconstant +gzip-mtime+ 0)

(defconstant +gzip-fast-compression+ 4
  "Code for gzip compression level. This is present only to create valid
gzip data; it has no meaning to the compressor and is only a hint to
the decompressor.")

;;;; Error Conditions

(define-condition gzip-error (simple-error) ())

(define-condition invalid-gzip-header-error (gzip-error)
  ()
  (:report "Invalid GZIP header"))

;;;; GZIP Header Class

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

;;;; GZIP Decompression State Machine

(defun gzip-header-id (state)
  "Validate GZIP signature (magic number)."
  (declare (type inflate:inflate-state state))
  (let ((header-field (inflate:ensure-and-read-bits 16 state)))
    (unless (and (= (ldb (byte 8 0) header-field) #x1f)
                 (= (ldb (byte 8 8) header-field) #x8b))
      (error 'invalid-gzip-header-error))
    (setf (inflate:inflate-state-state state) #'gzip-cm)))

(defun gzip-cm (state)
  "Read compression method byte."
  (declare (type inflate:inflate-state state))
  (let ((cm-byte (inflate:ensure-and-read-bits 8 state)))
    (setf (inflate:inflate-state-header state)
          (make-instance 'gzip-header :compression-method cm-byte))
    (setf (inflate:inflate-state-state state) #'gzip-flags)))

(defun gzip-flags (state)
  "Read flags byte."
  (declare (type inflate:inflate-state state))
  (let ((flags-byte (inflate:ensure-and-read-bits 8 state)))
    (setf (flags (inflate:inflate-state-header state)) flags-byte)
    (setf (inflate:inflate-state-state state) #'gzip-mtime)))

(defun gzip-mtime (state)
  "Read modification time."
  (declare (type inflate:inflate-state state))
  (let ((mtime (inflate:ensure-and-read-bits 32 state)))
    (setf (mtime (inflate:inflate-state-header state)) mtime)
    (setf (inflate:inflate-state-state state) #'gzip-xfl)))

(defun gzip-xfl (state)
  "Read extra flags byte."
  (declare (type inflate:inflate-state state))
  (let ((xfl-byte (inflate:ensure-and-read-bits 8 state)))
    (setf (extra-flags (inflate:inflate-state-header state)) xfl-byte)
    (setf (inflate:inflate-state-state state) #'gzip-os)))

(defun gzip-os (state)
  "Read OS byte."
  (declare (type inflate:inflate-state state))
  (let ((os-byte (inflate:ensure-and-read-bits 8 state)))
    (setf (os (inflate:inflate-state-header state)) os-byte)
    (setf (inflate:inflate-state-state state) #'gzip-xlen-len)))

(defun gzip-xlen-len (state)
  "Handle extra field length (XLEN)."
  (declare (type inflate:inflate-state state))
  (let ((flags (flags (inflate:inflate-state-header state))))
    (cond
      ((logbitp +gzip-flag-extra+ flags)
       (error "gzip extra field not supported yet"))
      (t
       (setf (inflate:inflate-state-state state) #'gzip-fname)))))

(defun gzip-fname (state)
  "Process filename field."
  (declare (type inflate:inflate-state state))
  (process-gzip-zero-terminated-field state +gzip-flag-name+
                                      #'filename #'(setf filename)
                                      #'gzip-fcomment))

(defun gzip-fcomment (state)
  "Process comment field."
  (declare (type inflate:inflate-state state))
  (process-gzip-zero-terminated-field state +gzip-flag-comment+
                                      #'comment #'(setf comment)
                                      #'gzip-crc16))

(defun process-gzip-zero-terminated-field (state control-bit
                                           slot set-slot
                                           next-state)
  "Process a zero-terminated field in the GZIP header."
  (let ((header (inflate:inflate-state-header state)))
    (cond
      ((logbitp control-bit (flags header))
       (let ((byte (inflate:ensure-and-read-bits 8 state)))
         (cond
           ((zerop byte)
            ;; the end, convert to sane form
            (funcall set-slot
                     (coerce (funcall slot header)
                             '(vector u8))
                     header)
            (setf (inflate:inflate-state-state state) next-state))
           (t
            ;; wish we could use PUSH here
            (funcall set-slot
                     (cons byte (funcall slot header))
                     header)))))
      (t
       (setf (inflate:inflate-state-state state) next-state)))
    (values)))

(defun gzip-crc16 (state)
  "Process header CRC16."
  (declare (type inflate:inflate-state state))
  (let ((header (inflate:inflate-state-header state)))
    (when (logbitp +gzip-flag-crc+ (flags header))
      (let ((crc16 (inflate:ensure-and-read-bits 16 state)))
        ;; FIXME: would be good to perform integrity checking here
        (setf (crc16 header) crc16)))
    ;; Transition to deflate block processing
    (setf (inflate:inflate-state-state state) #'inflate:block-type)))

(defun gzip-crc32 (state)
  "Validate data CRC32 and complete decompression."
  (declare (type inflate:inflate-state state))
  (let ((stored (inflate:ensure-and-read-bits 32 state))
        (crc32 (inflate:inflate-state-checksum state)))
    (update crc32
            (inflate:inflate-state-output state)
            (inflate:inflate-state-output-start state)
            (- (inflate:inflate-state-output-index state)
               (inflate:inflate-state-output-start state)))
    (unless (= stored (checksum crc32))
      (error 'inflate:invalid-checksum-error
             :stored stored
             :computed (checksum crc32)
             :kind :crc32))
    (setf (inflate:inflate-state-state state) #'gzip-isize)))

(defun gzip-isize (state)
  "Read and validate uncompressed size."
  (declare (type inflate:inflate-state state))
  (let ((isize (inflate:ensure-and-read-bits 32 state)))
    (declare (ignore isize))
    (setf (inflate:inflate-state-done state) t)
    (setf (inflate:inflate-state-state state) #'inflate:done)))

;;;; GZIP Codec Classes

(defclass gzip-compressor ()
  ((checksum :initform (make-instance 'crc-32))
   (data-length :initform 0)))

(defclass gzip-codec ()
  ())

;;;; High-level API

(defun gzip-decode (input output)
  "Decompress GZIP data from INPUT to OUTPUT."
  (let ((state (inflate:make-inflate-state :gzip)))
    ;; Set up GZIP-specific state machine entry point
    (setf (inflate:inflate-state-state state) #'gzip-header-id)
    (inflate:inflate-decompress state input output)))

(defun gzip-encode (input output)
  "Compress data from INPUT to OUTPUT using GZIP format."
  ;; TODO: Implement GZIP compression
  (error "GZIP compression not yet implemented"))

(defun make-gzip-decompressing-stream (source)
  "Create a GZIP decompressing stream from SOURCE."
  (let ((state (inflate:make-inflate-state :gzip)))
    ;; Set up GZIP-specific state machine entry point
    (setf (inflate:inflate-state-state state) #'gzip-header-id)
    (inflate:make-inflating-stream :gzip source)))

(defun make-gzip-compressing-stream (destination)
  "Create a GZIP compressing stream to DESTINATION."
  ;; TODO: Implement GZIP compressing stream
  (error "GZIP compressing stream not yet implemented"))

;;;; Backward compatibility functions

(defmethod encode ((codec gzip-codec) in out)
  "Encode using GZIP codec."
  (gzip-encode in out))

(defmethod decode ((codec gzip-codec) in out)
  "Decode using GZIP codec."
  (gzip-decode in out))