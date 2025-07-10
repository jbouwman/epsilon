;;;;; ZLIB Compression/Decompression Module
;;;;; 
;;;;; This module implements RFC 1950 ZLIB format support with header processing,
;;;;; Adler-32 validation, and proper integration with DEFLATE decompression.
;;;;; Extracted from epsilon.lib.codec for modularity.

(defpackage :epsilon.lib.zlib
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.function
   :epsilon.lib.type
   :epsilon.lib.checksum.adler-32
   :epsilon.lib.checksum.generic)
  (:local-nicknames
   (:inflate :epsilon.lib.inflate))
  (:export
   ;; Main functions
   :zlib-encode
   :zlib-decode
   :make-zlib-decompressing-stream
   :make-zlib-compressing-stream
   
   ;; Header and data structures
   :zlib-header
   :zlib-header-p
   :zlib-codec
   :zlib-compressor
   
   ;; Header accessors
   :flags
   :cmf
   :fdict
   :adler32
   
   ;; Header parsing functions
   :zlib-compression-method
   :zlib-compression-info
   :zlib-flag-fcheck
   :zlib-flag-flevel
   
   ;; Constants
   :+zlib-compression-method+
   :+zlib-flag-fdict+
   :+zlib-flevel-fastest+
   :+zlib-flevel-fast+
   :+zlib-flevel-default+
   :+zlib-flevel-maximum+
   :+zlib-magic+
   
   ;; Errors
   :invalid-zlib-header-error
   :zlib-error))

(in-package :epsilon.lib.zlib)

;;;; ZLIB Constants

(defconstant +zlib-compression-method+ 8)
(defconstant +zlib-flag-fdict+ 5)
(defconstant +zlib-magic+ #x789c)

;;; Compression levels
(defconstant +zlib-flevel-fastest+ 0)
(defconstant +zlib-flevel-fast+ 1)
(defconstant +zlib-flevel-default+ 2)
(defconstant +zlib-flevel-maximum+ 3)

;;;; Helper Functions

(defun zlib-compression-method (cmf-byte)
  "Extract compression method from CMF byte."
  (declare (type u8 cmf-byte))
  (ldb (byte 4 0) cmf-byte))

(defun zlib-compression-info (cmf-byte)
  "Extract compression info from CMF byte."
  (declare (type u8 cmf-byte))
  (ldb (byte 4 4) cmf-byte))

(defun zlib-flag-fcheck (flag-byte)
  "Extract FCHECK value from flag byte."
  (declare (type u8 flag-byte))
  (ldb (byte 4 0) flag-byte))

(defun zlib-flag-flevel (flag-byte)
  "Extract FLEVEL value from flag byte."
  (declare (type u8 flag-byte))
  (ldb (byte 2 6) flag-byte))

;;;; Error Conditions

(define-condition zlib-error (simple-error) ())

(define-condition invalid-zlib-header-error (zlib-error)
  ()
  (:report "Invalid ZLIB header"))

;;;; ZLIB Header Class

(defclass zlib-header ()
  ((flags :initarg :flags :accessor flags)
   (cmf :initarg :cmf :accessor cmf)
   (fdict :initarg :fdict :accessor fdict)
   (adler32 :initarg :adler32 :accessor adler32)))

;;;; ZLIB Decompression State Machine

(defun zlib-cmf (state)
  "Read compression method and flags (CMF) byte."
  (declare (type inflate:inflate-state state))
  (let ((cmf-byte (inflate:ensure-and-read-bits 8 state)))
    (setf (inflate:inflate-state-header state)
          (make-instance 'zlib-header :cmf cmf-byte))
    (setf (inflate:inflate-state-state state) #'zlib-flags)))

(defun zlib-flags (state)
  "Read flags byte and validate header."
  (declare (type inflate:inflate-state state))
  (let ((flags-byte (inflate:ensure-and-read-bits 8 state))
        (header (inflate:inflate-state-header state)))
    ;; Validate header checksum
    (unless (zerop (mod (+ (* (cmf header) 256) flags-byte) 31))
      (error 'invalid-zlib-header-error))
    (setf (flags header) flags-byte)
    (setf (inflate:inflate-state-state state) #'zlib-fdict)))

(defun zlib-fdict (state)
  "Process dictionary if present."
  (declare (type inflate:inflate-state state))
  (let* ((header (inflate:inflate-state-header state))
         (flags-byte (flags header)))
    (when (logbitp +zlib-flag-fdict+ flags-byte)
      (let ((fdict (inflate:ensure-and-read-bits 32 state)))
        (setf (fdict header) fdict)))
    ;; Transition to deflate block processing
    (setf (inflate:inflate-state-state state) #'inflate:block-type)))

(defun check-zlib-adler32 (state)
  "Validate Adler-32 checksum and complete decompression."
  (declare (type inflate:inflate-state state))
  (let ((stored (let ((x (inflate:ensure-and-read-bits 32 state)))
                  (logior (ash (ldb (byte 8 0) x) 24)
                          (ash (ldb (byte 8 8) x) 16)
                          (ash (ldb (byte 8 16) x) 8)
                          (ldb (byte 8 24) x))))
        (adler32 (inflate:inflate-state-checksum state)))
    (update adler32
            (inflate:inflate-state-output state)
            (inflate:inflate-state-output-start state)
            (- (inflate:inflate-state-output-index state)
               (inflate:inflate-state-output-start state)))
    (unless (= stored
               (checksum adler32))
      (error 'inflate:invalid-checksum-error
             :stored stored
             :computed (checksum adler32)
             :kind :adler32))
    (setf (inflate:inflate-state-done state) t)
    (setf (inflate:inflate-state-state state) #'inflate:done)))

;;;; ZLIB Codec Classes

(defclass zlib-compressor ()
  ((checksum :initform (make-instance 'adler-32))))

(defclass zlib-codec ()
  ())

;;;; High-level API

(defun zlib-decode (input output)
  "Decompress ZLIB data from INPUT to OUTPUT."
  (let ((state (inflate:make-inflate-state :zlib)))
    ;; Set up ZLIB-specific state machine entry point
    (setf (inflate:inflate-state-state state) #'zlib-cmf)
    (inflate:inflate-decompress state input output)))

(defun zlib-encode (input output)
  "Compress data from INPUT to OUTPUT using ZLIB format."
  ;; TODO: Implement ZLIB compression
  (error "ZLIB compression not yet implemented"))

(defun make-zlib-decompressing-stream (source)
  "Create a ZLIB decompressing stream from SOURCE."
  (let ((state (inflate:make-inflate-state :zlib)))
    ;; Set up ZLIB-specific state machine entry point
    (setf (inflate:inflate-state-state state) #'zlib-cmf)
    (inflate:make-inflating-stream :zlib source)))

(defun make-zlib-compressing-stream (destination)
  "Create a ZLIB compressing stream to DESTINATION."
  ;; TODO: Implement ZLIB compressing stream
  (error "ZLIB compressing stream not yet implemented"))

;;;; Backward compatibility functions

(defmethod encode ((codec zlib-codec) in out)
  "Encode using ZLIB codec."
  (zlib-encode in out))

(defmethod decode ((codec zlib-codec) in out)
  "Decode using ZLIB codec."
  (zlib-decode in out))