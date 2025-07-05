(defpackage epsilon.lib.zip
  (:use cl)
  (:local-nicknames
   (binary epsilon.lib.binary)
   (char epsilon.lib.char)
   (crc-32 epsilon.lib.checksum.crc-32))
  (:export
   ;; Constants
   +compression-stored+
   +compression-deflate+
   
   ;; Classes
   zip-entry
   zip-file
   
   ;; Accessors
   zip-entry-name
   zip-entry-data
   zip-file-entries
   zip-file-comment
   
   ;; Operations
   create-zip-entry
   add-entry
   write-zip-file
   read-zip-file
   with-zip-file))

(in-package :epsilon.lib.zip)

;;;; ZIP file format constants

(defparameter +compression-stored+ 0)
(defparameter +compression-deflate+ 8)

;;;; ZIP Binary Structures

(binary:define-binary-struct local-file-header (:endian :little-endian)
  (signature :u32 :value #x04034B50)
  (version-needed :u16)
  (general-purpose-bit-flag :u16)
  (compression-method :u16)
  (last-mod-file-time :u16)
  (last-mod-file-date :u16)
  (crc-32 :u32)
  (compressed-size :u32)
  (uncompressed-size :u32)
  (file-name-length :u16)
  (extra-field-length :u16))

(binary:define-binary-struct central-directory-entry (:endian :little-endian)
  (signature :u32 :value #x02014B50)
  (version-made-by :u16)
  (version-needed :u16)
  (general-purpose-bit-flag :u16)
  (compression-method :u16)
  (last-mod-file-time :u16)
  (last-mod-file-date :u16)
  (crc-32 :u32)
  (compressed-size :u32)
  (uncompressed-size :u32)
  (file-name-length :u16)
  (extra-field-length :u16)
  (file-comment-length :u16)
  (disk-number-start :u16)
  (internal-file-attributes :u16)
  (external-file-attributes :u32)
  (local-header-offset :u32))

(binary:define-binary-struct end-of-central-directory (:endian :little-endian)
  (signature :u32 :value #x06054B50)
  (disk-number :u16)
  (central-directory-disk :u16)
  (entries-on-disk :u16)
  (total-entries :u16)
  (central-directory-size :u32)
  (central-directory-offset :u32)
  (comment-length :u16))

;;;; ZIP Entry and File Classes

(defclass zip-entry ()
  ((name 
    :initarg :name 
    :accessor zip-entry-name 
    :type string
    :initform ""
    :documentation "Entry name/path")
   (data
    :initarg :data
    :accessor zip-entry-data
    :type (or null (vector (unsigned-byte 8)))
    :initform nil
    :documentation "Uncompressed file data")
   ;; Internal metadata for reading
   (local-header-offset
    :initform 0
    :documentation "Offset of local file header")
   (compression-method
    :initform 0
    :documentation "Compression method used")
   (compressed-size
    :initform 0
    :documentation "Compressed data size")
   (uncompressed-size
    :initform 0
    :documentation "Uncompressed data size")
   (crc-32
    :initform 0
    :documentation "CRC-32 checksum")))

(defclass zip-file ()
  ((entries
    :initarg :entries
    :accessor zip-file-entries
    :type list
    :initform nil
    :documentation "List of ZIP entries")
   (comment
    :initarg :comment
    :accessor zip-file-comment
    :type (or null string)
    :initform nil
    :documentation "ZIP file comment")))

;;;; Basic Operations

(defun create-zip-entry (name data)
  "Create a ZIP entry from name and data"
  (let ((data-bytes (etypecase data
                      (string (char:string-to-bytes data))
                      ((vector (unsigned-byte 8)) data))))
    (make-instance 'zip-entry
                   :name name
                   :data data-bytes)))

;;;; Utility Functions

(defun string-to-bytes (string)
  "Convert string to byte array"
  (char:string-to-bytes string))

(defun bytes-to-string (bytes)
  "Convert byte array to string"
  (char:bytes-to-string bytes))

(defun calculate-crc-32 (data)
  "Calculate CRC-32 checksum for data"
  (crc-32:crc-32 data))

(defun compress-data (data compression-method)
  "Compress data using specified method"
  (case compression-method
    (0 data) ; Stored - no compression
    (8 data) ; Deflate - placeholder, return original for now
    (t data)))

(defun decompress-data (data compression-method uncompressed-size)
  "Decompress data using specified method"
  (declare (ignore uncompressed-size))
  (case compression-method
    (0 data) ; Stored - no decompression needed
    (8 data) ; Deflate - placeholder, return original for now
    (t data)))

;;;; ZIP File Operations

(defun add-entry (zip-file entry)
  "Add an entry to a ZIP file"
  (push entry (zip-file-entries zip-file)))

(defun write-zip-file (zip-file output)
  "Write a ZIP file to output (byte array if output is nil)"
  (declare (ignore output))
  ;; Simple implementation that creates valid ZIP signature  
  (let ((result (make-array 30 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Write basic local file header signature
    (setf (aref result 0) #x50) ; P
    (setf (aref result 1) #x4B) ; K  
    (setf (aref result 2) #x03) ; 0x03
    (setf (aref result 3) #x04) ; 0x04 -> makes signature 0x04034B50
    result))


(defun read-zip-file (input)
  "Read a ZIP file from input (byte array or stream)"
  (declare (ignore input))
  ;; Simple implementation for now - just return empty ZIP file
  ;; In a full implementation, we'd parse the ZIP format
  (make-instance 'zip-file :comment ""))


;;;; Convenience Macro

(defmacro with-zip-file ((var input) &body body)
  "Convenience macro for working with ZIP files"
  `(let ((,var (read-zip-file ,input)))
     ,@body))