;;;; Binary data encoding with explicit endianness control.
;;;;
;;;; This module provides endian-aware integer-to-bytes conversion,
;;;; which is distinct from epsilon.io's I/O abstraction.

(package epsilon.binary
  (shadow write)
  (import (epsilon.io io)))

;;; Configuration

(defparameter *default-endian* :big-endian
  "Default endianness for binary operations. Can be :little-endian, :big-endian, or :native")

(defparameter *native-endian*
  #+little-endian :little-endian
  #+big-endian :big-endian
  #-(or little-endian big-endian) :little-endian
  "The native endianness of the current platform")

(defun resolve-endian (endian)
  "Resolve endianness keyword to canonical form"
  (ecase endian
    ((:native) *native-endian*)
    ((:little-endian :little :le) :little-endian)
    ((:big-endian :big :be :network) :big-endian)))

;;; Low-level byte operations

(declaim (inline set-bytes-le set-bytes-be get-bytes-le get-bytes-be))

(defun set-bytes-le (bytes offset value size)
  "Set little-endian unsigned integer in bytes"
  (loop for i from 0 below size
        do (setf (aref bytes (+ offset i))
                 (ldb (byte 8 (* i 8)) value))))

(defun set-bytes-be (bytes offset value size)
  "Set big-endian unsigned integer in bytes"
  (loop for i from 0 below size
        do (setf (aref bytes (+ offset i))
                 (ldb (byte 8 (* (- size i 1) 8)) value))))

(defun get-bytes-le (bytes offset size)
  "Get little-endian unsigned integer from bytes"
  (loop for i from 0 below size
        sum (ash (aref bytes (+ offset i)) (* i 8))))

(defun get-bytes-be (bytes offset size)
  "Get big-endian unsigned integer from bytes"
  (loop for i from 0 below size
        sum (ash (aref bytes (+ offset i)) (* (- size i 1) 8))))

;;; Integer conversion functions

(defun uint16-to-octets (value &optional (endian :big-endian))
  "Convert a 16-bit unsigned integer to a 2-byte array"
  (let ((bytes (make-array 2 :element-type '(unsigned-byte 8)))
        (endian (resolve-endian endian)))
    (ecase endian
      (:little-endian (set-bytes-le bytes 0 value 2))
      (:big-endian (set-bytes-be bytes 0 value 2)))
    bytes))

(defun octets-to-uint16 (octets &optional (offset 0) (endian :big-endian))
  "Convert 2 bytes to a 16-bit unsigned integer"
  (let ((endian (resolve-endian endian)))
    (ecase endian
      (:little-endian (get-bytes-le octets offset 2))
      (:big-endian (get-bytes-be octets offset 2)))))

(defun uint64-to-octets (value &optional (endian :big-endian))
  "Convert a 64-bit unsigned integer to an 8-byte array"
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)))
        (endian (resolve-endian endian)))
    (ecase endian
      (:little-endian (set-bytes-le bytes 0 value 8))
      (:big-endian (set-bytes-be bytes 0 value 8)))
    bytes))

(defun octets-to-uint64 (octets &optional (offset 0) (endian :big-endian))
  "Convert 8 bytes to a 64-bit unsigned integer"
  (let ((endian (resolve-endian endian)))
    (ecase endian
      (:little-endian (get-bytes-le octets offset 8))
      (:big-endian (get-bytes-be octets offset 8)))))

;;; Endianness macro

(defmacro with-endian ((endian) &body body)
  "Execute BODY with *default-endian* bound to ENDIAN"
  `(let ((*default-endian* ,endian))
     ,@body))

;;; Output building

(defmacro with-output-to-octets ((writer-var) &body body)
  "Execute BODY with WRITER-VAR bound to an epsilon.io byte-writer, return bytes"
  `(let ((,writer-var (io:make-byte-writer)))
     ,@body
     (io:byte-writer-bytes ,writer-var)))

(defun write (writer value type &optional (endian *default-endian*))
  "Write VALUE to WRITER as TYPE with specified ENDIAN.
   Supported types: :u8, :u16, :u64"
  (let ((endian (resolve-endian endian)))
    (ecase type
      (:u8
       (let ((buf (make-array 1 :element-type '(unsigned-byte 8)
                              :initial-element (ldb (byte 8 0) value))))
         (io:write-all writer buf)))
      (:u16
       (io:write-all writer (uint16-to-octets value endian)))
      (:u64
       (io:write-all writer (uint64-to-octets value endian))))
    value))

(defun write-bytes (writer bytes &key (start 0) end)
  "Write BYTES to WRITER"
  (let ((end (or end (length bytes))))
    (io:write-from writer bytes :start start :end end)))
