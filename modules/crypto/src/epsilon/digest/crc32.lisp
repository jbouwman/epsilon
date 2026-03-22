;;;; epsilon.digest.crc32 - CRC-32 hasher implementation
;;;;
;;;; CRC-32 is a non-cryptographic checksum algorithm commonly used for:
;;;; - Error detection in network protocols and file formats
;;;; - ZIP file checksums
;;;; - Fast data integrity verification
;;;;
;;;; WARNING: CRC-32 is NOT cryptographically secure.
;;;; Do not use for security purposes, only for error detection.
;;;;
;;;; This implementation uses the IEEE 802.3 polynomial (same as ZIP, Ethernet).

(defpackage epsilon.digest.crc32
  (:use :cl)
  (:require (epsilon.digest.protocol proto)
            (epsilon.typeclass tc))
  (:enter t))

;;; ============================================================================
;;; CRC-32 Lookup Table
;;; ============================================================================

(defvar *crc32-table* nil
  "CRC-32 lookup table (256 entries).")

(defun %init-crc32-table ()
  "Initialize the CRC-32 lookup table using IEEE 802.3 polynomial."
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (i 256)
      (let ((crc i))
        (declare (type (unsigned-byte 32) crc))
        (dotimes (j 8)
          (if (logbitp 0 crc)
              (setf crc (logxor #xEDB88320 (ash crc -1)))
              (setf crc (ash crc -1))))
        (setf (aref table i) crc)))
    table))

(defun ensure-crc32-table ()
  "Ensure CRC-32 table is initialized."
  (unless *crc32-table*
    (setf *crc32-table* (%init-crc32-table)))
  *crc32-table*)

;;; ============================================================================
;;; CRC-32 Hasher Structure
;;; ============================================================================

(defstruct (crc32-hasher (:constructor %make-crc32-hasher))
  "CRC-32 stateful hasher.

   CRC holds the current CRC-32 state (inverted during computation).
   FINALIZED-P tracks whether finalize has been called."
  (crc #xFFFFFFFF :type (unsigned-byte 32))
  (finalized-p nil :type boolean))

;;; ============================================================================
;;; Hasher Creation
;;; ============================================================================

(defun make-crc32-hasher ()
  "Create a new CRC-32 hasher."
  (ensure-crc32-table)
  (%make-crc32-hasher))

;;; ============================================================================
;;; Hasher Protocol Implementation
;;; ============================================================================

(tc:definstance proto:hasher crc32-hasher
  (proto:hasher-update (hasher data &key (start 0) (end (length data)))
    (when (crc32-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (let ((table (ensure-crc32-table))
          (crc (crc32-hasher-crc hasher)))
      (declare (type (simple-array (unsigned-byte 32) (256)) table)
               (type (simple-array (unsigned-byte 8) (*)) data)
               (type (unsigned-byte 32) crc)
               (type fixnum start end))
      (loop for i fixnum from start below end
            do (let ((index (logand #xFF (logxor crc (aref data i)))))
                 (setf crc (logxor (aref table index) (ash crc -8)))))
      (setf (crc32-hasher-crc hasher) crc))
    hasher)

  (proto:hasher-finalize (hasher &key output output-length)
    (declare (ignore output-length))
    (when (crc32-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (setf (crc32-hasher-finalized-p hasher) t)
    ;; XOR with all 1s to get final CRC
    (let* ((crc (logxor (crc32-hasher-crc hasher) #xFFFFFFFF))
           (result (or output (make-array 4 :element-type '(unsigned-byte 8)))))
      ;; Store as big-endian (network byte order)
      (setf (aref result 0) (ldb (byte 8 24) crc)
            (aref result 1) (ldb (byte 8 16) crc)
            (aref result 2) (ldb (byte 8 8) crc)
            (aref result 3) (ldb (byte 8 0) crc))
      result))

  (proto:hasher-reset (hasher)
    (setf (crc32-hasher-crc hasher) #xFFFFFFFF
          (crc32-hasher-finalized-p hasher) nil)
    hasher)

  (proto:hasher-copy (hasher)
    (when (crc32-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (%make-crc32-hasher :crc (crc32-hasher-crc hasher)))

  (proto:hasher-algorithm (hasher)
    (declare (ignore hasher))
    :crc32)

  (proto:hasher-output-length (hasher)
    (declare (ignore hasher))
    4)

  (proto:hasher-block-length (hasher)
    (declare (ignore hasher))
    1))

;;; ============================================================================
;;; One-Shot Functions
;;; ============================================================================

(defun crc32 (data &key (start 0) (end (length data)))
  "Compute CRC-32 checksum of DATA[start:end]. Returns 4-byte array (big-endian)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end))
  (let ((table (ensure-crc32-table))
        (crc #xFFFFFFFF))
    (declare (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) crc))
    (loop for i fixnum from start below end
          do (let ((index (logand #xFF (logxor crc (aref data i)))))
               (setf crc (logxor (aref table index) (ash crc -8)))))
    ;; XOR with all 1s and convert to byte array
    (let ((final-crc (logxor crc #xFFFFFFFF))
          (result (make-array 4 :element-type '(unsigned-byte 8))))
      (setf (aref result 0) (ldb (byte 8 24) final-crc)
            (aref result 1) (ldb (byte 8 16) final-crc)
            (aref result 2) (ldb (byte 8 8) final-crc)
            (aref result 3) (ldb (byte 8 0) final-crc))
      result)))

(defun crc32-int (data &key (start 0) (end (length data)))
  "Compute CRC-32 checksum of DATA[start:end]. Returns unsigned 32-bit integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end))
  (let ((table (ensure-crc32-table))
        (crc #xFFFFFFFF))
    (declare (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) crc))
    (loop for i fixnum from start below end
          do (let ((index (logand #xFF (logxor crc (aref data i)))))
               (setf crc (logxor (aref table index) (ash crc -8)))))
    (logxor crc #xFFFFFFFF)))

;;; ============================================================================
;;; Hash-Bytes Method
;;; ============================================================================

(defmethod proto:hash-bytes ((algorithm (eql :crc32)) data &key (start 0) (end (length data))
                                                                key output-length)
  "Compute CRC-32 via generic interface."
  (declare (ignore key output-length))
  (crc32 data :start start :end end))
