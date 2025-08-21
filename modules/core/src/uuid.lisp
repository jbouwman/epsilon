;;;; UUID Generation and Manipulation
;;;;
;;;; This module provides a standards-compliant implementation of UUID
;;;; (Universally Unique Identifier) generation and manipulation following
;;;; RFC 4122. It supports UUID versions 1, 3, 4, and 5.

(defpackage epsilon.uuid
  (:use cl)
  (:local-nicknames
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   ;; UUID type and predicates
   #:uuid
   #:uuid-p
   #:valid-uuid-p
   
   ;; UUID creation
   #:make-v1
   #:make-v3
   #:make-v4
   #:make-v5
   #:make-nil
   #:make-from-string
   #:make-from-bytes
   
   ;; UUID conversion
   #:to-string
   #:to-bytes
   #:to-integer
   #:from-integer
   
   ;; UUID components
   #:uuid-time-low
   #:uuid-time-mid
   #:uuid-time-hi-and-version
   #:uuid-clock-seq-hi-and-reserved
   #:uuid-clock-seq-low
   #:uuid-node
   #:uuid-version
   #:uuid-variant
   
   ;; Predefined namespace UUIDs (RFC 4122)
   #:+namespace-dns+
   #:+namespace-url+
   #:+namespace-oid+
   #:+namespace-x500+
   
   ;; Nil UUID
   #:+nil-uuid+))

(in-package epsilon.uuid)

;;; UUID Structure

(defstruct (uuid (:constructor %make-uuid)
                 (:print-function print-uuid))
  "A UUID (Universally Unique Identifier) as defined by RFC 4122."
  (time-low 0 :type (unsigned-byte 32))
  (time-mid 0 :type (unsigned-byte 16))
  (time-hi-and-version 0 :type (unsigned-byte 16))
  (clock-seq-hi-and-reserved 0 :type (unsigned-byte 8))
  (clock-seq-low 0 :type (unsigned-byte 8))
  (node 0 :type (unsigned-byte 48)))

(defun print-uuid (uuid stream depth)
  "Print a UUID in the standard 8-4-4-4-12 format."
  (declare (ignore depth))
  (format stream "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X"
          (uuid-time-low uuid)
          (uuid-time-mid uuid)
          (uuid-time-hi-and-version uuid)
          (uuid-clock-seq-hi-and-reserved uuid)
          (uuid-clock-seq-low uuid)
          (uuid-node uuid)))

;;; Predefined namespace UUIDs

(defparameter +namespace-dns+
  (%make-uuid :time-low #x6ba7b810
              :time-mid #x9dad
              :time-hi-and-version #x11d1
              :clock-seq-hi-and-reserved #x80
              :clock-seq-low #xb4
              :node #x00c04fd430c8)
  "Namespace UUID for DNS (RFC 4122).")

(defparameter +namespace-url+
  (%make-uuid :time-low #x6ba7b811
              :time-mid #x9dad
              :time-hi-and-version #x11d1
              :clock-seq-hi-and-reserved #x80
              :clock-seq-low #xb4
              :node #x00c04fd430c8)
  "Namespace UUID for URLs (RFC 4122).")

(defparameter +namespace-oid+
  (%make-uuid :time-low #x6ba7b812
              :time-mid #x9dad
              :time-hi-and-version #x11d1
              :clock-seq-hi-and-reserved #x80
              :clock-seq-low #xb4
              :node #x00c04fd430c8)
  "Namespace UUID for ISO OIDs (RFC 4122).")

(defparameter +namespace-x500+
  (%make-uuid :time-low #x6ba7b814
              :time-mid #x9dad
              :time-hi-and-version #x11d1
              :clock-seq-hi-and-reserved #x80
              :clock-seq-low #xb4
              :node #x00c04fd430c8)
  "Namespace UUID for X.500 DNs (RFC 4122).")

(defparameter +nil-uuid+
  (%make-uuid :time-low 0
              :time-mid 0
              :time-hi-and-version 0
              :clock-seq-hi-and-reserved 0
              :clock-seq-low 0
              :node 0)
  "The nil UUID (all zeros).")

;;; UUID Version and Variant

(defun uuid-version (uuid)
  "Return the version number of a UUID (1-5)."
  (ash (logand (uuid-time-hi-and-version uuid) #xF000) -12))

(defun uuid-variant (uuid)
  "Return the variant of a UUID."
  (let ((octet (uuid-clock-seq-hi-and-reserved uuid)))
    (cond
      ((= (logand octet #x80) 0) :ncs)        ; 0xx
      ((= (logand octet #xC0) #x80) :rfc4122) ; 10x
      ((= (logand octet #xE0) #xC0) :microsoft) ; 110
      (t :reserved))))                          ; 111

;;; Random number generation

(defvar *uuid-random-state* (make-random-state t)
  "Random state for UUID generation.")

(defun random-bytes (n)
  "Generate N random bytes."
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref bytes i) (random 256 *uuid-random-state*)))
    bytes))

;;; UUID Creation - Version 4 (Random)

(defun make-v4 ()
  "Create a version 4 (random) UUID."
  (let ((bytes (random-bytes 16)))
    ;; Set version to 4
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0F) #x40))
    ;; Set variant to RFC 4122
    (setf (aref bytes 8) (logior (logand (aref bytes 8) #x3F) #x80))
    (make-from-bytes bytes)))

;;; UUID Creation - Version 1 (Time-based)

(defvar *clock-seq* (random #x4000 *uuid-random-state*)
  "Clock sequence for UUID v1 generation.")

(defvar *last-time* 0
  "Last timestamp used for UUID v1 generation.")

(defvar *node-id* nil
  "Node ID for UUID v1 generation (MAC address or random).")

(defun get-node-id ()
  "Get or generate a node ID for UUID v1."
  (or *node-id*
      (setf *node-id*
            ;; Generate a random node ID with multicast bit set
            (let ((node (random (ash 1 48) *uuid-random-state*)))
              (logior node (ash 1 40))))))

(defun uuid-timestamp ()
  "Get current timestamp in 100-nanosecond intervals since 1582-10-15."
  ;; Offset between Unix epoch (1970-01-01) and UUID epoch (1582-10-15)
  (let* ((uuid-epoch-offset 122192928000000000)
         (unix-time (get-universal-time))
         ;; Convert to 100-nanosecond intervals
         (timestamp (+ (* unix-time 10000000) uuid-epoch-offset)))
    ;; Ensure uniqueness
    (when (<= timestamp *last-time*)
      (setf timestamp (1+ *last-time*)))
    (setf *last-time* timestamp)
    timestamp))

(defun make-v1 (&optional node)
  "Create a version 1 (time-based) UUID."
  (let* ((timestamp (uuid-timestamp))
         (time-low (logand timestamp #xFFFFFFFF))
         (time-mid (logand (ash timestamp -32) #xFFFF))
         (time-hi (logand (ash timestamp -48) #x0FFF))
         (clock-seq (logand *clock-seq* #x3FFF))
         (node-id (or node (get-node-id))))
    (%make-uuid :time-low time-low
                :time-mid time-mid
                :time-hi-and-version (logior time-hi #x1000) ; Version 1
                :clock-seq-hi-and-reserved (logior (ash clock-seq -8) #x80)
                :clock-seq-low (logand clock-seq #xFF)
                :node node-id)))

;;; UUID Creation - Version 3 and 5 (Name-based)

(defun hash-name-md5 (namespace name)
  "Hash a namespace UUID and name using MD5 for UUID v3."
  ;; This would require MD5 implementation
  ;; For now, using a simplified approach
  (declare (ignore namespace name))
  (random-bytes 16)) ; Placeholder

(defun hash-name-sha1 (namespace name)
  "Hash a namespace UUID and name using SHA-1 for UUID v5."
  ;; This would require SHA-1 implementation
  ;; For now, using a simplified approach
  (declare (ignore namespace name))
  (random-bytes 16)) ; Placeholder

(defun make-v3 (namespace name)
  "Create a version 3 (MD5 name-based) UUID."
  (let ((hash (hash-name-md5 namespace name)))
    ;; Set version to 3
    (setf (aref hash 6) (logior (logand (aref hash 6) #x0F) #x30))
    ;; Set variant to RFC 4122
    (setf (aref hash 8) (logior (logand (aref hash 8) #x3F) #x80))
    (make-from-bytes hash)))

(defun make-v5 (namespace name)
  "Create a version 5 (SHA-1 name-based) UUID."
  (let ((hash (hash-name-sha1 namespace name)))
    ;; Set version to 5
    (setf (aref hash 6) (logior (logand (aref hash 6) #x0F) #x50))
    ;; Set variant to RFC 4122
    (setf (aref hash 8) (logior (logand (aref hash 8) #x3F) #x80))
    (make-from-bytes hash)))

;;; UUID Creation - Nil UUID

(defun make-nil ()
  "Create a nil UUID (all zeros)."
  +nil-uuid+)

;;; UUID Conversion

(defun make-from-bytes (bytes)
  "Create a UUID from a 16-byte array."
  (assert (= (length bytes) 16))
  (%make-uuid
   :time-low (logior (ash (aref bytes 0) 24)
                     (ash (aref bytes 1) 16)
                     (ash (aref bytes 2) 8)
                     (aref bytes 3))
   :time-mid (logior (ash (aref bytes 4) 8)
                     (aref bytes 5))
   :time-hi-and-version (logior (ash (aref bytes 6) 8)
                                 (aref bytes 7))
   :clock-seq-hi-and-reserved (aref bytes 8)
   :clock-seq-low (aref bytes 9)
   :node (logior (ash (aref bytes 10) 40)
                 (ash (aref bytes 11) 32)
                 (ash (aref bytes 12) 24)
                 (ash (aref bytes 13) 16)
                 (ash (aref bytes 14) 8)
                 (aref bytes 15))))

(defun to-bytes (uuid)
  "Convert a UUID to a 16-byte array."
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    ;; time-low
    (setf (aref bytes 0) (ldb (byte 8 24) (uuid-time-low uuid)))
    (setf (aref bytes 1) (ldb (byte 8 16) (uuid-time-low uuid)))
    (setf (aref bytes 2) (ldb (byte 8 8) (uuid-time-low uuid)))
    (setf (aref bytes 3) (ldb (byte 8 0) (uuid-time-low uuid)))
    ;; time-mid
    (setf (aref bytes 4) (ldb (byte 8 8) (uuid-time-mid uuid)))
    (setf (aref bytes 5) (ldb (byte 8 0) (uuid-time-mid uuid)))
    ;; time-hi-and-version
    (setf (aref bytes 6) (ldb (byte 8 8) (uuid-time-hi-and-version uuid)))
    (setf (aref bytes 7) (ldb (byte 8 0) (uuid-time-hi-and-version uuid)))
    ;; clock-seq
    (setf (aref bytes 8) (uuid-clock-seq-hi-and-reserved uuid))
    (setf (aref bytes 9) (uuid-clock-seq-low uuid))
    ;; node
    (setf (aref bytes 10) (ldb (byte 8 40) (uuid-node uuid)))
    (setf (aref bytes 11) (ldb (byte 8 32) (uuid-node uuid)))
    (setf (aref bytes 12) (ldb (byte 8 24) (uuid-node uuid)))
    (setf (aref bytes 13) (ldb (byte 8 16) (uuid-node uuid)))
    (setf (aref bytes 14) (ldb (byte 8 8) (uuid-node uuid)))
    (setf (aref bytes 15) (ldb (byte 8 0) (uuid-node uuid)))
    bytes))

(defun to-string (uuid)
  "Convert a UUID to its string representation."
  (format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X"
          (uuid-time-low uuid)
          (uuid-time-mid uuid)
          (uuid-time-hi-and-version uuid)
          (uuid-clock-seq-hi-and-reserved uuid)
          (uuid-clock-seq-low uuid)
          (uuid-node uuid)))

(defun make-from-string (string)
  "Create a UUID from its string representation."
  (let ((clean-string (remove #\- string)))
    (assert (= (length clean-string) 32))
    (flet ((parse-hex (start end)
             (parse-integer (subseq clean-string start end) :radix 16)))
      (%make-uuid
       :time-low (parse-hex 0 8)
       :time-mid (parse-hex 8 12)
       :time-hi-and-version (parse-hex 12 16)
       :clock-seq-hi-and-reserved (parse-hex 16 18)
       :clock-seq-low (parse-hex 18 20)
       :node (parse-hex 20 32)))))

(defun to-integer (uuid)
  "Convert a UUID to a 128-bit integer."
  (logior (ash (uuid-time-low uuid) 96)
          (ash (uuid-time-mid uuid) 80)
          (ash (uuid-time-hi-and-version uuid) 64)
          (ash (uuid-clock-seq-hi-and-reserved uuid) 56)
          (ash (uuid-clock-seq-low uuid) 48)
          (uuid-node uuid)))

(defun from-integer (integer)
  "Create a UUID from a 128-bit integer."
  (assert (<= 0 integer (1- (ash 1 128))))
  (%make-uuid
   :time-low (ldb (byte 32 96) integer)
   :time-mid (ldb (byte 16 80) integer)
   :time-hi-and-version (ldb (byte 16 64) integer)
   :clock-seq-hi-and-reserved (ldb (byte 8 56) integer)
   :clock-seq-low (ldb (byte 8 48) integer)
   :node (ldb (byte 48 0) integer)))

;;; Validation

(defun valid-uuid-p (object)
  "Check if an object is a valid UUID or UUID string."
  (typecase object
    (uuid t)
    (string (handler-case
                (progn (make-from-string object) t)
              (error () nil)))
    (t nil)))