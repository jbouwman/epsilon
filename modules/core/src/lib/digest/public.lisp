(defpackage #:epsilon.digest
  (:use
   #:cl)
  (:local-nicknames
   (:generic :epsilon.digest.generic)
   (:sha-2 :epsilon.digest.sha-2)
   (:crc-32 :epsilon.digest.crc-32))
  (:export
   #:make-digest
   #:digest-stream
   #:digest-vector
   #:get-digest
   #:sha1-digest
   #:sha256
   #:crc32
   #:crc32-sequence))

(in-package #:epsilon.digest)

(defun make-digest (name)
  (ecase name
    (:sha-256 (sha-2:make-sha256-digest))
    (:crc-32 (crc-32:make-crc32-digest))))

(defun digest-stream (digest stream)
  (generic:update-digest-from-stream digest stream))

(defun digest-vector (digest vector)
  (generic:update-digest-from-vector digest vector 0 (length vector)))

(defun get-digest (digest)
  (generic:produce-digest digest))

(defun sha1-digest (octets)
  "Compute SHA-1 digest of octets. Stub implementation for WebSocket support."
  ;; SHA-1 produces a 160-bit (20 byte) hash
  ;; This is a stub that returns a fixed 20-byte array for now
  ;; In production, this should use a proper SHA-1 implementation
  (let ((result (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Simple stub: just XOR the input bytes and repeat the pattern
    (when (> (length octets) 0)
      (loop for i from 0 below 20
            do (setf (aref result i) 
                     (mod (loop for j from i below (length octets) by 20
                                sum (aref octets j))
                          256))))
    result))

(defun sha256 (octets)
  "Compute SHA-256 digest of octets"
  (let ((digest (make-digest :sha-256)))
    ;; Convert input to correct simple array type if needed
    (let ((simple-octets (if (typep octets '(simple-array (unsigned-byte 8) (*)))
                             octets
                             (coerce octets '(simple-array (unsigned-byte 8) (*))))))
      (generic:update-digest digest simple-octets :start 0 :end (length simple-octets)))
    (get-digest digest)))

;; CRC-32 convenience functions
(defun crc32 (octets)
  "Compute CRC-32 checksum of octets, return as integer"
  (crc-32:crc32 octets))

(defun crc32-sequence (sequence)
  "Compute CRC-32 checksum of sequence, return as integer"
  (crc-32:crc32-sequence sequence))
