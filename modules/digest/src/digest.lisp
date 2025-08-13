(defpackage #:epsilon.digest
  (:use #:cl)
  (:local-nicknames
   (:sha-2 :epsilon.digest.sha-2)
   (:crc-32 :epsilon.digest.crc-32)
   (:generic :epsilon.digest.generic))
  (:export
   ;; One-shot hash functions (Rust-style)
   #:sha256
   #:sha1
   #:crc32
   
   ;; Streaming hash interface (Go-style)
   #:make-sha256
   #:make-sha1  
   #:make-crc32
   #:update
   #:finalize
   #:reset
   
   ;; Utility
   #:bytes-to-hex
   
   ;; Legacy compatibility
   #:sha1-digest))

(in-package #:epsilon.digest)

;;; Utility functions

(defun normalize-input (data)
  "Convert various input types to (simple-array (unsigned-byte 8) (*))"
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    (string 
     (map '(simple-array (unsigned-byte 8) (*)) #'char-code data))
    (vector 
     (if (every (lambda (x) (typep x '(unsigned-byte 8))) data)
         (coerce data '(simple-array (unsigned-byte 8) (*)))
         (error "Vector contains non-byte values")))
    (list 
     (if (every (lambda (x) (typep x '(unsigned-byte 8))) data)
         (coerce data '(simple-array (unsigned-byte 8) (*)))
         (error "List contains non-byte values")))))

(defun bytes-to-hex (bytes)
  "Convert byte array to hex string"
  (format nil "~{~2,'0X~}" (coerce bytes 'list)))

;;; One-shot hash functions (Rust-style convenience functions)

(defun sha256 (data)
  "Compute SHA-256 hash of data. Data can be string, vector, or byte array."
  (let ((digest (sha-2:make-sha256-digest))
        (bytes (normalize-input data)))
    (generic:update-digest digest bytes :start 0 :end (length bytes))
    (generic:produce-digest digest)))

(defun sha1 (data)
  "Compute SHA-1 hash of data. Currently a stub implementation."
  (let ((bytes (normalize-input data)))
    ;; Simple stub implementation - should be replaced with real SHA-1
    (let ((result (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
      (when (> (length bytes) 0)
        (loop for i from 0 below 20
              do (setf (aref result i) 
                       (mod (loop for j from i below (length bytes) by 20
                                  sum (aref bytes j))
                            256))))
      result)))

(defun crc32 (data)
  "Compute CRC-32 checksum of data, return as integer."
  (let ((bytes (normalize-input data)))
    (crc-32:crc32 bytes)))

;;; Streaming hash interface (Go-style stateful interface)

(defstruct hasher
  digest
  algorithm)

(defun make-sha256 ()
  "Create a new SHA-256 hasher"
  (make-hasher :digest (sha-2:make-sha256-digest) :algorithm :sha256))

(defun make-sha1 ()
  "Create a new SHA-1 hasher"
  ;; For now, return nil since we don't have a real SHA-1 implementation
  (make-hasher :digest nil :algorithm :sha1))

(defun make-crc32 ()
  "Create a new CRC-32 hasher"
  (make-hasher :digest (crc-32:make-crc32-digest) :algorithm :crc32))

(defun update (hasher data)
  "Add data to the hash. Data can be string, vector, or byte array."
  (let ((bytes (normalize-input data)))
    (case (hasher-algorithm hasher)
      ((:sha256 :crc32)
       (generic:update-digest (hasher-digest hasher) bytes 
                             :start 0 :end (length bytes)))
      (:sha1
       ;; SHA-1 not implemented for streaming yet
       (error "SHA-1 streaming not implemented")))
    nil))

(defun finalize (hasher)
  "Finalize the hash and return the result"
  (case (hasher-algorithm hasher)
    (:sha256 (generic:produce-digest (hasher-digest hasher)))
    (:crc32 (let ((result (generic:produce-digest (hasher-digest hasher))))
              ;; Convert 4-byte result to integer
              (+ (ash (aref result 0) 24)
                 (ash (aref result 1) 16)
                 (ash (aref result 2) 8)
                 (aref result 3))))
    (:sha1 (error "SHA-1 not implemented"))))

(defun reset (hasher)
  "Reset the hasher to initial state"
  (case (hasher-algorithm hasher)
    (:sha256 (setf (hasher-digest hasher) (sha-2:make-sha256-digest)))
    (:crc32 (setf (hasher-digest hasher) (crc-32:make-crc32-digest)))
    (:sha1 (setf (hasher-digest hasher) nil)))
  nil)

;; Legacy compatibility function
(defun sha1-digest (octets)
  "Legacy SHA-1 function. Use (sha1 data) instead."
  (sha1 octets))