;;;; Stub implementations for WebSocket dependencies
;;;; This file provides minimal implementations to allow WebSocket to compile
;;;; In production, these should be replaced with proper implementations

;;; Base64 stub package
(defpackage #:epsilon.base64
  (:use #:cl)
  (:export #:octets-to-base64))

(in-package #:epsilon.base64)

(defun octets-to-base64 (octets)
  "Stub base64 encoder"
  ;; Simple stub that converts to hex for now
  (format nil "~{~2,'0X~}" (coerce octets 'list)))

;;; UUID stub package  
(defpackage #:epsilon.uuid
  (:use #:cl)
  (:export #:make-uuid-string))

(in-package #:epsilon.uuid)

(defun make-uuid-string ()
  "Generate a stub UUID string"
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (random #xFFFFFFFF)
          (random #xFFFF)
          (logior #x4000 (random #x0FFF))  ; Version 4
          (logior #x8000 (random #x3FFF))  ; Variant bits
          (random #xFFFFFFFFFFFF)))