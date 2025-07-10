;;;; MsgPack Binary Shim for Core Module
;;;;
;;;; This file provides backward compatibility for msgpack-binary support in core
;;;; by delegating to the separate msgpack module when available.

(defpackage :epsilon.lib.msgpack.binary
  (:use :cl)
  (:export #:msgpack-header
           #:msgpack-string
           #:msgpack-binary
           #:msgpack-array-header
           #:msgpack-map-header
           #:msgpack-ext
           #:msgpack-timestamp
           #:encode-with-binary-structs
           #:decode-with-binary-structs
           #:detect-msgpack-format))

(in-package :epsilon.lib.msgpack.binary)

(defun msgpack-binary-available-p ()
  "Check if the msgpack binary module is loaded and available."
  (find-package :epsilon.lib.msgpack.binary.impl))

(defun ensure-msgpack-binary-loaded ()
  "Ensure the msgpack binary module is loaded, error if not available."
  (unless (msgpack-binary-available-p)
    (error "MessagePack binary support requires the epsilon.msgpack module to be loaded")))

;; Re-export types by creating wrapper functions
(defun msgpack-header (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-HEADER" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-string (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-STRING" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-binary (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-BINARY" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-array-header (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-ARRAY-HEADER" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-map-header (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-MAP-HEADER" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-ext (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-EXT" :epsilon.lib.msgpack.binary.impl) args))

(defun msgpack-timestamp (&rest args)
  (ensure-msgpack-binary-loaded)
  (apply (find-symbol "MSGPACK-TIMESTAMP" :epsilon.lib.msgpack.binary.impl) args))

(defun encode-with-binary-structs (object)
  "Encode using binary structures."
  (ensure-msgpack-binary-loaded)
  (funcall (find-symbol "ENCODE-WITH-BINARY-STRUCTS" :epsilon.lib.msgpack.binary.impl) object))

(defun decode-with-binary-structs (bytes)
  "Decode using binary structures."
  (ensure-msgpack-binary-loaded)
  (funcall (find-symbol "DECODE-WITH-BINARY-STRUCTS" :epsilon.lib.msgpack.binary.impl) bytes))

(defun detect-msgpack-format (bytes)
  "Detect MessagePack format."
  (ensure-msgpack-binary-loaded)
  (funcall (find-symbol "DETECT-MSGPACK-FORMAT" :epsilon.lib.msgpack.binary.impl) bytes))

;;; Export a feature to indicate msgpack-binary shim is loaded
(pushnew :epsilon-msgpack-binary-shim *features*)