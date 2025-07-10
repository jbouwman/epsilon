;;;; MsgPack Shim for Core Module
;;;;
;;;; This file provides backward compatibility for msgpack support in core
;;;; by delegating to the separate msgpack module when available.

(defpackage :epsilon.lib.msgpack
  (:use :cl)
  (:export #:encode
           #:decode
           #:+timestamp-type+))

(in-package :epsilon.lib.msgpack)

(defun msgpack-available-p ()
  "Check if the msgpack module is loaded and available."
  (find-package :epsilon.lib.msgpack.impl))

(defun ensure-msgpack-loaded ()
  "Ensure the msgpack module is loaded, error if not available."
  (unless (msgpack-available-p)
    (error "MessagePack support requires the epsilon.msgpack module to be loaded")))

(defun encode (object)
  "Encode an object to MessagePack format."
  (ensure-msgpack-loaded)
  (funcall (find-symbol "ENCODE" :epsilon.lib.msgpack.impl) object))

(defun decode (bytes)
  "Decode MessagePack bytes to an object."
  (ensure-msgpack-loaded)
  (funcall (find-symbol "DECODE" :epsilon.lib.msgpack.impl) bytes))

(defun get-timestamp-type ()
  "Get the timestamp extension type constant."
  (ensure-msgpack-loaded)
  (symbol-value (find-symbol "+TIMESTAMP-TYPE+" :epsilon.lib.msgpack.impl)))

;; Define constant that delegates to the real one
(define-symbol-macro +timestamp-type+ (get-timestamp-type))

;;; Export a feature to indicate msgpack shim is loaded
(pushnew :epsilon-msgpack-shim *features*)