;;;; ALPN (Application-Layer Protocol Negotiation) Support
;;;;
;;;; This file provides ALPN support for TLS connections,
;;;; enabling HTTP/2 negotiation

(defpackage :epsilon.crypto.alpn
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
  (:export
   #:set-alpn-protocols
   #:get-selected-protocol
   #:make-alpn-protos-buffer
   #:+alpn-http2+
   #:+alpn-http11+
   #:+alpn-http10+))

(in-package :epsilon.crypto.alpn)

;;;; ALPN Protocol Constants

(defparameter +alpn-http2+ "h2"
  "HTTP/2 over TLS")

(defparameter +alpn-http11+ "http/1.1"
  "HTTP/1.1")

(defparameter +alpn-http10+ "http/1.0"
  "HTTP/1.0")

;;;; ALPN Buffer Creation

(defun make-alpn-protos-buffer (protocols)
  "Create ALPN protocol buffer in wire format.
   Each protocol is prefixed with its length as a single byte.
   Example: (make-alpn-protos-buffer '(\"h2\" \"http/1.1\"))
   Returns (values buffer length)"
  (let* ((total-len (reduce #'+ protocols 
                            :key (lambda (p) (1+ (length p)))
                            :initial-value 0))
         (buffer (make-array total-len :element-type '(unsigned-byte 8)))
         (pos 0))
    
    (dolist (proto protocols)
      (let ((len (length proto)))
        ;; Write length byte
        (setf (aref buffer pos) len)
        (incf pos)
        ;; Write protocol string
        (loop for char across proto
              do (setf (aref buffer pos) (char-code char))
                 (incf pos))))
    
    (values buffer total-len)))

(defun set-alpn-protocols (ctx-or-ssl protocols &key context-p)
  "Set ALPN protocols for SSL context or connection.
   protocols: list of protocol strings e.g. '(\"h2\" \"http/1.1\")
   context-p: t if ctx-or-ssl is a context, nil if it's a connection"
  (multiple-value-bind (buffer len)
      (make-alpn-protos-buffer protocols)
    
    (sb-alien:with-alien ((proto-buf (sb-alien:array sb-alien:unsigned-char 256)))
      ;; Copy buffer to alien array
      (loop for i from 0 below len
            do (setf (sb-alien:deref proto-buf i) (aref buffer i)))
      
      ;; Call appropriate FFI function
      (let ((result (if context-p
                        (ffi:%ssl-ctx-set-alpn-protos ctx-or-ssl
                                                      (sb-alien:alien-sap proto-buf)
                                                      len)
                      (ffi:%ssl-set-alpn-protos ctx-or-ssl
                                               (sb-alien:alien-sap proto-buf)
                                               len))))
        ;; OpenSSL returns 0 on success for ALPN functions
        (zerop result)))))

(defun get-selected-protocol (ssl)
  "Get the ALPN protocol selected during handshake.
   Returns protocol string or nil if none selected."
  (sb-alien:with-alien ((data-ptr sb-alien:system-area-pointer)
                        (len-ptr sb-alien:unsigned-int))
    (ffi:%ssl-get0-alpn-selected ssl 
                                 (sb-alien:alien-sap (sb-alien:addr data-ptr))
                                 (sb-alien:alien-sap (sb-alien:addr len-ptr)))
    
    (let ((len (sb-alien:deref len-ptr))
          (data (sb-alien:deref data-ptr)))
      (when (and (> len 0)
                 (not (sb-sys:sap= data (sb-sys:int-sap 0))))
        ;; Convert to string
        (let ((result (make-string len)))
          (loop for i from 0 below len
                do (setf (char result i) 
                        (code-char (sb-sys:sap-ref-8 data i))))
          result)))))

;;;; ALPN Selection Callback Support

(defun create-alpn-select-callback (preferred-protocols)
  "Create an ALPN selection callback that prefers certain protocols.
   This is used on the server side to select from client's offered protocols."
  ;; Note: Full callback implementation would require CFFI or similar
  ;; For now, we'll rely on OpenSSL's default selection
  (lambda (ssl client-protos client-protos-len)
    (declare (ignore ssl client-protos client-protos-len))
    ;; Default behavior - OpenSSL will select first mutually supported protocol
    preferred-protocols))