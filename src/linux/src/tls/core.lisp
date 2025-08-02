(defpackage epsilon.tls
  (:use cl)
  (:export
   ;; TLS Context
   make-tls-context
   tls-context-p
   load-cert-file
   load-key-file
   set-verify-mode
   
   ;; TLS Connection
   tls-connect
   tls-accept
   tls-close
   with-tls-connection
   
   ;; TLS I/O
   tls-read
   tls-write
   tls-stream
   
   ;; TLS State
   tls-connected-p
   tls-get-peer-certificate
   tls-get-cipher
   
   ;; Constants
   +tls-verify-none+
   +tls-verify-peer+
   +tls-verify-fail-if-no-peer-cert+
   +tls-verify-client-once+))

(in-package epsilon.tls)

;;; Stub implementation - TODO: Fix epsilon.foreign dependency

(defconstant +tls-verify-none+ 0)
(defconstant +tls-verify-peer+ 1)
(defconstant +tls-verify-fail-if-no-peer-cert+ 2)
(defconstant +tls-verify-client-once+ 4)

(defclass tls-context ()
  ((server-p :initarg :server-p :reader tls-context-server-p)))

(defun tls-context-p (obj)
  (typep obj 'tls-context))

(defun make-tls-context (&key server-p)
  (make-instance 'tls-context :server-p server-p))

(defun load-cert-file (context cert-file)
  (declare (ignore context cert-file))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun load-key-file (context key-file)
  (declare (ignore context key-file))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun set-verify-mode (context mode)
  (declare (ignore context mode))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defclass tls-connection ()
  ((context :initarg :context)
   (socket :initarg :socket)))

(defun tls-connect (context socket &key hostname)
  (declare (ignore context socket hostname))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-accept (context socket)
  (declare (ignore context socket))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-close (connection)
  (declare (ignore connection))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defmacro with-tls-connection ((var context socket &key hostname) &body body)
  (declare (ignore var context socket hostname body))
  `(error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-read (connection buffer &key (start 0) end)
  (declare (ignore connection buffer start end))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-write (connection buffer &key (start 0) end)
  (declare (ignore connection buffer start end))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-stream (connection)
  (declare (ignore connection))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-connected-p (connection)
  (declare (ignore connection))
  nil)

(defun tls-get-peer-certificate (connection)
  (declare (ignore connection))
  (error "TLS not implemented - epsilon.foreign dependency missing"))

(defun tls-get-cipher (connection)
  (declare (ignore connection))
  (error "TLS not implemented - epsilon.foreign dependency missing"))