(defpackage #:epsilon.net.tls
  (:use #:cl
        #:epsilon.lib.binding
        #:epsilon.lib.char
        #:epsilon.lib.stream
        #:epsilon.lib.list
        #:epsilon.lib.symbol
        #:epsilon.lib.type
        #:epsilon.sys.sync.lock
        #:epsilon.sys.sync.thread)
  (:local-nicknames (#:ffi #:epsilon.sys.ffi))
  (:export #:ensure-initialized
           #:make-context
           #:+ssl-verify-none+
           #:+ssl-verify-peer+
           #:with-global-context
           #:use-certificate-chain-file
           #:use-private-key-file
           #:make-ssl-client-stream
           #:make-ssl-server-stream))

(in-package :epsilon.net.tls)

;;; Global state

(defvar *ssl-global-context* nil)
(defvar *ssl-global-method* nil)

(defun ssl-initialized-p ()
  (and *ssl-global-context* *ssl-global-method*))

(defvar *tmp-rsa-key-512* nil)
(defvar *tmp-rsa-key-1024* nil)
(defvar *tmp-rsa-key-2048* nil)

(defvar *ssl-check-verify-p* :unspecified
  "DEPRECATED.
Use the (MAKE-SSL-CLIENT-STREAM .. :VERIFY ?) to enable/disable verification.
MAKE-CONTEXT also allows to enab/disable verification.")

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun detect-macos-custom-openssl-installations ()
  (dolist (dir-feature '(("/opt/local/lib/" :tls-macports-found)
                         ("/usr/local/opt/openssl/lib/" :tls-homebrew-found)
                         ("/opt/homebrew/opt/openssl/lib/" :tls-homebrew-arm64-found)
                         ("/usr/local/lib/" :tls-personalized-install-found)))
    (destructuring-bind (dir feature) dir-feature
      (if (and (epsilon.sys.fs:file-p (concatenate 'string dir "libssl.dylib"))
               (epsilon.sys.fs:file-p (concatenate 'string dir "libcrypto.dylib")))
          (pushnew feature *features*)
          (setf *features* (remove feature *features*))))))

(defun detect-custom-openssl-installations-if-macos ()
  (when (member :darwin *features*)
    (detect-macos-custom-openssl-installations)))

(detect-custom-openssl-installations-if-macos)

(ffi:define-foreign-library libcrypto
  (:windows (:or #+(and windows x86-64) "libcrypto-3-x64.dll"
                 #+(and windows x86) "libcrypto-3.dll"
                 #+(and windows x86-64) "libcrypto-1_1-x64.dll"
                 #+(and windows x86) "libcrypto-1_1.dll"
                 "libeay32.dll"))
  (:openbsd "libcrypto.so")
  
  ((:and :darwin :tls-macports-found) "/opt/local/lib/libcrypto.dylib")
  ((:and :darwin :arm64 :tls-homebrew-arm64-found) "/opt/homebrew/opt/openssl/lib/libcrypto.dylib")
  ((:and :darwin
         (:not :arm64)
         :tls-homebrew-found) "/usr/local/opt/openssl/lib/libcrypto.dylib")
  ((:and :darwin :tls-personalized-install-found) "/usr/local/lib/libcrypto.dylib")
  (:darwin (:or
            "/usr/lib/libcrypto.46.dylib"
            "/usr/lib/libcrypto.44.dylib"
            "/usr/lib/libcrypto.42.dylib"
            "/usr/lib/libcrypto.41.dylib"
            "/usr/lib/libcrypto.35.dylib"
            "libcrypto.dylib"
            "/usr/lib/libcrypto.dylib"))
  (:unix (:or "libcrypto.so.1.1"
              "libcrypto.so.1.0.0"
              "libcrypto.so.3"
              "libcrypto.so")))

(ffi:define-foreign-library libssl
  (:windows (:or #+(and windows x86-64) "libssl-3-x64.dll"
                 #+(and windows x86) "libssl-3.dll"
                 #+(and windows x86-64) "libssl-1_1-x64.dll"
                 #+(and windows x86) "libssl-1_1.dll"
                 "libssl32.dll"
                 "ssleay32.dll"))

  ((:and :darwin :tls-macports-found) "/opt/local/lib/libssl.dylib")
  ((:and :darwin :x86-64 :tls-homebrew-found) "/usr/local/opt/openssl/lib/libssl.dylib")
  ((:and :darwin :arm64 :tls-homebrew-arm64-found) "/opt/homebrew/opt/openssl/lib/libssl.dylib")
  ((:and :darwin :tls-personalized-install-found) "/usr/local/lib/libssl.dylib")
  (:darwin (:or ;; System-provided libraries, with version in the file name.
            "/usr/lib/libssl.48.dylib"
            "/usr/lib/libssl.46.dylib"
            "/usr/lib/libssl.44.dylib"
            "/usr/lib/libssl.43.dylib"
            "/usr/lib/libssl.35.dylib"
            "libssl.dylib"
            "/usr/lib/libssl.dylib"))
  (:openbsd "libssl.so")
  (:unix (:or "libssl.so.1.1"
              "libssl.so.1.0.2m"
              "libssl.so.1.0.2k"
              "libssl.so.1.0.2"
              "libssl.so.1.0.1l"
              "libssl.so.1.0.1j"
              "libssl.so.1.0.1f"
              "libssl.so.1.0.1e"
              "libssl.so.1.0.1"
              "libssl.so.1.0.0q"
              "libssl.so.1.0.0"
              "libssl.so.0.9.8ze"
              "libssl.so.0.9.8"
              "libssl.so.10"
              "libssl.so.4"
              "libssl.so.3"
              "libssl.so"))
  (t (:default "libssl3")))

(ffi:use-foreign-library libcrypto)
(ffi:use-foreign-library libssl)

)

;;; Condition hierarchy
;;;

(define-condition tls-error (error)
  ())

(define-condition ssl-error (tls-error)
  (
   ;; Stores list of error codes
   ;; (as returned by the READ-SSL-ERROR-QUEUE function)
   (queue :initform nil :initarg :queue :reader ssl-error-queue)

   ;; The queue formatted using ERR_print_errors.
   ;; If this value is present, ignore the QUEUE field (which will
   ;; be empty, most likely, because ERR_print_errors cleans the queue).
   ;;
   ;; That's the preferred way, becuase it includes more info
   ;; than the printing we implemented in Lisp. In particualr, in includes
   ;; the optional string added by ERR_add_error_data, which
   ;; we use to provide error details of unexpected lisp errors
   ;; in Lisp BIO. Consider migrating all the code to PRINTED-QUEUE,
   ;; for example, when working on
   ;; https://github.com/cl-plus-ssl/cl-plus-ssl/issues/75.
   (printed-queue :initform nil
                  :initarg :printed-queue
                  :reader printed-queue)))


(defun read-ssl-error-queue ()
  (loop
     :for error-code = (err-get-error)
     :until (zerop error-code)
     :collect error-code))

(defun format-ssl-error-queue (stream-designator queue-designator)
  "STREAM-DESIGNATOR is the same as CL:FORMAT accepts: T, NIL, or a stream.
QUEUE-DESIGNATOR is either a list of error codes (as returned
by READ-SSL-ERROR-QUEUE) or an SSL-ERROR condition."
  (flet ((body (stream)

           ;; If printed-queue is present, just use it
           (when (and (typep queue-designator 'ssl-error)
                      (printed-queue queue-designator))
             (format stream "ERR_print_errors(): ~A"
                     (printed-queue queue-designator))
             (return-from body))

           (let ((queue (etypecase queue-designator
                          (ssl-error (ssl-error-queue queue-designator))
                          (list queue-designator))))
             (format stream "SSL error queue")
             (if queue
                 (progn
                   (format stream ":~%")
                   (loop
                      :for error-code :in queue
                      :do (format stream "~a~%" (err-error-string error-code (epsilon.sys.ffi:null-pointer)))))
                 (format stream " is empty.")))))
    (case stream-designator
      ((t) (body *standard-output*))
      ((nil) (let ((s (make-string-output-stream :element-type 'character)))
               (unwind-protect
                    (body s)
                 (close s))
               (get-output-stream-string s)))
      (otherwise (body stream-designator)))))


(define-condition ssl-error/handle (ssl-error)
  (;; Misnamed, better to be called CODE :READER SSL-ERROR-CODE
   ;; becuase OpenSSL docs use the term RET for return
   ;; values of IO calls like SSL_Read, etc, while
   ;; here we store explanation of such failures
   ;; as returned by SSL_get_error called
   ;; after the failure.
   ;; Unfortunately, SSL-ERROR-CODE is already used
   ;; by SSL-ERROR-VERIFY condition class below
   ;; for return values of SSL_get_verify_result,
   ;; and that's already exported from tls package.
   ;; Using the same generic function for two different
   ;; types of error codes is not the best approach.
   ;; Keeping it as is for now.
   ;; Or maybe the intention was for SSL-SIGNAL-ERROR
   ;; to really pass RET here (the IO call return value)?
   ;; Unlikely, RET is not very useful.
   (ret :initarg :ret
        :reader ssl-error-ret
        :documentation "The error code returned by SSL_get_error. " )
   (handle :initarg :handle
           :reader ssl-error-handle))
  (:documentation "Base condition for lisp wrappers of SSL_get_error return values.")
  (:report (lambda (condition stream)
             (format stream "Unspecified error ~A on handle ~A. "
                     (ssl-error-ret condition)
                     (ssl-error-handle condition))
             (format-ssl-error-queue stream condition))))

(define-condition ssl-error-initialize (ssl-error)
  ((reason  :initarg :reason
            :reader ssl-error-reason))
  (:report (lambda (condition stream)
             (format stream "SSL initialization error: ~A. "
                     (ssl-error-reason condition))
             (format-ssl-error-queue stream condition))))

(define-condition ssl-error-want-something (ssl-error/handle)
  ())

;;;SSL_ERROR_NONE
(define-condition ssl-error-none (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL I/O operation completed. This result code is returned if and
    only if ret > 0.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A completed (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_ZERO_RETURN
(define-condition ssl-error-zero-return (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL connection has been closed. If the protocol version is SSL 3.0
    or TLS 1.0, this result code is returned only if a closure alert has
    occurred in the protocol, i.e. if the connection has been closed cleanly.
    Note that in this case SSL_ERROR_ZERO_RETURN
    does not necessarily indicate that the underlying transport has been
    closed.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL connection on handle ~A has been closed (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_WANT_READ
(define-condition ssl-error-want-read (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a READ (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_WANT_WRITE
(define-condition ssl-error-want-write (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a WRITE (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_WANT_CONNECT
(define-condition ssl-error-want-connect (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. The underlying BIO was not connected yet to the peer
    and the call would block in connect()/accept(). The SSL
    function should be called again when the connection is established. These
    messages can only appear with a BIO_s_connect() or
    BIO_s_accept() BIO, respectively. In order to find out, when
    the connection has been successfully established, on many platforms
    select() or poll() for writing on the socket file
    descriptor can be used.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a connect first (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_WANT_X509_LOOKUP
(define-condition ssl-error-want-x509-lookup (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete because an application callback set by
    SSL_CTX_set_client_cert_cb() has asked to be called again. The
    TLS/SSL I/O function should be called again later. Details depend on the
    application.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: An application callback wants to be called again (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_SYSCALL
(define-condition ssl-error-syscall (ssl-error/handle)
  ((syscall :initarg :syscall))
  (:documentation
   "Some I/O error occurred. The OpenSSL error queue may contain more
    information on the error. If the error queue is empty (i.e. ERR_get_error() returns 0),
    ret can be used to find out more about the error: If ret == 0, an EOF was observed that
    violates the protocol. If ret == -1, the underlying BIO reported an I/O error (for socket
    I/O on Unix systems, consult errno for details).")
  (:report (lambda (condition stream)
             (if (zerop (length (ssl-error-queue condition)))
                 (case (ssl-error-ret condition)
                   (0 (format stream "An I/O error occurred: An unexpected EOF was observed on handle ~A (SSL_get_error: ~A). "
                              (ssl-error-handle condition)
                              (ssl-error-ret condition)))
                   (-1 (format stream "An I/O error occurred in the underlying BIO (SSL_get_error: ~A). "
                               (ssl-error-ret condition)))
                   (otherwise (format stream "An I/O error occurred: undocumented reason (SSL_get_error: ~A). "
                                      (ssl-error-ret condition))))
                 (format stream "An UNKNOWN I/O error occurred in the underlying BIO (SSL_get_error: ~A). "
                         (ssl-error-ret condition)))
             (format-ssl-error-queue stream condition))))

;; SSL_ERROR_SSL
(define-condition ssl-error-ssl (ssl-error/handle)
  ((;; When SSL_Connect or SSL_Accept fail due to
    ;; the SSL_VERIFY_PEER flag and bad peer certificate,
    ;; the error queue simply says "certificate verify failed"
    ;; and the user needs to call SSL_get_verify_result
    ;; to find our the exact verification error (expired cert,
    ;; can't get issuer cert locally, etc).
    ;;
    ;; To facilitate debugging and logging, we
    ;; automaticall store the SSL_get_verify_result
    ;; in this slot and use it in the printed
    ;; representation of the condition.
    ;;
    ;; Ideally, we should only collect the verification
    ;; error if the error queue includes reason code
    ;; SSL_R_CERTIFICATE_VERIFY_FAILED for library
    ;; code ERR_LIB_SSL, but this would require
    ;; us to implement the logic of OpenSSL macros
    ;; ERR_raise, ERR_PACK, taking OpenSSL version into
    ;; account - those macros produce different number
    ;; for that reason code in different OpenSSL versions.
    ;; Here are snippets of printed error queues, starting
    ;; with error code:
    ;;   openssl-0.9.8zh
    ;;     14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed:s3_clnt.c:973:
    ;;   openssl-1.1.1p
    ;;     1416F086:SSL routines:tls_process_server_certificate:certificate verify failed:ssl/statem/statem_clnt.c:1919:
    ;;   openssl-3.0.4
    ;;     0A000086:SSL routines:tls_post_process_server_certificate:certificate verify failed:ssl/statem/statem_clnt.c:1887:
    ;; Therefore we simply collect the verification
    ;; error if it is present at the time of SSL_Connect
    ;; or SSL_Accept failure - see how the
    ;; collecting-verify-error macro is used.
    ;; This approach, however, will not collect verification
    ;; error if it happens not on the initial handshake,
    ;; but during session renegotiation.
    verify-error :type (or null string)
                 :initform nil
                 :accessor ssl-error-ssl-verify-error))
  (:documentation
   "A failure in the SSL library occurred, usually a protocol error. The
    OpenSSL error queue contains more information on the error.")
  (:report (lambda (condition stream)
             (format stream
                     "A failure in the SSL library occurred on handle ~A (SSL_get_error: ~A). "
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (format-ssl-error-queue stream condition)
             (when (ssl-error-ssl-verify-error condition)
               (format stream
                       "~A"
                       (ssl-error-ssl-verify-error condition))))))

(defparameter *late-bound-foreign-function-pointers*
  (make-hash-table :test 'equal))

(defmacro defcfun-late-bound (name-and-options &body body)
  (assert (not (eq (lastcar body)
                   '&rest))
          (body)
          "The BODY format is implemented in a limited way
comparing to FFI:DEFCFUN - we don't support the &REST which specifies vararg
functions. Feel free to implement the support if you have a use case.")
  (assert (and (>= (length name-and-options) 2)
               (stringp (first name-and-options))
               (symbolp (second name-and-options)))
          (name-and-options)
          "Unsupported NAME-AND-OPTIONS format: ~S.
\(Of all the NAME-AND-OPTIONS variants allowed by FFI:DEFCFUN we have only
implemented support for (FOREIGN-NAME LISP-NAME ...) where FOREIGN-NAME is a
STRING and LISP-NAME is a SYMBOL. Fell free to implement support the remaining
variants if you have use cases for them.)"
          name-and-options)

  (let ((foreign-name-str (first name-and-options))
        (lisp-name (second name-and-options))
        (docstring (when (stringp (car body)) (pop body)))
        (return-type (first body))
        (arg-names (mapcar #'first (rest body)))
        (arg-types (mapcar #'second (rest body)))
        (library (getf (cddr name-and-options) :library))
        (convention (getf (cddr name-and-options) :convention))
        (ptr-var (gensym (string 'ptr))))
    `(progn
       (setf (gethash ,foreign-name-str *late-bound-foreign-function-pointers*)
             (or (ffi:foreign-symbol-pointer ,foreign-name-str
                                              ,@(when library `(:library ',library)))
                 'foreign-symbol-not-found))
       (defun ,lisp-name (,@arg-names)
         ,@(when docstring (list docstring))
         (let ((,ptr-var (gethash ,foreign-name-str *late-bound-foreign-function-pointers*)))
           (when (null ,ptr-var)
             (error "Unexpacted state, no value in *late-bound-foreign-function-pointers* for ~A"
                    ,foreign-name-str))
           (when (eq ,ptr-var 'foreign-symbol-not-found)
             (error "The current version of OpenSSL libcrypto doesn't provide ~A"
                    ,foreign-name-str))
           (ffi:foreign-funcall-pointer ,ptr-var
                                         ,(when convention (list convention))
                                         ,@(mapcan #'list arg-types arg-names)
                                         ,return-type))))))

(defmacro defcfun-versioned ((&key since vanished) name-and-options &body body)
  (if (and (or since vanished)
           (member :cmucl *features*))
      `(defcfun-late-bound ,name-and-options ,@body)
      `(ffi:defcfun ,name-and-options ,@body)))


;;; Code for checking that we got the correct foreign symbols right.
;;; Implemented only for LispWorks for now.
(defvar *tls-ssl-foreign-function-names* nil)
(defvar *tls-crypto-foreign-function-names* nil)

(defmacro define-ssl-function-ex ((&key since vanished) name-and-options &body body)
  `(progn
     ;; debugging
     ,@(unless (or since vanished)
         `((pushnew ,(car name-and-options)
                    *tls-ssl-foreign-function-names*
                    :test 'equal)))
     (defcfun-versioned (:since ,since :vanished ,vanished)
         ,(append name-and-options '(:library libssl))
       ,@body)))

(defmacro define-ssl-function (name-and-options &body body)
  `(define-ssl-function-ex () ,name-and-options ,@body))

(defmacro define-crypto-function-ex ((&key since vanished) name-and-options &body body)
  `(progn
     ;; debugging
     ,@(unless (or since vanished)
         `(( pushnew ,(car name-and-options)
                     *tls-crypto-foreign-function-names*
                     :test 'equal)))
     (defcfun-versioned (:since ,since :vanished ,vanished)
         ,(append name-and-options
                  #-tls-foreign-libs-already-loaded '(:library libcrypto))
       ,@body)))

(defmacro define-crypto-function (name-and-options &body body)
  `(define-crypto-function-ex () ,name-and-options ,@body))


;;; Constants
;;;
(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defconstant +SSL-CTRL-OPTIONS+ 32)
(defconstant +SSL_CTRL_SET_SESS_CACHE_MODE+ 44)
(defconstant +SSL_CTRL_MODE+ 33)

(defconstant +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+ 2)

(defconstant +RSA_F4+ #x10001)

(defconstant +SSL-SESS-CACHE-OFF+ #x0000
  "No session caching for client or server takes place.")
(defconstant +SSL-SESS-CACHE-CLIENT+ #x0001
  "Client sessions are added to the session cache.
As there is no reliable way for the OpenSSL library to know whether a session should be reused
or which session to choose (due to the abstract BIO layer the SSL engine does not have details
about the connection), the application must select the session to be reused by using the
SSL-SET-SESSION function. This option is not activated by default.")
(defconstant +SSL-SESS-CACHE-SERVER+ #x0002
  "Server sessions are added to the session cache.
When a client proposes a session to be reused, the server looks for the corresponding session
in (first) the internal session cache (unless +SSL-SESS-CACHE-NO-INTERNAL-LOOKUP+ is set), then
(second) in the external cache if available. If the session is found, the server will try to
reuse the session. This is the default.")
(defconstant +SSL-SESS-CACHE-BOTH+ (logior +SSL-SESS-CACHE-CLIENT+ +SSL-SESS-CACHE-SERVER+)
  "Enable both +SSL-SESS-CACHE-CLIENT+ and +SSL-SESS-CACHE-SERVER+ at the same time.")
(defconstant +SSL-SESS-CACHE-NO-AUTO-CLEAR+ #x0080
  "Normally the session cache is checked for expired sessions every 255 connections using the
SSL-CTX-FLUSH-SESSIONS function. Since this may lead to a delay which cannot be controlled,
the automatic flushing may be disabled and SSL-CTX-FLUSH-SESSIONS can be called explicitly
by the application.")
(defconstant +SSL-SESS-CACHE-NO-INTERNAL-LOOKUP+ #x0100
  "By setting this flag, session-resume operations in an SSL/TLS server will not automatically
look up sessions in the internal cache, even if sessions are automatically stored there.
If external session caching callbacks are in use, this flag guarantees that all lookups are
directed to the external cache. As automatic lookup only applies for SSL/TLS servers, the flag
has no effect on clients.")
(defconstant +SSL-SESS-CACHE-NO-INTERNAL-STORE+ #x0200
  "Depending on the presence of +SSL-SESS-CACHE-CLIENT+ and/or +SSL-SESS-CACHE-SERVER+, sessions
negotiated in an SSL/TLS handshake may be cached for possible reuse. Normally a new session is
added to the internal cache as well as any external session caching (callback) that is configured
for the SSL_CTX. This flag will prevent sessions being stored in the internal cache (though the
application can add them manually using SSL-CTX-ADD-SESSION). Note: in any SSL/TLS servers where
external caching is configured, any successful session lookups in the external cache (ie. for
session-resume requests) would normally be copied into the local cache before processing continues
- this flag prevents these additions to the internal cache as well.")
(defconstant +SSL-SESS-CACHE-NO-INTERNAL+ (logior +SSL-SESS-CACHE-NO-INTERNAL-LOOKUP+ +SSL-SESS-CACHE-NO-INTERNAL-STORE+)
  "Enable both +SSL-SESS-CACHE-NO-INTERNAL-LOOKUP+ and +SSL-SESS-CACHE-NO-INTERNAL-STORE+ at the same time.")

(defconstant +SSL-VERIFY-NONE+ #x00)
(defconstant +SSL-VERIFY-PEER+ #x01)
(defconstant +SSL-VERIFY-FAIL-IF-NO-PEER-CERT+ #x02)
(defconstant +SSL-VERIFY-CLIENT-ONCE+ #x04)

(defconstant +x509-v-ok+ 0)

(defconstant +SSL-OP-ALL+ #x80000BFF)

(defconstant +SSL-OP-IGNORE-UNEXPECTED-EOF+ #b10000000)

(defconstant +SSL-OP-NO-SSLv2+   #x01000000)
(defconstant +SSL-OP-NO-SSLv3+   #x02000000)
(defconstant +SSL-OP-NO-TLSv1+   #x04000000)
(defconstant +SSL-OP-NO-TLSv1-2+ #x08000000)
(defconstant +SSL-OP-NO-TLSv1-1+ #x10000000)

(defconstant +SSL-CTRL-SET-MIN-PROTO-VERSION+ 123)
(defconstant +SSL-CTRL-SET-MAX-PROTO-VERSION+ 124)

(defconstant +SSL3-VERSION+ #x0300)
(defconstant +TLS1-VERSION+ #x0301)
(defconstant +TLS1-1-VERSION+ #x0302)
(defconstant +TLS1-2-VERSION+ #x0303)
(defconstant +TLS1-3-VERSION+ #x0304)
(defconstant +DTLS1-VERSION+ #xFEFF)
(defconstant +DTLS1-2-VERSION+ #xFEFD)

;;; Function definitions
;;;

(ffi:defcfun (#-windows "close" #+windows "closesocket" close-socket)
    :int
  (socket :int))

(declaim (inline ssl-write ssl-read ssl-connect ssl-accept))

(ffi:defctype ssl-method :pointer)
(ffi:defctype ssl-ctx :pointer)
(ffi:defctype ssl-pointer :pointer)


(define-crypto-function-ex (:vanished "1.1.0") ("SSLeay" ssl-eay)
        :long)

(define-crypto-function-ex (:since "1.1.0") ("OpenSSL_version_num" openssl-version-num)
        :long)

(defun compat-openssl-version ()
  (or (ignore-errors (openssl-version-num))
      (ignore-errors (ssl-eay))
      (error "No OpenSSL version number could be determined, both SSLeay and OpenSSL_version_num failed.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +openssl-version-status-strings+
    '("dev"
      "beta 1" "beta 2" "beta 3" "beta 4" "beta 5" "beta 6" "beta 7"
      "beta 8" "beta 9" "beta 10" "beta 11" "beta 12" "beta 13" "beta 14"
      "release")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +openssl-version-patch-characters+
    '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))

(deftype openssl-version-patch ()
  `(or (integer 0 #xff)
       (member ,@+openssl-version-patch-characters+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun openssl-version-status-p (status)
    (or (typep status '(integer 0 #xf))
        (member status +openssl-version-status-strings+ :test #'string=))))

(deftype openssl-version-status ()
  '(satisfies openssl-version-status-p))

(defun encode-openssl-version-impl (major minor &optional (fix 0) (patch 0) (status "release"))
  (check-type major (integer 0 3))
  (check-type minor (integer 0 #xff))
  (check-type fix (integer 0 #xff))
  (check-type patch openssl-version-patch)
  (check-type status openssl-version-status)
  (let ((patch-int (if (integerp patch)
                       patch
                       ;; a = 1, b = 2, and so on:
                       (1+ (position patch +openssl-version-patch-characters+))))
        (status-int (if (integerp status)
                        status
                        ;; dev = 0, beta 1 = 1, beta 2 = 2, ..., beta 14 = 14, release = 15
                        (position status +openssl-version-status-strings+ :test #'string=))))
    (logior (ash major 28)
            (ash minor 20)
            (ash fix 12)
            (ash patch-int 4)
            status-int)))

(assert (= (encode-openssl-version-impl 0 9 6 0 "dev")
           #x00906000))
(assert (= (encode-openssl-version-impl 0 9 6 #\b "beta 3")
           #x00906023))
(assert (= (encode-openssl-version-impl 0 9 6 #\e "release")
           #x0090605f))
(assert (= (encode-openssl-version-impl 0 9 6 #\e)
           #x0090605f))
(assert (= (encode-openssl-version-impl 1 0 0 #\s)
           #x1000013f))

(defun encode-openssl-version (major minor &optional (patch-or-fix 0))
  "Builds a version number to compare with the version returned by OpenSSL.

The integer representation of OpenSSL version has bit fields
for major, minor, fix, patch and status varlues.

Versions before OpenSSL 3 have user readable representations
for all those fields. For example, 0.9.6b beta 3. Here
0 - major, 9 - minor, 6 - fix, b - patch, beta 3 - status.
https://www.openssl.org/docs/man1.1.1/man3/OPENSSL_VERSION_NUMBER.html

Since OpenSSL 3, the third number in user readable repersentation
is patch. The fix and status are not used and have 0 in the corresponding
bit fields.
https://www.openssl.org/docs/man3.0/man3/OPENSSL_VERSION_NUMBER.html
https://www.openssl.org/policies/general/versioning-policy.html

As usually with OpenSSL docs, if the above links disappear becuase
those OpenSSL versions are out of maintenance, use the Wayback Machine.

Note: the _really_ old formats (<= 0.9.4) are not supported."
  (if (>= major 3)
      (encode-openssl-version-impl major minor 0 patch-or-fix)
      (encode-openssl-version-impl major minor patch-or-fix)))

(defun openssl-is-at-least (major minor &optional (patch-or-fix 0))
  (>= (compat-openssl-version)
      (encode-openssl-version major minor patch-or-fix)))

(defun openssl-is-not-even (major minor &optional (patch-or-fix 0))
  (< (compat-openssl-version)
     (encode-openssl-version major minor patch-or-fix)))

(defun libresslp ()
  ;; LibreSSL can be distinguished by
  ;; OpenSSL_version_num() always returning 0x020000000,
  ;; where 2 is the major version number.
  ;; http://man.openbsd.org/OPENSSL_VERSION_NUMBER.3
  ;; And OpenSSL will never use the major version 2:
  ;; "This document outlines the design of OpenSSL 3.0, the next version of OpenSSL after 1.1.1"
  ;; https://www.openssl.org/docs/OpenSSL300Design.html
  (= #x20000000 (compat-openssl-version)))

(define-ssl-function ("SSL_get_version" ssl-get-version)
    :string
  (ssl ssl-pointer))
(define-ssl-function-ex (:vanished "1.1.0") ("SSL_load_error_strings" ssl-load-error-strings)
    :void)
(define-ssl-function-ex (:vanished "1.1.0") ("SSL_library_init" ssl-library-init)
    :int)

(define-ssl-function-ex (:vanished "1.1.0")
    ("OpenSSL_add_all_digests" openssl-add-all-digests)
  :void)

;;
;; We don't refer SSLv2_client_method as the default
;; builds of OpenSSL do not have it, due to insecurity
;; of the SSL v2 protocol (see https://www.openssl.org/docs/ssl/SSL_CTX_new.html
;; and https://github.com/cl-plus-ssl/cl-plus-ssl/issues/6)
;;
;; (define-ssl-function ("SSLv2_client_method" ssl-v2-client-method)
;;     ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv23_client_method" ssl-v23-client-method)
    ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv23_server_method" ssl-v23-server-method)
    ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv23_method" ssl-v23-method)
    ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv3_client_method" ssl-v3-client-method)
    ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv3_server_method" ssl-v3-server-method)
    ssl-method)
(define-ssl-function-ex (:vanished "1.1.0") ("SSLv3_method" ssl-v3-method)
    ssl-method)
(define-ssl-function ("TLSv1_client_method" ssl-TLSv1-client-method)
    ssl-method)
(define-ssl-function ("TLSv1_server_method" ssl-TLSv1-server-method)
    ssl-method)
(define-ssl-function ("TLSv1_method" ssl-TLSv1-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_1_client_method" ssl-TLSv1-1-client-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_1_server_method" ssl-TLSv1-1-server-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_1_method" ssl-TLSv1-1-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_2_client_method" ssl-TLSv1-2-client-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_2_server_method" ssl-TLSv1-2-server-method)
    ssl-method)
(define-ssl-function-ex (:since "1.0.2") ("TLSv1_2_method" ssl-TLSv1-2-method)
    ssl-method)
(define-ssl-function-ex (:since "1.1.0") ("TLS_method" tls-method)
    ssl-method)

(define-ssl-function ("SSL_CTX_new" ssl-ctx-new)
    ssl-ctx
  (method ssl-method))
(define-ssl-function ("SSL_new" ssl-new)
    ssl-pointer
  (ctx ssl-ctx))
(define-ssl-function ("SSL_get_fd" ssl-get-fd)
    :int
  (ssl ssl-pointer))
(define-ssl-function ("SSL_set_fd" ssl-set-fd)
    :int
  (ssl ssl-pointer)
  (fd :int))
(define-ssl-function ("SSL_set_bio" ssl-set-bio)
    :void
  (ssl ssl-pointer)
  (rbio :pointer)
  (wbio :pointer))
(define-ssl-function ("SSL_get_error" ssl-get-error)
    :int
  (ssl ssl-pointer)
  (ret :int))
(define-ssl-function ("SSL_set_connect_state" ssl-set-connect-state)
    :void
  (ssl ssl-pointer))
(define-ssl-function ("SSL_set_accept_state" ssl-set-accept-state)
    :void
  (ssl ssl-pointer))
(define-ssl-function ("SSL_connect" ssl-connect)
    :int
  (ssl ssl-pointer))
(define-ssl-function ("SSL_accept" ssl-accept)
    :int
  (ssl ssl-pointer))
(define-ssl-function ("SSL_write" ssl-write)
    :int
  (ssl ssl-pointer)
  (buf :pointer)
  (num :int))
(define-ssl-function ("SSL_read" ssl-read)
    :int
  (ssl ssl-pointer)
  (buf :pointer)
  (num :int))
(define-ssl-function ("SSL_shutdown" ssl-shutdown)
    :int
  (ssl ssl-pointer))
(define-ssl-function ("SSL_free" ssl-free)
    :void
  (ssl ssl-pointer))
(define-ssl-function ("SSL_CTX_free" ssl-ctx-free)
    :void
  (ctx ssl-ctx))
(define-ssl-function-ex (:since "1.0")  ("SSL_set_alpn_protos" ssl-set-alpn-protos)
    :int
  (SSL :pointer)
  (text :string)
  (len :int))
(define-ssl-function-ex (:since "1.0") ("SSL_get0_alpn_selected" ssl-get0-alpn-selected)
    :void
  (SSL :pointer)
  (text (:pointer :string))
  (len (:pointer :int)))
(define-crypto-function ("BIO_ctrl" bio-set-fd)
    :long
  (bio :pointer)
  (cmd :int)
  (larg :long)
  (parg :pointer))
(define-crypto-function ("BIO_new_socket" bio-new-socket)
    :pointer
  (fd :int)
  (close-flag :int))
(define-crypto-function ("BIO_new" bio-new)
    :pointer
  (method :pointer))
(define-crypto-function ("BIO_free" bio-free)
    :pointer
  (method :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_get_new_index" bio-new-index)
  :int)

(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_new" bio-meth-new)
    :pointer
  (type :int)
  (name :string))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_puts" bio-set-puts)
  :int
  (meth :pointer)
  (puts :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_write" bio-set-write)
  :int
  (meth :pointer)
  (puts :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_read" bio-set-read)
  :int
  (meth :pointer)
  (read :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_gets" bio-set-gets)
  :int
  (meth :pointer)
  (read :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_create" bio-set-create)
  :int
  (meth :pointer)
  (read :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_destroy" bio-set-destroy)
  :int
  (meth :pointer)
  (read :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_meth_set_ctrl" bio-set-ctrl)
  :int
  (meth :pointer)
  (read :pointer))
(define-crypto-function-ex (:since "1.1.0") ("BIO_set_init" bio-set-init)
  :int
  (meth :pointer)
  (value :int))
(define-crypto-function-ex (:since "1.1.0") ("BIO_set_flags" bio-set-flags)
  :int
  (meth :pointer)
  (value :int))
(define-crypto-function-ex (:since "1.1.0") ("BIO_clear_flags" bio-clear-flags)
  :int
  (meth :pointer)
  (value :int))
(define-crypto-function-ex (:since "1.1.0") ("BIO_test_flags" bio-test-flags)
  :int
  (meth :pointer)
  (value :int))

(define-crypto-function ("ERR_get_error" err-get-error)
    :unsigned-long)
(define-crypto-function ("ERR_error_string" err-error-string)
    :string
  (e :unsigned-long)
  (buf :pointer))
(define-crypto-function-ex (:vanished "3.0.0") ("ERR_put_error" err-put-error)
  :void
  (lib :int)
  (func :int)
  (reason :int)
  ;; The file is :pointer instead of :string, becasue the file
  ;; name should not be dalocated after the function call
  ;; returns - that must be a long living char array.
  (file :pointer)
  (line :int))

(defconstant +err_lib_none+ 1)
(defconstant +err_r_fatal+ 64)
(defconstant +err_r_internal_error+ (logior 4 +err_r_fatal+))

(define-crypto-function-ex (:since "3.0.0") ("ERR_new" err-new)
  :void)

(define-crypto-function-ex (:since "3.0.0") ("ERR_set_debug" err-set-debug)
  :void
  (file :string)
  (line :int)
  (func :string))

(define-crypto-function-ex (:since "3.0.0") ("ERR_set_error" err-set-error)
  :void
  (lib :int)
  (reason :int)
  (fmt :string)
  &rest)

;; Is that a new function in 1.0.2 or existed forever?
(define-crypto-function-ex (:since "1.0.2")
    ("ERR_get_next_error_library" err-get-next-error-library)
  :int)

(define-crypto-function ("ERR_add_error_data" err-add-error-data)
  :void
  (num :int)
  &rest)

;; Is that a new function in 3.0.0 or existed before?
(define-crypto-function-ex (:since "3.0.0") ("ERR_add_error_txt" err-add-error-txt)
  :void
  (sep :string)
  (txt :string))

(define-crypto-function ("ERR_print_errors" err-print-errors)
  :void
  (bio :pointer))

(define-ssl-function ("SSL_set_cipher_list" ssl-set-cipher-list)
    :int
  (ssl ssl-pointer)
  (str :string))
(define-ssl-function-ex (:since "1.1.1") ("SSL_set_ciphersuites" ssl-set-ciphersuites)
    :int
  (ssl ssl-pointer)
  (str :string))

(define-ssl-function ("SSL_use_RSAPrivateKey_file" ssl-use-rsa-privatekey-file)
    :int
  (ssl ssl-pointer)
  (str :string)
  ;; either +ssl-filetype-pem+ or +ssl-filetype-asn1+
  (type :int))
(define-ssl-function
    ("SSL_CTX_use_RSAPrivateKey_file" ssl-ctx-use-rsa-privatekey-file)
    :int
  (ctx ssl-ctx)
  (type :int))
(define-ssl-function ("SSL_use_PrivateKey_file" ssl-use-privatekey-file)
  :int
  (ssl ssl-pointer)
  (str :string)
  ;; either +ssl-filetype-pem+ or +ssl-filetype-asn1+
  (type :int))
(define-ssl-function
    ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-privatekey-file)
  :int
  (ctx ssl-ctx)
  (file :string)
  (type :int))
(define-ssl-function ("SSL_use_certificate_file" ssl-use-certificate-file)
    :int
  (ssl ssl-pointer)
  (str :string)
  (type :int))

(define-ssl-function ("SSL_CTX_ctrl" ssl-ctx-ctrl)
    :long
  (ctx ssl-ctx)
  (cmd :int)
  ;; Despite declared as long in the original OpenSSL headers,
  ;; passing to larg for example 2181041151 which is the result of
  ;;     (logior tls::+SSL-OP-ALL+
  ;;             tls::+SSL-OP-NO-SSLv2+
  ;;             tls::+SSL-OP-NO-SSLv3+)
  ;; causes FFI on 32 bit platforms to signal an error
  ;; "The value 2181041151 is not of the expected type (SIGNED-BYTE 32)"
  ;; The problem is that 2181041151 requires 32 bits by itself and
  ;; there is no place left for the sign bit.
  ;; In C the compiler silently coerces unsigned to signed,
  ;; but FFI raises this error.
  ;; Therefore we use :UNSIGNED-LONG for LARG.
  (larg :unsigned-long)
  (parg :pointer))

(define-ssl-function ("SSL_ctrl" ssl-ctrl)
    :long
  (ssl :pointer)
  (cmd :int)
  (larg :long)
  (parg :pointer))

#+new-openssl
(define-ssl-function ("SSL_CTX_set_options" ssl-ctx-set-options)
                 :long
               (ctx :pointer)
               (options :long))
#-new-openssl
(defun ssl-ctx-set-options (ctx options)
  (ssl-ctx-ctrl ctx +SSL-CTRL-OPTIONS+ options (ffi:null-pointer)))
(defun ssl-ctx-set-min-proto-version (ctx version)
  (ssl-ctx-ctrl ctx +SSL-CTRL-SET-MIN-PROTO-VERSION+ version (ffi:null-pointer)))
(defun ssl-ctx-set-max-proto-version (ctx version)
  (ssl-ctx-ctrl ctx +SSL-CTRL-SET-MAX-PROTO-VERSION+ version (ffi:null-pointer)))
(define-ssl-function ("SSL_CTX_set_cipher_list" ssl-ctx-set-cipher-list)
    :int
  (ctx :pointer)
  (ciphers :string))
(define-ssl-function-ex (:since "1.1.1") ("SSL_CTX_set_ciphersuites" ssl-ctx-set-ciphersuites)
    :int
  (ctx :pointer)
  (ciphersuites :string))
(define-ssl-function ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file)
    :int
  (ctx ssl-ctx)
  (str :string))
(define-ssl-function ("SSL_CTX_load_verify_locations" ssl-ctx-load-verify-locations)
    :int
  (ctx ssl-ctx)
  (CAfile :string)
  (CApath :string))
(define-ssl-function ("SSL_CTX_set_client_CA_list" ssl-ctx-set-client-ca-list)
    :void
  (ctx ssl-ctx)
  (list ssl-pointer))
(define-ssl-function ("SSL_load_client_CA_file" ssl-load-client-ca-file)
    ssl-pointer
  (file :string))

(define-ssl-function ("SSL_CTX_set_default_passwd_cb" ssl-ctx-set-default-passwd-cb)
    :void
  (ctx ssl-ctx)
  (pem_passwd_cb :pointer))

(define-crypto-function-ex (:vanished "1.1.0") ("CRYPTO_num_locks" crypto-num-locks) :int)
(define-crypto-function-ex (:vanished "1.1.0") ("CRYPTO_set_locking_callback" crypto-set-locking-callback)
    :void
  (fun :pointer))
(define-crypto-function-ex (:vanished "1.1.0") ("CRYPTO_set_id_callback" crypto-set-id-callback)
    :void
  (fun :pointer))

(define-crypto-function ("RAND_seed" rand-seed)
    :void
  (buf :pointer)
  (num :int))
(define-crypto-function ("RAND_bytes" rand-bytes)
    :int
  (buf :pointer)
  (num :int))

(define-ssl-function ("SSL_CTX_set_verify_depth" ssl-ctx-set-verify-depth)
    :void
  (ctx :pointer)
  (depth :int))

(define-ssl-function ("SSL_CTX_set_verify" ssl-ctx-set-verify)
    :void
  (ctx :pointer)
  (mode :int)
  (verify-callback :pointer))

(define-ssl-function ("SSL_get_verify_result" ssl-get-verify-result)
    :long
  (ssl ssl-pointer))

(define-ssl-function-ex (:vanished "3.0.0") ("SSL_get_peer_certificate" ssl-get-peer-certificate)
    :pointer
  (ssl ssl-pointer))

(define-ssl-function-ex (:since "3.0.0") ("SSL_get1_peer_certificate" ssl-get1-peer-certificate)
    :pointer
  (ssl ssl-pointer))

(defun compat-ssl-get1-peer-certificate (handle)
  (funcall (if (openssl-is-at-least 3 0 0)
               'ssl-get1-peer-certificate
               'ssl-get-peer-certificate)
           handle))

;;; X509 & ASN1
(define-crypto-function ("X509_free" x509-free)
    :void
  (x509 :pointer))

(define-crypto-function ("X509_NAME_oneline" x509-name-oneline)
    :pointer
  (x509-name :pointer)
  (buf :pointer)
  (size :int))

(define-crypto-function ("X509_NAME_get_index_by_NID" x509-name-get-index-by-nid)
    :int
  (name :pointer)
  (nid :int)
  (lastpos :int))

(define-crypto-function ("X509_NAME_get_entry" x509-name-get-entry)
    :pointer
  (name :pointer)
  (log :int))

(define-crypto-function ("X509_NAME_ENTRY_get_data" x509-name-entry-get-data)
    :pointer
  (name-entry :pointer))

(define-crypto-function ("X509_get_issuer_name" x509-get-issuer-name)
    :pointer                            ; *X509_NAME
  (x509 :pointer))

(define-crypto-function ("X509_get_subject_name" x509-get-subject-name)
    :pointer                            ; *X509_NAME
  (x509 :pointer))

(define-crypto-function-ex (:since "1.1.0") ("X509_get0_notBefore" x509-get0-not-before)
    :pointer                            ; *ASN1_TIME
  (x509 :pointer))

(define-crypto-function-ex (:since "1.1.0") ("X509_get0_notAfter" x509-get0-not-after)
    :pointer                            ; *ASN1_TIME
  (x509 :pointer))

(define-crypto-function ("X509_get_ext_d2i" x509-get-ext-d2i)
    :pointer
  (cert :pointer)
  (nid :int)
  (crit :pointer)
  (idx :pointer))

(define-crypto-function ("X509_STORE_CTX_get_error" x509-store-ctx-get-error)
    :int
  (ctx :pointer))

(define-crypto-function ("d2i_X509" d2i-x509)
    :pointer
  (*px :pointer)
  (in :pointer)
  (len :int))

(define-crypto-function ("X509_digest" x509-digest)
    :int
  (cert :pointer)
  (type :pointer)
  (buf :pointer)
  (*len :pointer))

(define-crypto-function ("PEM_write_bio_X509" pem-write-x509)
  :int
  (bio :pointer)
  (x509 :pointer))

(define-crypto-function ("PEM_read_bio_X509" pem-read-x509)
  :pointer
  ;; all args are :pointers in fact, but they are NULL anyway
  (bio :pointer)
  (x509 :int)
  (callback :int)
  (passphrase :int))

;;; EVP

(define-crypto-function ("EVP_get_digestbyname" evp-get-digest-by-name)
    :pointer
  (name :string))

(define-crypto-function-ex (:vanished "3.0.0") ("EVP_MD_size" evp-md-size)
    :int
  (evp :pointer))

(define-crypto-function-ex (:since "3.0.0") ("EVP_MD_get_size" evp-md-get-size)
    :int
  (evp :pointer))


;; GENERAL-NAME types
(defconstant +GEN-OTHERNAME+  0)
(defconstant +GEN-EMAIL+  1)
(defconstant +GEN-DNS+    2)
(defconstant +GEN-X400+ 3)
(defconstant +GEN-DIRNAME+  4)
(defconstant +GEN-EDIPARTY+ 5)
(defconstant +GEN-URI+    6)
(defconstant +GEN-IPADD+  7)
(defconstant +GEN-RID+    8)

(defconstant +v-asn1-octet-string+ 4)
(defconstant +v-asn1-utf8string+ 12)
(defconstant +v-asn1-printablestring+ 19)
(defconstant +v-asn1-teletexstring+ 20)
(defconstant +v-asn1-iastring+ 22)
(defconstant +v-asn1-universalstring+ 28)
(defconstant +v-asn1-bmpstring+ 30)


(defconstant +NID-subject-alt-name+ 85)
(defconstant +NID-commonName+   13)

(ffi:defcstruct general-name
  (type :int)
  (data :pointer))

(define-crypto-function-ex (:vanished "1.1.0") ("sk_value" sk-value)
    :pointer
  (stack :pointer)
  (index :int))

(define-crypto-function-ex (:vanished "1.1.0") ("sk_num" sk-num)
    :int
  (stack :pointer))

(define-crypto-function-ex (:since "1.1.0") ("OPENSSL_sk_value" openssl-sk-value)
    :pointer
  (stack :pointer)
  (index :int))

(define-crypto-function-ex (:since "1.1.0") ("OPENSSL_sk_num" openssl-sk-num)
    :int
  (stack :pointer))

(declaim (ftype (function (ffi:foreign-pointer fixnum) ffi:foreign-pointer) sk-general-name-value))
(defun sk-general-name-value (names index)
  (if (and (not (libresslp))
           (openssl-is-at-least 1 1))
      (openssl-sk-value names index)
      (sk-value names index)))

(declaim (ftype (function (ffi:foreign-pointer) fixnum) sk-general-name-num))
(defun sk-general-name-num (names)
  (if (and (not (libresslp))
           (openssl-is-at-least 1 1))
      (openssl-sk-num names)
      (sk-num names)))

(define-crypto-function ("GENERAL_NAMES_free" general-names-free)
    :void
  (general-names :pointer))

(define-crypto-function ("ASN1_STRING_data" asn1-string-data)
    :pointer
  (asn1-string :pointer))

(define-crypto-function ("ASN1_STRING_length" asn1-string-length)
    :int
  (asn1-string :pointer))

(define-crypto-function ("ASN1_STRING_type" asn1-string-type)
    :int
  (asn1-string :pointer))

(ffi:defcstruct asn1_string_st
  (length :int)
  (type :int)
  (data :pointer)
  (flags :long))

(define-crypto-function ("ASN1_TIME_check" asn1-time-check)
    :int
  (asn1-string :pointer))

(define-crypto-function ("ASN1_UTCTIME_check" asn1-utctime-check)
    :int
  (asn1-string :pointer))

;; X509 & ASN1 - end

(define-ssl-function ("SSL_CTX_set_default_verify_paths" ssl-ctx-set-default-verify-paths)
    :int
  (ctx :pointer))

(define-ssl-function-ex (:since "1.1.0") ("SSL_CTX_set_default_verify_dir" ssl-ctx-set-default-verify-dir)
    :int
  (ctx :pointer))

(define-ssl-function-ex (:since "1.1.0") ("SSL_CTX_set_default_verify_file" ssl-ctx-set-default-verify-file)
    :int
  (ctx :pointer))

(define-crypto-function ("RSA_generate_key" rsa-generate-key)
    :pointer
  (num :int)
  (e :unsigned-long)
  (callback :pointer)
  (opt :pointer))

(define-crypto-function ("RSA_free" rsa-free)
    :void
  (rsa :pointer))

(define-ssl-function-ex (:vanished "1.1.0") ("SSL_CTX_set_tmp_rsa_callback" ssl-ctx-set-tmp-rsa-callback)
    :pointer
  (ctx :pointer)
  (callback :pointer))

(defun ssl-ctx-set-session-cache-mode (ctx mode)
  (ssl-ctx-ctrl ctx +SSL_CTRL_SET_SESS_CACHE_MODE+ mode (ffi:null-pointer)))

(defun ssl-set-tlsext-host-name (ctx hostname)
  (ssl-ctrl ctx 55 #|SSL_CTRL_SET_TLSEXT_HOSTNAME|# 0 #|TLSEXT_NAMETYPE_host_name|# hostname))

(defconstant +CRYPTO-LOCK+ 1)
(defconstant +CRYPTO-UNLOCK+ 2)
(defconstant +CRYPTO-READ+ 4)
(defconstant +CRYPTO-WRITE+ 8)

;;xx

(defparameter *bio-blockp* t)
(defvar *bio-socket*)

(defvar *bio-is-opaque*
  "Since openssl 1.1.0, bio properties should be accessed using
 functions, not directly using C structure slots.
 Intialized to T for such openssl versions.")
(defvar *lisp-bio-type*)
(defvar *bio-lisp-method* nil)

(defconstant +BIO_TYPE_SOURCE_SINK+ #x0400)
(defconstant +BIO_TYPE_DESCRIPTOR+ #x0100)

(defconstant +bio-type-socket+ (logior 5
                                       +BIO_TYPE_SOURCE_SINK+
                                       +BIO_TYPE_DESCRIPTOR+))

(defconstant +BIO_CTRL_EOF+ 2)
(defconstant +BIO_CTRL_FLUSH+ 11)

(defconstant +BIO_FLAGS_READ+ 1)
(defconstant +BIO_FLAGS_WRITE+ 2)
(defconstant +BIO_FLAGS_IO_SPECIAL+ 4)
(defconstant +BIO_FLAGS_RWS+ (logior +BIO_FLAGS_READ+
                                     +BIO_FLAGS_WRITE+
                                     +BIO_FLAGS_IO_SPECIAL+))
(defconstant +BIO_FLAGS_SHOULD_RETRY+ 8)
(defconstant +BIO_FLAGS_IN_EOF+ #x800)

(ffi:defcstruct bio-method
  (type :int)
  (name :pointer)
  (bwrite :pointer)
  (bread :pointer)
  (bputs :pointer)
  (bgets :pointer)
  (ctrl :pointer)
  (create :pointer)
  (destroy :pointer)
  (callback-ctrl :pointer))

(ffi:defcstruct bio
  (method :pointer)
  (callback :pointer)
  (cb-arg :pointer)
  (init :int)
  (shutdown :int)
  (flags :int)
  (retry-reason :int)
  (num :int)
  (ptr :pointer)
  (next-bio :pointer)
  (prev-bio :pointer)
  (references :int)
  (num-read :unsigned-long)
  (num-write :unsigned-long)
  (crypto-ex-data-stack :pointer)
  (crypto-ex-data-dummy :int))

(defun lisp-bio-type ()
  (or (ignore-errors
        (logior (bio-new-index) +BIO_TYPE_SOURCE_SINK+))
      ;; Old OpenSSL and LibreSSL do not nave BIO_get_new_index,
      ;; in this case fallback to BIO_TYPE_SOCKET.
      ;; fixmy: Maybe that's wrong, but presumably still better than some
      ;; random value here.
      +bio-type-socket+))

(defun make-bio-lisp-method-slots ()
  (let ((m (ffi:foreign-alloc '(:struct bio-method))))
    (setf (ffi:foreign-slot-value m '(:struct bio-method) 'type)
          *lisp-bio-type*)
    (macrolet ((slot (name)
                 `(ffi:foreign-slot-value m '(:struct bio-method) ,name)))
      (setf (slot 'name) (ffi:foreign-string-alloc "lisp"))
      (setf (slot 'bwrite) (ffi:callback lisp-write))
      (setf (slot 'bread) (ffi:callback lisp-read))
      (setf (slot 'bputs) (ffi:callback lisp-puts))
      (setf (slot 'bgets) (ffi:callback lisp-gets))
      (setf (slot 'ctrl) (ffi:callback lisp-ctrl))
      (setf (slot 'create) (ffi:callback lisp-create-slots))
      (setf (slot 'destroy) (ffi:callback lisp-destroy-slots))
      (setf (slot 'callback-ctrl) (ffi:null-pointer)))
    m))

(defun make-bio-lisp-method-opaque ()
  (let ((m (bio-meth-new *lisp-bio-type* "lisp")))
    (bio-set-puts m (ffi:callback lisp-puts))
    (bio-set-write m (ffi:callback lisp-write))
    (bio-set-read m (ffi:callback lisp-read))
    (bio-set-gets m (ffi:callback lisp-gets))
    (bio-set-create m (ffi:callback lisp-create-opaque))
    (bio-set-destroy m (ffi:callback lisp-destroy-opaque))
    (bio-set-ctrl m (ffi:callback lisp-ctrl))
    m))

(defun make-bio-lisp-method ()
  (if *bio-is-opaque*
      (make-bio-lisp-method-opaque)
      (make-bio-lisp-method-slots)))

(defun bio-init ()
  (setf *bio-is-opaque*
        ;; (openssl-is-at-least 1 1) - this is not precise in case of LibreSSL,
        ;; therefore use the following:
        (not (null (ffi:foreign-symbol-pointer "BIO_get_new_index"
                                                :library 'libcrypto)))
        *lisp-bio-type* (lisp-bio-type)
        *bio-lisp-method* (make-bio-lisp-method)))

(defun bio-new-lisp ()
  (unless *bio-lisp-method* (bio-init))
  (let ((new (bio-new *bio-lisp-method*)))
    (if (or (null new) (ffi:null-pointer-p new))
        (error "Cannot create bio method: ~a"
               (err-error-string (err-get-error) (ffi:null-pointer)))
        new)))

(defun bio-set-flags-slots (bio &rest flags)
  (setf (ffi:foreign-slot-value bio '(:struct bio) 'flags)
        (logior (ffi:foreign-slot-value bio '(:struct bio) 'flags)
                (apply #'logior flags))))

(defun compat-bio-set-flags (bio &rest flags)
    (if *bio-is-opaque*
        (bio-set-flags bio (apply #'logior flags)) ;; FFI function since OpenSSL 1.1.0
        (apply #'bio-set-flags-slots bio flags)))

(defun bio-clear-flags-slots (bio &rest flags)
  (setf (ffi:foreign-slot-value bio '(:struct bio) 'flags)
        (logandc2 (ffi:foreign-slot-value bio '(:struct bio) 'flags)
                  (apply #'logior flags))))

(defun compat-bio-clear-flags (bio &rest flags)
  (if *bio-is-opaque*
      (bio-clear-flags bio (apply #'logior flags)) ;; FFI function since OpenSSL 1.1.0
      (apply #'bio-clear-flags-slots bio flags)))

(defun bio-test-flags-slots (bio &rest flags)
  (logand (ffi:foreign-slot-value bio '(:struct bio) 'flags)
          (apply #'logior flags)))

(defun compat-bio-test-flags (bio &rest flags)
  (if *bio-is-opaque*
      (bio-test-flags bio (apply #'logior flags)) ;; FFI function since OpenSSL 1.1.0
      (apply #'bio-test-flags-slots bio flags)))

(defun clear-retry-flags (bio)
  (compat-bio-clear-flags bio
                          +BIO_FLAGS_RWS+
                          +BIO_FLAGS_SHOULD_RETRY+))

(defun set-retry-read (bio)
  (compat-bio-set-flags bio
                        +BIO_FLAGS_READ+
                        +BIO_FLAGS_SHOULD_RETRY+))


;;; Error handling for all the defcallback's:
;;;
;;; We want to avoid non-local exits across C stack,
;;; as CFFI tutorial recommends:
;;; https://common-lisp.net/project/cffi/manual/html_node/Tutorial_002dCallbacks.html.
;;;
;;; In tls this means the following nested calls:
;;;
;;;   1) Lisp: tls stream user code ->
;;;   2) C: OpenSSL C functions ->
;;;   3) Lisp: BIO implementation function
;;;        signals error and the controls is passed
;;;        to (1), without proper C cleanup.
;;;
;;; Therefore our BIO implementation functions catch all unexpected
;;; serious-conditions, arrange for BIO_should_retry
;;; to say "do not retry", and return error status (most often -1).
;;;
;;; We could try to return the real number of bytes read / written -
;;; the documentation of BIO_read and friends just says return byte
;;; number without making any special case for error:
;;;
;;; >    (...) return either the amount of data successfully read or
;;; >    written (if the return value is positive) or that no data was
;;; >    successfully read or written if the result is 0 or -1. If the
;;; >    return value is -2 then the operation is not implemented in the
;;; >    specific BIO type. The trailing NUL is not included in the length
;;; >    returned by BIO_gets().
;;;
;;; But let's not complicate the implementation, especially taking into
;;; account that we don't know how many bytes the low level
;;; Lisp function has really written before signalling
;;; the condition. Our main goal is to avoid crossing C stack,
;;; and we only consider unexpected errors here.

(defparameter *file-name* (ffi:foreign-string-alloc "tls/src/bio.lisp"))

(defparameter *lib-num-for-errors*
  (if (openssl-is-at-least 1 0 2)
      (err-get-next-error-library)
      +err_lib_none+))

(defun put-to-openssl-error-queue (condition)
  (handler-case
      (let ((err-msg (format nil
                             "Unexpected serious-condition ~A in the Lisp BIO: ~A"
                             (type-of condition)
                             condition)))
        (if (openssl-is-at-least 3 0)
            (progn
              (err-new)
              (err-set-debug *file-name* 0 (ffi:null-pointer))
              (err-set-error *lib-num-for-errors*
                             +err_r_internal_error+
                             "%s"
                             :string err-msg))
            (progn
              (err-put-error *lib-num-for-errors*
                             0
                             +err_r_internal_error+
                             *file-name*
                             0)
              (err-add-error-data 1
                                  :string
                                  err-msg))))
    (serious-condition (c)
      (warn "~A when saving Lisp BIO error to OpenSSL error queue: ~A"
            (type-of c) c))))

(ffi:defcallback lisp-write :int ((bio :pointer) (buf :pointer) (n :int))
  (handler-case
      (progn (dotimes (i n)
               (write-byte (ffi:mem-ref buf :unsigned-char i) *bio-socket*))
             (finish-output *bio-socket*)
             n)
    (serious-condition (c)
      (clear-retry-flags bio)
      (put-to-openssl-error-queue c)
      -1)))

(ffi:defcallback lisp-read :int ((bio :pointer) (buf :pointer) (n :int))
  (handler-case
      (let ((i 0))
        (handler-case
            (progn
              (clear-retry-flags bio)
              (loop
                while (and (< i n)
                           (or *bio-blockp* (listen *bio-socket*)))
                do
                   (setf (ffi:mem-ref buf :unsigned-char i)
                         (read-byte *bio-socket*))
                   (incf i))
              (when (zerop i) (set-retry-read bio)))
          (end-of-file ()
            (compat-bio-set-flags bio +BIO_FLAGS_IN_EOF+)
            ;; now just return the number of bytes read so far
            ))
        ;; Old OpenSSL treats zero as EOF and signals an error:
        ;; "The TLS/SSL connection on handle #<A Foreign Pointer #x7F42DC082880> has been closed (return code: 5)"
        ;; despite our implementation of (BIO_ctrl ... +BIO_CTRL_EOF+)
        ;; returns false.
        ;; (This was observed on openssl-1.1.0j. And
        ;; on OpenSSL 3 it does not happen).
        ;; Since both 0 and -1 are allowed by the docs,
        ;; let's return -1 instead of 0.
        (if (= 0 i) -1 i))
    (serious-condition (c)
      (clear-retry-flags bio)
      (put-to-openssl-error-queue c)
      -1)))

(ffi:defcallback lisp-gets :int ((bio :pointer) (buf :pointer) (n :int))
  (handler-case
      (let ((i 0)
            (max-chars (1- n)))
        (clear-retry-flags bio)
        (handler-case
            (loop
              with char
              and exit = nil
              while (and (< i max-chars)
                         (null exit)
                         (or *bio-blockp* (listen *bio-socket*)))
              do
                 (setf char (read-byte *bio-socket*)
                       exit (= char 10))
                 (setf (ffi:mem-ref buf :unsigned-char i) char)
                 (incf i))
          (end-of-file ()
            (compat-bio-set-flags bio +BIO_FLAGS_IN_EOF+)))
        (setf (ffi:mem-ref buf :unsigned-char i) 0)
        i)
    (serious-condition (c)
      (clear-retry-flags bio)
      (put-to-openssl-error-queue c)
      -1)))

(ffi:defcallback lisp-puts :int ((bio :pointer) (buf :string))
  (handler-case
      (progn
        (write-line buf (make-char-output-stream *bio-socket*
                                                 :encoding (make-encoding :ascii))) ; TODO preallocate
        ;; puts is not specified to return length, but BIO expects it :(
        (1+ (length buf)))
    (serious-condition (c)
      (clear-retry-flags bio)
      (put-to-openssl-error-queue c)
      -1)))

(ffi:defcallback lisp-ctrl :int
    ((bio :pointer) (cmd :int) (larg :long) (parg :pointer))
  (declare (ignore larg parg))
  (cond
    ((eql cmd +BIO_CTRL_EOF+)
     (if (zerop (compat-bio-test-flags bio +BIO_FLAGS_IN_EOF+))
         0
         1))
    ((eql cmd +BIO_CTRL_FLUSH+) 1)
    (t
     ;; (warn "lisp-ctrl(~A,~A,~A)" cmd larg parg)
     0)))

;;; The create and destroy handlers mostly consist
;;; of setting zero values to some BIO fields,
;;; which seem redundant, because OpenSSl most likely
;;; does this itself. But we follow example of the
;;; standard OpenSSL BIO types implementation.
;;; Like the file_new / file_free here:
;;; https://github.com/openssl/openssl/blob/4ccad35756dfa9df657f3853810101fa9d6ca525/crypto/bio/bss_file.c#L109

(ffi:defcallback lisp-create-slots :int ((bio :pointer))
  (handler-case
      (progn
        (setf (ffi:foreign-slot-value bio '(:struct bio) 'init) 1) ; the only useful thing?
        (setf (ffi:foreign-slot-value bio '(:struct bio) 'num) 0)
        (setf (ffi:foreign-slot-value bio '(:struct bio) 'ptr) (ffi:null-pointer))
        (setf (ffi:foreign-slot-value bio '(:struct bio) 'flags) 0)
        1)
    (serious-condition (c)
      (put-to-openssl-error-queue c)
      0)))

(ffi:defcallback lisp-create-opaque :int ((bio :pointer))
  (handler-case
      (progn
        (bio-set-init bio 1) ; the only useful thing?
        (clear-retry-flags bio)
        1)
    (serious-condition (c)
      (put-to-openssl-error-queue c)
      0)))

(ffi:defcallback lisp-destroy-slots :int ((bio :pointer))
  (handler-case
      (cond
        ((ffi:null-pointer-p bio) 0)
        (t
         (setf (ffi:foreign-slot-value bio '(:struct bio) 'init) 0)
         (setf (ffi:foreign-slot-value bio '(:struct bio) 'flags) 0)
         1))
    (serious-condition (c)
      (put-to-openssl-error-queue c)
      0)))

(ffi:defcallback lisp-destroy-opaque :int ((bio :pointer))
  (handler-case
      (cond
        ((ffi:null-pointer-p bio) 0)
        (t
         (bio-set-init bio 0)
         (clear-retry-flags bio)
         1))
    (serious-condition (c)
      (put-to-openssl-error-queue c)
      0)))

;;; Convenience macros
(defmacro with-bio-output-to-string ((bio) &body body)
  "Evaluate BODY with BIO bound to a SSL BIO structure that writes to
a Common Lisp string.  The string is returned."
  `(let ((*bio-socket* (make-instance 'fast-output-stream))
	 (,bio (bio-new-lisp)))
     (unwind-protect
          (progn ,@body)
       (bio-free ,bio))
     (u8-to-string (finish-output-stream *bio-socket*))))

(defmacro with-bio-input-from-string ((bio string) &body body)
  "Evaluate BODY with BIO bound to a SSL BIO structure that reads from
a Common Lisp STRING."
  `(let ((*bio-socket* (make-vector-stream (string-to-u8 ,string)))
	 (,bio (bio-new-lisp)))
     (unwind-protect
          (progn ,@body)
       (bio-free ,bio))))

(setf *bio-lisp-method* nil)    ;force reinit if anything changed here

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ssl-error-none+ 0)
  (defconstant +ssl-error-ssl+ 1)
  (defconstant +ssl-error-want-read+ 2)
  (defconstant +ssl-error-want-write+ 3)
  (defconstant +ssl-error-want-x509-lookup+ 4)
  (defconstant +ssl-error-syscall+ 5)
  (defconstant +ssl-error-zero-return+ 6)
  (defconstant +ssl-error-want-connect+ 7))


(defun collect-verify-error (ssl-error-ssl-condition handle)
  (let ((code (ssl-get-verify-result handle)))
    (unless (eql code +x509-v-ok+)
      (setf (ssl-error-ssl-verify-error ssl-error-ssl-condition)
            (format nil "SSL_get_verify_result: ~d~@[ ~a~]"
                    code (ssl-verify-error-keyword code))))))

(defun collecting-verify-error-impl (handle body-fn)
  (handler-bind ((ssl-error-ssl (lambda (c)
                                  (collect-verify-error c handle))))
    (funcall body-fn)))

(defmacro collecting-verify-error ((handle) &body body)
  `(collecting-verify-error-impl ,handle (lambda () ,@body)))

(defun err-print-errors-to-string ()
  (with-bio-output-to-string (bio)
    (err-print-errors bio)))

(defun ssl-signal-error (handle syscall error-code ret)
  "RET is return value of the failed SYSCALL (like SSL_read, SSL_connect,
SSL_shutdown, etc - most of them designate failure by returning
RET <= 0, althought SSL_shutdow fails with RET < 0.

ERROR-CODE is return value of SSL_get_error - an explanation of the failure.
"
  (let ((printed-queue (err-print-errors-to-string))
        ;; FixMe: the error queue is emptied by (err-print-errors-to-string)
        ;;        above so the QUEUE becomes an empty list.
        (queue (read-ssl-error-queue)))
    ;; BAD: The IF below is responsible to represent the "Unexpected EOF"
    ;; situation, which is when the remote peer closes
    ;; TCP connection without sending TLS close_notify alert,
    ;; as a situation of normal close_notify alert received.
    ;;
    ;; OpenSSL before version 3.0 signals the Unexpected EOF
    ;; as error-code = SSL_ERROR_SYSCALL and ret = 0.
    ;; Normal termination is signalled by error-code = SSL_ERROR_ZERO_RETURN.
    ;;
    ;; As you see below, the IF turns the former into the latter.
    ;;
    ;; We should not suppress the Unexpected EOF error, because
    ;; some protocols on top of TLS may be attacked with TLS truncation
    ;; attack. For example HTTP 0.9, where response size is not specified
    ;; by the server but instead end of message is indicated by server closing
    ;; the connection.
    ;;
    ;; In such protocols a malicious middle-man can insert an unencrypted
    ;; TCP FIN packet, thus giving the client a partial response. OpenSSL treats
    ;; this as an Unexpected EOF error, but tls turns it into
    ;; the ssl-error-zero-return condition, which is then internally
    ;; converted simply to an end of ssl-stream. Thus the user will treat
    ;; the truncated response as authoritative and complete.
    ;;
    ;; Since OpenSSL 3.0 the suppression does not happen
    ;; and tls user receives an error condition, because
    ;; the Unexpected EOF is reported as error-code = SSL_ERROR_SSL.
    ;;
    ;; The only reason we currently keep this not fixed for older OpenSSL
    ;; is potential backwards compatibility problems with existing
    ;; Common Lisp libraries and applications and the fact
    ;; that protocols where message sizes are usually
    ;; explicitly indicated (like HTTP 1.1 where Content-Length or
    ;; chunked encoding are used) truncation can be detected
    ;; without relying to TLS and thus some servers close TCP
    ;; connections without sending TLS close_notify alert.
    ;; Some libraries or applications may be relying onto
    ;; silent end of stream after full message is received
    ;; according to the size indicated by the protocol.
    ;;
    ;; See one example of this, discussion and links in
    ;; https://github.com/cl-plus-ssl/cl-plus-ssl/issues/166
    (if (and (eql error-code #.+ssl-error-syscall+)
             (not (zerop ret)))
        (error 'ssl-error-syscall
               :handle handle
               :ret error-code
               :printed-queue printed-queue
               :queue queue
               :syscall syscall)
        (error (case error-code
                 (#.+ssl-error-none+ 'ssl-error-none)
                 (#.+ssl-error-ssl+ 'ssl-error-ssl)
                 (#.+ssl-error-want-read+ 'ssl-error-want-read)
                 (#.+ssl-error-want-write+ 'ssl-error-want-write)
                 (#.+ssl-error-want-x509-lookup+ 'ssl-error-want-x509-lookup)
                 (#.+ssl-error-zero-return+ 'ssl-error-zero-return)
                 (#.+ssl-error-want-connect+ 'ssl-error-want-connect)
                 (#.+ssl-error-syscall+ 'ssl-error-zero-return) ; this is intentional here. we got an EOF from the syscall (ret is 0)
                 (t 'ssl-error/handle))
               :handle handle
               :ret error-code
               :printed-queue printed-queue
               :queue queue))))

(defparameter *ssl-verify-error-alist*
  '((0 :X509_V_OK)
    (2 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT)
    (3 :X509_V_ERR_UNABLE_TO_GET_CRL)
    (4 :X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE)
    (5 :X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE)
    (6 :X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY)
    (7 :X509_V_ERR_CERT_SIGNATURE_FAILURE)
    (8 :X509_V_ERR_CRL_SIGNATURE_FAILURE)
    (9 :X509_V_ERR_CERT_NOT_YET_VALID)
    (10 :X509_V_ERR_CERT_HAS_EXPIRED)
    (11 :X509_V_ERR_CRL_NOT_YET_VALID)
    (12 :X509_V_ERR_CRL_HAS_EXPIRED)
    (13 :X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD)
    (14 :X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD)
    (15 :X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD)
    (16 :X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD)
    (17 :X509_V_ERR_OUT_OF_MEM)
    (18 :X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT)
    (19 :X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN)
    (20 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY)
    (21 :X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE)
    (22 :X509_V_ERR_CERT_CHAIN_TOO_LONG)
    (23 :X509_V_ERR_CERT_REVOKED)
    (24 :X509_V_ERR_INVALID_CA)
    (25 :X509_V_ERR_PATH_LENGTH_EXCEEDED)
    (26 :X509_V_ERR_INVALID_PURPOSE)
    (27 :X509_V_ERR_CERT_UNTRUSTED)
    (28 :X509_V_ERR_CERT_REJECTED)
    (29 :X509_V_ERR_SUBJECT_ISSUER_MISMATCH)
    (30 :X509_V_ERR_AKID_SKID_MISMATCH)
    (31 :X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH)
    (32 :X509_V_ERR_KEYUSAGE_NO_CERTSIGN)
    (50 :X509_V_ERR_APPLICATION_VERIFICATION)))

(defun ssl-verify-error-keyword (code)
  (cadr (assoc code *ssl-verify-error-alist*)))

(defun ssl-verify-error-code (keyword)
  (caar (member keyword *ssl-verify-error-alist* :key #'cadr)))

(define-condition ssl-error-verify (ssl-error)
  ((stream :initarg :stream
           :reader ssl-error-stream
           :documentation "The SSL stream whose peer certificate didn't verify.")
   (error-code :initarg :error-code
               :reader ssl-error-code
               :documentation "The peer certificate verification error code
(as returned by functions like SSL_get_verify_result or X509_STORE_CTX_get_error)."))
  (:report (lambda (condition stream)
             (let ((code (ssl-error-code condition)))
               (format stream "SSL verify error: ~d~@[ ~a~]"
                       code (ssl-verify-error-keyword code)))))
  (:documentation "This condition is signalled on SSL connection when a peer certificate doesn't verify."))

(define-condition ssl-error-call (ssl-error)
  ((message :initarg :message))
  (:documentation
   "A failure in the SSL library occurred..")
  (:report (lambda (condition stream)
             (format stream "A failure in OpenSSL library occurred~@[: ~A~]. "
                     (slot-value condition 'message))
             (format-ssl-error-queue stream condition))))

(define-condition asn1-error (tls-error)
  ()
  (:documentation "Asn1 syntax error"))

(define-condition invalid-asn1-string (tls-error)
  ((type :initarg :type :initform nil))
  (:documentation "ASN.1 string parsing/validation error")
  (:report (lambda (condition stream)
             (format stream "ASN.1 syntax error: invalid asn1 string (expected type ~a)" (slot-value condition 'type))))) ;; TODO: when moved to grovel use enum symbol here

(define-condition server-certificate-missing (tls-error simple-error)
  ()
  (:documentation "SSL server didn't present a certificate"))

;;; Waiting for output to be possible

(defun seconds-until-deadline (deadline)
  (/ (- deadline (get-internal-real-time))
     internal-time-units-per-second))

(defun output-wait (stream fd deadline)
  (declare (ignore stream))
  (let ((timeout
         ;; *deadline* is handled by wait-until-fd-usable automatically,
         ;; but we need to turn a user-specified deadline into a timeout
         (when deadline
           (seconds-until-deadline deadline))))
    (sb-sys:wait-until-fd-usable fd :output timeout)))

;;; Waiting for input to be possible

(defun input-wait (stream fd deadline)
  (declare (ignore stream))
  (let ((timeout
         ;; *deadline* is handled by wait-until-fd-usable automatically,
         ;; but we need to turn a user-specified deadline into a timeout
         (when deadline
           (seconds-until-deadline deadline))))
    (sb-sys:wait-until-fd-usable fd :input timeout)))

;;; Funcall wrapper

(declaim (inline ensure-ssl-funcall))
(defun ensure-ssl-funcall (stream success-test func handle &rest other-args)
  (loop
     (let ((ret
            (let ((*bio-socket* (ssl-stream-socket stream))) ;for Lisp-BIO callbacks
              (apply func handle other-args))))
       (when (funcall success-test ret)
         (return ret))
       (let ((error (ssl-get-error handle ret)))
         (case error
           (#.+ssl-error-want-read+
            (input-wait stream
                        (ssl-get-fd handle)
                        (ssl-stream-deadline stream)))
           (#.+ssl-error-want-write+
            (output-wait stream
                         (ssl-get-fd handle)
                         (ssl-stream-deadline stream)))
           (t
            (ssl-signal-error handle func error ret)))))))

(declaim (inline nonblocking-ssl-funcall))
(defun nonblocking-ssl-funcall (stream success-test func handle &rest other-args)
  (loop
     (let ((ret
            (let ((*bio-socket* (ssl-stream-socket stream))) ;for Lisp-BIO callbacks
              (apply func handle other-args))))
       (when (funcall success-test ret)
         (return ret))
       (let ((error (ssl-get-error handle ret)))
         (case error
           ((#.+ssl-error-want-read+ #.+ssl-error-want-write+)
            (return ret))
           (t
            (ssl-signal-error handle func error ret)))))))




;; Default Cipher List

(defvar *default-cipher-list* nil)

(defparameter *default-buffer-size* 2048
  "The default size for input and output buffers of SSL-STREAM objects")

(defclass ssl-stream (fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  ((ssl-stream-socket
    :initarg :socket
    :accessor ssl-stream-socket)
   (close-callback
    :initarg :close-callback
    :accessor ssl-close-callback)
   (handle
    :initform nil
    :accessor ssl-stream-handle)
   (deadline
    :initform nil
    :initarg :deadline
    :accessor ssl-stream-deadline)
   (output-buffer
    :accessor ssl-stream-output-buffer)
   (output-pointer
    :initform 0
    :accessor ssl-stream-output-pointer)
   (input-buffer
    :accessor ssl-stream-input-buffer)
   (peeked-byte
    :initform nil
    :accessor ssl-stream-peeked-byte)))

(defun make-buffer (size)
  (ffi:make-shareable-byte-vector size))

(defun s/b-replace (seq buf &key (start1 0) end1 (start2 0) end2)
  (replace seq buf :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defun b/s-replace (buf seq &key (start1 0) end1 (start2 0) end2)
  (replace buf seq :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defmethod initialize-instance :after ((stream ssl-stream)
                                       &key
                                       (buffer-size *default-buffer-size*)
                                       (input-buffer-size buffer-size)
                                       (output-buffer-size buffer-size)
                                       &allow-other-keys)
  (setf (ssl-stream-output-buffer stream)
        (make-buffer output-buffer-size))
  (setf (ssl-stream-input-buffer stream)
        (make-buffer input-buffer-size)))

(defmethod print-object ((object ssl-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~A" (ssl-stream-socket object))))

(defclass ssl-server-stream (ssl-stream)
  ((certificate
    :initarg :certificate
    :accessor ssl-stream-certificate)
   (key
    :initarg :key
    :accessor ssl-stream-key)))

(defmethod stream-element-type ((stream ssl-stream))
  'u8)

(defmethod close ((stream ssl-stream) &key abort)
  (cond
    ((ssl-stream-handle stream)
     (unless abort
       (force-output stream)
       (ensure-ssl-funcall stream
                           (complement #'minusp)
                           #'ssl-shutdown
                           (ssl-stream-handle stream)))
     (ssl-free (ssl-stream-handle stream))
     (setf (ssl-stream-handle stream) nil)
     (when (streamp (ssl-stream-socket stream))
       (close (ssl-stream-socket stream) :abort abort))
     (when (ssl-close-callback stream)
       (funcall (ssl-close-callback stream)))
     t)
    (t
     nil)))

(defmethod open-stream-p ((stream ssl-stream))
  (and (ssl-stream-handle stream) t))

(defmethod stream-listen ((stream ssl-stream))
  (or (ssl-stream-peeked-byte stream)
      (setf (ssl-stream-peeked-byte stream)
            (let* ((buf (ssl-stream-input-buffer stream))
                   (handle (ssl-stream-handle stream))
                   (*bio-blockp* nil) ;; for the Lisp-BIO
                   (n (ffi:with-pointer-to-vector-data (ptr buf)
                        (nonblocking-ssl-funcall
                         stream #'plusp #'ssl-read handle ptr 1))))
              (and (> n 0) (elt buf 0))))))

(defmethod stream-read-byte ((stream ssl-stream))
  (or (prog1
          (ssl-stream-peeked-byte stream)
        (setf (ssl-stream-peeked-byte stream) nil))
      (handler-case
          (let ((buf (ssl-stream-input-buffer stream))
                (handle (ssl-stream-handle stream)))
            (ffi:with-pointer-to-vector-data (ptr buf)
              (ensure-ssl-funcall
               stream #'plusp #'ssl-read handle ptr 1))
            (elt buf 0))
        (ssl-error-zero-return ()     ;SSL_read returns 0 on end-of-file
          :eof))))

(defmethod stream-read-sequence ((stream ssl-stream) seq &optional start end)
  (let ((start (or start 0))
        (end (or end (length seq))))
    (when (and (< start end) (ssl-stream-peeked-byte stream))
      (setf (elt seq start) (ssl-stream-peeked-byte stream))
      (setf (ssl-stream-peeked-byte stream) nil)
      (incf start))
    (let ((buf (ssl-stream-input-buffer stream))
          (handle (ssl-stream-handle stream)))
      (loop
        for length = (min (- end start) (length buf))
        while (plusp length)
        do
           (handler-case
               (let ((read-bytes
                       (ffi:with-pointer-to-vector-data (ptr buf)
                         (ensure-ssl-funcall
                          stream #'plusp #'ssl-read handle ptr length))))
                 (s/b-replace seq buf :start1 start :end1 (+ start read-bytes))
                 (incf start read-bytes))
             (ssl-error-zero-return ()
               (return))))
      start)))

(defmethod stream-write-byte ((stream ssl-stream) b)
  (let ((buf (ssl-stream-output-buffer stream)))
    (when (eql (length buf) (ssl-stream-output-pointer stream))
      (force-output stream))
    (setf (elt buf (ssl-stream-output-pointer stream)) b)
    (incf (ssl-stream-output-pointer stream)))
  b)

(defmethod stream-write-sequence ((stream ssl-stream) seq &optional start end)
  (let ((buf (ssl-stream-output-buffer stream))
        (start (or start 0))
        (end (or end (length seq))))
    (when (> (+ (- end start) (ssl-stream-output-pointer stream)) (length buf))
      (force-output stream)
      (while (> (- end start) (length buf))
        (b/s-replace buf seq :start2 start)
        (incf start (length buf))
        (setf (ssl-stream-output-pointer stream) (length buf))
        (force-output stream)))
    (b/s-replace buf seq
                 :start1 (ssl-stream-output-pointer stream)
                 :start2 start
                 :end2 end)
    (incf (ssl-stream-output-pointer stream) (- end start)))
  seq)

(defmethod stream-finish-output ((stream ssl-stream))
  (stream-force-output stream))

(defmethod stream-force-output ((stream ssl-stream))
  (let ((buf (ssl-stream-output-buffer stream))
        (fill-ptr (ssl-stream-output-pointer stream))
        (handle (ssl-stream-handle stream)))
    (when (plusp fill-ptr)
      (unless handle
        (error "output operation on closed SSL stream"))
      (ffi:with-pointer-to-vector-data (ptr buf)
        (ensure-ssl-funcall stream #'plusp #'ssl-write handle ptr fill-ptr))
      (setf (ssl-stream-output-pointer stream) 0))))

#+(and clozure-common-lisp (not windows))
(defun install-nonblock-flag (fd)
  (ccl::fd-set-flags fd (logior (ccl::fd-get-flags fd)
                                ;; read-from-string is necessary because
                                ;; CLISP and perhaps other Lisps are confused
                                ;; by #$, signaling
                                ;; "undefined dispatch character $",
                                ;; even though the defun in conditionalized by
                                ;; #+clozure-common-lisp
                                #.(read-from-string "#$O_NONBLOCK"))))

#+(and sbcl (not win32))
(defun install-nonblock-flag (fd)
  (sb-posix:fcntl fd
                  sb-posix::f-setfl
                  (logior (sb-posix:fcntl fd sb-posix::f-getfl)
                          sb-posix::o-nonblock)))

#-(or (and clozure-common-lisp (not windows)) sbcl)
(defun install-nonblock-flag (fd)
  (declare (ignore fd)))

#+(and sbcl win32)
(defun install-nonblock-flag (fd)
  (when (boundp 'sockint::fionbio)
    (sockint::ioctl fd sockint::fionbio 1)))

;;; interface functions
;;;

(defvar *default-unwrap-stream-p* t
  "Default value for UNWRAP-STREAM-P function parameter.

If true (the default), epsilon.net.tls will try to extract file descriptor
from the given TCP Lisp stream and tell OpenSSL to use a socket BIO
based on that file descriptor;
otherwise use a Lisp BIO wrapping the TCP Lisp stream.")

(defun install-handle-and-bio (stream handle socket unwrap-stream-p)
  (setf (ssl-stream-handle stream) handle)
  (when unwrap-stream-p
    (let ((fd (stream-fd socket)))
      (when fd
        (setf socket fd))))
  (etypecase socket
    (integer
     (install-nonblock-flag socket)
     (ssl-set-fd handle socket))
    (stream
     (ssl-set-bio handle (bio-new-lisp) (bio-new-lisp))))

  ;; The below call setting +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+ mode
  ;; existed since commit 5bd5225.
  ;; It is implemented wrong - ssl-ctx-ctrl expects
  ;; a context as the first parameter, not handle.
  ;; It was lucky to not crush on Linux and Windows,
  ;; untill crash was detedcted on OpenBSD + LibreSSL.
  ;; See https://github.com/cl-plus-ssl/cl-plus-ssl/pull/42.
  ;; We keep this code commented but not removed because
  ;; we don't know what David Lichteblau meant when
  ;; added this - maybe he has some idea?
  ;; (Although modifying global context is a bad
  ;; thing to do for install-handle-and-bio function,
  ;; also we don't see a need for movable buffer -
  ;; we don't repeat calls to ssl functions with
  ;; moved buffer).
  ;;
  ;; (ssl-ctx-ctrl handle
  ;;   +SSL_CTRL_MODE+
  ;;   +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
  ;;   (ffi:null-pointer))

  socket)

(defun install-key-and-cert (handle key certificate)
  (when certificate
    (unless (eql 1 (ssl-use-certificate-file handle
                                             certificate
                                             +ssl-filetype-pem+))
      (error 'ssl-error-initialize
             :reason (format nil "Can't load certificate ~A" certificate))))
  (when key
    (unless (eql 1 (ssl-use-privatekey-file handle
                                            key
                                            +ssl-filetype-pem+))
      (error 'ssl-error-initialize :reason (format nil "Can't load private key file ~A" key)))))

(defun x509-certificate-names (x509-certificate)
  (unless (ffi:null-pointer-p x509-certificate)
    (ffi:with-foreign-pointer (buf 1024)
      (let ((issuer-name (x509-get-issuer-name x509-certificate))
            (subject-name (x509-get-subject-name x509-certificate)))
        (values
         (unless (ffi:null-pointer-p issuer-name)
           (x509-name-oneline issuer-name buf 1024)
           (ffi:foreign-string-to-lisp buf))
         (unless (ffi:null-pointer-p subject-name)
           (x509-name-oneline subject-name buf 1024)
           (ffi:foreign-string-to-lisp buf)))))))

(defmethod ssl-stream-handle ((stream char-stream)) ; TODO is this right ?
  (ssl-stream-handle (binary-stream stream)))

(defun ssl-stream-x509-certificate (ssl-stream)
  (compat-ssl-get1-peer-certificate (ssl-stream-handle ssl-stream)))

(defun ssl-load-global-verify-locations (&rest pathnames)
  "PATHNAMES is a list of pathnames to PEM files containing server and CA certificates.
Install these certificates to use for verifying on all SSL connections.
After RELOAD, you need to call this again."
  (ensure-initialized)
  (dolist (path pathnames)
    (ffi:with-foreign-strings ((cafile path))
      (unless (eql 1 (ssl-ctx-load-verify-locations
                      *ssl-global-context*
                      cafile
                      (ffi:null-pointer)))
        (error "ssl-ctx-load-verify-locations failed.")))))

(defun ssl-set-global-default-verify-paths ()
  "Load the system default verification certificates.
After RELOAD, you need to call this again."
  (ensure-initialized)
  (unless (eql 1 (ssl-ctx-set-default-verify-paths *ssl-global-context*))
    (error "ssl-ctx-set-default-verify-paths failed.")))

(defun ssl-check-verify-p ()
  "DEPRECATED. Use the (MAKE-SSL-CLIENT-STREAM .. :VERIFY ?) to enable/disable verification.
Also, MAKE-CONTEXT has :VERIFY-MODE option.

Return true if SSL connections will error if the certificate doesn't verify."
  (and *ssl-check-verify-p* (not (eq *ssl-check-verify-p* :unspecified))))

(defun (setf ssl-check-verify-p) (check-verify-p)
  "DEPRECATED. Use the (MAKE-SSL-CLIENT-STREAM .. :VERIFY ?) to enable/disable verification.
Also, MAKE-CONTEXT has :VERIFY-MODE option.

If CHECK-VERIFY-P is true, signal connection errors if the server certificate doesn't verify."
  (setf *ssl-check-verify-p* (not (null check-verify-p))))

(defun ssl-verify-init (&key
                        (verify-depth nil)
                        (verify-locations nil))
  "DEPRECATED.
Use the (MAKE-SSL-CLIENT-STREAM .. :VERIFY ?) to enable/disable verification.
Use (MAKE-CONTEXT ... :VERIFY-LOCATION ? :VERIFY-DEPTH ?) to control the verification depth and locations.
MAKE-CONTEXT also allows to enab/disable verification."
  (check-type verify-depth (or null integer))
  (ensure-initialized)
  (when verify-depth
    (ssl-ctx-set-verify-depth *ssl-global-context* verify-depth))
  (when verify-locations
    (apply #'ssl-load-global-verify-locations verify-locations)
    ;; This makes (setf (ssl-check-verify) nil) persistent
    (unless (null *ssl-check-verify-p*)
      (setf (ssl-check-verify-p) t))
    t))

(defun maybe-verify-client-stream (ssl-stream verify-mode hostname)
  ;; VERIFY-MODE is one of NIL, :OPTIONAL, :REQUIRED
  ;; HOSTNAME is either NIL or a string.
  (when verify-mode
    (let* ((handle (ssl-stream-handle ssl-stream))
           (srv-cert (compat-ssl-get1-peer-certificate handle)))
      (unwind-protect
           (progn
             (when (and (eq :required verify-mode)
                        (ffi:null-pointer-p srv-cert))
               (error 'server-certificate-missing
                      :format-control "The server didn't present a certificate."))
             (let ((err (ssl-get-verify-result handle)))
               (unless (eql err +x509-v-ok+)
                 (error 'ssl-error-verify :stream ssl-stream :error-code err)))
             (when (and hostname
                        (not (ffi:null-pointer-p srv-cert)))
               (or (verify-hostname srv-cert hostname)
                   ;; verify-hostname must either return true
                   ;; or signal an error
                   (error "Unexpected NIL returned by epsilon.net.tls:VERIFY-HOSTNAME for ~A"
                          hostname))))
        (unless (ffi:null-pointer-p srv-cert)
          (x509-free srv-cert))))))

(defmacro with-new-ssl ((var) &body body)
  (with-gensyms (ssl)
    `(let* ((,ssl (ssl-new *ssl-global-context*))
            (,var ,ssl))
       (when (ffi:null-pointer-p ,ssl)
         (error 'ssl-error-call :message "Unable to create SSL structure" :queue (read-ssl-error-queue)))
       (handler-bind ((error (lambda (_)
                               (declare (ignore _))
                               (ssl-free ,ssl))))
         ,@body))))

(defvar *make-ssl-client-stream-verify-default*
  (if (member :windows *features*)
      ;; On Windows we can't yet initialize context with
      ;; trusted certifying authorities from system configuration.
      ;; ssl-ctx-set-default-verify-paths only helps
      ;; on Unix-like platforms.
      ;; See https://github.com/cl-plus-ssl/cl-plus-ssl/issues/54.
      nil
      :required)
  "Helps to mitigate the change in default behaviour of
MAKE-SSL-CLIENT-STREAM - previously it worked as if :VERIFY NIL
but then :VERIFY :REQUIRED became the default on non-Windows platforms.
Change this variable if you want the previous behaviour.")

(defun make-alpn-proto-string (protocols)
  "Convert list of protocol names to the wire-format byte string."
  (with-output-to-string (s)
    (dolist (proto protocols)
      (check-type proto string)
      (write-char (code-char (length proto)) s)
      (write-string proto s))))


(defvar *pem-password* ""
  "The callback registered with SSL_CTX_set_default_passwd_cb
will use this value.")

;; The macro to be used by other code to provide password
;; when loading PEM file.
(defmacro with-pem-password ((password) &body body)
  `(let ((*pem-password* (or ,password "")))
     ,@body))



;; fixme: free the context when errors happen in this function
(defun make-ssl-client-stream (socket
                               &key
                                 (unwrap-stream-p *default-unwrap-stream-p*)
                                 hostname
                                 close-callback
                                 (verify (if (ssl-check-verify-p)
                                             :optional
                                             *make-ssl-client-stream-verify-default*))
                                 alpn-protocols
                                 certificate key password
                                 (cipher-list *default-cipher-list*)
                                 method
                                 (buffer-size *default-buffer-size*)
                                 (input-buffer-size buffer-size)
                                 (output-buffer-size buffer-size))
  "Performs TLS/SSL handshake over the specified SOCKET using
the SSL_connect OpenSSL function and returns a Lisp stream that
uses OpenSSL library to encrypt the output data when sending
it to the socket and to decrypt the input received.

Uses a global SSL_CTX instance, which can be overriden
by WITH-GLOBAL-CONTEXT. (The global SSL_CTX is
passed as a parameter to an internall call of SSL_new.)

    SOCKET - represents the socket to be wrapped into an SSL stream.
        Can be either a Lisp stream (of an implementation-dependent type) for that
        socket, or an integer file descriptor of that socket. If that's a
        stream, it will be closed automatically when the SSL stream
        is closed. Also, on CCL, (CCL:STREAM-DEADLINE SOCKET) will be used
        as a deadline for 'socket BIO' mode.
        See README.md / Usage / Timeouts and Deadlines for more information.
        If that's a file descriptor, it is not closed automatically
        (you can use CLOSE-CALLBACK to arrange for that).

    UNWRAP-STREAM-P - if true, (STREAM-FD SOCKET) will be attempted
        to extract the file descriptor. Otherwise the SOCKET
        is left as is. Anyway, if in result we end up with an integer
        file descriptor, a socket BIO is used; if we end up with a
        stream - Lisp BIO is used. This parameter defaults to
        *DEFAULT-UNWRAP-STREAM-P* which is initalized to true.
        See README.md / Usage for more information on BIO types.

    HOSTNAME if specified, will be sent by client during TLS negotiation,
        according to the Server Name Indication (SNI) extension to the TLS.
        If we connect to a server handling multiple domain names,
        this extension enables such server to choose certificate for the
        right domain. Also the HOSTNAME is used for hostname verification
        (if verification is enabled by VERIFY).

    CLOSE-CALLBACK - a function to be called when the created
        ssl stream is CL:CLOSE'ed. The only argument is this ssl stream.

    VERIFY can be specified either as NIL if no check should be performed,
        :OPTIONAL to verify the server's certificate if server presents one or
        :REQUIRED to verify the server's certificate and fail if an invalid
        or no certificate was presented. Defaults to
        *MAKE-SSL-CLIENT-STREAM-VERIFY-DEFAULT* which is initialized
        to :REQUIRED

        The verification includes verifying the HOSTNAME against the server
        ceritificate, using the VERIFY-HOSTNAME function.

        An error is signalled in case of the certificate or hostname
        verification failure.

        Note, the VERIFY logic expects that the global
        SSL_CTX object does not have the SSL_VERIFY_PEER
        flag enabled - the default for the epsilon.net.tls's global SSL_CTX.
        If the current global SSL_CTX object has SSL_VERIFY_PEER enabled,
        the SSL_Connect will perform certificate (but not hostname)
        verification on its own, and an error will be signalled for a
        bad certificate even with :VERIFY NIL.

    ALPN-PROTOCOLS, if specified, should be a list of alpn protocol names,
        such as \"h2\", that will be offered to the server. The protocol
        selected by the server can be retrieved with
        GET-SELECTED-ALPN-PROTOCOL.

    CERTIFICATE is the path to a file containing a PEM-encoded certificate.
        Note, if one certificate will be used for multiple TLS connections,
        it's better to load it into a common SSL_CTX (context) object rather
        than reading it for every new connection.

    KEY is the path to a PEM-encoded private key file of that certificate.

    PASSWORD the password to use for decryptipon of the KEY (if encrypted).

    CIPHER-LIST - If not NIL, must be a string to pass to SSL_set_cipher_list.
        An ERROR is signalled if SSL_CTX_set_cipher_list fails.
        Defaults to *DEFAULT-CIPHER-LIST* which is initialized to NIL.

    METHOD - usually you want to leave the default value. It is used
        to compute the parameter for OpenSSL function SSL_CTX_new when
        creating the global SSL_CTX object for epsilon.net.tls. This parameter only has
        effect on the first call, when the global SSL_CTX is not yet created.
        The default value is TLS_method on OpenSSL > 1.1.0 and SSLv23_method
        for older OpenSSL versions.

    BUFFER-SIZE - default value for both the INPUT-BUFFER-SIZE and
        OUTPUT-BUFFER-SIZE parameters. In turn defaults to the
        *DEFAULT-BUFFER-SIZE* special variable.

    INPUT-BUFFER-SIZE - size of the input buffer of the ssl stream.
        Defaults to the BUFFER-SIZE parameter.

    OUTPUT-BUFFER-SIZE - size of the output buffer of the ssl stream.
        Defaults to the BUFFER-SIZE parameter.
"
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-stream
                               :socket socket
                               :close-callback close-callback
                               :input-buffer-size input-buffer-size
                               :output-buffer-size output-buffer-size)))
    (with-new-ssl (handle)
      (if hostname
          (ffi:with-foreign-string (chostname hostname)
            (ssl-set-tlsext-host-name handle chostname)))
      (if alpn-protocols
          (ffi:with-foreign-string ((string len) (make-alpn-proto-string alpn-protocols))
            (ssl-set-alpn-protos handle string (1- len))))
      (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
      (ssl-set-connect-state handle)
      (when (and cipher-list
                 (zerop (ssl-set-cipher-list handle cipher-list)))
        (error 'ssl-error-initialize
               :reason
               "Can't set SSL cipher list: SSL_set_cipher_list returned 0"))
      (with-pem-password (password)
        (install-key-and-cert handle key certificate))
      (collecting-verify-error (handle)
        (ensure-ssl-funcall stream #'plusp #'ssl-connect handle))
      (maybe-verify-client-stream stream verify hostname)
      stream)))

;; fixme: free the context when errors happen in this function
(defun make-ssl-server-stream (socket
                               &key
                                 (unwrap-stream-p *default-unwrap-stream-p*)
                                 close-callback
                                 certificate key password
                                 (cipher-list *default-cipher-list*)
                                 method
                                 (buffer-size *default-buffer-size*)
                                 (input-buffer-size buffer-size)
                                 (output-buffer-size buffer-size))
  "Performs server-side TLS handshake over the specified SOCKET using
the SSL_accept OpenSSL function and returns a Lisp stream that
uses OpenSSL library to encrypt the output data when sending
it to the socket and to decrypt the input received.

Uses a global SSL_CTX instance, which can be overriden
by WITH-GLOBAL-CONTEXT. (The global SSL_CTX is
passed as a parameter to an internall call of SSL_new.)

All parameters have the same meaning as documented
for MAKE-SSL-CLIENT-STREAM.
"
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-server-stream
                               :socket socket
                               :close-callback close-callback
                               :certificate certificate
                               :key key
                               :input-buffer-size input-buffer-size
                               :output-buffer-size output-buffer-size)))
    (with-new-ssl (handle)
      (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
      (ssl-set-accept-state handle)
      (when (and cipher-list
                 (zerop (ssl-set-cipher-list handle cipher-list)))
        (error 'ssl-error-initialize
               :reason
               "Can't set SSL cipher list: SSL_set_cipher_list returned 0"))
      (with-pem-password (password)
        (install-key-and-cert handle key certificate))
      (collecting-verify-error (handle)
        (ensure-ssl-funcall stream #'plusp #'ssl-accept handle))
       stream)))

(defun get-selected-alpn-protocol (ssl-stream)
  "A wrapper around SSL_get0_alpn_selected.
Returns the ALPN protocol selected by server, or NIL if none was selected.

SSL-STREAM is the client ssl stream returned by make-ssl-client-stream. "
  (ffi:with-foreign-objects ((ptr :pointer) (len :pointer))
    (ssl-get0-alpn-selected (ssl-stream-handle ssl-stream) ptr len)
    (ffi:foreign-string-to-lisp (ffi:mem-ref ptr :pointer)
                                 :count (ffi:mem-ref len :int))))

(defgeneric stream-fd (stream)
  (:documentation "The STREAM's file descriptor as an integer,
if known / implemented for the current lisp.
Otherwise the STREAM itself. The result of this function can be
passed to MAKE-SSL-CLIENT-STREAM and MAKE-SSL-SERVER-STREAM."))
(defmethod stream-fd (stream) stream)

(defmethod stream-fd ((stream sb-sys:fd-stream))
  (sb-sys:fd-stream-fd stream))

(ffi:defcallback tmp-rsa-callback :pointer ((ssl :pointer) (export-p :int) (key-length :int))
  (declare (ignore ssl export-p))
  (flet ((rsa-key (length)
           (rsa-generate-key length
                             +RSA_F4+
                             (ffi:null-pointer)
                             (ffi:null-pointer))))
    (cond ((= key-length 512)
           (unless *tmp-rsa-key-512*
             (setf *tmp-rsa-key-512* (rsa-key key-length)))
           *tmp-rsa-key-512*)
          ((= key-length 1024)
           (unless *tmp-rsa-key-1024*
             (setf *tmp-rsa-key-1024* (rsa-key key-length)))
           *tmp-rsa-key-1024*)
          (t
           (unless *tmp-rsa-key-2048*
             (setf *tmp-rsa-key-2048* (rsa-key key-length)))
           *tmp-rsa-key-2048*))))


;;; Encrypted PEM files support
;;;

;; based on http://www.openssl.org/docs/ssl/SSL_CTX_set_default_passwd_cb.html

;; The callback itself
(ffi:defcallback pem-password-callback :int
    ((buf :pointer) (size :int) (rwflag :int) (unused :pointer))
  (declare (ignore rwflag unused))
  (let* ((password-str (coerce *pem-password* 'base-string))
         (tmp (ffi:foreign-string-alloc password-str)))
    (ffi:foreign-funcall "strncpy"
                          :pointer buf
                          :pointer tmp
                          :int size)
    (ffi:foreign-string-free tmp)
    (setf (ffi:mem-ref buf :char (1- size)) 0)
    (ffi:foreign-funcall "strlen" :pointer buf :int)))


;;; Initialization
;;;

(defun init-prng (seed-byte-sequence)
  (let* ((length (length seed-byte-sequence))
         (buf (ffi:make-shareable-byte-vector length)))
    (dotimes (i length)
      (setf (elt buf i) (elt seed-byte-sequence i)))
    (ffi:with-pointer-to-vector-data (ptr buf)
      (rand-seed ptr length))))

(defvar *locks*)

;; zzz as of early 2011, bxthreads is totally broken on SBCL wrt. explicit
;; locking of recursive locks.  with-recursive-lock works, but acquire/release
;; don't.  Hence we use non-recursize locks here (but can use a recursive
;; lock for the global lock).

(ffi:defcallback locking-callback :void
    ((mode :int)
     (n :int)
     (file :pointer) ;; could be (file :string), but we don't use FILE, so avoid the conversion
     (line :int))
  (declare (ignore file line))
  ;; (assert (logtest mode (logior +CRYPTO-READ+ +CRYPTO-WRITE+)))
  (let ((lock (elt *locks* n)))
    (cond
      ((logtest mode +CRYPTO-LOCK+)
       (acquire-lock lock))
      ((logtest mode +CRYPTO-UNLOCK+)
       (release-lock lock))
      (t
       (error "fell through")))))

(defvar *threads* (make-hash-table :weakness :key))
(defvar *thread-counter* 0)

(defparameter *global-lock*
  (make-recursive-lock "SSL initialization"))

;; zzz BUG: On a 32-bit system and under non-trivial load, this counter
;; is likely to wrap in less than a year.
(ffi:defcallback threadid-callback :unsigned-long ()
  (with-recursive-lock-held (*global-lock*)
    (let ((self (current-thread)))
      (or (gethash self *threads*)
          (setf (gethash self *threads*)
                (incf *thread-counter*))))))

(defun default-ssl-method ()
  (if (openssl-is-at-least 1 1)
      'tls-method
      'ssl-v23-method))

(defun initialize (&key method rand-seed)
  (when (or (openssl-is-not-even 1 1)
            ;; Old versions of LibreSSL
            ;; require this initialization
            ;; (https://github.com/cl-plus-ssl/cl-plus-ssl/pull/91),
            ;; new versions keep this API backwards
            ;; compatible so we can call it too.
            (libresslp))
    (setf *locks* (loop
                     repeat (crypto-num-locks)
                     collect (make-lock)))
    (crypto-set-locking-callback (ffi:callback locking-callback))
    (crypto-set-id-callback (ffi:callback threadid-callback))
    (ssl-load-error-strings)
    (ssl-library-init)
    ;; However, for OpenSSL_add_all_digests the LibreSSL breaks
    ;; the backward compatibility by removing the function.
    ;; https://github.com/cl-plus-ssl/cl-plus-ssl/pull/134
    (unless (libresslp)
      (openssl-add-all-digests)))

  (bio-init)

  (when rand-seed
    (init-prng rand-seed))
  (setf *ssl-check-verify-p* :unspecified)
  (setf *ssl-global-method* (funcall (or method (default-ssl-method))))
  (setf *ssl-global-context* (ssl-ctx-new *ssl-global-method*))
  (unless (eql 1 (ssl-ctx-set-default-verify-paths *ssl-global-context*))
    (error "ssl-ctx-set-default-verify-paths failed."))
  (ssl-ctx-set-session-cache-mode *ssl-global-context* 3)
  (ssl-ctx-set-default-passwd-cb *ssl-global-context*
                                 (ffi:callback pem-password-callback))
  (when (or (openssl-is-not-even 1 1)
            ;; Again, even if newer LibreSSL
            ;; don't need this call, they keep
            ;; the API compatibility so we can continue
            ;; making this call.
            (libresslp))
    (ssl-ctx-set-tmp-rsa-callback *ssl-global-context*
                                  (ffi:callback tmp-rsa-callback))))

(defun ensure-initialized (&key method (rand-seed nil))
  "In most cases you do *not* need to call this function, because it
is called automatically by all other functions. The only reason to
call it explicitly is to supply the RAND-SEED parameter. In this case
do it before calling any other functions.

Keyword arguments:

    METHOD - just leave the default value.

    RAND-SEED - an octet sequence to initialize OpenSSL random
        number generator. On many platforms, including Linux and
        Windows, it may be left NIL (default), because OpenSSL
        initializes the random number generator from OS specific
        service. But, for example, on Solaris it may be necessary
        to supply this value. The minimum length required by OpenSSL
        is 128 bits.
        See http://www.openssl.org/support/faq.html#USER1 for details.

        Hint: do not use Common Lisp RANDOM function to generate
        the RAND-SEED, because the function usually returns
        predictable values.
"
  (with-recursive-lock-held (*global-lock*)
    (unless (ssl-initialized-p)
      (initialize :method method :rand-seed rand-seed))))

(defun use-certificate-chain-file (certificate-chain-file)
  "Applies OpenSSL function SSL_CTX_use_certificate_chain_file
to the epsilon.net.tls's global SSL_CTX object and the specified
CERTIFICATE-CHAIN-FILE.

OpenSSL requires the certificates in the file to be sorted
starting with the subject's certificate (actual client or
server certificate), followed by intermediate CA certificates
if applicable, and ending at the highest level (root) CA.

Note: the RELOAD function clears the global context and in particular
the loaded certificate chain."
  (ensure-initialized)
  (ssl-ctx-use-certificate-chain-file *ssl-global-context* certificate-chain-file))

(defun use-private-key-file (private-key-file)
  "Applies OpenSSL function SSL_CTX_use_PrivateKey_file
to the epsilon.net.tls's global SSL_CTX object and the specified
PRIVATE-KEY-FILE.

Note: the RELOAD function clears the global context and in particular
the loaded private key."
  (ensure-initialized)
  (ssl-ctx-use-privatekey-file *ssl-global-context* private-key-file :ssl-filetype-pem))

(defun reload ()
  "If you save your application as a Lisp image,
call this function when that image is loaded,
to perform the necessary epsilon.net.tls re-initialization
(unless your lisp implementation automatically
re-loads foreign libraries and preserves their
memory accross image reloads).

This should work fine if the location and version of the
OpenSSL shared libraries have not changed.
If they have changed, you may get errors, as users report:
https://github.com/cl-plus-ssl/cl-plus-ssl/issues/167
"
  (detect-custom-openssl-installations-if-macos)
  (unless (member :epsilon.net.tls-foreign-libs-already-loaded
                  *features*)
    (ffi:use-foreign-library libcrypto)
    (ffi:load-foreign-library 'libssl))
  (setf *ssl-global-context* nil)
  (setf *ssl-global-method* nil)
  (setf *tmp-rsa-key-512* nil)
  (setf *tmp-rsa-key-1024* nil))

(define-condition hostname-verification-error (error)
  ())

(define-condition unable-to-match-altnames (hostname-verification-error)
  ())

(define-condition unable-to-decode-common-name (hostname-verification-error)
  ())

(define-condition unable-to-match-common-name (hostname-verification-error)
  ())

(defun case-insensitive-match (name hostname)
  (string-equal name hostname))

(defun remove-trailing-dot (string)
  (string-right-trim '(#\.) string))

(defun check-wildcard-in-leftmost-label (identifier wildcard-pos)
  (when-let ((dot-pos (position #\. identifier)))
    (> dot-pos wildcard-pos)))

(defun check-single-wildcard (identifier wildcard-pos)
  (not (find #\* identifier :start (1+ wildcard-pos))))

(defun check-two-labels-after-wildcard (after-wildcard)
  ;;at least two dots(in fact labels since we remove trailing dot first) after wildcard
  (when-let ((first-dot-aw-pos (position #\. after-wildcard)))
    (and (find #\. after-wildcard :start (1+ first-dot-aw-pos))
         first-dot-aw-pos)))

(defun validate-and-parse-wildcard-identifier (identifier hostname)
  (when-let ((wildcard-pos (position #\* identifier)))
    (when (and (>= (length hostname) (length identifier)) ;; wildcard should constiute at least one character
               (check-wildcard-in-leftmost-label identifier wildcard-pos)
               (check-single-wildcard identifier wildcard-pos))
      (let ((after-wildcard (subseq identifier (1+ wildcard-pos)))
            (before-wildcard (subseq identifier 0 wildcard-pos)))
        (when-let ((first-dot-aw-pos (check-two-labels-after-wildcard after-wildcard)))
          (if (and (= 0 (length before-wildcard))     ;; nothing before wildcard
                   (= wildcard-pos first-dot-aw-pos)) ;; i.e. dot follows *
              (values t before-wildcard after-wildcard t)
              (values t before-wildcard after-wildcard nil)))))))

(defun wildcard-not-in-a-label (before-wildcard after-wildcard)
  (let ((after-w-dot-pos (position #\. after-wildcard)))
    (and
     (not (search "xn--" before-wildcard))
     (not (search "xn--" (subseq after-wildcard 0 after-w-dot-pos))))))

(defun try-match-wildcard (before-wildcard after-wildcard single-char-wildcard pattern)
  ;; Compare AfterW part with end of pattern with length (length AfterW)
  ;; was Wildcard the only character in left-most label in identifier
  ;; doesn't matter since parts after Wildcard should match unconditionally.
  ;; However if Wildcard was the only character in left-most label we can't match this *.example.com and bar.foo.example.com
  ;; if i'm correct if it wasn't the only character
  ;; we can match like this: *o.example.com = bar.foo.example.com
  ;; but this is prohibited anyway thanks to check-vildcard-in-leftmost-label
  (if single-char-wildcard
      (let ((pattern-except-left-most-label
             (if-let ((first-hostname-dot-post (position #\. pattern)))
               (subseq pattern first-hostname-dot-post)
               pattern)))
        (case-insensitive-match after-wildcard pattern-except-left-most-label))
      (when (wildcard-not-in-a-label before-wildcard after-wildcard)
        ;; baz*.example.net and *baz.example.net and b*z.example.net would
        ;; be taken to match baz1.example.net and foobaz.example.net and
        ;; buzz.example.net, respectively
        (and
         (case-insensitive-match before-wildcard (subseq pattern 0 (length before-wildcard)))
         (case-insensitive-match after-wildcard (subseq pattern
                                                        (- (length pattern)
                                                           (length after-wildcard))))))))

(defun maybe-try-match-wildcard (name hostname)
  (multiple-value-bind (valid before-wildcard after-wildcard single-char-wildcard)
      (validate-and-parse-wildcard-identifier name hostname)
    (when valid
      (try-match-wildcard before-wildcard after-wildcard single-char-wildcard hostname))))

(defun try-match-hostname (name hostname)
  (let ((name (remove-trailing-dot name))
        (hostname (remove-trailing-dot hostname)))
    (or (case-insensitive-match name hostname)
        (maybe-try-match-wildcard name hostname))))

(defun try-match-hostnames (names hostname)
  (loop for name in names
        when (try-match-hostname name hostname) do
           (return t)))

(defun verify-hostname (cert hostname)
  "Verifies the HOSTNAME against the specified
CERT. Implemented for all OpenSSL versions,
using custom Lisp code (without relying on the functions
provided by newer OpenSSl versions, like SSL_set_verify).

Signals an error in case of verification failure.

Otherwise returns true"
  (or (try-match-hostnames (certificate-dns-alt-names cert)
                           hostname)
      (try-match-hostnames (or (certificate-subject-common-names cert)
                               (error 'unable-to-decode-common-name))
                           hostname)
      (error 'unable-to-match-common-name)))

#|
ASN1 string validation references:
- https://github.com/digitalbazaar/forge/blob/909e312878838f46ba6d70e90264650b05eb8bde/js/asn1.js
- http://www.obj-sys.com/asn1tutorial/node128.html
- https://github.com/deadtrickster/ssl_verify_hostname.erl/blob/master/src/ssl_verify_hostname.erl
- https://golang.org/src/encoding/asn1/asn1.go?m=text
|#
(defgeneric decode-asn1-string (asn1-string type))

(defun copy-bytes-to-lisp-vector (src-ptr vector count)
  (declare (type ->u8 vector)
           (type fixnum count))
  (dotimes (i count vector)
    (setf (aref vector i) (ffi:mem-aref src-ptr :unsigned-char i))))

(defun asn1-string-bytes-vector (asn1-string)
  (let* ((data (asn1-string-data asn1-string))
         (length (asn1-string-length asn1-string))
         (vector (ffi:make-shareable-byte-vector length)))
    (copy-bytes-to-lisp-vector data vector length)
    vector))

(defun asn1-iastring-char-p (byte)
  (declare (type u8 byte))
  (< byte #x80))

(defun asn1-iastring-p (bytes)
  (declare (type ->u8 bytes))
  (every #'asn1-iastring-char-p bytes))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-iastring+)))
  (let ((bytes (asn1-string-bytes-vector asn1-string)))
    (if (asn1-iastring-p bytes)
        (u8-to-string bytes)
        (error 'invalid-asn1-string :type '+v-asn1-iastring+))))

(defun asn1-printable-char-p (byte)
  (declare (type u8 byte))
  (cond
    ;; a-z
    ((and (>= byte #.(char-code #\a))
          (<= byte #.(char-code #\z)))
     t)
    ;; '-/
    ((and (>= byte #.(char-code #\'))
          (<= byte #.(char-code #\/)))
     t)
    ;; 0-9
    ((and (>= byte #.(char-code #\0))
          (<= byte #.(char-code #\9)))
     t)
    ;; A-Z
    ((and (>= byte #.(char-code #\A))
          (<= byte #.(char-code #\Z)))
     t)
    ;; other
    ((= byte #.(char-code #\ )) t)
    ((= byte #.(char-code #\:)) t)
    ((= byte #.(char-code #\=)) t)
    ((= byte #.(char-code #\?)) t)))

(defun asn1-printable-string-p (bytes)
  (declare (type ->u8 bytes))
  (every #'asn1-printable-char-p bytes))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-printablestring+)))
  (let* ((bytes (asn1-string-bytes-vector asn1-string)))
    (if (asn1-printable-string-p bytes)
        (u8-to-string bytes)
        (error 'invalid-asn1-string :type '+v-asn1-printablestring+))))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-utf8string+)))
  (let* ((data (asn1-string-data asn1-string))
         (length (asn1-string-length asn1-string)))
    (ffi:foreign-string-to-lisp data :count length :encoding :utf-8)))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-universalstring+)))
  (if (= 0 (mod (asn1-string-length asn1-string) 4))
      ;; ffi sometimes fails here on sbcl? idk why (maybe threading?)
      ;; fail: Illegal :UTF-32 character starting at position 48...
      ;; when (length bytes) is 48...
      ;; so I'm passing :count explicitly
      (or (ignore-errors (ffi:foreign-string-to-lisp (asn1-string-data asn1-string) :count (asn1-string-length asn1-string) :encoding :utf-32))
          (error 'invalid-asn1-string :type '+v-asn1-universalstring+))
      (error 'invalid-asn1-string :type '+v-asn1-universalstring+)))

(defun asn1-teletex-char-p (byte)
  (declare (type u8 byte))
  (and (>= byte #x20)
       (< byte #x80)))

(defun asn1-teletex-string-p (bytes)
  (declare (type ->u8 bytes))
  (every #'asn1-teletex-char-p bytes))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-teletexstring+)))
  (let ((bytes (asn1-string-bytes-vector asn1-string)))
    (if (asn1-teletex-string-p bytes)
        (u8-to-string bytes)
        (error 'invalid-asn1-string :type '+v-asn1-teletexstring+))))

(defmethod decode-asn1-string (asn1-string (type (eql +v-asn1-bmpstring+)))
  (if (= 0 (mod (asn1-string-length asn1-string) 2))
      (or (ignore-errors (ffi:foreign-string-to-lisp (asn1-string-data asn1-string) :count (asn1-string-length asn1-string) :encoding :utf-16/be))
          (error 'invalid-asn1-string :type '+v-asn1-bmpstring+))
      (error 'invalid-asn1-string :type '+v-asn1-bmpstring+)))

;; TODO: respect asn1-string type
(defun try-get-asn1-string-data (asn1-string allowed-types)
  (let ((type (asn1-string-type asn1-string)))
    (assert (member (asn1-string-type asn1-string) allowed-types) nil "Invalid asn1 string type")
    (decode-asn1-string asn1-string type)))

;; ASN1 Times are represented with ASN1 Strings
(defun decode-asn1-time (asn1-time)
  (when (zerop (asn1-time-check asn1-time))
    (error "asn1-time is not a syntactically valid ASN1 UTCTime"))
  (let ((time-string (u8-to-string (asn1-string-bytes-vector asn1-time))))
    (let* ((utctime-p (= 1 (asn1-utctime-check asn1-time)))
           (year-len (if utctime-p 2 4))
           (year-part (parse-integer (subseq time-string 0 year-len)))
           (year (if utctime-p
                     (if (>= year-part 50)
                         (+ 1900 year-part)
                         (+ 2000 year-part))
                     year-part)))
      (flet ((get-element-after-year (position)
               (parse-integer
                (subseq time-string
                        (+ position year-len)
                        (+ position year-len 2)))))
        (let ((month  (get-element-after-year 0))
              (day    (get-element-after-year 2))
              (hour   (get-element-after-year 4))
              (minute (get-element-after-year 6))
              (second (get-element-after-year 8)))
          (encode-universal-time second minute hour day month year 0))))))

(defun slurp-stream (stream)
  "Returns a sequence containing the STREAM bytes; the
sequence is created by FFI:MAKE-SHAREABLE-BYTE-VECTOR,
therefore it can safely be passed to
 FFI:WITH-POINTER-TO-VECTOR-DATA."
  (let ((seq (ffi:make-shareable-byte-vector (file-length stream))))
    (read-sequence seq stream)
    seq))

(defgeneric decode-certificate (format bytes)
  (:documentation
   "The BYTES must be created by FFI:MAKE-SHAREABLE-BYTE-VECTOR (because
we are going to pass them to FFI:WITH-POINTER-TO-VECTOR-DATA)"))

(defmethod decode-certificate ((format (eql :der)) bytes)
  (ffi:with-pointer-to-vector-data (buf* bytes)
    (ffi:with-foreign-object (buf** :pointer)
      (setf (ffi:mem-ref buf** :pointer) buf*)
      (let ((cert (d2i-x509 (ffi:null-pointer) buf** (length bytes))))
        (when (ffi:null-pointer-p cert)
          (error 'ssl-error-call :message "d2i-X509 failed" :queue (read-ssl-error-queue)))
        cert))))

(defun cert-format-from-path (path)
  ;; or match "pem" type too and raise unknown format error?
  (if (equal "der" (pathname-type path))
      :der
      :pem))

(defun decode-certificate-from-file (path &key format)
  (let ((bytes (with-open-file (stream path :element-type 'u8)
                 (slurp-stream stream)))
        (format (or format (cert-format-from-path path))))
    (decode-certificate format bytes)))

(defun certificate-alt-names (cert)
  #|
  * The return value is the decoded extension or NULL on
  * error. The actual error can have several different causes,
  * the value of *crit reflects the cause:
  * >= 0, extension found but not decoded (reflects critical value).
  * -1 extension not found.
  * -2 extension occurs more than once.
  |#
  (ffi:with-foreign-object (crit* :int)
    (let ((result (x509-get-ext-d2i cert +NID-subject-alt-name+ crit* (ffi:null-pointer))))
      (if (ffi:null-pointer-p result)
          (let ((crit (ffi:mem-ref crit* :int)))
            (cond
              ((>= crit 0)
               (error "X509_get_ext_d2i: subject-alt-name extension decoding error"))
              ((= crit -1) ;; extension not found, return NULL
               result)
              ((= crit -2)
               (error "X509_get_ext_d2i: subject-alt-name extension occurs more than once"))))
          result))))

(defun certificate-dns-alt-names (cert)
  (let ((altnames (certificate-alt-names cert)))
    (unless (ffi:null-pointer-p altnames)
      (unwind-protect
           (flet ((alt-name-to-string (alt-name)
                    (ffi:with-foreign-slots ((type data) alt-name (:struct general-name))
                      (case type
                        (#. +GEN-IPADD+
                         (let ((address (asn1-string-bytes-vector data)))
                           (epsilon.net:resolve address)))
                        (#. +GEN-DNS+
                         (or (try-get-asn1-string-data data '(#. +v-asn1-iastring+))
                             (error "Malformed certificate: possibly NULL in dns-alt-name")))))))
             (let ((altnames-count (sk-general-name-num altnames)))
               (loop for i from 0 below altnames-count
                     as alt-name = (sk-general-name-value altnames i)
                     collect (alt-name-to-string alt-name))))
        (general-names-free altnames)))))

(defun certificate-subject-common-names (cert)
  (let ((i -1)
        (subject-name (x509-get-subject-name cert)))
    (when (ffi:null-pointer-p subject-name)
      (error "X509_get_subject_name returned NULL"))
    (flet ((extract-cn ()
             (setf i (x509-name-get-index-by-nid subject-name +NID-commonName+ i))
             (when (>= i 0)
               (let* ((entry (x509-name-get-entry subject-name i)))
                 (when (ffi:null-pointer-p entry)
                   (error "X509_NAME_get_entry returned NULL"))
                 (let ((entry-data (x509-name-entry-get-data entry)))
                   (when (ffi:null-pointer-p entry-data)
                     (error "X509_NAME_ENTRY_get_data returned NULL"))
                   (try-get-asn1-string-data entry-data '(#.+v-asn1-utf8string+
                                                          #.+v-asn1-bmpstring+
                                                          #.+v-asn1-printablestring+
                                                          #.+v-asn1-universalstring+
                                                          #.+v-asn1-teletexstring+)))))))
      (loop
        as cn = (extract-cn)
        if cn collect cn
          if (not cn) do
            (loop-finish)))))

(defun certificate-not-after-time (certificate)
  "Returns a universal-time representing the time after
which the CERTIFICATE is not valid. Signals an ERROR if the
CERTIFICATE does not have a properly formatted time. "

  (when (or (openssl-is-not-even 1 1 0)
            (libresslp))
    (error "certificate-not-after-time currently requires version OpenSSL 1.1.0 or newer"))

  (let ((asn1-time (x509-get0-not-after certificate)))
    (when (ffi:null-pointer-p asn1-time)
      (error "X509_get0_notAfter returned NULL"))
    (decode-asn1-time asn1-time)))

(defun certificate-not-before-time (certificate)
  "Returns a universal-time representing the time before
which the CERTIFICATE is not valid. Signals an ERROR if
the CERTIFICATE does not have a properly formatted time."

  (when (or (openssl-is-not-even 1 1 0)
            (libresslp))
    (error "certificate-not-before-time currently requires version OpenSSL 1.1.0 or newer"))

  (let ((asn1-time (x509-get0-not-before certificate)))
    (when (ffi:null-pointer-p asn1-time)
      (error "X509_get0_notBefore returned NULL"))
    (decode-asn1-time asn1-time)))

(defun certificate-fingerprint (certificate &optional (algorithm :sha1))
  "Return the fingerprint of CERTIFICATE as a byte-vector. ALGORITHM is a string
designator for the digest algorithm to use (it defaults to SHA-1)."
  (ensure-initialized)
  (let ((evp (evp-get-digest-by-name (string algorithm))))
    (when (ffi:null-pointer-p evp)
      (error 'ssl-error-call
             :message (format nil "unknown digest algorithm ~A" algorithm)
             :queue (read-ssl-error-queue)))
    (let* ((size (funcall (if (openssl-is-at-least 3 0 0)
                              'evp-md-get-size
                              'evp-md-size)
                          evp))
           (fingerprint (ffi:make-shareable-byte-vector size)))
      (ffi:with-pointer-to-vector-data (buf fingerprint)
        (unless (= 1 (x509-digest certificate evp buf (ffi:null-pointer)))
          (error 'ssl-error-call
                 :message "failed to compute fingerprint of certificate"
                 :queue (read-ssl-error-queue))))
      fingerprint)))

(defun x509-cert-from-pem (pem)
  (with-bio-input-from-string (bio pem)
    (pem-read-x509 bio 0 0 0)))

(defun certificate-pem (x509)
  (with-bio-output-to-string (bio)
    ;; man PEM_write_bio_X509:
    ;; The write routines return 1 for success or 0 for failure.
    (unless (= 1 (pem-write-x509 bio x509))
      (error "X509 cert cant be printed: ~s"
             (epsilon.net.tls::err-error-string (epsilon.net.tls::err-get-error) (ffi:null-pointer))))))

(defun random-bytes (count)
  "Generates COUNT cryptographically strong pseudo-random bytes. Returns
the bytes as a SIMPLE-ARRAY with ELEMENT-TYPE u8. Signals
an ERROR in case of problems; for example, when the OpenSSL random number
generator has not been seeded with enough randomness to ensure an
unpredictable byte sequence."
  (let* ((result (->u8 count))
         (buf (make-buffer count))
         (ret (ffi:with-pointer-to-vector-data (ptr buf)
                (rand-bytes ptr count))))
    (when (/= 1 ret)
      (error "RANDOM-BYTES failed: error reported by the OpenSSL RAND_bytes function. ~A."
             (format-ssl-error-queue nil (read-ssl-error-queue))))
    (s/b-replace result buf)))

;; TODO: Should we define random-specific constants and condition classes for
;; RAND_F_RAND_GET_RAND_METHOD, RAND_F_SSLEAY_RAND_BYTES, RAND_R_PRNG_NOT_SEEDED
;; (defined in the rand.h file of the OpenSSl sources)?
;; Where to place these constants/condtitions, here or in the conditions.lisp?
;; On the other hand, those constants are just numbers defined for C,
;; for now we jsut report human readable strings, without possibility
;; to distinguish these error causes programmatically.


(define-condition verify-location-not-found-error (ssl-error)
  ((location :initarg :location))
  (:documentation "Unable to find verify locations")
  (:report (lambda (condition stream)
             (format stream "Unable to find verify location. Path: ~A" (slot-value condition 'location)))))

(defun validate-verify-location (location)
  (handler-case
      (cond
        ((epsilon.sys.fs:file-p location)
         (values location t))
        ((epsilon.sys.fs:dir-p location)
         (values location nil))
        (t
         (error 'verify-location-not-found-error :location location)))))

(defun add-verify-locations (ssl-ctx locations)
  (dolist (location locations)
    (multiple-value-bind (location isfile)
        (validate-verify-location location)
      (ffi:with-foreign-strings ((location-ptr location))
        (unless (= 1 (epsilon.net.tls::ssl-ctx-load-verify-locations
                      ssl-ctx
                      (if isfile location-ptr (ffi:null-pointer))
                      (if isfile (ffi:null-pointer) location-ptr)))
          (error 'ssl-error :queue (read-ssl-error-queue) :message (format nil "Unable to load verify location ~A" location)))))))

(defun ssl-ctx-set-verify-location (ssl-ctx location)
  (cond
    ((eq :default location)
     (unless (= 1 (ssl-ctx-set-default-verify-paths ssl-ctx))
       (error 'ssl-error-call
              :queue (read-ssl-error-queue)
              :message (format nil "Unable to load default verify paths"))))
     ((eq :default-file location)
      ;; supported since openssl 1.1.0
      (unless (= 1 (ssl-ctx-set-default-verify-file ssl-ctx))
        (error 'ssl-error-call
               :queue (read-ssl-error-queue)
               :message (format nil "Unable to load default verify file"))))
     ((eq :default-dir location)
      ;; supported since openssl 1.1.0
      (unless (= 1 (ssl-ctx-set-default-verify-dir ssl-ctx))
        (error 'ssl-error-call
               :queue (read-ssl-error-queue)
               :message (format nil "Unable to load default verify dir"))))
    ((stringp location)
     (add-verify-locations ssl-ctx (list location)))
    ((pathnamep location)
     (add-verify-locations ssl-ctx (list location)))
    ((and location (listp location))
     (add-verify-locations ssl-ctx location))
    ;; silently allow NIL as location
    (location
     (error "Invalid location ~a" location))))

(defun make-context (&key (method nil method-supplied-p)
                          disabled-protocols
                          (options (list +SSL-OP-ALL+))
                          min-proto-version
                          (session-cache-mode +ssl-sess-cache-server+)
                          (verify-location :default)
                          (verify-depth 100)
                          (verify-mode +ssl-verify-peer+)
                          verify-callback
                          cipher-list
                          (pem-password-callback 'pem-password-callback)
                          certificate-chain-file
                          private-key-file
                          private-key-password
                          (private-key-file-type +ssl-filetype-pem+))
  "Creates a new SSL_CTX using SSL_CTX_new and initializes it according to
the specified parameters.

After you're done using the context, don't forget to free it using SSL-CTX-FREE.

Exceptions:

    SSL-ERROR-INITIALIZE. When underlying SSL_CTX_new fails.

Keyword arguments:

    METHOD. Specifies which supported SSL/TLS to use.
        If not specified then TLS_method is used on OpenSSL
        versions supporing it (on legacy versions SSLv23_method is used).

    DISABLED-PROTOCOLS. List of +SSL-OP-NO-* constants. Denotes
        disabled SSL/TLS versions. When METHOD not specified
        defaults to (LIST +SSL-OP-NO-SSLV2+ +SSL-OP-NO-SSLV3+)

    OPTIONS. SSL context options list. Defaults to (list +SSL-OP-ALL+)

    SESSION-CACHE-MODE. Enable/Disable session caching.
        Defaults to +SSL-SESS-CACHE-SERVER+

    VERIFY-LOCATION. Location(s) to load CA from.

        Possible values:
            :DEFAULT - SSL_CTX_set_default_verify_paths will be called.
            :DEFAULT-FILE - SSL_CTX_set_default_verify_file will be called. Requires OpenSSL >= 1.1.0.
            :DEFAULT-DIR - SSL_CTX_set_default_verify_dir will be called. Requires OpenSSL >= 1.1.0.
            A STRING or a PATHNAME - will be passed to SSL_CTX_load_verify_locations
                as file or dir argument depending on wether it's really
                a file or a dir. Must exist on the file system and be available.
            A LIST - each value assumed to be either a STRING or a PATHNAME and
                will be passed to SSL_CTX_load_verify_locations as described above.

    VERIFY-DEPTH. Sets the maximum depth for the certificate chain verification
        that shall be allowed for context. Defaults to 100.

    VERIFY-MODE. The mode parameter to SSL_CTX_set_verify.
        Defaults to +VERIFY-PEER+

    VERIFY-CALLBACK. The verify_callback parameter to SSL_CTX_set_verify.
        Please note: if specified, must be a FFI callback i.e. defined as
        (DEFCALLBACK :INT ((OK :INT) (SSL-CTX :POINTER)) .. ).

    CIPHER-LIST. If specified, must be a string to pass to SSL_CTX_set_cipher_list.
        An ERROR is signalled if SSL_CTX_set_cipher_list fails.

    PEM-PASSWORD-CALLBACK. Sets the default password callback called when
        loading/storing a PEM certificate with encryption.
        Please note: this must be FFI callback i.e. defined as
        (FFI:DEFCALLBACK :INT ((BUF :POINTER) (SIZE :INT) (RWFLAG :INT) (UNUSED :POINTER)) .. ).
        Defaults to PEM-PASSWORD-CALLBACK which simply uses password
        provided by WITH-PEM-PASSWORD.
"
  (ensure-initialized)
  (let ((ssl-ctx (ssl-ctx-new (if method-supplied-p
                                  method
                                  (progn
                                    (unless disabled-protocols
                                      (setf disabled-protocols
                                            (list +SSL-OP-NO-SSLv2+
                                                  +SSL-OP-NO-SSLv3+)))
                                    (funcall (default-ssl-method)))))))
    (when (ffi:null-pointer-p ssl-ctx)
      (error 'ssl-error-initialize :reason "Can't create new SSL-CTX"
                                   :queue (read-ssl-error-queue)))
    (handler-bind ((error (lambda (_)
                            (declare (ignore _))
                            (ssl-ctx-free ssl-ctx))))
      (ssl-ctx-set-options ssl-ctx
                           (apply #'logior
                                  (append disabled-protocols options)))
      ;; Older OpenSSL versions might not have this SSL_ctrl call.
      ;; Having them error out is a sane default - it's better than to keep
      ;; on running with insecure values.
      ;; People that _have_ to use much too old OpenSSL versions will
      ;; have to call MAKE-CONTEXT with :MIN-PROTO-VERSION nil.
      ;;
      ;; As an aside: OpenSSL had the "SSL_OP_NO_TLSv1_2" constant since
      ;;   7409d7ad517    2011-04-29 22:56:51 +0000
      ;; so requiring a "new"er OpenSSL to match epsilon.net.tls's defauls shouldn't be a problem.
      (if min-proto-version
        (if (zerop (ssl-ctx-set-min-proto-version ssl-ctx min-proto-version))
          (error "Couldn't set minimum SSL protocol version!")))
      (ssl-ctx-set-session-cache-mode ssl-ctx session-cache-mode)
      (ssl-ctx-set-verify-location ssl-ctx verify-location)
      (ssl-ctx-set-verify-depth ssl-ctx verify-depth)
      (ssl-ctx-set-verify ssl-ctx verify-mode (if verify-callback
                                                  (ffi:get-callback verify-callback)
                                                  (ffi:null-pointer)))

      (when (and cipher-list
                 (zerop (ssl-ctx-set-cipher-list ssl-ctx cipher-list)))
        (error 'ssl-error-initialize
               :reason
               "Can't set SSL cipher list: SSL_CTX_set_cipher_list returned 0"
               :queue (read-ssl-error-queue)))
      (ssl-ctx-set-default-passwd-cb ssl-ctx (ffi:get-callback pem-password-callback))
      (when certificate-chain-file
        (ssl-ctx-use-certificate-chain-file ssl-ctx certificate-chain-file))
      (when private-key-file
        (with-pem-password (private-key-password)
          (ssl-ctx-use-privatekey-file ssl-ctx private-key-file private-key-file-type)))
      ssl-ctx)))

(defun call-with-global-context (ssl-ctx auto-free-p body-fn)
  (let* ((*ssl-global-context* ssl-ctx))
    (unwind-protect (funcall body-fn)
      (when auto-free-p
        (ssl-ctx-free ssl-ctx)))))

(defmacro with-global-context ((ssl-ctx &key auto-free-p) &body body)
  "Executes the BODY with *SSL-GLOBAL-CONTEXT* bound to the SSL-CTX.
If AUTO-FREE-P is true the context is freed using SSL-CTX-FREE before exit. "
  `(call-with-global-context ,ssl-ctx ,auto-free-p (lambda () ,@body)))
