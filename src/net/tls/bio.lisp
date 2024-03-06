(in-package net.tls)

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
               (net.tls::err-error-string (net.tls::err-get-error) (ffi:null-pointer)))
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
;;; In net.tls this means the following nested calls:
;;;
;;;   1) Lisp: net.tls stream user code ->
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

(defparameter *file-name* (ffi:foreign-string-alloc "net.tls/src/bio.lisp"))

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
        (write-line buf (make-char-stream *bio-socket*
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
