;;;; Fiber-aware I/O conventions
;;;;
;;;; Per the design rule from manual/architecture/sb-fiber-runtime.md,
;;;; standard streams stay standard -- callers OPT IN to fiber-aware
;;;; behaviour by calling these explicit wrappers.  Plain CL:READ /
;;;; CL:WRITE-SEQUENCE remain blocking syscalls.
;;;;
;;;; Surface:
;;;;   FIBER-WAIT-READABLE fd &key timeout cancellation
;;;;   FIBER-WAIT-WRITABLE fd &key timeout cancellation
;;;;   FIBER-SLEEP-MS ms &key cancellation
;;;;   FIBER-READ fd buf &key start end timeout cancellation
;;;;   FIBER-WRITE fd buf &key start end timeout cancellation
;;;;   FIBER-READ-EXACT fd buf &key start end timeout cancellation
;;;;   FIBER-WRITE-ALL fd buf &key start end timeout cancellation
;;;;   FIBER-ACCEPT listener-fd &key timeout cancellation
;;;;   FIBER-CONNECT fd sockaddr-sap addrlen &key timeout cancellation
;;;;
;;;; Each blocking syscall is wrapped in the canonical pattern:
;;;;   loop:
;;;;     fiber-wait-* (deadline-bounded)
;;;;     try the nonblocking syscall
;;;;     case n of:
;;;;       0  : EOF or success-with-no-data; return per the operation
;;;;       :would-block : retry
;;;;       n  : return n
;;;;
;;;; Callers MUST set the fd to O_NONBLOCK before calling these (the
;;;; FIBER-* layer does not change fd flags).  Without O_NONBLOCK the
;;;; first syscall blocks the carrier thread, defeating the point.

(defpackage :epsilon.scheduler.fiber-io
  (:use :cl)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.io-wait io-wait)
           (epsilon.foreign lib))
  (:export
   #:fiber-wait-readable
   #:fiber-wait-writable
   #:fiber-sleep-ms
   #:fiber-read
   #:fiber-write
   #:fiber-read-exact
   #:fiber-write-all
   #:fiber-accept
   #:fiber-connect
   #:fiber-set-nonblocking
   #:fiber-io-error
   #:fiber-io-error-errno
   #:fiber-io-error-syscall
   #:fiber-io-timeout))

(in-package :epsilon.scheduler.fiber-io)

;;; ---------------------------------------------------------------------------
;;; Errors
;;; ---------------------------------------------------------------------------

(define-condition fiber-io-error (error)
  ((errno   :initarg :errno   :reader fiber-io-error-errno)
   (syscall :initarg :syscall :reader fiber-io-error-syscall))
  (:report (lambda (c s)
             (format s "Fiber I/O ~A failed: errno=~D"
                     (fiber-io-error-syscall c)
                     (fiber-io-error-errno c)))))

(define-condition fiber-io-timeout (fiber-io-error) ()
  (:report (lambda (c s)
             (format s "Fiber I/O ~A timed out"
                     (fiber-io-error-syscall c)))))

;;; ---------------------------------------------------------------------------
;;; FFI
;;; ---------------------------------------------------------------------------

(lib:defshared %accept "accept" "libc" :int
  (fd :int) (addr :pointer) (addrlen :pointer))

(lib:defshared %connect "connect" "libc" :int
  (fd :int) (addr :pointer) (addrlen :unsigned-long))

(lib:defshared %fcntl "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int))

(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defparameter *o-nonblock*
  #+darwin #x0004
  #+linux  #o4000
  #-(or darwin linux) #o4000)

;;; sb-unix:eagain == 11 on Linux, 35 on Darwin (handled by the symbol).
;;; EINTR is 4 on both.  EINPROGRESS differs: 36 Darwin, 115 Linux.
(defconstant +eintr+ 4)
(defparameter *einprogress*
  #+darwin 36
  #+linux  115
  #-(or darwin linux) 115)

(defun fiber-set-nonblocking (fd)
  "Set FD to O_NONBLOCK.  Returns T on success, signals FIBER-IO-ERROR
on failure."
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error 'fiber-io-error :errno (sb-alien:get-errno) :syscall :fcntl-getfl))
    (when (zerop (logand flags *o-nonblock*))
      (let ((result (%fcntl fd +f-setfl+ (logior flags *o-nonblock*))))
        (when (< result 0)
          (error 'fiber-io-error :errno (sb-alien:get-errno)
                                 :syscall :fcntl-setfl))))
    t))

;;; ---------------------------------------------------------------------------
;;; Wait primitives
;;; ---------------------------------------------------------------------------

(defun fiber-wait-readable (fd &key timeout cancellation)
  "Park the current fiber until FD is readable.  Returns T on
readiness, NIL on timeout.  Signals FIBER-CANCELLED if CANCELLATION
fires before either.  TIMEOUT is in seconds."
  (io-wait:park-on-fd fd '(:in)
                      :timeout timeout
                      :cancellation cancellation))

(defun fiber-wait-writable (fd &key timeout cancellation)
  "Park the current fiber until FD is writable."
  (io-wait:park-on-fd fd '(:out)
                      :timeout timeout
                      :cancellation cancellation))

(defun fiber-sleep-ms (ms &key cancellation)
  "Yield the fiber for MS milliseconds."
  (sched:coroutine-sleep (/ ms 1000.0) :cancellation cancellation))

;;; ---------------------------------------------------------------------------
;;; Read / write
;;;
;;; Each one is the canonical EAGAIN loop from the IMPL-351 doc.  Uses
;;; SB-UNIX:UNIX-READ / UNIX-WRITE because they return (values N errno)
;;; without the surrounding allocation overhead of the FFI layer; this
;;; is the same pattern epsilon.crypto.native uses.
;;; ---------------------------------------------------------------------------

(defun fiber-read (fd buf &key (start 0) end timeout cancellation)
  "Read into BUF[START..END) (default END = (LENGTH BUF)).  Returns
the number of bytes read; 0 on EOF.  Parks the fiber on EAGAIN.
Times out / cancels per :TIMEOUT and :CANCELLATION."
  (let* ((end (or end (length buf)))
         (want (- end start)))
    (when (zerop want) (return-from fiber-read 0))
    (lib:with-foreign-memory ((sap :char :count want))
      (loop
        (multiple-value-bind (n errno) (sb-unix:unix-read fd sap want)
          (cond
            ((and n (plusp n))
             (loop for i from 0 below n
                   do (setf (aref buf (+ start i))
                            (sb-sys:sap-ref-8 sap i)))
             (return n))
            ((and n (zerop n))
             (return 0))
            ((or (= errno sb-unix:eagain)
                 (= errno +eintr+))
             (let ((ready (fiber-wait-readable fd :timeout timeout
                                                  :cancellation cancellation)))
               (unless ready
                 (error 'fiber-io-timeout :errno 0 :syscall :read))))
            (t
             (error 'fiber-io-error :errno errno :syscall :read))))))))

(defun fiber-write (fd buf &key (start 0) end timeout cancellation)
  "Write BUF[START..END) to FD.  Returns the number of bytes written
on a single successful syscall (may be < end-start if the kernel
didn't drain the whole buffer).  Use FIBER-WRITE-ALL when you need
to ensure every byte is written."
  (let* ((end (or end (length buf)))
         (want (- end start)))
    (when (zerop want) (return-from fiber-write 0))
    (lib:with-foreign-memory ((sap :char :count want))
      (loop for i from 0 below want
            do (setf (sb-sys:sap-ref-8 sap i) (aref buf (+ start i))))
      (loop
        (multiple-value-bind (n errno) (sb-unix:unix-write fd sap 0 want)
          (cond
            ((and n (>= n 0))
             (return n))
            ((or (= errno sb-unix:eagain)
                 (= errno +eintr+))
             (let ((ready (fiber-wait-writable fd :timeout timeout
                                                  :cancellation cancellation)))
               (unless ready
                 (error 'fiber-io-timeout :errno 0 :syscall :write))))
            (t
             (error 'fiber-io-error :errno errno :syscall :write))))))))

(defun fiber-read-exact (fd buf &key (start 0) end timeout cancellation)
  "Read exactly END-START bytes into BUF, looping until done.  Signals
FIBER-IO-ERROR on premature EOF.  Returns BUF."
  (let* ((end (or end (length buf)))
         (pos start))
    (loop while (< pos end)
          for got = (fiber-read fd buf :start pos :end end
                                       :timeout timeout
                                       :cancellation cancellation)
          do (when (zerop got)
               (error 'fiber-io-error :errno 0 :syscall :read-exact-eof))
             (incf pos got))
    buf))

(defun fiber-write-all (fd buf &key (start 0) end timeout cancellation)
  "Write every byte of BUF[START..END) to FD, looping over partial writes.
Returns the total bytes written."
  (let* ((end (or end (length buf)))
         (pos start))
    (loop while (< pos end)
          for sent = (fiber-write fd buf :start pos :end end
                                         :timeout timeout
                                         :cancellation cancellation)
          do (when (zerop sent)
               (error 'fiber-io-error :errno 0 :syscall :write-all-zero))
             (incf pos sent))
    (- pos start)))

;;; ---------------------------------------------------------------------------
;;; accept / connect
;;; ---------------------------------------------------------------------------

(defun fiber-accept (listener-fd &key timeout cancellation)
  "Accept a single connection from LISTENER-FD.  Returns the new
client fd; the peer address is discarded (callers that need it should
use a platform-specific wrapper).  LISTENER-FD must be O_NONBLOCK."
  (loop
    (let ((new-fd (%accept listener-fd (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
      (cond
        ((>= new-fd 0)
         (return new-fd))
        (t
         (let ((errno (sb-alien:get-errno)))
           (cond
             ((or (= errno sb-unix:eagain)
                  (= errno +eintr+))
              (let ((ready (fiber-wait-readable listener-fd
                                                 :timeout timeout
                                                 :cancellation cancellation)))
                (unless ready
                  (error 'fiber-io-timeout :errno 0 :syscall :accept))))
             (t (error 'fiber-io-error :errno errno :syscall :accept)))))))))

(defun fiber-connect (fd sockaddr-sap addrlen &key timeout cancellation)
  "Connect FD to the address represented by SOCKADDR-SAP / ADDRLEN.
Returns T on success.  FD must be O_NONBLOCK so we get EINPROGRESS
and can yield the fiber while the kernel completes the handshake.

Callers are responsible for the sockaddr layout -- this primitive is
a thin wrapper around POSIX connect(2)."
  (let ((rc (%connect fd sockaddr-sap addrlen)))
    (cond
      ((zerop rc) (return-from fiber-connect t))
      (t
       (let ((errno (sb-alien:get-errno)))
         (cond
           ((= errno *einprogress*)
            (let ((ready (fiber-wait-writable fd :timeout timeout
                                                 :cancellation cancellation)))
              (unless ready
                (error 'fiber-io-timeout :errno 0 :syscall :connect))
              ;; The OS reports the actual outcome via SO_ERROR; we
              ;; don't probe it here -- callers that need it can do
              ;; getsockopt directly.  This convention matches the
              ;; existing async-tcp-transport pattern.
              t))
           (t (error 'fiber-io-error :errno errno :syscall :connect))))))))
