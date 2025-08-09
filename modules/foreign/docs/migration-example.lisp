;;;; Example: Migrating epsilon.linux to use epsilon.foreign
;;;; 
;;;; This shows how platform-specific FFI code can be simplified
;;;; using the epsilon.foreign module.

;;; Before: Using sb-alien directly in epsilon.linux
#+nil
(progn
  ;; Define alien functions for epoll
  (sb-alien:define-alien-routine ("epoll_create" %epoll-create) sb-alien:int
    (size sb-alien:int))

  (sb-alien:define-alien-routine ("epoll_create1" %epoll-create1) sb-alien:int
    (flags sb-alien:int))

  (sb-alien:define-alien-routine ("epoll_ctl" %epoll-ctl) sb-alien:int
    (epfd sb-alien:int)
    (op sb-alien:int)
    (fd sb-alien:int)
    (event sb-alien:system-area-pointer))

  (sb-alien:define-alien-routine ("epoll_wait" %epoll-wait) sb-alien:int
    (epfd sb-alien:int)
    (events sb-alien:system-area-pointer)
    (maxevents sb-alien:int)
    (timeout sb-alien:int))

  (sb-alien:define-alien-routine ("close" %close) sb-alien:int
    (fd sb-alien:int)))

;;; After: Using epsilon.foreign
(progn
  ;; Load the foreign module
  (require :epsilon.foreign)
  
  ;; Define the same functions with defshared
  (lib:defshared %epoll-create "epoll_create" "libc" :int
    (size :int))

  (lib:defshared %epoll-create1 "epoll_create1" "libc" :int
    (flags :int))

  (lib:defshared %epoll-ctl "epoll_ctl" "libc" :int
    (epfd :int) (op :int) (fd :int) (event :pointer))

  (lib:defshared %epoll-wait "epoll_wait" "libc" :int
    (epfd :int) (events :pointer) (maxevents :int) (timeout :int))

  (lib:defshared %close "close" "libc" :int
    (fd :int)))

;;; Benefits of using epsilon.foreign:
;;;
;;; 1. Cleaner syntax - defshared is more readable than define-alien-routine
;;; 2. Automatic library management - no need to manually load .so files
;;; 3. Platform independence - works on Linux, macOS, Windows
;;; 4. Function caching - better performance for repeated calls
;;; 5. Consistent error handling - unified approach across all FFI calls

;;; Example: Complete epoll wrapper using epsilon.foreign
(defpackage epsilon.linux.epoll-v2
  (:use :cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   #:with-epoll
   #:add-fd
   #:wait-for-events))

(in-package epsilon.linux.epoll-v2)

;; Define all epoll functions
(lib:defshared epoll-create1 "epoll_create1" "libc" :int
  (flags :int))

(lib:defshared epoll-ctl "epoll_ctl" "libc" :int
  (epfd :int) (op :int) (fd :int) (event :pointer))

(lib:defshared epoll-wait "epoll_wait" "libc" :int
  (epfd :int) (events :pointer) (maxevents :int) (timeout :int))

(lib:defshared close "close" "libc" :int
  (fd :int))

;; Constants
(defconstant +epoll-ctl-add+ 1)
(defconstant +epollin+ 1)
(defconstant +epoll-cloexec+ #o2000000)

;; High-level API
(defmacro with-epoll ((epfd &key (flags 0)) &body body)
  `(let ((,epfd (epoll-create1 ,flags)))
     (unwind-protect
          (progn ,@body)
       (close ,epfd))))

(defun add-fd (epfd fd events)
  "Add a file descriptor to epoll"
  (lib:with-foreign-memory ((event 16)) ; epoll_event is 16 bytes
    ;; Set up the event structure
    (setf (sb-sys:sap-ref-32 event 0) events)    ; events field
    (setf (sb-sys:sap-ref-64 event 8) fd)        ; data.fd field
    (zerop (epoll-ctl epfd +epoll-ctl-add+ fd event))))

(defun wait-for-events (epfd &key (max-events 10) (timeout -1))
  "Wait for events on epoll file descriptor"
  (lib:with-foreign-memory ((events (* 16 max-events)))
    (let ((n (epoll-wait epfd events max-events timeout)))
      (when (plusp n)
        (loop for i from 0 below n
              for offset = (* i 16)
              collect (list :events (sb-sys:sap-ref-32 events offset)
                           :fd (sb-sys:sap-ref-64 events (+ offset 8))))))))

;;; Usage example:
#+nil
(with-epoll (epfd :flags +epoll-cloexec+)
  (add-fd epfd 0 +epollin+)  ; Monitor stdin
  (let ((events (wait-for-events epfd :timeout 1000)))
    (dolist (event events)
      (format t "Event on fd ~D: ~X~%" 
              (getf event :fd) 
              (getf event :events)))))