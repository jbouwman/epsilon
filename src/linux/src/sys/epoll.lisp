(defpackage :epsilon.sys.epoll
  (:use :cl)
  (:export
   ;; Core epoll operations  
   #:epoll-create
   #:epoll-create1
   #:epoll-ctl
   #:epoll-wait
   #:epoll-close
   
   ;; Event creation and management
   #:make-epoll-event
   #:epoll-event-events
   #:epoll-event-data
   
   ;; Event types
   #:+epollin+
   #:+epollout+
   #:+epollrdhup+
   #:+epollpri+
   #:+epollerr+
   #:+epollhup+
   #:+epollet+
   #:+epolloneshot+
   #:+epollwakeup+
   #:+epollexclusive+
   
   ;; Control operations
   #:+epoll-ctl-add+
   #:+epoll-ctl-mod+
   #:+epoll-ctl-del+
   
   ;; epoll_create1 flags
   #:+epoll-cloexec+
   
   ;; High-level interface
   #:with-epoll
   #:add-event
   #:modify-event
   #:delete-event
   #:wait-for-events
   
   ;; Utility functions
   #:make-epoll-data
   #:epoll-data-fd
   #:epoll-data-ptr
   #:epoll-data-u32
   #:epoll-data-u64
   
   ;; Event checking predicates
   #:epoll-event-readable-p
   #:epoll-event-writable-p
   #:epoll-event-error-p
   #:epoll-event-hangup-p))

(in-package :epsilon.sys.epoll)

;;;; epoll Constants

;; Event types (what to monitor)
(defconstant +epollin+      #x001)   ; Available for read
(defconstant +epollout+     #x004)   ; Available for write
(defconstant +epollrdhup+   #x2000)  ; Stream socket peer closed connection
(defconstant +epollpri+     #x002)   ; Exceptional condition
(defconstant +epollerr+     #x008)   ; Error condition (always monitored)
(defconstant +epollhup+     #x010)   ; Hang up (always monitored)
(defconstant +epollet+      #x80000000) ; Edge-triggered mode
(defconstant +epolloneshot+ #x40000000) ; One-shot mode
(defconstant +epollwakeup+  #x20000000) ; EPOLLWAKEUP
(defconstant +epollexclusive+ #x10000000) ; EPOLLEXCLUSIVE

;; Control operations for epoll_ctl
(defconstant +epoll-ctl-add+ 1)     ; Add file descriptor
(defconstant +epoll-ctl-del+ 2)     ; Remove file descriptor  
(defconstant +epoll-ctl-mod+ 3)     ; Modify file descriptor

;; epoll_create1 flags
(defconstant +epoll-cloexec+ #o2000000) ; Set FD_CLOEXEC

;;;; FFI Bindings using direct SBCL alien functions

;; Define alien functions for epoll system calls
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
  (fd sb-alien:int))

;;;; epoll_event Structure

;; struct epoll_event {
;;     uint32_t events;      /* Epoll events */
;;     epoll_data_t data;    /* User data variable */
;; };
;;
;; typedef union epoll_data {
;;     void    *ptr;
;;     int      fd;
;;     uint32_t u32;
;;     uint64_t u64;
;; } epoll_data_t;

(defstruct epoll-event
  "Represents an epoll event structure"
  (events 0 :type (unsigned-byte 32))     ; uint32_t events
  (data 0 :type (unsigned-byte 64)))      ; epoll_data_t (as 64-bit int)

(defun epoll-event-size ()
  "Size of epoll_event structure in bytes"
  ;; events (4 bytes) + data (8 bytes) = 12 bytes, but padded to 16 on x64
  16)

(defun pack-epoll-event (event buffer offset)
  "Pack epoll_event structure into SAP buffer"
  (let ((ptr (sb-sys:sap+ buffer offset)))
    ;; events (4 bytes)
    (setf (sb-sys:sap-ref-32 ptr 0) (epoll-event-events event))
    ;; data (8 bytes at offset 8 due to padding)
    (setf (sb-sys:sap-ref-64 ptr 8) (epoll-event-data event))))

(defun unpack-epoll-event (buffer offset)
  "Unpack epoll_event structure from SAP buffer"
  (let ((ptr (sb-sys:sap+ buffer offset)))
    (make-epoll-event
     :events (sb-sys:sap-ref-32 ptr 0)
     :data (sb-sys:sap-ref-64 ptr 8))))

;;;; epoll_data_t Helper Functions

(defun make-epoll-data (&key fd ptr u32 u64)
  "Create epoll_data_t union value"
  (cond
    (fd (logand fd #xffffffff))            ; Store fd as lower 32 bits
    (ptr ptr)                              ; Store pointer value
    (u32 (logand u32 #xffffffff))         ; Store 32-bit value
    (u64 (logand u64 #xffffffffffffffff)) ; Store 64-bit value
    (t 0)))

(defun epoll-data-fd (data)
  "Extract file descriptor from epoll_data_t"
  (logand data #xffffffff))

(defun epoll-data-ptr (data)
  "Extract pointer from epoll_data_t"
  data)

(defun epoll-data-u32 (data)
  "Extract 32-bit value from epoll_data_t"
  (logand data #xffffffff))

(defun epoll-data-u64 (data)
  "Extract 64-bit value from epoll_data_t"
  data)


;;;; High-Level epoll Interface

(defun epoll-create (&optional (size 1))
  "Create epoll file descriptor (legacy interface)"
  (let ((epfd (%epoll-create size)))
    (when (= epfd -1)
      (error "Failed to create epoll file descriptor"))
    epfd))

(defun epoll-create1 (&optional (flags 0))
  "Create epoll file descriptor with flags"
  (let ((epfd (%epoll-create1 flags)))
    (when (= epfd -1)
      (error "Failed to create epoll file descriptor"))
    epfd))

(defun epoll-close (epfd)
  "Close epoll file descriptor"
  (%close epfd))

(defun epoll-ctl (epfd operation fd event)
  "Control epoll file descriptor"
  (if event
      (sb-alien:with-alien ((event-buf (sb-alien:array sb-alien:char 16)))
        (let ((sap (sb-alien:alien-sap event-buf)))
          (pack-epoll-event event sap 0)
          (let ((result (%epoll-ctl epfd operation fd sap)))
            (when (= result -1)
              (error "epoll_ctl failed for operation ~A on fd ~A" operation fd))
            result)))
      (let ((result (%epoll-ctl epfd operation fd (sb-sys:int-sap 0))))
        (when (= result -1)
          (error "epoll_ctl failed for operation ~A on fd ~A" operation fd))
        result)))

(defun epoll-wait (epfd max-events timeout)
  "Wait for events on epoll file descriptor"
  (let ((buf-size (* max-events (epoll-event-size))))
    (sb-alien:with-alien ((events-buf (sb-alien:array sb-alien:char 1024))) ; Max 64 events (64*16=1024)
      (when (> buf-size 1024)
        (error "Too many events requested (max 64)"))
      (let ((sap (sb-alien:alien-sap events-buf)))
        (let ((result (%epoll-wait epfd sap max-events timeout)))
          (when (= result -1)
            (error "epoll_wait failed"))
          ;; Unpack returned events
          (loop for i from 0 below result
                collect (unpack-epoll-event sap (* i (epoll-event-size)))))))))

(defmacro with-epoll ((epfd-var &optional (flags 0)) &body body)
  "Execute body with epoll fd, automatically closing on exit"
  `(let ((,epfd-var (epoll-create1 ,flags)))
     (unwind-protect
          (progn ,@body)
       (epoll-close ,epfd-var))))

;;;; Convenience Functions

(defun add-event (epfd fd events &key data)
  "Add file descriptor to epoll"
  (let ((event (make-epoll-event
                :events events
                :data (or data (make-epoll-data :fd fd)))))
    (epoll-ctl epfd +epoll-ctl-add+ fd event)))

(defun modify-event (epfd fd events &key data)
  "Modify file descriptor in epoll"
  (let ((event (make-epoll-event
                :events events
                :data (or data (make-epoll-data :fd fd)))))
    (epoll-ctl epfd +epoll-ctl-mod+ fd event)))

(defun delete-event (epfd fd)
  "Delete file descriptor from epoll"
  (epoll-ctl epfd +epoll-ctl-del+ fd nil))

(defun wait-for-events (epfd max-events &optional (timeout -1))
  "Wait for events (timeout in milliseconds, -1 = block indefinitely)"
  (epoll-wait epfd max-events timeout))

;;;; Event Checking Predicates

(defun epoll-event-readable-p (event)
  "Check if event indicates readable data"
  (not (zerop (logand (epoll-event-events event) +epollin+))))

(defun epoll-event-writable-p (event)
  "Check if event indicates writable"
  (not (zerop (logand (epoll-event-events event) +epollout+))))

(defun epoll-event-error-p (event)
  "Check if event indicates error"
  (not (zerop (logand (epoll-event-events event) 
                      (logior +epollerr+ +epollhup+)))))

(defun epoll-event-hangup-p (event)
  "Check if event indicates peer hangup"
  (not (zerop (logand (epoll-event-events event) +epollrdhup+))))