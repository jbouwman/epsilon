(defpackage :epsilon.sys.kqueue
  (:use :cl)
  (:local-nicknames
   (#:lib #:epsilon.sys.lib))
  (:export
   ;; Core kqueue operations
   #:kqueue
   #:kevent
   #:close-kqueue
   
   ;; Event creation and management
   #:make-kevent
   #:kevent-ident
   #:kevent-filter
   #:kevent-flags
   #:kevent-fflags
   #:kevent-data
   #:kevent-udata
   
   ;; Event filters
   #:+evfilt-read+
   #:+evfilt-write+
   #:+evfilt-aio+
   #:+evfilt-vnode+
   #:+evfilt-proc+
   #:+evfilt-signal+
   #:+evfilt-timer+
   #:+evfilt-machport+
   #:+evfilt-fs+
   #:+evfilt-user+
   
   ;; Event flags
   #:+ev-add+
   #:+ev-delete+
   #:+ev-enable+
   #:+ev-disable+
   #:+ev-oneshot+
   #:+ev-clear+
   #:+ev-receipt+
   #:+ev-dispatch+
   #:+ev-udata-specific+
   #:+ev-dispatch2+
   #:+ev-vanished+
   #:+ev-sysflags+
   #:+ev-flag0+
   #:+ev-flag1+
   #:+ev-eof+
   #:+ev-error+
   
   ;; High-level interface
   #:with-kqueue
   #:add-event
   #:delete-event
   #:wait-for-events
   
   ;; Socket integration
   #:watch-socket-read
   #:watch-socket-write
   #:watch-timer
   #:watch-signal))

(in-package :epsilon.sys.kqueue)

;;;; kqueue Constants

;; Event filters
(defconstant +evfilt-read+      -1)
(defconstant +evfilt-write+     -2)
(defconstant +evfilt-aio+       -3)
(defconstant +evfilt-vnode+     -4)
(defconstant +evfilt-proc+      -5)
(defconstant +evfilt-signal+    -6)
(defconstant +evfilt-timer+     -7)
(defconstant +evfilt-machport+  -8)
(defconstant +evfilt-fs+        -9)
(defconstant +evfilt-user+      -10)

;; Event flags
(defconstant +ev-add+           #x0001)
(defconstant +ev-delete+        #x0002)
(defconstant +ev-enable+        #x0004)
(defconstant +ev-disable+       #x0008)
(defconstant +ev-oneshot+       #x0010)
(defconstant +ev-clear+         #x0020)
(defconstant +ev-receipt+       #x0040)
(defconstant +ev-dispatch+      #x0080)
(defconstant +ev-udata-specific+ #x0100)
(defconstant +ev-dispatch2+     #x0180)
(defconstant +ev-vanished+      #x0200)
(defconstant +ev-sysflags+      #xF000)
(defconstant +ev-flag0+         #x1000)
(defconstant +ev-flag1+         #x2000)

;; Return flags
(defconstant +ev-eof+           #x8000)
(defconstant +ev-error+         #x4000)

;;;; FFI Bindings

;; Core kqueue functions
(lib:defshared %kqueue "kqueue" "libc" :int ()
  :documentation "Create new kernel event queue")

(lib:defshared %kevent "kevent" "libc" :int 
  (kq :int)
  (changelist :pointer)
  (nchanges :int)
  (eventlist :pointer)
  (nevents :int)
  (timeout :pointer)
  :documentation "Control and wait for events on kqueue")

(lib:defshared %close "close" "libc" :int (fd :int)
  :documentation "Close file descriptor")

;;;; kevent Structure

;; struct kevent {
;;     uintptr_t ident;    /* identifier for this event */
;;     int16_t   filter;   /* filter for event */
;;     uint16_t  flags;    /* general flags */
;;     uint32_t  fflags;   /* filter-specific flags */
;;     intptr_t  data;     /* filter-specific data */
;;     void     *udata;    /* opaque user data identifier */
;; };

(defstruct kevent
  "Represents a kernel event structure"
  (ident 0 :type (unsigned-byte 64))      ; uintptr_t
  (filter 0 :type (signed-byte 16))       ; int16_t  
  (flags 0 :type (unsigned-byte 16))      ; uint16_t
  (fflags 0 :type (unsigned-byte 32))     ; uint32_t
  (data 0 :type (signed-byte 64))         ; intptr_t
  (udata 0 :type (unsigned-byte 64)))     ; void* (as integer)

(defun kevent-size ()
  "Size of kevent structure in bytes"
  ;; On 64-bit systems: 8+2+2+4+8+8 = 32 bytes
  32)

(defun pack-kevent (kevent buffer offset)
  "Pack kevent structure into foreign buffer"
  (let ((ptr (sb-alien:sap+ (sb-alien:alien-sap buffer) offset)))
    ;; ident (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 0) (kevent-ident kevent))
    ;; filter (2 bytes)  
    (setf (sb-alien:sap-ref-16 ptr 8) (kevent-filter kevent))
    ;; flags (2 bytes)
    (setf (sb-alien:sap-ref-16 ptr 10) (kevent-flags kevent))
    ;; fflags (4 bytes)
    (setf (sb-alien:sap-ref-32 ptr 12) (kevent-fflags kevent))
    ;; data (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 16) (kevent-data kevent))
    ;; udata (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 24) (kevent-udata kevent))))

(defun unpack-kevent (buffer offset)
  "Unpack kevent structure from foreign buffer"
  (let ((ptr (sb-alien:sap+ (sb-alien:alien-sap buffer) offset)))
    (make-kevent
     :ident (sb-alien:sap-ref-64 ptr 0)
     :filter (sb-alien:sap-ref-16 ptr 8)
     :flags (sb-alien:sap-ref-16 ptr 10)
     :fflags (sb-alien:sap-ref-32 ptr 12)
     :data (sb-alien:sap-ref-64 ptr 16)
     :udata (sb-alien:sap-ref-64 ptr 24))))

;;;; High-Level kqueue Interface

(defun kqueue ()
  "Create a new kqueue file descriptor"
  (let ((kq (%kqueue)))
    (when (= kq -1)
      (error "Failed to create kqueue"))
    kq))

(defun close-kqueue (kq)
  "Close kqueue file descriptor"
  (%close kq))

(defun kevent (kq changelist nevents timeout-sec)
  "Control and wait for events on kqueue"
  (let ((change-count (length changelist))
        (timeout-ptr nil))
    
    ;; Prepare timeout if specified
    (when timeout-sec
      (setf timeout-ptr 
            (lib:foreign-alloc :long :count 2 :finalize t))
      (setf (sb-alien:deref timeout-ptr 0) (floor timeout-sec))      ; tv_sec
      (setf (sb-alien:deref timeout-ptr 1) 
            (floor (* (mod timeout-sec 1) 1000000))))                ; tv_usec
    
    (lib:with-foreign-memory ((change-buf :char :count (* change-count (kevent-size)))
                              (event-buf :char :count (* nevents (kevent-size))))
      
      ;; Pack changelist into buffer
      (loop for i from 0
            for event in changelist
            do (pack-kevent event change-buf (* i (kevent-size))))
      
      ;; Call kevent
      (let ((result (%kevent kq 
                             (if changelist change-buf (sb-alien:null-alien))
                             change-count
                             event-buf
                             nevents
                             (or timeout-ptr (sb-alien:null-alien)))))
        (when (= result -1)
          (error "kevent failed"))
        
        ;; Unpack returned events
        (loop for i from 0 below result
              collect (unpack-kevent event-buf (* i (kevent-size))))))))

(defmacro with-kqueue ((kq-var) &body body)
  "Execute body with kqueue, automatically closing on exit"
  `(let ((,kq-var (kqueue)))
     (unwind-protect
          (progn ,@body)
       (close-kqueue ,kq-var))))

;;;; Convenience Functions

(defun add-event (kq ident filter &key flags fflags data udata)
  "Add an event to kqueue"
  (let ((event (make-kevent
                :ident ident
                :filter filter
                :flags (logior +ev-add+ (or flags 0))
                :fflags (or fflags 0)
                :data (or data 0)
                :udata (or udata 0))))
    (kevent kq (list event) 0 nil)))

(defun delete-event (kq ident filter)
  "Delete an event from kqueue"
  (let ((event (make-kevent
                :ident ident
                :filter filter
                :flags +ev-delete+)))
    (kevent kq (list event) 0 nil)))

(defun wait-for-events (kq max-events &optional timeout)
  "Wait for events on kqueue"
  (kevent kq nil max-events timeout))

;;;; Socket-Specific Helpers

(defun watch-socket-read (kq socket-fd &key udata)
  "Watch socket for read availability"
  (add-event kq socket-fd +evfilt-read+ :udata udata))

(defun watch-socket-write (kq socket-fd &key udata)
  "Watch socket for write availability"
  (add-event kq socket-fd +evfilt-write+ :udata udata))

(defun watch-timer (kq timer-id milliseconds &key udata oneshot)
  "Set up timer event"
  (add-event kq timer-id +evfilt-timer+
             :flags (if oneshot +ev-oneshot+ 0)
             :data milliseconds
             :udata udata))

(defun watch-signal (kq signal-num &key udata)
  "Watch for signal"
  (add-event kq signal-num +evfilt-signal+ :udata udata))