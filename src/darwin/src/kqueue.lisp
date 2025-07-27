(defpackage epsilon.kqueue
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   ;; Core kqueue operations
   kqueue
   kevent
   kqueue-close
   
   ;; Event management
   add-event
   remove-event
   wait-for-events
   
   ;; Event filters and flags
   +evfilt-read+
   +evfilt-write+
   +evfilt-except+
   +evfilt-signal+
   +evfilt-timer+
   +evfilt-vnode+
   +evfilt-proc+
   +evfilt-user+
   
   +ev-add+
   +ev-delete+
   +ev-enable+
   +ev-disable+
   +ev-oneshot+
   +ev-clear+
   +ev-eof+
   +ev-error+
   
   ;; High-level utilities
   with-kqueue
   poll-events
   
   ;; Structure types
   kevent-struct
   timespec-struct
   make-kevent-struct
   make-timespec-struct
   
   ;; kevent-struct accessors
   kevent-struct-ident
   kevent-struct-filter
   kevent-struct-flags
   kevent-struct-fflags
   kevent-struct-data
   kevent-struct-udata
   
   ;; timespec-struct accessors
   timespec-struct-tv-sec
   timespec-struct-tv-nsec))

(in-package epsilon.kqueue)

;;; BSD kqueue constants (macOS/FreeBSD)

;; Event filters
(defconstant +evfilt-read+ -1)
(defconstant +evfilt-write+ -2)
(defconstant +evfilt-except+ -15)
(defconstant +evfilt-signal+ -6)
(defconstant +evfilt-timer+ -7)
(defconstant +evfilt-vnode+ -4)
(defconstant +evfilt-proc+ -5)
(defconstant +evfilt-user+ -10)

;; Event flags
(defconstant +ev-add+ #x0001)
(defconstant +ev-delete+ #x0002)
(defconstant +ev-enable+ #x0004)
(defconstant +ev-disable+ #x0008)
(defconstant +ev-oneshot+ #x0010)
(defconstant +ev-clear+ #x0020)
(defconstant +ev-eof+ #x8000)
(defconstant +ev-error+ #x4000)

;;; Foreign function declarations using epsilon.foreign

;; int kqueue(void);
(defun %kqueue ()
  (lib:shared-call '("kqueue" "/usr/lib/libSystem.B.dylib") :int '()))

;; int kevent(int kq, const struct kevent *changelist, int nchanges,
;;            struct kevent *eventlist, int nevents, 
;;            const struct timespec *timeout);
(defun %kevent (kq changelist nchanges eventlist nevents timeout)
  (lib:shared-call '("kevent" "/usr/lib/libSystem.B.dylib") :int 
                   '(:int :pointer :int :pointer :int :pointer)
                   kq changelist nchanges eventlist nevents timeout))

;; int close(int fd);
(defun %close (fd)
  (lib:shared-call '("close" "/usr/lib/libSystem.B.dylib") :int '(:int) fd))

;;; Structure definitions

;; struct kevent {
;;     uintptr_t  ident;        /* identifier for this event */
;;     int16_t    filter;       /* filter for event */
;;     uint16_t   flags;        /* general flags */
;;     uint32_t   fflags;       /* filter-specific flags */
;;     intptr_t   data;         /* filter-specific data */
;;     void      *udata;        /* opaque user data identifier */
;; };

(defstruct kevent-struct
  ident    ; uintptr_t (use integer)
  filter   ; int16_t
  flags    ; uint16_t  
  fflags   ; uint32_t
  data     ; intptr_t (use integer)
  udata)   ; void* (use integer for pointer)

;; struct timespec {
;;     time_t tv_sec;        /* seconds */
;;     long   tv_nsec;       /* nanoseconds */
;; };

(defstruct timespec-struct
  tv-sec   ; time_t (long)
  tv-nsec) ; long

;;; Memory layout helpers

(defun make-kevent-memory (kevent-list)
  "Allocate memory for kevent structures"
  (let* ((count (length kevent-list))
         (size (* count 32)) ; 32 bytes per kevent on 64-bit systems
         (memory (lib:foreign-alloc size)))
    
    ;; Fill in the kevent structures using SAP operations
    (loop for i from 0
          for ke in kevent-list
          for offset = (* i 32)
          do (progn
               ;; Use SAP operations since memory is now a system area pointer
               (setf (sb-sys:sap-ref-64 memory (+ offset 0)) (or (kevent-struct-ident ke) 0))
               (setf (sb-sys:signed-sap-ref-16 memory (+ offset 8)) (or (kevent-struct-filter ke) 0))
               (setf (sb-sys:sap-ref-16 memory (+ offset 10)) (or (kevent-struct-flags ke) 0))
               (setf (sb-sys:sap-ref-32 memory (+ offset 12)) (or (kevent-struct-fflags ke) 0))
               (setf (sb-sys:sap-ref-64 memory (+ offset 16)) (or (kevent-struct-data ke) 0))
               (setf (sb-sys:sap-ref-64 memory (+ offset 24)) (or (kevent-struct-udata ke) 0))))
    
    (values memory count)))

(defun parse-kevent-memory (memory count)
  "Parse kevent structures from memory"
  (loop for i from 0 below count
        for offset = (* i 32)
        collect (make-kevent-struct
                 :ident (sb-sys:sap-ref-64 memory (+ offset 0))
                 :filter (sb-sys:signed-sap-ref-16 memory (+ offset 8))
                 :flags (sb-sys:sap-ref-16 memory (+ offset 10))
                 :fflags (sb-sys:sap-ref-32 memory (+ offset 12))
                 :data (sb-sys:sap-ref-64 memory (+ offset 16))
                 :udata (sb-sys:sap-ref-64 memory (+ offset 24)))))

(defun make-timespec-memory (seconds nanoseconds)
  "Allocate memory for timespec structure"
  (let ((memory (lib:foreign-alloc 16))) ; 16 bytes for timespec
    (setf (sb-sys:sap-ref-64 memory 0) seconds)      ; tv_sec
    (setf (sb-sys:sap-ref-64 memory 8) nanoseconds)  ; tv_nsec  
    memory))

;;; Core kqueue operations

(defun kqueue ()
  "Create a new kqueue file descriptor"
  (let ((fd (%kqueue)))
    (when (= fd -1)
      (error "Failed to create kqueue"))
    fd))

(defun kqueue-close (kq)
  "Close a kqueue file descriptor"
  (%close kq))

(defun kevent (kq changelist eventlist &optional timeout-seconds)
  "Perform kevent operation - register changes and/or wait for events"
  (let ((change-memory nil)
        (change-count 0)
        (event-memory nil)
        (event-count (length eventlist))
        (timeout-memory nil))
    
    (unwind-protect
         (progn
           ;; Prepare changelist if provided
           (when changelist
             (multiple-value-setq (change-memory change-count)
               (make-kevent-memory changelist)))
           
           ;; Prepare eventlist memory
           (when (> event-count 0)
             (setf event-memory (lib:foreign-alloc (* event-count 32))))
           
           ;; Prepare timeout if provided
           (when timeout-seconds
             (multiple-value-bind (sec nsec)
                 (truncate timeout-seconds)
               (setf nsec (truncate (* nsec 1000000000)))
               (setf timeout-memory (make-timespec-memory sec nsec))))
           
           ;; Call kevent
           (let ((result (%kevent kq 
                                  (or change-memory 0)
                                  change-count
                                  (or event-memory 0)
                                  event-count
                                  (or timeout-memory 0))))
             (cond
               ((= result -1) (error "kevent failed"))
               ((= result 0) '()) ; Timeout
               (t (parse-kevent-memory event-memory result)))))
      
      ;; Cleanup
      (when change-memory (lib:foreign-free change-memory))
      (when event-memory (lib:foreign-free event-memory))
      (when timeout-memory (lib:foreign-free timeout-memory)))))

;;; High-level event management

(defun add-event (kq fd filter &key flags data udata)
  "Add an event to monitor"
  (let ((changelist (list (make-kevent-struct
                           :ident fd
                           :filter filter
                           :flags (logior +ev-add+ (or flags 0))
                           :fflags 0
                           :data (or data 0)
                           :udata (or udata 0)))))
    (kevent kq changelist '())))

(defun remove-event (kq fd filter)
  "Remove an event from monitoring"
  (let ((changelist (list (make-kevent-struct
                           :ident fd
                           :filter filter
                           :flags +ev-delete+
                           :fflags 0
                           :data 0
                           :udata 0))))
    (kevent kq changelist '())))

(defun wait-for-events (kq max-events &optional timeout-seconds)
  "Wait for events to occur"
  (let ((eventlist (make-list max-events :initial-element nil)))
    (kevent kq nil eventlist timeout-seconds)))

;;; High-level utilities

(defmacro with-kqueue ((var) &body body)
  "Create kqueue with automatic cleanup"
  `(let ((,var (kqueue)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (kqueue-close ,var)))))

(defun poll-events (kq &key (max-events 64) (timeout 0))
  "Poll for events with reasonable defaults"
  (wait-for-events kq max-events timeout))

;;; Example usage and testing functions

(defun test-kqueue ()
  "Basic test of kqueue functionality"
  (with-kqueue (kq)
    (format t "Created kqueue: ~A~%" kq)
    
    ;; Add stdin for reading
    (add-event kq 0 +evfilt-read+)
    (format t "Added stdin read event~%")
    
    ;; Wait for events (non-blocking)
    (let ((events (poll-events kq :timeout 0)))
      (format t "Events: ~A~%" events))
    
    ;; Remove the event
    (remove-event kq 0 +evfilt-read+)
    (format t "Removed stdin read event~%")))

(defun simple-socket-monitor (socket-fd)
  "Monitor a socket file descriptor for read events"
  (with-kqueue (kq)
    (add-event kq socket-fd +evfilt-read+)
    (format t "Monitoring socket ~A for read events~%" socket-fd)
    
    (loop
      (let ((events (wait-for-events kq 1 1.0))) ; 1 second timeout
        (if events
            (progn
              (format t "Socket ready for reading~%")
              (return t))
            (format t "Waiting...~%"))))))
