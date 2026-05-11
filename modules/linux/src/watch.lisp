;;;; watch.lisp -- Filesystem watching via inotify
;;;;
;;;; Provides file and directory change notifications using Linux's
;;;; inotify API.  Each watcher holds an inotify fd and maps watch
;;;; descriptors to paths.

(defpackage epsilon.linux.watch
  (:import cl
           (epsilon.foreign lib)
           (epsilon.fs file))
  (:export
   #:watcher
   #:make-watcher
   #:watcher-add
   #:watcher-remove
   #:watcher-poll
   #:watcher-close
   #:with-watcher
   ;; Event accessors
   #:watch-event
   #:watch-event-path
   #:watch-event-types))

;;; ============================================================================
;;; inotify constants (from sys/inotify.h)
;;; ============================================================================

(defconstant +in-access+        #x00000001)
(defconstant +in-modify+        #x00000002)
(defconstant +in-attrib+        #x00000004)
(defconstant +in-close-write+   #x00000008)
(defconstant +in-close-nowrite+ #x00000010)
(defconstant +in-open+          #x00000020)
(defconstant +in-moved-from+    #x00000040)
(defconstant +in-moved-to+      #x00000080)
(defconstant +in-create+        #x00000100)
(defconstant +in-delete+        #x00000200)
(defconstant +in-delete-self+   #x00000400)
(defconstant +in-move-self+     #x00000800)

(defconstant +in-all-events+
  (logior +in-modify+ +in-attrib+ +in-close-write+
          +in-moved-from+ +in-moved-to+
          +in-create+ +in-delete+ +in-delete-self+ +in-move-self+)
  "All standard inotify event flags.")

;;; ============================================================================
;;; inotify FFI
;;; ============================================================================

(defun %inotify-init ()
  "Create a new inotify instance. Returns fd or -1."
  (lib:shared-call '("inotify_init" "libc.so.6") :int '()))

(defun %inotify-add-watch (fd path mask)
  "Add or modify a watch.  Returns watch descriptor or -1."
  (lib:shared-call '("inotify_add_watch" "libc.so.6") :int
                   '(:int :string :int)
                   fd path mask))

(defun %inotify-rm-watch (fd wd)
  "Remove a watch.  Returns 0 on success."
  (lib:shared-call '("inotify_rm_watch" "libc.so.6") :int
                   '(:int :int)
                   fd wd))

(defun %close (fd)
  (lib:shared-call '("close" "libc.so.6") :int '(:int) fd))

(defun %read (fd buf size)
  (lib:shared-call '("read" "libc.so.6") :int
                   '(:int :pointer :int)
                   fd buf size))

(defconstant +pollin+ #x0001)

(defun %poll (fd timeout-ms)
  "Poll a single fd for readability.  TIMEOUT-MS is milliseconds (-1 = block).
   Returns positive if ready, 0 on timeout, negative on error."
  ;; struct pollfd { int fd; short events; short revents; } = 8 bytes
  (let ((pollfd (lib:foreign-alloc 8)))
    (unwind-protect
         (progn
           (setf (sb-sys:signed-sap-ref-32 pollfd 0) fd)
           (setf (sb-sys:signed-sap-ref-16 pollfd 4) +pollin+)
           (setf (sb-sys:signed-sap-ref-16 pollfd 6) 0)
           (lib:shared-call '("poll" "libc.so.6") :int
                            '(:pointer :int :int)
                            pollfd 1 timeout-ms))
      (lib:foreign-free pollfd))))

;;; ============================================================================
;;; Watch Event
;;; ============================================================================

(defstruct watch-event
  "A filesystem change event."
  (path "" :type string)
  (types nil :type list))

(defun mask-to-event-types (mask)
  "Convert inotify mask to a list of event type keywords."
  (let ((types nil))
    (when (logtest mask +in-delete+)      (push :deleted types))
    (when (logtest mask +in-delete-self+)  (push :deleted types))
    (when (logtest mask +in-modify+)      (push :modified types))
    (when (logtest mask +in-attrib+)      (push :attributes types))
    (when (logtest mask +in-create+)      (push :created types))
    (when (logtest mask +in-moved-from+)  (push :renamed types))
    (when (logtest mask +in-moved-to+)    (push :renamed types))
    (when (logtest mask +in-move-self+)   (push :renamed types))
    (delete-duplicates (nreverse types))))

;;; ============================================================================
;;; inotify event parsing
;;; ============================================================================

;; struct inotify_event {
;;     int      wd;       /* Watch descriptor */
;;     uint32_t mask;     /* Mask of events */
;;     uint32_t cookie;   /* Unique cookie for rename pairs */
;;     uint32_t len;      /* Size of name field */
;;     char     name[];   /* Optional name */
;; };
;; Fixed size = 16 bytes, then variable-length name

(defconstant +inotify-event-size+ 16)
(defconstant +inotify-buf-size+ 4096)

(defun parse-inotify-events (buf bytes-read wd-to-path)
  "Parse raw inotify events from BUF.  Returns list of watch-event structs."
  (let ((events nil)
        (offset 0))
    (loop while (< offset bytes-read) do
      (let* ((wd (sb-sys:signed-sap-ref-32 buf offset))
             (mask (sb-sys:sap-ref-32 buf (+ offset 4)))
             ;; cookie at offset+8, skip
             (name-len (sb-sys:sap-ref-32 buf (+ offset 12)))
             (base-path (gethash wd wd-to-path))
             (child-name (when (> name-len 0)
                           ;; Read null-terminated string from name field
                           (let ((name-start (+ offset +inotify-event-size+)))
                             (sb-ext:octets-to-string
                              (let ((bytes (make-array name-len
                                                       :element-type '(unsigned-byte 8))))
                                (dotimes (i name-len)
                                  (setf (aref bytes i)
                                        (sb-sys:sap-ref-8 buf (+ name-start i))))
                                bytes)
                              :external-format :utf-8))))
             (path (if (and child-name (> (length child-name) 0)
                            (char/= (char child-name 0) #\Nul))
                     (file:join-paths (or base-path "") child-name)
                     (or base-path ""))))
        (push (make-watch-event
               :path path
               :types (mask-to-event-types mask))
              events)
        (incf offset (+ +inotify-event-size+ name-len))))
    (nreverse events)))

;;; ============================================================================
;;; Watcher
;;; ============================================================================

(defstruct (watcher (:constructor %make-watcher))
  "Filesystem watcher backed by an inotify fd."
  (ifd nil :type (or integer null))
  (wd-to-path (make-hash-table :test 'eql) :type hash-table)
  (path-to-wd (make-hash-table :test 'equal) :type hash-table))

(defun make-watcher ()
  "Create a new filesystem watcher."
  (let ((fd (%inotify-init)))
    (when (< fd 0)
      (error "inotify_init failed"))
    (%make-watcher :ifd fd)))

(defun watcher-add (watcher path &key (events +in-all-events+))
  "Watch PATH for filesystem events.
   EVENTS is a bitmask of IN_* constants (default: all events).
   Returns T on success."
  (let* ((abs-path (if (file:is-absolute path)
                     path
                     (file:join-paths (file:get-cwd) path)))
         (wd (%inotify-add-watch (watcher-ifd watcher) abs-path events)))
    (when (< wd 0)
      (return-from watcher-add nil))
    (setf (gethash wd (watcher-wd-to-path watcher)) abs-path)
    (setf (gethash abs-path (watcher-path-to-wd watcher)) wd)
    t))

(defun watcher-remove (watcher path)
  "Stop watching PATH.  Returns T if it was being watched."
  (let* ((abs-path (if (file:is-absolute path)
                     path
                     (file:join-paths (file:get-cwd) path)))
         (wd (gethash abs-path (watcher-path-to-wd watcher))))
    (when wd
      (%inotify-rm-watch (watcher-ifd watcher) wd)
      (remhash wd (watcher-wd-to-path watcher))
      (remhash abs-path (watcher-path-to-wd watcher))
      t)))

(defun watcher-poll (watcher &key (timeout nil) (max-events 64))
  "Poll for filesystem events.
   TIMEOUT is seconds (nil = block indefinitely, 0 = non-blocking).
   Returns a list of watch-event structs."
  (declare (ignore max-events))
  (let* ((timeout-ms (cond
                       ((null timeout) -1)
                       ((zerop timeout) 0)
                       (t (round (* timeout 1000)))))
         (ready (%poll (watcher-ifd watcher) timeout-ms)))
    (when (> ready 0)
      (let ((buf (lib:foreign-alloc +inotify-buf-size+)))
        (unwind-protect
             (let ((n (%read (watcher-ifd watcher) buf +inotify-buf-size+)))
               (if (> n 0)
                 (parse-inotify-events buf n (watcher-wd-to-path watcher))
                 nil))
          (lib:foreign-free buf))))))

(defun watcher-close (watcher)
  "Release all resources held by WATCHER."
  (maphash (lambda (wd path)
             (declare (ignore path))
             (%inotify-rm-watch (watcher-ifd watcher) wd))
           (watcher-wd-to-path watcher))
  (clrhash (watcher-wd-to-path watcher))
  (clrhash (watcher-path-to-wd watcher))
  (when (watcher-ifd watcher)
    (%close (watcher-ifd watcher))
    (setf (watcher-ifd watcher) nil)))

(defmacro with-watcher ((var) &body body)
  "Create a watcher with automatic cleanup."
  `(let ((,var (make-watcher)))
     (unwind-protect
          (progn ,@body)
       (watcher-close ,var))))
