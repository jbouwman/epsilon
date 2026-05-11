;;;; watch.lisp -- Filesystem watching via kqueue EVFILT_VNODE
;;;;
;;;; Provides file and directory change notifications using kqueue's
;;;; vnode filter.  Each watcher holds a dedicated kqueue fd and tracks
;;;; open file descriptors for watched paths.

(defpackage epsilon.darwin.watch
  (:import cl
           (epsilon.kqueue kqueue)
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
   #:watch-event-types
   ;; Vnode constants
   #:+note-delete+
   #:+note-write+
   #:+note-extend+
   #:+note-attrib+
   #:+note-link+
   #:+note-rename+
   #:+note-revoke+
   #:+note-funlock+
   #:+all-vnode-events+))

;;; ============================================================================
;;; Vnode fflags constants (from sys/event.h)
;;; ============================================================================

(defconstant +note-delete+  #x00000001 "File was deleted")
(defconstant +note-write+   #x00000002 "File was modified")
(defconstant +note-extend+  #x00000004 "File was extended")
(defconstant +note-attrib+  #x00000008 "Attributes changed")
(defconstant +note-link+    #x00000010 "Link count changed")
(defconstant +note-rename+  #x00000020 "File was renamed")
(defconstant +note-revoke+  #x00000040 "Access was revoked")
(defconstant +note-funlock+ #x00000100 "File was unlocked")

(defconstant +all-vnode-events+
  (logior +note-delete+ +note-write+ +note-extend+
          +note-attrib+ +note-link+ +note-rename+ +note-revoke+)
  "All standard vnode event flags.")

;;; ============================================================================
;;; POSIX open/close for file descriptors
;;; ============================================================================

(defconstant +o-rdonly+ 0)
(defconstant +o-evtonly+ #x8000 "Open for event monitoring only (macOS)")

(defun %open (path flags)
  "Open PATH with FLAGS, returning file descriptor or -1."
  (lib:shared-call '("open" "/usr/lib/libSystem.B.dylib") :int
                   '(:string :int :int)
                   path (logior flags +o-rdonly+) 0))

(defun %close (fd)
  "Close file descriptor FD."
  (lib:shared-call '("close" "/usr/lib/libSystem.B.dylib") :int '(:int) fd))

;;; ============================================================================
;;; Watch Event
;;; ============================================================================

(defstruct watch-event
  "A filesystem change event."
  (path "" :type string)
  (types nil :type list))

(defun fflags-to-event-types (fflags)
  "Convert kqueue vnode fflags to a list of event type keywords."
  (let ((types nil))
    (when (logtest fflags +note-delete+)  (push :deleted types))
    (when (logtest fflags +note-write+)   (push :modified types))
    (when (logtest fflags +note-extend+)  (push :extended types))
    (when (logtest fflags +note-attrib+)  (push :attributes types))
    (when (logtest fflags +note-link+)    (push :link types))
    (when (logtest fflags +note-rename+)  (push :renamed types))
    (when (logtest fflags +note-revoke+)  (push :revoked types))
    (nreverse types)))

;;; ============================================================================
;;; Watcher
;;; ============================================================================

(defstruct (watcher (:constructor %make-watcher))
  "Filesystem watcher backed by a kqueue fd.
   Tracks open file descriptors for watched paths."
  (kq nil :type (or integer null))
  (fd-to-path (make-hash-table :test 'eql) :type hash-table)
  (path-to-fd (make-hash-table :test 'equal) :type hash-table))

(defun make-watcher ()
  "Create a new filesystem watcher."
  (%make-watcher :kq (kqueue:kqueue)))

(defun watcher-add (watcher path &key (events +all-vnode-events+))
  "Watch PATH for filesystem events.
   EVENTS is a bitmask of NOTE_* constants (default: all events).
   Returns T on success, NIL if the path cannot be opened."
  (let* ((abs-path (if (file:is-absolute path)
                     path
                     (file:join-paths (file:get-cwd) path)))
         (existing-fd (gethash abs-path (watcher-path-to-fd watcher))))
    ;; Remove existing watch if re-adding
    (when existing-fd
      (watcher-remove watcher abs-path))
    ;; Open file for event monitoring only
    (let ((fd (%open abs-path +o-evtonly+)))
      (when (< fd 0)
        (return-from watcher-add nil))
      ;; Register vnode event with kqueue
      (handler-case
          (progn
            (kqueue:kevent (watcher-kq watcher)
                           (list (kqueue:make-kevent-struct
                                  :ident fd
                                  :filter kqueue:+evfilt-vnode+
                                  :flags (logior kqueue:+ev-add+ kqueue:+ev-clear+)
                                  :fflags events
                                  :data 0
                                  :udata 0))
                           nil)
            (setf (gethash fd (watcher-fd-to-path watcher)) abs-path)
            (setf (gethash abs-path (watcher-path-to-fd watcher)) fd)
            t)
        (error ()
          (%close fd)
          nil)))))

(defun watcher-remove (watcher path)
  "Stop watching PATH.  Returns T if it was being watched."
  (let* ((abs-path (if (file:is-absolute path)
                     path
                     (file:join-paths (file:get-cwd) path)))
         (fd (gethash abs-path (watcher-path-to-fd watcher))))
    (when fd
      (handler-case
          (kqueue:remove-event (watcher-kq watcher) fd kqueue:+evfilt-vnode+)
        (error () nil))
      (%close fd)
      (remhash fd (watcher-fd-to-path watcher))
      (remhash abs-path (watcher-path-to-fd watcher))
      t)))

(defun watcher-poll (watcher &key (timeout nil) (max-events 64))
  "Poll for filesystem events.
   TIMEOUT is seconds (nil = block indefinitely, 0 = non-blocking).
   Returns a list of watch-event structs."
  (let ((raw-events (kqueue:wait-for-events
                     (watcher-kq watcher)
                     max-events
                     timeout)))
    (let ((results nil))
      (dolist (ev raw-events)
        (let* ((fd (kqueue:kevent-struct-ident ev))
               (fflags (kqueue:kevent-struct-fflags ev))
               (path (gethash fd (watcher-fd-to-path watcher))))
          (when path
            (push (make-watch-event
                   :path path
                   :types (fflags-to-event-types fflags))
                  results))))
      (nreverse results))))

(defun watcher-close (watcher)
  "Release all resources held by WATCHER."
  (maphash (lambda (fd path)
             (declare (ignore path))
             (%close fd))
           (watcher-fd-to-path watcher))
  (clrhash (watcher-fd-to-path watcher))
  (clrhash (watcher-path-to-fd watcher))
  (when (watcher-kq watcher)
    (kqueue:kqueue-close (watcher-kq watcher))
    (setf (watcher-kq watcher) nil)))

(defmacro with-watcher ((var) &body body)
  "Create a watcher with automatic cleanup."
  `(let ((,var (make-watcher)))
     (unwind-protect
          (progn ,@body)
       (watcher-close ,var))))
