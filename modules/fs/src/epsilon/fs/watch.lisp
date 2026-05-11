;;;; watch.lisp -- Platform-independent filesystem watch interface
;;;;
;;;; Delegates to epsilon.darwin.watch (kqueue) or epsilon.linux.watch
;;;; (inotify) based on the current platform.  Provides a unified API
;;;; for monitoring file and directory changes.
;;;;
;;;; Usage:
;;;;   (fs.watch:with-watcher (w)
;;;;     (fs.watch:add w "src/")
;;;;     (loop for events = (fs.watch:poll w :timeout 1.0)
;;;;           do (dolist (ev events)
;;;;                (format t "~A: ~A~%"
;;;;                        (fs.watch:event-types ev)
;;;;                        (fs.watch:event-path ev)))))

(defpackage epsilon.fs.watch
  (:import cl
           (epsilon.file file))
  (:export
   ;; Watcher lifecycle
   #:make-watcher
   #:add
   #:remove-watch
   #:poll
   #:close-watcher
   #:with-watcher
   ;; Event accessors
   #:event-path
   #:event-types))

;;; ============================================================================
;;; Platform detection and delegation
;;; ============================================================================

(defvar *platform-module* nil
  "Cached platform watch module package.")

(defun platform-watch-package ()
  "Return the platform-specific watch package, loading if needed."
  (or *platform-module*
      (setf *platform-module*
            (or (find-package "EPSILON.DARWIN.WATCH")
                (find-package "EPSILON.LINUX.WATCH")
                (error "No filesystem watch implementation for this platform. ~
                        Requires epsilon.darwin or epsilon.linux.")))))

(defun platform-call (name &rest args)
  "Call function NAME in the platform watch package."
  (apply (symbol-function (find-symbol (string-upcase name) (platform-watch-package)))
         args))

;;; ============================================================================
;;; Watcher API
;;; ============================================================================

(defun make-watcher ()
  "Create a new filesystem watcher."
  (platform-call "MAKE-WATCHER"))

(defun add (watcher path)
  "Watch PATH for filesystem changes.
   PATH can be a file or directory.  Returns T on success."
  (platform-call "WATCHER-ADD" watcher path))

(defun remove-watch (watcher path)
  "Stop watching PATH.  Returns T if it was being watched."
  (platform-call "WATCHER-REMOVE" watcher path))

(defun poll (watcher &key (timeout nil) (max-events 64))
  "Poll for filesystem events.
   TIMEOUT is seconds (nil = block, 0 = non-blocking).
   Returns a list of events.  Use event-path and event-types to inspect."
  (platform-call "WATCHER-POLL" watcher :timeout timeout :max-events max-events))

(defun close-watcher (watcher)
  "Release all resources held by WATCHER."
  (platform-call "WATCHER-CLOSE" watcher))

(defmacro with-watcher ((var) &body body)
  "Create a watcher with automatic cleanup."
  `(let ((,var (make-watcher)))
     (unwind-protect
          (progn ,@body)
       (close-watcher ,var))))

;;; ============================================================================
;;; Event accessors
;;; ============================================================================

(defun event-path (event)
  "Return the filesystem path that changed."
  (platform-call "WATCH-EVENT-PATH" event))

(defun event-types (event)
  "Return a list of change type keywords.
   Possible values: :modified, :deleted, :created, :renamed, :attributes,
   :extended, :link, :revoked."
  (platform-call "WATCH-EVENT-TYPES" event))
