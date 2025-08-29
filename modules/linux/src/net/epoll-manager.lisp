;;;; Centralized Epoll Manager for Linux Networking
;;;;
;;;; This module provides a singleton epoll instance manager to efficiently
;;;; handle multiple sockets with a single epoll instance per thread.

(defpackage epsilon.net.epoll-manager
  (:use cl)
  (:local-nicknames
   (epoll epsilon.sys.epoll)
   (lib epsilon.foreign))
  (:export
   ;; Manager operations
   #:get-epoll-manager
   #:shutdown-epoll-manager
   #:with-epoll-manager
   
   ;; Socket registration
   #:register-socket
   #:unregister-socket
   #:modify-socket-events
   
   ;; Event waiting
   #:wait-for-socket
   #:wait-for-any-socket
   #:poll-sockets
   
   ;; Configuration
   #:*use-edge-triggered*
   #:*max-events*))

(in-package epsilon.net.epoll-manager)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defparameter *use-edge-triggered* nil
  "When true, use edge-triggered mode for better performance")

(defparameter *max-events* 64
  "Maximum number of events to retrieve in a single epoll_wait call")

;;; ============================================================================
;;; Thread-Local Epoll Manager
;;; ============================================================================

(defvar *thread-epoll-manager* nil
  "Thread-local epoll manager instance")

(defstruct epoll-manager
  "Manages a single epoll instance for multiple sockets"
  (epfd nil :type (or null integer))
  (registered-sockets (make-hash-table) :type hash-table)
  (lock (sb-thread:make-mutex :name "epoll-manager") :type sb-thread:mutex))

;;; ============================================================================
;;; Manager Lifecycle
;;; ============================================================================

(defun get-epoll-manager ()
  "Get or create the thread-local epoll manager"
  (unless *thread-epoll-manager*
    (let ((epfd (epoll:epoll-create1 epoll:+epoll-cloexec+)))
      (when (< epfd 0)
        (error "Failed to create epoll instance"))
      (setf *thread-epoll-manager*
            (make-epoll-manager :epfd epfd))))
  *thread-epoll-manager*)

(defun shutdown-epoll-manager ()
  "Shutdown the thread-local epoll manager"
  (when *thread-epoll-manager*
    (let ((manager *thread-epoll-manager*))
      (sb-thread:with-mutex ((epoll-manager-lock manager))
        ;; Unregister all sockets
        (maphash (lambda (fd info)
                   (declare (ignore info))
                   (handler-case
                       (epoll:epoll-ctl (epoll-manager-epfd manager)
                                        epoll:+epoll-ctl-del+
                                        fd nil)
                     (error (e)
                       (warn "Failed to unregister socket ~D: ~A" fd e))))
                 (epoll-manager-registered-sockets manager))
        ;; Clear the hash table
        (clrhash (epoll-manager-registered-sockets manager))
        ;; Close epoll
        (when (epoll-manager-epfd manager)
          (epoll:epoll-close (epoll-manager-epfd manager))
          (setf (epoll-manager-epfd manager) nil))))
    (setf *thread-epoll-manager* nil)))

(defmacro with-epoll-manager ((&key create-new) &body body)
  "Execute body with an epoll manager, optionally creating a new one"
  (let ((old-manager (gensym)))
    `(let ((,old-manager (when ,create-new *thread-epoll-manager*)))
       (when ,create-new
         (setf *thread-epoll-manager* nil))
       (unwind-protect
            (progn ,@body)
         (when ,create-new
           (shutdown-epoll-manager)
           (setf *thread-epoll-manager* ,old-manager))))))

;;; ============================================================================
;;; Socket Registration
;;; ============================================================================

(defun compute-epoll-events (events-list)
  "Convert a list of event keywords to epoll event mask"
  (let ((mask 0))
    (dolist (event events-list)
      (setf mask
            (logior mask
                    (case event
                      (:in epoll:+epollin+)
                      (:out epoll:+epollout+)
                      (:err epoll:+epollerr+)
                      (:hup epoll:+epollhup+)
                      (:rdhup epoll:+epollrdhup+)
                      (:pri epoll:+epollpri+)
                      (:et (if *use-edge-triggered* epoll:+epollet+ 0))
                      (:oneshot epoll:+epolloneshot+)
                      (t 0)))))
    mask))

(defun register-socket (fd events &key data)
  "Register a socket with the epoll manager"
  (let ((manager (get-epoll-manager)))
    (sb-thread:with-mutex ((epoll-manager-lock manager))
      (when (gethash fd (epoll-manager-registered-sockets manager))
        (error "Socket ~D is already registered" fd))
      
      (let* ((event-mask (compute-epoll-events events))
             (event-mask (if *use-edge-triggered*
                            (logior event-mask epoll:+epollet+)
                            event-mask))
             (epoll-event (epoll:make-epoll-event 
                          :events event-mask
                          :data (or data fd))))
        
        (handler-case
            (progn
              (epoll:epoll-ctl (epoll-manager-epfd manager)
                              epoll:+epoll-ctl-add+
                              fd epoll-event)
              (setf (gethash fd (epoll-manager-registered-sockets manager))
                    (list :events events :data data))
              t)
          (error (e)
            (error "Failed to register socket ~D: ~A" fd e)))))))

(defun unregister-socket (fd)
  "Unregister a socket from the epoll manager"
  (let ((manager (get-epoll-manager)))
    (sb-thread:with-mutex ((epoll-manager-lock manager))
      (when (gethash fd (epoll-manager-registered-sockets manager))
        (handler-case
            (epoll:epoll-ctl (epoll-manager-epfd manager)
                            epoll:+epoll-ctl-del+
                            fd nil)
          (error (e)
            (warn "Failed to unregister socket ~D: ~A" fd e)))
        (remhash fd (epoll-manager-registered-sockets manager))
        t))))

(defun modify-socket-events (fd new-events)
  "Modify the events monitored for a socket"
  (let ((manager (get-epoll-manager)))
    (sb-thread:with-mutex ((epoll-manager-lock manager))
      (let ((info (gethash fd (epoll-manager-registered-sockets manager))))
        (unless info
          (error "Socket ~D is not registered" fd))
        
        (let* ((data (getf info :data))
               (event-mask (compute-epoll-events new-events))
               (event-mask (if *use-edge-triggered*
                              (logior event-mask epoll:+epollet+)
                              event-mask))
               (epoll-event (epoll:make-epoll-event 
                            :events event-mask
                            :data (or data fd))))
          
          (handler-case
              (progn
                (epoll:epoll-ctl (epoll-manager-epfd manager)
                                epoll:+epoll-ctl-mod+
                                fd epoll-event)
                (setf (getf (gethash fd (epoll-manager-registered-sockets manager)) :events)
                      new-events)
                t)
            (error (e)
              (error "Failed to modify socket ~D: ~A" fd e))))))))

;;; ============================================================================
;;; Event Waiting
;;; ============================================================================

(defun wait-for-socket (fd events timeout-ms)
  "Wait for specific events on a socket"
  (let ((manager (get-epoll-manager)))
    ;; Ensure socket is registered
    (unless (gethash fd (epoll-manager-registered-sockets manager))
      (register-socket fd events))
    
    ;; Modify events if needed
    (let ((current-info (gethash fd (epoll-manager-registered-sockets manager))))
      (unless (equal (getf current-info :events) events)
        (modify-socket-events fd events)))
    
    ;; Wait for events
    (let ((events (epoll:wait-for-events (epoll-manager-epfd manager)
                                         *max-events*
                                         timeout-ms)))
      ;; Find events for our fd
      (find-if (lambda (event)
                 (= (epoll:epoll-event-data event) fd))
               events))))

(defun wait-for-any-socket (timeout-ms)
  "Wait for events on any registered socket"
  (let ((manager (get-epoll-manager)))
    (epoll:wait-for-events (epoll-manager-epfd manager)
                          *max-events*
                          timeout-ms)))

(defun poll-sockets ()
  "Poll for events without blocking"
  (wait-for-any-socket 0))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun registered-socket-count ()
  "Return the number of registered sockets"
  (let ((manager (get-epoll-manager)))
    (hash-table-count (epoll-manager-registered-sockets manager))))

(defun list-registered-sockets ()
  "Return a list of registered socket file descriptors"
  (let ((manager (get-epoll-manager)))
    (loop for fd being the hash-keys of (epoll-manager-registered-sockets manager)
          collect fd)))