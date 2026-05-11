;;;; Reactor-based Epoll Manager for Linux Networking
;;;;
;;;; A single reactor thread owns the epoll fd and dispatches events
;;;; to waiting threads via condition variables. This eliminates the
;;;; cross-thread event consumption bug where multiple threads calling
;;;; epoll_wait on the same epfd would steal each other's events.

(defpackage epsilon.net.reactor
  (:use cl)
  (:import
   (epsilon.sys.epoll epoll)
   (epsilon.foreign lib)
   (epsilon.sys.thread thread)
   (epsilon.sys.lock lock)
   (epsilon.sys.semaphore sem))
  (:export
   ;; Reactor lifecycle
   #:boot-reactor
   #:shutdown-reactor
   #:with-reactor

   ;; Socket registration
   #:register-socket
   #:unregister-socket
   #:modify-socket-events
   #:socket-registered-p

   ;; Event waiting
   #:wait-for-socket
   #:wait-for-any-socket
   #:register-socket-callback
   #:poll-sockets

   ;; Configuration
   #:*use-edge-triggered*
   #:*max-events*))

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defparameter *use-edge-triggered* nil
  "When true, use edge-triggered mode for better performance")

(defparameter *max-events* 64
  "Maximum number of events to retrieve in a single epoll_wait call")

;;; ============================================================================
;;; FFI for pipe-based reactor wakeup
;;; ============================================================================

(lib:defshared %pipe "pipe" "libc" :int
  (pipefd :pointer)
  :documentation "Create pipe")

(lib:defshared %read-fd "read" "libc" :long
  (fd :int) (buf :pointer) (count :unsigned-long)
  :documentation "Read from file descriptor")

(lib:defshared %write-fd "write" "libc" :long
  (fd :int) (buf :pointer) (count :unsigned-long)
  :documentation "Write to file descriptor")

(lib:defshared %close-fd "close" "libc" :int
  (fd :int)
  :documentation "Close file descriptor")

(lib:defshared %fcntl-mgr "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int)
  :documentation "File control operations")

;; Local constants (avoid circular dependency on epsilon.net.constants)
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #o4000)

;;; ============================================================================
;;; Waiter Structure
;;; ============================================================================

(defstruct waiter
  "A thread waiting for epoll events on a specific fd.
   The reactor sets DONE to t and stores the event in RESULT,
   then signals the semaphore to wake the waiting thread.
   When CALLBACK is non-nil, the reactor calls it instead of
   signaling the semaphore (one-shot, for coroutine integration)."
  (interest-mask 0 :type (unsigned-byte 32))
  (result nil)
  (done nil)
  (semaphore (sem:make-semaphore :name "waiter-sem"))
  (callback nil :type (or null function)))

;;; ============================================================================
;;; Epoll Manager Structure
;;; ============================================================================

(defstruct epoll-manager
  "Manages a single epoll instance with a dedicated reactor thread.
   The reactor is the sole caller of epoll_wait and dispatches events
   to per-fd waiters or the catch-all any-queue."
  ;; Core epoll state
  (epfd nil :type (or null integer))
  (registered-sockets (make-hash-table) :type hash-table)
  (lock (lock:make-lock "epoll-manager") :type lock:lock)
  ;; Reactor thread
  (reactor-thread nil)
  (reactor-running nil)
  ;; Wakeup pipe for signaling the reactor
  (wakeup-read-fd nil :type (or null integer))
  (wakeup-write-fd nil :type (or null integer))
  ;; Per-fd waiters: fd -> list of waiter structs
  (waiters (make-hash-table) :type hash-table)
  (waiters-lock (lock:make-lock "epoll-waiters") :type lock:lock)
  ;; Catch-all event queue for wait-for-any-socket
  (any-queue nil :type list)
  (any-queue-lock (lock:make-lock "epoll-any-queue") :type lock:lock)
  (any-queue-cv (lock:make-condition-variable :name "epoll-any-queue-cv")))

;;; ============================================================================
;;; Global Manager Instance
;;; ============================================================================

(defvar *epoll-manager* nil
  "Global epoll manager instance (shared across all threads)")

(defvar *epoll-manager-init-lock* (lock:make-lock "epoll-manager-init")
  "Lock for thread-safe initialization of the global manager")

;;; ============================================================================
;;; Wakeup Pipe
;;; ============================================================================

(defun create-wakeup-pipe ()
  "Create a non-blocking pipe for reactor wakeup. Returns (read-fd . write-fd)."
  (lib:with-foreign-memory ((pipefd :int :count 2))
    (let ((result (%pipe pipefd)))
      (when (< result 0)
        (error "Failed to create wakeup pipe"))
      (let ((read-fd (sb-sys:sap-ref-32 pipefd 0))
            (write-fd (sb-sys:sap-ref-32 pipefd 4)))
        ;; Set both ends to non-blocking
        (dolist (fd (list read-fd write-fd))
          (let ((flags (%fcntl-mgr fd +f-getfl+ 0)))
            (when (>= flags 0)
              (%fcntl-mgr fd +f-setfl+ (logior flags +o-nonblock+)))))
        (cons read-fd write-fd)))))

(defun wake-reactor (manager)
  "Wake the reactor thread from epoll_wait by writing to the wakeup pipe"
  (let ((write-fd (epoll-manager-wakeup-write-fd manager)))
    (when write-fd
      (handler-case
          (lib:with-foreign-memory ((buf :char :count 1))
            (setf (sb-sys:sap-ref-8 buf 0) 1)
            (%write-fd write-fd buf 1))
        (error () nil)))))

(defun drain-wakeup-pipe (manager)
  "Read and discard all pending bytes from the wakeup pipe"
  (let ((read-fd (epoll-manager-wakeup-read-fd manager)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count 64))
          (loop
            (let ((n (%read-fd read-fd buf 64)))
              (when (<= n 0) (return)))))
      (error () nil))))

;;; ============================================================================
;;; Manager Lifecycle
;;; ============================================================================

(defun boot-reactor ()
  "Get or create the global epoll manager with reactor thread.
   Thread-safe via double-checked locking."
  (or *epoll-manager*
      (lock:with-lock (*epoll-manager-init-lock*)
        (or *epoll-manager*
            (let* ((epfd (epoll:epoll-create1 epoll:+epoll-cloexec+))
                   (pipe (create-wakeup-pipe))
                   (manager (make-epoll-manager
                             :epfd epfd
                             :wakeup-read-fd (car pipe)
                             :wakeup-write-fd (cdr pipe))))
              ;; Register wakeup pipe read-end with epoll
              (epoll:epoll-ctl epfd epoll:+epoll-ctl-add+ (car pipe)
                               (epoll:make-epoll-event
                                :events epoll:+epollin+
                                :data (car pipe)))
              ;; Start reactor thread
              (setf (epoll-manager-reactor-running manager) t)
              (setf (epoll-manager-reactor-thread manager)
                    (thread:make-thread
                     (lambda () (reactor-loop manager))
                     :name "epoll-reactor"))
              (setf *epoll-manager* manager))))))

(defun shutdown-reactor ()
  "Shutdown the global epoll manager and reactor thread"
  (when *epoll-manager*
    (let ((manager *epoll-manager*))
      (setf *epoll-manager* nil)
      ;; Stop reactor
      (setf (epoll-manager-reactor-running manager) nil)
      (wake-reactor manager)
      (when (and (epoll-manager-reactor-thread manager)
                 (thread:thread-alive-p (epoll-manager-reactor-thread manager)))
        (handler-case
            (thread:join-thread (epoll-manager-reactor-thread manager) :timeout 2)
          (error () nil)))
      ;; Wake all blocked specific-fd waiters so they can return nil
      (let ((all-waiters nil))
        (lock:with-lock ((epoll-manager-waiters-lock manager))
          (maphash (lambda (fd waiters-list)
                     (declare (ignore fd))
                     (dolist (w waiters-list)
                       (push w all-waiters)))
                   (epoll-manager-waiters manager))
          (clrhash (epoll-manager-waiters manager)))
        ;; Signal each waiter via semaphore
        (dolist (w all-waiters)
          (setf (waiter-done w) t)
          (sem:signal-semaphore (waiter-semaphore w))))
      ;; Wake any-queue waiters
      (lock:with-lock ((epoll-manager-any-queue-lock manager))
        (lock:condition-broadcast (epoll-manager-any-queue-cv manager)))
      ;; Unregister all sockets from epoll
      (lock:with-lock ((epoll-manager-lock manager))
        (maphash (lambda (fd info)
                   (declare (ignore info))
                   (handler-case
                       (epoll:epoll-ctl (epoll-manager-epfd manager)
                                        epoll:+epoll-ctl-del+ fd nil)
                     (error () nil)))
                 (epoll-manager-registered-sockets manager))
        (clrhash (epoll-manager-registered-sockets manager)))
      ;; Close epoll fd and pipe fds
      (when (epoll-manager-epfd manager)
        (epoll:epoll-close (epoll-manager-epfd manager))
        (setf (epoll-manager-epfd manager) nil))
      (when (epoll-manager-wakeup-read-fd manager)
        (%close-fd (epoll-manager-wakeup-read-fd manager))
        (setf (epoll-manager-wakeup-read-fd manager) nil))
      (when (epoll-manager-wakeup-write-fd manager)
        (%close-fd (epoll-manager-wakeup-write-fd manager))
        (setf (epoll-manager-wakeup-write-fd manager) nil)))))

(defmacro with-reactor ((&key create-new) &body body)
  "Execute body with an epoll manager, optionally creating a new one"
  (let ((old-manager (gensym)))
    `(let ((,old-manager (when ,create-new *epoll-manager*)))
       (when ,create-new
         (setf *epoll-manager* nil))
       (unwind-protect
            (progn ,@body)
         (when ,create-new
           (shutdown-reactor)
           (setf *epoll-manager* ,old-manager))))))

;;; ============================================================================
;;; Reactor Loop
;;; ============================================================================

(defun reactor-loop (manager)
  "Main reactor event loop - the sole caller of epoll_wait.
   Dispatches events to specific-fd waiters or the any-socket queue.
   Backs off with a short sleep when events go unclaimed (to the any-queue)
   to prevent busy-looping on level-triggered always-ready sockets."
  (loop while (epoll-manager-reactor-running manager) do
    (let ((dispatched-to-waiter nil)
          (dispatched-to-any-queue nil))
      (handler-case
          (let ((events (epoll:wait-for-events (epoll-manager-epfd manager)
                                                16 50)))
            (when events
              (dolist (event events)
                (let ((fd (epoll:epoll-event-data event)))
                  (cond
                    ;; Wakeup pipe event - just drain it
                    ((eql fd (epoll-manager-wakeup-read-fd manager))
                     (drain-wakeup-pipe manager))
                    ;; Regular socket event
                    (t
                     (if (dispatch-event manager fd event)
                         (setf dispatched-to-waiter t)
                         (setf dispatched-to-any-queue t))))))))
        (error (e)
          (warn "Reactor error: ~A" e)
          (sleep 0.01)))
      ;; Back off when level-triggered events go unclaimed to prevent spinning
      (when (and dispatched-to-any-queue (not dispatched-to-waiter))
        (sleep 0.001)))))

(defun dispatch-event (manager fd event)
  "Dispatch an epoll event to the appropriate waiter(s).
   Specific-fd waiters take priority; unmatched events go to the any-queue.
   Returns t if a specific waiter was served, nil otherwise."
  (let ((delivered-waiters nil)
        (event-mask (epoll:epoll-event-events event))
        (error-mask (logior epoll:+epollerr+ epoll:+epollhup+)))
    ;; Try specific-fd waiters first (under waiters-lock)
    (lock:with-lock ((epoll-manager-waiters-lock manager))
      (let ((fd-waiters (gethash fd (epoll-manager-waiters manager))))
        (when fd-waiters
          (let ((remaining nil))
            (dolist (w fd-waiters)
              ;; Deliver if interest matches OR error/hangup occurred
              (if (or (not (zerop (logand event-mask (waiter-interest-mask w))))
                      (not (zerop (logand event-mask error-mask))))
                  (push w delivered-waiters)
                  (push w remaining)))
            (if remaining
                (setf (gethash fd (epoll-manager-waiters manager)) (nreverse remaining))
                (remhash fd (epoll-manager-waiters manager)))))))
    ;; Wake each delivered waiter (outside waiters-lock)
    (when delivered-waiters
      (dolist (w delivered-waiters)
        (setf (waiter-result w) event
              (waiter-done w) t)
        (if (waiter-callback w)
            (handler-case (funcall (waiter-callback w) event)
              (error () nil))
            (sem:signal-semaphore (waiter-semaphore w)))))
    ;; If no specific waiter matched, push to any-queue
    (unless delivered-waiters
      (lock:with-lock ((epoll-manager-any-queue-lock manager))
        (push event (epoll-manager-any-queue manager))
        (lock:condition-broadcast (epoll-manager-any-queue-cv manager))))
    (not (null delivered-waiters))))

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
  (let ((manager (boot-reactor)))
    (lock:with-lock ((epoll-manager-lock manager))
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
  (let ((manager (boot-reactor)))
    (lock:with-lock ((epoll-manager-lock manager))
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
  (let ((manager (boot-reactor)))
    (lock:with-lock ((epoll-manager-lock manager))
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

(defun socket-registered-p (fd)
  "Return non-nil if FD is currently registered with the epoll manager."
  (let ((manager (boot-reactor)))
    (not (null (gethash fd (epoll-manager-registered-sockets manager))))))

;;; ============================================================================
;;; Event Waiting
;;; ============================================================================

(defun wait-for-socket (fd events timeout-ms)
  "Wait for specific events on a socket.
   The reactor thread dispatches events via condition variable.
   Returns the epoll event or nil on timeout."
  (let* ((manager (boot-reactor))
         (interest-mask (compute-epoll-events events))
         (w (make-waiter :interest-mask interest-mask)))
    ;; Ensure socket is registered with epoll for our events of interest
    (lock:with-lock ((epoll-manager-lock manager))
      (let ((info (gethash fd (epoll-manager-registered-sockets manager))))
        (if info
            ;; Already registered - ensure our events are included
            (let ((current-mask (compute-epoll-events (getf info :events))))
              (unless (= (logand current-mask interest-mask) interest-mask)
                (let ((combined-mask (logior current-mask interest-mask)))
                  (let ((combined-mask (if *use-edge-triggered*
                                          (logior combined-mask epoll:+epollet+)
                                          combined-mask)))
                    (handler-case
                        (epoll:epoll-ctl (epoll-manager-epfd manager)
                                        epoll:+epoll-ctl-mod+ fd
                                        (epoll:make-epoll-event
                                         :events combined-mask
                                         :data (or (getf info :data) fd)))
                      (error () nil))))))
            ;; Not registered - register now
            (let ((event-mask (if *use-edge-triggered*
                                 (logior interest-mask epoll:+epollet+)
                                 interest-mask)))
              (handler-case
                  (progn
                    (epoll:epoll-ctl (epoll-manager-epfd manager)
                                    epoll:+epoll-ctl-add+ fd
                                    (epoll:make-epoll-event
                                     :events event-mask
                                     :data fd))
                    (setf (gethash fd (epoll-manager-registered-sockets manager))
                          (list :events events :data nil)))
                (error () nil))))))
    ;; Add waiter under waiters-lock, then wake reactor
    (lock:with-lock ((epoll-manager-waiters-lock manager))
      (push w (gethash fd (epoll-manager-waiters manager))))
    (wake-reactor manager)
    ;; Block on semaphore until reactor delivers an event or timeout expires
    (let ((timeout-s (when (and timeout-ms (>= timeout-ms 0))
                       (/ timeout-ms 1000.0))))
      (sem:wait-on-semaphore (waiter-semaphore w)
                                   :timeout (or timeout-s 30.0)))
    ;; Clean up waiter on timeout (reactor already removed it on success)
    (unless (waiter-done w)
      (lock:with-lock ((epoll-manager-waiters-lock manager))
        (let ((fd-waiters (gethash fd (epoll-manager-waiters manager))))
          (when fd-waiters
            (let ((remaining (remove w fd-waiters :test #'eq)))
              (if remaining
                  (setf (gethash fd (epoll-manager-waiters manager)) remaining)
                  (remhash fd (epoll-manager-waiters manager))))))))
    (waiter-result w)))

(defun register-socket-callback (fd events callback)
  "Register a one-shot callback for when FD is ready for EVENTS.
   CALLBACK receives the epoll event. Called from the reactor thread.
   Does not block the caller. For coroutine I/O integration.

   When the fd is already registered with the manager (e.g. an earlier
   park parked on a different direction), this widens the kernel's
   interest mask via EPOLL_CTL_MOD so it covers EVENTS too.  Without
   that step, a fd first parked on :IN and later parked on :OUT (or
   vice versa) would never receive the readiness wakeup, because the
   kernel only fires events that were in the original mask.  TLS
   handshakes alternate read and write parks on the same fd, so a
   missing widen step manifests as 5s deadline-exceeded errors mid-
   handshake."
  (let* ((manager (boot-reactor))
         (interest-mask (compute-epoll-events events))
         (w (make-waiter :interest-mask interest-mask
                         :callback callback)))
    ;; Ensure fd is registered with epoll, and that the kernel
    ;; interest mask covers `events'.
    (lock:with-lock ((epoll-manager-lock manager))
      (let ((info (gethash fd (epoll-manager-registered-sockets manager))))
        (cond
          ((null info)
           ;; First park on this fd -- ADD to epoll.
           (let ((event-mask (if *use-edge-triggered*
                                 (logior interest-mask epoll:+epollet+)
                                 interest-mask)))
             (handler-case
                 (progn
                   (epoll:epoll-ctl (epoll-manager-epfd manager)
                                    epoll:+epoll-ctl-add+ fd
                                    (epoll:make-epoll-event
                                     :events event-mask :data fd))
                   (setf (gethash fd
                                  (epoll-manager-registered-sockets manager))
                         (list :events events :data nil)))
               (error () nil))))
          (t
           ;; Already registered -- widen mask if our events aren't yet covered.
           (let ((current-mask (compute-epoll-events (getf info :events))))
             (unless (= (logand current-mask interest-mask) interest-mask)
               (let* ((combined (logior current-mask interest-mask))
                      (event-mask (if *use-edge-triggered*
                                      (logior combined epoll:+epollet+)
                                      combined)))
                 (handler-case
                     (progn
                       (epoll:epoll-ctl (epoll-manager-epfd manager)
                                        epoll:+epoll-ctl-mod+ fd
                                        (epoll:make-epoll-event
                                         :events event-mask
                                         :data (or (getf info :data) fd)))
                       ;; Track the widened interest set so the next park
                       ;; on this fd compares against the current mask
                       ;; rather than the original.
                       (setf (getf (gethash fd
                                            (epoll-manager-registered-sockets
                                             manager))
                                   :events)
                             (union events (getf info :events))))
                   (error () nil)))))))))
    ;; Add callback waiter
    (lock:with-lock ((epoll-manager-waiters-lock manager))
      (push w (gethash fd (epoll-manager-waiters manager))))
    (wake-reactor manager)
    w))

(defun wait-for-any-socket (timeout-ms)
  "Wait for events on any registered socket.
   Returns a list of epoll events from the any-queue."
  (let ((manager (boot-reactor)))
    (lock:with-lock ((epoll-manager-any-queue-lock manager))
      ;; If events are already queued, return them immediately
      (when (epoll-manager-any-queue manager)
        (return-from wait-for-any-socket
          (prog1 (nreverse (epoll-manager-any-queue manager))
            (setf (epoll-manager-any-queue manager) nil))))
      ;; Wait for the reactor to deliver events
      (let ((timeout-s (when (and timeout-ms (>= timeout-ms 0))
                         (/ timeout-ms 1000.0))))
        (lock:condition-wait (epoll-manager-any-queue-cv manager)
                                  (epoll-manager-any-queue-lock manager)
                                  :timeout (or timeout-s 1.0)))
      ;; Return whatever accumulated
      (prog1 (nreverse (epoll-manager-any-queue manager))
        (setf (epoll-manager-any-queue manager) nil)))))

(defun poll-sockets ()
  "Poll for events without blocking"
  (let ((manager (boot-reactor)))
    (lock:with-lock ((epoll-manager-any-queue-lock manager))
      (prog1 (nreverse (epoll-manager-any-queue manager))
        (setf (epoll-manager-any-queue manager) nil)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun registered-socket-count ()
  "Return the number of registered sockets"
  (let ((manager (boot-reactor)))
    (hash-table-count (epoll-manager-registered-sockets manager))))

(defun list-registered-sockets ()
  "Return a list of registered socket file descriptors"
  (let ((manager (boot-reactor)))
    (loop for fd being the hash-keys of (epoll-manager-registered-sockets manager)
          collect fd)))
