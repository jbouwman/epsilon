;;;; Reactor-based kqueue Manager for Darwin Networking
;;;;
;;;; A single reactor thread owns the kqueue fd and dispatches events
;;;; to waiting threads via condition variables.  Parallels the Linux
;;;; epoll-manager.  Exports:
;;;;
;;;;   * socket registration: REGISTER-SOCKET, UNREGISTER-SOCKET,
;;;;     MODIFY-SOCKET-EVENTS, SOCKET-REGISTERED-P
;;;;   * event waiting:      WAIT-FOR-SOCKET, WAIT-FOR-ANY-SOCKET,
;;;;                         REGISTER-SOCKET-CALLBACK, POLL-SOCKETS
;;;;   * reactor lifecycle:  BOOT-REACTOR, SHUTDOWN-REACTOR
;;;;
;;;; REGISTER-SOCKET-CALLBACK is the hook that epsilon.scheduler.io-wait
;;;; plugs into *FD-WAIT-REGISTER*, so parked fibers can be woken from
;;;; the reactor thread without the scheduler owning a kqueue itself.

(defpackage epsilon.net.reactor
  (:use cl)
  (:import
   (epsilon.kqueue kqueue)
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
   #:*max-events*))

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defparameter *max-events* 64
  "Maximum number of events to retrieve in a single kevent() call")

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
(defconstant +o-nonblock+ #x0004)

;;; ============================================================================
;;; Waiter Structure
;;; ============================================================================

(defstruct waiter
  "A thread or callback waiting for kqueue events on a specific fd.
   The reactor sets DONE to t and stores the event in RESULT, then either
   invokes CALLBACK (one-shot, for coroutine integration) or signals the
   semaphore to wake a blocked thread."
  (interest-mask 0 :type (unsigned-byte 32))
  (result nil)
  (done nil)
  (semaphore (sem:make-semaphore :name "kq-waiter-sem"))
  (callback nil :type (or null function)))

;;; Interest mask bits (reactor-internal: filter-agnostic event keywords).
;;; Kept compatible with the Linux manager's event keywords (:in :out :err
;;; :hup) so io-wait callers can pass the same list to either backend.
(defconstant +mask-in+  #x0001) ; readable
(defconstant +mask-out+ #x0002) ; writable
(defconstant +mask-err+ #x0004) ; error
(defconstant +mask-hup+ #x0008) ; peer closed / EOF

(defun compute-interest-mask (events-list)
  "Convert a list of event keywords to the manager's internal mask."
  (let ((mask 0))
    (dolist (event events-list)
      (setf mask
            (logior mask
                    (case event
                      (:in     +mask-in+)
                      (:out    +mask-out+)
                      (:err    +mask-err+)
                      (:hup    +mask-hup+)
                      (:rdhup  +mask-hup+)
                      (t 0)))))
    mask))

(defun filter-to-mask (filter flags)
  "Map a kqueue filter + flags to our internal event mask."
  (let ((mask 0))
    (cond
      ((= filter kqueue:+evfilt-read+) (setf mask +mask-in+))
      ((= filter kqueue:+evfilt-write+) (setf mask +mask-out+)))
    (when (not (zerop (logand flags kqueue:+ev-eof+)))
      (setf mask (logior mask +mask-hup+)))
    (when (not (zerop (logand flags kqueue:+ev-error+)))
      (setf mask (logior mask +mask-err+)))
    mask))

;;; ============================================================================
;;; Kqueue Manager Structure
;;; ============================================================================

(defstruct kqueue-manager
  "Manages a single kqueue instance with a dedicated reactor thread.
   The reactor is the sole caller of kevent() and dispatches events
   to per-fd waiters or the catch-all any-queue."
  ;; Core kqueue state
  (kqfd nil :type (or null integer))
  ;; fd -> plist of (:events events-list)
  (registered-sockets (make-hash-table) :type hash-table)
  (lock (lock:make-lock "kq-manager") :type lock:lock)
  ;; Reactor thread
  (reactor-thread nil)
  (reactor-running nil)
  ;; Wakeup pipe for signaling the reactor
  (wakeup-read-fd nil :type (or null integer))
  (wakeup-write-fd nil :type (or null integer))
  ;; Per-fd waiters: fd -> list of waiter structs
  (waiters (make-hash-table) :type hash-table)
  (waiters-lock (lock:make-lock "kq-waiters") :type lock:lock)
  ;; Catch-all event queue for wait-for-any-socket
  (any-queue nil :type list)
  (any-queue-lock (lock:make-lock "kq-any-queue") :type lock:lock)
  (any-queue-cv (lock:make-condition-variable :name "kq-any-queue-cv")))

;;; ============================================================================
;;; Global Manager Instance
;;; ============================================================================

(defvar *kqueue-manager* nil
  "Global kqueue manager instance (shared across all threads)")

(defvar *kqueue-manager-init-lock* (lock:make-lock "kq-manager-init")
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
        (dolist (fd (list read-fd write-fd))
          (let ((flags (%fcntl-mgr fd +f-getfl+ 0)))
            (when (>= flags 0)
              (%fcntl-mgr fd +f-setfl+ (logior flags +o-nonblock+)))))
        (cons read-fd write-fd)))))

(defun wake-reactor (manager)
  "Wake the reactor thread from kevent() by writing to the wakeup pipe."
  (let ((write-fd (kqueue-manager-wakeup-write-fd manager)))
    (when write-fd
      (handler-case
          (lib:with-foreign-memory ((buf :char :count 1))
            (setf (sb-sys:sap-ref-8 buf 0) 1)
            (%write-fd write-fd buf 1))
        (error () nil)))))

(defun drain-wakeup-pipe (manager)
  "Read and discard all pending bytes from the wakeup pipe."
  (let ((read-fd (kqueue-manager-wakeup-read-fd manager)))
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
  "Get or create the global kqueue manager with reactor thread.
   Thread-safe via double-checked locking."
  (or *kqueue-manager*
      (lock:with-lock (*kqueue-manager-init-lock*)
        (or *kqueue-manager*
            (let* ((kqfd (kqueue:kqueue))
                   (pipe (create-wakeup-pipe))
                   (manager (make-kqueue-manager
                             :kqfd kqfd
                             :wakeup-read-fd (car pipe)
                             :wakeup-write-fd (cdr pipe))))
              ;; Register wakeup-pipe read-end with kqueue so the reactor
              ;; wakes from a blocking kevent() when someone calls wake-reactor.
              (kqueue:add-event kqfd (car pipe) kqueue:+evfilt-read+)
              (setf (kqueue-manager-reactor-running manager) t)
              (setf (kqueue-manager-reactor-thread manager)
                    (thread:make-thread
                     (lambda () (reactor-loop manager))
                     :name "kqueue-reactor"))
              (setf *kqueue-manager* manager))))))

(defun shutdown-reactor ()
  "Shutdown the global kqueue manager and reactor thread."
  (when *kqueue-manager*
    (let ((manager *kqueue-manager*))
      (setf *kqueue-manager* nil)
      (setf (kqueue-manager-reactor-running manager) nil)
      (wake-reactor manager)
      (when (and (kqueue-manager-reactor-thread manager)
                 (thread:thread-alive-p (kqueue-manager-reactor-thread manager)))
        (handler-case
            (thread:join-thread (kqueue-manager-reactor-thread manager) :timeout 2)
          (error () nil)))
      ;; Wake all blocked specific-fd waiters so they can return nil
      (let ((all-waiters nil))
        (lock:with-lock ((kqueue-manager-waiters-lock manager))
          (maphash (lambda (fd waiters-list)
                     (declare (ignore fd))
                     (dolist (w waiters-list)
                       (push w all-waiters)))
                   (kqueue-manager-waiters manager))
          (clrhash (kqueue-manager-waiters manager)))
        (dolist (w all-waiters)
          (setf (waiter-done w) t)
          (sem:signal-semaphore (waiter-semaphore w))))
      ;; Wake any-queue waiters
      (lock:with-lock ((kqueue-manager-any-queue-lock manager))
        (lock:condition-broadcast (kqueue-manager-any-queue-cv manager)))
      ;; Unregister all sockets from kqueue (best effort; fd may be closed)
      (lock:with-lock ((kqueue-manager-lock manager))
        (maphash (lambda (fd info)
                   (declare (ignore info))
                   (handler-case
                       (kqueue:remove-event (kqueue-manager-kqfd manager)
                                            fd kqueue:+evfilt-read+)
                     (error () nil))
                   (handler-case
                       (kqueue:remove-event (kqueue-manager-kqfd manager)
                                            fd kqueue:+evfilt-write+)
                     (error () nil)))
                 (kqueue-manager-registered-sockets manager))
        (clrhash (kqueue-manager-registered-sockets manager)))
      ;; Close kqueue fd and pipe fds
      (when (kqueue-manager-kqfd manager)
        (kqueue:kqueue-close (kqueue-manager-kqfd manager))
        (setf (kqueue-manager-kqfd manager) nil))
      (when (kqueue-manager-wakeup-read-fd manager)
        (%close-fd (kqueue-manager-wakeup-read-fd manager))
        (setf (kqueue-manager-wakeup-read-fd manager) nil))
      (when (kqueue-manager-wakeup-write-fd manager)
        (%close-fd (kqueue-manager-wakeup-write-fd manager))
        (setf (kqueue-manager-wakeup-write-fd manager) nil)))))

(defmacro with-reactor ((&key create-new) &body body)
  "Execute body with a kqueue manager, optionally creating a new one."
  (let ((old-manager (gensym)))
    `(let ((,old-manager (when ,create-new *kqueue-manager*)))
       (when ,create-new
         (setf *kqueue-manager* nil))
       (unwind-protect
            (progn ,@body)
         (when ,create-new
           (shutdown-reactor)
           (setf *kqueue-manager* ,old-manager))))))

;;; ============================================================================
;;; Reactor Loop
;;; ============================================================================

(defun reactor-loop (manager)
  "Main reactor event loop -- the sole caller of kevent().
   Dispatches events to specific-fd waiters or the any-socket queue.
   Backs off with a short sleep when events go unclaimed to prevent
   busy-looping on level-triggered always-ready sockets."
  (loop while (kqueue-manager-reactor-running manager) do
    (let ((dispatched-to-waiter nil)
          (dispatched-to-any-queue nil))
      (handler-case
          (let ((events (kqueue:wait-for-events (kqueue-manager-kqfd manager)
                                                *max-events*
                                                0.05))) ; 50ms
            (when events
              (dolist (event events)
                (let ((fd (kqueue:kevent-struct-ident event)))
                  (cond
                    ;; Wakeup-pipe event: just drain
                    ((eql fd (kqueue-manager-wakeup-read-fd manager))
                     (drain-wakeup-pipe manager))
                    (t
                     (if (dispatch-event manager fd event)
                         (setf dispatched-to-waiter t)
                         (setf dispatched-to-any-queue t))))))))
        (error (e)
          (warn "Reactor error: ~A" e)
          (sleep 0.01)))
      (when (and dispatched-to-any-queue (not dispatched-to-waiter))
        (sleep 0.001)))))

(defun dispatch-event (manager fd event)
  "Dispatch a kqueue event to the appropriate waiter(s).
   Specific-fd waiters take priority; unmatched events go to the any-queue.
   Returns t if a specific waiter was served, nil otherwise."
  (let* ((delivered-waiters nil)
         (filter (kqueue:kevent-struct-filter event))
         (flags (kqueue:kevent-struct-flags event))
         (event-mask (filter-to-mask filter flags))
         (error-mask (logior +mask-err+ +mask-hup+)))
    (lock:with-lock ((kqueue-manager-waiters-lock manager))
      (let ((fd-waiters (gethash fd (kqueue-manager-waiters manager))))
        (when fd-waiters
          (let ((remaining nil))
            (dolist (w fd-waiters)
              (if (or (not (zerop (logand event-mask (waiter-interest-mask w))))
                      (not (zerop (logand event-mask error-mask))))
                  (push w delivered-waiters)
                  (push w remaining)))
            (if remaining
                (setf (gethash fd (kqueue-manager-waiters manager))
                      (nreverse remaining))
                (remhash fd (kqueue-manager-waiters manager)))))))
    (when delivered-waiters
      (dolist (w delivered-waiters)
        (setf (waiter-result w) event
              (waiter-done w) t)
        (if (waiter-callback w)
            (handler-case (funcall (waiter-callback w) event)
              (error () nil))
            (sem:signal-semaphore (waiter-semaphore w)))))
    (unless delivered-waiters
      (lock:with-lock ((kqueue-manager-any-queue-lock manager))
        (push event (kqueue-manager-any-queue manager))
        (lock:condition-broadcast (kqueue-manager-any-queue-cv manager))))
    (not (null delivered-waiters))))

;;; ============================================================================
;;; Socket Registration
;;; ============================================================================

(defun %add-filter (kqfd fd filter)
  "Best-effort add of a kqueue filter for FD.  Errors are rethrown with context."
  (handler-case
      (progn (kqueue:add-event kqfd fd filter) t)
    (error (e)
      (error "Failed to add kqueue filter ~A on fd ~D: ~A" filter fd e))))

(defun %remove-filter (kqfd fd filter)
  "Best-effort remove of a kqueue filter for FD; errors are suppressed."
  (handler-case (kqueue:remove-event kqfd fd filter)
    (error () nil)))

(defun %apply-events (kqfd fd events-list)
  "Install the kqueue filters corresponding to EVENTS-LIST for FD.
   :in / :accept map to EVFILT_READ; :out / :connect map to EVFILT_WRITE.
   Error/hup events are implicit with every filter on Darwin (via EV_EOF)."
  (let ((mask (compute-interest-mask events-list)))
    (when (not (zerop (logand mask +mask-in+)))
      (%add-filter kqfd fd kqueue:+evfilt-read+))
    (when (not (zerop (logand mask +mask-out+)))
      (%add-filter kqfd fd kqueue:+evfilt-write+))))

(defun %apply-event-diff (kqfd fd old-events new-events)
  "Update kqueue filters for FD from OLD-EVENTS to NEW-EVENTS."
  (let ((old-mask (compute-interest-mask old-events))
        (new-mask (compute-interest-mask new-events)))
    ;; Adds
    (when (and (not (zerop (logand new-mask +mask-in+)))
               (zerop (logand old-mask +mask-in+)))
      (%add-filter kqfd fd kqueue:+evfilt-read+))
    (when (and (not (zerop (logand new-mask +mask-out+)))
               (zerop (logand old-mask +mask-out+)))
      (%add-filter kqfd fd kqueue:+evfilt-write+))
    ;; Removes
    (when (and (not (zerop (logand old-mask +mask-in+)))
               (zerop (logand new-mask +mask-in+)))
      (%remove-filter kqfd fd kqueue:+evfilt-read+))
    (when (and (not (zerop (logand old-mask +mask-out+)))
               (zerop (logand new-mask +mask-out+)))
      (%remove-filter kqfd fd kqueue:+evfilt-write+))))

(defun register-socket (fd events &key data)
  "Register a socket with the kqueue manager for the given EVENTS.
   EVENTS is a list of :in, :out, :err, :hup (matching the Linux reactor
   API).  DATA is accepted for API parity but unused -- the reactor keys
   on the raw fd."
  (declare (ignore data))
  (let ((manager (boot-reactor)))
    (lock:with-lock ((kqueue-manager-lock manager))
      (when (gethash fd (kqueue-manager-registered-sockets manager))
        (error "Socket ~D is already registered" fd))
      (%apply-events (kqueue-manager-kqfd manager) fd events)
      (setf (gethash fd (kqueue-manager-registered-sockets manager))
            (list :events events))
      t)))

(defun unregister-socket (fd)
  "Unregister a socket from the kqueue manager.
   Best-effort: individual filter removals ignore errors (the fd may
   already have been closed by the kernel, which implicitly drops its
   filters)."
  (let ((manager (boot-reactor)))
    (lock:with-lock ((kqueue-manager-lock manager))
      (when (gethash fd (kqueue-manager-registered-sockets manager))
        (let ((kqfd (kqueue-manager-kqfd manager)))
          (%remove-filter kqfd fd kqueue:+evfilt-read+)
          (%remove-filter kqfd fd kqueue:+evfilt-write+))
        (remhash fd (kqueue-manager-registered-sockets manager))
        t))))

(defun modify-socket-events (fd new-events)
  "Modify the events monitored for a socket."
  (let ((manager (boot-reactor)))
    (lock:with-lock ((kqueue-manager-lock manager))
      (let ((info (gethash fd (kqueue-manager-registered-sockets manager))))
        (unless info
          (error "Socket ~D is not registered" fd))
        (%apply-event-diff (kqueue-manager-kqfd manager) fd
                           (getf info :events) new-events)
        (setf (getf (gethash fd (kqueue-manager-registered-sockets manager))
                    :events)
              new-events)
        t))))

(defun socket-registered-p (fd)
  "Return non-nil if FD is currently registered with the kqueue manager."
  (let ((manager (boot-reactor)))
    (not (null (gethash fd (kqueue-manager-registered-sockets manager))))))

;;; ============================================================================
;;; Event Waiting
;;; ============================================================================

(defun %ensure-fd-registered (manager fd events-list)
  "Ensure FD is registered with kqueue for at least EVENTS-LIST.
   If already registered, add any missing filters.  Silently tolerates
   kqueue errors (fd may have been closed concurrently)."
  (let ((kqfd (kqueue-manager-kqfd manager))
        (interest (compute-interest-mask events-list)))
    (lock:with-lock ((kqueue-manager-lock manager))
      (let ((info (gethash fd (kqueue-manager-registered-sockets manager))))
        (if info
            (let* ((current (compute-interest-mask (getf info :events)))
                   (missing (logandc2 interest current)))
              (when (not (zerop (logand missing +mask-in+)))
                (handler-case (kqueue:add-event kqfd fd kqueue:+evfilt-read+)
                  (error () nil)))
              (when (not (zerop (logand missing +mask-out+)))
                (handler-case (kqueue:add-event kqfd fd kqueue:+evfilt-write+)
                  (error () nil)))
              (when (not (zerop missing))
                (let ((combined (logior current interest))
                      (updated '()))
                  (when (not (zerop (logand combined +mask-in+)))
                    (push :in updated))
                  (when (not (zerop (logand combined +mask-out+)))
                    (push :out updated))
                  (setf (getf (gethash fd (kqueue-manager-registered-sockets
                                          manager))
                              :events)
                        (nreverse updated)))))
            (progn
              (when (not (zerop (logand interest +mask-in+)))
                (handler-case (kqueue:add-event kqfd fd kqueue:+evfilt-read+)
                  (error () nil)))
              (when (not (zerop (logand interest +mask-out+)))
                (handler-case (kqueue:add-event kqfd fd kqueue:+evfilt-write+)
                  (error () nil)))
              (setf (gethash fd (kqueue-manager-registered-sockets manager))
                    (list :events events-list))))))))

(defun wait-for-socket (fd events timeout-ms)
  "Wait for specific events on a socket.
   The reactor thread dispatches events via semaphore.
   Returns the kqueue event or nil on timeout."
  (let* ((manager (boot-reactor))
         (interest-mask (compute-interest-mask events))
         (w (make-waiter :interest-mask interest-mask)))
    (%ensure-fd-registered manager fd events)
    (lock:with-lock ((kqueue-manager-waiters-lock manager))
      (push w (gethash fd (kqueue-manager-waiters manager))))
    (wake-reactor manager)
    (let ((timeout-s (when (and timeout-ms (>= timeout-ms 0))
                       (/ timeout-ms 1000.0))))
      (sem:wait-on-semaphore (waiter-semaphore w)
                             :timeout (or timeout-s 30.0)))
    ;; Clean up waiter on timeout (reactor already removed it on success)
    (unless (waiter-done w)
      (lock:with-lock ((kqueue-manager-waiters-lock manager))
        (let ((fd-waiters (gethash fd (kqueue-manager-waiters manager))))
          (when fd-waiters
            (let ((remaining (remove w fd-waiters :test #'eq)))
              (if remaining
                  (setf (gethash fd (kqueue-manager-waiters manager))
                        remaining)
                  (remhash fd (kqueue-manager-waiters manager))))))))
    (waiter-result w)))

(defun register-socket-callback (fd events callback)
  "Register a one-shot callback for when FD is ready for EVENTS.
   CALLBACK receives the kqueue event and is invoked on the reactor
   thread.  Does not block the caller.  Intended for coroutine I/O
   integration via epsilon.scheduler.io-wait:*fd-wait-register*."
  (let* ((manager (boot-reactor))
         (interest-mask (compute-interest-mask events))
         (w (make-waiter :interest-mask interest-mask
                         :callback callback)))
    (%ensure-fd-registered manager fd events)
    (lock:with-lock ((kqueue-manager-waiters-lock manager))
      (push w (gethash fd (kqueue-manager-waiters manager))))
    (wake-reactor manager)
    w))

(defun wait-for-any-socket (timeout-ms)
  "Wait for events on any registered socket.
   Returns a list of kqueue events from the any-queue."
  (let ((manager (boot-reactor)))
    (lock:with-lock ((kqueue-manager-any-queue-lock manager))
      (when (kqueue-manager-any-queue manager)
        (return-from wait-for-any-socket
          (prog1 (nreverse (kqueue-manager-any-queue manager))
            (setf (kqueue-manager-any-queue manager) nil))))
      (let ((timeout-s (when (and timeout-ms (>= timeout-ms 0))
                         (/ timeout-ms 1000.0))))
        (lock:condition-wait (kqueue-manager-any-queue-cv manager)
                             (kqueue-manager-any-queue-lock manager)
                             :timeout (or timeout-s 1.0)))
      (prog1 (nreverse (kqueue-manager-any-queue manager))
        (setf (kqueue-manager-any-queue manager) nil)))))

(defun poll-sockets ()
  "Poll for events without blocking."
  (let ((manager (boot-reactor)))
    (lock:with-lock ((kqueue-manager-any-queue-lock manager))
      (prog1 (nreverse (kqueue-manager-any-queue manager))
        (setf (kqueue-manager-any-queue manager) nil)))))
