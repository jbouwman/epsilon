;;;; Unified Socket Operations - TCP and UDP
;;;;
;;;; This module provides the main socket operations for TCP and UDP networking.

(defpackage epsilon.net.sockets
  (:use cl)
  (:import
   (epsilon.net.constants const)
   (epsilon.net.errors errors)
   (epsilon.net.types types)
   (epsilon.net.address address)
   (epsilon.net.core core)
   (epsilon.sys.epoll epoll)
   (epsilon.net.reactor reactor)
   (epsilon.foreign lib)
   (epsilon.sys.thread thread))
  (:export
   ;; TCP Listener operations
   #:tcp-bind
   #:tcp-accept
   #:tcp-incoming
   #:tcp-try-accept
   #:tcp-poll-accept
   #:tcp-local-addr

   ;; TCP Stream operations
   #:tcp-connect
   #:tcp-read
   #:tcp-write
   #:tcp-write-all
   #:tcp-flush
   #:tcp-try-read
   #:tcp-try-write
   #:tcp-poll-read
   #:tcp-poll-write
   #:tcp-peer-addr
   #:tcp-shutdown
   #:tcp-close
   #:tcp-stream-reader
   #:tcp-stream-writer
   #:tcp-stream-byte-reader
   #:tcp-stream-byte-writer
   #:tcp-connected-p

   ;; Unix domain socket operations
   #:unix-connect
   #:unix-bind
   #:unix-accept
   #:unix-close
   #:unix-socket-io

   ;; UDP operations
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-poll-send
   #:udp-poll-recv

   ;; Unix domain datagram operations
   #:unix-dgram-bind
   #:unix-dgram-connect
   #:unix-dgram-send
   #:unix-dgram-recv
   #:unix-dgram-close))

;;; ============================================================================
;;; FFI for shutdown pipe
;;; ============================================================================

(lib:defshared %pipe "pipe" "libc" :int
  (pipefd :pointer)
  :documentation "Create pipe")

(lib:defshared %write-fd "write" "libc" :long
  (fd :int) (buf :pointer) (count :unsigned-long)
  :documentation "Write to file descriptor")

(lib:defshared %read-fd "read" "libc" :long
  (fd :int) (buf :pointer) (count :unsigned-long)
  :documentation "Read from file descriptor")

(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #o4000)

(lib:defshared %fcntl "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int)
  :documentation "File control operations")

(defun make-shutdown-pipe ()
  "Create a non-blocking pipe for listener shutdown notification.
   Returns (read-fd . write-fd)."
  (lib:with-foreign-memory ((pipefd :int :count 2))
    (let ((result (%pipe pipefd)))
      (when (< result 0)
        (error "Failed to create shutdown pipe"))
      (let ((read-fd (sb-sys:sap-ref-32 pipefd 0))
            (write-fd (sb-sys:sap-ref-32 pipefd 4)))
        (dolist (fd (list read-fd write-fd))
          (let ((flags (%fcntl fd +f-getfl+ 0)))
            (when (>= flags 0)
              (%fcntl fd +f-setfl+ (logior flags +o-nonblock+)))))
        (cons read-fd write-fd)))))

(defun signal-shutdown-pipe (write-fd)
  "Write a byte to the shutdown pipe to wake any blocked accept."
  (when write-fd
    (handler-case
        (lib:with-foreign-memory ((buf :char :count 1))
          (setf (sb-sys:sap-ref-8 buf 0) 1)
          (%write-fd write-fd buf 1))
      (error () nil))))

;;; ============================================================================
;;; Accept Epoll - private epoll for tcp-accept
;;; ============================================================================

(defun make-accept-epoll (listener-fd shutdown-read-fd)
  "Create a private epoll instance monitoring both the listener socket and
   the shutdown pipe.  Returns the epoll fd."
  (let ((epfd (epoll:epoll-create1 0)))
    (handler-case
        (progn
          ;; Monitor listener for incoming connections
          (epoll:epoll-ctl epfd epoll:+epoll-ctl-add+ listener-fd
                           (epoll:make-epoll-event
                            :events epoll:+epollin+
                            :data (epoll:make-epoll-data :fd listener-fd)))
          ;; Monitor shutdown pipe for wakeup
          (epoll:epoll-ctl epfd epoll:+epoll-ctl-add+ shutdown-read-fd
                           (epoll:make-epoll-event
                            :events epoll:+epollin+
                            :data (epoll:make-epoll-data :fd shutdown-read-fd)))
          epfd)
      (error (e)
        (epoll:epoll-close epfd)
        (error e)))))

(defun accept-epoll-wait (listener timeout-ms)
  "Wait for the listener to have incoming connections or for shutdown.
   Uses the listener's private epoll instance.
   Returns :ready when a connection may be available, :shutdown when the
   shutdown pipe was signaled, or nil on timeout."
  (let* ((epfd (types:tcp-listener-accept-epfd listener))
         (shutdown-fd (types:tcp-listener-shutdown-read-fd listener))
         (events (epoll:epoll-wait epfd 4 timeout-ms)))
    (dolist (event events)
      (let ((fd (epoll:epoll-data-fd (epoll:epoll-event-data event))))
        (when (eql fd shutdown-fd)
          (return-from accept-epoll-wait :shutdown))
        (when (eql fd (types:tcp-listener-handle listener))
          (return-from accept-epoll-wait :ready))))
    ;; No events -- timeout
    nil))

;;; ============================================================================
;;; Socket Event Waiting (Refactored to use Epoll Manager)
;;; ============================================================================

(defun wait-for-socket-ready (socket-fd events-list timeout-ms)
  "Wait for a socket to be ready for specified events using the shared epoll manager.
   Events is a list of keywords like '(:in) or '(:out).
   Returns the event if ready, nil on timeout."
  (let ((event (reactor:wait-for-socket socket-fd events-list timeout-ms)))
    (when event
      ;; Check if any of the requested data events are present.
      ;; EPOLLHUP/EPOLLERR can accompany EPOLLIN/EPOLLOUT (e.g. peer shutdown
      ;; with data still readable); in that case the caller should still attempt
      ;; the I/O operation and handle the result.
      (let ((event-mask (epoll:epoll-event-events event))
            (requested-mask (logior (if (member :in events-list) epoll:+epollin+ 0)
                                    (if (member :out events-list) epoll:+epollout+ 0))))
        (cond
          ;; Requested events are set -- let the caller try I/O
          ((not (zerop (logand event-mask requested-mask)))
           event)
          ;; Only error/hangup with none of the requested events
          ((or (logtest event-mask epoll:+epollerr+)
               (logtest event-mask epoll:+epollhup+))
           nil)
          ;; Unexpected event
          (t event))))))

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address.
   Supports both IPv4 and IPv6 addresses."
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (family (types:socket-address-family sock-addr))
         (af (address:socket-family-constant family))
         (socket-fd (core:create-socket af
                                        const:+sock-stream+
                                        const:+ipproto-tcp+)))

    (when reuse-addr
      (core:set-socket-reuse-addr socket-fd t))

    ;; For IPv6, disable V6ONLY to allow dual-stack (accept both IPv4 and IPv6)
    (when (eq family :ipv6)
      (lib:with-foreign-memory ((optval :int :count 1))
        (setf (sb-sys:sap-ref-32 optval 0) 0)
        (const:%setsockopt socket-fd const:+ipproto-ipv6+ const:+ipv6-v6only+ optval 4)))

    (handler-case
        (progn
          ;; Bind socket to address
          (let ((addr-size (address:sockaddr-size-for-family family)))
            (lib:with-foreign-memory ((sockaddr :char :count addr-size))
              (address:fill-sockaddr-for-address sockaddr sock-addr)
              (let ((result (const:%bind socket-fd sockaddr addr-size)))
                (errors:check-error result "bind"))))

          ;; Listen for connections
          (let ((result (const:%listen socket-fd backlog)))
            (errors:check-error result "listen"))

          ;; Set to non-blocking mode
          (core:set-nonblocking socket-fd)

          ;; Create shutdown pipe and private epoll for accept
          (let* ((pipe (make-shutdown-pipe))
                 (accept-epfd (make-accept-epoll socket-fd (car pipe))))

            ;; Get actual bound address (in case port was 0)
            (let ((actual-addr (core:get-local-address socket-fd)))
              (make-instance 'types:tcp-listener
                             :handle socket-fd
                             :local-address actual-addr
                             :backlog backlog
                             :accept-epfd accept-epfd
                             :shutdown-read-fd (car pipe)
                             :shutdown-write-fd (cdr pipe)))))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a connection.  Blocks until a connection arrives, the listener is
   shut down, or the optional timeout expires.
   Returns a tcp-stream on success, NIL on timeout or shutdown."
  (let ((timeout-ms (if timeout (round (* timeout 1000)) -1)))
    (loop
      (handler-case
          (lib:with-foreign-memory ((peer-sockaddr :char :count 28)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 28)
            (let ((client-fd (const:%accept (types:tcp-listener-handle listener)
                                            peer-sockaddr addrlen)))
              (cond
                ((>= client-fd 0)
                 ;; Success - got a connection
                 (core:set-nonblocking client-fd)
                 (let ((local-addr (core:get-local-address client-fd))
                       (peer-addr (address:parse-sockaddr-by-family peer-sockaddr)))
                   (return (make-instance 'types:tcp-stream
                                          :handle client-fd
                                          :local-address local-addr
                                          :peer-address peer-addr
                                          :connected-p t))))
                (t
                 ;; Check what error occurred
                 (errors:check-error client-fd "accept")))))
        (errors:would-block-error ()
          ;; Wait on private epoll for connection or shutdown
          (let ((result (accept-epoll-wait listener timeout-ms)))
            (case result
              (:shutdown (return nil))
              (:ready nil)  ; retry accept
              ((nil) (return nil)))))  ; timeout
        (error (e)
          (error 'errors:network-error
                 :message (format nil "Accept failed: ~A" e)))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (lib:with-foreign-memory ((peer-sockaddr :char :count 16)
                                (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) 16)
        (let ((client-fd (const:%accept (types:tcp-listener-handle listener)
                                        peer-sockaddr addrlen)))
          (when (>= client-fd 0)
            (core:set-nonblocking client-fd)
            (let ((local-addr (core:get-local-address client-fd))
                  (peer-addr (address:parse-sockaddr-in peer-sockaddr)))
              (make-instance 'types:tcp-stream
                             :handle client-fd
                             :local-address local-addr
                             :peer-address peer-addr
                             :connected-p t)))))
    (errors:would-block-error ()
      nil)
    (error (e)
      (error 'errors:network-error
             :message (format nil "Accept failed: ~A" e)))))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (when (eq :ready (accept-epoll-wait listener timeout-ms))
    (tcp-try-accept listener)))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (types:tcp-listener-local-address listener))

(defun tcp-incoming (listener)
  "Return a list of incoming connections"
  ;; This would ideally return a lazy sequence, but for now returns a list
  (loop for conn = (tcp-poll-accept listener 0)
        while conn
        collect conn))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server with optional timeout.
   ADDRESS can be a socket-address or string like 'host:port'.
   Resolves hostnames via DNS when the IP field is not a numeric address.
   Supports both IPv4 and IPv6 addresses.
   Signals network-error with detailed message on failure."
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         ;; Resolve hostname to numeric IP if needed
         (sock-addr (let ((ip (types:socket-address-ip sock-addr))
                          (detected (address:detect-address-family
                                     (types:socket-address-ip sock-addr))))
                      (if (member detected '(:ipv4 :ipv6))
                          sock-addr
                          (let ((resolved (address:resolve-address
                                           ip (types:socket-address-port sock-addr))))
                            (unless resolved
                              (error 'errors:network-error
                                     :message (format nil "DNS resolution failed for ~A" ip)))
                            (first resolved)))))
         (family (types:socket-address-family sock-addr))
         (af (address:socket-family-constant family))
         (target-desc (format nil "~A:~D"
                              (types:socket-address-ip sock-addr)
                              (types:socket-address-port sock-addr)))
         (socket-fd (core:create-socket af
                                        const:+sock-stream+
                                        const:+ipproto-tcp+)))

    (handler-case
        (progn
          ;; Set to non-blocking mode before connecting for async support
          (core:set-nonblocking socket-fd)

          ;; Try to connect
          (let ((addr-size (address:sockaddr-size-for-family family)))
           (lib:with-foreign-memory ((sockaddr :char :count addr-size))
            (address:fill-sockaddr-for-address sockaddr sock-addr)
            (let ((result (const:%connect socket-fd sockaddr addr-size)))
              (when (< result 0)
                ;; Check if it's a would-block error (async connection in progress)
                (let ((errno (errors:get-errno)))
                  (if (= errno const:+einprogress+)
                      ;; Wait for the async connect to complete
                      (let ((timeout-ms (if timeout
                                            (round (* timeout 1000))
                                            30000)))  ; default 30s
                        (reactor:register-socket socket-fd '(:out))
                        (unwind-protect
                            (let ((event (reactor:wait-for-socket
                                         socket-fd '(:out) timeout-ms)))
                              (unless event
                                (error 'errors:timeout-error
                                       :message (format nil "Connection to ~A timed out after ~A seconds"
                                                        target-desc (or timeout 30))))
                              ;; Check SO_ERROR -- async connect may have failed
                              (let ((so-error (core:get-socket-option
                                               socket-fd :error)))
                                (unless (zerop so-error)
                                  (let ((condition-class (errors:errno-to-condition so-error)))
                                    (error condition-class
                                           :message (format nil "connect to ~A failed: ~A"
                                                            target-desc
                                                            (errors:errno-to-string so-error)))))))
                          (reactor:unregister-socket socket-fd)))
                      ;; Other error - provide detailed message with target address
                      (let* ((errno-str (errors:errno-to-string errno))
                             (condition-class (errors:errno-to-condition errno)))
                        (error condition-class
                               :message (format nil "connect to ~A failed: ~A"
                                                target-desc errno-str)))))))))

          ;; Get local and peer addresses
          (let ((local-addr (core:get-local-address socket-fd))
                (peer-addr (core:get-peer-address socket-fd)))
            (make-instance 'types:tcp-stream
                           :handle socket-fd
                           :local-address local-addr
                           :peer-address peer-addr
                           :connected-p t)))
      (errors:network-error (e)
        ;; Network errors already have good messages, just close and re-raise
        (const:%close socket-fd)
        (error e))
      (error (e)
        ;; Wrap unexpected errors with context
        (const:%close socket-fd)
        (error 'errors:network-error
               :message (format nil "TCP connect to ~A failed: ~A" target-desc e))))))

(defun tcp-stream-reader (stream)
  "Get or create input stream for TCP stream"
  (or (types:tcp-stream-input stream)
      (setf (types:tcp-stream-input stream)
            (core:socket-to-stream (types:tcp-stream-handle stream) :input))))

(defun tcp-stream-writer (stream)
  "Get or create output stream for TCP stream"
  (or (types:tcp-stream-output stream)
      (setf (types:tcp-stream-output stream)
            (core:socket-to-stream (types:tcp-stream-handle stream) :output))))

(defun tcp-stream-byte-reader (stream)
  "Get a Lisp byte input stream for reading.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses no buffering to return data as soon as available."
  (or (types:tcp-stream-byte-input stream)
      (setf (types:tcp-stream-byte-input stream)
            (sb-sys:make-fd-stream (types:tcp-stream-handle stream)
                                   :input t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :none))))

(defun tcp-stream-byte-writer (stream)
  "Get a Lisp byte output stream for writing.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses :full buffering - caller must call force-output/finish-output."
  (or (types:tcp-stream-byte-output stream)
      (setf (types:tcp-stream-byte-output stream)
            (sb-sys:make-fd-stream (types:tcp-stream-handle stream)
                                   :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :full))))

(defun tcp-read (stream buffer &key (start 0) (end nil) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (let ((actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      (let ((result (loop
                      ;; recv() can return -1 with errno=EINTR when an
                      ;; in-process signal (SBCL GC, sb-ext:with-timeout
                      ;; tick, etc.) interrupts the syscall before any
                      ;; bytes were transferred. Per POSIX, the read
                      ;; should be restarted; without this loop CI
                      ;; surfaces the EINTR as `Network error: recv:
                      ;; Unknown error (4)' on otherwise-healthy
                      ;; connections (test-tls-server-accepts-https-
                      ;; request was the canonical victim).
                      for r = (const:%recv (types:tcp-stream-handle stream)
                                           buf (- actual-end start) 0)
                      until (or (>= r 0)
                                (/= (errors:get-errno) 4)) ; 4 = EINTR
                      finally (return r))))
        (cond
          ((> result 0)
           ;; Copy data from foreign buffer to Lisp buffer
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           result)
          ((= result 0)
           ;; Connection closed
           (setf (types:tcp-stream-connected-p stream) nil)
           0)
          (t
           ;; Error occurred
           (let ((errno (errors:get-errno)))
             (cond
               ;; Interrupted by signal before any data transferred -- retry.
               ;; SBCL's GC and signal-driven thread interruption routinely
               ;; hit blocking syscalls on a busy host, so a stray EINTR is
               ;; not a real network error.
               ((= errno const:+eintr+)
                (tcp-read stream buffer :start start :end end :timeout timeout))
               ((= errno const:+eagain+)
                (if timeout
                    ;; Wait for data with timeout
                    (let ((timeout-ms (round (* timeout 1000))))
                      (if (wait-for-socket-ready
                           (types:tcp-stream-handle stream)
                           '(:in)
                           timeout-ms)
                          ;; Retry read after epoll indicates readiness
                          (tcp-read stream buffer :start start :end end :timeout nil)
                          0)) ; Timeout occurred
                    0)) ; No data available, no timeout specified
               (t
                (setf (types:tcp-stream-connected-p stream) nil)
                (errors:check-error result "recv")
                0)))))))))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream with optional timeout"
  (let* ((buffer (etypecase data
                   (string (sb-ext:string-to-octets data))
                   (vector data)
                   (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      ;; Copy data to foreign buffer
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))

      (let ((result (const:%send (types:tcp-stream-handle stream)
                                 buf (- actual-end start) 0)))
        (cond
          ((>= result 0) result)
          (t
           (let ((errno (errors:get-errno)))
             (cond
               ;; See tcp-read: signal-driven EINTR is not a network error.
               ((= errno const:+eintr+)
                (tcp-write stream data :start start :end end :timeout timeout))
               ((= errno const:+eagain+)
                ;; Wait for socket to be writable, then retry
                (let ((timeout-ms (if timeout (round (* timeout 1000)) 30000)))
                  (if (wait-for-socket-ready
                       (types:tcp-stream-handle stream)
                       '(:out)
                       timeout-ms)
                      ;; Retry write after epoll indicates writability
                      (tcp-write stream data :start start :end end :timeout timeout)
                      0))) ; Timeout occurred
               (t
                (setf (types:tcp-stream-connected-p stream) nil)
                (errors:check-error result "send")
                0)))))))))

(defun tcp-write-all (stream data &key (start 0) (end nil))
  "Write all data to TCP stream"
  (let ((actual-end (or end (length data))))
    (loop with pos = start
          while (< pos actual-end)
          for bytes-written = (tcp-write stream data :start pos :end actual-end)
          do (incf pos bytes-written)
             (when (zerop bytes-written)
               ;; If no bytes were written, avoid infinite loop
               (sleep 0.001))
          finally (return (- actual-end start)))))

(defun tcp-flush (stream)
  "Flush any buffered output"
  ;; With direct socket operations, flushing is automatic
  (declare (ignore stream))
  t)

(defun tcp-try-read (stream buffer &key (start 0) (end nil))
  "Try to read without blocking"
  ;; Non-blocking read
  (tcp-read stream buffer :start start :end end))

(defun tcp-try-write (stream data &key (start 0) (end nil))
  "Try to write without blocking"
  ;; Non-blocking write
  (tcp-write stream data :start start :end end))

(defun tcp-poll-read (stream waker)
  "Poll for read readiness. Returns :ready if data is available,
   or :pending after registering WAKER for async notification."
  (let ((fd (types:tcp-stream-handle stream)))
    (core:set-nonblocking fd)
    (lib:with-foreign-memory ((buf :char :count 1))
      ;; MSG_PEEK (0x2) checks for data without consuming it
      (let ((result (const:%recv fd buf 1 2)))
        (cond
          ((> result 0) :ready)
          ((= result 0) :ready)  ;; EOF is also "ready" - reader will see 0 bytes
          (t
           (let ((errno (errors:get-errno)))
             (if (= errno const:+eagain+)
                 ;; No data yet - spawn a thread to wait and call waker
                 (progn
                   (thread:make-thread
                    (lambda ()
                      (wait-for-socket-ready fd '(:in) 5000)
                      (when waker (funcall waker)))
                    :name "tcp-poll-read-waker")
                   :pending)
                 ;; Some other error - treat as ready so the reader
                 ;; gets the error on the actual read call
                 :ready))))))))

(defun tcp-poll-write (stream waker)
  "Poll for write readiness. Returns :ready if writable,
   or :pending after registering WAKER for async notification."
  (let ((fd (types:tcp-stream-handle stream)))
    (if (wait-for-socket-ready fd '(:out) 0)
        :ready
        (progn
          (thread:make-thread
           (lambda ()
             (wait-for-socket-ready fd '(:out) 5000)
             (when waker (funcall waker)))
           :name "tcp-poll-write-waker")
          :pending))))

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (types:tcp-stream-peer-address stream))

(defun tcp-shutdown (socket-or-listener &key (how :both))
  "Shutdown a TCP stream or a TCP listener.

   For a tcp-stream, calls shutdown(2) with the direction selected
   by HOW and unregisters the fd from the epoll manager. Signals
   an error if shutdown(2) fails.

   For a tcp-listener, wakes any blocked tcp-accept calls via the
   listener's shutdown pipe and best-effort-shuts-down the kernel
   accept queue. HOW is accepted for symmetry with the stream
   case; any value means \"stop accepting\". Errors on the listener
   handle are swallowed because shutting down a listener is a
   cleanup path.

   Does not close the underlying file descriptor -- use tcp-close
   for that."
  (let ((shutdown-how (ecase how
                        (:read const:+shut-rd+)
                        (:write const:+shut-wr+)
                        (:both const:+shut-rdwr+))))
    (etypecase socket-or-listener
      (types:tcp-listener
       ;; Wake any blocked tcp-accept via the shutdown pipe first,
       ;; then ask the kernel to stop accepting new connections.
       ;; Both steps are best-effort: a listener that's already been
       ;; closed or shut down is a normal cleanup state.
       (signal-shutdown-pipe
        (types:tcp-listener-shutdown-write-fd socket-or-listener))
       (let ((handle (types:tcp-listener-handle socket-or-listener)))
         (when (and handle (>= handle 0))
           (handler-case
               (const:%shutdown handle shutdown-how)
             (error () nil)))))
      (types:tcp-stream
       (let ((handle (types:tcp-stream-handle socket-or-listener)))
         (let ((result (const:%shutdown handle shutdown-how)))
           (errors:check-error result "shutdown"))
         ;; Unregister from epoll manager
         (handler-case
             (reactor:unregister-socket handle)
           (error ()
             ;; Ignore errors - socket might not be registered
             nil)))
       (setf (types:tcp-stream-connected-p socket-or-listener) nil)))))

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (types:tcp-stream-connected-p stream))

(defun tcp-close (socket-or-listener)
  "Close a TCP listener or stream.
   Works with both tcp-listener and tcp-stream objects."
  (etypecase socket-or-listener
    (types:tcp-listener
     ;; Signal any blocked tcp-accept to wake up, then close the listener.
     ;; The private epoll fd and shutdown pipe fds are intentionally NOT
     ;; closed here -- the accept thread may still be inside epoll_wait
     ;; when this runs.  These lightweight fds will be cleaned up when
     ;; the listener is GC'd or the process exits.
     (signal-shutdown-pipe (types:tcp-listener-shutdown-write-fd socket-or-listener))
     (let ((handle (types:tcp-listener-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (core:close-socket handle))))
    (types:tcp-stream
     ;; Shutdown and close the stream's socket handle
     (let ((handle (types:tcp-stream-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (handler-case
             (const:%shutdown handle const:+shut-rdwr+)
           (error () nil))
         ;; Unregister from epoll manager
         (handler-case
             (reactor:unregister-socket handle)
           (error () nil))
         (core:close-socket handle)
         (setf (types:tcp-stream-connected-p socket-or-listener) nil))))))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address. Supports IPv4 and IPv6."
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (family (types:socket-address-family sock-addr))
         (af (address:socket-family-constant family))
         (socket-fd (core:create-socket af
                                        const:+sock-dgram+
                                        const:+ipproto-udp+)))

    (handler-case
        (progn
          (core:set-socket-reuse-addr socket-fd t)

          ;; Bind socket to address
          (let ((addr-size (address:sockaddr-size-for-family family)))
            (lib:with-foreign-memory ((sockaddr :char :count addr-size))
              (address:fill-sockaddr-for-address sockaddr sock-addr)
              (let ((result (const:%bind socket-fd sockaddr addr-size)))
                (errors:check-error result "UDP bind"))))

          (core:set-nonblocking socket-fd)

          ;; Get actual bound address (in case port was 0)
          (let ((actual-addr (core:get-local-address socket-fd)))
            (make-instance 'types:udp-socket
                           :handle socket-fd
                           :local-address actual-addr)))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address. Supports IPv4 and IPv6."
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (family (types:socket-address-family sock-addr))
         (addr-size (address:sockaddr-size-for-family family)))
    (handler-case
        (lib:with-foreign-memory ((sockaddr :char :count addr-size))
          (address:fill-sockaddr-for-address sockaddr sock-addr)
          (let ((result (const:%connect (types:udp-socket-handle socket) sockaddr addr-size)))
            (errors:check-error result "UDP connect")
            (setf (types:udp-socket-connected-peer socket) sock-addr)))
      (error (e)
        (error e)))))

(defun udp-send (socket data &key address (start 0) (end nil))
  "Send data on UDP socket.  When ADDRESS is nil, use the connected peer."
  (if address
      (udp-send-to socket data address :start start :end end)
      (let* ((buffer (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)
                       (list (coerce data 'vector))))
             (actual-end (or end (length buffer))))
        (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
          (loop for i from start below actual-end
                for j from 0
                do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
          (let ((result (const:%send (types:udp-socket-handle socket)
                                     buf (- actual-end start) 0)))
            (errors:check-error result "UDP send")
            result)))))

(defun udp-recv (socket buffer &key (start 0) (end nil))
  "Receive data on UDP socket"
  (let ((actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      (let ((result (const:%recv (types:udp-socket-handle socket)
                                 buf (- actual-end start) 0)))
        (cond
          ((> result 0)
           ;; Copy data from foreign buffer to Lisp buffer
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           result)
          ((= result 0)
           0) ; No data
          (t
           (let ((errno (errors:get-errno)))
             (if (= errno const:+eagain+)
                 0
                 (errors:check-error result "UDP recv")))
           0))))))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address. Supports IPv4 and IPv6."
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (family (types:socket-address-family sock-addr))
         (addr-size (address:sockaddr-size-for-family family))
         (buffer (etypecase data
                   (string (sb-ext:string-to-octets data))
                   (vector data)
                   (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start))
                              (sockaddr :char :count addr-size))
      ;; Copy data to foreign buffer
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))

      ;; Create destination address
      (address:fill-sockaddr-for-address sockaddr sock-addr)

      (let ((result (const:%sendto (types:udp-socket-handle socket)
                                   buf (- actual-end start) 0
                                   sockaddr addr-size)))
        (errors:check-error result "UDP sendto")
        result))))

(defun udp-recv-from (socket buffer &key (start 0) (end nil))
  "Receive data and sender address. Handles both IPv4 and IPv6 senders."
  (let ((actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start))
                              (sockaddr :char :count 28)
                              (addrlen :int :count 1))
      (setf (sb-sys:sap-ref-32 addrlen 0) 28)
      (let ((result (const:%recvfrom (types:udp-socket-handle socket)
                                     buf (- actual-end start) 0
                                     sockaddr addrlen)))
        (cond
          ((> result 0)
           ;; Copy data from foreign buffer to Lisp buffer
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           (values result (address:parse-sockaddr-by-family sockaddr)))
          ((= result 0)
           (values 0 nil))
          (t
           (let ((errno (errors:get-errno)))
             (if (= errno const:+eagain+)
                 (values 0 nil)
                 (progn
                   (errors:check-error result "UDP recvfrom")
                   (values 0 nil))))))))))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (types:udp-socket-local-address socket))

(defun udp-poll-send (socket timeout-ms)
  "Poll for send readiness with timeout"
  (wait-for-socket-ready (types:udp-socket-handle socket) '(:out) timeout-ms))

(defun udp-poll-recv (socket timeout-ms)
  "Poll for receive readiness with timeout"
  (wait-for-socket-ready (types:udp-socket-handle socket) '(:in) timeout-ms))

;;; ============================================================================
;;; Unix Domain Socket Operations
;;; ============================================================================

(defun unix-connect (path)
  "Connect to a Unix domain socket at PATH. Returns a unix-socket-stream."
  (let ((socket-fd (core:create-socket const:+af-unix+
                                        const:+sock-stream+
                                        0)))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count const:+sockaddr-un-size+))
            (let ((addr-len (address:make-sockaddr-un-into addr path)))
              (let ((result (const:%connect socket-fd addr addr-len)))
                (errors:check-error result "unix connect"))))
          (make-instance 'types:unix-socket-stream
                         :handle socket-fd
                         :connected-p t))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun unix-bind (path &key (backlog 128))
  "Create a Unix domain socket listener bound to PATH.
   Removes any stale socket file at PATH before binding.
   Returns a unix-socket-listener."
  (let ((socket-fd (core:create-socket const:+af-unix+
                                        const:+sock-stream+
                                        0)))
    (handler-case
        (progn
          ;; Remove stale socket file if present
          (handler-case (const:%unlink path)
            (error () nil))

          (lib:with-foreign-memory ((addr :char :count const:+sockaddr-un-size+))
            (let ((addr-len (address:make-sockaddr-un-into addr path)))
              (let ((result (const:%bind socket-fd addr addr-len)))
                (errors:check-error result "unix bind"))))

          (let ((result (const:%listen socket-fd backlog)))
            (errors:check-error result "unix listen"))

          (make-instance 'types:unix-socket-listener
                         :handle socket-fd
                         :path path))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun unix-accept (listener)
  "Accept a connection on a Unix domain socket listener.
   Returns a unix-socket-stream."
  (let ((client-fd (const:%accept (types:unix-socket-listener-handle listener)
                                   (sb-sys:int-sap 0)
                                   (sb-sys:int-sap 0))))
    (when (< client-fd 0)
      (errors:check-error client-fd "unix accept"))
    (make-instance 'types:unix-socket-stream
                   :handle client-fd
                   :connected-p t)))

(defun unix-socket-io (unix-stream)
  "Get or create a bidirectional (unsigned-byte 8) stream from a unix-socket-stream.
   The returned stream is suitable for binary protocol I/O."
  (or (types:unix-socket-stream-io-stream unix-stream)
      (setf (types:unix-socket-stream-io-stream unix-stream)
            (sb-sys:make-fd-stream (types:unix-socket-stream-handle unix-stream)
                                   :input t :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :full
                                   :auto-close nil))))

(defun unix-close (socket-or-listener)
  "Close a Unix domain socket stream or listener.
   For listeners, also removes the socket file.
   Safe to call multiple times."
  (etypecase socket-or-listener
    (types:unix-socket-listener
     (let ((handle (types:unix-socket-listener-handle socket-or-listener)))
       (when (and handle (integerp handle) (>= handle 0))
         (setf (types:unix-socket-listener-handle socket-or-listener) nil)
         (const:%close handle)
         ;; Remove the socket file
         (handler-case
             (const:%unlink (types:unix-socket-listener-path socket-or-listener))
           (error () nil)))))
    (types:unix-socket-stream
     (let ((handle (types:unix-socket-stream-handle socket-or-listener)))
       (when (and handle (integerp handle) (>= handle 0))
         (setf (types:unix-socket-stream-handle socket-or-listener) nil)
         ;; Close the io-stream first if it exists
         (when (types:unix-socket-stream-io-stream socket-or-listener)
           (handler-case
               (close (types:unix-socket-stream-io-stream socket-or-listener))
             (error () nil))
           (setf (types:unix-socket-stream-io-stream socket-or-listener) nil))
         (handler-case (const:%close handle)
           (error () nil))
         (setf (types:unix-socket-stream-connected-p socket-or-listener) nil))))))

;;; ============================================================================
;;; Unix Domain Datagram Socket Operations
;;; ============================================================================

(defun unix-dgram-bind (path)
  "Create a Unix datagram socket bound to PATH.
Removes any stale socket file at PATH before binding.
Returns a unix-dgram-socket."
  (let ((socket-fd (core:create-socket const:+af-unix+
                                        const:+sock-dgram+
                                        0)))
    (handler-case
        (progn
          ;; Remove stale socket file
          (handler-case (const:%unlink path)
            (error () nil))
          (lib:with-foreign-memory ((addr :char :count const:+sockaddr-un-size+))
            (let ((addr-len (address:make-sockaddr-un-into addr path)))
              (let ((result (const:%bind socket-fd addr addr-len)))
                (errors:check-error result "unix dgram bind"))))
          (make-instance 'types:unix-dgram-socket
                         :handle socket-fd
                         :path path))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun unix-dgram-connect (path)
  "Create a Unix datagram socket connected to PATH.
The socket can send with unix-dgram-send without specifying a destination.
Returns a unix-dgram-socket."
  (let ((socket-fd (core:create-socket const:+af-unix+
                                        const:+sock-dgram+
                                        0)))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count const:+sockaddr-un-size+))
            (let ((addr-len (address:make-sockaddr-un-into addr path)))
              (let ((result (const:%connect socket-fd addr addr-len)))
                (errors:check-error result "unix dgram connect"))))
          (make-instance 'types:unix-dgram-socket
                         :handle socket-fd))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun unix-dgram-send (socket data &key (start 0) (end nil))
  "Send DATA on a connected Unix datagram socket.
DATA is a (vector (unsigned-byte 8)).  Returns bytes sent."
  (let* ((actual-end (or end (length data)))
         (len (- actual-end start)))
    (lib:with-foreign-memory ((buf :char :count len))
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref data i)))
      (let ((result (const:%send (types:unix-dgram-socket-handle socket)
                                 buf len 0)))
        (errors:check-error result "unix dgram send")
        result))))

(defun unix-dgram-recv (socket buffer &key (start 0) (end nil))
  "Receive a datagram into BUFFER on a bound Unix datagram socket.
BUFFER is a (vector (unsigned-byte 8)).
Returns the number of bytes received, or 0 on empty read."
  (let* ((actual-end (or end (length buffer)))
         (len (- actual-end start)))
    (lib:with-foreign-memory ((buf :char :count len))
      (let ((result (const:%recv (types:unix-dgram-socket-handle socket)
                                 buf len 0)))
        (cond
          ((> result 0)
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           result)
          ((= result 0) 0)
          (t (errors:check-error result "unix dgram recv") 0))))))

(defun unix-dgram-close (socket)
  "Close a Unix datagram socket. Removes the socket file if bound."
  (let ((handle (types:unix-dgram-socket-handle socket)))
    (when (and handle (integerp handle) (>= handle 0))
      (setf (types:unix-dgram-socket-handle socket) nil)
      (const:%close handle)
      (when (types:unix-dgram-socket-path socket)
        (handler-case (const:%unlink (types:unix-dgram-socket-path socket))
          (error () nil))))))
