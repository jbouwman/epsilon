;;;; Unified socket operations - TCP and UDP
(defpackage epsilon.net.sockets
  (:use cl)
  (:import (epsilon.kqueue kqueue) (epsilon.foreign lib) (epsilon.log log))
  (:import-from epsilon.net.constants
                +af-inet+
                +af-inet6+
                +af-unix+
                +sock-stream+
                +ipproto-tcp+
                +sock-dgram+
                +ipproto-udp+
                +sol-socket+
                +so-reuseaddr+
                +shut-rd+
                +shut-wr+
                +shut-rdwr+
                +sockaddr-in6-size+
                +sockaddr-un-size+
                %socket
                %bind
                %listen
                %accept
                %connect
                %send
                %recv
                %close
                %shutdown
                %setsockopt
                %getsockname
                %getpeername
                %sendto
                %recvfrom
                %unlink
                %fcntl)
  (:import-from epsilon.net.core
                socket-address
                socket-address-ip
                socket-address-port
                socket-address-family
                tcp-listener
                tcp-listener-handle
                tcp-listener-local-address
                tcp-listener-kqueue
                tcp-listener-shutdown-read-fd
                tcp-listener-shutdown-write-fd
                tcp-stream
                tcp-stream-handle
                tcp-stream-local-address
                tcp-stream-peer-address
                tcp-stream-input
                tcp-stream-output
                tcp-stream-byte-input
                tcp-stream-byte-output
                tcp-stream-connected-p
                udp-socket
                udp-socket-handle
                udp-socket-local-address
                udp-socket-connected-peer
                unix-socket-address
                unix-socket-stream
                unix-socket-stream-handle
                unix-socket-stream-connected-p
                unix-socket-stream-io-stream
                unix-socket-listener
                unix-socket-listener-handle
                unix-socket-listener-path
                unix-dgram-socket
                unix-dgram-socket-handle
                unix-dgram-socket-path
                network-error
                connection-refused
                connection-reset
                address-in-use
                would-block-error
                get-errno
                errno-to-string)
  (:import-from epsilon.net.address
                normalize-address
                make-sockaddr-in-into
                parse-sockaddr-in
                parse-sockaddr-in6
                make-socket-address
                fill-sockaddr-for-address
                sockaddr-size-for-family
                socket-family-constant
                make-sockaddr-un-into)
  (:import-from epsilon.async set-nonblocking submit-async-operation make-async-operation)
  (:export
    ;; Common socket utilities
    create-socket
    bind-socket
    close-socket
    get-socket-address
    set-socket-reuse-address
    ;; TCP Listener operations
    tcp-bind
    tcp-accept
    tcp-incoming
    tcp-try-accept
    tcp-poll-accept
    tcp-local-addr
    tcp-close
    ;; TCP Stream operations
    tcp-connect
    tcp-read
    tcp-write
    tcp-write-all
    tcp-flush
    tcp-try-read
    tcp-try-write
    tcp-poll-read
    tcp-poll-write
    tcp-peer-addr
    tcp-shutdown
    tcp-stream-reader
    tcp-stream-writer
    tcp-stream-byte-reader
    tcp-stream-byte-writer
    tcp-connected-p
    ;; Unix domain socket operations
    unix-connect
    unix-bind
    unix-accept
    unix-close
    unix-socket-io
    ;; Unix datagram operations
    unix-dgram-bind
    unix-dgram-connect
    unix-dgram-send
    unix-dgram-recv
    unix-dgram-close
    ;; UDP operations
    udp-bind
    udp-connect
    udp-send
    udp-recv
    udp-send-to
    udp-recv-from
    udp-local-addr
    udp-try-send
    udp-try-recv
    udp-poll-send
    udp-poll-recv))

;;; ============================================================================
;;; FFI for shutdown pipe
;;; ============================================================================
(lib:defshared %pipe
  "pipe"
  "libc"
  :int
  (pipefd :pointer)
  :documentation
  "Create pipe")

(lib:defshared %write-fd
  "write"
  "libc"
  :long
  (fd :int)
  (buf :pointer)
  (count :unsigned-long)
  :documentation
  "Write to file descriptor")

(defconstant +f-getfl+
  3)
(defconstant +f-setfl+
  4)
(defconstant +o-nonblock+
  #o4)
; Darwin O_NONBLOCK
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
    (handler-case (lib:with-foreign-memory ((buf :char :count 1))
      (setf (sb-sys:sap-ref-8 buf 0) 1)
      (%write-fd write-fd buf 1))
      (error
       ()
       nil))))

;;; ============================================================================
;;; Common Socket Utilities
;;; ============================================================================
(defun create-socket (family type protocol)
  "Create a socket with specified family, type, and protocol"
  (let ((socket-fd (%socket family type protocol)))
    (when (< socket-fd 0)
      (error 'network-error
             :message (format nil "Failed to create socket: ~A" (errno-to-string (get-errno)))))
    socket-fd))

(defun bind-socket (socket-fd address)
  "Bind a socket to an address"
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (addr-size (sockaddr-size-for-family family)))
    (lib:with-foreign-memory ((addr :char :count addr-size))
      (fill-sockaddr-for-address addr sock-addr)
      (when (< (%bind socket-fd addr addr-size) 0)
        (error 'address-in-use
               :message (format nil
                                "Failed to bind socket to ~A:~D"
                                (socket-address-ip sock-addr)
                                (socket-address-port sock-addr)))))))

(defun close-socket (socket-fd)
  "Close a socket"
  (when (>= socket-fd 0)
    (%close socket-fd)))

(defun get-socket-address (socket-fd type)
  "Get local or peer address of socket"
  ;; Use max size for IPv6 to handle both address families
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+) (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
    (ecase type
      (:local
       (%getsockname socket-fd addr addrlen))
      (:peer
       (%getpeername socket-fd addr addrlen)))
    ;; Check sa_family to determine address type (offset 1 on Darwin)
    (let ((family (sb-sys:sap-ref-8 addr 1)))
      (cond
        ((= family +af-inet+)
         (parse-sockaddr-in addr))
        ((= family +af-inet6+)
         (parse-sockaddr-in6 addr))
        (t
         (error 'network-error :message (format nil "Unknown address family: ~D" family)))))))

(defun set-socket-reuse-address (socket-fd enable)
  "Set SO_REUSEADDR option on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0)
          (if enable
            1
            0))
    (%setsockopt socket-fd +sol-socket+ +so-reuseaddr+ optval 4)))

(defun normalize-data (data)
  "Convert string or vector data to byte vector"
  (etypecase data
    (string
     (sb-ext:string-to-octets data))
    (vector
     data)))

;;; ============================================================================
;;; TCP Implementation
;;; ============================================================================
(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address.
   BACKLOG specifies the maximum pending connections queue size (default 128).
   REUSE-ADDR enables SO_REUSEADDR socket option (default t)."
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (af-family (socket-family-constant family)))
    (handler-case (let* ((socket-fd (create-socket af-family +sock-stream+ +ipproto-tcp+))
                         (kq (kqueue:kqueue)))
      ;; Set reuse address option
      (when reuse-addr
        (set-socket-reuse-address socket-fd t))
      ;; Bind socket
      (bind-socket socket-fd sock-addr)
      ;; Listen
      (when (< (%listen socket-fd backlog) 0)
        (error 'network-error :message "Failed to listen on socket"))
      ;; Add to kqueue for accept events
      (kqueue:add-event kq socket-fd kqueue:+evfilt-read+)
      ;; Create shutdown pipe and register read end with kqueue
      (let* ((pipe (make-shutdown-pipe))
             (shutdown-read-fd (car pipe))
             (shutdown-write-fd (cdr pipe)))
        (kqueue:add-event kq shutdown-read-fd kqueue:+evfilt-read+)
        ;; Get actual bound address (in case port was 0)
        (let ((local-addr (get-socket-address socket-fd :local)))
          (make-instance 'tcp-listener
                         :handle socket-fd
                         :local-address local-addr
                         :kqueue kq
                         :backlog backlog
                         :shutdown-read-fd shutdown-read-fd
                         :shutdown-write-fd shutdown-write-fd))))
      (error
       (e)
       (error 'network-error :message (format nil "TCP bind failed: ~A" e))))))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a new incoming connection.
   Blocks until a connection arrives, the listener is shut down, or the
   optional timeout expires.
   Returns a tcp-stream on success, NIL on timeout or shutdown."
  (handler-case (let ((shutdown-fd (tcp-listener-shutdown-read-fd listener)))
    (loop ;; Wait for accept event or shutdown (block indefinitely when no timeout)
    (let ((events (handler-case (kqueue:wait-for-events (tcp-listener-kqueue listener) 2 timeout)
                    (error
                     ()
                     nil)))) ; Return nil if kqueue fails (e.g., socket closed)
      (unless events
        ;; No events or kqueue error - return nil
        (return nil))
      ;; Check if shutdown was signaled
      (when (and shutdown-fd (find shutdown-fd events :key #'kqueue:kevent-struct-ident))
        (return nil))
      ;; Use max size for IPv6 to handle both address families
      (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+) (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
        (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
          (cond
            ((>= client-fd 0)
             ;; Success - parse address based on family
             (let* ((family (sb-sys:sap-ref-8 addr 1))
                    (peer-addr (cond
                                 ((= family +af-inet+)
                                  (parse-sockaddr-in addr))
                                 ((= family +af-inet6+)
                                  (parse-sockaddr-in6 addr))
                                 (t
                                  (error 'network-error
                                         :message (format nil "Unknown family: ~D" family)))))
                    (local-addr (get-socket-address client-fd :local)))
               (return (values (make-instance 'tcp-stream
                                              :handle client-fd
                                              :local-address local-addr
                                              :peer-address peer-addr
                                              :connected-p t)
                               peer-addr))))
            (t
             ;; accept failed - get errno for specific error
             (let ((errno-val (get-errno)))
               ;; EBADF (9) means socket closed - return nil instead of error
               (if (= errno-val 9)
                 (return nil)
                 (error 'network-error
                        :message (format nil
                                         "Accept failed with errno ~D: ~A"
                                         errno-val
                                         (errno-to-string errno-val))))))))))))
    (network-error
     (e)
     ;; Re-raise network errors as-is
     (error e))
    (error
     (e)
     ;; Wrap other errors with more context
     (error 'network-error :message (format nil "TCP accept failed: ~A (~A)" e (type-of e))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case (let ((events (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 0)))
    (if events
      ;; Use max size for IPv6 to handle both address families
      (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+) (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
        (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
          (if (>= client-fd 0)
            (let* ((family (sb-sys:sap-ref-8 addr 1))
                   (peer-addr (cond
                                ((= family +af-inet+)
                                 (parse-sockaddr-in addr))
                                ((= family +af-inet6+)
                                 (parse-sockaddr-in6 addr))
                                (t
                                 (error 'network-error
                                        :message (format nil "Unknown family: ~D" family)))))
                   (local-addr (get-socket-address client-fd :local)))
              (values (make-instance 'tcp-stream
                                     :handle client-fd
                                     :local-address local-addr
                                     :peer-address peer-addr
                                     :connected-p t)
                      peer-addr))
            :would-block)))
      :would-block))
    (error
     (e)
     (error 'network-error :message (format nil "Try-accept failed: ~A" e)))))

(defun tcp-poll-accept (listener waker)
  "Poll for a connection (async operation)"
  (let ((result (tcp-try-accept listener)))
    (if (eq result :would-block)
      (progn
        (let ((async-op (make-async-operation :fd (tcp-listener-handle listener)
                                              :type
                                              :accept
                                              :callback waker)))
          (submit-async-operation async-op)
          :pending))
      result)))

(defun tcp-incoming (listener)
  "Returns an iterator/sequence over incoming connections"
  ;; Simple implementation - returns a list of accepted connections
  ;; In production, this would be a lazy sequence
  (declare (ignore listener))
  (list))

(defun tcp-local-addr (listener)
  "Get the local address the listener is bound to"
  (tcp-listener-local-address listener))

(defun tcp-close (socket-or-listener)
  "Close a TCP listener or stream.
   Works with both tcp-listener and tcp-stream objects."
  (etypecase socket-or-listener
    (tcp-listener
     ;; Signal any blocked tcp-accept to wake up, then close the listener.
     ;; The kqueue and shutdown pipe fds are intentionally NOT closed here --
     ;; the accept thread may still be inside kevent when this runs.
     (signal-shutdown-pipe (tcp-listener-shutdown-write-fd socket-or-listener))
     (let ((handle (tcp-listener-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (close-socket handle))))
    (tcp-stream
     ;; Shutdown and close the stream's socket handle
     (let ((handle (tcp-stream-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (handler-case (%shutdown handle +shut-rdwr+)
           (error
            ()
            nil))
         (close-socket handle)
         (setf (tcp-stream-connected-p socket-or-listener) nil))))))

(defun tcp-connect (address &key (timeout 30))
  "Connect to a remote TCP server.
   TIMEOUT specifies connection timeout in seconds (default 30).
   Uses blocking connect for reliability."
  (declare (ignore timeout)) ; For future use with select/poll
  (let ((sock-addr (normalize-address address)))
    (log:debug "tcp-connect: Connecting to ~A:~D (family ~A)~%"
               (socket-address-ip sock-addr)
               (socket-address-port sock-addr)
               (socket-address-family sock-addr))
    (let* ((family (socket-address-family sock-addr))
           (af-family (socket-family-constant family))
           (addr-size (sockaddr-size-for-family family))
           (socket-fd (create-socket af-family +sock-stream+ +ipproto-tcp+)))
      (log:debug "tcp-connect: Socket created, fd=~D, af=~D~%" socket-fd af-family)
      ;; Use blocking connect - simpler and more reliable
      ;; The socket starts in blocking mode by default
      (lib:with-foreign-memory ((addr :char :count addr-size))
        (fill-sockaddr-for-address addr sock-addr)
        (log:debug "tcp-connect: Calling %connect...~%")
        (let ((result (%connect socket-fd addr addr-size)))
          (log:debug "tcp-connect: %connect returned ~D~%" result)
          (when (< result 0)
            (let ((errno-val (get-errno)))
              (log:debug "tcp-connect: Connection errno ~D: ~A~%"
                         errno-val
                         (errno-to-string errno-val))
              (close-socket socket-fd) ; Clean up socket on failure
              (case errno-val
                (61
                 (error 'connection-refused
                        :message (format nil
                                         "Connection refused to ~A:~D"
                                         (socket-address-ip sock-addr)
                                         (socket-address-port sock-addr))))
                (t
                 (error 'network-error
                        :message (format nil
                                         "Connect failed to ~A:~D - errno ~D: ~A"
                                         (socket-address-ip sock-addr)
                                         (socket-address-port sock-addr)
                                         errno-val
                                         (errno-to-string errno-val)))))))
          ;; Connection successful - get local address
          (log:debug "tcp-connect: Getting local address...~%")
          (let ((local-addr (get-socket-address socket-fd :local)))
            (log:debug "tcp-connect: Connection successful!~%")
            (make-instance 'tcp-stream
                           :handle socket-fd
                           :local-address local-addr
                           :peer-address sock-addr
                           :connected-p t)))))))

(defun tcp-read (stream buffer &key (start 0) end timeout)
  "Read data from the stream into a buffer"
  (declare (ignore timeout))
  (let* ((end (or end (length buffer)))
         (count (- end start)))
    (handler-case (lib:with-foreign-memory ((buf :char :count count))
      (let ((bytes-read (%recv (tcp-stream-handle stream) buf count 0)))
        (cond
          ((> bytes-read 0)
           ;; Copy data to buffer
           (loop for i from 0 below bytes-read
                 do (setf (aref buffer (+ start i)) (sb-sys:sap-ref-8 buf i)))
           bytes-read)
          ((= bytes-read 0)
           ;; EOF - connection closed
           (setf (tcp-stream-connected-p stream) nil)
           0)
          (t
           (error 'connection-reset :message "Read error")))))
      (error
       (e)
       (error 'network-error :message (format nil "Read failed: ~A" e))))))

(defun tcp-write (stream data &key (start 0) end)
  "Write data to the stream"
  (let* ((data-bytes (normalize-data data))
         (end (or end (length data-bytes)))
         (count (- end start)))
    (handler-case (lib:with-foreign-memory ((buf :char :count count))
      ;; Copy data to foreign buffer
      (loop for i from 0 below
            count
            do (setf (sb-sys:sap-ref-8 buf i) (aref data-bytes (+ start i))))
      (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
        (if (>= bytes-written 0)
          bytes-written
          (error 'connection-reset :message "Write error"))))
      (error
       (e)
       (error 'network-error :message (format nil "Write failed: ~A" e))))))

(defun tcp-write-all (stream data)
  "Write all data, retrying as needed until complete"
  (let* ((data-bytes (normalize-data data))
         (total-bytes (length data-bytes))
         (bytes-written 0))
    (loop while (< bytes-written total-bytes)
          do (incf bytes-written
                   (tcp-write stream data-bytes :start bytes-written :end total-bytes)))))

(defun tcp-flush (stream)
  "Flush any buffered data to the network"
  ;; For raw sockets, there's no buffering at this level
  ;; If using Lisp streams, flush those
  (when (tcp-stream-output stream)
    (finish-output (tcp-stream-output stream))))

(defun tcp-try-read (stream buffer)
  "Try to read without blocking"
  (handler-case (progn
    ;; Set socket to non-blocking
    (set-nonblocking (tcp-stream-handle stream))
    (lib:with-foreign-memory ((buf :char :count (length buffer)))
      (let ((bytes-read (%recv (tcp-stream-handle stream) buf (length buffer) 0)))
        (cond
          ((> bytes-read 0)
           ;; Copy data to buffer
           (loop for i from 0 below bytes-read
                 do (setf (aref buffer i) (sb-sys:sap-ref-8 buf i)))
           bytes-read)
          ((= bytes-read 0)
           0)
          (t
           :would-block)))))
    (error
     ()
     :would-block)))

(defun tcp-try-write (stream data)
  "Try to write without blocking"
  (handler-case (progn
    ;; Set socket to non-blocking
    (set-nonblocking (tcp-stream-handle stream))
    (let* ((data-bytes (normalize-data data))
           (count (length data-bytes)))
      (lib:with-foreign-memory ((buf :char :count count))
        ;; Copy data to foreign buffer
        (loop for i from 0 below
              count
              do (setf (sb-sys:sap-ref-8 buf i) (aref data-bytes i)))
        (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
          (if (>= bytes-written 0)
            bytes-written
            :would-block)))))
    (error
     ()
     :would-block)))

(defun tcp-poll-read (stream waker)
  "Poll for read readiness. Returns :ready if data (or EOF) is available,
   or :pending after registering WAKER for async notification.
   Does not modify the fd's blocking mode."
  (let ((fd (tcp-stream-handle stream)))
    (lib:with-foreign-memory ((buf :char :count 1))
      ;; MSG_PEEK | MSG_DONTWAIT = 0x2 | 0x80 = 0x82
      ;; Non-blocking peek without modifying the fd's blocking state.
      (let ((result (%recv fd buf 1 #x82)))
        (cond
          ;; Data available
          ((> result 0)
           :ready)
          ;; EOF (peer closed) -- the fd IS readable, caller will see 0 from recv
          ((= result 0)
           :ready)
          ;; recv returned -1: check errno
          (t
           (let ((errno-val (get-errno)))
             (if (= errno-val 35) ;; EAGAIN/EWOULDBLOCK
               ;; No data yet, register for async notification
               (let ((async-op (make-async-operation :fd fd
                                                     :type
                                                     :read
                                                     :buffer (make-array 1
                                                                         :element-type '(unsigned-byte 8))
                                                     :callback waker)))
                 (submit-async-operation async-op)
                 :pending)
               ;; Real error
               (error 'network-error
                      :message (format nil "tcp-poll-read failed: ~A" (errno-to-string errno-val)))))))))))

(defun tcp-poll-write (stream waker)
  "Poll for write readiness"
  (let ((dummy-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (tcp-try-write stream dummy-data)))
      (if (eq result :would-block)
        (progn
          (let ((async-op (make-async-operation :fd (tcp-stream-handle stream)
                                                :type
                                                :write
                                                :buffer dummy-data
                                                :callback waker)))
            (submit-async-operation async-op)
            :pending))
        :ready))))

(defun tcp-peer-addr (stream)
  "Get the remote peer's address"
  (tcp-stream-peer-address stream))

(defun tcp-local-addr-stream (stream)
  "Get the local address of the socket"
  (tcp-stream-local-address stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown the TCP connection"
  (handler-case (let ((shutdown-how (ecase how
                                      (:read
                                       +shut-rd+)
                                      (:write
                                       +shut-wr+)
                                      (:both
                                       +shut-rdwr+))))
    (%shutdown (tcp-stream-handle stream) shutdown-how)
    (when (member how '(:both :read))
      (setf (tcp-stream-connected-p stream) nil)))
    (error
     (e)
     (error 'network-error :message (format nil "Shutdown failed: ~A" e)))))

(defun tcp-stream-reader (stream)
  "Get a Lisp character input stream for reading.
   Uses :none buffering to avoid blocking on sockets."
  (or (tcp-stream-input stream)
      (setf (tcp-stream-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :element-type 'character
                                   :buffering
                                   :none))))

(defun tcp-stream-writer (stream)
  "Get a Lisp character output stream for writing.
   Uses :line buffering so newlines trigger flushes."
  (or (tcp-stream-output stream)
      (setf (tcp-stream-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :element-type 'character
                                   :buffering
                                   :line))))

(defun tcp-stream-byte-reader (stream)
  "Get a Lisp byte input stream for reading.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses no buffering to return data as soon as available."
  (or (tcp-stream-byte-input stream)
      (setf (tcp-stream-byte-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :element-type '(unsigned-byte 8)
                                   :buffering
                                   :none))))

(defun tcp-stream-byte-writer (stream)
  "Get a Lisp byte output stream for writing.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses :full buffering - caller must call force-output/finish-output."
  (or (tcp-stream-byte-output stream)
      (setf (tcp-stream-byte-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering
                                   :full))))

(defun tcp-connected-p (stream)
  "Check if the stream is still connected"
  (tcp-stream-connected-p stream))

;;; ============================================================================
;;; UDP Implementation
;;; ============================================================================
(defun udp-bind (address)
  "Create a UDP socket bound to an address"
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (af-family (socket-family-constant family)))
    (handler-case (let ((socket-fd (create-socket af-family +sock-dgram+ +ipproto-udp+)))
      ;; Set socket to non-blocking mode
      (set-nonblocking socket-fd)
      ;; Bind socket
      (bind-socket socket-fd sock-addr)
      ;; Get actual bound address
      (let ((local-addr (get-socket-address socket-fd :local)))
        (make-instance 'udp-socket :handle socket-fd :local-address local-addr)))
      (error
       (e)
       (error 'network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to a default peer"
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (addr-size (sockaddr-size-for-family family)))
    (handler-case (progn
      (lib:with-foreign-memory ((addr :char :count addr-size))
        (fill-sockaddr-for-address addr sock-addr)
        (when (< (%connect (udp-socket-handle socket) addr addr-size) 0)
          (error 'network-error :message "UDP connect failed")))
      (setf (udp-socket-connected-peer socket) sock-addr))
      (error
       (e)
       (error 'network-error :message (format nil "UDP connect failed: ~A" e))))))

(defun udp-send (socket data &key address)
  "Send data on UDP socket.  When ADDRESS is nil, use the connected peer."
  (let ((data-bytes (normalize-data data))
        (count))
    (setf count (length data-bytes))
    (handler-case (if address
      ;; Addressed send via sendto
      (let* ((sock-addr (normalize-address address))
             (family (socket-address-family sock-addr))
             (addr-size (sockaddr-size-for-family family)))
        (lib:with-foreign-memory ((buf :char :count count) (addr-buf :char :count addr-size))
          (loop for i from 0 below
                count
                do (setf (sb-sys:sap-ref-8 buf i) (aref data-bytes i)))
          (fill-sockaddr-for-address addr-buf sock-addr)
          (let ((bytes-sent (%sendto (udp-socket-handle socket) buf count 0 addr-buf addr-size)))
            (if (>= bytes-sent 0)
              bytes-sent
              (error 'network-error :message (format nil "UDP send failed: errno ~D" (get-errno)))))))
      ;; Connected send via send
      (lib:with-foreign-memory ((buf :char :count count))
        (loop for i from 0 below
              count
              do (setf (sb-sys:sap-ref-8 buf i) (aref data-bytes i)))
        (let ((bytes-sent (%send (udp-socket-handle socket) buf count 0)))
          (if (>= bytes-sent 0)
            bytes-sent
            (error 'network-error :message (format nil "UDP send failed: errno ~D" (get-errno)))))))
      (error
       (e)
       (error 'network-error :message (format nil "UDP send failed: ~A" e))))))

(defun udp-recv (socket buffer)
  "Receive data from any sender"
  (handler-case ;; Use max size for IPv6 to handle both address families
  (lib:with-foreign-memory ((buf :char :count (length buffer)) (addr :char
                                                                     :count +sockaddr-in6-size+)
                                                               (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
    (log:debug "udp-recv: Calling %recvfrom with fd=~D, buffer-len=~D~%"
               (udp-socket-handle socket)
               (length buffer))
    (finish-output)
    (let ((bytes-read (%recvfrom (udp-socket-handle socket) buf (length buffer) 0 addr addrlen)))
      (log:debug "udp-recv: %recvfrom returned ~D~%" bytes-read)
      (finish-output)
      (cond
        ((> bytes-read 0)
         ;; Copy data to buffer
         (loop for i from 0 below bytes-read
               do (setf (aref buffer i) (sb-sys:sap-ref-8 buf i)))
         ;; Parse sender address based on family
         (let* ((family (sb-sys:sap-ref-8 addr 1))
                (sender-addr (cond
                               ((= family +af-inet+)
                                (parse-sockaddr-in addr))
                               ((= family +af-inet6+)
                                (parse-sockaddr-in6 addr))
                               (t
                                (error 'network-error
                                       :message (format nil "Unknown family: ~D" family))))))
           (values bytes-read sender-addr)))
        ((and (< bytes-read 0) (= (get-errno) 35)) ; EAGAIN - would block
         (values 0 nil)) ; No data available
        (t
         (error 'network-error :message (format nil "UDP recv failed: errno ~D" (get-errno)))))))
    (error
     (e)
     (error 'network-error :message (format nil "UDP recv failed: ~A" e)))))

(defun udp-send-to (socket data address)
  "Send data to address via UDP socket"
  (udp-send socket data :address address))

(defun udp-recv-from (socket buffer)
  "Receive data from UDP socket"
  (udp-recv socket buffer))

(defun udp-local-addr (socket)
  "Get the local address of the UDP socket"
  (udp-socket-local-address socket))

(defun udp-try-send (socket data address)
  "Try to send UDP data without blocking"
  (handler-case (udp-send socket data :address address)
    (would-block-error
     ()
     :would-block)
    (error
     ()
     :would-block)))

(defun udp-try-recv (socket buffer)
  "Try to receive UDP data without blocking"
  (handler-case (udp-recv socket buffer)
    (would-block-error
     ()
     :would-block)
    (error
     ()
     :would-block)))

(defun udp-poll-send (socket waker)
  "Poll for UDP write readiness (async operation)"
  (let ((dummy-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (udp-try-send socket dummy-data (make-socket-address "127.0.0.1" 1))))
      (if (eq result :would-block)
        (progn
          (let ((async-op (make-async-operation :fd (udp-socket-handle socket)
                                                :type
                                                :write
                                                :buffer dummy-data
                                                :callback waker)))
            (submit-async-operation async-op)
            :pending))
        :ready))))

(defun udp-poll-recv (socket waker)
  "Poll for UDP read readiness (async operation)"
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
    (let ((result (udp-try-recv socket buffer)))
      (if (eq result :would-block)
        (progn
          (let ((async-op (make-async-operation :fd (udp-socket-handle socket)
                                                :type
                                                :read
                                                :buffer buffer
                                                :callback waker)))
            (submit-async-operation async-op)
            :pending))
        result))))

;;; ============================================================================
;;; Unix Domain Socket Operations
;;; ============================================================================
(defun unix-check-error (result operation &optional context)
  "Check a Unix socket syscall result and signal an appropriate condition."
  (when (< result 0)
    (let* ((errno-val (get-errno))
           (errno-str (errno-to-string errno-val))
           (msg (if context
                  (format nil "~A ~A: ~A" operation context errno-str)
                  (format nil "~A: ~A" operation errno-str))))
      (case errno-val
        (61
         (error 'connection-refused :message msg))
        (48
         (error 'address-in-use :message msg))
        (54
         (error 'connection-reset :message msg))
        (t
         (error 'network-error :message msg))))))

(defun unix-connect (path)
  "Connect to a Unix domain socket at PATH. Returns a unix-socket-stream."
  (let ((socket-fd (create-socket +af-unix+ +sock-stream+ 0)))
    (handler-case (progn
      (lib:with-foreign-memory ((addr :char :count +sockaddr-un-size+))
        (let ((addr-len (make-sockaddr-un-into addr path)))
          (unix-check-error (%connect socket-fd addr addr-len) "Unix connect" path)))
      (make-instance 'unix-socket-stream :handle socket-fd :connected-p t))
      (error
       (e)
       (close-socket socket-fd)
       (error e)))))

(defun unix-bind (path &key (backlog 128))
  "Create a Unix domain socket listener bound to PATH.
   Removes any stale socket file at PATH before binding.
   Returns a unix-socket-listener."
  (let ((socket-fd (create-socket +af-unix+ +sock-stream+ 0)))
    (handler-case (progn
      ;; Remove stale socket file if present
      (handler-case (%unlink path)
        (error
         ()
         nil))
      (lib:with-foreign-memory ((addr :char :count +sockaddr-un-size+))
        (let ((addr-len (make-sockaddr-un-into addr path)))
          (unix-check-error (%bind socket-fd addr addr-len) "Unix bind" path)))
      (unix-check-error (%listen socket-fd backlog) "Unix listen" path)
      (make-instance 'unix-socket-listener :handle socket-fd :path path))
      (error
       (e)
       (close-socket socket-fd)
       (error e)))))

(defun unix-accept (listener)
  "Accept a connection on a Unix domain socket listener.
   Returns a unix-socket-stream."
  (let ((client-fd (%accept (unix-socket-listener-handle listener)
                            (sb-sys:int-sap 0)
                            (sb-sys:int-sap 0))))
    (unix-check-error client-fd "Unix accept")
    (make-instance 'unix-socket-stream :handle client-fd :connected-p t)))

(defun unix-socket-io (unix-stream)
  "Get or create a bidirectional (unsigned-byte 8) stream from a unix-socket-stream.
   The returned stream is suitable for binary protocol I/O."
  (or (unix-socket-stream-io-stream unix-stream)
      (setf (unix-socket-stream-io-stream unix-stream)
            (sb-sys:make-fd-stream (unix-socket-stream-handle unix-stream)
                                   :input t
                                   :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering
                                   :full
                                   :auto-close nil))))

(defun unix-close (socket-or-listener)
  "Close a Unix domain socket stream or listener.
   For listeners, also removes the socket file.
   Safe to call multiple times."
  (etypecase socket-or-listener
    (unix-socket-listener
     (let ((handle (unix-socket-listener-handle socket-or-listener)))
       (when (and handle (integerp handle) (>= handle 0))
         (setf (unix-socket-listener-handle socket-or-listener) nil)
         (close-socket handle)
         ;; Remove the socket file
         (handler-case (%unlink (unix-socket-listener-path socket-or-listener))
           (error
            ()
            nil)))))
    (unix-socket-stream
     (let ((handle (unix-socket-stream-handle socket-or-listener)))
       (when (and handle (integerp handle) (>= handle 0))
         (setf (unix-socket-stream-handle socket-or-listener) nil)
         ;; Close the io-stream first if it exists
         (when (unix-socket-stream-io-stream socket-or-listener)
           (handler-case (close (unix-socket-stream-io-stream socket-or-listener))
             (error
              ()
              nil))
           (setf (unix-socket-stream-io-stream socket-or-listener) nil))
         (handler-case (close-socket handle)
           (error
            ()
            nil))
         (setf (unix-socket-stream-connected-p socket-or-listener) nil))))))

;;; ============================================================================
;;; Unix Datagram Sockets
;;; ============================================================================

(defun unix-dgram-bind (path)
  "Create a Unix datagram socket bound to PATH.
Removes any stale socket file at PATH before binding.
Returns a unix-dgram-socket."
  (let ((socket-fd (create-socket +af-unix+ +sock-dgram+ 0)))
    (handler-case
        (progn
          (handler-case (%unlink path) (error () nil))
          (lib:with-foreign-memory ((addr :char :count +sockaddr-un-size+))
            (let ((addr-len (make-sockaddr-un-into addr path)))
              (unix-check-error (%bind socket-fd addr addr-len) "unix dgram bind")))
          (make-instance 'unix-dgram-socket :handle socket-fd :path path))
      (error (e)
        (%close socket-fd)
        (error e)))))

(defun unix-dgram-connect (path)
  "Create a Unix datagram socket connected to PATH.
Returns a unix-dgram-socket."
  (let ((socket-fd (create-socket +af-unix+ +sock-dgram+ 0)))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count +sockaddr-un-size+))
            (let ((addr-len (make-sockaddr-un-into addr path)))
              (unix-check-error (%connect socket-fd addr addr-len) "unix dgram connect")))
          (make-instance 'unix-dgram-socket :handle socket-fd))
      (error (e)
        (%close socket-fd)
        (error e)))))

(defun unix-dgram-send (socket data &key (start 0) (end nil))
  "Send DATA on a connected Unix datagram socket.
DATA is a (vector (unsigned-byte 8)). Returns bytes sent."
  (let* ((actual-end (or end (length data)))
         (len (- actual-end start)))
    (lib:with-foreign-memory ((buf :char :count len))
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref data i)))
      (let ((result (%send (unix-dgram-socket-handle socket) buf len 0)))
        (unix-check-error result "unix dgram send")
        result))))

(defun unix-dgram-recv (socket buffer &key (start 0) (end nil))
  "Receive a datagram into BUFFER on a bound Unix datagram socket.
BUFFER is a (vector (unsigned-byte 8)).
Returns the number of bytes received, or 0 on empty read."
  (let* ((actual-end (or end (length buffer)))
         (len (- actual-end start)))
    (lib:with-foreign-memory ((buf :char :count len))
      (let ((result (%recv (unix-dgram-socket-handle socket) buf len 0)))
        (cond
          ((> result 0)
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           result)
          ((= result 0) 0)
          (t (unix-check-error result "unix dgram recv") 0))))))

(defun unix-dgram-close (socket)
  "Close a Unix datagram socket. Removes the socket file if bound."
  (let ((handle (unix-dgram-socket-handle socket)))
    (when (and handle (integerp handle) (>= handle 0))
      (setf (unix-dgram-socket-handle socket) nil)
      (%close handle)
      (when (unix-dgram-socket-path socket)
        (handler-case (%unlink (unix-dgram-socket-path socket))
          (error () nil))))))
