;;;; Unified socket operations - TCP and UDP

(defpackage epsilon.net.sockets
  (:use cl)
  (:local-nicknames
   (kqueue epsilon.kqueue)
   (lib epsilon.foreign)
   (log epsilon.log))
  (:import-from epsilon.net.constants
   +af-inet+ +af-inet6+ +sock-stream+ +ipproto-tcp+ +sock-dgram+ +ipproto-udp+
   +sol-socket+ +so-reuseaddr+
   +shut-rd+ +shut-wr+ +shut-rdwr+
   +sockaddr-in6-size+
   %socket %bind %listen %accept %connect %send %recv %close %shutdown
   %setsockopt %getsockname %getpeername %sendto %recvfrom)
  (:import-from epsilon.net.core
   socket-address socket-address-ip socket-address-port socket-address-family
   tcp-listener tcp-listener-handle tcp-listener-local-address tcp-listener-kqueue
   tcp-stream tcp-stream-handle tcp-stream-local-address tcp-stream-peer-address
   tcp-stream-input tcp-stream-output tcp-stream-byte-input tcp-stream-byte-output
   tcp-stream-connected-p
   udp-socket udp-socket-handle udp-socket-local-address udp-socket-connected-peer
   network-error connection-refused connection-reset address-in-use would-block-error
   get-errno errno-to-string)
  (:import-from epsilon.net.address
   normalize-address make-sockaddr-in-into parse-sockaddr-in parse-sockaddr-in6
   make-socket-address fill-sockaddr-for-address sockaddr-size-for-family socket-family-constant)
  (:import-from epsilon.async
   set-nonblocking submit-async-operation make-async-operation)
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

(in-package epsilon.net.sockets)

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
               :message (format nil "Failed to bind socket to ~A:~D"
                                (socket-address-ip sock-addr)
                                (socket-address-port sock-addr)))))))

(defun close-socket (socket-fd)
  "Close a socket"
  (when (>= socket-fd 0)
    (%close socket-fd)))

(defun get-socket-address (socket-fd type)
  "Get local or peer address of socket"
  ;; Use max size for IPv6 to handle both address families
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
    (ecase type
      (:local (%getsockname socket-fd addr addrlen))
      (:peer (%getpeername socket-fd addr addrlen)))
    ;; Check sa_family to determine address type (offset 1 on Darwin)
    (let ((family (sb-sys:sap-ref-8 addr 1)))
      (cond
        ((= family +af-inet+) (parse-sockaddr-in addr))
        ((= family +af-inet6+) (parse-sockaddr-in6 addr))
        (t (error 'network-error
                  :message (format nil "Unknown address family: ~D" family)))))))

(defun set-socket-reuse-address (socket-fd enable)
  "Set SO_REUSEADDR option on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) (if enable 1 0))
    (%setsockopt socket-fd +sol-socket+ +so-reuseaddr+ optval 4)))

(defun normalize-data (data)
  "Convert string or vector data to byte vector"
  (etypecase data
    (string (sb-ext:string-to-octets data))
    (vector data)))

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
    (handler-case
        (let* ((socket-fd (create-socket af-family +sock-stream+ +ipproto-tcp+))
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

          ;; Get actual bound address (in case port was 0)
          (let ((local-addr (get-socket-address socket-fd :local)))
            (make-instance 'tcp-listener
                           :handle socket-fd
                           :local-address local-addr
                           :kqueue kq
                           :backlog backlog)))
      (error (e)
        (error 'network-error :message (format nil "TCP bind failed: ~A" e))))))

(defun tcp-accept (listener &key (timeout 1.0))
  "Accept a new incoming connection.
   If TIMEOUT is provided, waits up to that many seconds for each poll cycle.
   Returns NIL if the listener socket has been closed or if interrupted.
   With timeout, periodically returns NIL to allow checking for shutdown."
  (handler-case
      (loop
       ;; Wait for accept event with timeout
       (let ((events (handler-case
                         (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 timeout)
                       (error () nil))))  ; Return nil if kqueue fails (e.g., socket closed)
         (unless events
           ;; No events or kqueue error - return nil to allow shutdown check
           (return nil))
         ;; Use max size for IPv6 to handle both address families
         (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+)
                                   (addrlen :int :count 1))
           (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
           (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
             (cond
               ((>= client-fd 0)
                ;; Success - parse address based on family
                (let* ((family (sb-sys:sap-ref-8 addr 1))
                       (peer-addr (cond
                                    ((= family +af-inet+) (parse-sockaddr-in addr))
                                    ((= family +af-inet6+) (parse-sockaddr-in6 addr))
                                    (t (error 'network-error
                                              :message (format nil "Unknown family: ~D" family)))))
                       (local-addr (get-socket-address client-fd :local)))
                  (return (values
                           (make-instance 'tcp-stream
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
                             :message (format nil "Accept failed with errno ~D: ~A"
                                              errno-val
                                              (errno-to-string errno-val)))))))))))
    (network-error (e)
      ;; Re-raise network errors as-is
      (error e))
    (error (e)
      ;; Wrap other errors with more context
      (error 'network-error :message (format nil "TCP accept failed: ~A (~A)" e (type-of e))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (let ((events (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 0)))
        (if events
            ;; Use max size for IPv6 to handle both address families
            (lib:with-foreign-memory ((addr :char :count +sockaddr-in6-size+)
                                      (addrlen :int :count 1))
              (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
              (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
                (if (>= client-fd 0)
                    (let* ((family (sb-sys:sap-ref-8 addr 1))
                           (peer-addr (cond
                                        ((= family +af-inet+) (parse-sockaddr-in addr))
                                        ((= family +af-inet6+) (parse-sockaddr-in6 addr))
                                        (t (error 'network-error
                                                  :message (format nil "Unknown family: ~D" family)))))
                           (local-addr (get-socket-address client-fd :local)))
                      (values
                       (make-instance 'tcp-stream
                                      :handle client-fd
                                      :local-address local-addr
                                      :peer-address peer-addr
                                      :connected-p t)
                       peer-addr))
                    :would-block)))
            :would-block))
    (error (e)
      (error 'network-error :message (format nil "Try-accept failed: ~A" e)))))

(defun tcp-poll-accept (listener waker)
  "Poll for a connection (async operation)"
  (let ((result (tcp-try-accept listener)))
    (if (eq result :would-block)
        (progn
          (let ((async-op (make-async-operation
                           :fd (tcp-listener-handle listener)
                           :type :accept
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
     ;; Close the listener's socket handle (kqueue will be cleaned up when socket closes)
     (let ((handle (tcp-listener-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (close-socket handle))))
    (tcp-stream
     ;; Shutdown and close the stream's socket handle
     (let ((handle (tcp-stream-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         (handler-case
             (%shutdown handle +shut-rdwr+)
           (error () nil))
         (close-socket handle)
         (setf (tcp-stream-connected-p socket-or-listener) nil))))))

(defun tcp-connect (address &key (timeout 30))
  "Connect to a remote TCP server.
   TIMEOUT specifies connection timeout in seconds (default 30).
   Uses blocking connect for reliability."
  (declare (ignore timeout)) ; For future use with select/poll
  (let ((sock-addr (normalize-address address)))
    (log:debug "tcp-connect: Connecting to ~A:~D (family ~A)~%"
               (socket-address-ip sock-addr) (socket-address-port sock-addr)
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
                         errno-val (errno-to-string errno-val))
              (close-socket socket-fd)  ; Clean up socket on failure
              (case errno-val
                (61 (error 'connection-refused
                           :message (format nil "Connection refused to ~A:~D"
                                            (socket-address-ip sock-addr)
                                            (socket-address-port sock-addr))))
                (t (error 'network-error
                          :message (format nil "Connect failed to ~A:~D - errno ~D: ~A"
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

(defun tcp-read (stream buffer &key (start 0) end)
  "Read data from the stream into a buffer"
  (let* ((end (or end (length buffer)))
         (count (- end start)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count))
          (let ((bytes-read (%recv (tcp-stream-handle stream) buf count 0)))
            (cond
              ((> bytes-read 0)
               ;; Copy data to buffer
               (loop for i from 0 below bytes-read
                     do (setf (aref buffer (+ start i))
                              (sb-sys:sap-ref-8 buf i)))
               bytes-read)
              ((= bytes-read 0)
               ;; EOF - connection closed
               (setf (tcp-stream-connected-p stream) nil)
               0)
              (t
               (error 'connection-reset :message "Read error")))))
      (error (e)
        (error 'network-error :message (format nil "Read failed: ~A" e))))))

(defun tcp-write (stream data &key (start 0) end)
  "Write data to the stream"
  (let* ((data-bytes (normalize-data data))
         (end (or end (length data-bytes)))
         (count (- end start)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count))
          ;; Copy data to foreign buffer
          (loop for i from 0 below count
                do (setf (sb-sys:sap-ref-8 buf i)
                         (aref data-bytes (+ start i))))
          (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
            (if (>= bytes-written 0)
                bytes-written
                (error 'connection-reset :message "Write error"))))
      (error (e)
        (error 'network-error :message (format nil "Write failed: ~A" e))))))

(defun tcp-write-all (stream data)
  "Write all data, retrying as needed until complete"
  (let* ((data-bytes (normalize-data data))
         (total-bytes (length data-bytes))
         (bytes-written 0))
    (loop while (< bytes-written total-bytes)
          do (incf bytes-written
                   (tcp-write stream data-bytes
                              :start bytes-written
                              :end total-bytes)))))

(defun tcp-flush (stream)
  "Flush any buffered data to the network"
  ;; For raw sockets, there's no buffering at this level
  ;; If using Lisp streams, flush those
  (when (tcp-stream-output stream)
    (finish-output (tcp-stream-output stream))))

(defun tcp-try-read (stream buffer)
  "Try to read without blocking"
  (handler-case
      (progn
        ;; Set socket to non-blocking
        (set-nonblocking (tcp-stream-handle stream))
        (lib:with-foreign-memory ((buf :char :count (length buffer)))
          (let ((bytes-read (%recv (tcp-stream-handle stream) buf (length buffer) 0)))
            (cond
              ((> bytes-read 0)
               ;; Copy data to buffer
               (loop for i from 0 below bytes-read
                     do (setf (aref buffer i)
                              (sb-sys:sap-ref-8 buf i)))
               bytes-read)
              ((= bytes-read 0)
               0)
              (t :would-block)))))
    (error () :would-block)))

(defun tcp-try-write (stream data)
  "Try to write without blocking"
  (handler-case
      (progn
        ;; Set socket to non-blocking
        (set-nonblocking (tcp-stream-handle stream))
        (let* ((data-bytes (normalize-data data))
               (count (length data-bytes)))
          (lib:with-foreign-memory ((buf :char :count count))
            ;; Copy data to foreign buffer
            (loop for i from 0 below count
                  do (setf (sb-sys:sap-ref-8 buf i)
                           (aref data-bytes i)))
            (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
              (if (>= bytes-written 0)
                  bytes-written
                  :would-block)))))
    (error () :would-block)))

(defun tcp-poll-read (stream waker)
  "Poll for read readiness"
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
    (let ((result (tcp-try-read stream buffer)))
      (if (eq result :would-block)
          (progn
            (let ((async-op (make-async-operation
                             :fd (tcp-stream-handle stream)
                             :type :read
                             :buffer buffer
                             :callback waker)))
              (submit-async-operation async-op)
              :pending))
          result))))

(defun tcp-poll-write (stream waker)
  "Poll for write readiness"
  (let ((dummy-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (tcp-try-write stream dummy-data)))
      (if (eq result :would-block)
          (progn
            (let ((async-op (make-async-operation
                             :fd (tcp-stream-handle stream)
                             :type :write
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
  (handler-case
      (let ((shutdown-how (ecase how
                            (:read +shut-rd+)
                            (:write +shut-wr+)
                            (:both +shut-rdwr+))))
        (%shutdown (tcp-stream-handle stream) shutdown-how)
        (when (member how '(:both :read))
          (setf (tcp-stream-connected-p stream) nil)))
    (error (e)
      (error 'network-error :message (format nil "Shutdown failed: ~A" e)))))

(defun tcp-stream-reader (stream)
  "Get a Lisp character input stream for reading.
   Uses character element-type for text-based protocols (HTTP, ELS).
   Uses :none buffering to avoid blocking on sockets."
  (or (tcp-stream-input stream)
      (setf (tcp-stream-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :element-type 'character
                                   :buffering :none))))

(defun tcp-stream-writer (stream)
  "Get a Lisp character output stream for writing.
   Uses character element-type for text-based protocols (HTTP, ELS).
   Uses :line buffering so newlines trigger flushes."
  (or (tcp-stream-output stream)
      (setf (tcp-stream-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :element-type 'character
                                   :buffering :line))))

(defun tcp-stream-byte-reader (stream)
  "Get a Lisp byte input stream for reading.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses no buffering to return data as soon as available."
  (or (tcp-stream-byte-input stream)
      (setf (tcp-stream-byte-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :none))))

(defun tcp-stream-byte-writer (stream)
  "Get a Lisp byte output stream for writing.
   Uses (unsigned-byte 8) element-type for binary protocols.
   Uses :full buffering - caller must call force-output/finish-output."
  (or (tcp-stream-byte-output stream)
      (setf (tcp-stream-byte-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :full))))

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
    (handler-case
        (let ((socket-fd (create-socket af-family +sock-dgram+ +ipproto-udp+)))
          ;; Set socket to non-blocking mode
          (set-nonblocking socket-fd)

          ;; Bind socket
          (bind-socket socket-fd sock-addr)

          ;; Get actual bound address
          (let ((local-addr (get-socket-address socket-fd :local)))
            (make-instance 'udp-socket
                           :handle socket-fd
                           :local-address local-addr)))
      (error (e)
        (error 'network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to a default peer"
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (addr-size (sockaddr-size-for-family family)))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count addr-size))
            (fill-sockaddr-for-address addr sock-addr)
            (when (< (%connect (udp-socket-handle socket) addr addr-size) 0)
              (error 'network-error :message "UDP connect failed")))
          (setf (udp-socket-connected-peer socket) sock-addr))
      (error (e)
        (error 'network-error :message (format nil "UDP connect failed: ~A" e))))))

(defun udp-send (socket data address)
  "Send data to a specific address"
  (let* ((sock-addr (normalize-address address))
         (family (socket-address-family sock-addr))
         (addr-size (sockaddr-size-for-family family))
         (data-bytes (normalize-data data))
         (count (length data-bytes)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count)
                                  (addr :char :count addr-size))
          ;; Copy data to foreign buffer
          (loop for i from 0 below count
                do (setf (sb-sys:sap-ref-8 buf i)
                         (aref data-bytes i)))
          ;; Set up address
          (fill-sockaddr-for-address addr sock-addr)
          (log:debug "udp-send: Calling %sendto with fd=~D, count=~D~%" (udp-socket-handle socket) count)
          (finish-output)
          (let ((bytes-sent (%sendto (udp-socket-handle socket) buf count 0 addr addr-size)))
            (log:debug "udp-send: %sendto returned ~D~%" bytes-sent)
            (finish-output)
            (if (>= bytes-sent 0)
                bytes-sent
                (error 'network-error :message (format nil "UDP send failed: errno ~D" (get-errno))))))
      (error (e)
        (error 'network-error :message (format nil "UDP send failed: ~A" e))))))

(defun udp-recv (socket buffer)
  "Receive data from any sender"
  (handler-case
      ;; Use max size for IPv6 to handle both address families
      (lib:with-foreign-memory ((buf :char :count (length buffer))
                                (addr :char :count +sockaddr-in6-size+)
                                (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in6-size+)
        (log:debug "udp-recv: Calling %recvfrom with fd=~D, buffer-len=~D~%"
                   (udp-socket-handle socket) (length buffer))
        (finish-output)
        (let ((bytes-read (%recvfrom (udp-socket-handle socket)
                                     buf (length buffer) 0
                                     addr addrlen)))
          (log:debug "udp-recv: %recvfrom returned ~D~%" bytes-read)
          (finish-output)
          (cond
            ((> bytes-read 0)
             ;; Copy data to buffer
             (loop for i from 0 below bytes-read
                   do (setf (aref buffer i)
                            (sb-sys:sap-ref-8 buf i)))
             ;; Parse sender address based on family
             (let* ((family (sb-sys:sap-ref-8 addr 1))
                    (sender-addr (cond
                                   ((= family +af-inet+) (parse-sockaddr-in addr))
                                   ((= family +af-inet6+) (parse-sockaddr-in6 addr))
                                   (t (error 'network-error
                                             :message (format nil "Unknown family: ~D" family))))))
               (values bytes-read sender-addr)))
            ((and (< bytes-read 0) (= (get-errno) 35)) ; EAGAIN - would block
             (values 0 nil))  ; No data available
            (t
             (error 'network-error
                    :message (format nil "UDP recv failed: errno ~D" (get-errno)))))))
    (error (e)
      (error 'network-error :message (format nil "UDP recv failed: ~A" e)))))

(defun udp-send-to (socket data address)
  "Send data to address via UDP socket"
  (udp-send socket data address))

(defun udp-recv-from (socket buffer)
  "Receive data from UDP socket"
  (udp-recv socket buffer))

(defun udp-local-addr (socket)
  "Get the local address of the UDP socket"
  (udp-socket-local-address socket))

(defun udp-try-send (socket data address)
  "Try to send UDP data without blocking"
  (handler-case
      (udp-send socket data address)
    (would-block-error () :would-block)
    (error () :would-block)))

(defun udp-try-recv (socket buffer)
  "Try to receive UDP data without blocking"
  (handler-case
      (udp-recv socket buffer)
    (would-block-error () :would-block)
    (error () :would-block)))

(defun udp-poll-send (socket waker)
  "Poll for UDP write readiness (async operation)"
  (let ((dummy-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (udp-try-send socket dummy-data
                                (make-socket-address "127.0.0.1" 1))))
      (if (eq result :would-block)
          (progn
            (let ((async-op (make-async-operation
                             :fd (udp-socket-handle socket)
                             :type :write
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
            (let ((async-op (make-async-operation
                             :fd (udp-socket-handle socket)
                             :type :read
                             :buffer buffer
                             :callback waker)))
              (submit-async-operation async-op)
              :pending))
          result))))
