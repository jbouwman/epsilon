;;;; Unified Socket Operations - TCP and UDP
;;;;
;;;; This module provides the main socket operations for TCP and UDP networking.

(defpackage epsilon.net.sockets
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (address epsilon.net.address)
   (core epsilon.net.core)
   (epoll epsilon.sys.epoll)
   (epoll-mgr epsilon.net.epoll-manager)
   (lib epsilon.foreign))
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
   #:tcp-connected-p
   
   ;; UDP operations
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-poll-send
   #:udp-poll-recv))

(in-package epsilon.net.sockets)

;;; ============================================================================
;;; Socket Event Waiting (Refactored to use Epoll Manager)
;;; ============================================================================

(defun wait-for-socket-ready (socket-fd events-list timeout-ms)
  "Wait for a socket to be ready for specified events using the shared epoll manager.
   Events is a list of keywords like '(:in) or '(:out).
   Returns the event if ready, nil on timeout."
  (let ((event (epoll-mgr:wait-for-socket socket-fd events-list timeout-ms)))
    (when event
      ;; Check if the event indicates an error
      (let ((event-mask (epoll:epoll-event-events event)))
        (cond
          ((or (logtest event-mask epoll:+epollerr+)
               (logtest event-mask epoll:+epollhup+))
           nil)  ; Error or hangup
          (t event))))))  ; Normal event

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (socket-fd (core:create-socket const:+af-inet+ 
                                        const:+sock-stream+ 
                                        const:+ipproto-tcp+)))
    
    (when reuse-addr
      (core:set-socket-reuse-addr socket-fd t))
    
    (handler-case
        (progn
          ;; Bind socket to address
          (lib:with-foreign-memory ((sockaddr :char :count 16))
            (address:make-sockaddr-in-into sockaddr 
                                           (types:socket-address-ip sock-addr)
                                           (types:socket-address-port sock-addr))
            (let ((result (const:%bind socket-fd sockaddr 16)))
              (errors:check-error result "bind")))
          
          ;; Listen for connections
          (let ((result (const:%listen socket-fd backlog)))
            (errors:check-error result "listen"))
          
          ;; Set to non-blocking mode
          (core:set-nonblocking socket-fd)
          
          ;; Register with epoll manager for incoming connections
          (epoll-mgr:register-socket socket-fd '(:in))
          
          ;; Get actual bound address (in case port was 0)
          (let ((actual-addr (core:get-local-address socket-fd)))
            (make-instance 'types:tcp-listener
                           :handle socket-fd
                           :local-address actual-addr
                           :backlog backlog)))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a connection, with optional timeout in seconds"
  (let ((timeout-ms (if timeout (round (* timeout 1000)) -1)))
    (loop
      (handler-case
          (lib:with-foreign-memory ((peer-sockaddr :char :count 16)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 16)
            (let ((client-fd (const:%accept (types:tcp-listener-handle listener) 
                                            peer-sockaddr addrlen)))
              (cond
                ((>= client-fd 0)
                 ;; Success - got a connection
                 (core:set-nonblocking client-fd)
                 (let ((local-addr (core:get-local-address client-fd))
                       (peer-addr (address:parse-sockaddr-in peer-sockaddr)))
                   (return (make-instance 'types:tcp-stream
                                          :handle client-fd
                                          :local-address local-addr
                                          :peer-address peer-addr
                                          :connected-p t))))
                (t
                 ;; Check what error occurred
                 (errors:check-error client-fd "accept")))))
        (errors:would-block-error ()
          ;; Wait for socket to be ready with timeout
          (let ((event (wait-for-socket-ready 
                        (types:tcp-listener-handle listener)
                        '(:in)
                        timeout-ms)))
            (unless event
              ;; Timeout occurred
              (when timeout
                (error 'errors:timeout-error :message "Accept timeout")))))
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
  (when (wait-for-socket-ready (types:tcp-listener-handle listener) 
                               '(:in) 
                               timeout-ms)
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
  "Connect to a TCP server with optional timeout"
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (socket-fd (core:create-socket const:+af-inet+ 
                                        const:+sock-stream+ 
                                        const:+ipproto-tcp+)))
    
    (handler-case
        (progn
          ;; Set to non-blocking mode before connecting for async support
          (core:set-nonblocking socket-fd)
          
          ;; Try to connect
          (lib:with-foreign-memory ((sockaddr :char :count 16))
            (address:make-sockaddr-in-into sockaddr 
                                           (types:socket-address-ip sock-addr)
                                           (types:socket-address-port sock-addr))
            (let ((result (const:%connect socket-fd sockaddr 16)))
              (when (< result 0)
                ;; Check if it's a would-block error (async connection in progress)
                (let ((errno (errors:get-errno)))
                  (if (= errno const:+einprogress+)
                      (when timeout
                        ;; Wait for connection to complete with timeout
                        (let ((timeout-ms (round (* timeout 1000))))
                          ;; Register socket for write events
                          (epoll-mgr:register-socket socket-fd '(:out))
                          (unwind-protect
                              (let ((event (epoll-mgr:wait-for-socket 
                                           socket-fd '(:out) timeout-ms)))
                                (unless event
                                  (error 'errors:timeout-error 
                                         :message "Connection timeout")))
                            (epoll-mgr:unregister-socket socket-fd))))
                      ;; Other error, check and signal appropriately
                      (errors:check-error result "connect"))))))
          
          ;; Get local and peer addresses
          (let ((local-addr (core:get-local-address socket-fd))
                (peer-addr (core:get-peer-address socket-fd)))
            (make-instance 'types:tcp-stream
                           :handle socket-fd
                           :local-address local-addr
                           :peer-address peer-addr
                           :connected-p t)))
      (error (e)
        (const:%close socket-fd)
        (error e)))))

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

(defun tcp-read (stream buffer &key (start 0) (end nil) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (let ((actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      (let ((result (const:%recv (types:tcp-stream-handle stream) 
                                 buf (- actual-end start) 0)))
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
             (if (= errno const:+eagain+)
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
                     0) ; No data available, no timeout specified
                 (progn
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
             (if (= errno const:+eagain+)
                 (if timeout
                     ;; Wait for socket to be writable with timeout
                     (let ((timeout-ms (round (* timeout 1000))))
                       (if (wait-for-socket-ready 
                            (types:tcp-stream-handle stream) 
                            '(:out) 
                            timeout-ms)
                           ;; Retry write after epoll indicates writability
                           (tcp-write stream data :start start :end end :timeout nil)
                           0)) ; Timeout occurred
                     0) ; No bytes written, would block, no timeout specified
                 (progn
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

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (wait-for-socket-ready (types:tcp-stream-handle stream) '(:in) timeout-ms))

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (wait-for-socket-ready (types:tcp-stream-handle stream) '(:out) timeout-ms))

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (types:tcp-stream-peer-address stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown TCP stream"
  (let ((shutdown-how (ecase how
                        (:read const:+shut-rd+)
                        (:write const:+shut-wr+)
                        (:both const:+shut-rdwr+))))
    (let ((result (const:%shutdown (types:tcp-stream-handle stream) shutdown-how)))
      (errors:check-error result "shutdown"))
    ;; Unregister from epoll manager
    (handler-case
        (epoll-mgr:unregister-socket (types:tcp-stream-handle stream))
      (error ()
        ;; Ignore errors - socket might not be registered
        nil))
    (setf (types:tcp-stream-connected-p stream) nil)))

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (types:tcp-stream-connected-p stream))

(defun tcp-close (socket-or-listener)
  "Close a TCP listener or stream.
   Works with both tcp-listener and tcp-stream objects."
  (etypecase socket-or-listener
    (types:tcp-listener
     ;; Close the listener's socket handle
     (let ((handle (types:tcp-listener-handle socket-or-listener)))
       (when (and handle (>= handle 0))
         ;; Unregister from epoll manager if registered
         (handler-case
             (epoll-mgr:unregister-socket handle)
           (error () nil))
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
             (epoll-mgr:unregister-socket handle)
           (error () nil))
         (core:close-socket handle)
         (setf (types:tcp-stream-connected-p socket-or-listener) nil))))))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (socket-fd (core:create-socket const:+af-inet+ 
                                        const:+sock-dgram+ 
                                        const:+ipproto-udp+)))
    
    (handler-case
        (progn
          (core:set-socket-reuse-addr socket-fd t)
          
          ;; Bind socket to address
          (lib:with-foreign-memory ((sockaddr :char :count 16))
            (address:make-sockaddr-in-into sockaddr 
                                           (types:socket-address-ip sock-addr)
                                           (types:socket-address-port sock-addr))
            (let ((result (const:%bind socket-fd sockaddr 16)))
              (errors:check-error result "UDP bind")))
          
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
  "Connect UDP socket to remote address"
  (let ((sock-addr (etypecase address
                     (types:socket-address address)
                     (string (address:parse-address address)))))
    (handler-case
        (lib:with-foreign-memory ((sockaddr :char :count 16))
          (address:make-sockaddr-in-into sockaddr 
                                         (types:socket-address-ip sock-addr)
                                         (types:socket-address-port sock-addr))
          (let ((result (const:%connect (types:udp-socket-handle socket) sockaddr 16)))
            (errors:check-error result "UDP connect")
            (setf (types:udp-socket-connected-peer socket) sock-addr)))
      (error (e)
        (error e)))))

(defun udp-send (socket data &key (start 0) (end nil))
  "Send data on connected UDP socket"
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
      
      (let ((result (const:%send (types:udp-socket-handle socket) 
                                 buf (- actual-end start) 0)))
        (errors:check-error result "UDP send")
        result))))

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
  "Send data to specific address"
  (let* ((sock-addr (etypecase address
                      (types:socket-address address)
                      (string (address:parse-address address))))
         (buffer (etypecase data
                   (string (sb-ext:string-to-octets data))
                   (vector data)
                   (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start))
                              (sockaddr :char :count 16))
      ;; Copy data to foreign buffer
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
      
      ;; Create destination address
      (address:make-sockaddr-in-into sockaddr 
                                     (types:socket-address-ip sock-addr)
                                     (types:socket-address-port sock-addr))
      
      (let ((result (const:%sendto (types:udp-socket-handle socket) 
                                   buf (- actual-end start) 0
                                   sockaddr 16)))
        (errors:check-error result "UDP sendto")
        result))))

(defun udp-recv-from (socket buffer &key (start 0) (end nil))
  "Receive data and sender address"
  (let ((actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start))
                              (sockaddr :char :count 16)
                              (addrlen :int :count 1))
      (setf (sb-sys:sap-ref-32 addrlen 0) 16)
      (let ((result (const:%recvfrom (types:udp-socket-handle socket) 
                                     buf (- actual-end start) 0
                                     sockaddr addrlen)))
        (cond
          ((> result 0)
           ;; Copy data from foreign buffer to Lisp buffer
           (loop for i from 0 below result
                 do (setf (aref buffer (+ start i))
                          (sb-sys:sap-ref-8 buf i)))
           (values result (address:parse-sockaddr-in sockaddr)))
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