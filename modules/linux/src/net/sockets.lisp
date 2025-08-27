;;;; Unified Socket Operations - TCP and UDP
;;;;
;;;; Following the Darwin pattern - consolidate common socket operations

(defpackage epsilon.net.sockets
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (address epsilon.net.address)
   (core epsilon.net.core)
   (epoll epsilon.sys.epoll)
   (lib epsilon.foreign))
  (:export
   ;; Common socket utilities
   #:create-socket
   #:bind-socket
   #:close-socket
   #:get-socket-address
   #:set-socket-reuse-address
   
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
   #:udp-try-send
   #:udp-try-recv
   #:udp-poll-send
   #:udp-poll-recv))

(in-package epsilon.net.sockets)

;;; ============================================================================
;;; Common Socket Utilities
;;; ============================================================================

(defun create-socket (family type protocol)
  "Create a socket with specified family, type, and protocol"
  (core:create-socket family type protocol))

(defun bind-socket (socket-fd address)
  "Bind a socket to an address"
  (core:bind-socket socket-fd address))

(defun close-socket (socket-fd)
  "Close a socket"
  (core:close-socket socket-fd))

(defun get-socket-address (socket-fd type)
  "Get local or peer address of socket"
  (ecase type
    (:local (core:get-local-address socket-fd))
    (:peer (core:get-peer-address socket-fd))))

(defun set-socket-reuse-address (socket-fd enable)
  "Set SO_REUSEADDR option on socket"
  (core:set-socket-reuse-addr socket-fd enable))

;;; ============================================================================
;;; Epoll Integration Utilities
;;; ============================================================================

(defun ensure-socket-epoll (socket epoll-accessor)
  "Ensure socket has an epoll instance"
  (unless (funcall epoll-accessor socket)
    (let ((epoll-instance (epoll:epoll-create1 epoll:+epoll-cloexec+)))
      (setf (funcall epoll-accessor socket) epoll-instance)))
  (funcall epoll-accessor socket))

(defun wait-for-socket-ready (socket-fd epoll-fd events timeout-ms)
  "Wait for socket to be ready for specified events"
  ;; First ensure the socket is in epoll
  (handler-case
      (epoll:epoll-ctl epoll-fd epoll:+epoll-ctl-add+ socket-fd
                       (epoll:make-epoll-event :events events :data socket-fd))
    (error ()
      ;; Might already be added, try to modify instead
      (ignore-errors
        (epoll:epoll-ctl epoll-fd epoll:+epoll-ctl-mod+ socket-fd
                         (epoll:make-epoll-event :events events :data socket-fd)))))
  
  ;; Wait for events
  (let ((events-list (epoll:wait-for-events epoll-fd 1 timeout-ms)))
    (when events-list
      (let ((event (first events-list)))
        (= socket-fd (epoll:epoll-event-data event))))))

;;; ============================================================================
;;; TCP Implementation
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (let ((sock-addr (address:normalize-address address)))
    (handler-case
        (let* ((socket-fd (create-socket const:+af-inet+ const:+sock-stream+ const:+ipproto-tcp+))
               (epoll-instance (epoll:epoll-create1 epoll:+epoll-cloexec+)))
          ;; Set reuse address by default
          (when reuse-addr
            (set-socket-reuse-address socket-fd t))
          
          ;; Bind socket
          (bind-socket socket-fd sock-addr)
          
          ;; Listen
          (core:listen-socket socket-fd backlog)
          
          ;; Set non-blocking
          (core:set-nonblocking socket-fd)
          
          ;; Add to epoll for accept events
          (epoll:epoll-ctl epoll-instance epoll:+epoll-ctl-add+ socket-fd
                           (epoll:make-epoll-event :events epoll:+epollin+ :data socket-fd))
          
          ;; Get actual bound address (in case port was 0)
          (let ((local-addr (get-socket-address socket-fd :local)))
            (types:make-tcp-listener :handle socket-fd
                                     :local-address local-addr
                                     :epoll epoll-instance
                                     :backlog backlog)))
      (error (e)
        (error 'errors:network-error :message (format nil "TCP bind failed: ~A" e))))))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a new incoming connection. Blocks until a connection is available."
  (let ((timeout-ms (if timeout (round (* timeout 1000)) -1)))
    (loop
     (handler-case
         (lib:with-foreign-memory ((addr :char :count 16)
                                   (addrlen :int :count 1))
           (setf (sb-sys:sap-ref-32 addrlen 0) 16)
           (let ((client-fd (const:%accept (types:tcp-listener-handle listener) 
                                           addr addrlen)))
             (cond
               ((>= client-fd 0)
                ;; Success - create the stream
                (core:set-nonblocking client-fd)
                (let ((peer-addr (address:parse-sockaddr-in addr))
                      (local-addr (get-socket-address client-fd :local)))
                  (return (types:make-tcp-stream :handle client-fd
                                                 :local-address local-addr
                                                 :peer-address peer-addr
                                                 :connected-p t))))
               (t
                ;; Check for would-block
                (let ((errno (errors:get-errno)))
                  (if (= errno const:+eagain+)
                      ;; Wait for socket to be ready
                      (let ((events (epoll:wait-for-events 
                                     (types:tcp-listener-epoll listener) 1 timeout-ms)))
                        (unless events
                          (if timeout
                              (error 'errors:timeout-error 
                                     :message "Accept timeout")
                              ;; Continue waiting if no timeout
                              nil)))
                      ;; Other error
                      (errors:check-error client-fd "accept")))))))
       (error (e)
         (unless (typep e 'errors:network-error)
           (error 'errors:network-error :message (format nil "Accept failed: ~A" e)))
         (error e))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (lib:with-foreign-memory ((addr :char :count 16)
                                (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) 16)
        (let ((client-fd (const:%accept (types:tcp-listener-handle listener) 
                                        addr addrlen)))
          (when (>= client-fd 0)
            (core:set-nonblocking client-fd)
            (let ((peer-addr (address:parse-sockaddr-in addr))
                  (local-addr (get-socket-address client-fd :local)))
              (types:make-tcp-stream :handle client-fd
                                     :local-address local-addr
                                     :peer-address peer-addr
                                     :connected-p t)))))
    (errors:would-block-error ()
      nil)
    (error (e)
      (error 'errors:network-error :message (format nil "Try accept failed: ~A" e)))))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (let ((events (epoll:wait-for-events (types:tcp-listener-epoll listener) 1 timeout-ms)))
    (when events
      (tcp-try-accept listener))))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (types:tcp-listener-local-address listener))

(defun tcp-incoming (listener)
  "Return a list of pending incoming connections"
  (loop for conn = (tcp-poll-accept listener 0)
        while conn
        collect conn))

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server with optional timeout"
  (let* ((sock-addr (address:normalize-address address))
         (socket-fd (create-socket const:+af-inet+ const:+sock-stream+ const:+ipproto-tcp+)))
    
    (handler-case
        (progn
          ;; Set to non-blocking mode
          (core:set-nonblocking socket-fd)
          
          ;; Try to connect
          (lib:with-foreign-memory ((sockaddr :char :count 16))
            (address:make-sockaddr-in-into sockaddr 
                                           (types:socket-address-ip sock-addr)
                                           (types:socket-address-port sock-addr))
            (let ((result (const:%connect socket-fd sockaddr 16)))
              (when (< result 0)
                (let ((errno (errors:get-errno)))
                  (if (= errno const:+einprogress+)
                      ;; Async connection in progress
                      (when timeout
                        (let ((epoll-instance (epoll:epoll-create1 0))
                              (timeout-ms (round (* timeout 1000))))
                          (unwind-protect
                              (progn
                                (epoll:epoll-ctl epoll-instance epoll:+epoll-ctl-add+ socket-fd
                                                 (epoll:make-epoll-event :events epoll:+epollout+ 
                                                                         :data socket-fd))
                                (let ((events (epoll:wait-for-events epoll-instance 1 timeout-ms)))
                                  (unless events
                                    (error 'errors:timeout-error :message "Connection timeout"))))
                            (epoll:epoll-close epoll-instance))))
                      ;; Other error
                      (errors:check-error result "connect"))))))
          
          ;; Get local and peer addresses
          (let ((local-addr (get-socket-address socket-fd :local))
                (peer-addr (get-socket-address socket-fd :peer)))
            (types:make-tcp-stream :handle socket-fd
                                   :local-address local-addr
                                   :peer-address peer-addr
                                   :connected-p t)))
      (error (e)
        (close-socket socket-fd)
        (unless (typep e 'errors:network-error)
          (error 'errors:network-error :message (format nil "Connect failed: ~A" e)))
        (error e)))))

(defun tcp-read (stream buffer &key (start 0) (end (length buffer)) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (lib:with-foreign-memory ((buf :char :count (- end start)))
    (let ((result (const:%recv (types:tcp-stream-handle stream) buf (- end start) 0)))
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
                   (let ((timeout-ms (round (* timeout 1000)))
                         (epoll-instance (ensure-socket-epoll stream #'types:tcp-stream-epoll)))
                     (if (wait-for-socket-ready (types:tcp-stream-handle stream) 
                                                epoll-instance epoll:+epollin+ timeout-ms)
                         ;; Retry read
                         (tcp-read stream buffer :start start :end end :timeout nil)
                         0)) ; Timeout
                   0) ; No data, no timeout
               (progn
                 (setf (types:tcp-stream-connected-p stream) nil)
                 (errors:check-error result "recv")
                 0))))))))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream with optional timeout"
  (let* ((buffer (core:normalize-data data))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      ;; Copy data to foreign buffer
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
      
      (let ((result (const:%send (types:tcp-stream-handle stream) buf (- actual-end start) 0)))
        (cond
          ((>= result 0) result)
          (t
           (let ((errno (errors:get-errno)))
             (if (= errno const:+eagain+)
                 (if timeout
                     ;; Wait for writability with timeout
                     (let ((timeout-ms (round (* timeout 1000)))
                           (epoll-instance (ensure-socket-epoll stream #'types:tcp-stream-epoll)))
                       (if (wait-for-socket-ready (types:tcp-stream-handle stream) 
                                                  epoll-instance epoll:+epollout+ timeout-ms)
                           ;; Retry write
                           (tcp-write stream data :start start :end end :timeout nil)
                           0)) ; Timeout
                     0) ; Would block, no timeout
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
               ;; If no bytes were written, brief pause to avoid busy loop
               (sleep 0.001))
          finally (return (- actual-end start)))))

(defun tcp-flush (stream)
  "Flush any buffered output (no-op for direct socket operations)"
  (declare (ignore stream))
  t)

(defun tcp-try-read (stream buffer &key (start 0) (end (length buffer)))
  "Try to read without blocking"
  (tcp-read stream buffer :start start :end end :timeout 0))

(defun tcp-try-write (stream data &key (start 0) (end nil))
  "Try to write without blocking"
  (tcp-write stream data :start start :end end :timeout 0))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (let ((epoll-instance (ensure-socket-epoll stream #'types:tcp-stream-epoll)))
    (wait-for-socket-ready (types:tcp-stream-handle stream) 
                           epoll-instance epoll:+epollin+ timeout-ms)))

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (let ((epoll-instance (ensure-socket-epoll stream #'types:tcp-stream-epoll)))
    (wait-for-socket-ready (types:tcp-stream-handle stream) 
                           epoll-instance epoll:+epollout+ timeout-ms)))

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
    ;; Clean up epoll instance if it exists
    (when (types:tcp-stream-epoll stream)
      (epoll:epoll-close (types:tcp-stream-epoll stream))
      (setf (types:tcp-stream-epoll stream) nil))
    (setf (types:tcp-stream-connected-p stream) nil)))

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

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (types:tcp-stream-connected-p stream))

;;; ============================================================================
;;; UDP Implementation
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (let* ((sock-addr (address:normalize-address address))
         (socket-fd (create-socket const:+af-inet+ const:+sock-dgram+ const:+ipproto-udp+)))
    
    (handler-case
        (progn
          (set-socket-reuse-address socket-fd t)
          
          ;; Bind socket to address
          (bind-socket socket-fd sock-addr)
          
          (core:set-nonblocking socket-fd)
          
          ;; Get actual bound address (in case port was 0)
          (let ((actual-addr (get-socket-address socket-fd :local)))
            (types:make-udp-socket :handle socket-fd
                                   :local-address actual-addr)))
      (error (e)
        (close-socket socket-fd)
        (error 'errors:network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (let ((sock-addr (address:normalize-address address)))
    (lib:with-foreign-memory ((sockaddr :char :count 16))
      (address:make-sockaddr-in-into sockaddr 
                                     (types:socket-address-ip sock-addr)
                                     (types:socket-address-port sock-addr))
      (let ((result (const:%connect (types:udp-socket-handle socket) sockaddr 16)))
        (errors:check-error result "UDP connect")
        (setf (types:udp-socket-connected-peer socket) sock-addr)))))

(defun udp-send (socket data &key (start 0) (end nil))
  "Send data on connected UDP socket"
  (let* ((buffer (core:normalize-data data))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
      ;; Copy data to foreign buffer
      (loop for i from start below actual-end
            for j from 0
            do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
      
      (let ((result (const:%send (types:udp-socket-handle socket) buf (- actual-end start) 0)))
        (errors:check-error result "UDP send")
        result))))

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (lib:with-foreign-memory ((buf :char :count (- end start)))
    (let ((result (const:%recv (types:udp-socket-handle socket) buf (- end start) 0)))
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
               0 ; Would block
               (errors:check-error result "UDP recv"))))))))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address"
  (let* ((sock-addr (address:normalize-address address))
         (buffer (core:normalize-data data))
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
      
      (let ((result (const:%sendto (types:udp-socket-handle socket) buf (- actual-end start) 0
                                   sockaddr 16)))
        (errors:check-error result "UDP sendto")
        result))))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (lib:with-foreign-memory ((buf :char :count (- end start))
                            (sockaddr :char :count 16)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) 16)
    (let ((result (const:%recvfrom (types:udp-socket-handle socket) buf (- end start) 0
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
                 (values 0 nil)))))))))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (types:udp-socket-local-address socket))

(defun udp-try-send (socket data &key (start 0) (end nil))
  "Try to send without blocking"
  (udp-send socket data :start start :end end))

(defun udp-try-recv (socket buffer &key (start 0) (end (length buffer)))
  "Try to receive without blocking"
  (udp-recv socket buffer :start start :end end))

(defun udp-poll-send (socket timeout-ms)
  "Poll for send readiness with timeout"
  (let ((epoll-instance (ensure-socket-epoll socket #'types:udp-socket-epoll)))
    (wait-for-socket-ready (types:udp-socket-handle socket) 
                           epoll-instance epoll:+epollout+ timeout-ms)))

(defun udp-poll-recv (socket timeout-ms)
  "Poll for receive readiness with timeout"
  (let ((epoll-instance (ensure-socket-epoll socket #'types:udp-socket-epoll)))
    (wait-for-socket-ready (types:udp-socket-handle socket) 
                           epoll-instance epoll:+epollin+ timeout-ms)))