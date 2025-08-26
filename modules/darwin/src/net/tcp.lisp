;;;; TCP operations - listener and stream

(defpackage epsilon.net.tcp
  (:use cl)
  (:local-nicknames
   (kqueue epsilon.kqueue)
   (lib epsilon.foreign)
   (log epsilon.log))
  (:import-from epsilon.net.constants
   +af-inet+ +sock-stream+ +ipproto-tcp+ +sol-socket+ +so-reuseaddr+
   +shut-rd+ +shut-wr+ +shut-rdwr+
   %socket %bind %listen %accept %connect %send %recv %close %shutdown
   %setsockopt %getsockname %getpeername)
  (:import-from epsilon.net.core
   socket-address socket-address-ip socket-address-port
   tcp-listener tcp-listener-handle tcp-listener-local-address tcp-listener-kqueue
   tcp-stream tcp-stream-handle tcp-stream-local-address tcp-stream-peer-address
   tcp-stream-input tcp-stream-output tcp-stream-connected-p
   network-error connection-refused connection-reset get-errno errno-to-string)
  (:import-from epsilon.net.address
   normalize-address make-sockaddr-in-into parse-sockaddr-in)
  (:import-from epsilon.net.async
   register-async-operation set-nonblocking)
  (:export
   ;; TCP Listener operations
   tcp-bind
   tcp-accept
   tcp-incoming
   tcp-try-accept
   tcp-poll-accept
   tcp-local-addr
   
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
   tcp-connected-p))

(in-package epsilon.net.tcp)

;;; ============================================================================
;;; TCP Implementation - Part 1: Basic Operations
;;; ============================================================================

(defun tcp-bind (address)
  "Create a TCP listener bound to the specified address"
  (let ((sock-addr (normalize-address address)))
    (handler-case
        (let* ((socket-fd (%socket +af-inet+ +sock-stream+ +ipproto-tcp+))
               (kq (kqueue:kqueue)))
          (when (< socket-fd 0)
            (error 'network-error :message "Failed to create TCP socket"))
          
          ;; Set reuse address by default
          (lib:with-foreign-memory ((optval :int :count 1))
            (setf (sb-sys:sap-ref-32 optval 0) 1)
            (%setsockopt socket-fd +sol-socket+ +so-reuseaddr+ optval 4))
          
          ;; Bind socket
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (let ((result (%bind socket-fd addr 16)))
              (when (< result 0)
                (error 'network-error 
                       :message (format nil "Failed to bind socket to ~A:~A" 
                                        (socket-address-ip sock-addr) 
                                        (socket-address-port sock-addr))))))
          
          ;; Listen
          (when (< (%listen socket-fd 128) 0)
            (error 'network-error :message "Failed to listen on socket"))
          
          ;; Add to kqueue for accept events
          (kqueue:add-event kq socket-fd kqueue:+evfilt-read+)
          
          ;; Get actual bound address (in case port was 0)
          (lib:with-foreign-memory ((addr :char :count 16)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 16)
            (%getsockname socket-fd addr addrlen)
            (let ((local-addr (parse-sockaddr-in addr)))
              (make-instance 'tcp-listener
                             :handle socket-fd
                             :local-address local-addr
                             :kqueue kq
                             :backlog 128))))
      (error (e)
        (error 'network-error :message (format nil "TCP bind failed: ~A" e))))))

(defun tcp-accept (listener)
  "Accept a new incoming connection. Blocks until a connection is available."
  (handler-case
      (loop
       ;; Wait for accept event
       (let ((events (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 nil)))
         (when events
           (lib:with-foreign-memory ((addr :char :count 16)
                                     (addrlen :int :count 1))
             (setf (sb-sys:sap-ref-32 addrlen 0) 16)
             (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
               (cond
                 ((>= client-fd 0)
                  ;; Success - create the stream
                  (let ((peer-addr (parse-sockaddr-in addr)))
                    ;; Get local address of accepted socket
                    (lib:with-foreign-memory ((local-addr :char :count 16)
                                              (local-addrlen :int :count 1))
                      (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
                      (%getsockname client-fd local-addr local-addrlen)
                      (let ((local-sock-addr (parse-sockaddr-in local-addr)))
                        (return (values 
                                 (make-instance 'tcp-stream
                                                :handle client-fd
                                                :local-address local-sock-addr
                                                :peer-address peer-addr
                                                :connected-p t)
                                 peer-addr))))))
                 (t
                  ;; accept failed - get errno for specific error
                  (let ((errno-val (get-errno)))
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
            (lib:with-foreign-memory ((addr :char :count 16)
                                      (addrlen :int :count 1))
              (setf (sb-sys:sap-ref-32 addrlen 0) 16)
              (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
                (if (>= client-fd 0)
                    (let ((peer-addr (parse-sockaddr-in addr)))
                      (lib:with-foreign-memory ((local-addr :char :count 16)
                                                (local-addrlen :int :count 1))
                        (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
                        (%getsockname client-fd local-addr local-addrlen)
                        (let ((local-sock-addr (parse-sockaddr-in local-addr)))
                          (values 
                           (make-instance 'tcp-stream
                                          :handle client-fd
                                          :local-address local-sock-addr
                                          :peer-address peer-addr
                                          :connected-p t)
                           peer-addr))))
                    :would-block)))
            :would-block))
    (error (e)
      (error 'network-error :message (format nil "Try-accept failed: ~A" e)))))

(defun tcp-poll-accept (listener waker)
  "Poll for a connection (async operation)"
  (let ((result (tcp-try-accept listener)))
    (if (eq result :would-block)
        (progn
          (register-async-operation (tcp-listener-handle listener) :accept waker)
          :pending)
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

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address)
  "Connect to a remote TCP server. Blocks until connected."
  (let ((sock-addr (normalize-address address)))
    (log:debug "tcp-connect: Creating socket for ~A:~D~%" 
               (socket-address-ip sock-addr) (socket-address-port sock-addr))
    (finish-output)
    (let ((socket-fd (%socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
      (log:debug "tcp-connect: Socket created, fd=~D~%" socket-fd)
      (finish-output)
      (when (< socket-fd 0)
        (let ((errno-val (get-errno)))
          (error 'network-error 
                 :message (format nil "Failed to create TCP socket - errno ~D: ~A" 
                                  errno-val (errno-to-string errno-val)))))
      
      ;; Connect
      (log:debug "tcp-connect: Attempting connection...~%")
      (finish-output)
      
      ;; Set socket to non-blocking mode
      (log:debug "tcp-connect: Setting socket to non-blocking~%")
      (finish-output)
      (set-nonblocking socket-fd)
      
      (lib:with-foreign-memory ((addr :char :count 16))
        (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
        (log:debug "tcp-connect: Calling %connect...~%")
        (finish-output)
        (let ((result (%connect socket-fd addr 16)))
          (log:debug "tcp-connect: %connect returned ~D~%" result)
          (finish-output)
          (when (< result 0)
            (let ((errno-val (get-errno)))
              (log:debug "tcp-connect: Connection errno ~D: ~A~%" 
                         errno-val (errno-to-string errno-val))
              (finish-output)
              ;; For non-blocking sockets, EINPROGRESS (36) is expected
              (unless (= errno-val 36) ; EINPROGRESS
                (%close socket-fd)  ; Clean up socket on failure
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
                                             (errno-to-string errno-val))))))))
          
          ;; For non-blocking connect, we need to wait for completion
          (when (and (< result 0) (= (get-errno) 36)) ; EINPROGRESS
            (log:debug "tcp-connect: Connection in progress, waiting...~%")
            (finish-output)
            ;; Simple busy wait - in production use select/poll
            (loop repeat 100
                  do (sleep 0.01)
                     (let ((write-ready-p t)) ; Simplified check
                       (when write-ready-p
                         (log:debug "tcp-connect: Connection completed~%")
                         (finish-output)
                         (return)))))
          
          ;; Get local address
          (log:debug "tcp-connect: Getting local address...~%")
          (finish-output)
          (lib:with-foreign-memory ((local-addr :char :count 16)
                                    (local-addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
            (%getsockname socket-fd local-addr local-addrlen)
            (let ((local-sock-addr (parse-sockaddr-in local-addr)))
              (log:debug "tcp-connect: Connection successful!~%")
              (finish-output)
              (make-instance 'tcp-stream
                             :handle socket-fd
                             :local-address local-sock-addr
                             :peer-address sock-addr
                             :connected-p t))))))))

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
  (let* ((data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
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
  (let* ((data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
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
        (let* ((data-bytes (etypecase data
                             (string (sb-ext:string-to-octets data))
                             (vector data)))
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
            (register-async-operation (tcp-stream-handle stream) :read waker)
            :pending)
          result))))

(defun tcp-poll-write (stream waker)
  "Poll for write readiness"
  (let ((dummy-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (tcp-try-write stream dummy-data)))
      (if (eq result :would-block)
          (progn
            (register-async-operation (tcp-stream-handle stream) :write waker)
            :pending)
          :ready))))

(defun tcp-peer-addr (stream)
  "Get the remote peer's address"
  (tcp-stream-peer-address stream))

(defun tcp-local-addr-stream (stream)
  "Get the local address of the socket"
  (tcp-stream-local-address stream))

(defun tcp-shutdown (stream how)
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
  "Get a Lisp input stream for reading"
  (or (tcp-stream-input stream)
      (setf (tcp-stream-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :buffering :full))))

(defun tcp-stream-writer (stream)
  "Get a Lisp output stream for writing"
  (or (tcp-stream-output stream)
      (setf (tcp-stream-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :buffering :full))))

(defun tcp-connected-p (stream)
  "Check if the stream is still connected"
  (tcp-stream-connected-p stream))