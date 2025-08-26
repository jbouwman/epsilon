;;;; UDP socket operations

(defpackage epsilon.net.udp
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign)
   (log epsilon.log))
  (:import-from epsilon.net.constants
   +af-inet+ +sock-dgram+ +ipproto-udp+
   %socket %bind %connect %sendto %recvfrom %getsockname)
  (:import-from epsilon.net.core
   socket-address socket-address-ip socket-address-port
   udp-socket udp-socket-handle udp-socket-local-address udp-socket-connected-peer
   network-error address-in-use would-block-error get-errno)
  (:import-from epsilon.net.address
   normalize-address make-sockaddr-in-into parse-sockaddr-in make-socket-address)
  (:import-from epsilon.net.async
   register-async-operation set-nonblocking)
  (:export
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

(in-package epsilon.net.udp)

;;; ============================================================================
;;; UDP Implementation
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to an address"
  (let ((sock-addr (normalize-address address)))
    (handler-case
        (let ((socket-fd (%socket +af-inet+ +sock-dgram+ +ipproto-udp+)))
          (when (< socket-fd 0)
            (error 'network-error :message "Failed to create UDP socket"))
          
          ;; Set socket to non-blocking mode
          (set-nonblocking socket-fd)
          
          ;; Bind socket
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (when (< (%bind socket-fd addr 16) 0)
              (error 'address-in-use :message "Failed to bind UDP socket")))
          
          ;; Get actual bound address
          (lib:with-foreign-memory ((addr :char :count 16)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 16)
            (%getsockname socket-fd addr addrlen)
            (let ((local-addr (parse-sockaddr-in addr)))
              (make-instance 'udp-socket
                             :handle socket-fd
                             :local-address local-addr))))
      (error (e)
        (error 'network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to a default peer"
  (let ((sock-addr (normalize-address address)))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (when (< (%connect (udp-socket-handle socket) addr 16) 0)
              (error 'network-error :message "UDP connect failed")))
          (setf (udp-socket-connected-peer socket) sock-addr))
      (error (e)
        (error 'network-error :message (format nil "UDP connect failed: ~A" e))))))

(defun udp-send (socket data address)
  "Send data to a specific address"
  (let* ((sock-addr (normalize-address address))
         (data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
         (count (length data-bytes)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count)
                                  (addr :char :count 16))
          ;; Copy data to foreign buffer
          (loop for i from 0 below count
                do (setf (sb-sys:sap-ref-8 buf i)
                         (aref data-bytes i)))
          ;; Set up address
          (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
          (log:debug "udp-send: Calling %sendto with fd=~D, count=~D~%" (udp-socket-handle socket) count)
          (finish-output)
          (let ((bytes-sent (%sendto (udp-socket-handle socket) buf count 0 addr 16)))
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
      (lib:with-foreign-memory ((buf :char :count (length buffer))
                                (addr :char :count 16)
                                (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) 16)
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
             (let ((sender-addr (parse-sockaddr-in addr)))
               (values bytes-read sender-addr)))
            ((and (< bytes-read 0) (= (get-errno) 35)) ; EAGAIN - would block
             (values 0 nil))  ; No data available
            (t
             (error 'network-error 
                    :message (format nil "UDP recv failed: errno ~D" (get-errno)))))))
    (error (e)
      (error 'network-error :message (format nil "UDP recv failed: ~A" e)))))

(defun udp-send-to (socket data address)
  "Alias for udp-send for compatibility"
  (udp-send socket data address))

(defun udp-recv-from (socket buffer)
  "Alias for udp-recv for compatibility"
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
            (register-async-operation (udp-socket-handle socket) :write waker)
            :pending)
          :ready))))

(defun udp-poll-recv (socket waker)
  "Poll for UDP read readiness (async operation)"  
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
    (let ((result (udp-try-recv socket buffer)))
      (if (eq result :would-block)
          (progn
            (register-async-operation (udp-socket-handle socket) :read waker)
            :pending)
          result))))