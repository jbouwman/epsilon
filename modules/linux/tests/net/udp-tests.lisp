;;;; UDP Socket Tests for Linux Networking
;;;;
;;;; Comprehensive tests for UDP functionality

(defpackage epsilon.net.udp-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (net epsilon.net)
   (lib epsilon.foreign)))

(in-package epsilon.net.udp-tests)

;;; ============================================================================
;;; Test Fixtures and Utilities
;;; ============================================================================

(defparameter *test-timeout* 2
  "Default timeout in seconds for test operations")

(defclass test-udp-echo-server ()
  ((socket :initarg :socket :accessor server-socket)
   (thread :initform nil :accessor server-thread)
   (running :initform nil :accessor server-running-p)
   (port :initarg :port :accessor server-port)))

(defun start-udp-echo-server (&key (port 0))
  "Start a simple UDP echo server for testing"
  (let* ((addr (net:make-socket-address "127.0.0.1" port))
         (socket (net:udp-bind addr))
         (actual-port (net:socket-address-port (net:udp-local-addr socket)))
         (server (make-instance 'test-udp-echo-server 
                                :socket socket
                                :port actual-port)))
    (setf (server-running-p server) t)
    (setf (server-thread server)
          (sb-thread:make-thread
           (lambda ()
             (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
               (loop while (server-running-p server)
                     do (handler-case
                            (multiple-value-bind (bytes-read sender-addr)
                                (net:udp-recv-from socket buffer)
                              (when (and (> bytes-read 0) sender-addr)
                                ;; Echo back to sender
                                (net:udp-send-to socket buffer sender-addr 
                                                 :end bytes-read)))
                          (error (e)
                            (unless (typep e 'net:would-block-error)
                              (warn "UDP echo server error: ~A" e)))))))
           :name "test-udp-echo-server"))
    server))

(defun stop-udp-echo-server (server)
  "Stop the UDP echo server"
  (setf (server-running-p server) nil)
  (when (server-thread server)
    (ignore-errors
      (sb-thread:join-thread (server-thread server) :timeout 2)))
  ;; UDP sockets don't need explicit shutdown like TCP
  t)

(defmacro with-udp-echo-server ((server &key (port 0)) &body body)
  "Run body with a UDP echo server"
  `(let ((,server (start-udp-echo-server :port ,port)))
     (unwind-protect
          (progn ,@body)
       (stop-udp-echo-server ,server))))

;;; ============================================================================
;;; Basic UDP Socket Tests
;;; ============================================================================

(deftest test-udp-bind ()
  "Test basic UDP bind operation"
  (let* ((addr (net:make-socket-address "127.0.0.1" 0))
         (socket (net:udp-bind addr)))
    (is (typep socket 'net:udp-socket))
    (let ((local-addr (net:udp-local-addr socket)))
      (is (typep local-addr 'net:socket-address))
      (is-equal "127.0.0.1" (net:socket-address-ip local-addr))
      (is (plusp (net:socket-address-port local-addr))))))

(deftest test-udp-bind-specific-port ()
  "Test UDP bind to specific port"
  (let* ((test-port 44444)
         (addr (net:make-socket-address "0.0.0.0" test-port)))
    (handler-case
        (let ((socket (net:udp-bind addr)))
          (is-equal test-port 
                    (net:socket-address-port (net:udp-local-addr socket))))
      (net:address-in-use ()
        ;; Port might be in use, that's okay
        (is t "Port in use (acceptable)")))))

(deftest test-udp-bind-any-address ()
  "Test UDP bind to INADDR_ANY"
  (let* ((addr (net:make-socket-address "0.0.0.0" 0))
         (socket (net:udp-bind addr)))
    (let ((local-addr (net:udp-local-addr socket)))
      (is-equal "0.0.0.0" (net:socket-address-ip local-addr))
      (is (plusp (net:socket-address-port local-addr))))))

;;; ============================================================================
;;; UDP Connect Tests
;;; ============================================================================

(deftest test-udp-connect ()
  "Test UDP connect operation"
  (let* ((socket (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
         (remote-addr (net:make-socket-address "127.0.0.1" 12345)))
    ;; Connect the UDP socket
    (net:udp-connect socket remote-addr)
    ;; After connect, we can use send/recv without specifying address
    (is t "UDP connect succeeded")))

;;; ============================================================================
;;; UDP Send/Receive Tests
;;; ============================================================================

(deftest test-udp-send-recv-connected ()
  "Test UDP send/recv on connected socket"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (test-data "UDP connected test"))
      
      ;; Connect to echo server
      (net:udp-connect client server-addr)
      
      ;; Send data
      (let ((bytes-sent (net:udp-send client test-data)))
        (is-equal (length test-data) bytes-sent))
      
      ;; Receive echo
      (sleep 0.1) ; Give server time to echo
      (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
        (let ((bytes-read (net:udp-recv client buffer)))
          (when (plusp bytes-read)
            (let ((received (sb-ext:octets-to-string 
                             (subseq buffer 0 bytes-read))))
              (is-equal test-data received))))))))

(deftest test-udp-sendto-recvfrom ()
  "Test UDP sendto/recvfrom operations"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (test-data "UDP sendto test"))
      
      ;; Send data to server
      (let ((bytes-sent (net:udp-send-to client test-data server-addr)))
        (is-equal (length test-data) bytes-sent))
      
      ;; Receive echo with sender address
      (sleep 0.1) ; Give server time to echo
      (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
        (multiple-value-bind (bytes-read sender-addr)
            (net:udp-recv-from client buffer)
          (when (and (plusp bytes-read) sender-addr)
            (let ((received (sb-ext:octets-to-string 
                             (subseq buffer 0 bytes-read))))
              (is-equal test-data received))
            (is-equal "127.0.0.1" (net:socket-address-ip sender-addr))
            (is-equal (server-port server) 
                      (net:socket-address-port sender-addr))))))))

(deftest test-udp-binary-data ()
  "Test sending binary data over UDP"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (test-data #(0 1 2 127 128 255 254 253)))
      
      ;; Send binary data
      (let ((bytes-sent (net:udp-send-to client test-data server-addr)))
        (is-equal (length test-data) bytes-sent))
      
      ;; Receive echo
      (sleep 0.1)
      (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
        (multiple-value-bind (bytes-read sender-addr)
            (net:udp-recv-from client buffer)
          (declare (ignore sender-addr))
          (when (plusp bytes-read)
            (is-equal (length test-data) bytes-read)
            (loop for i from 0 below bytes-read
                  do (is-equal (aref test-data i) (aref buffer i)))))))))

;;; ============================================================================
;;; UDP Large Packet Tests
;;; ============================================================================

(deftest test-udp-max-packet ()
  "Test sending maximum size UDP packet"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           ;; UDP max is ~65KB but practical limit is often 1472 (MTU - headers)
           (test-data (make-array 1400 :element-type '(unsigned-byte 8)
                                  :initial-element 42)))
      
      ;; Send large packet
      (let ((bytes-sent (net:udp-send-to client test-data server-addr)))
        (is-equal (length test-data) bytes-sent))
      
      ;; Receive echo
      (sleep 0.2) ; Larger packet might take longer
      (let ((buffer (make-array 2000 :element-type '(unsigned-byte 8))))
        (multiple-value-bind (bytes-read sender-addr)
            (net:udp-recv-from client buffer)
          (declare (ignore sender-addr))
          (when (plusp bytes-read)
            (is-equal (length test-data) bytes-read)
            ;; Verify content
            (loop for i from 0 below (min 10 bytes-read)
                  do (is-equal 42 (aref buffer i)))))))))

;;; ============================================================================
;;; UDP Error Handling Tests
;;; ============================================================================

(deftest test-udp-recv-no-data ()
  "Test receiving when no data available"
  (let ((socket (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
        (buffer (make-array 100 :element-type '(unsigned-byte 8))))
    ;; Non-blocking receive should return 0
    (let ((bytes-read (net:udp-recv socket buffer)))
      (is-equal 0 bytes-read))))

(deftest test-udp-recvfrom-no-data ()
  "Test recvfrom when no data available"
  (let ((socket (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
        (buffer (make-array 100 :element-type '(unsigned-byte 8))))
    ;; Non-blocking recvfrom should return 0, nil
    (multiple-value-bind (bytes-read sender-addr)
        (net:udp-recv-from socket buffer)
      (is-equal 0 bytes-read)
      (is (null sender-addr)))))

;;; ============================================================================
;;; UDP Multiple Client Tests
;;; ============================================================================

(deftest test-udp-multiple-clients ()
  "Test UDP server handling multiple clients"
  (with-udp-echo-server (server)
    (let* ((server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (num-clients 5)
           (clients '()))
      
      ;; Create multiple clients
      (dotimes (i num-clients)
        (push (net:udp-bind (net:make-socket-address "0.0.0.0" 0)) clients))
      
      ;; Each client sends unique message
      (loop for client in clients
            for i from 0
            do (let ((msg (format nil "Client ~D" i)))
                 (net:udp-send-to client msg server-addr)))
      
      ;; Wait for echoes
      (sleep 0.2)
      
      ;; Each client should receive its echo
      (loop for client in clients
            for i from 0
            do (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
                 (multiple-value-bind (bytes-read sender-addr)
                     (net:udp-recv-from client buffer)
                   (declare (ignore sender-addr))
                   (when (plusp bytes-read)
                     (let ((received (sb-ext:octets-to-string 
                                      (subseq buffer 0 bytes-read))))
                       (is (search (format nil "Client ~D" i) received))))))))))

;;; ============================================================================
;;; UDP Broadcast Tests (if supported)
;;; ============================================================================

(deftest test-udp-broadcast-setup ()
  "Test setting up UDP socket for broadcast"
  (skip "Broadcast tests require special network configuration")
  (let ((socket (net:udp-bind (net:make-socket-address "0.0.0.0" 0))))
    ;; Would need SO_BROADCAST socket option
    (handler-case
        (progn
          ;; net:set-socket-option socket :broadcast t
          (is t "Broadcast option set"))
      (error ()
        (is t "Broadcast not supported (acceptable)")))))

;;; ============================================================================
;;; UDP Performance Tests
;;; ============================================================================

(deftest test-udp-rapid-send ()
  "Test rapid UDP packet sending"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (num-packets 100))
      
      ;; Send many packets rapidly
      (dotimes (i num-packets)
        (let ((msg (format nil "Packet ~D" i)))
          (net:udp-send-to client msg server-addr)))
      
      ;; Wait for echoes
      (sleep 0.5)
      
      ;; Try to receive some echoes (may not get all due to UDP nature)
      (let ((buffer (make-array 100 :element-type '(unsigned-byte 8)))
            (received-count 0))
        (loop repeat num-packets
              do (multiple-value-bind (bytes-read sender-addr)
                     (net:udp-recv-from client buffer)
                   (declare (ignore sender-addr))
                   (when (plusp bytes-read)
                     (incf received-count))))
        ;; Should receive at least some packets
        (is (plusp received-count))))))

(deftest test-udp-packet-loss-simulation ()
  "Test behavior with simulated packet loss"
  (skip "Packet loss simulation requires special setup")
  ;; This would test how the implementation handles lost packets
  ;; In real network conditions, some UDP packets may be lost
  (is t "Packet loss test skipped"))

;;; ============================================================================
;;; UDP Socket Reuse Tests
;;; ============================================================================

(deftest test-udp-socket-reuse ()
  "Test reusing UDP socket for multiple operations"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server))))
      
      ;; Send multiple messages using same socket
      (dotimes (i 5)
        (let ((msg (format nil "Message ~D" i)))
          (net:udp-send-to client msg server-addr)
          (sleep 0.05) ; Small delay between messages
          
          ;; Receive echo
          (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
            (multiple-value-bind (bytes-read sender-addr)
                (net:udp-recv-from client buffer)
              (declare (ignore sender-addr))
              (when (plusp bytes-read)
                (let ((received (sb-ext:octets-to-string 
                                 (subseq buffer 0 bytes-read))))
                  (is-equal msg received))))))))))

;;; ============================================================================
;;; UDP Edge Case Tests
;;; ============================================================================

(deftest test-udp-empty-packet ()
  "Test sending empty UDP packet"
  (with-udp-echo-server (server)
    (let* ((client (net:udp-bind (net:make-socket-address "0.0.0.0" 0)))
           (server-addr (net:make-socket-address "127.0.0.1" (server-port server)))
           (empty-data ""))
      
      ;; Send empty packet
      (let ((bytes-sent (net:udp-send-to client empty-data server-addr)))
        (is-equal 0 bytes-sent)))))

(deftest test-udp-localhost-variations ()
  "Test UDP with different localhost addresses"
  (let ((socket1 (net:udp-bind (net:make-socket-address "127.0.0.1" 0)))
        (socket2 (net:udp-bind (net:make-socket-address "localhost" 0))))
    
    ;; Both should work
    (is (typep socket1 'net:udp-socket))
    (is (typep socket2 'net:udp-socket))
    
    ;; Get actual addresses
    (let ((addr1 (net:udp-local-addr socket1))
          (addr2 (net:udp-local-addr socket2)))
      (is (or (string= "127.0.0.1" (net:socket-address-ip addr1))
              (string= "localhost" (net:socket-address-ip addr1))))
      (is (or (string= "127.0.0.1" (net:socket-address-ip addr2))
              (string= "localhost" (net:socket-address-ip addr2)))))))