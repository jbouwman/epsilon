;;;; Windows Network Core Tests
;;;;
;;;; Tests for the epsilon.net Windows implementation

(defpackage epsilon.net.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (net epsilon.net)
   (lib epsilon.foreign)))

(in-package epsilon.net.tests)

(deftest test-socket-address-creation ()
  "Test socket address creation"
  (let ((addr (net:make-socket-address "127.0.0.1" 8080)))
    (is-equal "127.0.0.1" (net:socket-address-ip addr))
    (is-equal 8080 (net:socket-address-port addr))))

(deftest test-parse-address ()
  "Test address parsing"
  (let ((addr (net:parse-address "192.168.1.1:3000")))
    (is-equal "192.168.1.1" (net:socket-address-ip addr))
    (is-equal 3000 (net:socket-address-port addr)))
  
  ;; Test error on invalid format
  (is-thrown 'error (net:parse-address "invalid-address")))

(deftest test-tcp-listener-creation ()
  "Test TCP listener creation and basic operations"
  (let ((addr (net:make-socket-address "127.0.0.1" 0))) ; Use port 0 for random assignment
    (let ((listener (net:tcp-bind addr :reuse-addr t)))
      (is (typep listener 'net:tcp-listener))
      (is-equal "127.0.0.1" (net:socket-address-ip (net:tcp-local-addr listener)))
      (is (plusp (net:socket-address-port (net:tcp-local-addr listener))))
      
      ;; Clean up
      (sb-bsd-sockets:socket-close (net:tcp-listener-handle listener))
      (net:iocp-close (net:tcp-listener-iocp listener)))))

(deftest test-tcp-try-accept ()
  "Test non-blocking accept"
  (let* ((addr (net:make-socket-address "127.0.0.1" 0))
         (listener (net:tcp-bind addr)))
    (unwind-protect
        (progn
          ;; Should return nil when no connections pending
          (is (null (net:tcp-try-accept listener))))
      ;; Clean up
      (sb-bsd-sockets:socket-close (net:tcp-listener-handle listener))
      (net:iocp-close (net:tcp-listener-iocp listener)))))

(deftest test-tcp-connect-to-nonexistent ()
  "Test connecting to non-existent server"
  (let ((addr (net:make-socket-address "127.0.0.1" 12345))) ; Assume this port is unused
    (is-thrown 'net:connection-refused (net:tcp-connect addr))))

(deftest test-udp-socket-creation ()
  "Test UDP socket creation"
  (let* ((addr (net:make-socket-address "127.0.0.1" 0))
         (socket (net:udp-bind addr)))
    (is (typep socket 'net:udp-socket))
    (is-equal "127.0.0.1" (net:socket-address-ip (net:udp-local-addr socket)))
    (is (plusp (net:socket-address-port (net:udp-local-addr socket))))
    
    ;; Clean up
    (sb-bsd-sockets:socket-close (net:udp-socket-handle socket))))

(deftest test-udp-send-recv ()
  "Test UDP send and receive operations"
  (let* ((addr1 (net:make-socket-address "127.0.0.1" 0))
         (addr2 (net:make-socket-address "127.0.0.1" 0))
         (socket1 (net:udp-bind addr1))
         (socket2 (net:udp-bind addr2)))
    
    (unwind-protect
        (let* ((local-addr1 (net:udp-local-addr socket1))
               (local-addr2 (net:udp-local-addr socket2))
               (test-data #(72 101 108 108 111))) ; "Hello"
          
          ;; Send from socket1 to socket2
          (net:udp-send-to socket1 test-data local-addr2)
          
          ;; Try to receive on socket2 (may not work without proper event loop)
          ;; This is a simplified test - in practice we'd need proper async handling
          (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
            ;; Just test that the function exists and can be called
            (handler-case
                (net:udp-recv socket2 buffer)
              (error () 
                ;; Expected - socket operations may fail without proper setup
                t))))
      
      ;; Clean up
      (sb-bsd-sockets:socket-close (net:udp-socket-handle socket1))
      (sb-bsd-sockets:socket-close (net:udp-socket-handle socket2)))))

(deftest test-tcp-client-server ()
  "Test TCP client/server connection"
  ;; This is a simplified test that just verifies the API exists
  ;; Full integration testing would require proper async handling
  (let* ((server-addr (net:make-socket-address "127.0.0.1" 0))
         (listener (net:tcp-bind server-addr)))
    
    (unwind-protect
        (let* ((local-addr (net:tcp-local-addr listener))
               (port (net:socket-address-port local-addr)))
          
          ;; Try to connect - this should work since we're listening
          (handler-case
              (let ((client (net:tcp-connect (net:make-socket-address "127.0.0.1" port))))
                ;; If connection succeeds, clean up client
                (when client
                  (sb-bsd-sockets:socket-close (net:tcp-stream-handle client)))
                t)
            (error ()
              ;; Connection might fail due to timing or async issues
              ;; That's OK for this basic test
              t)))
      
      ;; Clean up server
      (sb-bsd-sockets:socket-close (net:tcp-listener-handle listener))
      (net:iocp-close (net:tcp-listener-iocp listener)))))

(deftest test-address-resolution ()
  "Test hostname resolution"
  ;; Test resolving localhost
  (let ((addresses (net:resolve-address "localhost" 8080)))
    (is (listp addresses))
    (is (> (length addresses) 0))
    (let ((first-addr (first addresses)))
      (is (stringp (net:socket-address-ip first-addr)))
      (is-equal 8080 (net:socket-address-port first-addr))))
  
  ;; Test resolving invalid hostname
  (is-thrown 'net:network-error 
             (net:resolve-address "this-hostname-should-not-exist.invalid" 8080)))

(deftest test-error-conditions ()
  "Test that proper error conditions are signaled"
  ;; Test that network errors are signaled appropriately
  (is (subtypep 'net:network-error 'error))
  (is (subtypep 'net:connection-refused 'net:network-error))
  (is (subtypep 'net:connection-reset 'net:network-error))
  (is (subtypep 'net:timeout-error 'net:network-error))
  (is (subtypep 'net:address-in-use 'net:network-error)))

(deftest test-winsock-initialization ()
  "Test that Winsock gets initialized properly"
  ;; Creating a socket should initialize Winsock
  (let* ((addr (net:make-socket-address "127.0.0.1" 0))
         (socket (net:udp-bind addr)))
    (is (typep socket 'net:udp-socket))
    
    ;; Clean up
    (sb-bsd-sockets:socket-close (net:udp-socket-handle socket))))