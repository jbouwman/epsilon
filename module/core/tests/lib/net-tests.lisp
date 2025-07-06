(defpackage epsilon.lib.net.tests
  (:use
   cl
   epsilon.tool.test)
  (:local-nicknames
   (net epsilon.lib.net)))

(in-package :epsilon.lib.net.tests)

(deftest address-parsing
  "Test address parsing functionality"
  (multiple-value-bind (host port) (net:parse-address "example.com:8080")
    (is (string= host "example.com"))
    (is (= port 8080)))
  
  (multiple-value-bind (host port) (net:parse-address "localhost")
    (is (string= host "localhost"))
    (is (= port 80))))

(deftest hostname-resolution
  "Test hostname resolution"
  ;; Test localhost resolution
  (let ((localhost-ip (net:resolve-hostname "localhost")))
    (is (equalp localhost-ip #(127 0 0 1))))
  
  ;; Test 0.0.0.0 resolution
  (let ((any-ip (net:resolve-hostname "0.0.0.0")))
    (is (equalp any-ip #(0 0 0 0))))
  
  ;; Test IP address passthrough
  (let ((ip (net:resolve-hostname "192.168.1.1")))
    (is (equalp ip #(192 168 1 1)))))

(deftest socket-creation
  "Test basic socket creation"
  ;; Test TCP socket creation
  (let ((tcp-socket (net:make-tcp-socket)))
    (is (not (null tcp-socket)))
    (net:close-socket tcp-socket))
  
  ;; Test UDP socket creation  
  (let ((udp-socket (net:make-udp-socket)))
    (is (not (null udp-socket)))
    (net:close-socket udp-socket)))

(deftest socket-options
  "Test socket option getting and setting"
  (let ((socket (net:make-tcp-socket)))
    (unwind-protect
         (progn
           ;; Test setting and getting reuse-address
           (net:set-socket-option socket :reuse-address t)
           (is (net:get-socket-option socket :reuse-address))
           
           (net:set-socket-option socket :reuse-address nil)
           (is (not (net:get-socket-option socket :reuse-address))))
      (net:close-socket socket))))

(deftest listener-creation
  "Test TCP listener creation"
  ;; Use a high port number to avoid conflicts
  (let ((port 12345))
    (let ((listener (net:make-listener port :address "127.0.0.1")))
      (is (not (null listener)))
      (net:close-socket listener))))

(deftest connection-test
  "Test basic TCP connection (requires listener)"
  ;; This test creates a listener and connects to it
  (let ((port 12346))
    (let ((listener (net:make-listener port :address "127.0.0.1")))
      (unwind-protect
           (progn
             ;; Connect to the listener
             (let ((client (net:connect-tcp "127.0.0.1" port)))
               (unwind-protect
                    (progn
                      (is (not (null client)))
                      
                      ;; Accept the connection
                      (let ((server-side (net:accept-connection listener)))
                        (unwind-protect
                             (is (not (null server-side)))
                          (when server-side
                            (net:close-socket server-side)))))
                 (when client
                   (net:close-socket client)))))
        (when listener
          (net:close-socket listener))))))

(deftest stream-operations
  "Test socket stream creation and basic I/O"
  (let ((port 12347))
    (let ((listener (net:make-listener port :address "127.0.0.1")))
      (unwind-protect
           (progn
             ;; Connect to the listener  
             (let ((client (net:connect-tcp "127.0.0.1" port)))
               (unwind-protect
                    (progn
                      ;; Accept the connection
                      (let ((server-side (net:accept-connection listener)))
                        (unwind-protect
                             (progn
                               ;; Create streams
                               (let ((client-stream (net:socket-stream client))
                                     (server-stream (net:socket-stream server-side)))
                                 
                                 ;; Test basic I/O
                                 (write-line "Hello from client" client-stream)
                                 (force-output client-stream)
                                 
                                 (let ((received (read-line server-stream)))
                                   (is (string= received "Hello from client")))))
                          (when server-side
                            (net:close-socket server-side)))))
                 (when client
                   (net:close-socket client)))))
        (when listener
          (net:close-socket listener))))))

(deftest with-connection-macro
  "Test with-tcp-connection macro"
  (let ((port 12348))
    (let ((listener (net:make-listener port :address "127.0.0.1")))
      (unwind-protect
           (progn
             (let ((connection-worked nil))
               (net:with-tcp-connection (client "127.0.0.1" port)
                 (setf connection-worked (not (null client))))
               (is connection-worked)))
        (when listener
          (net:close-socket listener))))))

(deftest with-listener-macro 
  "Test with-tcp-listener macro"
  (let ((port 12349))
    (let ((listener-worked nil))
      (net:with-tcp-listener (listener port :address "127.0.0.1")
        (setf listener-worked (not (null listener))))
      (is listener-worked))))

(deftest error-handling
  "Test error handling for network operations"
  ;; Test connection to non-existent host
  (is-thrown (net:connection-error)
    (net:connect-tcp "999.999.999.999" 80))
  
  ;; Test binding to invalid port (port 0 should work, so use negative)
  (is-thrown (net:network-error)
    (net:make-listener -1)))
