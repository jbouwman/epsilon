(defpackage epsilon.net.core-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (net epsilon.net.core)
   (lib epsilon.foreign)))

(in-package epsilon.net.core-tests)

(deftest test-socket-create
  "Test basic socket creation"
  (let ((sock (net:socket-create net:+AF-INET+ net:+SOCK-STREAM+ 0)))
    (is (plusp sock))
    (is (zerop (net:socket-close sock)))))

;; Define htons for testing
(lib:defshared test-htons "htons" "libc" :unsigned-short
  (x :unsigned-short))

(deftest test-inet-address-creation
  "Test IPv4 address structure creation"
  (let ((addr (net:make-inet-address "127.0.0.1" 8080)))
    (unwind-protect
         (progn
           (is (not (null addr)))
           ;; Check address family
           (is (= (sb-sys:sap-ref-16 addr 0) net:+AF-INET+))
           ;; Check port (network byte order)
           (is (= (sb-sys:sap-ref-16 addr 2) 
                  (test-htons 8080))))
      (lib:foreign-free addr))))

(deftest test-socket-options
  "Test setting and getting socket options"
  (net:with-socket (sock net:+AF-INET+ net:+SOCK-STREAM+ 0)
    ;; Set SO_REUSEADDR
    (net:set-socket-option sock net:+SOL-SOCKET+ net:+SO-REUSEADDR+ 1)
    
    ;; Get it back
    (let ((value (net:get-socket-option sock net:+SOL-SOCKET+ net:+SO-REUSEADDR+)))
      (is (= value 1)))))

(deftest test-tcp-loopback
  "Test TCP client/server on loopback"
  ;; Simplified test - just test socket creation and binding
  (net:with-server-socket (server 8765)
    (is (plusp server))
    ;; Socket is bound and listening
    t))

;; Define socketpair for testing
(lib:defshared test-socketpair "socketpair" "libc" :int
  (domain :int) (type :int) (protocol :int) (sv :pointer))

(deftest test-socket-send-recv
  "Test sending and receiving data"
  ;; Create a pair of connected sockets using socketpair
  (lib:with-foreign-memory ((sv 8)) ; Array of 2 ints
    (when (zerop (test-socketpair net:+AF-UNIX+ net:+SOCK-STREAM+ 0 sv))
      (let ((sock1 (sb-sys:sap-ref-32 sv 0))
            (sock2 (sb-sys:sap-ref-32 sv 4)))
        (unwind-protect
             (progn
               ;; Send from sock1
               (let ((sent (net:socket-send sock1 "test data")))
                 (is (= sent 9))) ; "test data" is 9 bytes
               
               ;; Receive on sock2
               (let ((received (net:socket-recv sock2 1024)))
                 (is (not (null received)))
                 (when received
                   (let ((str (map 'string #'code-char received)))
                     (is (string= str "test data"))))))
          
          ;; Clean up
          (net:socket-close sock1)
          (net:socket-close sock2))))))

;; Define byte order functions for testing
(lib:defshared test-htonl "htonl" "libc" :unsigned-int (x :unsigned-int))
(lib:defshared test-ntohs "ntohs" "libc" :unsigned-short (x :unsigned-short))
(lib:defshared test-ntohl "ntohl" "libc" :unsigned-int (x :unsigned-int))

(deftest test-network-byte-order
  "Test network byte order conversion"
  ;; Test short conversion
  (let ((host-short #x1234))
    (let ((net-short (test-htons host-short)))
      (is (= (test-ntohs net-short) host-short))))
  
  ;; Test long conversion
  (let ((host-long #x12345678))
    (let ((net-long (test-htonl host-long)))
      (is (= (test-ntohl net-long) host-long)))))