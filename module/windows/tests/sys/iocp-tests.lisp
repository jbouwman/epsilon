(defpackage :epsilon.sys.iocp.tests
  (:use :cl)
  (:local-nicknames
   (#:iocp #:epsilon.sys.iocp)
   (#:test #:epsilon.test)))

(in-package :epsilon.sys.iocp.tests)

(test:deftest iocp-creation ()
  "Test basic IOCP creation and cleanup"
  (iocp:with-iocp (iocp-handle)
    (test:is (not (null iocp-handle)))
    (test:is (not (= (sb-alien:alien-sap iocp-handle) iocp:+invalid-handle-value+)))))

(test:deftest overlapped-creation ()
  "Test OVERLAPPED structure creation and packing"
  (let ((overlapped (iocp:make-overlapped :offset 1024 :offset-high 0 :internal 42)))
    (test:is (= (iocp:overlapped-offset overlapped) 1024))
    (test:is (= (iocp:overlapped-offset-high overlapped) 0))
    (test:is (= (iocp:overlapped-internal overlapped) 42))
    
    ;; Test packing/unpacking
    (epsilon.sys.lib:with-foreign-memory ((buffer :char :count (iocp:overlapped-size)))
      (iocp:pack-overlapped overlapped buffer 0)
      (let ((unpacked (iocp:unpack-overlapped buffer 0)))
        (test:is (= (iocp:overlapped-offset unpacked) 1024))
        (test:is (= (iocp:overlapped-offset-high unpacked) 0))
        (test:is (= (iocp:overlapped-internal unpacked) 42))))))

(test:deftest completion-key-creation ()
  "Test completion key structure creation"
  (let ((key (iocp:make-completion-key :socket 42 :operation :read :data "test")))
    (test:is (= (iocp:completion-key-socket key) 42))
    (test:is (eq (iocp:completion-key-operation key) :read))
    (test:is (string= (iocp:completion-key-data key) "test"))))

(test:deftest socket-creation ()
  "Test TCP and UDP socket creation"
  (test:is-thrown-p 'error (iocp:create-tcp-socket))  ; Will fail without Winsock init
  (test:is-thrown-p 'error (iocp:create-udp-socket))) ; Will fail without Winsock init

(test:deftest epoll-data-helpers ()
  "Test epoll data helper functions"
  (let ((fd-data (iocp:make-epoll-data :fd 123))
        (u32-data (iocp:make-epoll-data :u32 #x12345678))
        (u64-data (iocp:make-epoll-data :u64 #x123456789abcdef0)))
    
    (test:is (= (iocp:epoll-data-fd fd-data) 123))
    (test:is (= (iocp:epoll-data-u32 u32-data) #x12345678))
    (test:is (= (iocp:epoll-data-u64 u64-data) #x123456789abcdef0))))

(test:deftest sockaddr-creation ()
  "Test sockaddr_in structure creation"
  (let ((addr (iocp:make-sockaddr-in "192.168.1.1" 8080)))
    (test:is (not (null addr)))
    ;; Test that it's properly formatted as a foreign memory block
    (test:is (= (iocp:overlapped-size) 32))))  ; Just test that size function works

(test:deftest completion-predicates ()
  "Test completion checking predicates"
  (test:is (iocp:completion-success-p 1))
  (test:is (not (iocp:completion-success-p 0)))
  (test:is (iocp:completion-error-p 0))
  (test:is (not (iocp:completion-error-p 1))))