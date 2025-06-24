(defpackage :epsilon.sys.iocp.tests
  (:use :cl)
  (:local-nicknames
   (#:iocp #:epsilon.sys.iocp)
   (#:test #:epsilon.tool.test)))

(in-package :epsilon.sys.iocp.tests)

(test:deftest iocp-creation ()
  "Test basic IOCP creation and cleanup"
  (iocp:with-iocp (iocp-handle)
    (test:is (not (null iocp-handle)))
    (test:is (not (= (sb-alien:alien-sap iocp-handle) iocp:+invalid-handle-value+)))))

(test:deftest overlapped-creation ()
  "Test OVERLAPPED structure creation"
  (let ((overlapped (iocp:make-overlapped :offset 1024 :offset-high 0)))
    (test:is (= (iocp:overlapped-offset overlapped) 1024))
    (test:is (= (iocp:overlapped-offset-high overlapped) 0))))

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