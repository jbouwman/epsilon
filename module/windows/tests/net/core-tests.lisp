(defpackage :epsilon.net.tests
  (:use :cl)
  (:local-nicknames
   (#:net #:epsilon.net)
   (#:test #:epsilon.tool.test)))

(in-package :epsilon.net.tests)

(test:deftest address-parsing ()
  "Test address parsing functionality"
  (let ((addr1 (net:parse-address "127.0.0.1:8080"))
        (addr2 (net:parse-address "localhost")))
    (test:is (string= (net:address-host addr1) "127.0.0.1"))
    (test:is (= (net:address-port addr1) 8080))
    (test:is (string= (net:address-host addr2) "localhost"))
    (test:is (= (net:address-port addr2) 80))))

(test:deftest socket-creation ()
  "Test socket creation (will fail on non-Windows)"
  (test:is-thrown-p 'net:network-error 
    (net:socket-listen "127.0.0.1" 0)))

(test:deftest secure-context-creation ()
  "Test TLS context creation"
  (let ((ctx (net:make-secure-context :verify nil)))
    (test:is (not (null ctx)))
    (test:is (typep ctx 'net:secure-context))))

(test:deftest future-operations ()
  "Test future/await pattern"
  (let ((future (net:future :test :iocp nil :completion-key nil)))
    (test:is (not (null future)))
    (test:is (typep future 'net:future))
    (test:is (not (net:future-completed-p future)))
    
    ;; Test completing a future manually
    (setf (net:future-completed-p future) t)
    (setf (net:future-result future) "test-result")
    (test:is (string= (net:await future) "test-result"))))

(test:deftest socket-stream-wrapper ()
  "Test socket stream wrapper"
  (let* ((mock-socket (make-instance 'net:socket :handle 123))
         (wrapper (make-instance 'net:socket-stream-wrapper :socket mock-socket)))
    (test:is (not (null wrapper)))
    (test:is (typep wrapper 'net:socket-stream-wrapper))
    (test:is (eq (net:socket-from-stream wrapper) mock-socket))))

(test:deftest async-operations-stubs ()
  "Test async operations return errors appropriately"
  (let ((mock-socket (make-instance 'net:socket :handle 123))
        (buffer (make-array 10 :element-type '(unsigned-byte 8))))
    
    ;; These should all complete without throwing immediately
    ;; (though they may error during IOCP operations)
    (test:is-thrown-p 'net:network-error
      (net:async-read mock-socket buffer))
    (test:is-thrown-p 'net:network-error  
      (net:async-write mock-socket buffer))))