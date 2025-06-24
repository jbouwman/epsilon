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