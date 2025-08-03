(defpackage :epsilon.net.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:net #:epsilon.net)))

(in-package :epsilon.net.tests)

(deftest test-socket-creation ()
  "Test basic socket creation"
  (let ((socket (net:socket)))
    (is (typep socket 'net:socket))
    (is-equal :stream (net:socket-type socket))
    (is-not (net:socket-connected-p socket))
    (net:close socket)))

(deftest test-socket-address ()
  "Test socket address creation"
  (let ((addr (net:make-socket-address "localhost" 8080)))
    (is-equal "localhost" (net:net-address-host addr))
    (is-equal 8080 (net:net-address-port addr))))

(deftest test-socket-options ()
  "Test setting socket options"
  (let ((socket (net:socket)))
    (net:set-socket-option socket net:+socket-option-reuseaddr+ t)
    (net:set-socket-option socket net:+socket-option-keepalive+ t)
    (net:close socket)))

(deftest test-hostname-resolution ()
  "Test hostname resolution"
  ;; localhost should always resolve
  (let ((addr (net:resolve-hostname "localhost")))
    (is (not (null addr)))))

(deftest test-bind-and-listen ()
  "Test binding and listening"
  (let ((socket (net:socket))
        (addr (net:make-socket-address "127.0.0.1" 0))) ; Use port 0 for auto-assign
    (net:set-socket-option socket net:+socket-option-reuseaddr+ t)
    (net:bind socket addr)
    (net:listen socket)
    (net:close socket)))