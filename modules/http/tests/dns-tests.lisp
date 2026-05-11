;;;; DNS Resolution Integration Tests
;;;;
;;;; Tests for hostname resolution in make-socket-address.

(defpackage :epsilon.http.dns.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.net net)))

;;; Tests

(deftest test-make-socket-address-ipv4-literal ()
  "Test make-socket-address with IPv4 literal (fast path)"
  (let ((addr (net:make-socket-address "127.0.0.1" 8080)))
    (assert-true addr)
    (assert-equal "127.0.0.1" (net:socket-address-ip addr))
    (assert-equal 8080 (net:socket-address-port addr))
    (assert-equal :ipv4 (net:socket-address-family addr))))

(deftest test-make-socket-address-ipv6-literal ()
  "Test make-socket-address with IPv6 literal (fast path)"
  (let ((addr (net:make-socket-address "::1" 8080)))
    (assert-true addr)
    (assert-equal "::1" (net:socket-address-ip addr))
    (assert-equal 8080 (net:socket-address-port addr))
    (assert-equal :ipv6 (net:socket-address-family addr))))

(deftest test-make-socket-address-localhost ()
  "Test make-socket-address with localhost hostname"
  (let ((addr (net:make-socket-address "localhost" 8080)))
    (assert-true addr)
    (assert-equal 8080 (net:socket-address-port addr))
    ;; Should resolve to 127.0.0.1
    (assert-equal "127.0.0.1" (net:socket-address-ip addr))))

(deftest test-make-socket-address-zero-ip ()
  "Test make-socket-address with 0.0.0.0"
  (let ((addr (net:make-socket-address "0.0.0.0" 80)))
    (assert-true addr)
    (assert-equal "0.0.0.0" (net:socket-address-ip addr))
    (assert-equal :ipv4 (net:socket-address-family addr))))

(deftest test-resolve-address-ipv4 ()
  "Test resolve-address with IPv4 literal"
  (let ((addrs (net:resolve-address "127.0.0.1" 80)))
    (assert-true (listp addrs))
    (assert-true (>= (length addrs) 1))
    (assert-equal "127.0.0.1" (net:socket-address-ip (first addrs)))))

(deftest test-resolve-address-localhost ()
  "Test resolve-address with localhost"
  (let ((addrs (net:resolve-address "localhost" 80)))
    (assert-true (listp addrs))
    (assert-true (>= (length addrs) 1))))
