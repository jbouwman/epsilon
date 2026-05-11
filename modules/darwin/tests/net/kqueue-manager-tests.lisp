;;;; Kqueue Manager Tests (Darwin)
;;;;
;;;; Mirrors epsilon/modules/linux/tests/net/core-tests.lisp coverage
;;;; for the reactor-based kqueue manager.  Verifies registration
;;;; lifecycle, callback dispatch, and any-queue delivery.

(defpackage epsilon.net.reactor.tests
  (:use cl epsilon.test)
  (:import
   (epsilon.net net)
   (epsilon.net.core net-core)
   (epsilon.net.reactor reactor)
   (epsilon.kqueue kqueue)
   (epsilon.sys.lock lock)))

;;; ============================================================================
;;; Lifecycle
;;; ============================================================================

(deftest test-reactor-singleton
  "boot-reactor returns the same instance across calls."
  (let ((m1 (reactor:boot-reactor))
        (m2 (reactor:boot-reactor)))
    (assert-not-null m1)
    (assert-eq m1 m2)))

;;; ============================================================================
;;; Socket Registration
;;; ============================================================================

(deftest test-register-and-unregister-socket
  "A bound listener can be registered, queried, and unregistered."
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0))))
    (unwind-protect
         (let ((fd (net-core:tcp-listener-handle listener)))
           (reactor:boot-reactor)
           (assert-true (not (reactor:socket-registered-p fd)))
           (reactor:register-socket fd '(:in))
           (assert-true (reactor:socket-registered-p fd))
           (reactor:unregister-socket fd)
           (assert-true (not (reactor:socket-registered-p fd))))
      (ignore-errors (net:tcp-close listener)))))

(deftest test-modify-socket-events
  "modify-socket-events updates the registered event set."
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0))))
    (unwind-protect
         (let ((fd (net-core:tcp-listener-handle listener)))
           (reactor:boot-reactor)
           (reactor:register-socket fd '(:in))
           (assert-true (reactor:socket-registered-p fd))
           ;; Add :out interest
           (reactor:modify-socket-events fd '(:in :out))
           (assert-true (reactor:socket-registered-p fd))
           ;; Drop back to :in only
           (reactor:modify-socket-events fd '(:in))
           (assert-true (reactor:socket-registered-p fd))
           (reactor:unregister-socket fd))
      (ignore-errors (net:tcp-close listener)))))

;;; ============================================================================
;;; Callback Dispatch (parallel to the Linux test-epoll-callback-waiter)
;;; ============================================================================

(deftest test-kqueue-callback-waiter
  "register-socket-callback fires when a connected socket becomes readable."
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0)))
        (fired nil)
        (fired-lock (lock:make-lock "fired")))
    (unwind-protect
         (let* ((port (net:socket-address-port
                       (net:tcp-local-addr listener)))
                (addr (net:make-socket-address "127.0.0.1" port))
                (client (net:tcp-connect addr))
                (server (net:tcp-accept listener))
                (server-fd (net:tcp-stream-handle server)))
           (unwind-protect
                (progn
                  (reactor:register-socket-callback
                   server-fd '(:in)
                   (lambda (event)
                     (declare (ignore event))
                     (lock:with-lock (fired-lock)
                       (setf fired t))))
                  (let ((buf (make-array 5 :element-type '(unsigned-byte 8)
                                           :initial-contents '(1 2 3 4 5))))
                    (net:tcp-write client buf :start 0 :end 5))
                  (loop for i from 0 below 50
                        until (lock:with-lock (fired-lock) fired)
                        do (sleep 0.02))
                  (lock:with-lock (fired-lock)
                    (assert-true fired)))
             (ignore-errors (reactor:unregister-socket server-fd))
             (ignore-errors (net:tcp-close client))
             (ignore-errors (net:tcp-close server))))
      (ignore-errors (net:tcp-close listener)))))

;;; ============================================================================
;;; Any-Queue Delivery
;;; ============================================================================

(deftest test-wait-for-any-socket
  "Events for sockets without a specific waiter land on the any-queue."
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0))))
    (unwind-protect
         (let* ((port (net:socket-address-port
                       (net:tcp-local-addr listener)))
                (addr (net:make-socket-address "127.0.0.1" port))
                (client (net:tcp-connect addr))
                (server (net:tcp-accept listener)))
           (unwind-protect
                (let ((server-fd (net:tcp-stream-handle server)))
                  ;; Register without a waiter -- events go to any-queue.
                  (reactor:register-socket server-fd '(:in))
                  (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                                           :initial-contents '(7 8 9))))
                    (net:tcp-write client buf :start 0 :end 3))
                  ;; Drain until we find an event for our fd (stale events
                  ;; for prior tests' fds may be queued ahead of ours).
                  (let ((found nil))
                    (loop for i from 0 below 50 while (not found)
                          do (dolist (ev (reactor:wait-for-any-socket 50))
                               (when (= (kqueue:kevent-struct-ident ev) server-fd)
                                 (setf found t))))
                    (assert-true found))
                  (reactor:unregister-socket server-fd))
             (ignore-errors (net:tcp-close client))
             (ignore-errors (net:tcp-close server))))
      (ignore-errors (net:tcp-close listener)))))
