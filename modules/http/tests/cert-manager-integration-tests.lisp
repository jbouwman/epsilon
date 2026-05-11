;;;; Integration between epsilon.http.server and epsilon.cert-manager.
;;;;
;;;; These tests cover IMPL-344 Stage 2 -- hot-swappable TLS context so
;;;; the cert-manager's background renewal thread can install a fresh
;;;; certificate in a running listener without dropping connections.
;;;; Real TLS handshakes are covered by the mtls test suite; here we
;;;; verify the swap mechanism itself by standing up a server with a
;;;; sentinel "context" and asserting that swap! replaces it.

(defpackage epsilon.http.cert-manager-integration.tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.http.server server)
   (epsilon.sys.thread thread)))

(in-package epsilon.http.cert-manager-integration.tests)

(deftest swap-tls-context-replaces-current-context ()
  "server:swap-tls-context! atomically replaces what the accept loop
will see for newly-accepted connections. Sentinel keyword values stand
in for real crypto:tls-context objects -- we only want to verify the
slot accessor path here, not TLS behaviour."
  (let ((srv (make-instance 'server::http-server
                             :port 0
                             :socket nil
                             :tls-context :initial-context
                             :ssl-p t
                             :application nil)))
    (assert-eq :initial-context (server:server-tls-context srv))
    (server:swap-tls-context! srv :renewed-context)
    (assert-eq :renewed-context (server:server-tls-context srv))))

(deftest swap-tls-context-is-mutex-guarded ()
  "Concurrent swap! calls do not corrupt the slot."
  (let ((srv (make-instance 'server::http-server
                             :port 0
                             :socket nil
                             :tls-context :initial
                             :ssl-p t
                             :application nil))
        (threads nil))
    (dotimes (i 16)
      (push (thread:make-thread
             (lambda ()
               (dotimes (j 64)
                 (declare (ignorable j))
                 (server:swap-tls-context! srv :other)
                 (server:swap-tls-context! srv :initial))))
            threads))
    (dolist (th threads)
      (thread:join-thread th :timeout 10))
    ;; Whatever the final value is must be one of the two sentinels;
    ;; the mutex prevents tearing.
    (let ((final (server:server-tls-context srv)))
      (assert-true (or (eq final :initial) (eq final :other))))))
