;;;; HTTP/2 Server Lifecycle Tests
;;;;
;;;; Regression tests for start-http2-server / stop-http2-server and the
;;;; h2-accept-loop unwind-protect cleanup path.
;;;;
;;;; Before the Class B fix in epsilon-contrib/http/src/h2/server.lisp,
;;;; the cleanup path in h2-accept-loop called close-socket on the
;;;; listener, which delegated to net:tcp-shutdown -- and tcp-shutdown
;;;; had no method for tcp-listener, so the cleanup crashed with
;;;; NO-APPLICABLE-METHOD-ERROR on tcp-stream-handle.  That bug took
;;;; epsilon-pkg-server on lark into a recurring crash roughly every
;;;; six hours (matching its startup cadence).
;;;;
;;;; The tests here start a plaintext HTTP/2 server on 127.0.0.1:0
;;;; with a no-op handler, stop it, and assert that the shutdown is
;;;; clean.  No TLS, no real client connections -- just the lifecycle
;;;; that triggers the unwind-protect cleanup.

(defpackage :epsilon.http.h2.server-lifecycle-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.server h2-server)
   (epsilon.net net)))

(defun %noop-handler (headers body)
  (declare (ignore headers body))
  (list :status 204
        :headers '()
        :body ""))

(deftest test-h2-server-start-stop-clean ()
  "start-http2-server followed by stop-http2-server must not signal.
   Exercises the h2-accept-loop unwind-protect cleanup path which
   used to crash on NO-APPLICABLE-METHOD-ERROR when the listener was
   passed through close-socket / tcp-shutdown."
  (let ((server (h2-server:start-http2-server
                 :port 0
                 :host "127.0.0.1"
                 :ssl-p nil
                 :handler #'%noop-handler)))
    (assert-true server "start-http2-server returned NIL")
    (assert-true (h2-server:http2-server-running-p server)
                 "server should be running right after start")
    ;; Let the accept thread reach its epoll_wait.
    (sleep 0.2)
    ;; Clean shutdown -- must not raise.
    (h2-server:stop-http2-server server)
    (assert-true (null (h2-server:http2-server-running-p server))
                 "server should not be running after stop")))

(deftest test-h2-server-port-released-after-stop ()
  "After stop-http2-server returns, the port the server was bound to
   must be available again. Regression: a cleanup path that crashed
   before reaching tcp-close would leak the listener fd, preventing
   rebind."
  (let* ((first (h2-server:start-http2-server
                 :port 0
                 :host "127.0.0.1"
                 :ssl-p nil
                 :handler #'%noop-handler))
         (listener (h2-server::http2-server-socket first))
         (port (net:socket-address-port (net:tcp-local-addr listener))))
    (assert-true (> port 0) "expected OS-assigned port > 0")
    (sleep 0.2)
    (h2-server:stop-http2-server first)
    ;; Give the kernel a moment to release the port before rebinding.
    (sleep 0.1)
    ;; Rebind at the same port must succeed.
    (let ((second (h2-server:start-http2-server
                   :port port
                   :host "127.0.0.1"
                   :ssl-p nil
                   :handler #'%noop-handler)))
      (unwind-protect
           (assert-true (h2-server:http2-server-running-p second)
                        "rebinding at the previous port should succeed")
        (h2-server:stop-http2-server second)))))

(deftest test-h2-server-repeated-start-stop-cycles ()
  "Multiple back-to-back start/stop cycles must not leak listeners or
   thread state. This simulates the cadence that pkg-server experienced
   on lark: start, serve briefly, stop, restart."
  (dotimes (i 3)
    (let ((server (h2-server:start-http2-server
                   :port 0
                   :host "127.0.0.1"
                   :ssl-p nil
                   :handler #'%noop-handler)))
      (assert-true server (format nil "cycle ~D: server NIL" i))
      (sleep 0.1)
      (h2-server:stop-http2-server server)
      (assert-true (null (h2-server:http2-server-running-p server))
                   (format nil "cycle ~D: server still running after stop" i)))))
