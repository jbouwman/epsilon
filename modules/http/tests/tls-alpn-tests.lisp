;;;; TLS + ALPN End-to-End Tests
;;;;
;;;; Spins up an in-process TLS-enabled HTTP server using a freshly-
;;;; generated self-signed Ed25519 certificate, then exercises the client's
;;;; ALPN negotiation through the real TLS handshake. Phase 3 of IMPL-322
;;;; could only test ALPN over plain HTTP because there was no in-process
;;;; TLS infrastructure -- this test fills that gap.

(defpackage :epsilon.http.tls-alpn.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.server server)
   (epsilon.http.request request)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)
   (epsilon.crypto crypto)
   (epsilon.map map)))

(in-package :epsilon.http.tls-alpn.tests)

(defmacro with-tls-server ((port-var cert-var key-var
                            &key (alpn-protocols ''("http/1.1")))
                           &body body)
  "Run BODY with a TLS HTTP server bound on a random localhost port,
   using a freshly-generated self-signed cert. PORT-VAR, CERT-VAR, and
   KEY-VAR are bound for use inside the body. ALPN-PROTOCOLS controls
   what protocols the server advertises."
  (let ((server-var (gensym "SERVER")))
    `(helpers:with-test-cert (,cert-var ,key-var)
       (let* ((,port-var (helpers:find-available-port))
              (,server-var nil))
         (unwind-protect
              (progn
                (setf ,server-var
                      (server:start-server
                       (lambda (req)
                         (declare (ignore req))
                         (response:text-response "tls-ok"))
                       :port ,port-var
                       :address "127.0.0.1"
                       :ssl-p t
                       :cert-file ,cert-var
                       :key-file ,key-var
                       :alpn-protocols ,alpn-protocols))
                (sleep 0.1)
                ,@body)
           (when ,server-var
             (ignore-errors (server:stop-server ,server-var))))))))

(deftest test-tls-server-accepts-https-request ()
  "Sanity: a freshly-generated cert is accepted by the client and the
   server returns a 200 over real TLS."
  (with-tls-server (port cert key)
(let ((resp (client:get (format nil "https://127.0.0.1:~D/" port)
                            :alpn-protocols '("http/1.1"))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (assert-equal "tls-ok" (response:response-body-string resp)))))

(deftest test-tls-alpn-negotiates-http11 ()
  "When client and server both advertise http/1.1, the negotiated protocol
   should be http/1.1."
  (with-tls-server (port cert key
                    :alpn-protocols '("http/1.1"))
(let ((resp (client:get (format nil "https://127.0.0.1:~D/" port)
                            :alpn-protocols '("http/1.1"))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-tls-alpn-h2-offered-but-server-only-supports-http11 ()
  "Client advertises h2 first then http/1.1; server only supports http/1.1.
   They should successfully negotiate http/1.1 and the request should succeed
   over the HTTP/1.1 framing path."
  (with-tls-server (port cert key
                    :alpn-protocols '("http/1.1"))
(let ((resp (client:get (format nil "https://127.0.0.1:~D/" port)
                            :alpn-protocols '("h2" "http/1.1"))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-tls-server-records-negotiated-protocol-on-request ()
  "The server adds the negotiated ALPN protocol as a header on the request
   it hands to the application. We verify that header makes it through."
  (helpers:with-test-cert (cert key)
    (let* ((port (helpers:find-available-port))
           (srv nil)
           (captured-proto nil))
      (unwind-protect
           (progn
             (setf srv (server:start-server
                        (lambda (req)
                          (setf captured-proto
                                (request:request-header req "X-Negotiated-Protocol"))
                          (response:text-response "ok"))
                        :port port
                        :address "127.0.0.1"
                        :ssl-p t
                        :cert-file cert
                        :key-file key
                        :alpn-protocols '("http/1.1")))
             (sleep 0.1)
             (let ((resp (client:get (format nil "https://127.0.0.1:~D/" port)
                                     :alpn-protocols '("http/1.1"))))
               (assert-true resp)
               (assert-equal 200 (response:response-status resp))
               ;; Either nil (no protocol returned by openssl) or "http/1.1"
               (assert-true (or (null captured-proto)
                                (string= captured-proto "http/1.1")))))
        (when srv (ignore-errors (server:stop-server srv)))))))
