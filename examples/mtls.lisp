;;;; TLS Demonstration Examples
;;;;
;;;; Examples showing TLS with HTTP/1.1
;;;;
;;;; Requires: epsilon.http, epsilon.crypto

;; Load required modules
(epsilon.loader:load-module epsilon.loader:*environment* "epsilon.http")
(epsilon.loader:load-module epsilon.loader:*environment* "epsilon.crypto")

(defpackage :epsilon.examples.mtls-demo
  (:use :cl)
  (:local-nicknames
   (#:http #:epsilon.http.simple)
   (#:server #:epsilon.http.server)
   (#:req #:epsilon.http.request)
   (#:resp #:epsilon.http.response)
   (#:certs #:epsilon.crypto.certificates)
   (#:path #:epsilon.path))
  (:export #:run-demo
           #:setup-demo-certificates))

(in-package :epsilon.examples.mtls-demo)

;;;; Certificate Setup

(defun setup-demo-certificates ()
  "Generate certificates for the demonstration"
  (format t "~%=== Setting up TLS Demo Certificates ===~%")
  (let* ((output-dir (path:path-string
                      (path:path-join (path:home-directory)
                                      ".epsilon" "tls-demo"))))
    (ensure-directories-exist (concatenate 'string output-dir "/"))

    ;; Generate server certificate
    (format t "Generating server certificate...~%")
    (multiple-value-bind (server-cert server-key)
        (certs:generate-self-signed-certificate "localhost"
                                                :organization "Epsilon Server"
                                                :days 365)
      (let ((server-cert-file (concatenate 'string output-dir "/server.crt"))
            (server-key-file (concatenate 'string output-dir "/server.key")))
        (certs:save-certificate server-cert server-cert-file)
        (certs:save-private-key server-key server-key-file)

        (format t "~%Certificates generated in ~A~%" output-dir)
        (list :server-cert server-cert-file
              :server-key server-key-file)))))

;;;; HTTP/1.1 TLS Demo

(defun demo-server (cert-suite &key (port 8443))
  "Demonstrate HTTP/1.1 server with TLS"
  (format t "~%=== HTTP/1.1 TLS Server Demo ===~%")
  (format t "Starting server on port ~D...~%" port)

  ;; Define request handler
  (flet ((handler (request)
           (let ((path (req:request-path request)))
             (cond
               ;; Public endpoint
               ((string= path "/public")
                (resp:json-response
                 `((:message . "Public endpoint")
                   (:timestamp . ,(get-universal-time)))))

               ;; Info endpoint
               ((string= path "/info")
                (resp:json-response
                 `((:server . "Epsilon HTTP/1.1")
                   (:tls . t))))

               (t
                (resp:text-response "Not Found" :status 404))))))

    ;; Start server
    (server:start-server
     #'handler
     :port port
     :ssl-p t
     :cert-file (getf cert-suite :server-cert)
     :key-file (getf cert-suite :server-key))))

(defun demo-client (&key (port 8443))
  "Demonstrate HTTP/1.1 client with TLS"
  (format t "~%=== HTTP/1.1 TLS Client Demo ===~%")

  (format t "~%Connecting to server...~%")
  ;; Disable SSL verification for demo (self-signed cert)
  ;; Use IP address to avoid SNI hostname lookup
  (let ((epsilon.http.simple:*verify-ssl* nil))
    (handler-case
        (let ((response (http:http-get (format nil "https://127.0.0.1:~D/info" port))))
          (format t "   Status: ~A~%" (http:response-status response))
          (format t "   Body: ~A~%" (http:response-text response)))
      (error (e)
        (format t "   Error: ~A~%" e)))))

;;;; Complete Demo

(defun run-demo ()
  "Run TLS demonstration"
  (format t "~%")
  (format t "=====================================~%")
  (format t "   TLS Demonstration for Epsilon~%")
  (format t "=====================================~%")

  ;; Setup certificates
  (let ((certs (setup-demo-certificates)))

    ;; Start server
    (format t "~%--- HTTP/1.1 with TLS ---~%")
    (let ((server (demo-server certs :port 8443)))
      (unwind-protect
           (progn
             (sleep 1)  ; Let server start
             (demo-client :port 8443))
        (when server
          (server:stop-server server))))

    (format t "~%=== Demo Complete ===~%")))

;;;; Usage Instructions

(defun print-usage ()
  "Print usage instructions"
  (format t "~%TLS Demo Usage:~%")
  (format t "===============~%~%")
  (format t "1. Run demo:~%")
  (format t "   (epsilon.examples.mtls-demo:run-demo)~%~%")
  (format t "2. Generate test certificates:~%")
  (format t "   (epsilon.examples.mtls-demo:setup-demo-certificates)~%~%"))

;; Print usage on load
(print-usage)
