;;;; mTLS Demonstration Examples
;;;;
;;;; Complete examples showing mutual TLS authentication with HTTP/1.1 and HTTP/2

(defpackage :epsilon.examples.mtls-demo
  (:use :cl)
  (:local-nicknames
   (#:http #:epsilon.http)
   (#:http2 #:epsilon.http2)
   (#:http2-server #:epsilon.http2.server)
   (#:http2-client #:epsilon.http2.client)
   (#:server #:epsilon.http.server)
   (#:client #:epsilon.http.client)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:mtls #:epsilon.crypto.mtls)
   (#:certs #:epsilon.crypto.certificates)
   (#:test-certs #:epsilon.crypto.test-certs)))

(in-package :epsilon.examples.mtls-demo)

;;;; Certificate Setup

(defun setup-demo-certificates ()
  "Generate certificates for the demonstration"
  (format t "~%=== Setting up mTLS Demo Certificates ===~%")
  (let ((suite (test-certs:generate-mtls-test-suite 
                :output-dir "/tmp/mtls-demo/")))
    (format t "~%Certificates generated:~%")
    (format t "  CA: ~A~%" (getf suite :ca-cert))
    (format t "  Server: ~A~%" (getf suite :server-cert))
    (format t "  Client 1: ~A~%" (getf suite :client1-cert))
    (format t "  Client 2: ~A~%" (getf suite :client2-cert))
    suite))

;;;; HTTP/1.1 mTLS Demo

(defun demo-http1-server (cert-suite &key (port 8443))
  "Demonstrate HTTP/1.1 server with mTLS"
  (format t "~%=== HTTP/1.1 mTLS Server Demo ===~%")
  (format t "Starting server on port ~D with mutual TLS...~%" port)
  
  ;; Define request handler
  (flet ((handler (req)
           (let ((path (request:request-path req))
                 (client-cert (epsilon.map:get (request:request-headers req)
                                               "X-Client-Cert-Subject")))
             (cond
               ;; Public endpoint
               ((string= path "/public")
                (response:json-response 
                 `((:message . "Public endpoint - no auth required")
                   (:timestamp . ,(get-universal-time)))))
               
               ;; Authenticated endpoint
               ((string= path "/secure")
                (if client-cert
                    (response:json-response
                     `((:message . "Secure endpoint accessed")
                       (:client . ,client-cert)
                       (:timestamp . ,(get-universal-time))))
                  (response:text-response "Client certificate required" :status 401)))
               
               ;; Info endpoint
               ((string= path "/info")
                (response:json-response
                 `((:server . "Epsilon HTTP/1.1")
                   (:mtls . t)
                   (:client-cert . ,(if client-cert "present" "absent"))
                   (:protocol . ,(epsilon.map:get (request:request-headers req)
                                                  "X-Negotiated-Protocol")))))
               
               (t
                (response:text-response "Not Found" :status 404))))))
    
    ;; Start server
    (server:start-server 
     #'handler
     :port port
     :ssl-p t
     :cert-file (getf cert-suite :server-cert)
     :key-file (getf cert-suite :server-key)
     :ca-file (getf cert-suite :ca-cert)
     :require-client-cert nil  ; Make client cert optional
     :alpn-protocols '("http/1.1"))))

(defun demo-http1-client (cert-suite &key (port 8443))
  "Demonstrate HTTP/1.1 client with mTLS"
  (format t "~%=== HTTP/1.1 mTLS Client Demo ===~%")
  
  ;; Test without client certificate
  (format t "~%1. Connecting without client certificate...~%")
  (handler-case
      (client:with-connection (conn "localhost" port
                                   :ssl-p t
                                   :ca-file (getf cert-suite :ca-cert))
        (let ((response (http:http-get "/public")))
          (format t "   Public endpoint: ~A~%" 
                  (response:response-status response)))
        
        (let ((response (http:http-get "/secure")))
          (format t "   Secure endpoint: ~A (expected 401)~%"
                  (response:response-status response))))
    (error (e)
      (format t "   Error: ~A~%" e)))
  
  ;; Test with client certificate
  (format t "~%2. Connecting WITH client certificate...~%")
  (client:with-connection (conn "localhost" port
                               :ssl-p t
                               :cert-file (getf cert-suite :client1-cert)
                               :key-file (getf cert-suite :client1-key)
                               :ca-file (getf cert-suite :ca-cert))
    (let ((response (http:http-get "/secure")))
      (format t "   Secure endpoint: ~A~%" 
              (response:response-status response))
      (format t "   Response: ~A~%" 
              (response:response-body response)))
    
    (let ((response (http:http-get "/info")))
      (format t "   Server info: ~A~%"
              (response:response-body response)))))

;;;; HTTP/2 mTLS Demo

(defun demo-http2-server (cert-suite &key (port 8444))
  "Demonstrate HTTP/2 server with mTLS"
  (format t "~%=== HTTP/2 mTLS Server Demo ===~%")
  (format t "Starting HTTP/2 server on port ~D with mutual TLS...~%" port)
  
  ;; Define HTTP/2 handler
  (flet ((handler (request)
           (let ((path (cdr (assoc ":path" request :test #'string=)))
                 (client-cert (cdr (assoc "x-client-cert" request :test #'string=))))
             (cond
               ;; Public endpoint
               ((string= path "/api/public")
                (list :status 200
                      :headers '(("content-type" . "application/json"))
                      :body "{\"message\":\"HTTP/2 public endpoint\"}"))
               
               ;; Secure endpoint
               ((string= path "/api/secure")
                (if client-cert
                    (list :status 200
                          :headers '(("content-type" . "application/json"))
                          :body (format nil "{\"message\":\"Authenticated\",\"client\":\"~A\"}"
                                       client-cert))
                  (list :status 401
                        :headers '(("content-type" . "text/plain"))
                        :body "Unauthorized")))
               
               ;; Stream test endpoint
               ((string= path "/api/stream")
                (list :status 200
                      :headers '(("content-type" . "text/plain"))
                      :body "HTTP/2 stream response"))
               
               (t
                (list :status 404
                      :headers '(("content-type" . "text/plain"))
                      :body "Not Found"))))))
    
    ;; Start HTTP/2 server
    (http2-server:start-http2-server
     :port port
     :ssl-p t
     :cert-file (getf cert-suite :server-cert)
     :key-file (getf cert-suite :server-key)
     :ca-file (getf cert-suite :ca-cert)
     :require-client-cert nil
     :handler #'handler)))

(defun demo-http2-client (cert-suite &key (port 8444))
  "Demonstrate HTTP/2 client with mTLS"
  (format t "~%=== HTTP/2 mTLS Client Demo ===~%")
  
  ;; Connect with client certificate
  (format t "~%Connecting to HTTP/2 server with mTLS...~%")
  (http2-client:with-http2-connection 
      (conn "localhost" port
            :cert-file (getf cert-suite :client1-cert)
            :key-file (getf cert-suite :client1-key)
            :ca-file (getf cert-suite :ca-cert))
    
    (format t "Connected successfully with HTTP/2~%")
    
    ;; Test multiple requests over single connection
    (format t "~%1. Public endpoint:~%")
    (let ((response (http2-client:http2-get conn "/api/public")))
      (format t "   Status: ~A~%" (getf response :status))
      (format t "   Body: ~A~%" (getf response :body)))
    
    (format t "~%2. Secure endpoint (with client cert):~%")
    (let ((response (http2-client:http2-get conn "/api/secure")))
      (format t "   Status: ~A~%" (getf response :status))
      (format t "   Body: ~A~%" (getf response :body)))
    
    (format t "~%3. Multiple streams test:~%")
    (let ((responses (loop for i from 1 to 3
                          collect (http2-client:http2-get 
                                  conn 
                                  (format nil "/api/stream?id=~D" i)))))
      (format t "   Sent ~D requests over single connection~%" (length responses))
      (dolist (response responses)
        (format t "   - Status: ~A~%" (getf response :status))))))

;;;; Complete Demo

(defun run-complete-demo ()
  "Run complete mTLS demonstration"
  (format t "~%")
  (format t "=====================================~%")
  (format t "   mTLS Demonstration for Epsilon~%")
  (format t "=====================================~%")
  
  ;; Setup certificates
  (let ((certs (setup-demo-certificates)))
    
    ;; HTTP/1.1 Demo
    (format t "~%--- HTTP/1.1 with mTLS ---~%")
    (let ((server (demo-http1-server certs :port 8443)))
      (unwind-protect
           (progn
             (sleep 1)  ; Let server start
             (demo-http1-client certs :port 8443))
        (server:stop-server server)))
    
    ;; HTTP/2 Demo
    (format t "~%--- HTTP/2 with mTLS ---~%")
    (let ((server-thread 
           (sb-thread:make-thread
            (lambda () 
              (demo-http2-server certs :port 8444))
            :name "HTTP/2 demo server")))
      (unwind-protect
           (progn
             (sleep 1)  ; Let server start
             (demo-http2-client certs :port 8444))
        (sb-thread:terminate-thread server-thread)))
    
    ;; Cleanup
    (format t "~%=== Demo Complete ===~%")
    (format t "Cleaning up certificates...~%")
    (test-certs:cleanup-test-certs :output-dir "/tmp/mtls-demo/")
    (format t "Done!~%")))

;;;; Individual Examples

(defun example-mtls-api-server ()
  "Example: Secure API server with mTLS"
  (let ((certs (setup-demo-certificates)))
    (format t "~%Starting secure API server...~%")
    (format t "Server requires client certificates for /api/* endpoints~%")
    
    (server:start-server
     (lambda (req)
       (let ((path (request:request-path req))
             (client (epsilon.map:get (request:request-headers req)
                                     "X-Client-Cert-Subject")))
         (cond
           ((epsilon.string:starts-with-p path "/api/")
            (if client
                (response:json-response
                 `((:authorized . t)
                   (:client . ,client)
                   (:resource . ,path)))
              (response:text-response "Client certificate required" :status 403)))
           (t
            (response:text-response "Public area - no cert needed")))))
     :port 8443
     :ssl-p t
     :cert-file (getf certs :server-cert)
     :key-file (getf certs :server-key)
     :ca-file (getf certs :ca-cert)
     :require-client-cert nil)))

(defun example-microservice-communication ()
  "Example: Service-to-service mTLS communication"
  (let ((certs (setup-demo-certificates)))
    (format t "~%Demonstrating microservice mTLS communication...~%")
    
    ;; Service A (acts as server)
    (let ((service-a 
           (sb-thread:make-thread
            (lambda ()
              (server:start-server
               (lambda (req)
                 (response:json-response
                  `((:service . "A")
                    (:message . "Hello from Service A")
                    (:authenticated-client . ,(epsilon.map:get 
                                              (request:request-headers req)
                                              "X-Client-Cert-Subject")))))
               :port 9001
               :ssl-p t
               :cert-file (getf certs :server-cert)
               :key-file (getf certs :server-key)
               :ca-file (getf certs :ca-cert)
               :require-client-cert t))
            :name "Service A")))
      
      (sleep 1)
      
      ;; Service B (acts as client)
      (unwind-protect
           (client:with-connection (conn "localhost" 9001
                                        :ssl-p t
                                        :cert-file (getf certs :client1-cert)
                                        :key-file (getf certs :client1-key)
                                        :ca-file (getf certs :ca-cert))
             (let ((response (http:http-get "/")))
               (format t "Service B received: ~A~%"
                       (response:response-body response))))
        (sb-thread:terminate-thread service-a)))))

;;;; Usage Instructions

(defun print-usage ()
  "Print usage instructions"
  (format t "~%mTLS Demo Usage:~%")
  (format t "================~%~%")
  (format t "1. Run complete demo:~%")
  (format t "   (epsilon.examples.mtls-demo:run-complete-demo)~%~%")
  (format t "2. Start API server with mTLS:~%")
  (format t "   (epsilon.examples.mtls-demo:example-mtls-api-server)~%~%")
  (format t "3. Test microservice communication:~%")
  (format t "   (epsilon.examples.mtls-demo:example-microservice-communication)~%~%")
  (format t "4. Generate test certificates:~%")
  (format t "   (epsilon.examples.mtls-demo:setup-demo-certificates)~%~%"))

;; Print usage on load
(print-usage)