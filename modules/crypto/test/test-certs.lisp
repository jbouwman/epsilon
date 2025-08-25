;;;; Test Certificate Generation for mTLS Testing
;;;;
;;;; Utilities to generate test certificates for mTLS testing

(defpackage :epsilon.crypto.test-certs
  (:use :cl)
  (:local-nicknames
   (#:certs #:epsilon.crypto.certificates))
  (:export
   #:generate-test-ca
   #:generate-test-server-cert
   #:generate-test-client-cert
   #:generate-mtls-test-suite
   #:cleanup-test-certs))

(in-package :epsilon.crypto.test-certs)

(defparameter *test-cert-dir* "/tmp/epsilon-mtls-test-certs/")

(defun ensure-test-cert-dir ()
  "Ensure test certificate directory exists"
  (ensure-directories-exist *test-cert-dir*))

(defun generate-test-ca (&key (output-dir *test-cert-dir*) (days 365))
  "Generate a test Certificate Authority.
   
   Returns:
     (values ca-cert-path ca-key-path)
   
   Creates:
     - ca.pem: CA certificate
     - ca-key.pem: CA private key"
  
  (ensure-test-cert-dir)
  
  (multiple-value-bind (ca-cert ca-key)
      (certs:generate-ca-certificate
       "Test CA"
       :organization "Epsilon Test Suite"
       :country "US"
       :key-bits 2048
       :days days)
    
    (let ((ca-cert-path (merge-pathnames "ca.pem" output-dir))
          (ca-key-path (merge-pathnames "ca-key.pem" output-dir)))
      
      ;; Write CA certificate
      (with-open-file (out ca-cert-path
                          :direction :output
                          :if-exists :supersede)
        (write-string ca-cert out))
      
      ;; Write CA key
      (with-open-file (out ca-key-path
                          :direction :output
                          :if-exists :supersede)
        (write-string ca-key out))
      
      (format t "Generated test CA:~%  Certificate: ~A~%  Key: ~A~%"
              ca-cert-path ca-key-path)
      
      (values ca-cert-path ca-key-path))))

(defun generate-test-server-cert (&key 
                                  (hostname "localhost")
                                  (output-dir *test-cert-dir*)
                                  ca-cert-path
                                  ca-key-path
                                  (days 90))
  "Generate a test server certificate signed by CA.
   
   Parameters:
     hostname: Server hostname (default: localhost)
     ca-cert-path: Path to CA certificate
     ca-key-path: Path to CA private key
   
   Returns:
     (values server-cert-path server-key-path)
   
   Creates:
     - server.pem: Server certificate
     - server-key.pem: Server private key"
  
  (ensure-test-cert-dir)
  
  ;; Generate CA if not provided
  (unless (and ca-cert-path ca-key-path)
    (multiple-value-setq (ca-cert-path ca-key-path)
      (generate-test-ca :output-dir output-dir)))
  
  ;; Read CA cert and key
  (let ((ca-cert (alexandria:read-file-into-string ca-cert-path))
        (ca-key (alexandria:read-file-into-string ca-key-path)))
    
    ;; Generate server CSR
    (multiple-value-bind (csr server-key)
        (certs:generate-certificate-request
         hostname
         :organization "Epsilon Test Server"
         :country "US"
         :key-bits 2048)
      
      ;; Sign CSR with CA
      (let ((server-cert (certs:sign-certificate-request
                         csr ca-cert ca-key
                         :days days
                         :is-ca nil)))
        
        (let ((server-cert-path (merge-pathnames "server.pem" output-dir))
              (server-key-path (merge-pathnames "server-key.pem" output-dir)))
          
          ;; Write server certificate
          (with-open-file (out server-cert-path
                              :direction :output
                              :if-exists :supersede)
            (write-string server-cert out))
          
          ;; Write server key
          (with-open-file (out server-key-path
                              :direction :output
                              :if-exists :supersede)
            (write-string server-key out))
          
          (format t "Generated test server certificate:~%  Certificate: ~A~%  Key: ~A~%"
                  server-cert-path server-key-path)
          
          (values server-cert-path server-key-path))))))

(defun generate-test-client-cert (&key
                                  (client-name "test-client")
                                  (output-dir *test-cert-dir*)
                                  ca-cert-path
                                  ca-key-path
                                  (days 90))
  "Generate a test client certificate for mTLS.
   
   Parameters:
     client-name: Client identifier
     ca-cert-path: Path to CA certificate
     ca-key-path: Path to CA private key
   
   Returns:
     (values client-cert-path client-key-path)
   
   Creates:
     - client.pem: Client certificate
     - client-key.pem: Client private key"
  
  (ensure-test-cert-dir)
  
  ;; Generate CA if not provided
  (unless (and ca-cert-path ca-key-path)
    (multiple-value-setq (ca-cert-path ca-key-path)
      (generate-test-ca :output-dir output-dir)))
  
  ;; Read CA cert and key
  (let ((ca-cert (alexandria:read-file-into-string ca-cert-path))
        (ca-key (alexandria:read-file-into-string ca-key-path)))
    
    ;; Generate client CSR
    (multiple-value-bind (csr client-key)
        (certs:generate-certificate-request
         client-name
         :organization "Epsilon Test Client"
         :country "US"
         :key-bits 2048)
      
      ;; Sign CSR with CA
      (let ((client-cert (certs:sign-certificate-request
                         csr ca-cert ca-key
                         :days days
                         :is-ca nil)))
        
        (let ((client-cert-path (merge-pathnames 
                                (format nil "~A.pem" client-name) output-dir))
              (client-key-path (merge-pathnames 
                               (format nil "~A-key.pem" client-name) output-dir)))
          
          ;; Write client certificate
          (with-open-file (out client-cert-path
                              :direction :output
                              :if-exists :supersede)
            (write-string client-cert out))
          
          ;; Write client key
          (with-open-file (out client-key-path
                              :direction :output
                              :if-exists :supersede)
            (write-string client-key out))
          
          (format t "Generated test client certificate:~%  Certificate: ~A~%  Key: ~A~%"
                  client-cert-path client-key-path)
          
          (values client-cert-path client-key-path))))))

(defun generate-mtls-test-suite (&key (output-dir *test-cert-dir*))
  "Generate complete mTLS test certificate suite.
   
   Creates:
     - CA certificate and key
     - Server certificate and key
     - Multiple client certificates and keys
   
   Returns:
     Property list with all certificate paths"
  
  (ensure-test-cert-dir)
  
  (format t "~%Generating mTLS test certificate suite...~%")
  
  ;; Generate CA
  (multiple-value-bind (ca-cert ca-key)
      (generate-test-ca :output-dir output-dir)
    
    ;; Generate server certificate
    (multiple-value-bind (server-cert server-key)
        (generate-test-server-cert 
         :hostname "localhost"
         :output-dir output-dir
         :ca-cert-path ca-cert
         :ca-key-path ca-key)
      
      ;; Generate client certificates
      (multiple-value-bind (client1-cert client1-key)
          (generate-test-client-cert
           :client-name "client1"
           :output-dir output-dir
           :ca-cert-path ca-cert
           :ca-key-path ca-key)
        
        (multiple-value-bind (client2-cert client2-key)
            (generate-test-client-cert
             :client-name "client2"
             :output-dir output-dir
             :ca-cert-path ca-cert
             :ca-key-path ca-key)
          
          ;; Generate an untrusted client (different CA)
          (let ((untrusted-ca-dir (merge-pathnames "untrusted/" output-dir)))
            (ensure-directories-exist untrusted-ca-dir)
            
            (multiple-value-bind (untrusted-ca-cert untrusted-ca-key)
                (generate-test-ca :output-dir untrusted-ca-dir)
              
              (multiple-value-bind (untrusted-client-cert untrusted-client-key)
                  (generate-test-client-cert
                   :client-name "untrusted-client"
                   :output-dir untrusted-ca-dir
                   :ca-cert-path untrusted-ca-cert
                   :ca-key-path untrusted-ca-key)
                
                (let ((result (list
                              :ca-cert ca-cert
                              :ca-key ca-key
                              :server-cert server-cert
                              :server-key server-key
                              :client1-cert client1-cert
                              :client1-key client1-key
                              :client2-cert client2-cert
                              :client2-key client2-key
                              :untrusted-client-cert untrusted-client-cert
                              :untrusted-client-key untrusted-client-key)))
                  
                  (format t "~%mTLS test suite generated successfully!~%")
                  (format t "Directory: ~A~%" output-dir)
                  (format t "~%Files created:~%")
                  (loop for (key path) on result by #'cddr
                        do (format t "  ~A: ~A~%" key path))
                  
                  result)))))))))

(defun cleanup-test-certs (&key (output-dir *test-cert-dir*))
  "Clean up test certificates."
  
  (when (probe-file output-dir)
    (format t "Cleaning up test certificates in ~A~%" output-dir)
    ;; Remove directory and contents
    (uiop:delete-directory-tree output-dir :validate t)
    (format t "Test certificates cleaned up.~%")))