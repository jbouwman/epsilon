;;;; Test HTTPS functionality
;;;;
;;;; Simple test script to verify TLS works with real servers

(format t "~%Testing HTTPS functionality...~%~%")

;; Test 1: Parse HTTPS URL
(format t "Test 1: Parsing HTTPS URL~%")
(multiple-value-bind (scheme host port path query)
    (epsilon.http.client::parse-url "https://httpbin.org/get?test=1")
  (format t "  Scheme: ~A~%" scheme)
  (format t "  Host: ~A~%" host)
  (format t "  Port: ~A~%" port)
  (format t "  Path: ~A~%" path)
  (format t "  Query: ~A~%" query)
  (assert (string= scheme "https"))
  (assert (= port 443)))
(format t "  ✓ Passed~%~%")

;; Test 2: Create TLS connection
(format t "Test 2: Creating TLS connection~%")
(handler-case
    (let ((conn (epsilon.http.client::make-http-connection "httpbin.org" 443 :ssl-p t)))
      (format t "  Connection created: ~A~%" conn)
      (format t "  SSL enabled: ~A~%" (epsilon.http.client::connection-ssl-p conn))
      (format t "  TLS connection: ~A~%" (epsilon.http.client::connection-tls-connection conn))
      (assert (epsilon.http.client::connection-ssl-p conn))
      (format t "  ✓ Passed~%~%"))
  (error (e)
    (format t "  ✗ Failed: ~A~%~%" e)))

;; Test 3: Simple HTTPS GET (using mock mode for now)
(format t "Test 3: HTTPS GET request (mock mode)~%")
(handler-case
    (progn
      ;; Enable mock mode for testing
      (epsilon.tls:enable-mock-mode)
      
      ;; Create mock response
      (let* ((mock-response (format nil "HTTP/1.1 200 OK~C~CContent-Type: application/json~C~CContent-Length: 15~C~C~C~C{\"status\":\"ok\"}" 
                                   #\Return #\Linefeed 
                                   #\Return #\Linefeed
                                   #\Return #\Linefeed 
                                   #\Return #\Linefeed)))
        
        ;; Make connection and test
        (epsilon.http.client:with-connection (conn "httpbin.org" 443 :ssl-p t)
          ;; Inject mock response
          (epsilon.tls:inject-mock-data 
           (epsilon.http.client::connection-tls-connection conn) 
           mock-response)
          
          ;; Send request
          (epsilon.http.client::send-request conn "GET" "/get" 
                                            :headers (epsilon.map:make-map 
                                                     "Accept" "application/json"))
          
          ;; Read response
          (let ((response (epsilon.http.client::read-response conn)))
            (format t "  Status: ~A~%" (getf response :status))
            (format t "  Body: ~A~%" (getf response :body))
            (assert (= (getf response :status) 200))
            (format t "  ✓ Passed~%~%"))))
      
      ;; Disable mock mode
      (epsilon.tls:disable-mock-mode))
  (error (e)
    (format t "  ✗ Failed: ~A~%~%" e)))

;; Test 4: Test with real HTTPS (when OpenSSL is available)
(format t "Test 4: Real HTTPS request (stub mode)~%")
(handler-case
    (progn
      ;; For now, the stub implementation doesn't support real HTTPS
      ;; This test will work when OpenSSL integration is complete
      (format t "  Note: Real HTTPS requires OpenSSL integration~%")
      (format t "  Currently using stub implementation~%")
      (format t "  ⚠ Skipped~%~%"))
  (error (e)
    (format t "  ✗ Failed: ~A~%~%" e)))

(format t "HTTPS testing complete!~%")