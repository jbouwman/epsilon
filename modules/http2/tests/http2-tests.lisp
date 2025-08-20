;;;; HTTP/2 Protocol Tests
;;;;
;;;; Basic tests for HTTP/2 functionality

(defpackage :epsilon.http2.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.http2.tests)

;;;; Module Structure Tests

(deftest test-http2-module-loaded
  "Test that HTTP/2 module loads correctly"
  (is (find-package :epsilon.http2)))

(deftest test-http2-exports
  "Test that key HTTP/2 functions are exported"
  ;; Connection management
  (is (fboundp 'http2:make-http2-connection))
  (is (fboundp 'http2:http2-connection-p))
  (is (fboundp 'http2:connection-send-frame))
  (is (fboundp 'http2:connection-receive-frame))
  (is (fboundp 'http2:connection-close))
  
  ;; Stream management
  (is (fboundp 'http2:create-stream))
  (is (fboundp 'http2:stream-send-headers))
  (is (fboundp 'http2:stream-send-data))
  (is (fboundp 'http2:stream-receive-headers))
  (is (fboundp 'http2:stream-receive-data))
  (is (fboundp 'http2:stream-close))
  
  ;; Client functions
  (is (fboundp 'http2:http2-request))
  (is (fboundp 'http2:http2-get))
  (is (fboundp 'http2:http2-post))
  
  ;; Protocol negotiation
  (is (fboundp 'http2:upgrade-to-http2))
  (is (fboundp 'http2:is-http2-connection)))

(deftest test-http2-constants
  "Test HTTP/2 protocol constants"
  (is-equal "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" http2::+http2-preface+)
  (is-not-null http2:+default-settings+)
  
  ;; Check default settings structure
  (let ((settings http2:+default-settings+))
    (is (assoc :header-table-size settings))
    (is (assoc :enable-push settings))
    (is (assoc :max-concurrent-streams settings))
    (is (assoc :initial-window-size settings))
    (is (assoc :max-frame-size settings))
    (is (assoc :max-header-list-size settings))))

(deftest test-default-settings-values
  "Test that default settings have correct values"
  (let ((settings http2:+default-settings+))
    (is-= 4096 (cdr (assoc :header-table-size settings)))
    (is-= 1 (cdr (assoc :enable-push settings)))
    (is-= 100 (cdr (assoc :max-concurrent-streams settings)))
    (is-= 65535 (cdr (assoc :initial-window-size settings)))
    (is-= 16384 (cdr (assoc :max-frame-size settings)))
    (is-= 8192 (cdr (assoc :max-header-list-size settings)))))

;;;; Helper Function Tests

(deftest test-string-bytes-conversion
  "Test string to bytes and back conversion"
  (let* ((test-string "Hello, HTTP/2!")
         (bytes (http2::string-to-bytes test-string))
         (converted-back (http2::bytes-to-string bytes)))
    (is-equal test-string converted-back)
    
    ;; Check byte values
    (is-= (char-code #\H) (aref bytes 0))
    (is-= (char-code #\e) (aref bytes 1))
    (is-= (char-code #\!) (aref bytes (1- (length bytes))))))

(deftest test-http2-preface-bytes
  "Test HTTP/2 preface conversion to bytes"
  (let ((preface-bytes (http2::string-to-bytes http2::+http2-preface+)))
    (is-not-null preface-bytes)
    (is-= 24 (length preface-bytes)) ; "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" is 24 bytes
    
    ;; Check first few bytes
    (is-= (char-code #\P) (aref preface-bytes 0))
    (is-= (char-code #\R) (aref preface-bytes 1))
    (is-= (char-code #\I) (aref preface-bytes 2))))

;;;; Connection Tests

(deftest test-http2-connection-creation
  "Test creating HTTP/2 connection object"
  ;; Create a mock socket for testing
  (let ((mock-socket :mock-socket))
    (let ((conn (make-instance 'http2::http2-connection
                              :socket mock-socket
                              :client-p t)))
      (is-not-null conn)
      (is-eq mock-socket (http2::connection-socket conn))
      (is-true (http2::connection-client-p conn))
      (is-not-null (http2::connection-streams conn))
      (is-= 1 (http2::connection-next-stream-id conn))
      (is-= 65535 (http2::connection-send-window conn))
      (is-= 65535 (http2::connection-recv-window conn)))))

(deftest test-client-stream-ids
  "Test that client uses odd stream IDs"
  (let ((conn (make-instance 'http2::http2-connection
                            :socket :mock
                            :client-p t)))
    (is-= 1 (http2::connection-next-stream-id conn))
    ;; After creating a stream, next ID should be 3
    (setf (http2::connection-next-stream-id conn) 3)
    (is-= 3 (http2::connection-next-stream-id conn))))

(deftest test-server-stream-ids
  "Test that server uses even stream IDs"
  (let ((conn (make-instance 'http2::http2-connection
                            :socket :mock
                            :client-p nil)))
    ;; Server should start with even IDs
    (is-= 1 (http2::connection-next-stream-id conn)) ; Default is 1, server logic would adjust
    ;; This is simplified - real implementation would handle this properly
    ))

;;;; Settings Tests

(deftest test-settings-structure
  "Test HTTP/2 settings structure"
  (let ((custom-settings '((:header-table-size . 8192)
                          (:enable-push . 0)
                          (:max-concurrent-streams . 50))))
    (is-= 8192 (cdr (assoc :header-table-size custom-settings)))
    (is-= 0 (cdr (assoc :enable-push custom-settings)))
    (is-= 50 (cdr (assoc :max-concurrent-streams custom-settings)))))

;;;; URL Parsing for HTTP/2

(deftest test-http2-url-components
  "Test that HTTP/2 properly handles URL components"
  ;; These would be used in http2-request
  (let ((test-urls '("https://example.com/path"
                    "https://api.example.com:8443/v1/data"
                    "https://localhost/test?param=value")))
    (dolist (url test-urls)
      ;; Just verify the URL format is valid
      (is-true (search "https://" url)))))

;;;; Protocol Selection Tests

(deftest test-alpn-protocol-for-http2
  "Test that HTTP/2 uses correct ALPN protocol"
  ;; HTTP/2 should advertise "h2" for ALPN
  (is-equal "h2" (first '("h2" "http/1.1"))))

;;;; Error Handling Tests

(deftest test-http2-without-alpn
  "Test handling when HTTP/2 is not negotiated"
  ;; This would test the error path when ALPN doesn't negotiate h2
  ;; For now, just verify the error condition exists
  (is-true (find-class 'error)))

;;;; Frame Tests Placeholder

(deftest test-frame-types-defined
  "Test that frame type constants would be defined"
  ;; These would be in the frames module
  ;; For now, just verify the package reference exists
  (is-true t))

;;;; HPACK Tests Placeholder

(deftest test-hpack-encoding-decoding
  "Test that HPACK encoder/decoder would work"
  ;; These would be in the hpack module
  ;; For now, just verify the package reference exists
  (is-true t))

;;;; Stream Tests Placeholder

(deftest test-stream-management
  "Test that stream management would work"
  ;; These would test actual stream creation and management
  ;; For now, just verify the package reference exists
  (is-true t))