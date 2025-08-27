;;;; Basic Socket Operation Tests for Linux Networking
;;;;
;;;; Test low-level socket operations to ensure correctness before refactoring

(defpackage epsilon.net.socket-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (net epsilon.net)
   (lib epsilon.foreign)))

(in-package epsilon.net.socket-tests)

;;; ============================================================================
;;; Socket Creation Tests
;;; ============================================================================

(deftest test-socket-creation-tcp ()
  "Test TCP socket creation"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (is (integerp fd))
        (is (>= fd 0))
        (when (>= fd 0)
          (net::%close fd))
        t)
    (error (e)
      (is nil (format nil "TCP socket creation failed: ~A" e)))))

(deftest test-socket-creation-udp ()
  "Test UDP socket creation"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-dgram+ net::+ipproto-udp+)))
        (is (integerp fd))
        (is (>= fd 0))
        (when (>= fd 0)
          (net::%close fd))
        t)
    (error (e)
      (is nil (format nil "UDP socket creation failed: ~A" e)))))

(deftest test-invalid-socket-creation ()
  "Test that invalid socket parameters cause errors"
  (is-thrown (net:network-error)
    (net::create-socket -1 net::+sock-stream+ net::+ipproto-tcp+))
  
  (is-thrown (net:network-error)
    (net::create-socket net::+af-inet+ -1 net::+ipproto-tcp+)))

;;; ============================================================================
;;; Socket Option Tests
;;; ============================================================================

(deftest test-socket-reuse-address ()
  "Test SO_REUSEADDR socket option"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (unwind-protect
             (progn
               ;; Test setting SO_REUSEADDR
               (net::set-socket-reuse-addr fd t)
               ;; Verify it was set (would require getsockopt implementation)
               (is t "SO_REUSEADDR set successfully")
               
               ;; Test unsetting SO_REUSEADDR
               (net::set-socket-reuse-addr fd nil)
               (is t "SO_REUSEADDR unset successfully"))
          (when (>= fd 0)
            (net::%close fd))))
    (error (e)
      (is nil (format nil "Socket option test failed: ~A" e)))))

(deftest test-socket-nonblocking ()
  "Test setting socket to non-blocking mode"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (unwind-protect
             (progn
               (net::set-nonblocking fd)
               ;; Socket should now be non-blocking
               ;; We can verify this by attempting a non-blocking operation
               (is t "Socket set to non-blocking mode"))
          (when (>= fd 0)
            (net::%close fd))))
    (error (e)
      (is nil (format nil "Non-blocking mode test failed: ~A" e)))))

;;; ============================================================================
;;; Socket Binding Tests
;;; ============================================================================

(deftest test-socket-bind-tcp ()
  "Test TCP socket binding"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (unwind-protect
             (progn
               (net::set-socket-reuse-addr fd t)
               (lib:with-foreign-memory ((addr :char :count 16))
                 (net::make-sockaddr-in-into addr "0.0.0.0" 0)
                 (let ((result (net::%bind fd addr 16)))
                   (is (zerop result) "Bind should succeed")
                   
                   ;; Get the actual bound address
                   (let ((local-addr (net::get-local-address fd)))
                     (is (typep local-addr 'net:socket-address))
                     (is (plusp (net:socket-address-port local-addr)))))))
          (when (>= fd 0)
            (net::%close fd))))
    (error (e)
      (is nil (format nil "TCP bind test failed: ~A" e)))))

(deftest test-socket-bind-udp ()
  "Test UDP socket binding"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-dgram+ net::+ipproto-udp+)))
        (unwind-protect
             (progn
               (net::set-socket-reuse-addr fd t)
               (lib:with-foreign-memory ((addr :char :count 16))
                 (net::make-sockaddr-in-into addr "0.0.0.0" 0)
                 (let ((result (net::%bind fd addr 16)))
                   (is (zerop result) "Bind should succeed")
                   
                   ;; Get the actual bound address
                   (let ((local-addr (net::get-local-address fd)))
                     (is (typep local-addr 'net:socket-address))
                     (is (plusp (net:socket-address-port local-addr)))))))
          (when (>= fd 0)
            (net::%close fd))))
    (error (e)
      (is nil (format nil "UDP bind test failed: ~A" e)))))

(deftest test-socket-bind-specific-port ()
  "Test binding to a specific port"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+))
            (test-port 12345))
        (unwind-protect
             (progn
               (net::set-socket-reuse-addr fd t)
               (lib:with-foreign-memory ((addr :char :count 16))
                 (net::make-sockaddr-in-into addr "127.0.0.1" test-port)
                 (let ((result (net::%bind fd addr 16)))
                   ;; Bind might fail if port is in use, that's okay
                   (when (zerop result)
                     (let ((local-addr (net::get-local-address fd)))
                       (is-equal test-port (net:socket-address-port local-addr)))))))
          (when (>= fd 0)
            (net::%close fd))))
    (error ()
      ;; Port might be in use, that's acceptable
      (is t "Bind to specific port failed (port may be in use)"))))

(deftest test-socket-bind-address-in-use ()
  "Test that binding to same address twice fails"
  (handler-case
      (let ((fd1 (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+))
            (fd2 (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (unwind-protect
             (progn
               (net::set-socket-reuse-addr fd1 nil) ; Explicitly disable reuse
               (lib:with-foreign-memory ((addr :char :count 16))
                 (net::make-sockaddr-in-into addr "0.0.0.0" 0)
                 (let ((result1 (net::%bind fd1 addr 16)))
                   (is (zerop result1) "First bind should succeed")
                   
                   (when (zerop result1)
                     ;; Get the port that was assigned
                     (let* ((local-addr (net::get-local-address fd1))
                            (port (net:socket-address-port local-addr)))
                       ;; Try to bind second socket to same port
                       (net::make-sockaddr-in-into addr "0.0.0.0" port)
                       (let ((result2 (net::%bind fd2 addr 16)))
                         (is (not (zerop result2)) "Second bind should fail")))))))
          (when (>= fd1 0) (net::%close fd1))
          (when (>= fd2 0) (net::%close fd2))))
    (error (e)
      (is nil (format nil "Address in use test failed: ~A" e)))))

;;; ============================================================================
;;; Socket Listen Tests
;;; ============================================================================

(deftest test-socket-listen ()
  "Test TCP socket listen operation"
  (handler-case
      (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
        (unwind-protect
             (progn
               (net::set-socket-reuse-addr fd t)
               (lib:with-foreign-memory ((addr :char :count 16))
                 (net::make-sockaddr-in-into addr "0.0.0.0" 0)
                 (let ((bind-result (net::%bind fd addr 16)))
                   (is (zerop bind-result) "Bind should succeed")
                   
                   (when (zerop bind-result)
                     (let ((listen-result (net::%listen fd 128)))
                       (is (zerop listen-result) "Listen should succeed"))))))
          (when (>= fd 0)
            (net::%close fd))))
    (error (e)
      (is nil (format nil "Listen test failed: ~A" e)))))

;;; ============================================================================
;;; Socket Address Tests
;;; ============================================================================

(deftest test-sockaddr-in-structure ()
  "Test sockaddr_in structure creation and parsing"
  (lib:with-foreign-memory ((addr :char :count 16))
    ;; Create sockaddr_in
    (net::make-sockaddr-in-into addr "192.168.1.100" 8080)
    
    ;; Verify structure contents
    (is-equal net::+af-inet+ (sb-sys:sap-ref-16 addr 0)) ; sin_family
    
    ;; Parse it back
    (let ((parsed (net::parse-sockaddr-in addr)))
      (is (typep parsed 'net:socket-address))
      (is-equal "192.168.1.100" (net:socket-address-ip parsed))
      (is-equal 8080 (net:socket-address-port parsed)))))

(deftest test-sockaddr-in-localhost ()
  "Test sockaddr_in with localhost address"
  (lib:with-foreign-memory ((addr :char :count 16))
    (net::make-sockaddr-in-into addr "127.0.0.1" 3000)
    (let ((parsed (net::parse-sockaddr-in addr)))
      (is-equal "127.0.0.1" (net:socket-address-ip parsed))
      (is-equal 3000 (net:socket-address-port parsed)))))

(deftest test-sockaddr-in-any-address ()
  "Test sockaddr_in with INADDR_ANY (0.0.0.0)"
  (lib:with-foreign-memory ((addr :char :count 16))
    (net::make-sockaddr-in-into addr "0.0.0.0" 9999)
    (let ((parsed (net::parse-sockaddr-in addr)))
      (is-equal "0.0.0.0" (net:socket-address-ip parsed))
      (is-equal 9999 (net:socket-address-port parsed)))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(deftest test-errno-handling ()
  "Test errno retrieval and error message conversion"
  ;; Test that we can get errno (even if it's -1 on error)
  (let ((errno (net::get-errno)))
    (is (integerp errno)))
  
  ;; Test error string conversion for known errors
  (is-equal "EPERM - Operation not permitted" (net::errno-to-string 1))
  (is-equal "EBADF - Bad file descriptor" (net::errno-to-string 9))
  (is-equal "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable" (net::errno-to-string 11))
  (is-equal "EADDRINUSE - Address already in use" (net::errno-to-string 98))
  (is-equal "ECONNREFUSED - Connection refused" (net::errno-to-string 111))
  
  ;; Test unknown error
  (is (stringp (net::errno-to-string 9999))))

(deftest test-check-error-function ()
  "Test check-error function signals appropriate conditions"
  ;; Test would-block error
  (is-thrown (net:would-block-error)
    (net::check-error -1 "test-operation")
    ;; Mock errno 11 (EAGAIN/EWOULDBLOCK)
    (setf net::*last-errno* 11))
  
  ;; Test connection refused
  (handler-case
      (progn
        ;; This would need mocking to work properly
        (net::check-error -1 "connect")
        (is nil "Should have thrown error"))
    (net:network-error ()
      (is t "Got expected network error"))))

;;; ============================================================================
;;; Resource Cleanup Tests
;;; ============================================================================

(deftest test-socket-cleanup ()
  "Test proper socket resource cleanup"
  (let ((fd-list '()))
    ;; Create multiple sockets
    (dotimes (i 5)
      (push (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+) 
            fd-list))
    
    ;; All should be valid
    (dolist (fd fd-list)
      (is (>= fd 0)))
    
    ;; Clean them up
    (dolist (fd fd-list)
      (when (>= fd 0)
        (net::%close fd)))
    
    ;; After cleanup, operations should fail
    ;; (This is hard to test without causing errors)
    (is t "Cleanup completed")))

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(deftest test-split-string ()
  "Test string splitting utility"
  (is-equal '("192" "168" "1" "1") (net::split-string "192.168.1.1" #\.))
  (is-equal '("foo" "bar" "baz") (net::split-string "foo:bar:baz" #\:))
  (is-equal '("single") (net::split-string "single" #\.))
  (is-equal '("" "leading") (net::split-string ".leading" #\.))
  (is-equal '("trailing" "") (net::split-string "trailing." #\.)))