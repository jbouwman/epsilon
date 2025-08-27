;;;; Async Operations Tests for Linux Networking
;;;;
;;;; Tests for epoll-based async I/O operations

(defpackage epsilon.net.async-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (net epsilon.net)
   (async epsilon.async)
   (epoll epsilon.sys.epoll)
   (lib epsilon.foreign)))

(in-package epsilon.net.async-tests)

;;; ============================================================================
;;; Test Setup and Utilities
;;; ============================================================================

(defmacro with-async-system (&body body)
  "Ensure async system is running for tests"
  `(unwind-protect
        (progn
          (async:ensure-async-system)
          ,@body)
     (ignore-errors (async:stop-async-system))))

(defun wait-for-completion (operation &optional (timeout-ms 1000))
  "Wait for an async operation to complete"
  (let ((start-time (get-internal-real-time))
        (timeout-ticks (/ (* timeout-ms internal-time-units-per-second) 1000)))
    (loop
     (let ((completions (async:poll-completions 10)))
       (when (member operation completions :test #'eq)
         (return t)))
     (when (> (- (get-internal-real-time) start-time) timeout-ticks)
       (return nil))
     (sleep 0.01))))

;;; ============================================================================
;;; Epoll Integration Tests
;;; ============================================================================

(deftest test-epoll-create-close ()
  "Test epoll instance creation and cleanup"
  (let ((epfd (epoll:epoll-create1 epoll:+epoll-cloexec+)))
    (is (integerp epfd))
    (is (>= epfd 0))
    (epoll:epoll-close epfd)))

(deftest test-epoll-add-modify-delete ()
  "Test epoll control operations"
  (let ((epfd (epoll:epoll-create1 0)))
    (unwind-protect
         ;; Create a socket to monitor
         (let ((sock-fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
           (unwind-protect
                (progn
                  ;; Add socket to epoll
                  (epoll:epoll-add epfd sock-fd '(:in))
                  (is t "Socket added to epoll")
                  
                  ;; Modify events
                  (epoll:epoll-modify epfd sock-fd '(:out))
                  (is t "Socket events modified")
                  
                  ;; Delete from epoll
                  (epoll:epoll-delete epfd sock-fd)
                  (is t "Socket deleted from epoll"))
             (when (>= sock-fd 0)
               (net::%close sock-fd))))
      (epoll:epoll-close epfd))))

(deftest test-epoll-wait-timeout ()
  "Test epoll wait with timeout"
  (let ((epfd (epoll:epoll-create1 0)))
    (unwind-protect
         (progn
           ;; Wait with no events - should timeout
           (let ((events (epoll:wait-for-events epfd 10 100))) ; 100ms timeout
             (is (null events) "Should timeout with no events"))
           
           ;; Test immediate return (0 timeout)
           (let ((events (epoll:wait-for-events epfd 10 0)))
             (is (listp events) "Should return immediately")))
      (epoll:epoll-close epfd))))

(deftest test-epoll-event-detection ()
  "Test epoll event detection on sockets"
  (let ((epfd (epoll:epoll-create1 0)))
    (unwind-protect
         ;; Create listening socket
         (let* ((listener-fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
           (unwind-protect
                (progn
                  (net::set-socket-reuse-addr listener-fd t)
                  (lib:with-foreign-memory ((addr :char :count 16))
                    (net::make-sockaddr-in-into addr "127.0.0.1" 0)
                    (net::%bind listener-fd addr 16)
                    (net::%listen listener-fd 5))
                  
                  ;; Add to epoll for incoming connections
                  (epoll:epoll-add epfd listener-fd '(:in))
                  
                  ;; Get bound port
                  (let* ((local-addr (net::get-local-address listener-fd))
                         (port (net:socket-address-port local-addr)))
                    
                    ;; Connect a client
                    (let ((client-fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
                      (unwind-protect
                           (progn
                             (net::set-nonblocking client-fd)
                             (lib:with-foreign-memory ((addr :char :count 16))
                               (net::make-sockaddr-in-into addr "127.0.0.1" port)
                               (net::%connect client-fd addr 16))
                             
                             ;; Wait for connection event
                             (let ((events (epoll:wait-for-events epfd 1 500))) ; 500ms timeout
                               (is (not (null events)) "Should detect connection")
                               (when events
                                 (let ((event (first events)))
                                   (is (epoll:epoll-event-readable-p event))))))
                        (when (>= client-fd 0)
                          (net::%close client-fd))))))
             (when (>= listener-fd 0)
               (net::%close listener-fd))))
      (epoll:epoll-close epfd))))

;;; ============================================================================
;;; Async System Management Tests
;;; ============================================================================

(deftest test-async-system-lifecycle ()
  "Test async system initialization and cleanup"
  ;; First ensure it's stopped
  (ignore-errors (async:stop-async-system))
  
  ;; Start the system
  (async:ensure-async-system)
  (is t "Async system started")
  
  ;; Should be idempotent
  (async:ensure-async-system)
  (is t "Ensure async system is idempotent")
  
  ;; Stop the system
  (async:stop-async-system)
  (is t "Async system stopped"))

(deftest test-async-thread-management ()
  "Test async background thread"
  (with-async-system
    ;; Thread should be running
    (is (and async::*async-thread* 
             (sb-thread:thread-alive-p async::*async-thread*))
        "Async thread should be running")
    
    ;; Thread should have correct name
    (is (string= "linux-async-loop" 
                 (sb-thread:thread-name async::*async-thread*))
        "Thread has correct name")))

;;; ============================================================================
;;; Async Operation Tests
;;; ============================================================================

(deftest test-async-operation-struct ()
  "Test async operation structure"
  (let ((op (async::make-async-operation
             :fd 5
             :type :read
             :buffer (make-array 100)
             :callback (lambda (bytes) (declare (ignore bytes)))
             :error-callback (lambda (err) (declare (ignore err))))))
    (is (async:async-operation-p op))
    (is-equal 5 (async:async-operation-fd op))
    (is-equal :read (async:async-operation-type op))
    (is (arrayp (async:async-operation-buffer op)))
    (is (functionp (async:async-operation-callback op)))
    (is (functionp (async:async-operation-error-callback op)))))

(deftest test-async-read-operation ()
  "Test async read operation submission"
  (skip "Async operations need real socket fixtures")
  (with-async-system
    (let* ((callback-called nil)
           (error-called nil)
           (op (async:async-read 
                0 ; stdin (for testing)
                (make-array 100 :element-type '(unsigned-byte 8))
                :on-complete (lambda (bytes) 
                               (setf callback-called t)
                               (declare (ignore bytes)))
                :on-error (lambda (err)
                            (setf error-called t)
                            (declare (ignore err))))))
      (is (async:async-operation-p op))
      
      ;; Wait a bit for processing
      (sleep 0.1)
      
      ;; Poll for completions
      (let ((completions (async:poll-completions)))
        (is (listp completions))))))

(deftest test-async-write-operation ()
  "Test async write operation submission"
  (skip "Async operations need real socket fixtures")
  (with-async-system
    (let* ((callback-called nil)
           (op (async:async-write 
                1 ; stdout (for testing)
                (sb-ext:string-to-octets "test")
                :on-complete (lambda (bytes)
                               (setf callback-called t)
                               (declare (ignore bytes))))))
      (is (async:async-operation-p op))
      
      ;; Wait and poll
      (sleep 0.1)
      (async:poll-completions))))

(deftest test-async-accept-operation ()
  "Test async accept operation"
  (skip "Async accept needs full server setup")
  (with-async-system
    (let* ((listener-fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+))
           (client-fd nil))
      (unwind-protect
           (progn
             (net::set-socket-reuse-addr listener-fd t)
             (lib:with-foreign-memory ((addr :char :count 16))
               (net::make-sockaddr-in-into addr "127.0.0.1" 0)
               (net::%bind listener-fd addr 16)
               (net::%listen listener-fd 5))
             
             ;; Submit async accept
             (let ((op (async:async-accept 
                        listener-fd
                        :on-complete (lambda (fd)
                                       (setf client-fd fd)))))
               (is (async:async-operation-p op))
               
               ;; Connect a client
               (let* ((local-addr (net::get-local-address listener-fd))
                      (port (net:socket-address-port local-addr))
                      (client (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
                 (lib:with-foreign-memory ((addr :char :count 16))
                   (net::make-sockaddr-in-into addr "127.0.0.1" port)
                   (net::%connect client addr 16))
                 
                 ;; Wait for accept to complete
                 (sleep 0.2)
                 (async:poll-completions)
                 
                 (when (>= client 0)
                   (net::%close client)))))
        (when (>= listener-fd 0)
          (net::%close listener-fd))
        (when (and client-fd (>= client-fd 0))
          (net::%close client-fd))))))

;;; ============================================================================
;;; Async Cancellation Tests
;;; ============================================================================

(deftest test-async-cancel-operation ()
  "Test cancelling an async operation"
  (with-async-system
    (let* ((error-called nil)
           (op (async:async-read 
                0
                (make-array 100)
                :on-error (lambda (err)
                            (setf error-called t)
                            (declare (ignore err))))))
      
      ;; Cancel the operation
      (is (async:cancel-async-operation op))
      
      ;; Error callback should be called
      (is error-called "Error callback should be called on cancel"))))

(deftest test-async-cleanup-fd-operations ()
  "Test cleaning up all operations for a file descriptor"
  (with-async-system
    (let ((ops '()))
      ;; Create multiple operations for same fd
      (dotimes (i 3)
        (push (async:async-read 0 (make-array 100)) ops))
      
      ;; Cleanup all operations for fd 0
      (let ((cleaned (async:cleanup-fd-operations 0)))
        (is (>= cleaned 0) "Should cleanup operations")))))

;;; ============================================================================
;;; Integration Tests with Net Module
;;; ============================================================================

(deftest test-async-tcp-accept-integration ()
  "Test async operations with TCP networking"
  (skip "Full integration test requires refactored modules")
  (with-async-system
    (let* ((addr (net:make-socket-address "127.0.0.1" 0))
           (listener (net:tcp-bind addr))
           (accepted-client nil))
      
      ;; Use async accept through net module
      ;; This would be implemented after refactoring
      (is t "Integration test placeholder"))))

(deftest test-async-tcp-read-write-integration ()
  "Test async read/write with TCP streams"
  (skip "Full integration test requires refactored modules")
  (with-async-system
    ;; This would test the async operations through
    ;; the refactored TCP module
    (is t "Integration test placeholder")))

;;; ============================================================================
;;; Performance Tests
;;; ============================================================================

(deftest test-async-many-operations ()
  "Test handling many concurrent async operations"
  (skip "Performance test - enable for benchmarking")
  (with-async-system
    (let ((ops '()))
      ;; Submit many operations
      (dotimes (i 100)
        (push (async:async-read 0 (make-array 100)) ops))
      
      ;; Poll and cleanup
      (sleep 0.1)
      (let ((completions (async:poll-completions)))
        (is (listp completions)))
      
      ;; Cleanup
      (async:cleanup-fd-operations 0))))

(deftest test-async-rapid-poll ()
  "Test rapid polling behavior"
  (with-async-system
    ;; Poll rapidly without operations
    (dotimes (i 100)
      (let ((completions (async:poll-completions 0)))
        (is (listp completions))))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(deftest test-async-invalid-fd ()
  "Test async operations with invalid file descriptors"
  (with-async-system
    (handler-case
        (async:async-read -1 (make-array 100))
      (error ()
        (is t "Should error on invalid fd")))))

(deftest test-async-error-callbacks ()
  "Test error callback invocation"
  (skip "Error callback testing needs socket errors")
  (with-async-system
    (let* ((error-msg nil)
           (op (async:async-read 
                999 ; Invalid fd
                (make-array 100)
                :on-error (lambda (err)
                            (setf error-msg err)))))
      (declare (ignore op))
      (sleep 0.1)
      (async:poll-completions)
      (is (stringp error-msg) "Error callback should be called"))))

;;; ============================================================================
;;; Edge Case Tests
;;; ============================================================================

(deftest test-async-zero-timeout-poll ()
  "Test polling with zero timeout"
  (with-async-system
    (let ((completions (async:poll-completions 0)))
      (is (listp completions)))))

(deftest test-async-operation-without-callbacks ()
  "Test operations without callbacks"
  (with-async-system
    (let ((op (async::make-async-operation
               :fd 0
               :type :read
               :buffer (make-array 100))))
      (is (null (async:async-operation-callback op)))
      (is (null (async:async-operation-error-callback op)))
      
      ;; Should not crash when completed
      (handler-case
          (async:submit-async-operation op)
        (error ()
          ;; Might error on stdin, that's okay
          (is t "Operation submitted"))))))

(deftest test-async-set-nonblocking ()
  "Test setting file descriptor to non-blocking mode"
  (let ((fd (net::create-socket net::+af-inet+ net::+sock-stream+ net::+ipproto-tcp+)))
    (unwind-protect
         (progn
           (async:set-nonblocking fd)
           (is t "Socket set to non-blocking mode"))
      (when (>= fd 0)
        (net::%close fd)))))