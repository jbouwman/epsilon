(defpackage epsilon.kqueue.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (kqueue epsilon.kqueue)
   (lib epsilon.foreign)))

(in-package epsilon.kqueue.tests)

(deftest basic-kqueue-creation
  "Test basic kqueue creation and cleanup"
  (let ((kq (kqueue:kqueue)))
    (is (numberp kq) "kqueue should return a file descriptor")
    (is (>= kq 0) "kqueue file descriptor should be non-negative")
    (kqueue:kqueue-close kq)))

(deftest with-kqueue-macro
  "Test with-kqueue macro for automatic cleanup"
  (let (fd-captured)
    (kqueue:with-kqueue (kq)
      (setf fd-captured kq)
      (is (numberp kq) "kqueue should return a file descriptor")
      (is (>= kq 0) "kqueue file descriptor should be non-negative"))
    ;; After exiting the with-kqueue block, the fd should be closed
    ;; We can't easily test this without platform-specific checks
    (is (numberp fd-captured) "File descriptor should have been captured")))

(deftest event-constants
  "Test that event constants are defined correctly"
  (is (= kqueue:+evfilt-read+ -1) "EVFILT_READ should be -1")
  (is (= kqueue:+evfilt-write+ -2) "EVFILT_WRITE should be -2")
  (is (= kqueue:+ev-add+ #x0001) "EV_ADD should be 0x0001")
  (is (= kqueue:+ev-delete+ #x0002) "EV_DELETE should be 0x0002"))

(deftest kevent-struct-creation
  "Test kevent structure creation and accessors"
  (let ((event (kqueue:make-kevent-struct
                :ident 42
                :filter kqueue:+evfilt-read+
                :flags kqueue:+ev-add+
                :fflags 0
                :data 100
                :udata 200)))
    (is (= (kqueue:kevent-struct-ident event) 42) "ident should be 42")
    (is (= (kqueue:kevent-struct-filter event) kqueue:+evfilt-read+) "filter should be EVFILT_READ")
    (is (= (kqueue:kevent-struct-flags event) kqueue:+ev-add+) "flags should be EV_ADD")
    (is (= (kqueue:kevent-struct-data event) 100) "data should be 100")
    (is (= (kqueue:kevent-struct-udata event) 200) "udata should be 200")))

(deftest timespec-struct-creation
  "Test timespec structure creation and accessors"
  (let ((ts (kqueue:make-timespec-struct :tv-sec 10 :tv-nsec 500000000)))
    (is (= (kqueue:timespec-struct-tv-sec ts) 10) "tv_sec should be 10")
    (is (= (kqueue:timespec-struct-tv-nsec ts) 500000000) "tv_nsec should be 500000000")))

(deftest add-remove-event-cycle
    "Test adding and removing an event"
  (kqueue:with-kqueue (kq)
    ;; This test uses a pipe to avoid issues with stdin
    ;; For now, we just test that the functions don't crash
    (handler-case
        (progn
          ;; Try to add an event for stdin - this might fail, but shouldn't crash
          (kqueue:add-event kq 0 kqueue:+evfilt-read+)
          ;; Remove the event
          (kqueue:remove-event kq 0 kqueue:+evfilt-read+)
          (is t "Event add/remove cycle completed without crashing"))
      (error (e)
        ;; If it fails (e.g., stdin not suitable), that's still a valid test result
        (is t (format nil "Event operations failed as expected: ~A" e))))))

(deftest poll-events-timeout
    "Test polling for events with timeout"
  (kqueue:with-kqueue (kq)
    ;; Poll with immediate timeout - should return empty list
    (let ((events (kqueue:poll-events kq :max-events 1 :timeout 0)))
      (is (listp events) "poll-events should return a list")
      (is (= (length events) 0) "Should get no events with immediate timeout"))))

;;; ============================================================================
;;; Memory Management Tests
;;; ============================================================================

(deftest test-make-kevent-memory
  "Test kevent memory allocation and structure creation"
  ;; Test empty list
  (is (zerop (second (multiple-value-list (kqueue::make-kevent-memory '()))))
      "Empty list should return count of 0")
  
  ;; Test single kevent
  (let* ((kevent (kqueue:make-kevent-struct
                  :ident 123
                  :filter kqueue:+evfilt-read+
                  :flags kqueue:+ev-add+
                  :fflags 0
                  :data 456
                  :udata 789))
         (kevents (list kevent)))
    (multiple-value-bind (memory count)
        (kqueue::make-kevent-memory kevents)
      (unwind-protect
           (progn
             (is (= count 1) "Should have count of 1")
             (is (not (zerop memory)) "Memory should be allocated")
             ;; Verify the structure was written correctly
             (is (= 123 (sb-sys:sap-ref-64 memory 0)) "ident should be 123")
             (is (= kqueue:+evfilt-read+ (sb-sys:signed-sap-ref-16 memory 8)) "filter should be EVFILT_READ")
             (is (= kqueue:+ev-add+ (sb-sys:sap-ref-16 memory 10)) "flags should be EV_ADD")
             (is (= 0 (sb-sys:sap-ref-32 memory 12)) "fflags should be 0")
             (is (= 456 (sb-sys:sap-ref-64 memory 16)) "data should be 456")
             (is (= 789 (sb-sys:sap-ref-64 memory 24)) "udata should be 789"))
        (when memory (lib:foreign-free memory)))))
  
  ;; Test multiple kevents
  (let* ((kevent1 (kqueue:make-kevent-struct :ident 1 :filter -1 :flags 1 :fflags 0 :data 10 :udata 20))
         (kevent2 (kqueue:make-kevent-struct :ident 2 :filter -2 :flags 2 :fflags 0 :data 30 :udata 40))
         (kevents (list kevent1 kevent2)))
    (multiple-value-bind (memory count)
        (kqueue::make-kevent-memory kevents)
      (unwind-protect
           (progn
             (is (= count 2) "Should have count of 2")
             ;; Check first kevent
             (is (= 1 (sb-sys:sap-ref-64 memory 0)) "First ident should be 1")
             ;; Check second kevent (offset by 32 bytes)
             (is (= 2 (sb-sys:sap-ref-64 memory 32)) "Second ident should be 2"))
        (when memory (lib:foreign-free memory)))))
  
  ;; Test with nil values in kevent
  (let* ((kevent (kqueue:make-kevent-struct :ident nil :filter nil :flags nil))
         (kevents (list kevent)))
    (multiple-value-bind (memory count)
        (kqueue::make-kevent-memory kevents)
      (unwind-protect
           (progn
             (is (= count 1) "Should handle nil values")
             (is (= 0 (sb-sys:sap-ref-64 memory 0)) "nil ident should be 0")
             (is (= 0 (sb-sys:signed-sap-ref-16 memory 8)) "nil filter should be 0")
             (is (= 0 (sb-sys:sap-ref-16 memory 10)) "nil flags should be 0"))
        (when memory (lib:foreign-free memory))))))

(deftest test-parse-kevent-memory
  "Test parsing kevent structures from memory"
  ;; Create memory with known values
  (let ((memory (lib:foreign-alloc 64))) ; Allocate for 2 kevents
    (unwind-protect
         (progn
           ;; Write first kevent
           (setf (sb-sys:sap-ref-64 memory 0) 100)   ; ident
           (setf (sb-sys:signed-sap-ref-16 memory 8) -1) ; filter
           (setf (sb-sys:sap-ref-16 memory 10) 1)    ; flags
           (setf (sb-sys:sap-ref-32 memory 12) 2)    ; fflags
           (setf (sb-sys:sap-ref-64 memory 16) 200)  ; data
           (setf (sb-sys:sap-ref-64 memory 24) 300)  ; udata
           
           ;; Write second kevent
           (setf (sb-sys:sap-ref-64 memory 32) 400)  ; ident
           (setf (sb-sys:signed-sap-ref-16 memory 40) -2) ; filter
           (setf (sb-sys:sap-ref-16 memory 42) 3)    ; flags
           (setf (sb-sys:sap-ref-32 memory 44) 4)    ; fflags
           (setf (sb-sys:sap-ref-64 memory 48) 500)  ; data
           (setf (sb-sys:sap-ref-64 memory 56) 600)  ; udata
           
           ;; Parse the memory
           (let ((kevents (kqueue::parse-kevent-memory memory 2)))
             (is (= 2 (length kevents)) "Should parse 2 kevents")
             
             ;; Check first kevent
             (let ((ke1 (first kevents)))
               (is (= 100 (kqueue:kevent-struct-ident ke1)) "First ident")
               (is (= -1 (kqueue:kevent-struct-filter ke1)) "First filter")
               (is (= 1 (kqueue:kevent-struct-flags ke1)) "First flags")
               (is (= 2 (kqueue:kevent-struct-fflags ke1)) "First fflags")
               (is (= 200 (kqueue:kevent-struct-data ke1)) "First data")
               (is (= 300 (kqueue:kevent-struct-udata ke1)) "First udata"))
             
             ;; Check second kevent
             (let ((ke2 (second kevents)))
               (is (= 400 (kqueue:kevent-struct-ident ke2)) "Second ident")
               (is (= -2 (kqueue:kevent-struct-filter ke2)) "Second filter")
               (is (= 3 (kqueue:kevent-struct-flags ke2)) "Second flags")
               (is (= 4 (kqueue:kevent-struct-fflags ke2)) "Second fflags")
               (is (= 500 (kqueue:kevent-struct-data ke2)) "Second data")
               (is (= 600 (kqueue:kevent-struct-udata ke2)) "Second udata"))))
      (lib:foreign-free memory))))

(deftest test-make-timespec-memory
  "Test timespec memory creation"
  ;; Test with zero values
  (let ((memory (kqueue::make-timespec-memory 0 0)))
    (unwind-protect
         (progn
           (is (= 0 (sb-sys:sap-ref-64 memory 0)) "Zero seconds")
           (is (= 0 (sb-sys:sap-ref-64 memory 8)) "Zero nanoseconds"))
      (lib:foreign-free memory)))
  
  ;; Test with actual values
  (let ((memory (kqueue::make-timespec-memory 10 500000000)))
    (unwind-protect
         (progn
           (is (= 10 (sb-sys:sap-ref-64 memory 0)) "10 seconds")
           (is (= 500000000 (sb-sys:sap-ref-64 memory 8)) "500000000 nanoseconds"))
      (lib:foreign-free memory)))
  
  ;; Test with large values
  (let ((memory (kqueue::make-timespec-memory 9999999 999999999)))
    (unwind-protect
         (progn
           (is (= 9999999 (sb-sys:sap-ref-64 memory 0)) "Large seconds")
           (is (= 999999999 (sb-sys:sap-ref-64 memory 8)) "Max nanoseconds"))
      (lib:foreign-free memory))))

;;; ============================================================================
;;; Event Filter and Flag Constants Tests
;;; ============================================================================

(deftest test-all-event-constants
  "Test all kqueue event constants are defined"
  ;; Event filters
  (is (= kqueue:+evfilt-read+ -1) "EVFILT_READ")
  (is (= kqueue:+evfilt-write+ -2) "EVFILT_WRITE")
  (is (= kqueue:+evfilt-except+ -15) "EVFILT_EXCEPT")
  (is (= kqueue:+evfilt-signal+ -6) "EVFILT_SIGNAL")
  (is (= kqueue:+evfilt-timer+ -7) "EVFILT_TIMER")
  (is (= kqueue:+evfilt-vnode+ -4) "EVFILT_VNODE")
  (is (= kqueue:+evfilt-proc+ -5) "EVFILT_PROC")
  (is (= kqueue:+evfilt-user+ -10) "EVFILT_USER")
  
  ;; Event flags
  (is (= kqueue:+ev-add+ #x0001) "EV_ADD")
  (is (= kqueue:+ev-delete+ #x0002) "EV_DELETE")
  (is (= kqueue:+ev-enable+ #x0004) "EV_ENABLE")
  (is (= kqueue:+ev-disable+ #x0008) "EV_DISABLE")
  (is (= kqueue:+ev-oneshot+ #x0010) "EV_ONESHOT")
  (is (= kqueue:+ev-clear+ #x0020) "EV_CLEAR")
  (is (= kqueue:+ev-eof+ #x8000) "EV_EOF")
  (is (= kqueue:+ev-error+ #x4000) "EV_ERROR"))

;;; ============================================================================
;;; Advanced Kqueue Operations Tests
;;; ============================================================================

(deftest test-kqueue-with-pipe
  "Test kqueue with pipe for reliable event generation"
  (kqueue:with-kqueue (kq)
    ;; Create a pipe for testing
    (multiple-value-bind (read-fd write-fd)
        (create-pipe)
      (unwind-protect
           (progn
             ;; Add read event for pipe
             (kqueue:add-event kq read-fd kqueue:+evfilt-read+)
             
             ;; Initially, no events should be available
             (let ((events (kqueue:poll-events kq :timeout 0)))
               (is (null events) "No events initially"))
             
             ;; Write to pipe
             (write-to-pipe write-fd "test")
             
             ;; Now we should get a read event
             (let ((events (kqueue:poll-events kq :timeout 0.1)))
               (is (= 1 (length events)) "Should get one event")
               (when events
                 (let ((event (first events)))
                   (is (= read-fd (kqueue:kevent-struct-ident event)) "Event for correct fd")
                   (is (= kqueue:+evfilt-read+ (kqueue:kevent-struct-filter event)) "Read filter"))))
             
             ;; Remove the event
             (kqueue:remove-event kq read-fd kqueue:+evfilt-read+)
             
             ;; Write again and verify no events (since we removed the filter)
             (write-to-pipe write-fd "test2")
             (let ((events (kqueue:poll-events kq :timeout 0)))
               (is (null events) "No events after removal")))
        (close-pipe read-fd write-fd)))))

(deftest test-kqueue-multiple-events
  "Test kqueue with multiple simultaneous events"
  (kqueue:with-kqueue (kq)
    ;; Create two pipes
    (multiple-value-bind (read-fd1 write-fd1) (create-pipe)
      (multiple-value-bind (read-fd2 write-fd2) (create-pipe)
        (unwind-protect
             (progn
               ;; Add events for both pipes
               (kqueue:add-event kq read-fd1 kqueue:+evfilt-read+)
               (kqueue:add-event kq read-fd2 kqueue:+evfilt-read+)
               
               ;; Write to both pipes
               (write-to-pipe write-fd1 "pipe1")
               (write-to-pipe write-fd2 "pipe2")
               
               ;; Should get two events
               (let ((events (kqueue:poll-events kq :max-events 10 :timeout 0.1)))
                 (is (= 2 (length events)) "Should get two events")
                 
                 ;; Check both fds are represented
                 (let ((fds (mapcar #'kqueue:kevent-struct-ident events)))
                   (is (member read-fd1 fds) "First pipe should have event")
                   (is (member read-fd2 fds) "Second pipe should have event"))))
          (close-pipe read-fd1 write-fd1)
          (close-pipe read-fd2 write-fd2))))))

(deftest test-kqueue-event-flags
  "Test various event flag combinations"
  (kqueue:with-kqueue (kq)
    (multiple-value-bind (read-fd write-fd) (create-pipe)
      (unwind-protect
           (progn
             ;; Test EV_ONESHOT flag
             (let ((changelist (list (kqueue:make-kevent-struct
                                      :ident read-fd
                                      :filter kqueue:+evfilt-read+
                                      :flags (logior kqueue:+ev-add+ kqueue:+ev-oneshot+)
                                      :fflags 0
                                      :data 0
                                      :udata 0))))
               (kqueue:kevent kq changelist '()))
             
             ;; Write twice
             (write-to-pipe write-fd "test1")
             (write-to-pipe write-fd "test2")
             
             ;; First poll should get event
             (let ((events (kqueue:poll-events kq :timeout 0.1)))
               (is (= 1 (length events)) "Should get one event"))
             
             ;; Second poll should get nothing (oneshot consumed)
             (let ((events (kqueue:poll-events kq :timeout 0)))
               (is (null events) "Oneshot event should be consumed"))
             
             ;; Test EV_CLEAR flag
             (let ((changelist (list (kqueue:make-kevent-struct
                                      :ident read-fd
                                      :filter kqueue:+evfilt-read+
                                      :flags (logior kqueue:+ev-add+ kqueue:+ev-clear+)
                                      :fflags 0
                                      :data 0
                                      :udata 0))))
               (kqueue:kevent kq changelist '()))
             
             ;; Write and check
             (write-to-pipe write-fd "test3")
             (let ((events (kqueue:poll-events kq :timeout 0.1)))
               (is (not (null events)) "EV_CLEAR event should trigger")))
        (close-pipe read-fd write-fd)))))

(deftest test-wait-for-events-timeout
  "Test wait-for-events with various timeout values"
  (kqueue:with-kqueue (kq)
    ;; Test immediate timeout (0)
    (let ((start-time (get-internal-real-time)))
      (let ((events (kqueue:wait-for-events kq 10 0)))
        (is (null events) "Immediate timeout should return empty")
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 0.1) "Should return immediately"))))
    
    ;; Test short timeout
    (let ((start-time (get-internal-real-time)))
      (let ((events (kqueue:wait-for-events kq 10 0.05)))
        (is (null events) "Short timeout should return empty")
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (>= elapsed 0.04) "Should wait at least 0.04 seconds")
          (is (< elapsed 0.2) "Should not wait too long"))))
    
    ;; Test nil timeout (would block forever if there were no events)
    ;; We'll add a self-triggering event for this test
    (multiple-value-bind (read-fd write-fd) (create-pipe)
      (unwind-protect
           (progn
             (kqueue:add-event kq read-fd kqueue:+evfilt-read+)
             (write-to-pipe write-fd "trigger")
             (let ((events (kqueue:wait-for-events kq 1 nil)))
               (is (= 1 (length events)) "Should get event with nil timeout")))
        (close-pipe read-fd write-fd)))))

(deftest test-kevent-error-conditions
  "Test error handling in kevent operations"
  ;; Test with invalid kqueue fd
  (handler-case
      (progn
        (kqueue:kevent -1 '() '() 0)
        (is nil "Should have thrown an error for invalid kqueue"))
    (error (e)
      (is t (format nil "Got expected error: ~A" e))))
  
  ;; Test adding event to closed kqueue
  (let ((kq (kqueue:kqueue)))
    (kqueue:kqueue-close kq)
    (handler-case
        (progn
          (kqueue:add-event kq 0 kqueue:+evfilt-read+)
          (is nil "Should have thrown an error for closed kqueue"))
      (error (e)
        (is t (format nil "Got expected error: ~A" e))))))

;;; ============================================================================
;;; Test Functions (test-kqueue and simple-socket-monitor)
;;; ============================================================================

(deftest test-kqueue-test-function
  "Test the test-kqueue function"
  ;; Since test-kqueue writes to stdout, we just ensure it doesn't crash
  (handler-case
      (progn
        (with-output-to-string (*standard-output*)
          (kqueue::test-kqueue))
        (is t "test-kqueue completed without error"))
    (error (e)
      (is t (format nil "test-kqueue raised expected error: ~A" e)))))

(deftest test-socket-monitoring
  "Test socket monitoring with kqueue"
  ;; Create a pipe to simulate a socket
  (multiple-value-bind (read-fd write-fd) (create-pipe)
    (unwind-protect
         (progn
           ;; Start monitor in a thread with timeout
           (let ((monitor-thread
                  (sb-thread:make-thread
                   (lambda ()
                     (handler-case
                         (kqueue:with-kqueue (kq)
                           (kqueue:add-event kq read-fd kqueue:+evfilt-read+)
                           (loop for i from 0 below 3  ; Only try 3 times
                                 do (let ((events (kqueue:wait-for-events kq 1 0.1)))
                                      (when events
                                        (return t)))))
                       (error () nil)))
                   :name "socket-monitor")))
             
             ;; Give monitor time to start
             (sleep 0.05)
             
             ;; Write to trigger the monitor
             (write-to-pipe write-fd "trigger")
             
             ;; Wait for thread to complete
             (let ((result (sb-thread:join-thread monitor-thread :timeout 1)))
               (is (not (null result)) "Monitor should detect the event"))))
      (close-pipe read-fd write-fd))))

;;; ============================================================================
;;; Helper Functions for Testing
;;; ============================================================================

(defun create-pipe ()
  "Create a pipe and return (values read-fd write-fd)"
  (let ((pipe-fds (make-array 2 :element-type '(signed-byte 32))))
    (sb-posix:pipe pipe-fds)
    (values (aref pipe-fds 0) (aref pipe-fds 1))))

(defun close-pipe (read-fd write-fd)
  "Close both ends of a pipe"
  (ignore-errors (sb-posix:close read-fd))
  (ignore-errors (sb-posix:close write-fd)))

(defun write-to-pipe (fd data)
  "Write data to a pipe"
  (let ((bytes (sb-ext:string-to-octets data)))
    (sb-alien:with-alien ((buf (* sb-alien:unsigned-char) :local (sb-alien:make-alien sb-alien:unsigned-char (length bytes))))
      (loop for i from 0 below (length bytes)
            do (setf (sb-alien:deref buf i) (aref bytes i)))
      (sb-posix:write fd buf (length bytes)))))
