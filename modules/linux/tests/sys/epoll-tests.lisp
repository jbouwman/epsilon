(defpackage epsilon.sys.epoll-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (epoll epsilon.sys.epoll)
   (lib epsilon.foreign)))

(in-package epsilon.sys.epoll-tests)

(deftest test-epoll-create
  "Test basic epoll creation"
  (let ((epfd (epoll:epoll-create)))
    (is (plusp epfd))
    (is (zerop (epoll:epoll-close epfd)))))

(deftest test-epoll-create1
  "Test epoll creation with flags"
  (let ((epfd (epoll:epoll-create1 epoll:+epoll-cloexec+)))
    (is (plusp epfd))
    (is (zerop (epoll:epoll-close epfd)))))

(deftest test-epoll-event-creation
  "Test epoll event structure creation"
  (let ((event (epoll:make-epoll-event 
                :events epoll:+epollin+
                :data (epoll:make-epoll-data :fd 42))))
    (is (= (epoll:epoll-event-events event) epoll:+epollin+))
    (is (= (epoll:epoll-data-fd (epoll:epoll-event-data event)) 42))))

;; Define system functions for testing
(lib:defshared test-pipe "pipe" "libc" :int ((pipefd :pointer)))
(lib:defshared test-write "write" "libc" :long
  ((fd :int) (buf :pointer) (count :unsigned-long)))
(lib:defshared test-close "close" "libc" :int ((fd :int)))

(deftest test-epoll-with-pipe
  "Test epoll with a pipe"
  ;; Create a pipe for testing
  (let ((pipe-fds (lib:foreign-alloc 8))) ; 2 ints
    (unwind-protect
         (progn
           ;; Create pipe using epsilon.foreign
           (is (zerop (test-pipe pipe-fds)))
           
           (let ((read-fd (sb-sys:sap-ref-32 pipe-fds 0))
                 (write-fd (sb-sys:sap-ref-32 pipe-fds 4)))
             
             ;; Test with epoll
             (epoll:with-epoll (epfd)
               ;; Add read end to epoll
               (epoll:add-event epfd read-fd epoll:+epollin+)
               
               ;; Initially, no events
               (let ((events (epoll:wait-for-events epfd 10 0))) ; 0ms timeout
                 (is (null events)))
               
               ;; Write some data
               (let ((data "test"))
                 (lib:with-foreign-memory ((buf (length data)))
                   (loop for i from 0 below (length data)
                         do (setf (sb-sys:sap-ref-8 buf i) 
                                  (char-code (char data i))))
                   (test-write write-fd buf (length data))))
               
               ;; Now should have an event
               (let ((events (epoll:wait-for-events epfd 10 100))) ; 100ms timeout
                 (is (= (length events) 1))
                 (when events
                   (let ((event (first events)))
                     (is (epoll:epoll-event-readable-p event))
                     (is (= (epoll:epoll-data-fd (epoll:epoll-event-data event)) 
                            read-fd))))))
             
             ;; Clean up
             (test-close read-fd)
             (test-close write-fd)))
      (lib:foreign-free pipe-fds))))

(deftest test-epoll-data-union
  "Test epoll_data union functionality"
  ;; Test fd storage
  (let ((data (epoll:make-epoll-data :fd 12345)))
    (is (= (epoll:epoll-data-fd data) 12345)))
  
  ;; Test u32 storage
  (let ((data (epoll:make-epoll-data :u32 #xDEADBEEF)))
    (is (= (epoll:epoll-data-u32 data) #xDEADBEEF)))
  
  ;; Test u64 storage
  (let ((data (epoll:make-epoll-data :u64 #x123456789ABCDEF0)))
    (is (= (epoll:epoll-data-u64 data) #x123456789ABCDEF0))))

(deftest test-epoll-event-predicates
  "Test event checking predicates"
  (let ((readable (epoll:make-epoll-event :events epoll:+epollin+))
        (writable (epoll:make-epoll-event :events epoll:+epollout+))
        (error-event (epoll:make-epoll-event :events epoll:+epollerr+))
        (combined (epoll:make-epoll-event 
                   :events (logior epoll:+epollin+ epoll:+epollout+))))
    
    (is (epoll:epoll-event-readable-p readable))
    (is (not (epoll:epoll-event-writable-p readable)))
    
    (is (epoll:epoll-event-writable-p writable))
    (is (not (epoll:epoll-event-readable-p writable)))
    
    (is (epoll:epoll-event-error-p error-event))
    
    (is (epoll:epoll-event-readable-p combined))
    (is (epoll:epoll-event-writable-p combined))))

(deftest test-epoll-memory-packing
  "Test epoll_event structure packing/unpacking"
  (lib:with-foreign-memory ((buffer 16))
    (let ((original (epoll:make-epoll-event
                     :events (logior epoll:+epollin+ epoll:+epollout+)
                     :data (epoll:make-epoll-data :fd 789))))
      
      ;; Pack and unpack
      (epoll::pack-epoll-event original buffer 0)
      (let ((unpacked (epoll::unpack-epoll-event buffer 0)))
        
        ;; Verify
        (is (= (epoll:epoll-event-events unpacked)
               (epoll:epoll-event-events original)))
        (is (= (epoll:epoll-event-data unpacked)
               (epoll:epoll-event-data original)))))))