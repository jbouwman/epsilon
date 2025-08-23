(defpackage epsilon.sys.advanced-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.sys.advanced-tests)

;;;; Advanced FFI tests - structs, arrays, callbacks

;; Test struct: timespec
;; struct timespec {
;;     time_t tv_sec;
;;     long   tv_nsec;
;; };

(lib:defshared clock-gettime "clock_gettime" "libc" :int 
  (clockid :int) (tp :pointer)
  :documentation "Get time from clock")

(deftest test-struct-passing
  "Test passing structures to C functions"
  ;; Allocate space for timespec struct (16 bytes on 64-bit)
  (let ((timespec-ptr (lib:foreign-alloc 16)))
    (unwind-protect
         (progn
           ;; CLOCK_REALTIME = 0
           (let ((result (clock-gettime 0 timespec-ptr)))
             (is (= result 0)) ; Success
             ;; Read seconds from struct (first 8 bytes)
             (let ((seconds (sb-sys:sap-ref-64 timespec-ptr 0)))
               (is (> seconds 0)) ; Should be positive epoch time
               (is (< seconds 2000000000))))) ; Sanity check - before year 2033
      (lib:foreign-free timespec-ptr))))

;; Array handling tests
(lib:defshared qsort "qsort" "libc" :void
  (base :pointer) (nmemb :unsigned-long) (size :unsigned-long) (compar :pointer)
  :documentation "Sort array using comparator function")

(deftest test-array-handling
  "Test array passing and manipulation"
  ;; Create array of 5 integers
  (let* ((count 5)
         (int-size 4)
         (array-ptr (lib:foreign-alloc (* count int-size))))
    (unwind-protect
         (progn
           ;; Initialize array: [5, 2, 8, 1, 9]
           (setf (sb-sys:sap-ref-32 array-ptr 0) 5)
           (setf (sb-sys:sap-ref-32 array-ptr 4) 2)
           (setf (sb-sys:sap-ref-32 array-ptr 8) 8)
           (setf (sb-sys:sap-ref-32 array-ptr 12) 1)
           (setf (sb-sys:sap-ref-32 array-ptr 16) 9)
           
           ;; Verify initial values
           (is (= (sb-sys:sap-ref-32 array-ptr 0) 5))
           (is (= (sb-sys:sap-ref-32 array-ptr 12) 1)))
      (lib:foreign-free array-ptr))))

;; Callback tests
(deftest test-callback-preparation
  "Test preparation for callback functionality"
  ;; For now, just test that we can create function pointers
  ;; Full callback implementation will come later
  (let ((fn-ptr (lib:lib-function (lib:lib-open "libc") "strcmp")))
    (is (not (null fn-ptr)))
    (is (sb-sys:system-area-pointer-p fn-ptr))))

;; String array tests
(lib:defshared getenv "getenv" "libc" :pointer
  (name :string)
  :documentation "Get environment variable")

(lib:defshared setenv "setenv" "libc" :int
  (name :string) (value :string) (overwrite :int)
  :documentation "Set environment variable")

(deftest test-string-return
  "Test functions that return strings"
  ;; Set a test environment variable
  (let ((result (setenv "EPSILON_TEST_VAR" "test_value" 1)))
    (is (= result 0)) ; Success
    
    ;; Get it back
    (let ((value-ptr (getenv "EPSILON_TEST_VAR")))
      (is (not (sb-sys:sap= value-ptr (sb-sys:int-sap 0)))) ; Should not be NULL
      ;; Convert C string to Lisp string
      (when (not (sb-sys:sap= value-ptr (sb-sys:int-sap 0)))
        ;; Read C string manually
        (let ((value (with-output-to-string (s)
                       (loop for i from 0
                             for byte = (sb-sys:sap-ref-8 value-ptr i)
                             until (zerop byte)
                             do (write-char (code-char byte) s)))))
          (is (string= value "test_value")))))))

;; File descriptor tests
(lib:defshared pipe "pipe" "libc" :int
  (pipefd :pointer)
  :documentation "Create pipe")

(lib:defshared libc-close "close" "libc" :int
  (fd :int)
  :documentation "Close file descriptor")

(deftest test-output-parameters
  "Test functions with output parameters"
  ;; Allocate space for two file descriptors (2 * 4 bytes)
  (let ((fds (lib:foreign-alloc 8)))
    (unwind-protect
         (progn
           ;; Create pipe
           (let ((result (pipe fds)))
             (is (= result 0)) ; Success
             ;; Read the two file descriptors
             (let ((read-fd (sb-sys:sap-ref-32 fds 0))
                   (write-fd (sb-sys:sap-ref-32 fds 4)))
               (is (>= read-fd 0))
               (is (>= write-fd 0))
               (is (not (= read-fd write-fd)))
               ;; Clean up - close the file descriptors
               (libc-close read-fd)
               (libc-close write-fd))))
      (lib:foreign-free fds))))

;; Complex number tests (if supported)
#++
(deftest test-complex-numbers
  "Test complex number handling"
  ;; This would test _Complex types if we add support
  (skip "Complex number support not yet implemented"))

;; Union tests
(deftest test-union-handling
  "Test union type handling"
  ;; epoll_data_t is a union - we already handle it as a 64-bit value
  ;; This is effectively tested in the memory allocation tests
  (let ((data (lib:make-epoll-data :fd 42)))
    (is (= (lib:epoll-data-fd data) 42))))

;; Bitfield tests
(deftest test-bitfield-operations
  "Test bitfield handling"
  ;; Many C APIs use bitfields for flags
  ;; Test OR-ing flags together
  (let ((flags 0))
    (setf flags (logior flags #x001)) ; Add EPOLLIN
    (setf flags (logior flags #x004)) ; Add EPOLLOUT
    (is (= flags #x005))
    (is (not (zerop (logand flags #x001)))) ; Test EPOLLIN is set
    (is (zerop (logand flags #x008)))))     ; Test EPOLLERR is not set