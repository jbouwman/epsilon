(defpackage epsilon.sys.advanced-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign))
  (:enter t))

;;;; Advanced FFI tests - structs, arrays, callbacks

;; Test struct: timespec
;; struct timespec {
;;     time_t tv_sec;
;;     long   tv_nsec;
;; };

(lib:defshared clock-gettime "clock_gettime" nil :int
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
             (assert-true (= result 0)) ; Success
             ;; Read seconds from struct (first 8 bytes)
             (let ((seconds (sb-sys:sap-ref-64 timespec-ptr 0)))
               (assert-true (> seconds 0)) ; Should be positive epoch time
               (assert-true (< seconds 2000000000))))) ; Sanity check - before year 2033
      (lib:foreign-free timespec-ptr))))

;; Array handling tests
(lib:defshared qsort "qsort" nil :void
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
           (assert-true (= (sb-sys:sap-ref-32 array-ptr 0) 5))
           (assert-true (= (sb-sys:sap-ref-32 array-ptr 12) 1)))
      (lib:foreign-free array-ptr))))

;; Callback tests
(deftest test-callback-preparation
  "Test preparation for callback functionality"
  ;; For now, just test that we can create function pointers
  ;; Full callback implementation will come later
  ;; Use nil to access already-loaded libc symbols via default library
  (let ((fn-ptr (lib:lib-function (lib:lib-open nil) "strcmp")))
    (assert-true (not (null fn-ptr)))
    (assert-true (sb-sys:system-area-pointer-p fn-ptr))))

;; String array tests
(lib:defshared getenv "getenv" nil :pointer
  (name :string)
  :documentation "Get environment variable")

(lib:defshared setenv "setenv" nil :int
  (name :string) (value :string) (overwrite :int)
  :documentation "Set environment variable")

(deftest test-string-return
  "Test functions that return strings"
  ;; Set a test environment variable
  (let ((result (setenv "EPSILON_TEST_VAR" "test_value" 1)))
    (assert-true (= result 0)) ; Success

    ;; Get it back
    (let ((value-ptr (getenv "EPSILON_TEST_VAR")))
      (assert-true (not (sb-sys:sap= value-ptr (sb-sys:int-sap 0)))) ; Should not be NULL
      ;; Convert C string to Lisp string
      (when (not (sb-sys:sap= value-ptr (sb-sys:int-sap 0)))
        ;; Read C string manually
        (let ((value (with-output-to-string (s)
                       (loop for i from 0
                             for byte = (sb-sys:sap-ref-8 value-ptr i)
                             until (zerop byte)
                             do (write-char (code-char byte) s)))))
          (assert-true (string= value "test_value")))))))

;; File descriptor tests
(lib:defshared pipe "pipe" nil :int
  (pipefd :pointer)
  :documentation "Create pipe")

(lib:defshared libc-close "close" nil :int
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
             (assert-true (= result 0)) ; Success
             ;; Read the two file descriptors
             (let ((read-fd (sb-sys:sap-ref-32 fds 0))
                   (write-fd (sb-sys:sap-ref-32 fds 4)))
               (assert-true (>= read-fd 0))
               (assert-true (>= write-fd 0))
               (assert-true (not (= read-fd write-fd)))
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

;; Bitfield tests
(deftest test-bitfield-operations
  "Test bitfield handling"
  ;; Many C APIs use bitfields for flags
  ;; Test OR-ing flags together
  (let ((flags 0))
    (setf flags (logior flags #x001)) ; Add EPOLLIN
    (setf flags (logior flags #x004)) ; Add EPOLLOUT
    (assert-true (= flags #x005))
    (assert-true (not (zerop (logand flags #x001)))) ; Test EPOLLIN is set
    (assert-true (zerop (logand flags #x008)))))     ; Test EPOLLERR is not set
