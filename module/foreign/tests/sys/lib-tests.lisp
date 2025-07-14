(defpackage epsilon.lib.sys.tests
  (:use
   cl
   epsilon.lib.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.sys.lib.impl)))

(in-package epsilon.lib.sys.tests)

;; Basic FFI tests using libc functions

;; Simple libc function bindings for testing

#++
(lib:defshared test-strlen "strlen" "libc" :unsigned-long (str :string)
  :documentation "Get length of C string")

#++
(lib:defshared test-malloc "malloc" "libc" :pointer (size :unsigned-long)
  :documentation "Allocate memory")

#++
(lib:defshared test-free "free" "libc" :void (ptr :pointer)
  :documentation "Free memory")

;;;; Basic FFI operation tests

#++
(deftest test-basic-library-loading
  "Test that we can load and close a library"
  (let ((handle (lib:lib-open "libc")))
    (is (not (null handle)))
    (is (lib:lib-close handle))))

#++
(deftest test-function-lookup
  "Test that we can find functions in a library"
  (let ((handle (lib:lib-open "libc")))
    (unwind-protect
         (let ((strlen-ptr (lib:lib-function handle "strlen")))
           (is (not (null strlen-ptr))))
      (lib:lib-close handle))))

#++
(deftest test-simple-string-function
  "Test calling a simple string function"
  (let ((result (test-strlen "hello")))
    (is (= result 5)))
  (let ((result (test-strlen "")))
    (is (= result 0)))
  (let ((result (test-strlen "test")))
    (is (= result 4))))

#++
(deftest test-memory-allocation
  "Test basic memory allocation and deallocation"
  (let ((ptr (test-malloc 1024)))
    (is (not (null ptr)))
    (test-free ptr) ; Should not crash
    t)) ; Success if we get here

#++
(deftest test-type-conversion
  "Test type conversion in shared-call"
  ;; Test that we can call functions with different argument types
  (is (= 5 (test-strlen "hello")))
  (is (= 0 (test-strlen ""))))

#++
(deftest test-multiple-calls
  "Test making multiple FFI calls"
  (let ((results (loop for i from 1 to 10
                       collect (test-strlen (make-string i :initial-element #\a)))))
    (is (equal results '(1 2 3 4 5 6 7 8 9 10)))))

;;;; Error handling tests

#++
(deftest test-nonexistent-library
  "Test error handling for nonexistent library"
  (is-thrown (error) (lib:lib-open "nonexistent-library-12345")))

#++
(deftest test-nonexistent-function
  "Test error handling for nonexistent function"
  (let ((handle (lib:lib-open "libc")))
    (unwind-protect
         (let ((fn-ptr (lib:lib-function handle "nonexistent_function_12345")))
           (is (null fn-ptr))) ; Should return nil for missing functions
      (lib:lib-close handle))))
