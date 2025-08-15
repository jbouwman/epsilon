(defpackage epsilon.foreign.trampoline-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (trampoline epsilon.foreign.trampoline)))

(in-package epsilon.foreign.trampoline-tests)

;;;; Tests for the new trampoline-based FFI system

(deftest test-trampoline-creation
  "Test that we can create compiled trampolines"
  (let ((trampoline (trampoline:make-ffi-trampoline :int '())))
    (is (functionp trampoline))
    ;; Test with getpid
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "getpid"))
           (result (funcall trampoline fn-addr)))
      (is (integerp result))
      (is (> result 0)))))

(deftest test-trampoline-with-args
  "Test trampolines with arguments"
  (let ((trampoline (trampoline:make-ffi-trampoline :unsigned-long '(:string))))
    (is (functionp trampoline))
    ;; Test with strlen
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "strlen"))
           (test-string "Hello, World!")
           (result (funcall trampoline fn-addr test-string)))
      (is (= result (length test-string))))))

(deftest test-signature-caching
  "Test that trampolines are cached by signature"
  (let ((trampoline1 (trampoline:get-or-create-trampoline :int '()))
        (trampoline2 (trampoline:get-or-create-trampoline :int '())))
    ;; Should return the same trampoline
    (is (eq trampoline1 trampoline2)))
  
  (let ((trampoline1 (trampoline:get-or-create-trampoline :int '(:int)))
        (trampoline2 (trampoline:get-or-create-trampoline :int '(:int :int))))
    ;; Different signatures should give different trampolines
    (is (not (eq trampoline1 trampoline2)))))

(deftest test-trampoline-performance
  "Test that trampolines are faster than eval"
  (let* ((lib-handle (lib:lib-open "libc"))
         (fn-addr (lib:lib-function lib-handle "getpid"))
         (trampoline (lib::make-ffi-trampoline :int '()))
         (iterations 10000))
    
    ;; Warm up
    (dotimes (i 100)
      (funcall trampoline fn-addr))
    
    ;; Test trampoline performance
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (funcall trampoline fn-addr))
      (let ((trampoline-time (- (get-internal-real-time) start)))
        
        ;; Test current eval-based approach
        (let ((start2 (get-internal-real-time)))
          (dotimes (i iterations)
            (lib:shared-call (list "getpid" "libc") :int '()))
          (let ((eval-time (- (get-internal-real-time) start2)))
            
            ;; Trampoline should be faster
            (is (< trampoline-time eval-time))
            (format t "~%Trampoline: ~D ms, Eval: ~D ms, Speedup: ~,2Fx~%"
                    trampoline-time eval-time 
                    (/ (float eval-time) trampoline-time))))))))

(deftest test-type-descriptor
  "Test C type descriptor system"
  (let ((int-type (lib::get-c-type :int)))
    (is (lib::c-type-p int-type))
    (is (eq (lib::c-type-base int-type) :int))
    (is (= (lib::c-type-size int-type) 4))
    (is (= (lib::c-type-alignment int-type) 4)))
  
  (let ((pointer-type (lib::get-c-type :pointer)))
    (is (lib::c-type-p pointer-type))
    (is (eq (lib::c-type-base pointer-type) :pointer))
    (is (= (lib::c-type-size pointer-type) 8)) ; On 64-bit
    (is (= (lib::c-type-alignment pointer-type) 8))))

(deftest test-converter-functions
  "Test type conversion functions"
  ;; String to C string
  (let* ((lisp-string "Hello")
         (c-string (lib::convert-to-foreign lisp-string :string)))
    ;; sb-alien:make-alien-string returns an alien value, not a raw SAP
    ;; So we need to get the SAP from it
    (let ((sap (if (sb-sys:system-area-pointer-p c-string)
                   c-string
                   (sb-alien:alien-sap c-string))))
      (is (sb-sys:system-area-pointer-p sap))
      ;; Verify content
      (is (= (sb-sys:sap-ref-8 sap 0) (char-code #\H)))
      (is (= (sb-sys:sap-ref-8 sap 1) (char-code #\e)))
      (is (= (sb-sys:sap-ref-8 sap 5) 0)))) ; Null terminator
  
  ;; Integer conversion
  (let* ((lisp-int 42)
         (c-int (lib::convert-to-foreign lisp-int :int)))
    (is (= c-int 42)))
  
  ;; Pointer conversion
  (let ((null-ptr (lib::convert-to-foreign nil :pointer)))
    (is (sb-sys:sap= null-ptr (sb-sys:int-sap 0)))))

(deftest test-enhanced-defshared
  "Test the enhanced defshared macro"
  ;; Define a function using the new system
  (lib:defshared-fast test-strlen "strlen" "libc" :unsigned-long
    (str :string))
  
  ;; Test it works
  (is (= (test-strlen "Hello") 5))
  (is (= (test-strlen "") 0))
  (is (= (test-strlen "Test string") 11)))

(deftest test-multiple-argument-trampolines
  "Test trampolines with multiple arguments"
  (let ((trampoline (lib::make-ffi-trampoline :int '(:pointer :pointer :unsigned-long))))
    (is (functionp trampoline))
    ;; Test with memcmp
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "memcmp"))
           (data1 (lib:foreign-alloc 10))
           (data2 (lib:foreign-alloc 10)))
      (unwind-protect
           (progn
             ;; Initialize with same data
             (dotimes (i 10)
               (setf (sb-sys:sap-ref-8 data1 i) i)
               (setf (sb-sys:sap-ref-8 data2 i) i))
             ;; Should be equal
             (is (= (funcall trampoline fn-addr data1 data2 10) 0))
             ;; Make them different
             (setf (sb-sys:sap-ref-8 data2 5) 255)
             ;; Should not be equal
             (is (/= (funcall trampoline fn-addr data1 data2 10) 0)))
        (lib:foreign-free data1)
        (lib:foreign-free data2)))))

(deftest test-void-return-trampolines
  "Test trampolines for void-returning functions"
  (let ((trampoline (lib::make-ffi-trampoline :void '(:pointer))))
    (is (functionp trampoline))
    ;; Test with free (doesn't crash)
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "free"))
           (ptr (lib:foreign-alloc 100)))
      ;; This should not error
      (is (null (funcall trampoline fn-addr ptr))))))

(deftest test-signature-registry
  "Test the signature registry system"
  ;; Clear registry first
  (lib::clear-signature-registry)
  
  ;; Register some signatures
  (lib::register-signature 'my-getpid :int '())
  (lib::register-signature 'my-strlen :unsigned-long '(:string))
  
  ;; Retrieve them
  (let ((sig1 (lib::get-signature 'my-getpid))
        (sig2 (lib::get-signature 'my-strlen)))
    (is (lib::ffi-signature-p sig1))
    (is (lib::ffi-signature-p sig2))
    (is (eq (lib::ffi-signature-return-type sig1) :int))
    (is (equal (lib::ffi-signature-arg-types sig2) '(:string)))))