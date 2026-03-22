(defpackage epsilon.foreign.trampoline-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (trampoline epsilon.foreign.trampoline))
  (:enter t))

;;;; Tests for the new trampoline-based FFI system

(deftest test-trampoline-creation
  "Test that we can create compiled trampolines"
  (let ((trampoline (trampoline:make-ffi-trampoline :int '())))
    (assert-true (functionp trampoline))
    ;; Test with getpid
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "getpid"))
           (result (funcall trampoline fn-addr)))
      (assert-true (integerp result))
      (assert-true (> result 0)))))

(deftest test-trampoline-with-args
  "Test trampolines with arguments"
  (let ((trampoline (trampoline:make-ffi-trampoline :unsigned-long '(:string))))
    (assert-true (functionp trampoline))
    ;; Test with strlen
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "strlen"))
           (test-string "Hello, World!")
           (result (funcall trampoline fn-addr test-string)))
      (assert-true (= result (length test-string))))))

(deftest test-signature-caching
  "Test that trampolines are cached by signature"
  (let ((trampoline1 (trampoline:get-or-create-trampoline :int '()))
        (trampoline2 (trampoline:get-or-create-trampoline :int '())))
    ;; Should return the same trampoline
    (assert-true (eq trampoline1 trampoline2)))

  (let ((trampoline1 (trampoline:get-or-create-trampoline :int '(:int)))
        (trampoline2 (trampoline:get-or-create-trampoline :int '(:int :int))))
    ;; Different signatures should give different trampolines
    (assert-true (not (eq trampoline1 trampoline2)))))

(deftest test-trampoline-performance
  "Test that trampolines are faster than eval"
  (skip "Trampoline performance benchmarking not yet stable")
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
            (assert-true (< trampoline-time eval-time))
            (format t "~%Trampoline: ~D ms, Eval: ~D ms, Speedup: ~,2Fx~%"
                    trampoline-time eval-time
                    (/ (float eval-time) trampoline-time))))))))

(deftest test-type-descriptor
  "Test C type descriptor system"
  (skip "Type descriptor system not yet implemented")
  (let ((int-type (lib::get-c-type :int)))
    (assert-true (lib::c-type-p int-type))
    (assert-true (eq (lib::c-type-base int-type) :int))
    (assert-true (= (lib::c-type-size int-type) 4))
    (assert-true (= (lib::c-type-alignment int-type) 4)))

  (let ((pointer-type (lib::get-c-type :pointer)))
    (assert-true (lib::c-type-p pointer-type))
    (assert-true (eq (lib::c-type-base pointer-type) :pointer))
    (assert-true (= (lib::c-type-size pointer-type) 8)) ; On 64-bit
    (assert-true (= (lib::c-type-alignment pointer-type) 8))))

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
      (assert-true (sb-sys:system-area-pointer-p sap))
      ;; Verify content
      (assert-true (= (sb-sys:sap-ref-8 sap 0) (char-code #\H)))
      (assert-true (= (sb-sys:sap-ref-8 sap 1) (char-code #\e)))
      (assert-true (= (sb-sys:sap-ref-8 sap 5) 0)))) ; Null terminator

  ;; Integer conversion
  (let* ((lisp-int 42)
         (c-int (lib::convert-to-foreign lisp-int :int)))
    (assert-true (= c-int 42)))

  ;; Pointer conversion
  (let ((null-ptr (lib::convert-to-foreign nil :pointer)))
    (assert-true (sb-sys:sap= null-ptr (sb-sys:int-sap 0)))))

(deftest test-enhanced-defshared
  "Test the enhanced defshared macro"
  ;; Define a function using the new system
  (lib:defshared-fast test-strlen "strlen" "libc" :unsigned-long
    (str :string))

  ;; Test it works
  (assert-true (= (test-strlen "Hello") 5))
  (assert-true (= (test-strlen "") 0))
  (assert-true (= (test-strlen "Test string") 11)))

(deftest test-multiple-argument-trampolines
  "Test trampolines with multiple arguments"
  (skip "Multiple argument trampoline support not yet implemented")
  (let ((trampoline (lib::make-ffi-trampoline :int '(:pointer :pointer :unsigned-long))))
    (assert-true (functionp trampoline))
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
             (assert-true (= (funcall trampoline fn-addr data1 data2 10) 0))
             ;; Make them different
             (setf (sb-sys:sap-ref-8 data2 5) 255)
             ;; Should not be equal
             (assert-true (/= (funcall trampoline fn-addr data1 data2 10) 0)))
        (lib:foreign-free data1)
        (lib:foreign-free data2)))))

(deftest test-void-return-trampolines
  "Test trampolines for void-returning functions"
  (skip "Void return trampolines not yet implemented")
  (let ((trampoline (lib::make-ffi-trampoline :void '(:pointer))))
    (assert-true (functionp trampoline))
    ;; Test with free (doesn't crash)
    (let* ((lib-handle (lib:lib-open "libc"))
           (fn-addr (lib:lib-function lib-handle "free"))
           (ptr (lib:foreign-alloc 100)))
      ;; This should not error
      (assert-true (null (funcall trampoline fn-addr ptr))))))

(deftest test-signature-registry
  "Test the signature registry system"
  (skip "Signature registry system not yet implemented")
  ;; Clear registry first
  (lib::clear-signature-registry)

  ;; Register some signatures
  (lib::register-signature 'my-getpid :int '())
  (lib::register-signature 'my-strlen :unsigned-long '(:string))

  ;; Retrieve them
  (let ((sig1 (lib::get-signature 'my-getpid))
        (sig2 (lib::get-signature 'my-strlen)))
    (assert-true (lib::ffi-signature-p sig1))
    (assert-true (lib::ffi-signature-p sig2))
    (assert-true (eq (lib::ffi-signature-return-type sig1) :int))
    (assert-true (equal (lib::ffi-signature-arg-types sig2) '(:string)))))
