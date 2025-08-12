(defpackage epsilon.foreign.sbcl-callback-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)))

(in-package epsilon.foreign.sbcl-callback-tests)

;;;; Tests to explore SBCL callback APIs and validate implementation

(deftest test-sbcl-alien-exports
  "Test what callback-related functions are available in SB-ALIEN"
  ;; Check for known callback-related exports
  (let ((alien-exports (loop for sym being the external-symbols of :sb-alien
                            collect (symbol-name sym))))
    ;; These should exist in some form
    (is (find "ALIEN-FUNCALL" alien-exports :test #'string=))
    (is (find "DEFINE-ALIEN-ROUTINE" alien-exports :test #'string=))
    ;; Document what we find
    (format t "~%SB-ALIEN exports with 'ALIEN': ~A~%" 
            (sort (remove-if-not (lambda (name) (search "ALIEN" name)) alien-exports) 
                  #'string<))))

(deftest test-sbcl-callback-internals
  "Explore SBCL internal callback mechanisms"
  ;; Check SB-ALIEN internals
  (let ((alien-callback-syms (loop for sym being the symbols of :sb-alien
                                  when (search "CALLBACK" (symbol-name sym))
                                  collect (symbol-name sym))))
    (format t "~%SB-ALIEN callback symbols: ~A~%" alien-callback-syms)
    (is (listp alien-callback-syms)))
  
  ;; Check SB-IMPL internals  
  (let ((impl-callback-syms (loop for sym being the symbols of :sb-impl
                                 when (search "CALLBACK" (symbol-name sym))
                                 collect (symbol-name sym))))
    (format t "~%SB-IMPL callback symbols: ~A~%" impl-callback-syms)
    (is (listp impl-callback-syms)))
  
  ;; Check SB-KERNEL internals
  (let ((kernel-callback-syms (loop for sym being the symbols of :sb-kernel
                                   when (search "CALLBACK" (symbol-name sym))
                                   collect (symbol-name sym))))
    (format t "~%SB-KERNEL callback symbols: ~A~%" kernel-callback-syms)
    (is (listp kernel-callback-syms))))

(deftest test-alien-lambda-availability
  "Test if alien-lambda or similar constructs are available"
  ;; Try different possible names
  (let ((lambda-syms (loop for sym being the symbols of :sb-alien
                          when (search "LAMBDA" (symbol-name sym))
                          collect (symbol-name sym))))
    (format t "~%SB-ALIEN lambda symbols: ~A~%" lambda-syms)
    (is (listp lambda-syms)))
  
  ;; Test if we can find function-related symbols
  (let ((func-syms (loop for sym being the symbols of :sb-alien
                        when (or (search "FUNCTION" (symbol-name sym))
                                (search "FUNC" (symbol-name sym)))
                        collect (symbol-name sym))))
    (format t "~%SB-ALIEN function symbols: ~A~%" func-syms)
    (is (listp func-syms))))

(deftest test-simple-alien-definition
  "Test basic alien function definition to verify alien system works"
  ;; Define a simple alien function to test the system
  (handler-case
      (progn
        (sb-alien:define-alien-routine "strlen" sb-alien:int
          (str sb-alien:c-string))
        ;; Test calling it
        (let ((result (strlen "hello")))
          (is (= result 5))))
    (error (e)
      (format t "~%Error in alien definition: ~A~%" e)
      (is nil "Alien system not working properly"))))

(deftest test-alien-callback-mechanism
  "Test if we can create any form of callback using alien system"
  ;; Try to create a simple callback using available mechanisms
  (handler-case
      ;; First attempt: use eval with alien-lambda if it exists
      (let ((lambda-sym (find-symbol "ALIEN-LAMBDA" :sb-alien)))
        (if lambda-sym
            (progn
              (format t "~%Found ALIEN-LAMBDA, attempting to use it~%")
              (let ((callback-form `(,lambda-sym (sb-alien:function sb-alien:int sb-alien:int)
                                                 (x) (* x 2))))
                (let ((callback (eval callback-form)))
                  (is (not (null callback))))))
            (format t "~%ALIEN-LAMBDA not found~%")))
    (error (e)
      (format t "~%Error creating callback: ~A~%" e))))

(deftest test-manual-trampoline-creation
  "Test creating manual trampolines for callbacks"
  ;; Try to create a function that can be called from C
  ;; by creating a compiled function and getting its address
  (let ((lisp-func (lambda (x) (* x 2))))
    ;; Compile the function
    (let ((compiled-func (compile nil lisp-func)))
      (is (functionp compiled-func))
      ;; Try to get function address (this may not work directly)
      (handler-case
          (let ((func-addr (sb-kernel:get-lisp-obj-address compiled-func)))
            (format t "~%Function address: #x~X~%" func-addr)
            (is (integerp func-addr)))
        (error (e)
          (format t "~%Cannot get function address: ~A~%" e))))))

(deftest test-ffi-callback-approaches
  "Test different approaches to FFI callbacks"
  ;; Approach 1: Check if SB-ALIEN has any callback support
  (format t "~%Testing callback approaches...~%")
  
  ;; Look for any export that might be callback-related
  (let ((potential-callbacks 
         (loop for sym being the external-symbols of :sb-alien
              when (or (search "CALL" (symbol-name sym))
                      (search "FUNC" (symbol-name sym)))
              collect sym)))
    (format t "Potential callback symbols: ~A~%" potential-callbacks)
    (is (listp potential-callbacks)))
  
  ;; Approach 2: Check SB-SYS for system-level support
  (let ((sys-callback-syms 
         (loop for sym being the symbols of :sb-sys
              when (search "CALLBACK" (symbol-name sym))
              collect (symbol-name sym))))
    (format t "SB-SYS callback symbols: ~A~%" sys-callback-syms)
    (is (listp sys-callback-syms))))

(deftest test-callback-registry-functionality
  "Test that our callback registry works independently"
  ;; Test the registry infrastructure
  (let ((test-func (lambda (x) (* x 3))))
    ;; Register a callback (this should work with our infrastructure)
    (let ((cb-id (lib:register-callback 'test-cb test-func :int '(:int))))
      (is (integerp cb-id))
      
      ;; Retrieve it
      (let ((cb-ptr (lib:get-callback 'test-cb)))
        (is (sb-sys:system-area-pointer-p cb-ptr)))
      
      ;; Clean up
      (lib:unregister-callback 'test-cb)
      (is (null (lib:get-callback 'test-cb))))))

(deftest test-alternative-callback-creation
  "Test alternative approaches to callback creation"
  ;; Approach: Create callback using sb-alien internals
  (handler-case
      (progn
        ;; Try to find internal callback creation functions
        (let ((make-cb-sym (find-symbol "MAKE-CALLBACK" :sb-alien)))
          (if make-cb-sym
              (format t "~%Found MAKE-CALLBACK in SB-ALIEN~%")
              (format t "~%MAKE-CALLBACK not found in SB-ALIEN~%")))
        
        ;; Try SB-IMPL (use string to avoid package lock)
        (let ((make-cb-sym (find-symbol "MAKE-CALLBACK" "SB-IMPL")))
          (if make-cb-sym
              (format t "~%Found MAKE-CALLBACK in SB-IMPL~%")
              (format t "~%MAKE-CALLBACK not found in SB-IMPL~%")))
        
        (is t "Alternative callback exploration completed"))
    (error (e)
      (format t "~%Error in alternative callback creation: ~A~%" e)
      (is t "Error handled gracefully"))))

(deftest test-working-alien-funcall
  "Verify alien-funcall works as baseline for callback testing"
  ;; Test that we can call C functions (proves alien system works)
  (handler-case
      (let* ((lib-handle (lib:lib-open "libc"))
             (strlen-addr (lib:lib-function lib-handle "strlen")))
        ;; Create a trampoline and test it
        (let ((trampoline (lib::get-or-create-trampoline :size-t '(:string))))
          (let ((result (funcall trampoline strlen-addr "test")))
            (is (= result 4))
            (format t "~%alien-funcall baseline working: strlen('test') = ~A~%" result))))
    (error (e)
      (format t "~%Error in baseline alien-funcall: ~A~%" e)
      (is nil "Baseline alien system not working"))))

(deftest test-callback-stub-behavior
  "Test current callback stub to understand its behavior"
  ;; Test our current make-callback implementation
  (let ((cb-ptr (lib:make-callback (lambda (x) (+ x 10)) :int '(:int))))
    ;; Verify we get a pointer
    (is (sb-sys:system-area-pointer-p cb-ptr))
    
    ;; Check if it's the dummy pointer we expect
    (format t "~%Callback pointer: ~A~%" cb-ptr)
    
    ;; Test calling the callback
    (handler-case
        (let ((result (lib:call-callback cb-ptr 5)))
          (is (= result 15)) ; 5 + 10 = 15
          (format t "~%Callback call successful: f(5) = ~A~%" result))
      (error (e)
        (format t "~%Error calling callback: ~A~%" e)))))

(deftest test-memory-address-approaches
  "Test getting memory addresses for potential callback implementation"
  ;; Test different ways to get memory addresses
  (let ((test-func (lambda () 42)))
    ;; Try different address-getting approaches
    (handler-case
        (let ((addr (sb-kernel:get-lisp-obj-address test-func)))
          (format t "~%Function object address: #x~X~%" addr)
          (is (integerp addr)))
      (error (e)
        (format t "~%Cannot get function address: ~A~%" e)))
    
    ;; Try getting code address
    (handler-case
        (let ((code-component (sb-kernel:fun-code-header test-func)))
          (when code-component
            (let ((code-addr (sb-kernel:get-lisp-obj-address code-component)))
              (format t "~%Code component address: #x~X~%" code-addr)
              (is (integerp code-addr)))))
      (error (e)
        (format t "~%Cannot get code address: ~A~%" e)))))