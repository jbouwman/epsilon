(defpackage epsilon.foreign.callback-api-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (impl epsilon.foreign.callback-impl)
   (callback epsilon.foreign.callback)))

(in-package epsilon.foreign.callback-api-tests)

;;;; Safe tests to explore SBCL callback APIs without crashes

(deftest test-callback-api-exploration
  "Safely explore what callback APIs are available"
  ;; This should run without crashing
  (let ((result (impl:test-callback-apis)))
    (is (eq result t))))

(deftest test-simple-callback-creation
  "Test creating a simple callback that we can call safely"
  ;; Create callback with our dummy implementation
  (let ((cb (callback:make-callback (lambda (x) (* x 2)) :int '(:int))))
    (is (sb-sys:system-area-pointer-p cb))
    ;; Test that we can call it safely
    (let ((result (callback:call-callback cb 5)))
      (is (= result 10)))))

(deftest test-callback-registry-operations
  "Test callback registry operations work correctly"
  ;; Test registration
  (let ((cb-id (callback:register-callback 'test-cb 
                                      (lambda (a b) (+ a b))
                                      :int '(:int :int))))
    (is (integerp cb-id))
    
    ;; Test retrieval
    (let ((cb-ptr (callback:get-callback 'test-cb)))
      (is (sb-sys:system-area-pointer-p cb-ptr))
      
      ;; Test calling
      (let ((result (callback:call-callback cb-ptr 3 7)))
        (is (= result 10)))
      
      ;; Test cleanup
      (callback:unregister-callback 'test-cb)
      (is (null (callback:get-callback 'test-cb))))))

(deftest test-defcallback-macro
  "Test the defcallback convenience macro"
  ;; Define a callback
  (callback:defcallback test-multiply :int ((x :int) (factor :int))
    (* x factor))
  
  ;; Get its pointer
  (let ((cb-ptr (callback:callback-pointer 'test-multiply)))
    (is (sb-sys:system-area-pointer-p cb-ptr))
    
    ;; Test calling it
    (let ((result (callback:call-callback cb-ptr 6 7)))
      (is (= result 42)))
    
    ;; Clean up
    (callback:unregister-callback 'test-multiply)))

(deftest test-closure-capture
  "Test that callbacks can capture lexical variables"
  (let ((multiplier 10))
    (let ((cb (callback:make-callback 
               (lambda (x) (* x multiplier))
               :int '(:int))))
      ;; Test initial value
      (let ((result1 (callback:call-callback cb 5)))
        (is (= result1 50)))
      
      ;; Change captured variable
      (setf multiplier 20)
      
      ;; Test updated value
      (let ((result2 (callback:call-callback cb 5)))
        (is (= result2 100))))))

(deftest test-real-callback-creation
  "Test attempting to create real callbacks"
  ;; Try to create a real callback
  (let ((real-cb (impl:create-real-callback 
                  (lambda (x) (+ x 1))
                  :int '(:int))))
    (is (sb-sys:system-area-pointer-p real-cb))
    (format t "~%Real callback pointer: ~A~%" real-cb)))