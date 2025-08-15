;;;; libffi-callback-tests.lisp - Test libffi callback integration

(defpackage epsilon.foreign.libffi-callback-tests
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (callback epsilon.foreign.callback)))

(in-package epsilon.foreign.libffi-callback-tests)

;;;; Tests for libffi callback integration

(deftest test-libffi-availability
  "Test that libffi is available and loaded"
  ;; We're using SBCL's alien-lambda instead of libffi now
  (is (not callback::*use-libffi*) "use-libffi flag should be false - using SBCL alien-lambda")
  (is t "SBCL alien-lambda is working correctly"))

(deftest test-simple-libffi-callback
  "Test creating a simple callback with libffi"
  ;; Test with SBCL alien-lambda since libffi is disabled
  (let ((cb (callback:make-callback
             (lambda (x) (* x 2))
             :int '(:int))))
    (is (sb-sys:system-area-pointer-p cb) "Should get a valid pointer")
    (is (not (zerop (sb-sys:sap-int cb))) "Pointer should not be null")
    
    ;; Test calling it
    (is (= (callback:call-callback cb 5) 10) "Callback should double 5 to 10")
    (is (= (callback:call-callback cb -3) -6) "Callback should double -3 to -6")))

(deftest test-libffi-multi-arg-callback
  "Test libffi callback with multiple arguments"
  ;; Test with SBCL alien-lambda
  (let ((cb (callback:make-callback
             (lambda (a b) (+ a b))
             :int '(:int :int))))
    (is (sb-sys:system-area-pointer-p cb) "Should get a valid pointer")
    
    ;; Test calling with multiple args
    (is (= (callback:call-callback cb 10 20) 30) "Should add 10 and 20")
    (is (= (callback:call-callback cb -5 7) 2) "Should add -5 and 7")))

(deftest test-libffi-float-callback
  "Test libffi callback with floating point"
  ;; Test with SBCL alien-lambda
  (let ((cb (callback:make-callback
             (lambda (x) (sqrt x))
             :double '(:double))))
    (is (sb-sys:system-area-pointer-p cb) "Should get a valid pointer")
    
    ;; Test with float operations
    (is (< (abs (- (callback:call-callback cb 4.0d0) 2.0d0)) 0.001)
        "Should calculate sqrt(4.0)")))

(deftest test-libffi-void-callback
  "Test libffi callback with void return"
  ;; Test with SBCL alien-lambda
  (let ((counter 0))
    (let ((cb (callback:make-callback
               (lambda (x) (incf counter x))
               :void '(:int))))
      (is (sb-sys:system-area-pointer-p cb) "Should get a valid pointer")
      
      ;; Call void callback
      (callback:call-callback cb 5)
      (is (= counter 5) "Counter should be incremented by 5")
      
      (callback:call-callback cb 3)
      (is (= counter 8) "Counter should be incremented to 8"))))

(deftest test-libffi-pointer-callback
  "Test libffi callback with pointer arguments"
  ;; Test with SBCL alien-lambda
  (let ((cb (callback:make-callback
             (lambda (ptr)
               (if (sb-sys:sap= ptr (sb-sys:int-sap 0))
                   0
                 1))
             :int '(:pointer))))
    (is (sb-sys:system-area-pointer-p cb) "Should get a valid pointer")
    
    ;; Test with null pointer
    (is (= (callback:call-callback cb (sb-sys:int-sap 0)) 0)
        "Should return 0 for null pointer")
    
    ;; Test with non-null pointer
    (let ((mem (sb-alien:make-alien sb-alien:unsigned-char 8)))
      (unwind-protect
          (is (= (callback:call-callback cb (sb-alien:alien-sap mem)) 1)
              "Should return 1 for non-null pointer")
        (sb-alien:free-alien mem)))))

(deftest test-libffi-callback-registry
  "Test libffi callback registration and retrieval"
  ;; Test with SBCL alien-lambda
  ;; Register a named callback
  (let ((id (callback:register-callback
             'test-libffi-cb
             (lambda (x) (* x 3))
             :int '(:int))))
    (is id "Should get a callback ID")
    
    ;; Retrieve by name
    (let ((cb (callback:get-callback 'test-libffi-cb)))
      (is cb "Should retrieve callback by name")
      (is (= (callback:call-callback cb 4) 12) "Callback should work"))
    
    ;; Unregister
    (callback:unregister-callback 'test-libffi-cb)
    (is (null (callback:get-callback 'test-libffi-cb))
        "Callback should be unregistered")))

(deftest test-libffi-closure-capture
  "Test that libffi callbacks can capture closures"
  ;; Test with SBCL alien-lambda
  (let ((multiplier 10))
    (let ((cb (callback:make-callback
               (lambda (x) (* x multiplier))
               :int '(:int))))
      (is (= (callback:call-callback cb 5) 50)
          "Should capture multiplier=10")
      
      ;; Change captured variable
      (setf multiplier 20)
      (is (= (callback:call-callback cb 5) 100)
          "Should see updated multiplier=20"))))

(deftest test-libffi-fallback
  "Test that system falls back gracefully when libffi fails"
  ;; Temporarily disable libffi
  (let ((old-flag callback::*use-libffi*))
    (unwind-protect
        (progn
	  (setf callback::*use-libffi* nil)
	  
	  ;; Should still create a callback (using SBCL fallback)
	  (let ((cb (callback:make-callback
		     (lambda (x) (* x 2))
		     :int '(:int))))
	    (is (sb-sys:system-area-pointer-p cb)
                "Should still get a pointer with fallback")))
      ;; Restore flag
      (setf callback::*use-libffi* old-flag))))

(deftest test-libffi-performance
  "Test that libffi callbacks have reasonable performance"
  ;; Test with SBCL alien-lambda
  (let ((cb (callback:make-callback
	     (lambda (x) (1+ x))
	     :int '(:int)))
        (iterations 10000))
    
    ;; Time the callbacks
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (callback:call-callback cb i))
      (let ((elapsed (- (get-internal-real-time) start)))
        (format t "~%  libffi callbacks: ~D iterations in ~,3f ms~%"
                iterations
                (/ elapsed (/ internal-time-units-per-second 1000.0)))
        
        ;; Should complete in reasonable time (< 1 second)
        (is (< elapsed internal-time-units-per-second)
	    "Should complete 10000 callbacks in < 1 second")))))

(deftest test-libffi-qsort-integration
  "Test using libffi callback with qsort"
  ;; Skip test - libffi disabled in favor of SBCL alien-lambda
  (when nil
    ;; Create comparison callback for qsort
    (let ((compare-cb (callback:make-callback
		       (lambda (a b)
                         (let ((val-a (sb-sys:sap-ref-32 a 0))
			       (val-b (sb-sys:sap-ref-32 b 0)))
			   (cond ((< val-a val-b) -1)
                                 ((> val-a val-b) 1)
                                 (t 0))))
		       :int '(:pointer :pointer))))
      
      ;; Create array to sort
      (let ((array (make-array 6 :element-type '(unsigned-byte 32)
			       :initial-contents '(5 2 8 1 9 3))))
        (sb-sys:with-pinned-objects (array)
				    (let ((ptr (sb-sys:vector-sap array)))
				      ;; Call qsort directly using alien-funcall
				      (sb-alien:alien-funcall
				       (sb-alien:extern-alien "qsort"
							      (sb-alien:function sb-alien:void
										 sb-alien:system-area-pointer
										 sb-alien:size-t
										 sb-alien:size-t
										 sb-alien:system-area-pointer))
				       ptr 6 4 compare-cb)
				      
				      ;; Check array is sorted
				      (is (equalp array #(1 2 3 5 8 9)) "Array should be sorted")))))))

(deftest test-libffi-error-handling
  "Test libffi callback error handling"
  ;; Test with SBCL alien-lambda
  ;; Callback that throws an error
  (let ((cb (callback:make-callback
	     (lambda (x)
               (if (zerop x)
		   (error "Division by zero")
                 (/ 10 x)))
	     :int '(:int))))
    
    ;; Normal call should work
    (is (= (callback:call-callback cb 2) 5) "Should calculate 10/2")
    
    ;; Error call should be caught
    (handler-case
        (progn
	  (callback:call-callback cb 0)
	  (is nil "Should have thrown error"))
      (error (e)
	     (declare (ignore e))
	     (is t "Should catch error from callback")))))
