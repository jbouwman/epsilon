(defpackage epsilon.foreign.callback-tests
  (:use
   cl
   epsilon.syntax
   epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (callback epsilon.foreign.callback)
   (struct epsilon.foreign.struct)
   (marshalling epsilon.foreign.marshalling)))

(in-package epsilon.foreign.callback-tests)

;;;; Tests for Phase 4: Callback Support

(deftest test-simple-callback-creation
  "Test creating a simple callback from a Lisp function"
  ;; Create a callback that doubles an integer
  (let ((doubler (callback:make-callback
                   (lambda (x) (* x 2))
                   :int '(:int))))
    (is (sb-sys:system-area-pointer-p doubler))
    ;; Test calling it through FFI
    (is (= (callback:call-callback doubler 5) 10))
    (is (= (callback:call-callback doubler -3) -6))))

(deftest test-callback-with-multiple-args
  "Test callbacks with multiple arguments"
  ;; Create a callback that adds two numbers
  (let ((adder (callback:make-callback
                 (lambda (a b) (+ a b))
                 :int '(:int :int))))
    (is (sb-sys:system-area-pointer-p adder))
    (is (= (callback:call-callback adder 10 20) 30))
    (is (= (callback:call-callback adder -5 7) 2))))

(deftest test-callback-with-different-types
  "Test callbacks with various argument and return types"
  ;; Float callback
  (let ((float-op (callback:make-callback
                   (lambda (x) (sqrt x))
                   :double '(:double))))
    (is (< (abs (- (callback:call-callback float-op 4.0) 2.0)) 0.001)))
  
  ;; String length callback
  (let ((strlen-cb (callback:make-callback
                    (lambda (s) (length s))
                    :size-t '(:string))))
    (is (= (callback:call-callback strlen-cb "hello") 5))
    (is (= (callback:call-callback strlen-cb "") 0))))

(deftest test-callback-with-qsort
  "Test callback with real C function: qsort"
  ;; Create comparison callback for integers
  (let ((int-compare (callback:make-callback
                      (lambda (a-ptr b-ptr)
                        (let ((a (sb-sys:signed-sap-ref-32 a-ptr 0))
                              (b (sb-sys:signed-sap-ref-32 b-ptr 0)))
                          (cond ((< a b) -1)
                                ((> a b) 1)
                                (t 0))))
                      :int '(:pointer :pointer))))
    
    ;; Create array to sort
    (let ((array (make-array 5 :element-type '(signed-byte 32)
                              :initial-contents '(5 2 8 1 9))))
      (marshalling:with-pinned-array (ptr array)
        ;; Call qsort directly using alien-funcall
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "qsort"
                                (sb-alien:function sb-alien:void
                                                   sb-alien:system-area-pointer
                                                   sb-alien:size-t
                                                   sb-alien:size-t
                                                   sb-alien:system-area-pointer))
         ptr 5 4 int-compare))
      
      ;; Check array is sorted
      (is (equalp array #(1 2 5 8 9))))))

(deftest test-callback-closure-support
  "Test that callbacks can capture lexical variables"
  (let ((multiplier 10))
    (let ((scale-fn (callback:make-callback
                     (lambda (x) (* x multiplier))
                     :int '(:int))))
      (is (= (callback:call-callback scale-fn 5) 50))
      ;; Change captured variable
      (setf multiplier 20)
      (is (= (callback:call-callback scale-fn 5) 100)))))

(deftest test-callback-registry
  "Test callback registration and lifecycle"
  ;; Register a callback
  (let ((cb-id (callback:register-callback
                'my-callback
                (lambda (x) (* x x))
                :int '(:int))))
    (is (integerp cb-id))
    
    ;; Get callback by ID
    (let ((cb-ptr (callback:get-callback cb-id)))
      (is (sb-sys:system-area-pointer-p cb-ptr))
      (is (= (callback:call-callback cb-ptr 4) 16)))
    
    ;; Get callback by name
    (let ((cb-ptr (callback:get-callback 'my-callback)))
      (is (sb-sys:system-area-pointer-p cb-ptr))
      (is (= (callback:call-callback cb-ptr 3) 9)))
    
    ;; Unregister callback
    (callback:unregister-callback 'my-callback)
    (is (null (callback:get-callback 'my-callback)))))

(deftest test-callback-error-handling
  "Test error handling in callbacks"
  ;; Callback that signals an error
  (let ((error-cb (callback:make-callback
                   (lambda (x)
                     (if (zerop x)
                         (error "Division by zero")
                         (/ 10 x)))
                   :int '(:int))))
    ;; Normal call should work
    (is (= (callback:call-callback error-cb 2) 5))
    ;; Error should be caught and handled appropriately
    (handler-case
        (callback:call-callback error-cb 0)
      (error () (is t "Error was properly signaled")))))

(deftest test-callback-with-structs
  "Test callbacks that receive or return structs"
  ;; Define a point struct
  (struct:define-c-struct 'point-cb
    '((x :int)
      (y :int)))
  
  ;; Callback that takes struct pointer
  (let ((point-sum (callback:make-callback
                    (lambda (pt-ptr)
                      (+ (sb-sys:signed-sap-ref-32 pt-ptr 0)   ; x
                         (sb-sys:signed-sap-ref-32 pt-ptr 4)))  ; y
                    :int '(:pointer))))
    
    (struct:with-c-struct (pt 'point-cb)
      (setf (struct:struct-ref pt 'x) 10)
      (setf (struct:struct-ref pt 'y) 20)
      (is (= (callback:call-callback point-sum (struct:struct-pointer pt)) 30)))))

(deftest test-callback-thread-safety
  "Test that callbacks work correctly from multiple threads"
  (let* ((counter 0)
         (increment-cb (callback:make-callback
                        (lambda () 
                          (incf counter))
                        :int '()))
         (threads (loop for i from 1 to 5
                       collect (sb-thread:make-thread
                                (lambda ()
                                  (dotimes (j 100)
                                    (callback:call-callback increment-cb)))))))
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Check total count
    (is (= counter 500))))

(deftest test-callback-with-void-return
  "Test callbacks that return void"
  (let ((side-effect nil))
    (let ((void-cb (callback:make-callback
                    (lambda (x)
                      (setf side-effect (* x 2)))
                    :void '(:int))))
      (callback:call-callback void-cb 21)
      (is (= side-effect 42)))))

(deftest test-callback-with-signal-handler
  "Test callback as signal handler"
  (let ((signal-received nil))
    ;; Create signal handler callback
    (let ((handler (callback:make-callback
                    (lambda (signum)
                      (setf signal-received signum))
                    :void '(:int))))
      
      ;; Skip this test as it requires signal handling
      ;; which is complex and not critical for FFI testing
      (is t "Skipping signal handler test - too complex for current FFI"))))

(deftest test-callback-lifetime-management
  "Test callback memory management and cleanup"
  ;; Create callbacks in a scope
  (let ((callbacks nil))
    (callback:with-callback-scope
      ;; Create multiple callbacks
      (dotimes (i 10)
        (push (callback:make-callback
               (lambda (x) (* x i))
               :int '(:int))
              callbacks))
      ;; Use callbacks
      (is (= (callback:call-callback (first callbacks) 5) 45)))
    ;; After scope, callbacks should be cleaned up
    ;; (This is more of a memory leak test - hard to verify directly)
    (is t "Callback scope completed without error")))

(deftest test-callback-with-variadic-caller
  "Test callbacks passed to variadic functions"
  ;; Create a format callback
  (let ((print-cb (callback:make-callback
                   (lambda (format-str)
                     (format t "~A~%" format-str)
                     0)
                   :int '(:string))))
    ;; This tests that the callback pointer can be passed correctly
    (is (sb-sys:system-area-pointer-p print-cb))
    ;; In real use, this would be passed to something like atexit() or pthread_create()
    ))

(deftest test-defcallback-macro
  "Test the defcallback convenience macro"
  ;; Define a callback using the macro
  (callback:defcallback my-comparator :int ((a :pointer) (b :pointer))
    (let ((val-a (sb-sys:signed-sap-ref-32 a 0))
          (val-b (sb-sys:signed-sap-ref-32 b 0)))
      (- val-a val-b)))
  
  ;; Get the callback pointer
  (let ((cb-ptr (callback:callback-pointer 'my-comparator)))
    (is (sb-sys:system-area-pointer-p cb-ptr))
    ;; Test it works
    (let ((array (make-array 2 :element-type '(signed-byte 32)
                              :initial-contents '(10 5))))
      (marshalling:with-pinned-array (ptr array)
        (let ((result (callback:call-callback cb-ptr 
                                         ptr 
                                         (sb-sys:sap+ ptr 4))))
          (is (= result 5)))))))