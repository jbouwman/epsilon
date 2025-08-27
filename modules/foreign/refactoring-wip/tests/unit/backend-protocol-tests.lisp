;;;; Backend Protocol Tests for epsilon.foreign
;;;;
;;;; Tests for the backend abstraction layer - written before implementation (TDD)

(defpackage epsilon.foreign.backend-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (#:fw epsilon.foreign.test-framework)
   (#:backend epsilon.foreign.backend.protocol)))

(in-package epsilon.foreign.backend-tests)

;;;; Backend Registration Tests

(deftest test-backend-registration ()
  "Test backend registration and discovery"
  ;; Test registering a new backend
  (let ((test-backend (make-instance 'backend:test-backend
                                      :name "test-backend"
                                      :capabilities '(:basic :strings))))
    (backend:register-backend :test test-backend)
    (is (eq (backend:find-backend :test) test-backend))
    
    ;; Test duplicate registration
    (is-error (backend:register-backend :test test-backend)
              'backend:backend-already-registered)
    
    ;; Test finding non-existent backend
    (is (null (backend:find-backend :nonexistent)))
    
    ;; Test listing all backends
    (let ((all-backends (backend:list-backends)))
      (is (member :test all-backends :key #'car)))
    
    ;; Clean up
    (backend:unregister-backend :test)
    (is (null (backend:find-backend :test)))))

(deftest test-backend-capabilities ()
  "Test backend capability reporting"
  (let ((basic-backend (make-instance 'backend:mock-backend
                                       :capabilities '(:basic)))
        (full-backend (make-instance 'backend:mock-backend
                                      :capabilities '(:basic :callbacks 
                                                      :structs-by-value :varargs))))
    
    ;; Test capability checking
    (is (backend:has-capability-p basic-backend :basic))
    (is (not (backend:has-capability-p basic-backend :callbacks)))
    
    (is (backend:has-capability-p full-backend :callbacks))
    (is (backend:has-capability-p full-backend :structs-by-value))
    
    ;; Test capability requirements
    (is (backend:supports-signature-p basic-backend 
                                       :int '(:int :pointer)))
    (is (not (backend:supports-signature-p basic-backend
                                            :struct '(:struct))))))

(deftest test-backend-selection ()
  "Test automatic backend selection based on requirements"
  ;; Register multiple test backends
  (let ((simple-backend (make-instance 'backend:mock-backend
                                        :name "simple"
                                        :capabilities '(:basic)
                                        :priority 10))
        (advanced-backend (make-instance 'backend:mock-backend
                                          :name "advanced"
                                          :capabilities '(:basic :callbacks :structs-by-value)
                                          :priority 20)))
    
    (backend:register-backend :simple simple-backend)
    (backend:register-backend :advanced advanced-backend)
    
    ;; Test selection for simple signature
    (is (eq (backend:select-backend-for-signature :int '(:int))
            simple-backend))
    
    ;; Test selection for complex signature requiring callbacks
    (is (eq (backend:select-backend-for-signature :void '(:callback))
            advanced-backend))
    
    ;; Test selection with no suitable backend
    (is-error (backend:select-backend-for-signature :void '(:unknown-type))
              'backend:no-suitable-backend)
    
    ;; Clean up
    (backend:unregister-backend :simple)
    (backend:unregister-backend :advanced)))

;;;; Backend Protocol Implementation Tests

(deftest test-backend-call-protocol ()
  "Test the backend call protocol"
  (let ((mock-backend (make-instance 'backend:mock-backend)))
    
    ;; Set up mock expectations
    (backend:expect-call mock-backend
                         :function-address #x12345678
                         :return-type :int
                         :arg-types '(:int :pointer)
                         :args '(42 #.(sb-sys:int-sap 0))
                         :return-value 84)
    
    ;; Test the call
    (let ((result (backend:backend-call mock-backend
                                         #x12345678
                                         :int
                                         '(:int :pointer)
                                         42
                                         #.(sb-sys:int-sap 0))))
      (is (= result 84)))
    
    ;; Verify expectations were met
    (is (backend:verify-expectations mock-backend))))

(deftest test-backend-memory-operations ()
  "Test backend memory allocation and deallocation"
  (let ((mock-backend (make-instance 'backend:mock-backend)))
    
    ;; Test allocation
    (let ((ptr (backend:backend-alloc mock-backend 1024)))
      (is (sb-sys:system-area-pointer-p ptr))
      (is (not (sb-sys:sap= ptr (sb-sys:int-sap 0))))
      
      ;; Test deallocation
      (backend:backend-free mock-backend ptr)
      
      ;; Test double-free detection
      (is-error (backend:backend-free mock-backend ptr)
                'backend:double-free-error))))

(deftest test-backend-error-handling ()
  "Test backend error propagation"
  (let ((failing-backend (make-instance 'backend:failing-backend)))
    
    ;; Test call failure
    (is-error (backend:backend-call failing-backend
                                     #x12345678 :int '() )
              'backend:backend-call-error)
    
    ;; Test allocation failure
    (is-error (backend:backend-alloc failing-backend 
                                      most-positive-fixnum)
              'backend:allocation-error)
    
    ;; Test graceful degradation
    (backend:with-fallback-backend (:simple)
      (let ((result (backend:backend-call failing-backend
                                           #x12345678 :int '() )))
        (is (integerp result))))))

;;;; Backend Performance Tests

(deftest test-backend-performance-characteristics ()
  "Test that backends report accurate performance characteristics"
  (let ((fast-backend (make-instance 'backend:trampoline-backend))
        (slow-backend (make-instance 'backend:libffi-backend)))
    
    ;; Test performance hints
    (is (< (backend:estimated-call-overhead fast-backend)
           (backend:estimated-call-overhead slow-backend)))
    
    ;; Test caching hints
    (is (backend:supports-caching-p fast-backend))
    
    ;; Test JIT compilation hints
    (is (backend:supports-jit-p fast-backend))
    (is (not (backend:supports-jit-p slow-backend)))))

;;;; Integration Tests

(deftest test-backend-switching ()
  "Test dynamic backend switching"
  (let ((backend-a (make-instance 'backend:mock-backend :name "A"))
        (backend-b (make-instance 'backend:mock-backend :name "B")))
    
    (backend:register-backend :a backend-a)
    (backend:register-backend :b backend-b)
    
    ;; Test default backend
    (is (eq backend:*default-backend* backend-a))
    
    ;; Test switching
    (backend:with-backend backend-b
      (is (eq backend:*current-backend* backend-b)))
    
    ;; Test restoration
    (is (eq backend:*current-backend* backend-a))
    
    ;; Clean up
    (backend:unregister-backend :a)
    (backend:unregister-backend :b)))

(deftest test-backend-thread-safety ()
  "Test thread-safe backend operations"
  (let ((backend (make-instance 'backend:thread-safe-backend))
        (results (make-array 10 :initial-element nil)))
    
    ;; Spawn multiple threads using the backend
    (let ((threads
           (loop for i below 10
                 collect (sb-thread:make-thread
                          (lambda (index)
                            (setf (aref results index)
                                  (backend:backend-call backend
                                                         #x1000
                                                         :int
                                                         '(:int)
                                                         index)))
                          :arguments (list i)))))
      
      ;; Wait for all threads
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      
      ;; Verify results
      (loop for i below 10
            do (is (= (aref results i) (* i 2)))))))

;;;; Mock Backend Implementation for Testing

(defclass mock-backend (backend:backend)
  ((expectations :initform '() :accessor backend-expectations)
   (calls :initform '() :accessor backend-calls)))

(defmethod backend:backend-call ((backend mock-backend) address return-type arg-types &rest args)
  (let ((call (list :address address 
                    :return-type return-type 
                    :arg-types arg-types 
                    :args args)))
    (push call (backend-calls backend))
    
    ;; Find matching expectation
    (let ((expectation (find-if (lambda (exp)
                                   (and (= (getf exp :function-address) address)
                                        (eq (getf exp :return-type) return-type)
                                        (equal (getf exp :arg-types) arg-types)
                                        (equal (getf exp :args) args)))
                                 (backend-expectations backend))))
      (if expectation
          (getf expectation :return-value)
          (error "Unexpected call: ~S" call)))))

(defun expect-call (backend &key function-address return-type arg-types args return-value)
  "Set up expectation for a backend call"
  (push (list :function-address function-address
              :return-type return-type
              :arg-types arg-types
              :args args
              :return-value return-value)
        (backend-expectations backend)))

(defun verify-expectations (backend)
  "Verify all expectations were met"
  (= (length (backend-expectations backend))
     (length (backend-calls backend))))