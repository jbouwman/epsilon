;;;; Debug test to understand broadcast-operation behavior

(defpackage epsilon.compute.test-debug
  (:use cl)
  (:local-nicknames
   (c epsilon.compute)))

(in-package epsilon.compute.test-debug)

(defun run-debug-tests ()
  "Run debug tests to understand what's happening"
  (format t "~%=== Testing broadcast-operation ===~%")
  
  ;; Test 1: Simple scalar addition
  (handler-case
      (let ((result (c:broadcast-operation #'+ 2 3)))
        (format t "Test 1: (broadcast-operation #'+ 2 3) => ~A~%" result))
    (error (e)
      (format t "Test 1 ERROR: ~A~%" e)))
  
  ;; Test 2: Scalar-array broadcasting
  (handler-case
      (let ((result (c:broadcast-operation #'+ 1 #(2 3 4))))
        (format t "Test 2: (broadcast-operation #'+ 1 #(2 3 4)) => ~A~%" result))
    (error (e) 
      (format t "Test 2 ERROR: ~A~%" e)))
  
  ;; Test 3: Unary minus
  (handler-case
      (let ((result (c:broadcast-operation #'- #(5 10 15))))
        (format t "Test 3: (broadcast-operation #'- #(5 10 15)) => ~A~%" result))
    (error (e)
      (format t "Test 3 ERROR: ~A~%" e)))
  
  ;; Test 4: Large number
  (handler-case
      (let ((result (c:broadcast-operation #'+ most-positive-fixnum #(1 2 3))))
        (format t "Test 4: Large number result type: ~A, value sample: ~A~%" 
                (type-of result) 
                (when (arrayp result) (aref result 0))))
    (error (e)
      (format t "Test 4 ERROR: ~A~%" e)))
  
  (format t "~%=== End debug tests ===~%"))

(run-debug-tests)