;;;; Broadcasting error handling tests
;;;; Ensure proper error detection and helpful error messages

(defpackage epsilon.compute.broadcasting-error-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (bc epsilon.compute.broadcasting)))

(in-package epsilon.compute.broadcasting-error-tests)

(deftest test-broadcasting-shape-errors
  "Test that incompatible shapes produce clear errors"
  
  ;; Incompatible dimensions should error
  (is-thrown (error) (c:broadcast-operation #'+ #(1 2 3) #(4 5)))
  (is-thrown (error) (c:broadcast-operation #'* #2A((1 2)) #2A((3 4 5))))
  
  ;; Non-broadcastable shapes
  (is-thrown (error) (c:broadcast-shapes '(3 4) '(5 6)))
  (is-thrown (error) (c:broadcast-shapes '(2 3) '(3 2)))
  
  ;; Dimension mismatch that can't be broadcast
  (is-thrown (error) (c:broadcast-operation #'+ 
                                            #3A(((1 2) (3 4)))
                                            #2A((5 6 7)))))

(deftest test-broadcasting-type-errors
  "Test type validation in broadcasting operations"
  
  ;; Non-numeric operations should error appropriately
  (is-thrown (type-error) (c:broadcast-operation #'+ "string" #(1 2 3)))
  (is-thrown (type-error) (c:broadcast-operation #'* #(1 2) :keyword))
  
  ;; NIL and empty lists should be handled gracefully
  (is-thrown (error) (c:broadcast-operation #'+ nil #(1 2 3)))
  (is-thrown (error) (c:broadcast-operation #'+ #(1 2 3) '())))

(deftest test-broadcasting-memory-limits
  "Test that memory limits are enforced"
  
  ;; Attempting to create result larger than memory limit should error
  ;; This is a placeholder - actual limit depends on implementation
  (let ((huge-dim 1000000))  ; 1M elements
    ;; Should handle large but reasonable arrays
    (let ((result (c:broadcast-operation #'+ 
                                         (make-array huge-dim :initial-element 1)
                                         1)))
      (is (= (array-dimension result 0) huge-dim)))
    
    ;; But should error on truly massive operations
    ;; Note: This test is disabled by default to avoid OOM
    #+nil
    (is-thrown (error) 
      (c:broadcast-operation #'* 
                            (make-array '(1000000 1000) :initial-element 1)
                            (make-array '(1000 1000000) :initial-element 1)))))

(deftest test-broadcasting-error-messages
  "Test that error messages are helpful and informative"
  
  ;; Check that error messages contain useful information
  (handler-case 
      (c:broadcast-shapes '(3 4) '(5 6))
    (error (e)
      ;; Error message should mention the shapes
      (let ((msg (princ-to-string e)))
        (is (search "3" msg))
        (is (search "4" msg))
        (is (search "5" msg))
        (is (search "6" msg))
        ;; Should indicate it's a broadcasting error
        (is (or (search "broadcast" (string-downcase msg))
                (search "incompatible" (string-downcase msg)))))))
  
  ;; Test dimension mismatch error message
  (handler-case
      (c:broadcast-operation #'+ #(1 2 3) #(4 5))
    (error (e)
      (let ((msg (string-downcase (princ-to-string e))))
        ;; Should mention the actual dimensions
        (is (or (search "3" msg) (search "three" msg)))
        (is (or (search "2" msg) (search "two" msg)))))))

(deftest test-broadcasting-invalid-operations
  "Test handling of invalid operations"
  
  ;; Non-function first argument
  (is-thrown (error) (c:broadcast-operation "not-a-function" 1 2))
  (is-thrown (error) (c:broadcast-operation nil #(1 2) #(3 4)))
  
  ;; Operations that don't make sense for arrays
  (is-thrown (error) (c:broadcast-operation #'eq #(1 2) #(1 2)))
  
  ;; Division by zero should signal an error
  (is-thrown (division-by-zero) 
    (c:broadcast-operation #'/ #(1.0 2.0 3.0) 0))

(deftest test-broadcasting-edge-cases
  "Test edge cases in broadcasting"
  
  ;; Empty arrays
  (is (equalp (c:broadcast-operation #'+ #() #()) #()))
  
  ;; Single element arrays
  (is (equalp (c:broadcast-operation #'* #(5) #(3)) #(15)))
  
  ;; Zero-dimensional arrays (scalars wrapped in arrays)
  (let ((scalar-array (make-array '() :initial-element 10)))
    (is (= (c:broadcast-operation #'+ scalar-array 5) 15)))
  
  ;; Very high dimensional arrays (if supported)
  (let ((high-dim (make-array '(1 1 1 1 1 1 1 1) :initial-element 2)))
    (is (equalp (array-dimensions 
                 (c:broadcast-operation #'* high-dim 3))
                '(1 1 1 1 1 1 1 1)))))

(deftest test-broadcasting-function-arity
  "Test that operations handle wrong number of arguments correctly"
  
  ;; Unary operations with too many args
  (is-thrown (error) (c:broadcast-operation #'sqrt #(4 9) #(16 25)))
  
  ;; Binary operations with wrong number of args
  ;; Actually, broadcast-operation should handle multiple args via reduction
  (is (numberp (c:broadcast-operation #'+ 1 2 3 4 5)))
  
  ;; But some operations don't make sense with multiple args
  (is-thrown (error) (c:broadcast-operation #'/ 10 2 2)))) ; Should this be 10/2/2=2.5 or error?

(deftest test-broadcasting-special-values
  "Test handling of special numeric values"
  
  ;; Large number handling
  (let ((large-val most-positive-fixnum))
    (is (arrayp (c:broadcast-operation #'+ large-val #(1 2 3)))))
  
  ;; Small number handling  
  (let ((small-val least-positive-single-float))
    (is (arrayp (c:broadcast-operation #'* small-val #(1.0 2.0 3.0)))))
  
  ;; Zero handling
  (is (equalp (c:broadcast-operation #'* 0 #(1 2 3)) #(0 0 0)))
  (is (equalp (c:broadcast-operation #'* #(1 2 3) 0) #(0 0 0))))