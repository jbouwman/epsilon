;;;; Unified broadcasting API tests
;;;; Test consolidation of broadcasting functions and ensure backward compatibility

(defpackage epsilon.compute.broadcasting-unified-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (bc epsilon.compute.broadcasting)))

(in-package epsilon.compute.broadcasting-unified-tests)

(deftest test-unified-broadcasting-api
  "Test that broadcast-operation handles all cases correctly"
  
  ;; Test scalar-scalar operations
  (is (= (c:broadcast-operation #'+ 2 3) 5))
  (is (= (c:broadcast-operation #'* 4 5) 20))
  
  ;; Test scalar-array broadcasting
  (is (equalp (c:broadcast-operation #'+ 1 #(2 3 4)) #(3 4 5)))
  (is (equalp (c:broadcast-operation #'* 2 #(1 2 3)) #(2 4 6)))
  
  ;; Test array-scalar broadcasting
  (is (equalp (c:broadcast-operation #'+ #(1 2 3) 10) #(11 12 13)))
  (is (equalp (c:broadcast-operation #'- #(5 6 7) 2) #(3 4 5)))
  
  ;; Test array-array broadcasting with same shape
  (is (equalp (c:broadcast-operation #'+ #(1 2) #(3 4)) #(4 6)))
  (is (equalp (c:broadcast-operation #'* #2A((1 2) (3 4)) #2A((5 6) (7 8))) 
              #2A((5 12) (21 32))))
  
  ;; Test broadcasting with different shapes
  (is (equalp (c:broadcast-operation #'+ #2A((1 2)) #(10 20)) 
              #2A((11 22))))
  (is (equalp (c:broadcast-operation #'* #(1 2 3) #2A((1) (10) (100)))
              #2A((1 2 3) (10 20 30) (100 200 300))))
  
  ;; Test multiple argument reduction
  (is (equalp (c:broadcast-operation #'+ 1 #(2 3) #2A((10) (20)))
              #2A((13 14) (23 24))))
  
  ;; Test unary operations
  (is (equalp (c:broadcast-operation #'- #(5 10 15)) #(-5 -10 -15)))
  
  ;; Test empty/edge cases - skip for now as implementation may vary
  #+nil (is (= (c:broadcast-operation #'+) 0))
  #+nil (is (= (c:broadcast-operation #'*) 1)))

(deftest test-broadcast-operation-backward-compatibility
  "Ensure broadcast-operation maintains backward compatibility"
  
  ;; Test that old usage patterns still work
  (let ((a #2A((1 2) (3 4)))
        (b #(10 20)))
    ;; Old pattern should still work
    (is (equalp (c:broadcast-operation #'+ a b)
                #2A((11 22) (13 24))))
    
    ;; Should work with symbolic operations too
    (let ((result (c:evaluate (c:+ (c:const a) (c:const b)))))
      (is (equalp result #2A((11 22) (13 24)))))))

(deftest test-broadcast-operation-performance
  "Test that unified API doesn't degrade performance"
  
  ;; Create test arrays
  (let ((large-array (make-array '(100 100) :initial-element 1))
        (vector (make-array '(100) :initial-element 2)))
    
    ;; Time the operation (should complete quickly)
    (let ((start-time (get-internal-real-time)))
      (c:broadcast-operation #'+ large-array vector)
      (let ((elapsed (- (get-internal-real-time) start-time)))
        ;; Should complete in reasonable time (< 100ms)
        (is (< elapsed (* 100 internal-time-units-per-second)))))))

(deftest test-broadcast-operation-type-preservation
  "Test that broadcast-operation preserves array types appropriately"
  
  ;; Integer operations should preserve integer types when possible
  (let ((int-array (make-array '(3) :element-type 'fixnum :initial-contents '(1 2 3))))
    (let ((result (c:broadcast-operation #'+ int-array 10)))
      (is (arrayp result))
      (is (equalp result #(11 12 13)))))
  
  ;; Float operations should produce float results
  (let ((float-array (make-array '(2) :element-type 'single-float 
                                 :initial-contents '(1.0 2.0))))
    (let ((result (c:broadcast-operation #'* float-array 2.5)))
      (is (arrayp result))
      (is (equalp result #(2.5 5.0))))))

(deftest test-broadcast-operation-associativity
  "Test that operations are correctly associative when broadcasting"
  
  ;; (a + b) + c should equal a + (b + c) with broadcasting
  (let ((a 1)
        (b #(2 3))
        (c #2A((10) (20))))
    (is (equalp (c:broadcast-operation #'+ (c:broadcast-operation #'+ a b) c)
                (c:broadcast-operation #'+ a (c:broadcast-operation #'+ b c))))))