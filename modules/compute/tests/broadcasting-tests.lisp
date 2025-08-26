(defpackage epsilon.compute.broadcasting-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)
   (types epsilon.compute.types)))

(in-package epsilon.compute.broadcasting-tests)

(deftest test-broadcast-shapes-basic
  "Test basic shape broadcasting rules"
  ;; Scalar broadcasts to any shape
  (is (equal (c:broadcast-shapes '() '(3 4)) '(3 4)))
  (is (equal (c:broadcast-shapes '(3 4) '()) '(3 4)))
  
  ;; Same shapes are compatible
  (is (equal (c:broadcast-shapes '(3 4) '(3 4)) '(3 4)))
  
  ;; Broadcasting with dimension of size 1
  (is (equal (c:broadcast-shapes '(1 4) '(3 4)) '(3 4)))
  (is (equal (c:broadcast-shapes '(3 1) '(3 4)) '(3 4)))
  (is (equal (c:broadcast-shapes '(1) '(3 4)) '(3 4)))
  
  ;; Different rank tensors
  (is (equal (c:broadcast-shapes '(4) '(3 4)) '(3 4)))
  (is (equal (c:broadcast-shapes '(3 1 5) '(5)) '(3 1 5)))
  (is (equal (c:broadcast-shapes '(3 1 5) '(1 5)) '(3 1 5)))
  
  ;; Complex broadcasting
  (is (equal (c:broadcast-shapes '(2 1 3) '(1 4 1)) '(2 4 3))))

(deftest test-broadcast-shapes-errors
  "Test broadcasting shape incompatibility detection"
  ;; Incompatible shapes should signal error
  (is-thrown (error) (c:broadcast-shapes '(3 4) '(5 6)))
  (is-thrown (error) (c:broadcast-shapes '(3 4) '(3 5)))
  (is-thrown (error) (c:broadcast-shapes '(2 3) '(4)))
  
  ;; Dimension mismatch
  (is-thrown (error) (c:broadcast-shapes '(3 2) '(2 3))))

(deftest test-outer-product-basic
  "Test outer product operation"
  (skip "OUTER-PRODUCT operation not yet implemented")
  ;; Vector outer product
  (let* ((v1 (c:const #(1 2 3)))
         (v2 (c:const #(4 5)))
         (result (c:evaluate (c:outer-product v1 v2))))
    (is (arrayp result))
    (is (= (array-dimension result 0) 3))
    (is (= (array-dimension result 1) 2))
    (is (= (aref result 0 0) 4))
    (is (= (aref result 0 1) 5))
    (is (= (aref result 1 0) 8))
    (is (= (aref result 1 1) 10))
    (is (= (aref result 2 0) 12))
    (is (= (aref result 2 1) 15)))
  
  ;; Scalar-vector outer product
  (let* ((s (c:const 2))
         (v (c:const #(3 4 5)))
         (result (c:evaluate (c:outer-product s v))))
    (is (equalp result #(6 8 10))))
  
  ;; Matrix-vector outer product
  (let* ((m (c:const #2A((1 2) (3 4))))
         (v (c:const #(5 6)))
         (result (c:evaluate (c:outer-product m v))))
    ;; Result should be 3D tensor
    (is (= (array-rank result) 3))
    (is (= (array-dimension result 0) 2))
    (is (= (array-dimension result 1) 2))
    (is (= (array-dimension result 2) 2))))

(deftest test-broadcast-addition
  "Test broadcasting in addition"
  (skip "Broadcasting in addition not fully implemented")
  ;; Scalar + Vector
  (let ((result (c:evaluate (c:+ 1 (c:const #(2 3 4))))))
    (is (equalp result #(3 4 5))))
  
  ;; Vector + Vector (different shapes)
  (let ((result (c:evaluate (c:+ (c:const #(1 2 3)) 
                                 (c:const #2A((10) (20) (30)))))))
    (is (equalp result #2A((11) (22) (33)))))
  
  ;; Matrix + Vector (row broadcast)
  (let ((result (c:evaluate (c:+ (c:const #2A((1 2 3) (4 5 6)))
                                 (c:const #(10 20 30))))))
    (is (equalp result #2A((11 22 33) (14 25 36)))))
  
  ;; Matrix + Column vector
  (let ((result (c:evaluate (c:+ (c:const #2A((1 2) (3 4)))
                                 (c:const #2A((10) (20)))))))
    (is (equalp result #2A((11 12) (23 24))))))

(deftest test-broadcast-multiplication
  "Test broadcasting in multiplication"
  ;; Scalar * Matrix
  (let ((result (c:evaluate (c:* 2 (c:const #2A((1 2) (3 4)))))))
    (is (equalp result #2A((2 4) (6 8)))))
  
  ;; Vector * Vector (element-wise with broadcast)
  (let ((result (c:evaluate (c:* (c:const #(1 2))
                                 (c:const #2A((3) (4)))))))
    (is (equalp result #2A((3 6) (4 8)))))
  
  ;; Complex broadcast
  (let ((result (c:evaluate (c:* (c:const #3A(((1 2) (3 4))))
                                 (c:const #(5 6))))))
    (is (= (array-rank result) 3))
    (is (= (aref result 0 0 0) 5))
    (is (= (aref result 0 0 1) 12))))

(deftest test-broadcast-with-symbolic
  "Test broadcasting with symbolic expressions"
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         ;; x is scalar, y is vector
         (expr (c:+ x y))
         (result (c:evaluate expr '((x . 10) 
                                   (y . #(1 2 3))))))
    (is (equalp result #(11 12 13))))
  
  ;; Matrix variable + scalar
  (let* ((M (c:var 'M))
         (expr (c:* M 2))
         (result (c:evaluate expr '((M . #2A((1 2) (3 4)))))))
    (is (equalp result #2A((2 4) (6 8))))))

(deftest test-broadcast-reduction
  "Test broadcasting in reduction operations"
  (skip "Broadcasting in reduction operations not yet implemented")
  ;; Sum along axis with broadcasting
  (let* ((tensor (c:const #3A(((1 2) (3 4)) ((5 6) (7 8)))))
         (weights (c:const #(0.5 1.5)))
         ;; Weighted sum along first axis
         (weighted (c:* tensor weights))
         (result (c:evaluate (c:sum weighted :axis 0))))
    (is (= (array-rank result) 2))
    (is (= (array-dimension result 0) 2))
    (is (= (array-dimension result 1) 2))))

(deftest test-broadcast-shape-inference
  "Test shape inference with broadcasting"
  (skip "Shape inference with broadcasting not fully implemented")
  (let* ((x (c:var 'x :shape '(3 1)))
         (y (c:var 'y :shape '(1 4)))
         (z (c:+ x y)))
    (is (equal (c:infer-shape z) '(3 4))))
  
  ;; Chain of broadcasts
  (let* ((a (c:var 'a :shape '(5 1 3)))
         (b (c:var 'b :shape '(1 4 1)))
         (c (c:var 'c :shape '(3)))
         (result (c:+ (c:* a b) c)))
    (is (equal (c:infer-shape result) '(5 4 3)))))

(deftest test-broadcast-gradient
  "Test gradient computation with broadcasting"
  (skip "DIFF operation and gradient broadcasting not yet implemented")
  ;; Gradient of broadcasted operation should unbroadcast correctly
  (let* ((x (c:var 'x))  ; scalar
         (y (c:var 'y))  ; vector
         (loss (c:sum (c:* x y)))
         (grad-x (c:grad loss 'x))
         (grad-y (c:grad loss 'y)))
    ;; Gradient wrt scalar should sum all elements
    (let ((result-x (c:evaluate grad-x '((x . 2) (y . #(3 4 5))))))
      (is (= result-x 12)))  ; sum of y
    ;; Gradient wrt vector should broadcast scalar
    (let ((result-y (c:evaluate grad-y '((x . 2) (y . #(3 4 5))))))
      (is (equalp result-y #(2 2 2))))))

(deftest test-einsum-broadcasting
  "Test einsum with implicit broadcasting"
  (skip "Einsum with dtype features not yet implemented")
  ;; Batch matrix multiply with broadcasting
  (let* ((A (c:const #3A(((1 2) (3 4)))))  ; 1x2x2
         (B (c:const #2A((5 6) (7 8))))       ; 2x2
         ;; Should broadcast B to match A's batch dimension
         (result (c:evaluate (c:einsum "bij,jk->bik" A B))))
    (is (= (array-rank result) 3))
    (is (= (array-dimension result 0) 1))
    (is (equalp (aref result 0) #2A((19 22) (43 50)))))
  |#)

(deftest test-broadcast-memory-efficiency
  "Test that broadcasting doesn't unnecessarily copy data"
  (epsilon.test:skip "Memory optimization not yet implemented")
  #|
  ;; Large tensor broadcast should be lazy/efficient
  (let* ((large-shape '(1000 1000))
         (small-vec (c:const #(1 2 3 4 5)))
         (broadcast-op (c:+ (c:zeros large-shape) small-vec)))
    ;; Should not run out of memory or take too long
    (is (c:lazy-p broadcast-op))
    ;; Force evaluation of a small slice
    (let ((slice (c:slice broadcast-op '(0 5) '(0 5))))
      (is (arrayp (c:evaluate slice)))))
  |#)

(deftest test-broadcast-type-promotion
  "Test type promotion during broadcasting"
  (epsilon.test:skip "Type promotion not yet implemented")
  #|
  ;; Integer + Float should promote to float
  (let ((result (c:evaluate (c:+ (c:const 1 :dtype :int32)
                                 (c:const #(1.5 2.5) :dtype :float64)))))
    (is (equalp result #(2.5 3.5)))
    (is (eq (c:dtype-of result) :float64)))
  
  ;; Complex broadcasting
  (let ((result (c:evaluate (c:* (c:const #(1 2) :dtype :complex)
                                 (c:const 2.0 :dtype :float64)))))
    (is (eq (c:dtype-of result) :complex))))

(deftest test-broadcast-errors
  "Test error handling in broadcasting"
  ;; Incompatible shapes
  (is-thrown (error) 
    (c:evaluate (c:+ (c:const #(1 2 3)) (c:const #(4 5)))))
  
  ;; Invalid broadcast dimension
  (is-thrown (error)
    (c:evaluate (c:+ (c:const #2A((1 2) (3 4))) 
                    (c:const #(1 2 3)))))
  
  ;; Outer product dimension mismatch
  (is-thrown (type-error)
    (c:outer-product (c:const "not-a-tensor") (c:const #(1 2)))))