(defpackage epsilon.compute.matrix-ops-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.matrix-ops-tests)

(deftest test-determinant-2x2
  "Test 2x2 matrix determinant evaluation"
  (let* ((matrix (c:const #2A((1 2) (3 4))))
         (det-expr (c:det matrix))
         (result (c:evaluate det-expr)))
    (is (= result -2))))

(deftest test-determinant-symbolic
  "Test symbolic determinant expression creation"
  (let* ((matrix (c:var 'A))
         (det-expr (c:det matrix)))
    (is (sym:expr-p det-expr))
    (is (eq (sym:expr-op det-expr) 'det))))

(deftest test-cross-product-basic
  "Test basic cross product evaluation"
  (let* ((v1 (c:const #(1 0 0)))
         (v2 (c:const #(0 1 0)))
         (cross-expr (c:cross v1 v2))
         (result (c:evaluate cross-expr)))
    (is (equalp result #(0 0 1)))))

(deftest test-cross-product-general
  "Test general cross product evaluation"
  (let* ((v1 (c:const #(1 2 3)))
         (v2 (c:const #(4 5 6)))
         (cross-expr (c:cross v1 v2))
         (result (c:evaluate cross-expr)))
    (is (equalp result #(-3 6 -3)))))

(deftest test-cross-product-symbolic  
  "Test symbolic cross product expression creation"
  (let* ((v1 (c:var 'u))
         (v2 (c:var 'v))
         (cross-expr (c:cross v1 v2)))
    (is (sym:expr-p cross-expr))
    (is (eq (sym:expr-op cross-expr) 'cross))))