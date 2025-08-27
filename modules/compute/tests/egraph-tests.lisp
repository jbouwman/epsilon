;;;; E-graph Tests

(defpackage epsilon.compute.egraph-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)
   (egraph epsilon.compute.egraph)))

(in-package epsilon.compute.egraph-tests)

(deftest test-egraph-creation
  "Test basic e-graph creation"
  (let ((eg (c:create-egraph)))
    (is (egraph:egraph-p eg))
    ;; Just test that it's created, don't check internals
    (is (not (null eg)))))

(deftest test-simple-addition
  "Test adding expressions to e-graph"
  (let* ((eg (c:create-egraph))
         (x (sym:sym 'x))
         (expr (c:+ x 0)))
    (let ((id (egraph:add-expr eg expr)))
      (is (integerp id))
      (is (> id 0)))))

(deftest test-equality-saturation
  "Test basic equality saturation"
  (skip "E-graph saturation creates too many equivalences - needs optimization")
  (let* ((x (sym:sym 'x))
         (expr (c:+ x 0)))
    ;; Should simplify x + 0 to x
    (let ((optimized (c:optimize-with-egraph expr)))
      (is (sym:var-p optimized))
      (is (eq (sym:var-name optimized) 'x)))))

(deftest test-associativity
  "Test associativity rewriting"
  (skip "egraph saturation and rule application not yet implemented")
  (let* ((x (sym:sym 'x))
         (y (sym:sym 'y))
         (z (sym:sym 'z))
         (expr1 (c:+ (c:+ x y) z))
         (expr2 (c:+ x (c:+ y z))))
    ;; Both should be recognized as equivalent
    (let ((eg (c:create-egraph)))
      (let ((id1 (egraph:add-expr eg expr1))
            (id2 (egraph:add-expr eg expr2)))
        (c:saturate-rules eg egraph:*standard-rules*)
        ;; After saturation, they should be in the same e-class
        (is (= (egraph:find-eclass eg id1)
               (egraph:find-eclass eg id2)))))))

(deftest test-commutativity
  "Test commutative rewriting"
  (skip "egraph saturation and rule application not yet implemented")
  (let* ((x (sym:sym 'x))
         (y (sym:sym 'y))
         (expr1 (c:+ x y))
         (expr2 (c:+ y x)))
    (let ((eg (c:create-egraph)))
      (let ((id1 (egraph:add-expr eg expr1))
            (id2 (egraph:add-expr eg expr2)))
        (c:saturate-rules eg egraph:*standard-rules*)
        (is (= (egraph:find-eclass eg id1)
               (egraph:find-eclass eg id2)))))))

(deftest test-distributivity
  "Test distribution rewriting"
  (skip "egraph saturation and rule application not yet implemented")
  (let* ((x (sym:sym 'x))
         (y (sym:sym 'y))
         (z (sym:sym 'z))
         (expr1 (c:* x (c:+ y z)))
         (expr2 (c:+ (c:* x y) (c:* x z))))
    (let ((eg (c:create-egraph)))
      (let ((id1 (egraph:add-expr eg expr1))
            (id2 (egraph:add-expr eg expr2)))
        (c:saturate-rules eg egraph:*standard-rules*)
        (is (= (egraph:find-eclass eg id1)
               (egraph:find-eclass eg id2)))))))

(deftest test-cost-extraction
  "Test cost-based extraction"
  (skip "optimize-with-egraph and cost extraction not yet implemented")
  (let* ((x (sym:sym 'x))
         (expr (c:+ (c:+ x 0) 0)))  ; Nested additions with zeros
    (let ((optimized (c:optimize-with-egraph expr)))
      ;; Should extract the simplest form: x
      (is (sym:var-p optimized))
      (is (eq (sym:var-name optimized) 'x)))))

(deftest test-multiplication-by-zero
  "Test multiplication by zero simplification"
  (skip "optimize-with-egraph not yet implemented")
  (let* ((x (sym:sym 'x))
         (expr (c:* x 0)))
    ;; Debug: check the expression structure
    (format t "~%MUL-ZERO: Expression: ~S~%" expr)
    (format t "MUL-ZERO: Expression type: ~S~%" (type-of expr))
    (format t "MUL-ZERO: Is expr-p?: ~S~%" (sym:expr-p expr))
    (let ((optimized (c:optimize-with-egraph expr)))
      ;; Debug output
      (unless (sym:const-p optimized)
        (format t "MUL-ZERO: Expected const, got: ~S~%" optimized))
      (is (sym:const-p optimized))
      (when (sym:const-p optimized)
        (is (zerop (sym:const-value optimized)))))))

(deftest test-multiplication-by-one
  "Test multiplication by one simplification"
  (skip "optimize-with-egraph not yet implemented")
  (let* ((x (sym:sym 'x))
         (expr (c:* x 1)))
    (let ((optimized (c:optimize-with-egraph expr)))
      (is (sym:var-p optimized))
      (is (eq (sym:var-name optimized) 'x)))))

(deftest test-complex-optimization
  "Test optimization of complex expression"
  (skip "optimize-with-egraph not yet implemented")
  (let* ((x (sym:sym 'x))
         (y (sym:sym 'y))
         ;; (x + 0) * (y + (0 * z)) should simplify to x * y
         (z (sym:sym 'z))
         (expr (c:* (c:+ x 0) (c:+ y (c:* 0 z)))))
    (let ((optimized (c:optimize-with-egraph expr)))
      ;; Should be equivalent to x * y
      (is (sym:expr-p optimized))
      (is (eq (sym:expr-op optimized) '*)))))

(deftest test-rebuild-performance
  "Test that rebuilding maintains correctness"
  (skip "saturate-rules and egraph rebuilding not yet implemented")
  (let* ((x (sym:sym 'x))
         (y (sym:sym 'y))
         (z (sym:sym 'z))
         (eg (c:create-egraph))
         ;; Create a complex expression with many equivalences
         (expr1 (c:+ x (c:+ y z)))
         (expr2 (c:+ (c:+ x y) z))
         (expr3 (c:+ y (c:+ x z))))
    
    (let ((id1 (egraph:add-expr eg expr1))
          (id2 (egraph:add-expr eg expr2))
          (id3 (egraph:add-expr eg expr3)))
      
      ;; Apply rules and rebuild
      (c:saturate-rules eg egraph:*standard-rules* :limit 5)
      
      ;; All should be in the same equivalence class
      (let ((canon1 (egraph:find-eclass eg id1))
            (canon2 (egraph:find-eclass eg id2))
            (canon3 (egraph:find-eclass eg id3)))
        (is (= canon1 canon2))
        (is (= canon2 canon3))))))