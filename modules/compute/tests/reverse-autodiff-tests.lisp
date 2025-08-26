(defpackage epsilon.compute.reverse-autodiff-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (ad epsilon.compute.autodiff)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.reverse-autodiff-tests)

;;; Tape-based reverse-mode autodiff tests

(deftest test-tape-construction
  "Test computation tape construction"
  ;; Simple expression: y = x^2
  (let* ((x (c:var 'x))
         (y (c:* x x))
         (tape (ad:build-tape y '((x . 3)))))
    (is (ad:tape-p tape))
    (is (= (ad:tape-node-count tape) 2))  ; x (shared), *
    (is (= (ad:tape-output-value tape) 9))
    
    ;; Check tape nodes have correct structure
    (let ((nodes (ad:tape-nodes tape)))
      (is (>= (length nodes) 2))
      ;; Each node should have: value, parents, adjoint
      (dolist (node nodes)
        (is (ad:tape-node-p node))
        (is (numberp (ad:tape-node-value node)))))))

(deftest test-backward-pass
  "Test backward pass gradient propagation"
  ;; f(x) = x^2, df/dx = 2x
  (let* ((x (c:var 'x))
         (f (c:* x x))
         (grad (ad:reverse-diff f '(x) '((x . 3)))))
    (is (= (first grad) 6)))  ; 2*3 = 6
  
  ;; f(x) = x^3, df/dx = 3x^2
  (let* ((x (c:var 'x))
         (f (c:* (c:* x x) x))
         (grad (ad:reverse-diff f '(x) '((x . 2)))))
    (is (= (first grad) 12)))  ; 3*2^2 = 12
  
  ;; f(x,y) = x*y, df/dx = y, df/dy = x
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f (c:* x y))
         (grad (ad:reverse-diff f '(x y) '((x . 3) (y . 4)))))
    (is (= (first grad) 4))   ; df/dx = y = 4
    (is (= (second grad) 3)))) ; df/dy = x = 3

(deftest test-chain-rule
  "Test chain rule in reverse mode"
  ;; f(x) = sin(x^2), df/dx = 2x*cos(x^2)
  (let* ((x (c:var 'x))
         (x2 (c:* x x))
         (f (c:sin x2))
         (grad (ad:reverse-diff f '(x) '((x . 1)))))
    ;; At x=1: 2*1*cos(1) ≈ 2*0.5403 ≈ 1.0806
    (is (< (abs (- (first grad) (* 2 (cos 1)))) 0.001)))
  
  ;; g(x) = exp(2x), dg/dx = 2*exp(2x)
  (let* ((x (c:var 'x))
         (f (c:exp (c:* 2 x)))
         (grad (ad:reverse-diff f '(x) '((x . 0)))))
    ;; At x=0: 2*exp(0) = 2
    (is (= (first grad) 2))))

(deftest test-gradient-accumulation
  "Test gradient accumulation for shared subexpressions"
  ;; f(x) = x^2 + 2x, df/dx = 2x + 2
  (let* ((x (c:var 'x))
         (f (c:+ (c:* x x) (c:* 2 x)))
         (grad (ad:reverse-diff f '(x) '((x . 3)))))
    (is (= (first grad) 8)))  ; 2*3 + 2 = 8
  
  ;; Multiple paths to same variable
  ;; f(x) = (x + 1) * (x - 1) = x^2 - 1, df/dx = 2x
  (let* ((x (c:var 'x))
         (plus1 (c:+ x 1))
         (minus1 (c:- x 1))
         (f (c:* plus1 minus1))
         (grad (ad:reverse-diff f '(x) '((x . 5)))))
    (is (= (first grad) 10)))) ; 2*5 = 10

(deftest test-multivariate-gradients
  "Test gradients with multiple variables"
  ;; f(x,y,z) = x*y + y*z + x*z
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (z (c:var 'z))
         (f (c:+ (c:* x y) (c:+ (c:* y z) (c:* x z))))
         (grad (ad:gradient f '(x y z) '((x . 2) (y . 3) (z . 4)))))
    ;; df/dx = y + z = 3 + 4 = 7
    (is (= (first grad) 7))
    ;; df/dy = x + z = 2 + 4 = 6
    (is (= (second grad) 6))
    ;; df/dz = y + x = 3 + 2 = 5
    (is (= (third grad) 5))))

(deftest test-vector-jacobian-product
  "Test vector-Jacobian product (VJP) for efficiency"
  ;; Multiple outputs: f = [x^2, x*y, y^2]
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f1 (c:* x x))
         (f2 (c:* x y))
         (f3 (c:* y y))
         (v #(1 2 3))  ; Vector for VJP
         (vjp (ad:vector-jacobian-product (list f1 f2 f3) '(x y) 
                                          '((x . 2) (y . 3)) v)))
    ;; Jacobian = [[2x, 0], [y, x], [0, 2y]]
    ;; At (2,3): [[4, 0], [3, 2], [0, 6]]
    ;; v^T * J = [1,2,3] * [[4,0],[3,2],[0,6]] = [10, 22]
    (is (= (aref vjp 0) 10))
    (is (= (aref vjp 1) 22))))

(deftest test-checkpointing
  "Test gradient checkpointing for memory efficiency"
  (epsilon.test:skip "Checkpointing not yet implemented")
  #|
  ;; Large computation with checkpoints
  (let* ((x (c:var 'x))
         ;; Create a deep computation graph
         (layer1 (c:* x x))
         (layer2 (c:+ layer1 x))
         (layer3 (c:* layer2 layer2))
         (layer4 (c:+ layer3 layer2))
         ;; Enable checkpointing
         (grad (ad:reverse-diff-with-checkpoints 
                layer4 '(x) '((x . 2))
                :checkpoint-layers '(layer2))))
    ;; Verify gradient is correct despite checkpointing
    (is (numberp (first grad)))
    ;; Memory usage should be lower (tested separately)
    (is (ad:checkpoints-used-p)))
  |#)

(deftest test-custom-vjp-rules
  "Test custom vector-Jacobian product rules"
  ;; Register custom VJP for efficient operations
  (ad:register-vjp-rule 'matrix-multiply
    (lambda (args adjoints)
      ;; Custom efficient rule for matrix multiply
      (destructuring-bind (a b) args
        (destructuring-bind (da) adjoints
          (list (c:dot da (c:transpose b))
                (c:dot (c:transpose a) da))))))
  
  ;; Test custom rule is used
  (let* ((A (c:var 'A :shape '(3 4)))
         (B (c:var 'B :shape '(4 5)))
         (C (c:dot A B))
         (grad (ad:reverse-diff C '(A B) 
                               '((A . #2A((1 2 3 4)
                                         (5 6 7 8)
                                         (9 10 11 12)))
                                 (B . #2A((1 0 0 0 0)
                                         (0 1 0 0 0)
                                         (0 0 1 0 0)
                                         (0 0 0 1 0)))))))
    (is (arrayp (first grad)))
    (is (arrayp (second grad)))))

(deftest test-higher-order-reverse
  "Test higher-order derivatives with reverse mode"
  ;; Second derivative using reverse-over-reverse
  (let* ((x (c:var 'x))
         (f (c:* (c:* x x) x))  ; x^3
         ;; First derivative: 3x^2
         (df (ad:reverse-diff f '(x) '((x . 2))))
         ;; Second derivative: 6x
         (d2f (ad:reverse-diff-symbolic f 'x)))
    ;; Create gradient of gradient
    (let ((hessian-diag (ad:reverse-diff d2f '(x) '((x . 2)))))
      (is (= (first hessian-diag) 12)))))  ; 6*2 = 12

(deftest test-sparse-gradients
  "Test sparse gradient representation for efficiency"
  ;; Large sparse computation
  (let* ((vars (loop for i from 1 to 100
                     collect (c:var (intern (format nil "X~A" i)))))
         ;; Only use first 3 variables
         (f (c:+ (first vars) (c:* (second vars) (third vars))))
         (var-names (mapcar #'sym:var-name vars))
         (bindings (loop for v in var-names
                        collect (cons v 1)))
         (grad (ad:sparse-gradient f var-names bindings)))
    ;; Gradient should be sparse
    (is (ad:sparse-gradient-p grad))
    ;; Only 3 non-zero entries
    (is (= (ad:sparse-gradient-nnz grad) 3))
    ;; Check values
    (is (= (ad:sparse-gradient-get grad 0) 1))    ; df/dx1 = 1
    (is (= (ad:sparse-gradient-get grad 1) 1))    ; df/dx2 = x3 = 1
    (is (= (ad:sparse-gradient-get grad 2) 1))    ; df/dx3 = x2 = 1
    ;; Rest should be zero
    (loop for i from 3 below 100
          do (is (= (ad:sparse-gradient-get grad i) 0)))))

(deftest test-gradient-clipping
  "Test gradient clipping for numerical stability"
  ;; Large gradients that need clipping
  (let* ((x (c:var 'x))
         (f (c:exp (c:* 10 x)))  ; Very steep gradient
         (grad (ad:reverse-diff f '(x) '((x . 1))
                                :clip-norm 1.0)))
    ;; Gradient should be clipped
    (is (<= (abs (first grad)) 1.0)))
  
  ;; Test per-element clipping
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f (c:+ (c:* 1000 x) y))
         (grad (ad:reverse-diff f '(x y) '((x . 1) (y . 1))
                               :clip-value 10.0)))
    ;; x gradient should be clipped to 10
    (is (= (first grad) 10.0))
    ;; y gradient should be unchanged
    (is (= (second grad) 1.0))))

(deftest test-nan-gradient-handling
  "Test handling of NaN and Inf gradients"
  ;; Division by zero
  (let* ((x (c:var 'x))
         (f (c:/ 1 x))
         (grad (ad:reverse-diff f '(x) '((x . 0))
                               :handle-nan :zero)))
    ;; Should replace NaN with 0
    (is (= (first grad) 0)))
  
  ;; Log of negative number
  (let* ((x (c:var 'x))
         (f (c:log x))
         (grad (ad:reverse-diff f '(x) '((x . -1))
                               :handle-nan :error)))
    ;; Should signal error
    (is-thrown error (first grad))))

(deftest test-stop-gradient
  "Test stop-gradient operation"
  ;; Gradient should not flow through stop-gradient
  (let* ((x (c:var 'x))
         (y (c:* x 2))
         (y-stopped (ad:stop-gradient y))
         (f (c:* y-stopped x))
         (grad (ad:reverse-diff f '(x) '((x . 3)))))
    ;; df/dx = y (not 2*y as would be without stop)
    ;; y = 2*3 = 6
    (is (= (first grad) 6))))

(deftest test-gradient-tape-memory
  "Test memory management in gradient tape"
  ;; Create large computation
  (let* ((x (c:var 'x))
         (f (loop for i from 1 to 1000
                  for acc = x then (c:+ acc (c:* x x))
                  finally (return acc))))
    ;; Should handle large tape efficiently
    (let ((initial-memory (ad:tape-memory-usage))
          (grad (ad:reverse-diff f '(x) '((x . 1))))
          (peak-memory (ad:peak-tape-memory)))
      ;; Memory should be freed after computation
      (is (< (ad:tape-memory-usage) (* 2 initial-memory)))
      ;; Peak memory should be reasonable
      (is (< peak-memory (* 1000 initial-memory))))))

(deftest test-mixed-mode-autodiff
  "Test mixing forward and reverse mode for efficiency"
  ;; Hessian computation using mixed mode
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f (c:+ (c:* x (c:* x y)) (c:* y y)))  ; x^2*y + y^2
         (hess (ad:hessian-mixed-mode f '(x y) '((x . 2) (y . 3)))))
    ;; Hessian at (2,3):
    ;; [[2y, 2x], [2x, 2]] = [[6, 4], [4, 2]]
    (is (= (aref hess 0 0) 6))
    (is (= (aref hess 0 1) 4))
    (is (= (aref hess 1 0) 4))
    (is (= (aref hess 1 1) 2))))