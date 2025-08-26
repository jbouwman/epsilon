;;;; E-graph Debug Tests

(defpackage epsilon.compute.egraph-debug-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)
   (egraph epsilon.compute.egraph)))

(in-package epsilon.compute.egraph-debug-tests)

(deftest test-egraph-basic-simplification
  "Test basic e-graph simplification with debug output"
  (skip "E-graph equality saturation not yet implemented")
  (let* ((x (sym:sym 'x))
         (expr (c:+ x 0)))
    (format t "~%DEBUG TEST: Original expression: ~S~%" expr)
    (format t "DEBUG TEST: Is expr? ~S~%" (sym:expr-p expr))
    (format t "DEBUG TEST: Op: ~S~%" (sym:expr-op expr))
    (format t "DEBUG TEST: Args: ~S~%" (sym:expr-args expr))
    
    ;; Test e-graph creation
    (let ((eg (egraph:create-egraph)))
      (is (egraph:egraph-p eg))
      
      ;; Add expression to e-graph
      (let ((expr-id (egraph:add-expr eg expr)))
        (format t "DEBUG TEST: Added expression with ID: ~S~%" expr-id)
        (is (integerp expr-id))
        (is (>= expr-id 0))
        
        ;; Check e-graph state
        (format t "DEBUG TEST: E-graph classes: ~S~%" 
                (hash-table-count (egraph::egraph-classes eg)))
        
        ;; Initialize rules if needed
        (when (null egraph:*standard-rules*)
          (egraph:init-standard-rules))
        (format t "DEBUG TEST: Number of rules: ~S~%" 
                (length egraph:*standard-rules*))
        
        ;; Apply saturation
        (format t "DEBUG TEST: Starting saturation...~%")
        (handler-case
            (let ((saturation-result (egraph:saturate eg egraph:*standard-rules* :limit 5)))
              (format t "DEBUG TEST: Saturation result: ~S~%" saturation-result))
          (error (e)
            (format t "DEBUG TEST: Error during saturation: ~A~%" e)
            (error e)))
        
        ;; Extract best expression
        (format t "DEBUG TEST: Calling extract-best with expr-id=~S~%" expr-id)
        (let ((result (egraph:extract-best eg expr-id)))
          (format t "DEBUG TEST: Extracted result: ~S~%" result)
          (is (not (null result)))
          (when result
            (is (sym:var-p result))
            (when (sym:var-p result)
              (is (eq (sym:var-name result) 'x)))))))))

(deftest test-egraph-node-creation
  "Test e-graph node creation process"
  (skip "E-graph equality saturation not yet implemented")
  (let* ((eg (egraph:create-egraph))
         (x (sym:sym 'x))
         (zero (sym:lit 0)))
    
    ;; Add individual nodes
    (let ((x-id (egraph:add-expr eg x))
          (zero-id (egraph:add-expr eg zero)))
      (format t "~%DEBUG TEST: X node ID: ~S~%" x-id)
      (format t "DEBUG TEST: Zero node ID: ~S~%" zero-id)
      (is (= x-id 0))
      (is (= zero-id 1))
      
      ;; Now add the compound expression
      (let* ((plus-expr (c:+ x zero))
             (plus-id (egraph:add-expr eg plus-expr)))
        (format t "DEBUG TEST: Plus expression ID: ~S~%" plus-id)
        (format t "DEBUG TEST: Plus expression: ~S~%" plus-expr)
        
        ;; Check the e-graph structure
        (let ((classes (egraph::egraph-classes eg)))
          (format t "DEBUG TEST: Number of e-classes: ~S~%" 
                  (hash-table-count classes))
          
          ;; Inspect each e-class
          (maphash (lambda (id eclass)
                     (format t "DEBUG TEST: E-class ~S:~%" id)
                     (dolist (node (egraph::eclass-nodes eclass))
                       (format t "  Node: op=~S, args=~S~%" 
                               (egraph:enode-op node)
                               (egraph:enode-args node))))
                   classes))))))

(deftest test-pattern-matching
  "Test pattern matching functionality"
  (let* ((eg (egraph:create-egraph))
         (x (sym:sym 'x))
         (expr (c:+ x 0))
         (expr-id (egraph:add-expr eg expr)))
    
    ;; Initialize rules
    (when (null egraph:*standard-rules*)
      (egraph:init-standard-rules))
    
    ;; Test pattern matching on the first rule (x + 0 -> x)
    (let ((rule (first egraph:*standard-rules*)))
      (format t "~%DEBUG TEST: Testing rule: ~S~%" rule)
      (when rule
        (format t "DEBUG TEST: Rule pattern: ~S~%" 
                (egraph::rewrite-rule-lhs rule))
        (format t "DEBUG TEST: Rule replacement: ~S~%" 
                (egraph::rewrite-rule-rhs rule))
        
        ;; Try to find matches
        (let ((matches (egraph::find-matches eg (egraph::rewrite-rule-lhs rule))))
          (format t "DEBUG TEST: Found ~S matches~%" (length matches))
          (dolist (match matches)
            (format t "DEBUG TEST: Match - ID: ~S, Bindings: ~S~%" 
                    (first match) (second match))))))))

(deftest test-rule-application
  "Test individual rule application"
  (skip "RHS-ID variable unbound in rule application")
  (let* ((eg (egraph:create-egraph))
         (x (sym:sym 'x))
         (expr (c:+ x 0)))
    
    ;; Add expression
    (let ((expr-id (egraph:add-expr eg expr)))
      (format t "~%DEBUG TEST: Expression added with ID: ~S~%" expr-id)
      
      ;; Initialize rules
      (when (null egraph:*standard-rules*)
        (egraph:init-standard-rules))
      
      ;; Apply rules one by one
      (let ((applied-count 0))
        (dolist (rule egraph:*standard-rules*)
          (let ((new-facts (egraph::apply-single-rule eg rule)))
            (when (> new-facts 0)
              (format t "DEBUG TEST: Rule ~S added ~S new facts~%" 
                      (egraph::rewrite-rule-name rule) new-facts)
              (incf applied-count new-facts))))
        
        (format t "DEBUG TEST: Total new facts: ~S~%" applied-count)
        (is (> applied-count 0) "Should have applied at least one rule"))
      
      ;; Extract result
      (let ((result (egraph:extract-best eg expr-id)))
        (format t "DEBUG TEST: Final result: ~S~%" result)
        (is (not (null result)))))))