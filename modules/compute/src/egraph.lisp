;;;; E-graph implementation for equality saturation
;;;;
;;;; This module implements an e-graph (equality graph) data structure
;;;; for performing equality saturation and program optimization.
;;;;
;;;; Key features:
;;;;   - Union-find data structure for efficient equivalence class management
;;;;   - Pattern matching and rewrite rules
;;;;   - Equality saturation with configurable limits
;;;;   - Integration with epsilon.compute.symbolic expressions

(defpackage epsilon.compute.egraph
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (types epsilon.compute.types))
  (:export
   ;; E-graph structure
   :egraph
   :make-egraph
   :egraph-p
   :create-egraph
   
   ;; E-nodes
   :enode
   :make-enode
   :enode-p
   :enode-op
   :enode-args
   
   ;; Operations
   :add-expr
   :merge-eclasses
   :find-eclass
   :rebuild
   :saturate
   :extract-best
   
   ;; Rules
   :rewrite-rule
   :make-rewrite-rule
   :*standard-rules*
   :init-standard-rules
   
   ;; High-level API
   :create-egraph
   :optimize-with-egraph
   :saturate-rules))

(in-package epsilon.compute.egraph)

;;; Union-Find Data Structure with Path Compression

(defstruct unionfind
  "Union-find data structure for managing equivalence classes"
  (parent (make-hash-table :test 'eql))
  (rank (make-hash-table :test 'eql)))

(defun uf-make-set (uf x)
  "Create a new set containing x"
  (setf (gethash x (unionfind-parent uf)) x)
  (setf (gethash x (unionfind-rank uf)) 0)
  x)

(defun uf-find (uf x)
  "Find the root of the set containing x with path compression"
  (let ((parent (gethash x (unionfind-parent uf))))
    (if (or (null parent) (eql parent x))
        x
        (let ((root (uf-find uf parent)))
          ;; Path compression
          (setf (gethash x (unionfind-parent uf)) root)
          root))))

(defun uf-union (uf x y)
  "Union the sets containing x and y, return the new root"
  (let ((root-x (uf-find uf x))
        (root-y (uf-find uf y)))
    (if (eql root-x root-y)
        root-x
        (let ((rank-x (gethash root-x (unionfind-rank uf) 0))
              (rank-y (gethash root-y (unionfind-rank uf) 0)))
          (cond
            ((< rank-x rank-y)
             (setf (gethash root-x (unionfind-parent uf)) root-y)
             root-y)
            ((> rank-x rank-y)
             (setf (gethash root-y (unionfind-parent uf)) root-x)
             root-x)
            (t
             (setf (gethash root-y (unionfind-parent uf)) root-x)
             (incf (gethash root-x (unionfind-rank uf)))
             root-x))))))

;;; E-node structure

(defstruct enode
  "An e-node represents an operation with arguments that are e-class IDs"
  (op nil)         ; The operation (symbol, number, or other value)
  (args nil))      ; List of e-class IDs

(defun enode-hash (enode)
  "Compute hash for an e-node"
  (sxhash (cons (enode-op enode) (enode-args enode))))

(defun enode-equal (e1 e2)
  "Check if two e-nodes are equal"
  (and (equal (enode-op e1) (enode-op e2))
       (equal (enode-args e1) (enode-args e2))))

;;; E-class structure

(defstruct eclass
  "An equivalence class of e-nodes"
  (id nil)
  (nodes nil)      ; List of e-nodes in this class
  (parents nil))   ; List of parent e-nodes that reference this class

;;; E-graph structure

(defstruct egraph
  "The e-graph data structure"
  (unionfind (make-unionfind))
  (classes (make-hash-table :test 'eql))      ; Map from e-class ID to eclass
  (memo (make-hash-table :test 'equal))       ; Map from canonical e-node to e-class ID
  (next-id 1)                                  ; Next available e-class ID
  (worklist nil))                              ; Worklist for rebuilding

(defun create-egraph ()
  "Create a new empty e-graph"
  (make-egraph))

(defun egraph-add-enode (egraph enode)
  "Add an e-node to the e-graph, return its e-class ID"
  (let* ((canonical (canonicalize-enode egraph enode))
         (existing-id (gethash canonical (egraph-memo egraph))))
    (if existing-id
(uf-find (egraph-unionfind egraph) existing-id)
        (let ((new-id (egraph-next-id egraph)))
          ;; Create new e-class
          (incf (egraph-next-id egraph))
          (uf-make-set (egraph-unionfind egraph) new-id)
          (let ((new-class (make-eclass :id new-id :nodes (list canonical))))
            (setf (gethash new-id (egraph-classes egraph)) new-class)
            (setf (gethash canonical (egraph-memo egraph)) new-id)
            ;; Add to parent lists
            (dolist (arg-id (enode-args canonical))
              (let ((parent-class (gethash arg-id (egraph-classes egraph))))
                (when parent-class
                  (push (cons canonical new-id) (eclass-parents parent-class)))))
            new-id)))))

(defun canonicalize-enode (egraph enode)
  "Canonicalize an e-node by finding canonical IDs for its arguments"
  (make-enode :op (enode-op enode)
              :args (mapcar (lambda (id) (uf-find (egraph-unionfind egraph) id))
                           (enode-args enode))))

(defun expr-to-enode (expr)
  "Convert a symbolic expression to an e-node structure"
  (cond
    ((sym:var-p expr)
     (make-enode :op (sym:var-name expr) :args nil))
    ((sym:const-p expr)
     (make-enode :op (sym:const-value expr) :args nil))
    ((sym:expr-p expr)
     (let ((op (sym:expr-op expr))
           (arg-exprs (sym:expr-args expr)))
       ;; This is a placeholder - we need the e-class IDs of the arguments
       ;; This will be completed in the add-expr function
       (make-enode :op op :args arg-exprs)))
    (t (error "Unknown expression type: ~S" expr))))

(defun add-expr (egraph expr)
  "Add a symbolic expression to the e-graph, return its e-class ID"
  (cond
    ((sym:var-p expr)
     (let ((enode (make-enode :op (sym:var-name expr) :args nil)))
       (egraph-add-enode egraph enode)))
    
    ((sym:const-p expr)
     (let ((enode (make-enode :op (sym:const-value expr) :args nil)))
       (egraph-add-enode egraph enode)))
    
    ((sym:expr-p expr)
     ;; Recursively add arguments first, then add this expression
     (let* ((op (sym:expr-op expr))
            (arg-exprs (sym:expr-args expr)))
       (let* ((arg-ids (mapcar (lambda (arg) (add-expr egraph arg)) arg-exprs))
              (enode (make-enode :op op :args arg-ids)))
         (egraph-add-enode egraph enode))))
    
    (t (error "Cannot add expression of unknown type: ~S" expr))))

(defun merge-eclasses (egraph id1 id2)
  "Merge two equivalence classes and mark rebuild as needed"
  (let ((canon1 (uf-find (egraph-unionfind egraph) id1))
        (canon2 (uf-find (egraph-unionfind egraph) id2)))
    (if (= canon1 canon2)
        canon1  ; Already equivalent
        (let* ((new-canon (uf-union (egraph-unionfind egraph) canon1 canon2))
               (class1 (gethash canon1 (egraph-classes egraph)))
               (class2 (gethash canon2 (egraph-classes egraph))))
          ;; Merge e-nodes from both classes
          (when (and class1 class2)
            (let ((merged-nodes (append (eclass-nodes class1) (eclass-nodes class2)))
                  (merged-parents (append (eclass-parents class1) (eclass-parents class2))))
              (setf (gethash new-canon (egraph-classes egraph))
                    (make-eclass :id new-canon
                                :nodes merged-nodes
                                :parents merged-parents))
              ;; Remove old classes
              (unless (= new-canon canon1)
                (remhash canon1 (egraph-classes egraph)))
              (unless (= new-canon canon2)
                (remhash canon2 (egraph-classes egraph)))
              ;; Add parents to worklist for rebuilding
              (dolist (parent merged-parents)
                (pushnew parent (egraph-worklist egraph) :test #'equal))))
          new-canon))))

(defun rebuild (egraph)
  "Rebuild the e-graph to maintain congruence closure"
  (loop while (egraph-worklist egraph)
        do (let ((work-item (pop (egraph-worklist egraph))))
             (when (consp work-item)
               (let ((enode (car work-item))
                     (old-id (cdr work-item)))
                 ;; Re-canonicalize the e-node
                 (let* ((canonical (canonicalize-enode egraph enode))
                        (existing-id (gethash canonical (egraph-memo egraph))))
                   (cond
                     ;; If canonical form already exists in memo with different ID, merge
                     ((and existing-id (not (= existing-id old-id)))
                      (let ((new-canonical-id (merge-eclasses egraph existing-id old-id)))
                        ;; Update memo with the new canonical ID
                        (setf (gethash canonical (egraph-memo egraph)) new-canonical-id)))
                     ;; If this e-node form doesn't exist, add it
                     ((null existing-id)
                      (setf (gethash canonical (egraph-memo egraph)) old-id)))))))))

;;; Cost-based extraction

(defun extract-best (egraph root-id &key (cost-fn #'expression-size-cost))
  "Extract the best expression from an e-class using the given cost function"
  (let ((canonical-id (uf-find (egraph-unionfind egraph) root-id)))
    (extract-minimum-cost egraph canonical-id cost-fn)))

(defun extract-minimum-cost (egraph eclass-id cost-fn)
  "Extract minimum cost expression from e-class using dynamic programming"
  (let ((costs (make-hash-table :test 'eql))
        (best-nodes (make-hash-table :test 'eql)))
    ;; Initialize costs for this e-class
    (extract-costs egraph eclass-id cost-fn costs best-nodes)
    ;; Reconstruct the best expression
    (reconstruct-expression egraph eclass-id best-nodes)))

(defun extract-costs (egraph eclass-id cost-fn costs best-nodes)
  "Compute minimum costs for all e-classes reachable from eclass-id"
  (unless (gethash eclass-id costs)
    (let ((canonical-id (uf-find (egraph-unionfind egraph) eclass-id)))
      (let ((eclass (gethash canonical-id (egraph-classes egraph))))
        (if eclass
            (let ((min-cost most-positive-fixnum)
                  (best-node nil))
              (dolist (enode (eclass-nodes eclass))
                ;; Recursively compute costs for arguments first
                (dolist (arg-id (enode-args enode))
                  (let ((canonical-arg (uf-find (egraph-unionfind egraph) arg-id)))
                    (extract-costs egraph canonical-arg cost-fn costs best-nodes)))
                ;; Compute cost of this e-node
                (let ((node-cost (compute-enode-cost enode cost-fn costs)))
                  (when (< node-cost min-cost)
                    (setf min-cost node-cost)
                    (setf best-node enode))))
              ;; Record the best choice for this e-class
              (setf (gethash canonical-id costs) min-cost)
              (setf (gethash canonical-id best-nodes) best-node))
            ;; Handle missing e-class gracefully
            (progn
              (setf (gethash canonical-id costs) 1)
              (setf (gethash canonical-id best-nodes) (make-enode :op 'unknown :args nil))))))))

(defun compute-enode-cost (enode cost-fn costs)
  "Compute the cost of an e-node given costs of its arguments"
  (let ((arg-costs (mapcar (lambda (id) (gethash id costs 1)) (enode-args enode))))
    (funcall cost-fn enode arg-costs)))


(defun expression-size-cost (enode arg-costs)
  "Simple cost function: size of expression tree"
  (declare (ignore enode))
  (1+ (reduce #'+ arg-costs :initial-value 0)))

(defun operation-count-cost (enode arg-costs)
  "Cost function based on operation complexity"
  (let ((op (enode-op enode)))
    (+ (case op
         ((+ -) 1)
         (* 2)
         (/ 3)
         (^ 4)
         ((sin cos tan) 5)
         ((exp log) 6)
         (t 1))
       (reduce #'+ arg-costs :initial-value 0))))

(defun depth-cost (enode arg-costs)
  "Cost function favoring shallow expressions"
  (declare (ignore enode))
  (if (null arg-costs)
      1
      (1+ (reduce #'max arg-costs :initial-value 0))))

(defun reconstruct-expression (egraph eclass-id best-nodes)
  "Reconstruct the symbolic expression from the best e-node choices"
  (let ((best-node (gethash eclass-id best-nodes)))
    (if best-node
        (if (null (enode-args best-node))
            ;; Leaf node (variable or constant)
            (let ((op (enode-op best-node)))
              (cond
                ((symbolp op) (sym:sym op))
                ((numberp op) (sym:lit op))
                (t op)))
            ;; Internal node
            (let* ((op (enode-op best-node))
                   (arg-exprs (mapcar (lambda (arg-id)
                                       (reconstruct-expression egraph arg-id best-nodes))
                                     (enode-args best-node))))
              (sym:symbolic op arg-exprs)))
        ;; No best node found, return a placeholder
        (sym:sym 'unknown))))

(defun extract-minimal-expr (egraph eclass-id)
  "Extract a minimal expression from an e-class"
  (extract-best egraph eclass-id))

(defun find-eclass (egraph id)
  "Find canonical e-class ID using union-find"
  (uf-find (egraph-unionfind egraph) id))

(defstruct rewrite-rule
  "A rewrite rule for the e-graph"
  (name nil)
  (lhs nil)   ; Left-hand side pattern
  (rhs nil)   ; Right-hand side pattern or function
  (condition nil))  ; Optional condition function

(defun make-rewrite (name lhs rhs &key condition)
  "Create a rewrite rule"
  (make-rewrite-rule :name name :lhs lhs :rhs rhs :condition condition))

(defparameter *standard-rules* nil
  "Standard algebraic rewrite rules - initialized after package loads")

(defun init-standard-rules ()
  "Initialize standard rules after packages are loaded"
  ;; Find the operator symbols at runtime
  (let ((add-op (find-symbol "+" "EPSILON.COMPUTE"))
        (sub-op (find-symbol "-" "EPSILON.COMPUTE"))
        (mul-op (find-symbol "*" "EPSILON.COMPUTE"))
        (div-op (find-symbol "/" "EPSILON.COMPUTE"))
        (pow-op (find-symbol "^" "EPSILON.COMPUTE"))
        (sin-op (find-symbol "SIN" "EPSILON.COMPUTE"))
        (cos-op (find-symbol "COS" "EPSILON.COMPUTE"))
        (exp-op (find-symbol "EXP" "EPSILON.COMPUTE"))
        (log-op (find-symbol "LOG" "EPSILON.COMPUTE")))
    (setf *standard-rules*
          (list
            ;; Additive identity
            (make-rewrite 'add-zero-right `(,add-op ?x 0) '?x)
            (make-rewrite 'add-zero-left `(,add-op 0 ?x) '?x)
            
            ;; Subtractive identity  
            (make-rewrite 'sub-zero `(,sub-op ?x 0) '?x)
            (make-rewrite 'sub-self `(,sub-op ?x ?x) '0)
            
            ;; Multiplicative identity
            (make-rewrite 'mul-one-right `(,mul-op ?x 1) '?x)
            (make-rewrite 'mul-one-left `(,mul-op 1 ?x) '?x)
            
            ;; Multiplicative zero
            (make-rewrite 'mul-zero-right `(,mul-op ?x 0) '0)
            (make-rewrite 'mul-zero-left `(,mul-op 0 ?x) '0)
            
            ;; Division identity and rules
            (make-rewrite 'div-one `(,div-op ?x 1) '?x)
            (make-rewrite 'div-self `(,div-op ?x ?x) '1 :condition (lambda (bindings) (not-zero-p bindings '?x)))
            
            ;; Exponent rules
            (make-rewrite 'pow-zero `(,pow-op ?x 0) '1 :condition (lambda (bindings) (not-zero-p bindings '?x)))
            (make-rewrite 'pow-one `(,pow-op ?x 1) '?x)
            (make-rewrite 'pow-zero-base `(,pow-op 0 ?x) '0 :condition (lambda (bindings) (positive-p bindings '?x)))
            (make-rewrite 'one-pow `(,pow-op 1 ?x) '1)
            
            ;; Commutativity
            (make-rewrite 'add-comm `(,add-op ?x ?y) `(,add-op ?y ?x))
            (make-rewrite 'mul-comm `(,mul-op ?x ?y) `(,mul-op ?y ?x))
            
            ;; Associativity
            (make-rewrite 'add-assoc `(,add-op (,add-op ?x ?y) ?z) `(,add-op ?x (,add-op ?y ?z)))
            (make-rewrite 'mul-assoc `(,mul-op (,mul-op ?x ?y) ?z) `(,mul-op ?x (,mul-op ?y ?z)))
            
            ;; Distribution
            (make-rewrite 'distribute-right `(,mul-op ?x (,add-op ?y ?z)) 
                         `(,add-op (,mul-op ?x ?y) (,mul-op ?x ?z)))
            (make-rewrite 'distribute-left `(,mul-op (,add-op ?x ?y) ?z)
                         `(,add-op (,mul-op ?x ?z) (,mul-op ?y ?z)))
            
            ;; Factoring (reverse distribution)
            (make-rewrite 'factor-add `(,add-op (,mul-op ?x ?y) (,mul-op ?x ?z))
                         `(,mul-op ?x (,add-op ?y ?z)))
            
            ;; Double negation
            (make-rewrite 'double-neg `(,sub-op 0 (,sub-op 0 ?x)) '?x)))
    
    ;; Add conditional rules for transcendental functions within the let scope
    (when sin-op
      (push (make-rewrite 'sin-zero `(,sin-op 0) '0) *standard-rules*))
    (when cos-op  
      (push (make-rewrite 'cos-zero `(,cos-op 0) '1) *standard-rules*))
    (when exp-op
      (push (make-rewrite 'exp-zero `(,exp-op 0) '1) *standard-rules*))
    (when log-op
      (push (make-rewrite 'log-one `(,log-op 1) '0) *standard-rules*)
      (when exp-op
        (push (make-rewrite 'log-exp `(,log-op (,exp-op ?x)) '?x) *standard-rules*)
        (push (make-rewrite 'exp-log `(,exp-op (,log-op ?x)) '?x 
                           :condition (lambda (bindings) (positive-p bindings '?x))) *standard-rules*)))
    ;; Reverse to maintain intended order
    (setf *standard-rules* (nreverse *standard-rules*))))

;; Condition functions for rules
(defun not-zero-p (bindings var)
  "Check if variable binding is not zero"
  (let ((binding (assoc var bindings)))
    (when binding
      (let ((value (cdr binding)))
        (not (and (integerp value) (zerop value)))))))

(defun positive-p (bindings var)
  "Check if variable binding is positive"
  (let ((binding (assoc var bindings)))
    (when binding
      (let ((value (cdr binding)))
        (and (numberp value) (> value 0))))))

;;; Pattern matching for rules

(defstruct pattern-var
  "A pattern variable (starts with ?)"
  (name nil))

(defun pattern-var-p (obj)
  "Check if object is a pattern variable (symbol starting with ?)"
  (and (symbolp obj)
       (let ((name (symbol-name obj)))
         (and (> (length name) 0)
              (char= (char name 0) #\?)))))

(defun make-pattern-var (name)
  "Create a pattern variable from a symbol"
  (intern (format nil "?~A" name)))

(defun match-pattern (pattern expr bindings)
  "Match a pattern against an expression, return updated bindings or nil"
  (cond
    ;; Pattern variable
    ((pattern-var-p pattern)
     (let ((existing (assoc pattern bindings)))
       (if existing
           (when (expr-equal-p (cdr existing) expr)
             bindings)
           (cons (cons pattern expr) bindings))))
    
    ;; Lists must match recursively
    ((and (listp pattern) (listp expr)
          (= (length pattern) (length expr)))
     (match-pattern-list pattern expr bindings))
    
    ;; Literals must match exactly
    ((equal pattern expr) bindings)
    
    (t nil)))

(defun match-pattern-list (pattern-list expr-list bindings)
  "Match a list of patterns against a list of expressions"
  (if (null pattern-list)
      bindings
      (let ((new-bindings (match-pattern (first pattern-list)
                                        (first expr-list)
                                        bindings)))
        (if new-bindings
            (match-pattern-list (rest pattern-list) (rest expr-list) new-bindings)
            nil))))

(defun expr-equal-p (expr1 expr2)
  "Check if two expressions are structurally equal"
  (cond
    ((and (sym:var-p expr1) (sym:var-p expr2))
     (eq (sym:var-name expr1) (sym:var-name expr2)))
    ((and (sym:const-p expr1) (sym:const-p expr2))
     (eql (sym:const-value expr1) (sym:const-value expr2)))
    ((and (sym:expr-p expr1) (sym:expr-p expr2))
     (and (eq (sym:expr-op expr1) (sym:expr-op expr2))
          (expr-list-equal-p (sym:expr-args expr1)
                           (sym:expr-args expr2))))
    ((and (listp expr1) (listp expr2))
     (expr-list-equal-p expr1 expr2))
    (t (equal expr1 expr2))))

(defun expr-list-equal-p (list1 list2)
  "Check if two lists of expressions are equal"
  (and (= (length list1) (length list2))
       (every #'expr-equal-p list1 list2)))

(defun apply-substitution (pattern bindings)
  "Apply variable bindings to a pattern to get a concrete expression"
  (cond
    ((pattern-var-p pattern)
     (let ((binding (assoc pattern bindings)))
       (if binding (cdr binding) pattern)))
    ((listp pattern)
     (mapcar (lambda (sub-pattern) (apply-substitution sub-pattern bindings))
             pattern))
    (t pattern)))

(defun apply-rewrites (egraph rules)
  "Apply all rewrite rules to the e-graph, return number of new facts discovered"
  (let ((new-facts 0))
    (dolist (rule rules)
      (incf new-facts (apply-single-rule egraph rule)))
    new-facts))

(defun apply-single-rule (egraph rule)
  "Apply a single rewrite rule to all matching e-nodes in the e-graph"
  (let ((matches (find-matches egraph (rewrite-rule-lhs rule)))
        (new-facts 0))
    (dolist (match matches)
      (let* ((bindings (cdr match))
             (matched-id (car match))
             ;; Apply the rule condition if present
             (condition-met (or (null (rewrite-rule-condition rule))
                              (funcall (rewrite-rule-condition rule) bindings))))
        (when condition-met
          (let* ((rhs-pattern (rewrite-rule-rhs rule))
                 (rhs-expr (if (functionp rhs-pattern)
                             (funcall rhs-pattern bindings)
                             (apply-substitution rhs-pattern bindings))))
            ;; Determine the RHS e-class ID
            (let ((rhs-id 
                   (cond
                     ;; If RHS is a pattern variable that resolved to an e-class ID
                     ((and (integerp rhs-expr) (>= rhs-expr 0))
                      rhs-expr)
                     ;; If RHS is a literal number, create a const
                     ((numberp rhs-expr)
                      (add-expr egraph (sym:lit rhs-expr)))
                     ;; Otherwise, convert pattern to expression
                     (t
                      (let ((rhs-expr-concrete (pattern-to-symbolic-expr-with-egraph egraph rhs-expr bindings)))
                        (add-expr egraph rhs-expr-concrete))))))
              ;; Merge the matched e-class with the RHS e-class
              (unless (= matched-id rhs-id)
                (merge-eclasses egraph matched-id rhs-id)
                (incf new-facts)))))))
    new-facts))

(defun find-matches (egraph pattern)
  "Find all e-nodes that match the given pattern, return list of (eclass-id . bindings)"
  (let ((matches nil))
    (maphash (lambda (enode eclass-id)
               (let ((bindings (match-enode-against-pattern egraph enode pattern)))
                 (when bindings
                   (push (cons eclass-id bindings) matches))))
             (egraph-memo egraph))
    matches))

(defun match-enode-against-pattern (egraph enode pattern)
  "Match an e-node against a pattern, return bindings or nil"
  (cond
    ;; Atomic pattern (number, symbol) - must match enode op exactly
    ((atom pattern)
     (when (and (null (enode-args enode))
                (equal (enode-op enode) pattern))
       '()))
    
    ;; List pattern - match structure recursively
    ((listp pattern)
     (let ((pattern-op (first pattern))
           (pattern-args (rest pattern)))
       (when (and (eq (enode-op enode) pattern-op)
                  (= (length (enode-args enode)) (length pattern-args)))
         ;; Try to match all arguments
         (match-args-against-patterns egraph (enode-args enode) pattern-args '()))))
    
    (t nil)))

(defun match-args-against-patterns (egraph arg-ids patterns bindings)
  "Match a list of e-class IDs against patterns, return unified bindings or nil"
  (if (null patterns)
      bindings
      (let* ((arg-id (first arg-ids))
             (pattern (first patterns))
             (new-bindings (match-eclass-against-pattern egraph arg-id pattern bindings)))
        (when new-bindings
          (match-args-against-patterns egraph (rest arg-ids) (rest patterns) new-bindings)))))

(defun match-eclass-against-pattern (egraph eclass-id pattern bindings)
  "Match an e-class against a pattern, return updated bindings or nil"
  (cond
    ;; Pattern variable
    ((pattern-var-p pattern)
     (let ((existing (assoc pattern bindings)))
       (if existing
           ;; Variable already bound - check consistency
           (when (= (cdr existing) eclass-id)
             bindings)
           ;; New binding
           (cons (cons pattern eclass-id) bindings))))
    
    ;; Literal pattern - check if e-class contains matching literal
    ((atom pattern)
     (when (eclass-contains-literal egraph eclass-id pattern)
       bindings))
    
    ;; Complex pattern - check if any e-node in the class matches
    ((listp pattern)
     (let ((eclass (gethash eclass-id (egraph-classes egraph))))
       (when eclass
         (some (lambda (enode)
                 (match-enode-against-pattern egraph enode pattern))
               (eclass-nodes eclass)))))
    
    (t nil)))

(defun eclass-contains-literal (egraph eclass-id literal)
  "Check if an e-class contains a literal value"
  (let ((canonical-id (uf-find (egraph-unionfind egraph) eclass-id)))
    (let ((eclass (gethash canonical-id (egraph-classes egraph))))
      (when eclass
        (some (lambda (enode)
                (and (null (enode-args enode))
                     (equal (enode-op enode) literal)))
              (eclass-nodes eclass))))))


;; Utility functions for checking special values
(defun is-zero-eclass (egraph eclass-id)
  "Check if an e-class represents zero"
  (eclass-contains-literal egraph eclass-id 0))

(defun is-one-eclass (egraph eclass-id)
  "Check if an e-class represents one"
  (eclass-contains-literal egraph eclass-id 1))

;; Helper functions for pattern-to-expression conversion

(defun enode-to-pattern (enode)
  "Convert an e-node to a pattern (for internal use)"
  (if (null (enode-args enode))
      (enode-op enode)
      (cons (enode-op enode) (enode-args enode))))

(defun pattern-to-symbolic-expr (pattern)
  "Convert a pattern (with variables substituted) to a symbolic expression"
  (cond
    ((symbolp pattern)
     (sym:sym pattern))
    ((numberp pattern)
     (sym:lit pattern))
    ((listp pattern)
     (let ((op (first pattern))
           (args (rest pattern)))
       (if args
           (sym:symbolic op (mapcar #'pattern-to-symbolic-expr args))
           (sym:sym op))))
    (t pattern)))

(defun pattern-to-symbolic-expr-with-egraph (egraph pattern bindings)
  "Convert a pattern to symbolic expression, substituting e-class IDs with expressions"
  (cond
    ((pattern-var-p pattern)
     ;; Look up the binding and convert e-class ID to expression
     (let ((binding (assoc pattern bindings)))
       (if binding
           (let ((eclass-id (cdr binding)))
             (if (integerp eclass-id)
                 ;; Extract the best expression from this e-class
                 (extract-best egraph eclass-id)
                 eclass-id))
           pattern)))
    ((symbolp pattern)
     (sym:sym pattern))
    ((numberp pattern)
     (sym:lit pattern))
    ((listp pattern)
     (let ((op (first pattern))
           (args (rest pattern)))
       (if args
           (sym:symbolic op 
             (mapcar (lambda (arg) (pattern-to-symbolic-expr-with-egraph egraph arg bindings))
                     args))
           (sym:sym op))))
    (t pattern)))

(defun saturate (egraph rules &key (limit 10) (timeout nil))
  "Run equality saturation until fixed point or limits reached"
  (let ((iteration 0)
        (start-time (get-internal-real-time)))
    (loop
      (when (and timeout 
                 (> (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)
                    timeout))
        (return :timeout))
      
      (when (>= iteration limit)
        (return :limit-reached))
      
      (let ((new-facts (apply-rewrites egraph rules)))
        (rebuild egraph)
        (incf iteration)
        
        (when (zerop new-facts)
          (return :saturated))))))

;; Standard rules are defined above

(defun optimize-with-egraph (expr &key (rules *standard-rules*) (iterations 10))
  "Optimize expression using e-graph equality saturation"
  ;; Ensure rules are initialized
  (when (null *standard-rules*)
    (init-standard-rules))
  (let ((egraph (create-egraph)))
    ;; Add the expression to the e-graph
    (let ((expr-id (add-expr egraph expr)))
      ;; Run equality saturation
      (saturate egraph rules :limit iterations)
      ;; Extract the best equivalent expression
      (let ((result (extract-best egraph expr-id)))
        ;; Return result, or original if nil
        (or result expr)))))

(defun saturate-rules (egraph rules &key (limit 10))
  "Apply rewrite rules to an e-graph until saturation - interface for tests"
  (saturate egraph rules :limit limit))

;; Rules will be initialized on first use
