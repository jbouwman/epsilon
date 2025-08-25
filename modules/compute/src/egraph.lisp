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
  (format t "~%DEBUG egraph-add-enode: Adding enode with op: ~S, args: ~S~%" 
          (enode-op enode) (enode-args enode))
  (let* ((canonical (canonicalize-enode egraph enode))
         (existing-id (gethash canonical (egraph-memo egraph))))
    (format t "  -> Canonical enode: op=~S, args=~S~%" 
            (enode-op canonical) (enode-args canonical))
    (if existing-id
        (progn
          (format t "  -> Found existing e-class: ~S~%" existing-id)
          (uf-find (egraph-unionfind egraph) existing-id))
        (let ((new-id (egraph-next-id egraph)))
          (format t "  -> Creating new e-class with ID: ~S~%" new-id)
          ;; Create new e-class
          (incf (egraph-next-id egraph))
          (uf-make-set (egraph-unionfind egraph) new-id)
          (let ((new-class (make-eclass :id new-id :nodes (list canonical))))
            (setf (gethash new-id (egraph-classes egraph)) new-class)
            (setf (gethash canonical (egraph-memo egraph)) new-id)
            (format t "  -> Added to memo. Memo now has ~S entries~%" 
                    (hash-table-count (egraph-memo egraph)))
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
  (format t "~%DEBUG add-expr: Adding expr: ~S (type: ~S)~%" expr (type-of expr))
  (cond
    ((sym:var-p expr)
     (format t "  -> It's a var: ~S~%" (sym:var-name expr))
     (let ((enode (make-enode :op (sym:var-name expr) :args nil)))
       (egraph-add-enode egraph enode)))
    
    ((sym:const-p expr)
     (format t "  -> It's a const: ~S~%" (sym:const-value expr))
     (let ((enode (make-enode :op (sym:const-value expr) :args nil)))
       (egraph-add-enode egraph enode)))
    
    ((sym:expr-p expr)
     ;; Recursively add arguments first, then add this expression
     (let* ((op (sym:expr-op expr))
            (arg-exprs (sym:expr-args expr)))
       (format t "  -> It's an expr with op: ~S, args: ~S~%" op arg-exprs)
       (let* ((arg-ids (mapcar (lambda (arg) 
                                (format t "  -> Adding arg: ~S~%" arg)
                                (add-expr egraph arg)) 
                              arg-exprs))
              (enode (make-enode :op op :args arg-ids)))
         (format t "  -> Created enode with op: ~S, arg-ids: ~S~%" op arg-ids)
         (let ((result-id (egraph-add-enode egraph enode)))
           (format t "  -> Added enode, got ID: ~S~%" result-id)
           (format t "  -> Memo now has ~S entries~%" (hash-table-count (egraph-memo egraph)))
           result-id))))
    
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
                 ;; Re-canonicalize and potentially merge
                 (let* ((canonical (canonicalize-enode egraph enode))
                        (existing-id (gethash canonical (egraph-memo egraph))))
                   (when (and existing-id (not (= existing-id old-id)))
                     (merge-eclasses egraph existing-id old-id))))))))

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
    (let ((eclass (gethash eclass-id (egraph-classes egraph))))
      (when eclass
        (let ((min-cost most-positive-fixnum)
              (best-node nil))
          (dolist (enode (eclass-nodes eclass))
            ;; Recursively compute costs for arguments
            (dolist (arg-id (enode-args enode))
              (extract-costs egraph arg-id cost-fn costs best-nodes))
            ;; Compute cost of this e-node
            (let ((node-cost (compute-enode-cost enode cost-fn costs)))
              (when (< node-cost min-cost)
                (setf min-cost node-cost)
                (setf best-node enode))))
          ;; Record the best choice for this e-class
          (setf (gethash eclass-id costs) min-cost)
          (setf (gethash eclass-id best-nodes) best-node))))))

(defun compute-enode-cost (enode cost-fn costs)
  "Compute the cost of an e-node given costs of its arguments"
  (let ((arg-costs (mapcar (lambda (id) (gethash id costs 0)) (enode-args enode))))
    (funcall cost-fn enode arg-costs)))

(defun expression-size-cost (enode arg-costs)
  "Simple cost function: size of expression tree"
  (declare (ignore enode))
  (1+ (reduce #'+ arg-costs :initial-value 0)))

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
        (mul-op (find-symbol "*" "EPSILON.COMPUTE")))
    ;; Debug: print what symbols we found
    (format t "~%DEBUG init-standard-rules: add-op=~S mul-op=~S~%" add-op mul-op)
    (setf *standard-rules*
          (list
            ;; Additive identity
            (make-rewrite 'add-zero-right `(,add-op ?x 0) '?x)
            (make-rewrite 'add-zero-left `(,add-op 0 ?x) '?x)
            
            ;; Multiplicative identity
            (make-rewrite 'mul-one-right `(,mul-op ?x 1) '?x)
            (make-rewrite 'mul-one-left `(,mul-op 1 ?x) '?x)
            
            ;; Multiplicative zero
            (make-rewrite 'mul-zero-right `(,mul-op ?x 0) '0)
            (make-rewrite 'mul-zero-left `(,mul-op 0 ?x) '0)
            
            ;; Commutativity
            (make-rewrite 'add-comm `(,add-op ?x ?y) `(,add-op ?y ?x))
            (make-rewrite 'mul-comm `(,mul-op ?x ?y) `(,mul-op ?y ?x))
            
            ;; Associativity
            (make-rewrite 'add-assoc `(,add-op (,add-op ?x ?y) ?z) `(,add-op ?x (,add-op ?y ?z)))
            (make-rewrite 'mul-assoc `(,mul-op (,mul-op ?x ?y) ?z) `(,mul-op ?x (,mul-op ?y ?z)))
            
            ;; Distribution
            (make-rewrite 'distribute `(,mul-op ?x (,add-op ?y ?z)) 
                         `(,add-op (,mul-op ?x ?y) (,mul-op ?x ?z)))))))

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
    ;; Debug: check if multiplication-by-zero rules find matches
    (when (and (member (rewrite-rule-name rule) '(mul-zero-right mul-zero-left))
               (null matches))
      (format t "~%DEBUG: No matches for rule ~A, pattern: ~S~%" 
              (rewrite-rule-name rule) (rewrite-rule-lhs rule)))
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
  (let ((matches nil)
        (mul-op (find-symbol "*" "EPSILON.COMPUTE")))
    ;; Debug: check what's in the memo table
    (when (and (listp pattern) (eq (first pattern) mul-op))
      (format t "~%DEBUG find-matches: Looking for mul pattern, memo has ~A entries~%" 
              (hash-table-count (egraph-memo egraph)))
      (maphash (lambda (enode eclass-id)
                 (when (eq (enode-op enode) mul-op)
                   (format t "  Found mul enode: ~S -> ~S~%" enode eclass-id)))
               (egraph-memo egraph)))
    ;; For now, implement specific pattern matching for our standard rules
    (maphash (lambda (enode eclass-id)
               (let ((bindings (match-specific-patterns egraph enode pattern)))
                 (when bindings
                   (push (cons eclass-id bindings) matches))))
             (egraph-memo egraph))
    matches))

(defun match-specific-patterns (egraph enode pattern)
  "Match specific patterns we know about"
  (let ((add-op (find-symbol "+" "EPSILON.COMPUTE"))
        (mul-op (find-symbol "*" "EPSILON.COMPUTE")))
    ;; Debug: print what we're trying to match
    (when (and (listp pattern) (member (first pattern) (list add-op mul-op)))
      (format t "~%DEBUG match: enode-op=~S pattern-op=~S eq?=~S~%" 
              (enode-op enode) (first pattern) (eq (enode-op enode) (first pattern))))
    (cond
      ;; Match (+ ?x 0) - addition with zero
      ((and (listp pattern) (eq (first pattern) add-op) (= (length pattern) 3)
            (pattern-var-p (second pattern)) (eql (third pattern) 0))
       (when (and (eq (enode-op enode) add-op) (= (length (enode-args enode)) 2))
         ;; Check if second argument is zero
         (let ((second-arg-id (second (enode-args enode))))
           (when (is-zero-eclass egraph second-arg-id)
             (list (cons (second pattern) (first (enode-args enode))))))))
      
      ;; Match (+ 0 ?x) - addition with zero (left)
      ((and (listp pattern) (eq (first pattern) add-op) (= (length pattern) 3)
            (eql (second pattern) 0) (pattern-var-p (third pattern)))
       (when (and (eq (enode-op enode) add-op) (= (length (enode-args enode)) 2))
         ;; Check if first argument is zero
         (let ((first-arg-id (first (enode-args enode))))
           (when (is-zero-eclass egraph first-arg-id)
             (list (cons (third pattern) (second (enode-args enode))))))))
      
      ;; Match (* ?x 0) - multiplication by zero
      ((and (listp pattern) (eq (first pattern) mul-op) (= (length pattern) 3)
            (pattern-var-p (second pattern)) (eql (third pattern) 0))
       (when (and (eq (enode-op enode) mul-op) (= (length (enode-args enode)) 2))
         (let ((second-arg-id (second (enode-args enode))))
           (when (is-zero-eclass egraph second-arg-id)
             ;; Return bindings for the match
             (list (cons (second pattern) (first (enode-args enode))))))))
      
      ;; Match (* ?x 1) - multiplication by one
      ((and (listp pattern) (eq (first pattern) mul-op) (= (length pattern) 3)
            (pattern-var-p (second pattern)) (eql (third pattern) 1))
       (when (and (eq (enode-op enode) mul-op) (= (length (enode-args enode)) 2))
         (let ((second-arg-id (second (enode-args enode))))
           (when (is-one-eclass egraph second-arg-id)
             (list (cons (second pattern) (first (enode-args enode))))))))
      
      ;; Handle left versions for multiplication
      ((and (listp pattern) (eq (first pattern) mul-op) (= (length pattern) 3)
            (eql (second pattern) 0) (pattern-var-p (third pattern)))
       (when (and (eq (enode-op enode) mul-op) (= (length (enode-args enode)) 2))
         (let ((first-arg-id (first (enode-args enode))))
           (when (is-zero-eclass egraph first-arg-id)
             (list (cons (third pattern) (second (enode-args enode))))))))
      
      ((and (listp pattern) (eq (first pattern) mul-op) (= (length pattern) 3)
            (eql (second pattern) 1) (pattern-var-p (third pattern)))
       (when (and (eq (enode-op enode) mul-op) (= (length (enode-args enode)) 2))
         (let ((first-arg-id (first (enode-args enode))))
           (when (is-one-eclass egraph first-arg-id)
             (list (cons (third pattern) (second (enode-args enode))))))))
      
      (t nil))))

(defun is-zero-eclass (egraph eclass-id)
  "Check if an e-class represents zero"
  (let ((canonical-id (uf-find (egraph-unionfind egraph) eclass-id)))
    (let ((eclass (gethash canonical-id (egraph-classes egraph))))
      (when eclass
        (some (lambda (enode)
                (and (null (enode-args enode))
                     (eql (enode-op enode) 0)))
              (eclass-nodes eclass))))))

(defun is-one-eclass (egraph eclass-id)
  "Check if an e-class represents one"
  (let ((canonical-id (uf-find (egraph-unionfind egraph) eclass-id)))
    (let ((eclass (gethash canonical-id (egraph-classes egraph))))
      (when eclass
        (some (lambda (enode)
                (and (null (enode-args enode))
                     (eql (enode-op enode) 1)))
              (eclass-nodes eclass))))))

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
  (format t "~%DEBUG optimize-with-egraph: Called with expr: ~S~%" expr)
  ;; Ensure rules are initialized
  (when (null *standard-rules*)
    (format t "  -> Initializing standard rules~%")
    (init-standard-rules))
  (let ((egraph (create-egraph)))
    (format t "  -> Created e-graph~%")
    ;; Add the expression to the e-graph
    (let ((expr-id (add-expr egraph expr)))
      (format t "  -> Added expr, got ID: ~S~%" expr-id)
      (format t "  -> E-graph memo has ~S entries~%" (hash-table-count (egraph-memo egraph)))
      ;; Run equality saturation
      (saturate egraph rules :limit iterations)
      (format t "  -> After saturation, memo has ~S entries~%" (hash-table-count (egraph-memo egraph)))
      ;; Extract the best equivalent expression
      (let ((result (extract-best egraph expr-id)))
        (format t "  -> Extracted best: ~S~%" result)
        ;; Return result, or original if nil
        (or result expr)))))

(defun saturate-rules (egraph rules &key (limit 10))
  "Apply rewrite rules to an e-graph until saturation - interface for tests"
  (saturate egraph rules :limit limit))

;; Rules will be initialized on first use