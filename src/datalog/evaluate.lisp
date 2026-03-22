;;;; evaluate.lisp - Semi-Naive Bottom-Up Evaluator
;;;;
;;;; Evaluates stratified Datalog programs using the semi-naive algorithm:
;;;; for each stratum, iteratively apply rules using only facts derived in
;;;; the previous iteration (delta) until no new facts are produced (fixpoint).
;;;;
;;;; Handles: positive atoms, comparisons, negation, is-expressions,
;;;; and aggregation.

(defpackage :epsilon.datalog.evaluate
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:ast :epsilon.datalog.ast)
                    (:unify :epsilon.datalog.unify)
                    (:strat :epsilon.datalog.stratify)
                    (:mem :epsilon.datalog.backend.memory)
                    (:m :epsilon.map)
                    (:match :epsilon.match))
  (:export
   #:evaluate #:evaluate-with-aggregation
   #:query
   #:evaluate-stratum #:apply-rule #:evaluate-body))

(in-package :epsilon.datalog.evaluate)

;;; ============================================================
;;; Arithmetic Expression Evaluation
;;; ============================================================

(defun variable-keyword-p (x)
  "Check if X is a Datalog variable keyword (starts with ?)."
  (and (keywordp x)
       (> (length (symbol-name x)) 1)
       (char= #\? (char (symbol-name x) 0))))

(defun eval-arith-expr (expr subst)
  "Evaluate an arithmetic expression under a substitution.
   Variables (keywords starting with ?) are looked up in the substitution.
   Supports +, -, *, / operators."
  (match:match expr
    ((? numberp n) n)
    ((? variable-keyword-p var)
     (let ((bound (unify:lookup subst var)))
       (if bound
           (if (ast:constant-p bound)
               (ast:constant-value bound)
               (error "Variable ~A is not ground in arithmetic expression" var))
           (error "Unbound variable ~A in arithmetic expression" var))))
    ((list* op rest)
     (let ((args (mapcar (lambda (a) (eval-arith-expr a subst)) rest)))
       (case op
         (+ (reduce #'+ args))
         (- (if (= 1 (length args)) (- (first args)) (reduce #'- args)))
         (* (reduce #'* args))
         (/ (reduce #'/ args))
         (otherwise (error "Unknown arithmetic operator: ~A" op)))))
    (_ (error "Invalid arithmetic expression: ~S" expr))))

;;; ============================================================
;;; Comparison Evaluation
;;; ============================================================

(defun eval-comparison (comparison subst)
  "Evaluate a comparison under a substitution. Returns T or NIL."
  (let* ((left-term (ast:comparison-left comparison))
         (right-term (ast:comparison-right comparison))
         (left-val (resolve-term-value left-term subst))
         (right-val (resolve-term-value right-term subst)))
    (when (and left-val right-val)
      (let ((op (ast:comparison-op comparison)))
        (case op
          (:= (equal left-val right-val))
          (:> (> left-val right-val))
          (:< (< left-val right-val))
          (:>= (>= left-val right-val))
          (:<= (<= left-val right-val))
          (:<> (not (equal left-val right-val)))
          (otherwise (error "Unknown comparison operator: ~A" op)))))))

(defun resolve-term-value (term subst)
  "Resolve a term to its ground value under substitution. Returns the value or nil."
  (let ((resolved (unify:apply-subst subst term)))
    (if (ast:constant-p resolved)
        (ast:constant-value resolved)
        nil)))

;;; ============================================================
;;; Fact Matching
;;; ============================================================

(defun make-fact-atom (relation tuple)
  "Create a ground atom from a relation name and tuple (list of values)."
  (ast:make-atom
   :relation relation
   :terms (mapcar #'ast:constant tuple)))

(defun match-atom-against-facts (atom store subst)
  "Find all substitutions that extend SUBST and match ATOM against facts in STORE.
   Returns a list of substitutions."
  (let ((relation (ast:atom-relation atom))
        (results nil))
    (dolist (tuple (mem:all-facts store relation))
      (let ((fact-atom (make-fact-atom relation tuple)))
        (let ((new-subst (unify:unify-atoms atom fact-atom subst)))
          (when new-subst
            (push new-subst results)))))
    results))

;;; ============================================================
;;; Body Evaluation
;;; ============================================================

(defun evaluate-body (body-literals store subst)
  "Evaluate body literals left-to-right, threading substitutions.
   Positive atoms: match against facts.
   Comparisons: filter substitutions.
   Negations: check that no matching facts exist.
   Is-expressions: evaluate arithmetic and bind result.
   Returns a list of substitutions satisfying all literals."
  (if (null body-literals)
      (list subst)
      (let ((literal (first body-literals))
            (rest-body (rest body-literals)))
        (match:match literal
          ((? ast:atom-p)
           (mapcan (lambda (s) (evaluate-body rest-body store s))
                   (match-atom-against-facts literal store subst)))
          ((? ast:comparison-p)
           (when (eval-comparison literal subst)
             (evaluate-body rest-body store subst)))
          ((? ast:negation-p)
           (when (eval-negation literal store subst)
             (evaluate-body rest-body store subst)))
          ((? ast:is-expr-p)
           (let ((new-subst (eval-is-expr literal subst)))
             (when new-subst
               (evaluate-body rest-body store new-subst))))
          ((? ast:text-search-p)
           (let ((new-substs (eval-text-search literal store subst)))
             (when new-substs
               (mapcan (lambda (s) (evaluate-body rest-body store s))
                       new-substs))))
          (_ (error "Unknown body literal type: ~S" literal))))))

(defun eval-negation (negation store subst)
  "Evaluate a negation: check that no facts match the negated atom
   that also satisfy the guard constraints. Returns T if the negation holds."
  (let* ((neg-atom (ast:negation-atom negation))
         (guards (ast:negation-guards negation))
         (matching-substs (match-atom-against-facts neg-atom store subst)))
    ;; Filter by guards
    (when guards
      (setf matching-substs
            (remove-if-not
             (lambda (s)
               (every (lambda (guard) (eval-comparison guard s))
                      guards))
             matching-substs)))
    ;; Negation holds if NO matching substitutions exist
    (null matching-substs)))

(defun eval-is-expr (is-expr subst)
  "Evaluate an is-expression: compute the arithmetic result and bind to target.
   Returns extended substitution or nil on failure."
  (handler-case
      (let* ((result (eval-arith-expr (ast:is-expr-expression is-expr) subst))
             (target (ast:is-expr-target is-expr))
             (target-name (ast:variable-name target)))
        (unify:extend subst target-name (ast:constant result)))
    (error () nil)))

;;; ============================================================
;;; Text-Search Evaluation (In-Memory)
;;; ============================================================

(defun eval-text-search (text-search store subst)
  "Evaluate a text-search predicate in-memory using substring matching.
   Checks if the target variable is already bound; if so, tests the bound
   value against the query. If unbound, this is a no-op filter (returns
   the current substitution unchanged, as actual text-search filtering
   happens at the Meilisearch backend level).
   Returns a list of substitutions."
  (declare (ignore store))
  (let* ((target (ast:text-search-target text-search))
         (query (ast:text-search-query text-search))
         (target-name (ast:variable-name target))
         (bound (unify:lookup subst target-name)))
    (if bound
        ;; Target is already bound -- check substring match
        (let ((val (if (ast:constant-p bound)
                       (ast:constant-value bound)
                       nil)))
          (if (and val (stringp val)
                   (search (string-downcase query) (string-downcase val)))
              (list subst)
              nil))
        ;; Target is unbound -- pass through (text-search acts as hint)
        (list subst))))

;;; ============================================================
;;; Rule Application
;;; ============================================================

(defun apply-rule (rule store)
  "Apply a rule to the fact store. Returns a list of new tuples for the head relation.
   Each tuple is a list of ground values."
  (let ((head (ast:rule-head rule))
        (body (ast:rule-body rule))
        (new-tuples nil))
    (let ((substs (evaluate-body body store unify:+empty-subst+)))
      (dolist (subst substs)
        (when (unify:ground-atom-p subst head)
          (let ((tuple (unify:extract-values subst head)))
            (pushnew tuple new-tuples :test #'equal)))))
    new-tuples))

;;; ============================================================
;;; Semi-Naive Evaluation
;;; ============================================================

(defun collect-body-relations (rule)
  "Extract all relation keywords referenced in a rule's body (positive atoms
   and negated atoms)."
  (let ((rels nil))
    (dolist (lit (ast:rule-body rule))
      (match:match lit
        ((? ast:atom-p)
         (pushnew (ast:atom-relation lit) rels))
        ((? ast:negation-p)
         (pushnew (ast:atom-relation (ast:negation-atom lit)) rels))
        (_ nil)))
    rels))

(defun build-body-relation-index (rules)
  "Build a hash-table mapping each body relation keyword to the list of rules
   that reference it. Used to determine which rules need re-evaluation when
   a relation gains new facts."
  (let ((index (make-hash-table :test 'eq)))
    (dolist (rule rules)
      (dolist (rel (collect-body-relations rule))
        (push rule (gethash rel index))))
    index))

(defun apply-rules-naive (rules store)
  "Apply all rules to store, collect new facts. Returns (values new-store delta-count)."
  (let ((new-store store)
        (delta-count 0))
    (dolist (rule rules)
      (let* ((head-rel (ast:head-relation rule))
             (new-tuples (apply-rule rule new-store)))
        (dolist (tuple new-tuples)
          (unless (mem:has-fact-p new-store head-rel tuple)
            (setf new-store (mem:assert-fact new-store head-rel tuple))
            (incf delta-count)))))
    (values new-store delta-count)))

(defun evaluate-stratum (rules store &key (max-iterations 1000))
  "Evaluate a single stratum to fixpoint using dependency-driven iteration.
   Only re-applies rules whose body relations gained new facts since the
   last iteration. Uses per-relation count snapshots for fast convergence
   detection. Returns the updated store."
  (let* ((dep-index (build-body-relation-index rules))
         ;; Initially all relations mentioned in rule bodies are dirty
         (dirty (make-hash-table :test 'eq)))
    ;; Seed dirty set with all body relations that currently have facts
    (dolist (rule rules)
      (dolist (rel (collect-body-relations rule))
        (setf (gethash rel dirty) t)))
    (loop for iteration from 1 to max-iterations
          do (let ((active-rules nil)
                   (next-dirty (make-hash-table :test 'eq)))
               ;; Collect rules that have at least one dirty body relation
               (maphash (lambda (rel _v)
                          (declare (ignore _v))
                          (dolist (rule (gethash rel dep-index))
                            (pushnew rule active-rules :test #'eq)))
                        dirty)
               (when (null active-rules)
                 (return store))
               ;; Apply each active rule, using count snapshots for convergence
               (dolist (rule active-rules)
                 (let* ((head-rel (ast:head-relation rule))
                        (count-before (mem:count-pattern store head-rel))
                        (new-tuples (apply-rule rule store)))
                   (setf store (mem:assert-facts store head-rel new-tuples))
                   (let ((count-after (mem:count-pattern store head-rel)))
                     (when (> count-after count-before)
                       (setf (gethash head-rel next-dirty) t)))))
               ;; Check convergence
               (when (= 0 (hash-table-count next-dirty))
                 (return store))
               (setf dirty next-dirty))
          finally (error "Evaluation did not converge after ~D iterations"
                         max-iterations))))

;;; ============================================================
;;; Full Program Evaluation
;;; ============================================================

(defun evaluate (program store &key (max-iterations 1000))
  "Evaluate a Datalog program against the fact store.
   Stratifies the program, evaluates each stratum to fixpoint.
   Returns the final store with all derived facts."
  (let* ((stratified (strat:stratify program))
         (rules-by-stratum (strat:stratified-program-rules-by-stratum stratified)))
    (dolist (entry rules-by-stratum)
      (let ((rules (cdr entry)))
        (when rules
          (setf store (evaluate-stratum rules store
                                        :max-iterations max-iterations)))))
    store))

;;; ============================================================
;;; Query Interface
;;; ============================================================

(defun query (program store relation &key constraints order-by limit)
  "Evaluate a program and query a specific relation.
   CONSTRAINTS is a list of (op var-index value) for filtering.
   ORDER-BY is a list of (field-index :asc/:desc) for sorting.
   LIMIT is the maximum number of results.
   Returns a list of tuples."
  (let* ((result-store (evaluate program store))
         (tuples (mem:all-facts result-store relation)))
    ;; Apply constraints
    (when constraints
      (setf tuples (apply-constraints tuples constraints)))
    ;; Apply ordering
    (when order-by
      (setf tuples (apply-ordering tuples order-by)))
    ;; Apply limit
    (when limit
      (setf tuples (subseq tuples 0 (min limit (length tuples)))))
    tuples))

(defun apply-constraints (tuples constraints)
  "Filter tuples by constraints. Each constraint is (op field-index value)."
  (dolist (constraint constraints)
    (destructuring-bind (op field-index value) constraint
      (setf tuples
            (remove-if-not
             (lambda (tuple)
               (let ((field-val (nth field-index tuple)))
                 (case op
                   (:= (equal field-val value))
                   (:> (> field-val value))
                   (:< (< field-val value))
                   (:>= (>= field-val value))
                   (:<= (<= field-val value))
                   (:<> (not (equal field-val value)))
                   (otherwise t))))
             tuples))))
  tuples)

(defun apply-ordering (tuples order-by)
  "Sort tuples by field index and direction. ORDER-BY is (field-index :asc/:desc)."
  (destructuring-bind (field-index direction) order-by
    (sort (copy-list tuples)
          (if (eq direction :asc)
              (lambda (a b) (< (nth field-index a) (nth field-index b)))
              (lambda (a b) (> (nth field-index a) (nth field-index b)))))))

;;; ============================================================
;;; Aggregation
;;; ============================================================

(defun evaluate-with-aggregation (program store &key (max-iterations 1000))
  "Evaluate with aggregation support. After fixpoint, compute aggregates."
  ;; First evaluate to fixpoint
  (let ((result (evaluate program store :max-iterations max-iterations)))
    ;; Then apply aggregation rules
    (dolist (rule (ast:program-rules program))
      (let ((head-terms (ast:atom-terms (ast:rule-head rule))))
        (when (some #'ast:aggregate-p head-terms)
          (setf result (apply-aggregation-rule rule result)))))
    result))

(defun apply-aggregation-rule (rule store)
  "Apply an aggregation rule. Group by non-aggregate head variables,
   compute aggregate over the aggregate variable."
  (let* ((head (ast:rule-head rule))
         (head-terms (ast:atom-terms head))
         (head-rel (ast:atom-relation head))
         ;; Find group-by positions and aggregate position
         (group-indices nil)
         (agg-index nil)
         (agg-op nil)
         (agg-var nil))
    ;; Classify head terms
    (loop for term in head-terms
          for i from 0
          do (if (ast:aggregate-p term)
                 (setf agg-index i
                       agg-op (ast:aggregate-op term)
                       agg-var (ast:aggregate-var term))
                 (push i group-indices)))
    (setf group-indices (nreverse group-indices))

    (when (and agg-index agg-op)
      ;; Evaluate body to get all tuples
      (let* ((body (ast:rule-body rule))
             (all-substs (evaluate-body body store unify:+empty-subst+)))
        ;; Group by non-aggregate head variables
        (let ((groups (make-hash-table :test 'equal)))
          (dolist (subst all-substs)
            (let* ((group-key (mapcar
                               (lambda (i)
                                 (let ((term (nth i head-terms)))
                                   (when (ast:variable-p term)
                                     (resolve-term-value term subst))))
                               group-indices))
                   (agg-val (resolve-term-value (ast:variable agg-var) subst)))
              (when agg-val
                (push agg-val (gethash group-key groups nil)))))

          ;; Compute aggregates and assert results
          (maphash
           (lambda (group-key values)
             (let* ((agg-result (compute-aggregate agg-op values))
                    (tuple (make-list (length head-terms))))
               ;; Fill in group-by values
               (loop for idx in group-indices
                     for val in group-key
                     do (setf (nth idx tuple) val))
               ;; Fill in aggregate result
               (setf (nth agg-index tuple) agg-result)
               (setf store (mem:assert-fact store head-rel tuple))))
           groups))))
    store))

(defun compute-aggregate (op values)
  "Compute an aggregate over a list of values."
  (case op
    (:count (length values))
    (:sum (reduce #'+ values))
    (:avg (if values (/ (reduce #'+ values) (length values)) 0))
    (:min (reduce #'min values))
    (:max (reduce #'max values))
    (otherwise (error "Unknown aggregate operator: ~A" op))))
