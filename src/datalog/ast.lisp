;;;; ast.lisp - Datalog AST
;;;;
;;;; Immutable data structures representing Datalog programs: terms, atoms,
;;;; rules, comparisons, negation, is-expressions, relation declarations,
;;;; and programs.
;;;;
;;;; Terms are sum types via defdata. Product types use defstruct with
;;;; :read-only t slots, following the pattern of epsilon.sql.query.ast.

(defpackage :epsilon.datalog.ast
  (:use :cl :epsilon.syntax)
  (:shadow #:atom #:variable #:constant)
  (:export
   ;; Terms (sum type)
   #:term #:term-p
   #:variable #:variable-p #:variable-name
   #:constant #:constant-p #:constant-value
   #:aggregate #:aggregate-p #:aggregate-op #:aggregate-var
   ;; Atoms
   #:atom #:atom-p #:make-atom #:atom-relation #:atom-terms
   ;; Comparisons
   #:comparison #:comparison-p #:make-comparison
   #:comparison-op #:comparison-left #:comparison-right
   ;; Negation
   #:negation #:negation-p #:make-negation
   #:negation-atom #:negation-guards
   ;; Is-Expression
   #:is-expr #:is-expr-p #:make-is-expr
   #:is-expr-target #:is-expr-expression
   ;; Rules
   #:rule #:rule-p #:make-rule #:rule-head #:rule-body
   ;; Field and Relation Declarations
   #:field-decl #:field-decl-p #:make-field-decl
   #:field-decl-name #:field-decl-field-type
   #:relation-decl #:relation-decl-p #:make-relation-decl
   #:relation-decl-name #:relation-decl-fields
   ;; Programs
   #:program #:program-p #:make-program
   #:program-name #:program-relations #:program-rules
   ;; Utility predicates
   #:body-literal-p
   #:head-relation
   #:rule-body-atoms #:rule-body-comparisons
   #:rule-body-negations #:rule-body-is-exprs
   ;; Text Search
   #:text-search #:text-search-p #:make-text-search
   #:text-search-target #:text-search-query
   #:rule-body-text-searches
   #:atom-variables
   #:rule-head-variables #:rule-body-variables
   #:program-derived-relations #:program-base-relations))

(in-package :epsilon.datalog.ast)

;;; ============================================================
;;; Terms (sum type)
;;; ============================================================

(epsilon.data:defdata term ()
  "A Datalog term: variable, constant, or aggregate."
  (variable name)
  (constant value)
  (aggregate op var)
  (:deriving show eq-class))

;;; ============================================================
;;; Atoms
;;; ============================================================

(defstruct (atom (:constructor %make-atom) (:copier nil))
  "A Datalog atom: a relation applied to a list of terms."
  (relation nil :read-only t)       ; keyword -- relation name
  (terms nil :type list :read-only t)) ; list of term

(defun make-atom (&key relation terms)
  "Construct an atom with the given relation name and terms."
  (%make-atom :relation relation :terms terms))

;;; ============================================================
;;; Comparisons
;;; ============================================================

(defstruct (comparison (:constructor %make-comparison) (:copier nil))
  "A comparison predicate: op applied to left and right terms."
  (op nil :type keyword :read-only t)   ; :=, :>, :<, :>=, :<=, :<>
  (left nil :read-only t)               ; term
  (right nil :read-only t))             ; term

(defun make-comparison (&key op left right)
  "Construct a comparison with operator and two terms."
  (%make-comparison :op op :left left :right right))

(defun comparison (op left right)
  "Convenience constructor for comparisons."
  (make-comparison :op op :left left :right right))

;;; ============================================================
;;; Negation
;;; ============================================================

(defstruct (negation (:constructor %make-negation) (:copier nil))
  "A negated literal: an atom with optional guard constraints.
   Guards are comparisons that restrict the negated atom's variables."
  (atom nil :read-only t)               ; atom
  (guards nil :type list :read-only t)) ; list of comparison

(defun make-negation (&key atom guards)
  "Construct a negation with an atom and optional guard comparisons."
  (%make-negation :atom atom :guards guards))

;;; ============================================================
;;; Is-Expression
;;; ============================================================

(defstruct (is-expr (:constructor %make-is-expr) (:copier nil))
  "An is-expression: binds a variable to the result of an arithmetic expression.
   The expression is an s-expression using +, -, *, / and variable/constant terms."
  (target nil :read-only t)             ; variable term
  (expression nil :read-only t))        ; s-expression (to be compiled/evaluated)

(defun make-is-expr (&key target expression)
  "Construct an is-expression binding target variable to expression."
  (%make-is-expr :target target :expression expression))

;;; ============================================================
;;; Text-Search Predicate
;;; ============================================================

(defstruct (text-search (:constructor %make-text-search) (:copier nil))
  "A text-search predicate: full-text search on a relation field.
   Used in rule bodies to match values via Meilisearch full-text search."
  (target nil :read-only t)               ; variable term (bound to matched value)
  (query nil :type string :read-only t))  ; search query string

(defun make-text-search (&key target query)
  "Construct a text-search predicate binding target to query results."
  (%make-text-search :target target :query query))

;;; ============================================================
;;; Rules
;;; ============================================================

(defstruct (rule (:constructor %make-rule) (:copier nil))
  "A Datalog rule: head :- body.
   Body is a list of atoms, comparisons, negations, and is-expressions."
  (head nil :read-only t)               ; atom
  (body nil :type list :read-only t))   ; list of (atom | comparison | negation | is-expr)

(defun make-rule (&key head body)
  "Construct a rule with a head atom and body literals."
  (%make-rule :head head :body body))

;;; ============================================================
;;; Relation Declarations
;;; ============================================================

(defstruct (field-decl (:constructor %make-field-decl) (:copier nil))
  "A field declaration within a relation: name and type."
  (name nil :type keyword :read-only t)
  (field-type nil :type keyword :read-only t))

(defun make-field-decl (&key name field-type)
  "Construct a field declaration."
  (%make-field-decl :name name :field-type field-type))

(defstruct (relation-decl (:constructor %make-relation-decl) (:copier nil))
  "A relation (EDB) declaration with typed fields."
  (name nil :type keyword :read-only t)
  (fields nil :type list :read-only t)) ; list of field-decl

(defun make-relation-decl (&key name fields)
  "Construct a relation declaration."
  (%make-relation-decl :name name :fields fields))

;;; ============================================================
;;; Programs
;;; ============================================================

(defstruct (program (:constructor %make-program) (:copier nil))
  "A Datalog program: named collection of relation declarations and rules."
  (name nil :type keyword :read-only t)
  (relations nil :type list :read-only t) ; list of relation-decl
  (rules nil :type list :read-only t))    ; list of rule

(defun make-program (&key name relations rules)
  "Construct a Datalog program."
  (%make-program :name name :relations relations :rules rules))

;;; ============================================================
;;; Utility predicates
;;; ============================================================

(defun body-literal-p (x)
  "Check if X is a valid body literal (atom, comparison, negation, is-expr, or text-search)."
  (or (atom-p x) (comparison-p x) (negation-p x) (is-expr-p x) (text-search-p x)))

(defun head-relation (rule)
  "Return the relation name from a rule's head atom."
  (atom-relation (rule-head rule)))

(defun rule-body-atoms (rule)
  "Return only the positive atoms from a rule's body (no comparisons, negations, is-exprs)."
  (remove-if-not #'atom-p (rule-body rule)))

(defun rule-body-comparisons (rule)
  "Return only the comparisons from a rule's body."
  (remove-if-not #'comparison-p (rule-body rule)))

(defun rule-body-negations (rule)
  "Return only the negations from a rule's body."
  (remove-if-not #'negation-p (rule-body rule)))

(defun rule-body-is-exprs (rule)
  "Return only the is-expressions from a rule's body."
  (remove-if-not #'is-expr-p (rule-body rule)))

(defun rule-body-text-searches (rule)
  "Return only the text-search predicates from a rule's body."
  (remove-if-not #'text-search-p (rule-body rule)))

(defun atom-variables (atom)
  "Return list of variable names occurring in an atom's terms."
  (loop for term in (atom-terms atom)
        when (variable-p term)
          collect (variable-name term)))

(defun rule-head-variables (rule)
  "Return variable names in the rule's head."
  (atom-variables (rule-head rule)))

(defun rule-body-variables (rule)
  "Return all variable names occurring in the rule's body atoms."
  (let ((vars nil))
    (dolist (lit (rule-body rule))
      (cond
        ((atom-p lit)
         (dolist (v (atom-variables lit))
           (pushnew v vars)))
        ((comparison-p lit)
         (when (variable-p (comparison-left lit))
           (pushnew (variable-name (comparison-left lit)) vars))
         (when (variable-p (comparison-right lit))
           (pushnew (variable-name (comparison-right lit)) vars)))
        ((negation-p lit)
         (dolist (v (atom-variables (negation-atom lit)))
           (pushnew v vars)))
        ((is-expr-p lit)
         (when (variable-p (is-expr-target lit))
           (pushnew (variable-name (is-expr-target lit)) vars)))
        ((text-search-p lit)
         (when (variable-p (text-search-target lit))
           (pushnew (variable-name (text-search-target lit)) vars)))))
    (nreverse vars)))

(defun program-derived-relations (program)
  "Return list of relation names that appear as rule heads (IDB relations)."
  (remove-duplicates (mapcar #'head-relation (program-rules program))))

(defun program-base-relations (program)
  "Return list of relation names declared but not derived (EDB relations)."
  (let ((derived (program-derived-relations program)))
    (remove-if (lambda (name) (member name derived))
               (mapcar #'relation-decl-name (program-relations program)))))
