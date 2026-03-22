;;;; parser.lisp - Datalog S-expression Parser
;;;;
;;;; Transforms Lisp s-expressions into Datalog AST nodes.
;;;; No separate tokenizer needed -- the Lisp reader handles everything.
;;;;
;;;; Variable detection: symbols starting with ?
;;;; Wildcards: _ generates unique anonymous variables
;;;; Comparison operators: =, >, <, >=, <=, <>, not

(defpackage :epsilon.datalog.parser
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:ast :epsilon.datalog.ast)
                    (:match :epsilon.match))
  (:export
   #:parse-term #:parse-field #:parse-relation
   #:parse-body-literal #:parse-atom #:parse-comparison
   #:parse-negation #:parse-is-expr #:parse-arithmetic-expr
   #:parse-rule #:parse-program #:parse-query))

(in-package :epsilon.datalog.parser)

;;; ============================================================
;;; Variable / Wildcard Detection
;;; ============================================================

(defvar *wildcard-counter* 0
  "Counter for generating unique anonymous variable names.")

(defun variable-symbol-p (sym)
  "Check if SYM is a Datalog variable (symbol starting with ?)."
  (and (symbolp sym)
       (> (length (symbol-name sym)) 1)
       (char= #\? (char (symbol-name sym) 0))))

(defun wildcard-symbol-p (sym)
  "Check if SYM is a wildcard (_)."
  (and (symbolp sym)
       (string= "_" (symbol-name sym))))

(defun comparison-op-p (sym)
  "Check if SYM is a comparison operator."
  (member sym '(= > < >= <= <> not) :test #'string=))

(defun aggregate-op-p (sym)
  "Check if SYM is an aggregate operator."
  (member sym '(count sum avg min max) :test #'string=))

(defun to-keyword (sym)
  "Convert a symbol to a keyword."
  (if (keywordp sym)
      sym
      (intern (symbol-name sym) :keyword)))

(defun op-to-keyword (sym)
  "Convert a comparison operator symbol to its keyword form."
  (to-keyword sym))

(defun fresh-wildcard ()
  "Generate a fresh anonymous variable name."
  (let ((name (intern (format nil "?_WILD~D" (incf *wildcard-counter*)) :keyword)))
    name))

;;; ============================================================
;;; Term Parsing
;;; ============================================================

(defun parse-term (form)
  "Parse a term from a form. Variables start with ?, _ is wildcard, else constant."
  (match:match form
    ((? wildcard-symbol-p)  (ast:variable (fresh-wildcard)))
    ((? variable-symbol-p)  (ast:variable (to-keyword form)))
    ((? keywordp)           (ast:constant form))
    ((? numberp)            (ast:constant form))
    ((? stringp)            (ast:constant form))
    ((? symbolp s)          (ast:constant (to-keyword s)))
    (_                      (ast:constant form))))

;;; ============================================================
;;; Relation Declaration Parsing
;;; ============================================================

(defun parse-field (form)
  "Parse a field declaration (name type) from a list."
  (destructuring-bind (name type) form
    (ast:make-field-decl :name (to-keyword name) :field-type (to-keyword type))))

(defun parse-relation (form)
  "Parse a relation declaration: (relation name (field type) ...)
   Returns a relation-decl AST node."
  (let* ((body (rest form))          ; skip 'relation
         (name (to-keyword (first body)))
         (field-forms (rest body)))
    (ast:make-relation-decl
     :name name
     :fields (mapcar #'parse-field field-forms))))

;;; ============================================================
;;; Rule Body Literal Parsing
;;; ============================================================

(defun not-symbol-p (x)
  (and (symbolp x) (string= "NOT" (symbol-name x))))

(defun is-symbol-p (x)
  (and (symbolp x) (string= "IS" (symbol-name x))))

(defun text-search-symbol-p (x)
  (and (symbolp x) (string= "TEXT-SEARCH" (symbol-name x))))

(defun relation-symbol-p (x)
  (and (symbolp x) (string= "RELATION" (symbol-name x))))

(defun rule-symbol-p (x)
  (and (symbolp x) (string= "RULE" (symbol-name x))))

(defun parse-body-literal (form)
  "Parse a single body literal.
   Dispatches on the form's head to determine the type:
   - (not ...) -> negation
   - (is ...) -> is-expression
   - (= ...), (> ...), (< ...) etc. -> comparison
   - (relation-name ...) -> positive atom"
  (match:match (first form)
    ((? not-symbol-p)         (parse-negation form))
    ((? is-symbol-p)          (parse-is-expr form))
    ((? text-search-symbol-p) (parse-text-search form))
    ((? comparison-op-p)      (parse-comparison form))
    (_                        (parse-atom form))))

(defun parse-atom (form)
  "Parse a positive atom: (relation-name term1 term2 ...)"
  (let ((relation (to-keyword (first form)))
        (terms (mapcar #'parse-term (rest form))))
    (ast:make-atom :relation relation :terms terms)))

(defun parse-comparison (form)
  "Parse a comparison: (op left right)"
  (destructuring-bind (op left right) form
    (ast:make-comparison :op (op-to-keyword op)
                         :left (parse-term left)
                         :right (parse-term right))))

(defun parse-negation (form)
  "Parse a negation: (not (atom ...) guard1 guard2 ...)
   The first argument after 'not' is the negated atom.
   Remaining arguments are guard comparisons."
  (let* ((body (rest form))          ; skip 'not
         (neg-atom-form (first body))
         (guard-forms (rest body)))
    (ast:make-negation
     :atom (parse-atom neg-atom-form)
     :guards (mapcar #'parse-comparison guard-forms))))

(defun parse-is-expr (form)
  "Parse an is-expression: (is ?var expression)
   The expression uses variable keywords (:?name) and arithmetic ops."
  (destructuring-bind (is-sym target &rest expr-parts) form
    (declare (ignore is-sym))
    (let ((target-term (parse-term target))
          (expression (if (= 1 (length expr-parts))
                         (parse-arithmetic-expr (first expr-parts))
                         ;; Multiple parts: implicit addition
                         (cons '+ (mapcar #'parse-arithmetic-expr expr-parts)))))
      (ast:make-is-expr :target target-term :expression expression))))

(defun parse-text-search (form)
  "Parse a text-search: (text-search ?var \"query\")
   The first argument is the target variable, the second is the search query string."
  (destructuring-bind (ts-sym target query) form
    (declare (ignore ts-sym))
    (ast:make-text-search :target (parse-term target) :query query)))

(defun parse-arithmetic-expr (form)
  "Parse an arithmetic expression. Variables become keywords for later substitution."
  (match:match form
    ((? variable-symbol-p) (to-keyword form))
    ((? numberp n)         n)
    ((list* op args)       (cons op (mapcar #'parse-arithmetic-expr args)))
    (_                     form)))

;;; ============================================================
;;; Rule Parsing
;;; ============================================================

(defun parse-rule (form)
  "Parse a rule: (rule (head-rel t1 t2 ...) body1 body2 ...)
   Returns a rule AST node."
  (let* ((body (rest form))          ; skip 'rule
         (head-form (first body))
         (body-forms (rest body)))
    (ast:make-rule
     :head (parse-atom head-form)
     :body (mapcar #'parse-body-literal body-forms))))

;;; ============================================================
;;; Program Parsing
;;; ============================================================

(defun parse-program (form)
  "Parse a defprogram: (defprogram name relation1 ... rule1 ...)
   Returns a program AST node."
  (let* ((body (rest form))          ; skip 'defprogram
         (name (to-keyword (first body)))
         (items (rest body))
         (relations nil)
         (rules nil))
    ;; Reset wildcard counter for each program
    (setf *wildcard-counter* 0)
    ;; Classify each item as relation or rule
    (dolist (item items)
      (match:match (first item)
        ((? relation-symbol-p) (push (parse-relation item) relations))
        ((? rule-symbol-p)     (push (parse-rule item) rules))
        (_                     (error "Unknown program form: ~S" item))))
    (ast:make-program
     :name name
     :relations (nreverse relations)
     :rules (nreverse rules))))

;;; ============================================================
;;; Query Parsing
;;; ============================================================

(defun parse-query (form)
  "Parse a query form: (query program-name (rel ?v1 ?v2 ...) constraint ... :order-by ... :limit N)
   Returns (values pattern constraints options) where:
   - pattern is an atom (the query pattern)
   - constraints is a list of comparisons
   - options is a plist (:order-by ... :limit ...)"
  (let* ((body (rest form))          ; skip 'query
         (program-name (to-keyword (first body)))
         (rest-body (rest body))
         (pattern nil)
         (constraints nil)
         (options nil))
    (declare (ignore program-name))
    ;; First non-keyword-option form is the query pattern
    (setf pattern (parse-atom (first rest-body)))
    ;; Remaining forms: constraints and keyword options
    (loop for remaining = (rest rest-body) then (rest remaining)
          while remaining
          for item = (first remaining)
          do (cond
               ;; Keyword options
               ((keywordp item)
                (setf options (append options (list item (second remaining))))
                (setf remaining (rest remaining)))
               ;; Comparison constraint
               ((and (listp item) (comparison-op-p (first item)))
                (push (parse-comparison item) constraints))
               ;; Atom constraint
               ((listp item)
                (push (parse-body-literal item) constraints))))
    (values pattern (nreverse constraints) options)))
