;;;; Pattern matching system for algebraic simplification
;;;; 
;;;; This module provides a pattern matching engine that supports:
;;;;   - Variable binding with constraints
;;;;   - Associative-commutative matching
;;;;   - Sequence patterns
;;;;   - Guard conditions

(defpackage epsilon.compute.pattern
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (map epsilon.map))
  (:export
   ;; Pattern types
   :pattern
   :pattern-p
   :make-pattern
   :pattern-var
   :pattern-const
   :pattern-expr
   :pattern-sequence
   
   ;; Pattern variables
   :pattern-var-p
   :pattern-var-name
   :pattern-var-constraint
   
   ;; Matching
   :match
   :match-all
   :substitute-bindings
   
   ;; Utilities
   :compile-pattern
   :pattern-variables
   :bindings-p
   :empty-bindings
   :extend-bindings
   :lookup-binding))

(in-package epsilon.compute.pattern)

;;; Pattern representation

(defstruct pattern-var
  "A pattern variable that can match expressions"
  name                    ; Symbol naming the variable
  constraint)             ; Optional predicate function

(defstruct pattern
  "A pattern for matching expressions"
  type                    ; :var, :const, :expr, :sequence
  value                   ; The pattern content
  guard)                  ; Optional guard condition

;;; Pattern construction

(defun pattern-const (value)
  "Create a constant pattern"
  (make-pattern :type :const :value value))

(defun pattern-expr (op &rest args)
  "Create an expression pattern"
  (make-pattern :type :expr 
                :value (cons op args)))

(defun pattern-sequence (name &optional constraint)
  "Create a sequence pattern that matches zero or more expressions"
  (make-pattern :type :sequence
                :value (make-pattern-var :name name 
                                         :constraint constraint)))

;;; Bindings management

(defparameter *empty-bindings* map:+empty+
  "Empty binding environment")

(defun empty-bindings ()
  "Return empty bindings"
  *empty-bindings*)

(defun bindings-p (obj)
  "Check if object is a bindings map"
  (map:map-p obj))

(defun extend-bindings (var value bindings)
  "Add a new binding"
  (map:assoc bindings var value))

(defun lookup-binding (var bindings)
  "Look up a variable in bindings"
  (map:get bindings var))

;;; Pattern matching

(defun match (pattern expr &optional (bindings (empty-bindings)))
  "Match pattern against expression, returning bindings or nil"
  (cond
    ;; Pattern variable
    ((pattern-var-p pattern)
     (match-var pattern expr bindings))
    
    ;; Structured pattern
    ((pattern-p pattern)
     (case (pattern-type pattern)
       (:const (match-const pattern expr bindings))
       (:expr (match-expr pattern expr bindings))
       (:sequence (match-sequence pattern expr bindings))
       (:var (match-var (pattern-value pattern) expr bindings))))
    
    ;; Direct symbolic patterns
    ((symbolp pattern)
     (if (and (symbolp expr) (eq pattern expr))
         bindings
         nil))
    
    ;; Direct constant patterns
    ((numberp pattern)
     (if (and (numberp expr) (= pattern expr))
         bindings
         nil))
    
    ;; List patterns (expression patterns)
    ((listp pattern)
     (match-list pattern expr bindings))
    
    ;; Unknown pattern type
    (t nil)))

(defun match-var (var expr bindings)
  "Match a pattern variable"
  (let* ((name (if (pattern-var-p var)
                   (pattern-var-name var)
                   var))
         (constraint (when (pattern-var-p var)
                      (pattern-var-constraint var)))
         (existing (lookup-binding name bindings)))
    (cond
      ;; Already bound - check consistency
      (existing
       (if (equal existing expr)
           bindings
           nil))
      
      ;; Check constraint if present
      (constraint
       (if (funcall constraint expr)
           (extend-bindings name expr bindings)
           nil))
      
      ;; No constraint - bind freely
      (t (extend-bindings name expr bindings)))))

(defun match-const (pattern expr bindings)
  "Match a constant pattern"
  (let ((value (pattern-value pattern)))
    (cond
      ;; Numeric constant
      ((numberp value)
       (if (and (numberp expr) (= value expr))
           bindings
           (when (sym:const-p expr)
             (if (= value (sym:const-value expr))
                 bindings
                 nil))))
      
      ;; Symbolic constant
      ((sym:const-p value)
       (if (and (sym:const-p expr)
                (equal (sym:const-value value)
                       (sym:const-value expr)))
           bindings
           nil))
      
      ;; Other constants
      (t (if (equal value expr)
             bindings
             nil)))))

(defun match-expr (pattern expr bindings)
  "Match an expression pattern"
  (unless (sym:expr-p expr)
    (return-from match-expr nil))
  
  (let* ((pattern-value (pattern-value pattern))
         (op (car pattern-value))
         (args (cdr pattern-value))
         (expr-op (sym:expr-op expr))
         (expr-args (sym:expr-args expr)))
    
    ;; Check operation matches
    (unless (or (eq op expr-op)
                (and (symbolp op) 
                     (string= (symbol-name op) 
                             (symbol-name expr-op))))
      (return-from match-expr nil))
    
    ;; Handle associative-commutative matching for + and *
    (if (member op '(+ *))
        (match-ac op args expr-args bindings)
        (match-args args expr-args bindings))))

(defun match-list (pattern expr bindings)
  "Match a list pattern as an expression"
  (when (sym:expr-p expr)
    (let ((op (car pattern))
          (args (cdr pattern)))
      (when (eq op (sym:expr-op expr))
        (match-args args (sym:expr-args expr) bindings)))))

(defun match-args (pattern-args expr-args bindings)
  "Match argument lists"
  (cond
    ;; Both empty - success
    ((and (null pattern-args) (null expr-args))
     bindings)
    
    ;; Length mismatch
    ((or (null pattern-args) (null expr-args))
     nil)
    
    ;; Check for sequence pattern
    ((and (pattern-p (car pattern-args))
          (eq (pattern-type (car pattern-args)) :sequence))
     (match-sequence-in-args pattern-args expr-args bindings))
    
    ;; Match first and recurse
    (t
     (let ((new-bindings (match (car pattern-args) 
                                (car expr-args) 
                                bindings)))
       (when new-bindings
         (match-args (cdr pattern-args) 
                    (cdr expr-args) 
                    new-bindings))))))

(defun match-sequence (pattern exprs bindings)
  "Match a sequence pattern against multiple expressions"
  (let* ((var (pattern-value pattern))
         (name (pattern-var-name var))
         (constraint (pattern-var-constraint var)))
    ;; For now, bind the sequence as a list
    (if (or (null constraint)
            (every constraint exprs))
        (extend-bindings name exprs bindings)
        nil)))

(defun match-sequence-in-args (pattern-args expr-args bindings)
  "Handle sequence pattern within argument list"
  ;; Simple implementation - sequence must be at end
  (when (null (cdr pattern-args))
    (match-sequence (car pattern-args) expr-args bindings)))

(defun match-ac (op pattern-args expr-args bindings)
  "Associative-commutative matching"
  ;; Simplified AC matching - try all permutations
  ;; For production, use more efficient algorithm
  (cond
    ;; Same length - try direct match and permutations
    ((= (length pattern-args) (length expr-args))
     (or (match-args pattern-args expr-args bindings)
         (try-permutations op pattern-args expr-args bindings)))
    
    ;; Different lengths - need more sophisticated AC matching
    (t nil)))

(defun try-permutations (op pattern-args expr-args bindings)
  "Try matching with permuted arguments (for commutative ops)"
  ;; Simple implementation for 2-arg case
  (when (= (length pattern-args) 2)
    (match-args pattern-args 
                (reverse expr-args) 
                bindings)))

;;; Substitution

(defun substitute-bindings (pattern bindings)
  "Substitute bindings into a pattern to create expression"
  (cond
    ;; Pattern variable - look up binding
    ((pattern-var-p pattern)
     (or (lookup-binding (pattern-var-name pattern) bindings)
         pattern))
    
    ;; Structured pattern
    ((pattern-p pattern)
     (case (pattern-type pattern)
       (:const (pattern-value pattern))
       (:var (or (lookup-binding (pattern-var-name (pattern-value pattern)) 
                                bindings)
                pattern))
       (:expr 
        (let* ((value (pattern-value pattern))
               (op (car value))
               (args (cdr value)))
          (sym:symbolic op 
                       (mapcar (lambda (arg)
                                (substitute-bindings arg bindings))
                              args))))
       (:sequence 
        (lookup-binding (pattern-var-name (pattern-value pattern)) 
                       bindings))))
    
    ;; Symbol - might be a variable name
    ((symbolp pattern)
     (or (lookup-binding pattern bindings)
         pattern))
    
    ;; List - expression pattern
    ((listp pattern)
     (sym:symbolic (car pattern)
                  (mapcar (lambda (arg)
                           (substitute-bindings arg bindings))
                         (cdr pattern))))
    
    ;; Constants pass through
    (t pattern)))

;;; Pattern compilation (optimization)

(defun compile-pattern (pattern)
  "Compile pattern for efficient matching"
  ;; For now, just return the pattern
  ;; Future: compile to matching function
  pattern)

(defun pattern-variables (pattern)
  "Extract all variables from a pattern"
  (let ((vars '()))
    (labels ((extract (p)
               (cond
                 ((pattern-var-p p)
                  (pushnew (pattern-var-name p) vars))
                 ((pattern-p p)
                  (case (pattern-type p)
                    (:var (extract (pattern-value p)))
                    (:expr (mapc #'extract (cdr (pattern-value p))))
                    (:sequence (extract (pattern-value p)))))
                 ((listp p)
                  (mapc #'extract (cdr p))))))
      (extract pattern))
    vars))

;;; Convenience constructors

(defun var (name &optional constraint)
  "Create a pattern variable"
  (make-pattern-var :name name :constraint constraint))

(defun is-number-p (x)
  "Constraint: must be a number"
  (or (numberp x)
      (and (sym:const-p x)
           (numberp (sym:const-value x)))))

(defun is-positive-p (x) 
  "Constraint: must be positive"
  (or (and (numberp x) (plusp x))
      (and (sym:const-p x)
           (numberp (sym:const-value x))
           (plusp (sym:const-value x)))))