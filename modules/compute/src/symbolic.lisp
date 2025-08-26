(defpackage epsilon.compute.symbolic
  (:use :cl)
  (:local-nicknames
   (types epsilon.compute.types))
  (:export
   ;; Expression structure
   :expr
   :expr-p
   :make-expr
   :expr-op
   :expr-args
   :expr-type
   :expr-metadata
   
   ;; Variables
   :var
   :var-p
   :make-var
   :var-name
   :var-type
   :var-metadata
   
   ;; Constants
   :const
   :const-p
   :make-const
   :const-value
   :const-type
   
   ;; Expression construction
   :symbolic
   :sym
   :lit
   
   ;; Expression analysis
   :free-vars
   :bound-vars
   :depends-on-p
   :constant-p
   :linear-p
   :polynomial-p
   
   ;; Expression manipulation
   :subst-vars
   :replace-var
   :rename-vars
   :canonicalize
   
   ;; Pattern matching
   :match-expr
   :unify
   :apply-substitution
   
   ;; Equality
   :expr-equal-p
   :var-equal-p))

(in-package epsilon.compute.symbolic)

;;; Expression representation

(defstruct expr
  "Symbolic expression tree"
  (op nil :type symbol)
  (args nil :type list)
  (type nil :type (or null types:compute-type))
  (metadata nil :type list))

(defstruct var
  "Symbolic variable"
  (name nil :type symbol)
  (type nil :type (or null types:compute-type))
  (metadata nil :type list))

(defstruct const
  "Constant value"
  (value nil)
  (type nil :type (or null types:compute-type)))

;;; Smart constructors

(defun symbolic (op &rest args)
  "Create a symbolic expression"
  (let* ((arg-types (mapcar #'get-expr-type args))
         (result-type (apply #'types:infer-type op arg-types)))
    (make-expr :op op :args args :type result-type)))

(defun sym (name &optional type metadata)
  "Create a symbolic variable with optional type and metadata"
  (make-var :name name 
            :type (or type (types:symbolic-type))
            :metadata metadata))

(defun lit (value &optional type)
  "Create a literal constant"
  (make-const :value value 
              :type (or type (infer-literal-type value))))

(defun infer-literal-type (value)
  "Infer type from a literal value"
  (cond
    ((numberp value) (types:scalar-type))
    ((vectorp value) (types:vector-type (length value)))
    ((and (arrayp value) (= (array-rank value) 2))
     (types:matrix-type (array-dimension value 0) (array-dimension value 1)))
    ((arrayp value) (types:tensor-type (array-dimensions value)))
    (t (types:symbolic-type))))

(defun get-expr-type (expr)
  "Get the type of an expression"
  (cond
    ((expr-p expr) (expr-type expr))
    ((var-p expr) (var-type expr))
    ((const-p expr) (const-type expr))
    (t (types:symbolic-type))))

;;; Expression analysis

(defun free-vars (expr)
  "Get all free variables in an expression"
  (cond
    ((var-p expr) (list expr))
    ((const-p expr) nil)
    ((expr-p expr)
     (remove-duplicates 
      (reduce #'append (mapcar #'free-vars (expr-args expr)))
      :test #'var-equal-p))
    (t nil)))

(defun var-equal-p (v1 v2)
  "Check if two variables are equal"
  (and (var-p v1) (var-p v2)
       (eq (var-name v1) (var-name v2))))

(defun depends-on-p (expr var)
  "Check if expression depends on a variable"
  (member var (free-vars expr) :test #'var-equal-p))

(defun constant-p (expr)
  "Check if expression is constant"
  (null (free-vars expr)))

(defun linear-p (expr var)
  "Check if expression is linear in a variable"
  (cond
    ((var-equal-p expr var) t)
    ((not (depends-on-p expr var)) t)
    ((expr-p expr)
     (case (expr-op expr)
       ((+ -)
        (every (lambda (arg) (linear-p arg var)) (expr-args expr)))
       ((*) 
        (let ((dependent-args (remove-if-not 
                              (lambda (arg) (depends-on-p arg var))
                              (expr-args expr))))
          (and (<= (length dependent-args) 1)
               (every (lambda (arg) (linear-p arg var)) dependent-args))))
       (otherwise nil)))
    (t nil)))

;;; Expression manipulation

(defun subst-vars (expr bindings)
  "Substitute variables with values in an expression"
  (cond
    ((var-p expr)
     (or (cdr (assoc (var-name expr) bindings :test #'eq))
         expr))
    ((const-p expr) expr)
    ((expr-p expr)
     (symbolic (expr-op expr)
               (mapcar (lambda (arg) (subst-vars arg bindings))
                      (expr-args expr))))
    (t expr)))

(defun replace-var (expr old-var new-var)
  "Replace all occurrences of a variable"
  (subst-vars expr (list (cons (var-name old-var) new-var))))

(defun rename-vars (expr &optional (prefix "x"))
  "Rename all variables with a systematic naming scheme"
  (let* ((vars (free-vars expr))
         (new-names (loop for i from 1 to (length vars)
                         collect (intern (format nil "~A~D" prefix i))))
         (bindings (mapcar (lambda (var name)
                            (cons (var-name var) (sym name (var-type var))))
                          vars new-names)))
    (subst-vars expr bindings)))

(defun canonicalize (expr)
  "Convert expression to canonical form"
  (cond
    ((or (var-p expr) (const-p expr)) expr)
    ((expr-p expr)
     (let ((op (expr-op expr))
           (args (mapcar #'canonicalize (expr-args expr))))
       (case op
         ((+)
          ;; Sort commutative operations
          (symbolic '+ (sort args #'expr-less-p)))
         ((*)
          (symbolic '* (sort args #'expr-less-p)))
         (otherwise
          (symbolic op args)))))
    (t expr)))

(defun expr-less-p (e1 e2)
  "Ordering for canonical form"
  (cond
    ((and (var-p e1) (var-p e2))
     (string< (symbol-name (var-name e1))
              (symbol-name (var-name e2))))
    ((var-p e1) t)
    ((var-p e2) nil)
    ((and (const-p e1) (const-p e2))
     (< (const-value e1) (const-value e2)))
    ((const-p e1) t)
    ((const-p e2) nil)
    (t nil)))

;;; Pattern matching

(defun match-expr (pattern expr &optional bindings)
  "Match an expression against a pattern"
  (cond
    ;; Pattern variable matches anything
    ((and (var-p pattern) (not (assoc (var-name pattern) bindings)))
     (cons (cons (var-name pattern) expr) bindings))
    
    ;; Already bound variable must match
    ((var-p pattern)
     (let ((binding (cdr (assoc (var-name pattern) bindings))))
       (when (expr-equal-p binding expr)
         bindings)))
    
    ;; Constants must match exactly
    ((and (const-p pattern) (const-p expr))
     (when (equal (const-value pattern) (const-value expr))
       bindings))
    
    ;; Expressions must have same operator and match all args
    ((and (expr-p pattern) (expr-p expr)
          (eq (expr-op pattern) (expr-op expr))
          (= (length (expr-args pattern)) (length (expr-args expr))))
     (loop for p-arg in (expr-args pattern)
           for e-arg in (expr-args expr)
           for new-bindings = (match-expr p-arg e-arg bindings)
           when (null new-bindings) return nil
           do (setf bindings new-bindings)
           finally (return bindings)))
    
    (t nil)))

(defun expr-equal-p (e1 e2)
  "Check if two expressions are structurally equal"
  (cond
    ((and (var-p e1) (var-p e2))
     (var-equal-p e1 e2))
    ((and (const-p e1) (const-p e2))
     (equal (const-value e1) (const-value e2)))
    ((and (expr-p e1) (expr-p e2))
     (and (eq (expr-op e1) (expr-op e2))
          (= (length (expr-args e1)) (length (expr-args e2)))
          (every #'expr-equal-p (expr-args e1) (expr-args e2))))
    (t nil)))

(defun apply-substitution (template bindings)
  "Apply bindings from pattern matching to a template"
  (subst-vars template bindings))