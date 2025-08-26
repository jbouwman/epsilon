;;;; Simplification rules using pattern matching
;;;;
;;;; This module defines algebraic simplification rules using
;;;; the pattern matching system for more powerful transformations.

(defpackage epsilon.compute.simplify-rules
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (pat epsilon.compute.pattern)
   (map epsilon.map))
  (:export
   ;; Rule management
   :simplification-rule
   :make-simplification-rule
   :simplification-rule-name
   :simplification-rule-pattern
   :simplification-rule-replacement
   :simplification-rule-condition
   :simplification-rule-priority
   
   ;; Rule registry
   :*rule-registry*
   :register-rule
   :unregister-rule
   :get-rule
   :list-rules
   :clear-rules
   :init-standard-rules
   
   ;; Rule application
   :apply-rule
   :apply-rules
   :apply-rules-exhaustive
   
   ;; Simplification strategies
   :simplify-with-rules
   :simplify-bottom-up
   :simplify-top-down
   :simplify-iterative))

(in-package epsilon.compute.simplify-rules)

;;; Rule representation

(defstruct simplification-rule
  "A simplification rule with pattern matching"
  name                    ; Symbol naming the rule
  pattern                 ; Pattern to match
  replacement            ; Replacement pattern or function
  condition              ; Optional guard condition
  priority)              ; Priority (higher = apply first)

;;; Rule registry

(defparameter *rule-registry* map:+empty+
  "Registry of simplification rules by name")

(defparameter *rules-by-priority* '()
  "Rules sorted by priority")

(defun register-rule (name pattern replacement &key condition (priority 0))
  "Register a new simplification rule"
  (let ((rule (make-simplification-rule
               :name name
               :pattern pattern
               :replacement replacement
               :condition condition
               :priority priority)))
    (setf *rule-registry* (map:assoc *rule-registry* name rule))
    (setf *rules-by-priority*
          (sort (cons rule (remove name *rules-by-priority* 
                                  :key #'simplification-rule-name))
                #'> :key #'simplification-rule-priority))
    rule))

(defun unregister-rule (name)
  "Remove a rule from the registry"
  (setf *rule-registry* (map:dissoc *rule-registry* name))
  (setf *rules-by-priority* 
        (remove name *rules-by-priority* :key #'simplification-rule-name)))

(defun get-rule (name)
  "Get a rule by name"
  (map:get *rule-registry* name))

(defun list-rules ()
  "List all registered rules"
  *rules-by-priority*)

(defun clear-rules ()
  "Clear all rules"
  (setf *rule-registry* map:+empty+)
  (setf *rules-by-priority* '()))

;;; Rule application

(defun apply-rule (rule expr)
  "Apply a single rule to an expression"
  (let ((bindings (pat:match (simplification-rule-pattern rule) expr)))
    (when bindings
      ;; Check condition if present
      (when (simplification-rule-condition rule)
        (unless (funcall (simplification-rule-condition rule) bindings expr)
          (return-from apply-rule nil)))
      
      ;; Apply replacement
      (let ((replacement (simplification-rule-replacement rule)))
        (if (functionp replacement)
            (funcall replacement bindings expr)
            (pat:substitute-bindings replacement bindings))))))

(defun apply-rules (expr &optional (rules *rules-by-priority*))
  "Apply rules to expression, returning first match"
  (dolist (rule rules)
    (let ((result (apply-rule rule expr)))
      (when result
        (return-from apply-rules result))))
  nil)

(defun apply-rules-exhaustive (expr &optional (max-iterations 100))
  "Apply rules repeatedly until fixed point"
  (loop for i from 0 below max-iterations
        for new-expr = (or (apply-rules expr) expr)
        when (sym:expr-equal-p new-expr expr)
          return expr
        do (setf expr new-expr)
        finally (return expr)))

;;; Simplification strategies

(defun simplify-with-rules (expr)
  "Main simplification entry point"
  (simplify-bottom-up expr))

(defun simplify-bottom-up (expr)
  "Bottom-up simplification strategy"
  (cond
    ;; Atoms are already simple
    ((or (sym:var-p expr) 
         (sym:const-p expr)
         (numberp expr))
     expr)
    
    ;; Simplify subexpressions first
    ((sym:expr-p expr)
     (let* ((op (sym:expr-op expr))
            (args (sym:expr-args expr))
            (simplified-args (mapcar #'simplify-bottom-up args))
            (simplified-expr (sym:symbolic op simplified-args)))
       ;; Then apply rules to the result
       (or (apply-rules simplified-expr)
           simplified-expr)))
    
    ;; Unknown type
    (t expr)))

(defun simplify-top-down (expr)
  "Top-down simplification strategy"
  (let ((simplified (or (apply-rules expr) expr)))
    (cond
      ;; Atoms stay the same
      ((or (sym:var-p simplified) 
           (sym:const-p simplified)
           (numberp simplified))
       simplified)
      
      ;; Recurse into subexpressions
      ((sym:expr-p simplified)
       (sym:symbolic (sym:expr-op simplified)
                    (mapcar #'simplify-top-down 
                           (sym:expr-args simplified))))
      
      ;; Unknown type
      (t simplified))))

(defun simplify-iterative (expr &optional (max-iterations 10))
  "Iterative simplification with cycle detection"
  (let ((seen (make-hash-table :test 'equal))
        (current expr))
    (loop for i from 0 below max-iterations
          do (setf (gethash current seen) t)
             (setf current (simplify-bottom-up current))
          when (gethash current seen)
            return current
          finally (return current))))

;;; Standard algebraic rules

(defun init-standard-rules ()
  "Initialize standard algebraic simplification rules"
  (clear-rules)
  
  ;; Identity rules (priority 100)
  (register-rule 'add-zero-right
                '(+ x 0)
                'x
                :priority 100)
  
  (register-rule 'add-zero-left
                '(+ 0 x)
                'x
                :priority 100)
  
  (register-rule 'mul-one-right
                '(* x 1)
                'x
                :priority 100)
  
  (register-rule 'mul-one-left
                '(* 1 x)
                'x
                :priority 100)
  
  (register-rule 'mul-zero-right
                '(* x 0)
                0
                :priority 100)
  
  (register-rule 'mul-zero-left
                '(* 0 x)
                0
                :priority 100)
  
  (register-rule 'div-one
                '(/ x 1)
                'x
                :priority 100)
  
  (register-rule 'sub-self
                '(- x x)
                0
                :priority 100)
  
  (register-rule 'div-self
                '(/ x x)
                1
                :condition (lambda (bindings expr)
                            (declare (ignore expr))
                            (let ((x (pat:lookup-binding 'x bindings)))
                              (not (and (numberp x) (zerop x)))))
                :priority 100)
  
  ;; Power rules (priority 90)
  (register-rule 'pow-zero
                '(^ x 0)
                1
                :condition (lambda (bindings expr)
                            (declare (ignore expr))
                            (let ((x (pat:lookup-binding 'x bindings)))
                              (not (and (numberp x) (zerop x)))))
                :priority 90)
  
  (register-rule 'pow-one
                '(^ x 1)
                'x
                :priority 90)
  
  (register-rule 'pow-two
                '(^ x 2)
                '(* x x)
                :priority 80)
  
  ;; Negation rules (priority 85)
  (register-rule 'neg-neg
                '(- (- x))
                'x
                :priority 85)
  
  (register-rule 'sub-as-add-neg
                '(- x y)
                '(+ x (- y))
                :priority 70)
  
  ;; Distribution rules (priority 60)
  (register-rule 'distribute-mul-add
                '(* a (+ b c))
                '(+ (* a b) (* a c))
                :condition (lambda (bindings expr)
                            ;; Only distribute if it simplifies
                            (let ((a (pat:lookup-binding 'a bindings)))
                              (or (numberp a)
                                  (and (sym:const-p a)
                                       (numberp (sym:const-value a))))))
                :priority 60)
  
  ;; Collection rules (priority 50)
  (register-rule 'collect-like-terms
                '(+ (* a x) (* b x))
                (lambda (bindings expr)
                  (let ((a (pat:lookup-binding 'a bindings))
                        (b (pat:lookup-binding 'b bindings))
                        (x (pat:lookup-binding 'x bindings)))
                    (if (and (numberp a) (numberp b))
                        (sym:symbolic '* (+ a b) x)
                        nil)))
                :priority 50)
  
  ;; Constant folding (priority 40)
  (register-rule 'fold-add
                '(+ a b)
                (lambda (bindings expr)
                  (let ((a (pat:lookup-binding 'a bindings))
                        (b (pat:lookup-binding 'b bindings)))
                    (when (and (numberp a) (numberp b))
                      (+ a b))))
                :priority 40)
  
  (register-rule 'fold-mul
                '(* a b)
                (lambda (bindings expr)
                  (let ((a (pat:lookup-binding 'a bindings))
                        (b (pat:lookup-binding 'b bindings)))
                    (when (and (numberp a) (numberp b))
                      (* a b))))
                :priority 40)
  
  (register-rule 'fold-sub
                '(- a b)
                (lambda (bindings expr)
                  (let ((a (pat:lookup-binding 'a bindings))
                        (b (pat:lookup-binding 'b bindings)))
                    (when (and (numberp a) (numberp b))
                      (- a b))))
                :priority 40)
  
  (register-rule 'fold-div
                '(/ a b)
                (lambda (bindings expr)
                  (let ((a (pat:lookup-binding 'a bindings))
                        (b (pat:lookup-binding 'b bindings)))
                    (when (and (numberp a) (numberp b) (not (zerop b)))
                      (/ a b))))
                :priority 40)
  
  ;; Logarithm rules (priority 30)
  (register-rule 'log-exp
                '(log (exp x))
                'x
                :priority 30)
  
  (register-rule 'exp-log
                '(exp (log x))
                'x
                :condition (lambda (bindings expr)
                            (declare (ignore expr))
                            (let ((x (pat:lookup-binding 'x bindings)))
                              (or (not (numberp x))
                                  (plusp x))))
                :priority 30)
  
  ;; Trigonometric identities (priority 20)
  (register-rule 'sin-squared-plus-cos-squared
                '(+ (^ (sin x) 2) (^ (cos x) 2))
                1
                :priority 20)
  
  (register-rule 'tan-as-sin-cos
                '(tan x)
                '(/ (sin x) (cos x))
                :priority 10)
  
  ;; More trigonometric identities
  (register-rule 'sin-zero
                '(sin 0)
                0
                :priority 25)
  
  (register-rule 'cos-zero
                '(cos 0)
                1
                :priority 25)
  
  (register-rule 'sin-pi-half
                '(sin (/ pi 2))
                1
                :priority 25)
  
  (register-rule 'cos-pi-half
                '(cos (/ pi 2))
                0
                :priority 25)
  
  (register-rule 'sin-neg
                '(sin (- x))
                '(- (sin x))
                :priority 20)
  
  (register-rule 'cos-neg
                '(cos (- x))
                '(cos x)
                :priority 20)
  
  (register-rule 'tan-neg
                '(tan (- x))
                '(- (tan x))
                :priority 20)
  
  ;; Double angle formulas
  (register-rule 'sin-double-angle
                '(sin (* 2 x))
                '(* 2 (sin x) (cos x))
                :priority 15)
  
  (register-rule 'cos-double-angle
                '(cos (* 2 x))
                '(- (^ (cos x) 2) (^ (sin x) 2))
                :priority 15)
  
  ;; More logarithm rules
  (register-rule 'log-one
                '(log 1)
                0
                :priority 35)
  
  (register-rule 'log-e
                '(log e)
                1
                :priority 35)
  
  (register-rule 'exp-zero
                '(exp 0)
                1
                :priority 35)
  
  (register-rule 'exp-one
                '(exp 1)
                'e
                :priority 35)
  
  (register-rule 'log-sqrt
                '(log (sqrt x))
                '(* (/ 1 2) (log x))
                :condition (lambda (bindings expr)
                            (declare (ignore expr))
                            (let ((x (pat:lookup-binding 'x bindings)))
                              (or (not (numberp x))
                                  (plusp x))))
                :priority 25)
  
  (register-rule 'sqrt-squared
                '(sqrt (^ x 2))
                '(abs x)
                :priority 30)
  
  ;; Exponential rules
  (register-rule 'exp-add
                '(exp (+ x y))
                '(* (exp x) (exp y))
                :priority 25)
  
  (register-rule 'exp-sub
                '(exp (- x y))
                '(/ (exp x) (exp y))
                :priority 25)
  
  (register-rule 'exp-mul
                '(exp (* n x))
                '(^ (exp x) n)
                :condition (lambda (bindings expr)
                            (declare (ignore expr))
                            (let ((n (pat:lookup-binding 'n bindings)))
                              (or (numberp n)
                                  (and (sym:const-p n)
                                       (numberp (sym:const-value n))))))
                :priority 25)
  
  t) ; Return true to indicate success

;;; Initialize rules on load
(init-standard-rules)