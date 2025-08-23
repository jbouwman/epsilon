(defpackage epsilon.compute
  (:use :cl)
  (:local-nicknames
   (types epsilon.compute.types)
   (sym epsilon.compute.symbolic)
   (simp epsilon.compute.simplify))
  (:export
   ;; Main API
   :compute
   :expr
   :var
   :const
   :simplify
   :evaluate
   :substitute
   
   ;; Mathematical operations (override CL symbols)
   :+
   :-
   :*
   :/
   :^
   :sin
   :cos
   :tan
   :exp
   :log
   :sqrt
   :abs
   
   ;; Matrix operations
   :transpose
   :inverse
   :det
   :trace
   :dot
   :cross
   
   ;; Calculus
   :diff
   :grad
   :integrate
   
   ;; Convenience functions
   :x
   :y
   :z
   :t))

(in-package epsilon.compute)

;;; Shadow Common Lisp arithmetic to work with symbolic expressions

(defun ensure-expr (x)
  "Convert value to expression if needed"
  (cond
    ((or (sym:expr-p x) (sym:var-p x) (sym:const-p x)) x)
    ((numberp x) (sym:lit x))
    ((symbolp x) (sym:sym x))
    (t x)))

(defun + (&rest args)
  "Symbolic addition"
  (if (every #'numberp args)
      (apply #'cl:+ args)
      (sym:symbolic '+ (mapcar #'ensure-expr args))))

(defun - (&rest args)
  "Symbolic subtraction"
  (if (every #'numberp args)
      (apply #'cl:- args)
      (case (length args)
        (1 (sym:symbolic '- (ensure-expr (first args))))
        (t (sym:symbolic '- (mapcar #'ensure-expr args))))))

(defun * (&rest args)
  "Symbolic multiplication"
  (if (every #'numberp args)
      (apply #'cl:* args)
      (sym:symbolic '* (mapcar #'ensure-expr args))))

(defun / (&rest args)
  "Symbolic division"
  (if (every #'numberp args)
      (apply #'cl:/ args)
      (sym:symbolic '/ (mapcar #'ensure-expr args))))

(defun ^ (base exponent)
  "Symbolic exponentiation"
  (if (and (numberp base) (numberp exponent))
      (cl:expt base exponent)
      (sym:symbolic '^ (ensure-expr base) (ensure-expr exponent))))

;;; Transcendental functions

(defun sin (x)
  "Symbolic sine"
  (if (numberp x)
      (cl:sin x)
      (sym:symbolic 'sin (ensure-expr x))))

(defun cos (x)
  "Symbolic cosine"
  (if (numberp x)
      (cl:cos x)
      (sym:symbolic 'cos (ensure-expr x))))

(defun tan (x)
  "Symbolic tangent"
  (if (numberp x)
      (cl:tan x)
      (sym:symbolic 'tan (ensure-expr x))))

(defun exp (x)
  "Symbolic exponential"
  (if (numberp x)
      (cl:exp x)
      (sym:symbolic 'exp (ensure-expr x))))

(defun log (x &optional base)
  "Symbolic logarithm"
  (cond
    ((and (numberp x) (null base))
     (cl:log x))
    ((and (numberp x) (numberp base))
     (cl:log x base))
    (base
     (/ (sym:symbolic 'log (ensure-expr x))
        (sym:symbolic 'log (ensure-expr base))))
    (t
     (sym:symbolic 'log (ensure-expr x)))))

(defun sqrt (x)
  "Symbolic square root"
  (if (numberp x)
      (cl:sqrt x)
      (sym:symbolic 'sqrt (ensure-expr x))))

(defun abs (x)
  "Symbolic absolute value"
  (if (numberp x)
      (cl:abs x)
      (sym:symbolic 'abs (ensure-expr x))))

;;; Matrix operations

(defun transpose (matrix)
  "Symbolic matrix transpose"
  (sym:symbolic 'transpose (ensure-expr matrix)))

(defun inverse (matrix)
  "Symbolic matrix inverse"
  (sym:symbolic 'inverse (ensure-expr matrix)))

(defun det (matrix)
  "Symbolic matrix determinant"
  (sym:symbolic 'det (ensure-expr matrix)))

(defun trace (matrix)
  "Symbolic matrix trace"
  (sym:symbolic 'trace (ensure-expr matrix)))

(defun dot (v1 v2)
  "Symbolic dot product"
  (sym:symbolic 'dot (ensure-expr v1) (ensure-expr v2)))

(defun cross (v1 v2)
  "Symbolic cross product"
  (sym:symbolic 'cross (ensure-expr v1) (ensure-expr v2)))

;;; Calculus

(defun diff (expr var)
  "Symbolic differentiation"
  (let ((expr (ensure-expr expr))
        (var (if (symbolp var) (sym:sym var) var)))
    (differentiate expr var)))

(defun differentiate (expr var)
  "Core differentiation algorithm"
  (cond
    ;; d/dx(x) = 1
    ((sym:var-equal-p expr var)
     (sym:lit 1))
    
    ;; d/dx(c) = 0 for constant c
    ((or (sym:const-p expr)
         (and (sym:var-p expr) (not (sym:var-equal-p expr var))))
     (sym:lit 0))
    
    ;; Apply differentiation rules
    ((sym:expr-p expr)
     (case (sym:expr-op expr)
       ;; Sum rule: d/dx(f + g) = df/dx + dg/dx
       (+
        (sym:symbolic '+ 
                     (mapcar (lambda (arg) (differentiate arg var))
                            (sym:expr-args expr))))
       
       ;; Product rule: d/dx(f * g) = f * dg/dx + df/dx * g
       (*
        (if (= (length (sym:expr-args expr)) 2)
            (let ((f (first (sym:expr-args expr)))
                  (g (second (sym:expr-args expr))))
              (sym:symbolic '+
                           (sym:symbolic '* f (differentiate g var))
                           (sym:symbolic '* (differentiate f var) g)))
            ;; For multiple factors, use generalized product rule
            (sym:symbolic '+
                         (loop for i from 0 below (length (sym:expr-args expr))
                               collect (sym:symbolic '*
                                                    (append
                                                     (subseq (sym:expr-args expr) 0 i)
                                                     (list (differentiate (nth i (sym:expr-args expr)) var))
                                                     (subseq (sym:expr-args expr) (1+ i))))))))
       
       ;; Quotient rule: d/dx(f/g) = (df/dx * g - f * dg/dx) / g^2
       (/
        (let ((f (first (sym:expr-args expr)))
              (g (second (sym:expr-args expr))))
          (sym:symbolic '/
                       (sym:symbolic '-
                                    (sym:symbolic '* (differentiate f var) g)
                                    (sym:symbolic '* f (differentiate g var)))
                       (sym:symbolic '^ g (sym:lit 2)))))
       
       ;; Power rule: d/dx(f^n) = n * f^(n-1) * df/dx
       (^
        (let ((base (first (sym:expr-args expr)))
              (exp (second (sym:expr-args expr))))
          (cond
            ;; Constant exponent
            ((sym:const-p exp)
             (sym:symbolic '*
                          exp
                          (sym:symbolic '^ base 
                                       (sym:lit (cl:- (sym:const-value exp) 1)))
                          (differentiate base var)))
            ;; General case: d/dx(f^g) = f^g * (g*df/dx/f + dg/dx*ln(f))
            (t
             (sym:symbolic '*
                          expr
                          (sym:symbolic '+
                                       (sym:symbolic '* 
                                                    (sym:symbolic '/ (differentiate base var) base)
                                                    exp)
                                       (sym:symbolic '*
                                                    (differentiate exp var)
                                                    (sym:symbolic 'log base))))))))
       
       ;; Trig functions
       (sin
        (sym:symbolic '* 
                     (sym:symbolic 'cos (first (sym:expr-args expr)))
                     (differentiate (first (sym:expr-args expr)) var)))
       
       (cos
        (sym:symbolic '* 
                     (sym:symbolic '- (sym:symbolic 'sin (first (sym:expr-args expr))))
                     (differentiate (first (sym:expr-args expr)) var)))
       
       (tan
        (sym:symbolic '*
                     (sym:symbolic '^ (sym:symbolic 'cos (first (sym:expr-args expr))) (sym:lit -2))
                     (differentiate (first (sym:expr-args expr)) var)))
       
       ;; Exponential and logarithm
       (exp
        (sym:symbolic '*
                     expr
                     (differentiate (first (sym:expr-args expr)) var)))
       
       (log
        (sym:symbolic '/
                     (differentiate (first (sym:expr-args expr)) var)
                     (first (sym:expr-args expr))))
       
       ;; Square root
       (sqrt
        (sym:symbolic '/
                     (differentiate (first (sym:expr-args expr)) var)
                     (sym:symbolic '* (sym:lit 2) expr)))
       
       (otherwise
        (sym:symbolic 'diff expr var))))
    
    (t (sym:lit 0))))

(defun grad (expr &rest vars)
  "Symbolic gradient"
  (if (null vars)
      (let ((free (sym:free-vars expr)))
        (mapcar (lambda (v) (diff expr v)) free))
      (mapcar (lambda (v) (diff expr v)) vars)))

(defun integrate (expr var &optional lower upper)
  "Symbolic integration (basic)"
  (let ((anti (antiderivative (ensure-expr expr) 
                              (if (symbolp var) (sym:sym var) var))))
    (if (and lower upper)
        (sym:symbolic '-
                     (substitute anti (list (cons (sym:var-name var) upper)))
                     (substitute anti (list (cons (sym:var-name var) lower))))
        anti)))

(defun antiderivative (expr var)
  "Find antiderivative (basic cases)"
  (cond
    ;; ∫x dx = x²/2
    ((sym:var-equal-p expr var)
     (sym:symbolic '/ (sym:symbolic '^ var (sym:lit 2)) (sym:lit 2)))
    
    ;; ∫c dx = cx
    ((sym:const-p expr)
     (sym:symbolic '* expr var))
    
    ;; ∫(f + g) dx = ∫f dx + ∫g dx
    ((and (sym:expr-p expr) (eq (sym:expr-op expr) '+))
     (sym:symbolic '+ (mapcar (lambda (arg) (antiderivative arg var))
                             (sym:expr-args expr))))
    
    ;; Default: return integral symbol
    (t (sym:symbolic 'integrate expr var))))

;;; Convenience variables

(defparameter x (sym:sym 'x) "Symbolic variable x")
(defparameter y (sym:sym 'y) "Symbolic variable y")
(defparameter z (sym:sym 'z) "Symbolic variable z")
(defparameter t (sym:sym 't) "Symbolic variable t")

;;; Main API functions

(defun expr (form)
  "Create an expression from a form"
  (ensure-expr form))

(defun var (name &optional type)
  "Create a symbolic variable"
  (sym:sym name type))

(defun const (value &optional type)
  "Create a constant"
  (sym:lit value type))

(defun simplify (expr)
  "Simplify an expression"
  (simp:simplify (ensure-expr expr)))

(defun substitute (expr bindings)
  "Substitute variables in an expression"
  (sym:substitute (ensure-expr expr) 
                  (mapcar (lambda (binding)
                           (cons (if (symbolp (car binding))
                                    (car binding)
                                    (sym:var-name (car binding)))
                                 (ensure-expr (cdr binding))))
                         bindings)))

(defun evaluate (expr &optional bindings)
  "Evaluate an expression numerically"
  (let ((substituted (if bindings
                        (substitute expr bindings)
                        expr)))
    (eval-expr substituted)))

(defun eval-expr (expr)
  "Evaluate an expression to a numeric value"
  (cond
    ((sym:const-p expr) (sym:const-value expr))
    ((sym:var-p expr) (error "Cannot evaluate variable ~A" (sym:var-name expr)))
    ((sym:expr-p expr)
     (let ((op (sym:expr-op expr))
           (args (mapcar #'eval-expr (sym:expr-args expr))))
       (apply (simp:get-numeric-op op) args)))
    ((numberp expr) expr)
    (t (error "Cannot evaluate ~A" expr))))