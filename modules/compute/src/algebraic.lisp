;;;; Advanced algebraic manipulation
;;;;
;;;; This module provides sophisticated algebraic operations:
;;;;   - Term collection and normalization
;;;;   - Polynomial manipulation
;;;;   - Factorization
;;;;   - Expansion

(defpackage epsilon.compute.algebraic
  (:use :cl)
  (:local-nicknames
   (sym epsilon.compute.symbolic)
   (map epsilon.map))
  (:export
   ;; Term manipulation
   :collect-terms
   :combine-like-terms
   :extract-coefficient
   :extract-variables
   :terms-similar-p
   
   ;; Polynomial operations
   :to-polynomial-form
   :from-polynomial-form
   :polynomial-degree
   :leading-coefficient
   :expand-product
   :factor-common
   
   ;; Expression normalization
   :normalize-expr
   :canonical-form
   :sort-terms
   :flatten-associative))

(in-package epsilon.compute.algebraic)

;;; Term representation and manipulation

(defstruct term
  "Represents a term as coefficient * variables"
  coefficient     ; Numeric coefficient
  variables)      ; List of (var . power) pairs

(defun extract-coefficient (expr)
  "Extract numeric coefficient from expression"
  (cond
    ((numberp expr) expr)
    ((sym:const-p expr) (sym:const-value expr))
    ((and (sym:expr-p expr)
          (eq (sym:expr-op expr) '*)
          (numberp (first (sym:expr-args expr))))
     (first (sym:expr-args expr)))
    ((and (sym:expr-p expr)
          (eq (sym:expr-op expr) '*))
     ;; Check if any arg is a number
     (let ((nums (remove-if-not #'numberp (sym:expr-args expr))))
       (if nums
           (apply #'* nums)
           1)))
    (t 1)))

(defun extract-variables (expr)
  "Extract variables and their powers from expression"
  (let ((vars (make-hash-table :test 'equal)))
    (labels ((extract (e power)
               (cond
                 ;; Variable
                 ((sym:var-p e)
                  (incf (gethash (sym:var-name e) vars 0) power))
                 
                 ;; Power expression
                 ((and (sym:expr-p e)
                       (eq (sym:expr-op e) '^))
                  (let ((base (first (sym:expr-args e)))
                        (exp (second (sym:expr-args e))))
                    (when (numberp exp)
                      (extract base (* power exp)))))
                 
                 ;; Product
                 ((and (sym:expr-p e)
                       (eq (sym:expr-op e) '*))
                  (dolist (arg (sym:expr-args e))
                    (unless (numberp arg)
                      (extract arg power))))
                 
                 ;; Other expressions treated as single units
                 ((sym:expr-p e)
                  (incf (gethash e vars 0) power)))))
      
      (extract expr 1))
    
    ;; Convert to sorted alist
    (let ((result '()))
      (maphash (lambda (var power)
                 (unless (zerop power)
                   (push (cons var power) result)))
               vars)
      (sort result (lambda (a b)
                    (string< (format nil "~A" (car a))
                            (format nil "~A" (car b))))))))

(defun expr-to-term (expr)
  "Convert expression to term representation"
  (make-term :coefficient (extract-coefficient expr)
             :variables (extract-variables expr)))

(defun term-to-expr (term)
  "Convert term back to expression"
  (let ((factors '()))
    ;; Add coefficient if not 1
    (unless (= (term-coefficient term) 1)
      (push (term-coefficient term) factors))
    
    ;; Add variables with powers
    (dolist (var-power (term-variables term))
      (let ((var (car var-power))
            (power (cdr var-power)))
        (cond
          ((= power 1)
           (push var factors))
          ((> power 1)
           (push (sym:symbolic '^ var power) factors)))))
    
    ;; Combine factors
    (cond
      ((null factors) 1)
      ((null (cdr factors)) (car factors))
      (t (sym:symbolic '* factors)))))

(defun terms-similar-p (term1 term2)
  "Check if two terms have the same variables"
  (equal (term-variables term1) (term-variables term2)))

;;; Term collection

(defun collect-terms (expr)
  "Collect like terms in an expression"
  (cond
    ;; Not a sum - return as is
    ((not (and (sym:expr-p expr)
               (eq (sym:expr-op expr) '+)))
     expr)
    
    ;; Process sum
    (t
     (let ((terms-map (make-hash-table :test 'equal))
           (const-sum 0))
       
       ;; Extract all terms
       (dolist (arg (flatten-sum expr))
         (cond
           ;; Numeric constant
           ((numberp arg)
            (incf const-sum arg))
           
           ;; Symbolic constant
           ((sym:const-p arg)
            (incf const-sum (sym:const-value arg)))
           
           ;; Term
           (t
            (let ((term (expr-to-term arg)))
              (incf (gethash (term-variables term) terms-map 0)
                    (term-coefficient term))))))
       
       ;; Rebuild expression
       (let ((result-terms '()))
         ;; Add collected terms
         (maphash (lambda (vars coeff)
                   (unless (zerop coeff)
                     (push (term-to-expr 
                           (make-term :coefficient coeff
                                     :variables vars))
                          result-terms)))
                 terms-map)
         
         ;; Add constant if non-zero
         (unless (zerop const-sum)
           (push const-sum result-terms))
         
         ;; Combine results
         (cond
           ((null result-terms) 0)
           ((null (cdr result-terms)) (car result-terms))
           (t (sym:symbolic '+ 
                           (sort result-terms #'term-order)))))))))

(defun flatten-sum (expr)
  "Flatten nested additions"
  (cond
    ((and (sym:expr-p expr)
          (eq (sym:expr-op expr) '+))
     (mapcan #'flatten-sum (sym:expr-args expr)))
    (t (list expr))))

(defun term-order (a b)
  "Ordering for terms (for canonical form)"
  ;; Order by degree, then lexicographically
  (let ((deg-a (expr-degree a))
        (deg-b (expr-degree b)))
    (cond
      ((> deg-a deg-b) t)
      ((< deg-a deg-b) nil)
      (t (string< (format nil "~A" a)
                  (format nil "~A" b))))))

(defun expr-degree (expr)
  "Get degree of expression"
  (cond
    ((numberp expr) 0)
    ((sym:const-p expr) 0)
    ((sym:var-p expr) 1)
    ((and (sym:expr-p expr)
          (eq (sym:expr-op expr) '^)
          (numberp (second (sym:expr-args expr))))
     (second (sym:expr-args expr)))
    ((and (sym:expr-p expr)
          (eq (sym:expr-op expr) '*))
     (reduce #'+ (mapcar #'expr-degree (sym:expr-args expr))))
    (t 1)))

;;; Expression expansion

(defun expand-product (expr)
  "Expand products (distribute multiplication)"
  (cond
    ;; Not a product
    ((not (and (sym:expr-p expr)
               (eq (sym:expr-op expr) '*)))
     expr)
    
    ;; Find sums to distribute over
    (t
     (let ((sums '())
           (others '()))
       (dolist (arg (sym:expr-args expr))
         (if (and (sym:expr-p arg)
                  (eq (sym:expr-op arg) '+))
             (push arg sums)
             (push others arg)))
       
       (if (null sums)
           expr
           (expand-product-helper (nreverse others) (nreverse sums)))))))

(defun expand-product-helper (factors sums)
  "Helper for product expansion"
  (if (null sums)
      (if (null (cdr factors))
          (car factors)
          (sym:symbolic '* factors))
      (let* ((sum (car sums))
             (terms (sym:expr-args sum))
             (expanded-terms
              (mapcar (lambda (term)
                       (expand-product-helper 
                        (cons term factors)
                        (cdr sums)))
                      terms)))
        (sym:symbolic '+ expanded-terms))))

;;; Factorization

(defun factor-common (expr)
  "Factor out common terms"
  (cond
    ;; Not a sum
    ((not (and (sym:expr-p expr)
               (eq (sym:expr-op expr) '+)))
     expr)
    
    ;; Find GCD of coefficients and common variables
    (t
     (let* ((terms (mapcar #'expr-to-term (sym:expr-args expr)))
            (gcd-coeff (reduce #'gcd (mapcar #'term-coefficient terms)))
            (common-vars (find-common-variables terms)))
       
       (if (and (= gcd-coeff 1) (null common-vars))
           expr
           (let ((common-factor (term-to-expr 
                                (make-term :coefficient gcd-coeff
                                          :variables common-vars)))
                 (factored-terms
                  (mapcar (lambda (term)
                           (term-to-expr
                            (make-term 
                             :coefficient (/ (term-coefficient term) gcd-coeff)
                             :variables (remove-variables 
                                       (term-variables term)
                                       common-vars))))
                         terms)))
             (sym:symbolic '*
                          common-factor
                          (sym:symbolic '+ factored-terms))))))))

(defun find-common-variables (terms)
  "Find variables common to all terms"
  (when terms
    (reduce #'intersect-variables 
            (mapcar #'term-variables (cdr terms))
            :initial-value (term-variables (car terms)))))

(defun intersect-variables (vars1 vars2)
  "Intersect variable lists, keeping minimum powers"
  (let ((result '()))
    (dolist (v1 vars1)
      (let ((v2 (assoc (car v1) vars2 :test 'equal)))
        (when v2
          (push (cons (car v1) (min (cdr v1) (cdr v2))) result))))
    (nreverse result)))

(defun remove-variables (vars to-remove)
  "Remove or reduce powers of variables"
  (let ((result '()))
    (dolist (v vars)
      (let ((r (assoc (car v) to-remove :test 'equal)))
        (if r
            (let ((new-power (- (cdr v) (cdr r))))
              (when (> new-power 0)
                (push (cons (car v) new-power) result)))
            (push v result))))
    (nreverse result)))

;;; Expression normalization

(defun normalize-expr (expr)
  "Convert to canonical normal form"
  (cond
    ;; Atoms are already normal
    ((or (numberp expr)
         (sym:var-p expr)
         (sym:const-p expr))
     expr)
    
    ;; Expressions
    ((sym:expr-p expr)
     (let ((op (sym:expr-op expr))
           (args (mapcar #'normalize-expr (sym:expr-args expr))))
       (case op
         (+
          (collect-terms (sym:symbolic op (flatten-associative op args))))
         
         (*
          (let ((flat (flatten-associative op args)))
            ;; Sort factors
            (sym:symbolic op (sort flat #'factor-order))))
         
         (otherwise
          (sym:symbolic op args)))))
    
    ;; Unknown
    (t expr)))

(defun flatten-associative (op args)
  "Flatten nested associative operations"
  (when (member op '(+ *))
    (let ((result '()))
      (dolist (arg args)
        (if (and (sym:expr-p arg)
                 (eq (sym:expr-op arg) op))
            (setf result (append result (sym:expr-args arg)))
            (push arg result)))
      (nreverse result))))

(defun factor-order (a b)
  "Ordering for factors in products"
  ;; Numbers first, then variables, then expressions
  (cond
    ((numberp a) (not (numberp b)))
    ((numberp b) nil)
    ((sym:var-p a)
     (if (sym:var-p b)
         (string< (symbol-name (sym:var-name a))
                  (symbol-name (sym:var-name b)))
         t))
    ((sym:var-p b) nil)
    (t (string< (format nil "~A" a)
                (format nil "~A" b)))))

;;; Convenience functions

(defun combine-like-terms (expr)
  "Combine like terms in expression"
  (collect-terms (normalize-expr expr)))

(defun canonical-form (expr)
  "Convert to canonical form"
  (normalize-expr (collect-terms (expand-product expr))))

(defun sort-terms (expr)
  "Sort terms in canonical order"
  (normalize-expr expr))