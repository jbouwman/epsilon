;;;; unify.lisp - Datalog Unification
;;;;
;;;; Substitution maps over HAMT for binding Datalog variables to terms.
;;;; Simpler than Prolog unification: no function symbols means no occurs
;;;; check is needed.

(defpackage :epsilon.datalog.unify
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:ast :epsilon.datalog.ast)
                    (:m :epsilon.map))
  (:export
   #:+empty-subst+
   #:empty-subst-p #:lookup #:extend
   #:subst-bindings #:subst-count
   #:unify-terms #:unify-atoms
   #:apply-subst #:apply-subst-to-atom
   #:ground-atom-p #:extract-values))

(in-package :epsilon.datalog.unify)

;;; ============================================================
;;; Substitution Type
;;; ============================================================

;; A substitution is a HAMT map from variable names (keywords) to terms.

(defvar +empty-subst+ m:+empty+
  "The empty substitution.")

(defun empty-subst-p (subst)
  "Check if substitution is empty."
  (= 0 (m:count subst)))

(defun lookup (subst var-name)
  "Look up a variable name in the substitution. Returns the term or nil."
  (m:get subst var-name))

(defun extend (subst var-name term)
  "Extend substitution with a new binding. Returns a new substitution."
  (m:assoc subst var-name term))

(defun subst-bindings (subst)
  "Return the substitution as an alist of (var-name . term) pairs."
  (m:to-alist subst))

(defun subst-count (subst)
  "Return number of bindings in substitution."
  (m:count subst))

;;; ============================================================
;;; Term Unification
;;; ============================================================

(defun unify-terms (term1 term2 subst)
  "Unify two terms under the given substitution.
   Returns the extended substitution on success, or nil on failure.

   Rules:
   - variable + anything: bind variable (if unbound) or unify bound value
   - constant + constant: succeed if equal, fail otherwise
   - Two variables: bind one to the other"
  (when (null subst) (return-from unify-terms nil))
  (cond
    ;; Both are variables
    ((and (ast:variable-p term1) (ast:variable-p term2))
     (let ((name1 (ast:variable-name term1))
           (name2 (ast:variable-name term2)))
       (cond
         ;; Same variable
         ((eq name1 name2) subst)
         ;; term1 is bound
         ((lookup subst name1)
          (unify-terms (lookup subst name1) term2 subst))
         ;; term2 is bound
         ((lookup subst name2)
          (unify-terms term1 (lookup subst name2) subst))
         ;; Both unbound: bind term1 to term2
         (t (extend subst name1 term2)))))

    ;; term1 is a variable
    ((ast:variable-p term1)
     (let* ((name (ast:variable-name term1))
            (bound (lookup subst name)))
       (if bound
           (unify-terms bound term2 subst)
           (extend subst name term2))))

    ;; term2 is a variable
    ((ast:variable-p term2)
     (let* ((name (ast:variable-name term2))
            (bound (lookup subst name)))
       (if bound
           (unify-terms term1 bound subst)
           (extend subst name term1))))

    ;; Both are constants
    ((and (ast:constant-p term1) (ast:constant-p term2))
     (if (equal (ast:constant-value term1) (ast:constant-value term2))
         subst
         nil))

    ;; Incompatible types
    (t nil)))

;;; ============================================================
;;; Atom Unification
;;; ============================================================

(defun unify-atoms (atom1 atom2 subst)
  "Unify two atoms: same relation, same arity, unify terms pairwise.
   Returns extended substitution or nil on failure."
  (when (null subst) (return-from unify-atoms nil))
  ;; Relations must match
  (unless (eq (ast:atom-relation atom1) (ast:atom-relation atom2))
    (return-from unify-atoms nil))
  ;; Arity must match
  (let ((terms1 (ast:atom-terms atom1))
        (terms2 (ast:atom-terms atom2)))
    (unless (= (length terms1) (length terms2))
      (return-from unify-atoms nil))
    ;; Unify terms pairwise
    (loop for t1 in terms1
          for t2 in terms2
          do (setf subst (unify-terms t1 t2 subst))
          when (null subst) do (return-from unify-atoms nil))
    subst))

;;; ============================================================
;;; Substitution Application
;;; ============================================================

(defun apply-subst (subst term)
  "Apply substitution to a term, replacing variables with their bindings.
   Follows binding chains until a non-variable or unbound variable is reached."
  (cond
    ((ast:variable-p term)
     (let ((bound (lookup subst (ast:variable-name term))))
       (if bound
           (apply-subst subst bound)  ; chase binding chain
           term)))                     ; unbound variable stays
    (t term)))                         ; constants stay

(defun apply-subst-to-atom (subst atom)
  "Apply substitution to all terms in an atom."
  (ast:make-atom
   :relation (ast:atom-relation atom)
   :terms (mapcar (lambda (term) (apply-subst subst term))
                  (ast:atom-terms atom))))

(defun ground-atom-p (subst atom)
  "Check if all variables in the atom are bound in the substitution."
  (every (lambda (term)
           (let ((resolved (apply-subst subst term)))
             (ast:constant-p resolved)))
         (ast:atom-terms atom)))

(defun extract-values (subst atom)
  "Extract the ground values from an atom under a substitution.
   Returns a list of Lisp values (unwrapped from constant terms).
   Signals an error if the atom is not fully ground."
  (mapcar (lambda (term)
            (let ((resolved (apply-subst subst term)))
              (if (ast:constant-p resolved)
                  (ast:constant-value resolved)
                  (error "Variable ~A is not ground" (ast:variable-name resolved)))))
          (ast:atom-terms atom)))
