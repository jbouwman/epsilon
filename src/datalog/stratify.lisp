;;;; stratify.lisp - Datalog Stratification
;;;;
;;;; Analyzes rule dependencies to determine evaluation order. Rules with
;;;; negation or aggregation must be evaluated after the relations they
;;;; depend on negatively. Uses epsilon.graph for cycle detection on the
;;;; negative subgraph, then assigns strata via fixed-point propagation
;;;; on individual relations (no SCC condensation needed).

(defpackage :epsilon.datalog.stratify
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:ast :epsilon.datalog.ast)
                    (:m :epsilon.map)
                    (:graph :epsilon.graph))
  (:export
   ;; Dependency graph
   #:dep-edge #:dep-edge-p #:make-dep-edge
   #:dep-edge-from #:dep-edge-to #:dep-edge-polarity
   #:build-dependency-graph #:depends-on-p #:get-all-relations
   ;; Stratification
   #:stratified-program #:stratified-program-p
   #:stratified-program-program #:stratified-program-strata
   #:stratified-program-num-strata #:stratified-program-rules-by-stratum
   #:stratify #:stratum-of
   ;; Cycle detection and stratum assignment
   #:check-negative-cycles #:assign-strata))

(in-package :epsilon.datalog.stratify)

;;; ============================================================
;;; Dependency Graph
;;; ============================================================

;; A dependency edge from relation A to relation B means "A depends on B".
;; Polarity is :positive for normal body atoms, :negative for negated atoms.

(defstruct (dep-edge (:constructor make-dep-edge))
  "An edge in the dependency graph."
  (from nil :type keyword :read-only t)    ; relation name of rule head
  (to nil :type keyword :read-only t)      ; relation name of body atom
  (polarity :positive :type keyword :read-only t)) ; :positive or :negative

(defun build-dependency-graph (rules)
  "Build a dependency graph from a list of rules.
   Returns a list of dep-edge structs."
  (let ((edges nil))
    (dolist (rule rules)
      (let ((head-rel (ast:head-relation rule)))
        ;; Positive atoms in body
        (dolist (atom (ast:rule-body-atoms rule))
          (push (make-dep-edge :from head-rel
                               :to (ast:atom-relation atom)
                               :polarity :positive)
                edges))
        ;; Negated atoms in body
        (dolist (neg (ast:rule-body-negations rule))
          (push (make-dep-edge :from head-rel
                               :to (ast:atom-relation (ast:negation-atom neg))
                               :polarity :negative)
                edges))))
    (nreverse edges)))

(defun depends-on-p (edges from-rel to-rel)
  "Check if FROM-REL depends on TO-REL (directly)."
  (some (lambda (e) (and (eq (dep-edge-from e) from-rel)
                         (eq (dep-edge-to e) to-rel)))
        edges))

(defun get-all-relations (rules)
  "Get all relation names that appear in rules (heads and bodies)."
  (let ((rels nil))
    (dolist (rule rules)
      (pushnew (ast:head-relation rule) rels)
      (dolist (atom (ast:rule-body-atoms rule))
        (pushnew (ast:atom-relation atom) rels))
      (dolist (neg (ast:rule-body-negations rule))
        (pushnew (ast:atom-relation (ast:negation-atom neg)) rels)))
    rels))

;;; ============================================================
;;; Negative Cycle Detection
;;; ============================================================

(defun check-negative-cycles (relations edges)
  "Check for negative cycles (unstratifiable programs).
   Finds SCCs in the negative-only subgraph. Any SCC with more than
   one node, or a single node with a self-edge, indicates a negative
   cycle. Signals an error if found."
  (let* ((neg-adj (make-hash-table :test 'eq)))
    ;; Build adjacency for negative edges only
    (dolist (e edges)
      (when (eq (dep-edge-polarity e) :negative)
        (push (dep-edge-to e) (gethash (dep-edge-from e) neg-adj))))
    ;; Find SCCs in the negative subgraph
    (let ((sccs (graph:tarjan-scc relations
                                  (lambda (v) (gethash v neg-adj))
                                  :test 'eq)))
      (dolist (scc sccs)
        (cond
          ((> (length scc) 1)
           (error "Unstratifiable program: negative cycle between ~{~A~^, ~}" scc))
          ((= (length scc) 1)
           (let ((rel (first scc)))
             (when (member rel (gethash rel neg-adj))
               (error "Unstratifiable program: ~A negatively depends on itself"
                      rel)))))))))

;;; ============================================================
;;; Stratification
;;; ============================================================

(defun assign-strata (relations edges)
  "Assign stratum numbers based on dependency edge constraints.
   Negative edges force the dependent into a higher stratum than
   its dependency. Positive edges propagate strata (same or higher).
   Uses Datalog cycle detection on the negative subgraph to verify
   stratifiability, then a fixed-point iteration to propagate strata
   across individual relations (no SCC condensation needed).
   Returns a map from relation name to stratum number (0-based)."
  ;; Check for negative cycles first
  (check-negative-cycles relations edges)

  (let ((strata (make-hash-table :test 'eq))
        (result m:+empty+))

    ;; Initialize all strata to 0
    (dolist (rel relations)
      (setf (gethash rel strata) 0))

    ;; Propagate stratum constraints until stable.
    ;; Negative edge (A -> B): stratum(A) >= stratum(B) + 1
    ;; Positive edge (A -> B): stratum(A) >= stratum(B)
    ;; Converges because strata only increase and the negative
    ;; subgraph is acyclic (guaranteed by check-negative-cycles).
    (let ((changed t))
      (loop while changed do
        (setf changed nil)
        (dolist (e edges)
          (let* ((from-stratum (gethash (dep-edge-from e) strata 0))
                 (to-stratum (gethash (dep-edge-to e) strata 0))
                 (needed (if (eq (dep-edge-polarity e) :negative)
                             (1+ to-stratum)
                             to-stratum)))
            (when (> needed from-stratum)
              (setf (gethash (dep-edge-from e) strata) needed)
              (setf changed t))))))

    ;; Build result map
    (dolist (rel relations)
      (setf result (m:assoc result rel (gethash rel strata))))

    result))

;;; ============================================================
;;; Public API
;;; ============================================================

(defstruct (stratified-program (:constructor %make-stratified-program))
  "A program with stratum assignments."
  (program nil :read-only t)                ; original program
  (strata nil :read-only t)                 ; map: relation -> stratum number
  (num-strata nil :type integer :read-only t)
  (rules-by-stratum nil :type list :read-only t)) ; list of (stratum . rules)

(defun stratify (program)
  "Stratify a Datalog program. Returns a stratified-program.
   Signals an error if the program has negative cycles."
  (let* ((rules (ast:program-rules program))
         (edges (build-dependency-graph rules))
         (all-rels (get-all-relations rules))
         (strata-map (assign-strata all-rels edges)))

    ;; Determine number of strata
    (let ((num-strata (if (= 0 (m:count strata-map))
                          1
                          (1+ (reduce #'max (m:vals strata-map))))))

      ;; Group rules by stratum (based on head relation)
      (let ((rules-by-stratum
              (loop for s from 0 below num-strata
                    collect (cons s (remove-if-not
                                    (lambda (rule)
                                      (let ((stratum (m:get strata-map
                                                            (ast:head-relation rule)
                                                            0)))
                                        (= s stratum)))
                                    rules)))))

        (%make-stratified-program
         :program program
         :strata strata-map
         :num-strata num-strata
         :rules-by-stratum rules-by-stratum)))))

(defun stratum-of (stratified relation)
  "Return the stratum number for a relation in a stratified program."
  (m:get (stratified-program-strata stratified) relation 0))
