;;;; graph.lisp - Declarative Graph Queries via Datalog
;;;;
;;;; Provides pre-parsed Datalog programs for common graph operations
;;;; (transitive closure, cycle detection) and a convenience API so callers
;;;; don't need to manage stores or programs directly.
;;;;
;;;; Programs are parsed once at load time and reused on every call.
;;;; Ordering delegates to epsilon.graph:topological-sort because
;;;; depth-based ordering in Datalog requires max aggregation in
;;;; recursive strata, which the evaluator doesn't yet support.

(defpackage :epsilon.datalog.graph
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:parser :epsilon.datalog.parser)
                    (:eval :epsilon.datalog.evaluate)
                    (:mem :epsilon.datalog.backend.memory)
                    (:graph :epsilon.graph))
  (:export
   ;; Pre-parsed programs
   #:+transitive-closure-program+
   #:+cycle-detection-program+
   ;; Convenience API
   #:edges-to-store
   #:transitive-closure
   #:reachable-from
   #:dependents-of
   #:detect-cycles
   #:topological-order))

(in-package :epsilon.datalog.graph)

;;; ============================================================
;;; Pre-parsed Programs
;;; ============================================================

(define-constant +transitive-closure-program+
    (parser:parse-program
     '(defprogram transitive-closure
        (relation edge (from :any) (to :any))
        (rule (reachable ?x ?y) (edge ?x ?y))
        (rule (reachable ?x ?z) (edge ?x ?y) (reachable ?y ?z))))
  "Transitive closure: (reachable ?x ?y) from (edge ?x ?y) base facts.")

(define-constant +cycle-detection-program+
    (parser:parse-program
     '(defprogram cycle-detection
        (relation edge (from :any) (to :any))
        (rule (reachable ?x ?y) (edge ?x ?y))
        (rule (reachable ?x ?z) (edge ?x ?y) (reachable ?y ?z))
        (rule (in-cycle ?x) (reachable ?x ?x))))
  "Cycle detection: extends TC with (in-cycle ?x) :- (reachable ?x ?x).")

;;; ============================================================
;;; Store Construction
;;; ============================================================

(defun edges-to-store (edges &key key-fn)
  "Convert a list of edges to a Datalog fact store.
   Each edge is a two-element list (from to).
   KEY-FN, if provided, is applied to each element of each edge."
  (let ((store (mem:make-store)))
    (dolist (edge edges store)
      (let ((from (first edge))
            (to (second edge)))
        (when key-fn
          (setf from (funcall key-fn from)
                to (funcall key-fn to)))
        (setf store (mem:assert-fact store :edge (list from to)))))))

;;; ============================================================
;;; Convenience API
;;; ============================================================

(defun transitive-closure (edges &key key-fn)
  "Compute the transitive closure of EDGES.
   Returns all (from to) reachable pairs as a list of two-element lists."
  (let* ((store (edges-to-store edges :key-fn key-fn))
         (result (eval:evaluate +transitive-closure-program+ store)))
    (mem:all-facts result :reachable)))

(defun reachable-from (edges node &key key-fn)
  "Return all nodes reachable from NODE in the graph defined by EDGES."
  (let ((all-pairs (transitive-closure edges :key-fn key-fn)))
    (loop for pair in all-pairs
          when (equal (first pair) node)
            collect (second pair))))

(defun dependents-of (edges node &key key-fn)
  "Return all nodes that transitively depend on NODE.
   This is the reverse direction: who can reach NODE."
  (let ((all-pairs (transitive-closure edges :key-fn key-fn)))
    (loop for pair in all-pairs
          when (equal (second pair) node)
            collect (first pair))))

(defun detect-cycles (edges &key key-fn)
  "Return a list of nodes that participate in cycles."
  (let* ((store (edges-to-store edges :key-fn key-fn))
         (result (eval:evaluate +cycle-detection-program+ store)))
    (mapcar #'first (mem:all-facts result :in-cycle))))

(defun topological-order (nodes edges &key key-fn)
  "Topologically sort NODES given EDGES.
   Cycle detection uses Datalog; ordering delegates to graph:topological-sort
   because depth-based ordering in Datalog requires max aggregation in
   recursive strata, which the evaluator doesn't yet support."
  (let ((cycles (detect-cycles edges :key-fn key-fn)))
    (when cycles
      (error 'graph:circular-dependency :cycle cycles)))
  ;; Build adjacency function from edges
  (let ((adj (make-hash-table :test 'equal)))
    (dolist (edge edges)
      (let ((from (if key-fn (funcall key-fn (first edge)) (first edge)))
            (to (if key-fn (funcall key-fn (second edge)) (second edge))))
        (push to (gethash from adj))))
    (graph:topological-sort nodes
                            (lambda (node) (gethash node adj))
                            :test 'equal)))
