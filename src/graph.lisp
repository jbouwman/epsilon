;;;; Tarjan's SCC algorithm and topological sort over arbitrary
;;;; graphs. Nodes can be any type; equality is controlled by :test.
;;;; The adjacency function takes a node and returns its successors
;;;; (dependencies in the "depends on" direction).

(defpackage epsilon.graph
  (:use cl)
  (:export
   tarjan-scc
   topological-sort
   circular-dependency
   circular-dependency-cycle))

(in-package epsilon.graph)

;;; ============================================================
;;; Conditions
;;; ============================================================

(define-condition circular-dependency (error)
  ((cycle :initarg :cycle :reader circular-dependency-cycle))
  (:report (lambda (c s)
             (format s "Circular dependency detected: ~S"
                     (circular-dependency-cycle c))))
  (:documentation "Signaled when a cycle is detected during topological sort."))

;;; ============================================================
;;; Tarjan's Strongly Connected Components
;;; ============================================================

(defun tarjan-scc (nodes adjacency-fn &key (test 'eql))
  "Find strongly connected components using Tarjan's algorithm.
   NODES is a list of all graph nodes.
   ADJACENCY-FN takes a node and returns a list of successor nodes.
   TEST is the equality test for node identity (default EQL).

   Returns a list of SCCs (each SCC is a list of nodes) in topological
   order (dependencies before dependents)."
  (let* ((index-counter 0)
         (stack nil)
         (on-stack (make-hash-table :test test))
         (indices (make-hash-table :test test))
         (lowlinks (make-hash-table :test test))
         (sccs nil))

    (labels ((strongconnect (v)
               (setf (gethash v indices) index-counter)
               (setf (gethash v lowlinks) index-counter)
               (incf index-counter)
               (push v stack)
               (setf (gethash v on-stack) t)

               ;; Visit successors
               (dolist (w (funcall adjacency-fn v))
                 (cond
                   ;; Not yet visited
                   ((not (nth-value 1 (gethash w indices)))
                    (strongconnect w)
                    (setf (gethash v lowlinks)
                          (min (gethash v lowlinks)
                               (gethash w lowlinks))))
                   ;; On stack (part of current SCC)
                   ((gethash w on-stack)
                    (setf (gethash v lowlinks)
                          (min (gethash v lowlinks)
                               (gethash w indices))))))

               ;; Root of SCC: pop all members
               (when (= (gethash v lowlinks) (gethash v indices))
                 (let ((scc nil))
                   (loop
                     (let ((w (pop stack)))
                       (setf (gethash w on-stack) nil)
                       (push w scc)
                       (when (funcall test w v) (return))))
                   (push scc sccs)))))

      ;; Visit all unvisited nodes
      (dolist (v nodes)
        (unless (nth-value 1 (gethash v indices))
          (strongconnect v))))

    ;; Tarjan produces SCCs in reverse topological order; reverse for
    ;; topological order (dependencies first).
    (nreverse sccs)))

;;; ============================================================
;;; Topological Sort
;;; ============================================================

(defun topological-sort (nodes adjacency-fn &key (on-cycle :error) (test 'eql))
  "Topologically sort NODES based on ADJACENCY-FN.
   ADJACENCY-FN takes a node and returns its dependency list (successors).
   TEST is the equality test for node identity (default EQL).

   ON-CYCLE controls cycle handling:
     :error   - signal CIRCULAR-DEPENDENCY condition (default)
     :collect - return cycles as second value

   Returns:
     Primary value: sorted list of nodes (dependencies first)
     Secondary value (when on-cycle is :collect): list of SCCs with cycles"
  (let ((sccs (tarjan-scc nodes adjacency-fn :test test))
        (sorted nil)
        (cycles nil))
    (dolist (scc sccs)
      (if (> (length scc) 1)
          ;; Multi-node SCC = cycle
          (ecase on-cycle
            (:error
             (error 'circular-dependency :cycle scc))
            (:collect
             (push scc cycles)
             ;; Still include nodes in output (arbitrary order within SCC)
             (dolist (node scc)
               (push node sorted))))
          ;; Single-node SCC: check for self-loop
          (let ((node (first scc)))
            (if (member node (funcall adjacency-fn node) :test test)
                ;; Self-loop
                (ecase on-cycle
                  (:error
                   (error 'circular-dependency :cycle scc))
                  (:collect
                   (push scc cycles)
                   (push node sorted)))
                ;; No cycle
                (push node sorted)))))
    (values (nreverse sorted) (nreverse cycles))))
