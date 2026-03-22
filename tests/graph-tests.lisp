;;;; Graph Utility Tests
;;;;
;;;; Tests for Tarjan's SCC and topological sort.

(defpackage epsilon.graph-tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:require (epsilon.graph graph))
  (:enter t))

;;; ============================================================
;;; Tarjan's SCC Tests
;;; ============================================================

(deftest test-tarjan-empty-graph
  "Empty node list returns empty SCC list"
  (assert-true (null (graph:tarjan-scc nil (lambda (n) (declare (ignore n)) nil)))))

(deftest test-tarjan-single-node
  "Single node with no edges"
  (let ((sccs (graph:tarjan-scc '(:a) (lambda (n) (declare (ignore n)) nil))))
    (assert-true (= 1 (length sccs)))
    (assert-true (equal '((:a)) sccs))))

(deftest test-tarjan-linear-chain
  "Linear chain: a -> b -> c (no cycles)"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b))
                  (:b '(:c))
                  (:c nil))))
         (sccs (graph:tarjan-scc '(:a :b :c) adj)))
    ;; Three singleton SCCs in topological order
    (assert-true (= 3 (length sccs)))
    (assert-true (every (lambda (scc) (= 1 (length scc))) sccs))))

(deftest test-tarjan-diamond
  "Diamond: a -> b, a -> c, b -> d, c -> d (no cycles)"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b :c))
                  (:b '(:d))
                  (:c '(:d))
                  (:d nil))))
         (sccs (graph:tarjan-scc '(:a :b :c :d) adj)))
    (assert-true (= 4 (length sccs)))
    (assert-true (every (lambda (scc) (= 1 (length scc))) sccs))))

(deftest test-tarjan-cycle
  "Simple cycle: a -> b -> a"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b))
                  (:b '(:a)))))
         (sccs (graph:tarjan-scc '(:a :b) adj)))
    ;; One SCC containing both nodes
    (assert-true (= 1 (length sccs)))
    (assert-true (= 2 (length (first sccs))))))

(deftest test-tarjan-self-loop
  "Self-loop: a -> a"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:a)))))
         (sccs (graph:tarjan-scc '(:a) adj)))
    ;; Single SCC with one node
    (assert-true (= 1 (length sccs)))
    (assert-true (= 1 (length (first sccs))))))

;;; ============================================================
;;; Topological Sort Tests
;;; ============================================================

(deftest test-toposort-empty
  "Empty graph sorts to empty list"
  (multiple-value-bind (sorted cycles)
      (graph:topological-sort nil (lambda (n) (declare (ignore n)) nil) :on-cycle :collect)
    (assert-true (null sorted))
    (assert-true (null cycles))))

(deftest test-toposort-linear-chain
  "Linear chain: a -> b -> c. Dependencies first."
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b))
                  (:b '(:c))
                  (:c nil))))
         (sorted (graph:topological-sort '(:a :b :c) adj)))
    ;; c before b before a
    (assert-true (= 3 (length sorted)))
    (assert-true (< (position :c sorted) (position :b sorted)))
    (assert-true (< (position :b sorted) (position :a sorted)))))

(deftest test-toposort-diamond
  "Diamond: d before b and c, both before a"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b :c))
                  (:b '(:d))
                  (:c '(:d))
                  (:d nil))))
         (sorted (graph:topological-sort '(:a :b :c :d) adj)))
    (assert-true (= 4 (length sorted)))
    (assert-true (< (position :d sorted) (position :b sorted)))
    (assert-true (< (position :d sorted) (position :c sorted)))
    (assert-true (< (position :b sorted) (position :a sorted)))
    (assert-true (< (position :c sorted) (position :a sorted)))))

(deftest test-toposort-cycle-error
  "Cycle with :on-cycle :error signals circular-dependency"
  (let ((adj (lambda (n)
               (case n
                 (:a '(:b))
                 (:b '(:a))))))
    (assert-condition (graph:circular-dependency)
      (graph:topological-sort '(:a :b) adj :on-cycle :error))))

(deftest test-toposort-cycle-collect
  "Cycle with :on-cycle :collect returns cycles as second value"
  (let ((adj (lambda (n)
               (case n
                 (:a '(:b))
                 (:b '(:a))))))
    (multiple-value-bind (sorted cycles)
        (graph:topological-sort '(:a :b) adj :on-cycle :collect)
      ;; Both nodes still in sorted output
      (assert-true (= 2 (length sorted)))
      ;; One cycle detected
      (assert-true (= 1 (length cycles)))
      (assert-true (= 2 (length (first cycles)))))))

(deftest test-toposort-self-loop-error
  "Self-loop with :on-cycle :error signals circular-dependency"
  (let ((adj (lambda (n)
               (case n
                 (:a '(:a))))))
    (assert-condition (graph:circular-dependency)
      (graph:topological-sort '(:a) adj :on-cycle :error))))

(deftest test-toposort-self-loop-collect
  "Self-loop with :on-cycle :collect returns self-loop as cycle"
  (let ((adj (lambda (n)
               (case n
                 (:a '(:a))))))
    (multiple-value-bind (sorted cycles)
        (graph:topological-sort '(:a) adj :on-cycle :collect)
      (assert-true (= 1 (length sorted)))
      (assert-true (= 1 (length cycles))))))

(deftest test-toposort-string-keys-equal
  "String keys with :test 'equal"
  (let* ((adj (lambda (n)
                (cond
                  ((string= n "a") '("b"))
                  ((string= n "b") '("c"))
                  (t nil))))
         (sorted (graph:topological-sort '("a" "b" "c") adj :test 'equal)))
    (assert-true (= 3 (length sorted)))
    (assert-true (< (position "c" sorted :test #'string=)
           (position "b" sorted :test #'string=)))
    (assert-true (< (position "b" sorted :test #'string=)
           (position "a" sorted :test #'string=)))))

(deftest test-toposort-keyword-keys-eq
  "Keyword keys with :test 'eq"
  (let* ((adj (lambda (n)
                (case n
                  (:x '(:y))
                  (:y '(:z))
                  (:z nil))))
         (sorted (graph:topological-sort '(:x :y :z) adj :test 'eq)))
    (assert-true (= 3 (length sorted)))
    (assert-true (< (position :z sorted) (position :y sorted)))
    (assert-true (< (position :y sorted) (position :x sorted)))))

(deftest test-toposort-mixed-cycle-and-dag
  "Graph with one cycle component and one DAG component"
  (let* ((adj (lambda (n)
                (case n
                  (:a '(:b))
                  (:b '(:a))  ; cycle: a <-> b
                  (:c '(:d))
                  (:d nil)))))
    (multiple-value-bind (sorted cycles)
        (graph:topological-sort '(:a :b :c :d) adj :on-cycle :collect)
      (assert-true (= 4 (length sorted)))
      (assert-true (= 1 (length cycles)))
      ;; DAG part: d before c
      (assert-true (< (position :d sorted) (position :c sorted))))))

(deftest test-toposort-disconnected-nodes
  "Disconnected nodes with no edges"
  (let ((sorted (graph:topological-sort '(:a :b :c)
                                        (lambda (n) (declare (ignore n)) nil))))
    (assert-true (= 3 (length sorted)))))
