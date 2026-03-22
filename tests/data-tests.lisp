(defpackage epsilon.data-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.data data)
            (epsilon.match m))
  (:enter t))

;;; Define test data types

(data:defdata maybe (a)
  "Optional value type"
  (just a)
  (nothing))

(data:defdata tree (a)
  "Binary tree"
  (leaf a)
  (branch tree tree))

(data:defdata color ()
  "RGB color values"
  (red)
  (green)
  (blue))

(data:defdata expr ()
  "Simple expression type"
  (lit number)
  (add expr expr)
  (mul expr expr))

;;; Constructor tests

(deftest test-nullary-constructor
  (let ((n (nothing)))
    (assert-true (data:variant-p n))
    (assert-true (eq 'nothing (data:variant-tag n)))
    (assert-true (null (data:variant-values n)))))

(deftest test-unary-constructor
  (let ((j (just 42)))
    (assert-true (data:variant-p j))
    (assert-true (eq 'just (data:variant-tag j)))
    (assert-true (equal '(42) (data:variant-values j)))))

(deftest test-binary-constructor
  (let ((b (branch (leaf 1) (leaf 2))))
    (assert-true (data:variant-p b))
    (assert-true (eq 'branch (data:variant-tag b)))
    (assert-true (= 2 (length (data:variant-values b))))))

;;; Predicate tests

(deftest test-variant-predicate
  (assert-true (just-p (just 1)))
  (assert-true (not (just-p (nothing))))
  (assert-true (nothing-p (nothing)))
  (assert-true (not (nothing-p (just 1)))))

(deftest test-type-predicate
  (assert-true (maybe-p (just 1)))
  (assert-true (maybe-p (nothing)))
  (assert-true (tree-p (leaf 42)))
  (assert-true (tree-p (branch (leaf 1) (leaf 2))))
  (assert-true (not (maybe-p (leaf 1)))))

(deftest test-nullary-predicates
  (assert-true (red-p (red)))
  (assert-true (green-p (green)))
  (assert-true (blue-p (blue)))
  (assert-true (not (red-p (green)))))

;;; Accessor tests

(deftest test-unary-accessor
  (let ((j (just 42)))
    (assert-true (= 42 (just-a j)))))

(deftest test-binary-accessors
  (let ((b (branch (leaf 1) (leaf 2))))
    (assert-true (leaf-p (branch-tree b)))
    (assert-true (= 1 (leaf-a (branch-tree b))))))

(deftest test-accessor-wrong-type
  (assert-condition (error)
    (just-a (nothing))))

(deftest test-expr-accessors
  (let ((e (add (lit 1) (mul (lit 2) (lit 3)))))
    (assert-true (lit-p (add-expr e)))
    (assert-true (mul-p (add-expr0 e)))))  ; Testing the numbered accessor fallback

;;; Variant info tests

(deftest test-variant-type
  (assert-true (eq 'maybe (data:variant-type (just 1))))
  (assert-true (eq 'tree (data:variant-type (leaf 42)))))

(deftest test-list-variants
  (let ((variants (data:list-variants 'maybe)))
    (assert-true (member 'just variants))
    (assert-true (member 'nothing variants))
    (assert-true (= 2 (length variants)))))

;;; Nested structures

(deftest test-nested-tree
  (let ((tree (branch
               (branch (leaf 1) (leaf 2))
               (branch (leaf 3) (leaf 4)))))
    (assert-true (tree-p tree))
    (assert-true (branch-p tree))
    (assert-true (branch-p (branch-tree tree)))
    (assert-true (leaf-p (branch-tree0 (branch-tree tree))))))

;;; Pattern matching integration

(deftest test-match-just
  (assert-true (= 42 (m:match (just 42)
              ((just x) x)
              (_ 0)))))

(deftest test-match-nothing
  (assert-true (= 0 (m:match (nothing)
             ((just x) x)
             (_ 0)))))

(deftest test-match-with-predicate
  (assert-true (eq :just (m:match (just 42)
                  ((? just-p) :just)
                  ((? nothing-p) :nothing)))))

(deftest test-match-tree
  (labels ((tree-sum (tr)
             (m:match tr
               ((? leaf-p l) (leaf-a l))
               ((? branch-p b)
                (+ (tree-sum (branch-tree b))
                   (tree-sum (branch-tree0 b)))))))
    (assert-true (= 10
           (tree-sum (branch
                      (branch (leaf 1) (leaf 2))
                      (branch (leaf 3) (leaf 4))))))))

;;; Print representation

(deftest test-print-nullary
  (let ((str (format nil "~A" (nothing))))
    (assert-true (search "NOTHING" str))))

(deftest test-print-with-values
  (let ((str (format nil "~A" (just 42))))
    (assert-true (search "JUST" str))
    (assert-true (search "42" str))))

;;; Documentation

(deftest test-type-documentation
  ;; Just verify the type was created; documentation is stored in metadata
  (assert-true (not (null (data:list-variants 'maybe)))))
