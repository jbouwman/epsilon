(defpackage #:epsilon.data.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:data #:epsilon.data)
   (#:m #:epsilon.match)))

(in-package #:epsilon.data.tests)

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
    (is (data:variant-p n))
    (is (eq 'nothing (data:variant-tag n)))
    (is (null (data:variant-values n)))))

(deftest test-unary-constructor
  (let ((j (just 42)))
    (is (data:variant-p j))
    (is (eq 'just (data:variant-tag j)))
    (is (equal '(42) (data:variant-values j)))))

(deftest test-binary-constructor
  (let ((b (branch (leaf 1) (leaf 2))))
    (is (data:variant-p b))
    (is (eq 'branch (data:variant-tag b)))
    (is (= 2 (length (data:variant-values b))))))

;;; Predicate tests

(deftest test-variant-predicate
  (is (just-p (just 1)))
  (is (not (just-p (nothing))))
  (is (nothing-p (nothing)))
  (is (not (nothing-p (just 1)))))

(deftest test-type-predicate
  (is (maybe-p (just 1)))
  (is (maybe-p (nothing)))
  (is (tree-p (leaf 42)))
  (is (tree-p (branch (leaf 1) (leaf 2))))
  (is (not (maybe-p (leaf 1)))))

(deftest test-nullary-predicates
  (is (red-p (red)))
  (is (green-p (green)))
  (is (blue-p (blue)))
  (is (not (red-p (green)))))

;;; Accessor tests

(deftest test-unary-accessor
  (let ((j (just 42)))
    (is (= 42 (just-a j)))))

(deftest test-binary-accessors
  (let ((b (branch (leaf 1) (leaf 2))))
    (is (leaf-p (branch-tree b)))
    (is (= 1 (leaf-a (branch-tree b))))))

(deftest test-accessor-wrong-type
  (is-thrown (error)
    (just-a (nothing))))

(deftest test-expr-accessors
  (let ((e (add (lit 1) (mul (lit 2) (lit 3)))))
    (is (lit-p (add-expr e)))
    (is (mul-p (add-expr0 e)))))  ; Testing the numbered accessor fallback

;;; Variant info tests

(deftest test-variant-type
  (is (eq 'maybe (data:variant-type (just 1))))
  (is (eq 'tree (data:variant-type (leaf 42)))))

(deftest test-list-variants
  (let ((variants (data:list-variants 'maybe)))
    (is (member 'just variants))
    (is (member 'nothing variants))
    (is (= 2 (length variants)))))

;;; Nested structures

(deftest test-nested-tree
  (let ((tree (branch
               (branch (leaf 1) (leaf 2))
               (branch (leaf 3) (leaf 4)))))
    (is (tree-p tree))
    (is (branch-p tree))
    (is (branch-p (branch-tree tree)))
    (is (leaf-p (branch-tree0 (branch-tree tree))))))

;;; Pattern matching integration

(deftest test-match-just
  (is (= 42 (m:match (just 42)
              ((just x) x)
              (_ 0)))))

(deftest test-match-nothing
  (is (= 0 (m:match (nothing)
             ((just x) x)
             (_ 0)))))

(deftest test-match-with-predicate
  (is (eq :just (m:match (just 42)
                  ((? just-p) :just)
                  ((? nothing-p) :nothing)))))

(deftest test-match-tree
  (labels ((tree-sum (tr)
             (m:match tr
               ((? leaf-p l) (leaf-a l))
               ((? branch-p b)
                (+ (tree-sum (branch-tree b))
                   (tree-sum (branch-tree0 b)))))))
    (is (= 10
           (tree-sum (branch
                      (branch (leaf 1) (leaf 2))
                      (branch (leaf 3) (leaf 4))))))))

;;; Print representation

(deftest test-print-nullary
  (let ((str (format nil "~A" (nothing))))
    (is (search "NOTHING" str))))

(deftest test-print-with-values
  (let ((str (format nil "~A" (just 42))))
    (is (search "JUST" str))
    (is (search "42" str))))

;;; Documentation

(deftest test-type-documentation
  ;; Just verify the type was created; documentation is stored in metadata
  (is (not (null (data:list-variants 'maybe)))))
