(defpackage #:epsilon.protocol.ext.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:pext #:epsilon.protocol.ext)))

(in-package #:epsilon.protocol.ext.tests)

;;; Define test protocols

(pext:defprotocol showable
  "Types that can be shown as strings"
  (show (obj) "Convert to string representation"))

(pext:defprotocol numeric
  "Types that support numeric operations"
  (add (a b) "Add two values")
  (mul (a b) "Multiply two values"))

(pext:defprotocol container
  "Types that hold values"
  (get-value (c) "Get the contained value")
  (map-value (f c) "Apply function to contained value"))

;;; Define a test type
(defstruct box value)

;;; Extend protocols to types

(pext:extend-type list
  showable
  (show (xs) (format nil "(~{~A~^ ~})" xs)))

(pext:extend-type box
  showable
  (show (b) (format nil "[~A]" (box-value b)))

  container
  (get-value (b) (box-value b))
  (map-value (b f) (make-box :value (funcall f (box-value b)))))

(pext:extend-type number
  numeric
  (add (a b) (+ a b))
  (mul (a b) (* a b)))

;;; Protocol definition tests

(deftest test-defprotocol-creates-generic
  (is (fboundp 'show))
  (is (fboundp 'add))
  (is (fboundp 'mul)))

(deftest test-protocol-methods
  (let ((methods (pext:protocol-methods 'showable)))
    (is (not (null methods)))
    (is (assoc 'show methods))))

;;; extend-type tests

(deftest test-extend-type-single-method
  (is (equal "(1 2 3)" (show '(1 2 3)))))

(deftest test-extend-type-multiple-methods
  (let ((b (make-box :value 42)))
    (is (equal "[42]" (show b)))
    (is (= 42 (get-value b)))))

(deftest test-extend-type-map-value
  (let* ((b (make-box :value 5))
         (result (map-value b #'1+)))
    (is (box-p result))
    (is (= 6 (box-value result)))))

(deftest test-extend-type-number
  (is (= 7 (add 3 4)))
  (is (= 12 (mul 3 4))))

;;; extend-protocol tests

(pext:defprotocol measurable
  "Types with a size"
  (size (obj) "Get the size"))

(pext:extend-protocol measurable
  list
  (size (xs) (length xs))

  string
  (size (s) (length s))

  vector
  (size (v) (length v)))

(deftest test-extend-protocol
  (is (= 3 (size '(1 2 3))))
  (is (= 5 (size "hello")))
  (is (= 4 (size #(a b c d)))))

;;; satisfies-p tests

(deftest test-satisfies-p-true
  (is (pext:satisfies-p '(1 2 3) 'showable))
  (is (pext:satisfies-p (make-box :value 1) 'showable))
  (is (pext:satisfies-p (make-box :value 1) 'container)))

(deftest test-satisfies-p-false
  ;; list doesn't implement container
  (is (not (pext:satisfies-p '(1 2 3) 'container))))

;;; list-implementations tests

(deftest test-list-implementations
  (let ((impls (pext:list-implementations 'showable)))
    (is (member 'list impls))
    (is (member 'box impls))))

;;; Protocol inheritance tests

(pext:defprotocol functor
  "Mappable containers"
  (fmap (f container) "Map function over container"))

(pext:defprotocol applicative
  "Applicative functors"
  (:extends functor)
  (pure (value hint) "Wrap in minimal context")
  (ap (ff fa) "Apply wrapped function"))

(pext:extend-type list
  functor
  (fmap (xs f) (mapcar f xs))

  applicative
  (pure (hint x) (declare (ignore hint)) (list x))
  (ap (xs fs) (mapcan (lambda (f) (mapcar f xs)) fs)))

(deftest test-protocol-extends
  (let ((p (epsilon.map:get epsilon.protocol.ext::*protocol-registry* 'applicative)))
    (is (not (null p)))
    (is (equal '(functor) (pext::protocol-extends p)))))

(deftest test-extended-protocol-methods
  (is (equal '(2 4 6) (fmap '(1 2 3) #'(lambda (x) (* x 2))))))

(deftest test-applicative-pure
  (is (equal '(42) (pure nil 42))))

(deftest test-applicative-ap
  (is (equal '(2 3 4 3 4 5) (ap '(1 2 3) (list #'1+ #'(lambda (x) (+ x 2)))))))

;;; Multiple protocols on same type

(deftest test-multiple-protocols-single-type
  (let ((b (make-box :value 10)))
    ;; showable protocol
    (is (stringp (show b)))
    ;; container protocol
    (is (= 10 (get-value b)))
    ;; Both should satisfy
    (is (pext:satisfies-p b 'showable))
    (is (pext:satisfies-p b 'container))))
