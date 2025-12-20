;;;; Tests for the protocol system

(defpackage :epsilon.test.protocol
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:proto :epsilon.protocol)
   (:map :epsilon.map)))

(in-package :epsilon.test.protocol)

;;; Test protocols

(proto:defprotocol showable
  "Types that can be shown as strings"
  (show (obj) "Convert to string representation"))

(proto:defprotocol numeric
  "Types that support numeric operations"
  (add (a b) "Add two values")
  (mul (a b) "Multiply two values"))

(proto:defprotocol container
  "Types that hold values"
  (get-value (c) "Get the contained value")
  (map-value (f c) "Apply function to contained value"))

(proto:defprotocol measurable
  "Types with a size"
  (size (obj) "Get the size"))

(proto:defprotocol functor
  "Mappable containers"
  (fmap (f container) "Map function over container"))

(proto:defprotocol applicative
  "Applicative functors"
  (:extends functor)
  (pure (value hint) "Wrap in minimal context")
  (ap (ff fa) "Apply wrapped function"))

;;; Test type

(defstruct box value)

;;; Protocol implementations

(proto:extend-type list
  showable
  (show (xs) (format nil "(~{~A~^ ~})" xs))

  functor
  (fmap (xs f) (mapcar f xs))

  applicative
  (pure (hint x) (declare (ignore hint)) (list x))
  (ap (xs fs) (mapcan (lambda (f) (mapcar f xs)) fs)))

(proto:extend-type box
  showable
  (show (b) (format nil "[~A]" (box-value b)))

  container
  (get-value (b) (box-value b))
  (map-value (b f) (make-box :value (funcall f (box-value b)))))

(proto:extend-type number
  numeric
  (add (a b) (+ a b))
  (mul (a b) (* a b)))

(proto:extend-protocol measurable
  list
  (size (xs) (length xs))

  string
  (size (s) (length s))

  vector
  (size (v) (length v)))

;;; Protocol definition tests

(deftest test-defprotocol-creates-generic
  (is (fboundp 'show))
  (is (fboundp 'add))
  (is (fboundp 'mul)))

(deftest test-protocol-registered
  (is (proto:protocol-exists-p 'showable))
  (is (proto:protocol-exists-p 'numeric))
  (is (proto:protocol-exists-p 'container)))

(deftest test-protocol-methods
  (let* ((protocol (proto:find-protocol 'showable))
         (methods (proto:protocol-methods protocol)))
    (is (not (null methods)))
    (is (assoc 'show methods))))

(deftest test-protocol-documentation
  (let ((protocol (proto:find-protocol 'showable)))
    (is-equal "Types that can be shown as strings"
              (proto:protocol-documentation protocol))))

;;; define-protocol compatibility

(deftest test-define-protocol
  (proto:define-protocol legacy-protocol
    (:version "2.0")
    (:documentation "Legacy syntax test")
    (:method legacy-method (x) "A legacy method"))

  (is (proto:protocol-exists-p 'legacy-protocol))
  (let ((protocol (proto:find-protocol 'legacy-protocol)))
    (is-equal "2.0" (proto:protocol-version protocol))
    (is-equal "Legacy syntax test" (proto:protocol-documentation protocol))
    (is (member 'legacy-method (mapcar #'first (proto:protocol-methods protocol)))))
  (is (fboundp 'legacy-method)))

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

(deftest test-extend-protocol
  (is (= 3 (size '(1 2 3))))
  (is (= 5 (size "hello")))
  (is (= 4 (size #(a b c d)))))

;;; satisfies-p tests

(deftest test-satisfies-p-true
  (is (proto:satisfies-p '(1 2 3) 'showable))
  (is (proto:satisfies-p (make-box :value 1) 'showable))
  (is (proto:satisfies-p (make-box :value 1) 'container)))

(deftest test-satisfies-p-false
  (is (not (proto:satisfies-p '(1 2 3) 'container))))

;;; list-implementations tests

(deftest test-list-implementations
  (let ((impls (proto:list-implementations 'showable)))
    (is (member 'list impls))
    (is (member 'box impls))))

;;; Protocol inheritance tests

(deftest test-protocol-extends
  (let ((p (proto:find-protocol 'applicative)))
    (is (not (null p)))
    (is (equal '(functor) (proto:protocol-extends p)))))

(deftest test-functor-fmap
  (is (equal '(2 4 6) (fmap '(1 2 3) #'(lambda (x) (* x 2))))))

(deftest test-applicative-pure
  (is (equal '(42) (pure nil 42))))

(deftest test-applicative-ap
  (is (equal '(2 3 4 3 4 5) (ap '(1 2 3) (list #'1+ #'(lambda (x) (+ x 2)))))))

;;; Multiple protocols on same type

(deftest test-multiple-protocols-single-type
  (let ((b (make-box :value 10)))
    (is (stringp (show b)))
    (is (= 10 (get-value b)))
    (is (proto:satisfies-p b 'showable))
    (is (proto:satisfies-p b 'container))))
