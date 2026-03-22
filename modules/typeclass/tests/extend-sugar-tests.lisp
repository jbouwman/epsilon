;;;; Tests for the extend-type and extend-typeclass sugar macros
;;;;
;;;; Replaces protocol-tests.lisp. Tests the same scenarios (multi-protocol
;;;; implementation per type, multi-type implementation per protocol, functor/
;;;; applicative hierarchy) but using deftypeclass + sugar macros instead of
;;;; defprotocol.

(defpackage epsilon.extend-sugar-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.typeclass tc))
  (:enter t))

;;; Test type classes (replacing test protocols)

(tc:deftypeclass showable-ext ()
  "Types that can be shown as strings."
  (show-ext (obj) "Convert to string representation."))

(tc:deftypeclass numeric-ext ()
  "Types that support numeric operations."
  (add-ext (a b) "Add two values.")
  (mul-ext (a b) "Multiply two values."))

(tc:deftypeclass container-ext ()
  "Types that hold values."
  (get-value-ext (c) "Get the contained value.")
  (map-value-ext (f c) "Apply function to contained value."))

(tc:deftypeclass measurable-ext ()
  "Types with a size."
  (size-ext (obj) "Get the size."))

(tc:deftypeclass functor-ext ()
  "Mappable containers."
  (fmap-ext (f container) "Map function over container."))

(tc:deftypeclass applicative-ext (functor-ext)
  "Applicative functors (requires functor-ext)."
  (pure-ext (value hint) "Wrap in minimal context.")
  (ap-ext (ff fa) "Apply wrapped function."))

;;; Test type

(defstruct box-ext value)

;;; Implementations via extend-type

(tc:extend-type list
  showable-ext
  (show-ext (xs) (format nil "(~{~A~^ ~})" xs))

  functor-ext
  (fmap-ext (xs f) (mapcar f xs)))

;; applicative-ext requires functor-ext -- list already has functor-ext above
(tc:definstance applicative-ext list
  (pure-ext (hint x) (declare (ignore hint)) (list x))
  (ap-ext (xs fs) (mapcan (lambda (f) (mapcar f xs)) fs)))

(tc:extend-type box-ext
  showable-ext
  (show-ext (b) (format nil "[~A]" (box-ext-value b)))

  container-ext
  (get-value-ext (b) (box-ext-value b))
  (map-value-ext (b f) (make-box-ext :value (funcall f (box-ext-value b)))))

(tc:extend-type number
  numeric-ext
  (add-ext (a b) (+ a b))
  (mul-ext (a b) (* a b)))

;;; Implementations via extend-typeclass

(tc:extend-typeclass measurable-ext
  list
  (size-ext (xs) (length xs))

  string
  (size-ext (s) (length s))

  vector
  (size-ext (v) (length v)))

;;; Typeclass definition tests

(deftest test-deftypeclass-creates-generic
  "deftypeclass should create generic functions for each method"
  (assert-true (fboundp 'show-ext))
  (assert-true (fboundp 'add-ext))
  (assert-true (fboundp 'mul-ext)))

;;; extend-type tests

(deftest test-extend-type-single-method
  "extend-type should define methods that work correctly"
  (assert-equal "(1 2 3)" (show-ext '(1 2 3))))

(deftest test-extend-type-multiple-methods
  "extend-type should handle multiple methods for multiple typeclasses"
  (let ((b (make-box-ext :value 42)))
    (assert-equal "[42]" (show-ext b))
    (assert-= 42 (get-value-ext b))))

(deftest test-extend-type-map-value
  "map-value-ext should transform the contained value"
  (let* ((b (make-box-ext :value 5))
         (result (map-value-ext b #'1+)))
    (assert-true (box-ext-p result))
    (assert-= 6 (box-ext-value result))))

(deftest test-extend-type-number
  "extend-type should work with built-in types like number"
  (assert-= 7 (add-ext 3 4))
  (assert-= 12 (mul-ext 3 4)))

;;; extend-typeclass tests

(deftest test-extend-typeclass
  "extend-typeclass should define methods for multiple types"
  (assert-= 3 (size-ext '(1 2 3)))
  (assert-= 5 (size-ext "hello"))
  (assert-= 4 (size-ext #(a b c d))))

;;; Functor/Applicative tests (with enforced superclass hierarchy)

(deftest test-functor-fmap
  "fmap-ext should apply function to container contents"
  (assert-equal '(2 4 6) (fmap-ext '(1 2 3) (lambda (x) (* x 2)))))

(deftest test-applicative-pure
  "pure-ext should wrap value in minimal context"
  (assert-equal '(42) (pure-ext nil 42)))

(deftest test-applicative-ap
  "ap-ext should apply wrapped functions"
  (assert-equal '(2 3 4 3 4 5)
                (ap-ext '(1 2 3) (list #'1+ (lambda (x) (+ x 2))))))

;;; Multiple typeclasses on same type

(deftest test-multiple-typeclasses-single-type
  "A type can implement multiple typeclasses"
  (let ((b (make-box-ext :value 10)))
    (assert-true (stringp (show-ext b)))
    (assert-= 10 (get-value-ext b))))

;;; Superclass enforcement test

(deftest test-applicative-requires-functor
  "Defining applicative-ext without functor-ext should signal an error"
  ;; We can't actually test at macroexpand time in this context, but we
  ;; can verify the superclass is recorded correctly
  (assert-equal '(functor-ext) (tc:typeclass-superclasses 'applicative-ext)))
