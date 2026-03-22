;;;; Tests for the type class system

(defpackage epsilon.typeclass-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.typeclass tc))
  (:enter t))

;;; ============================================================
;;; Basic type class definition
;;; ============================================================

(tc:deftypeclass showable ()
  "Types with a string representation."
  (show-value (object) "Return a string representation."))

(tc:deftypeclass eq-class ()
  "Types with equality."
  (equal-p (a b) "Return T if A and B are equal."))

(tc:deftypeclass ord (eq-class)
  "Totally ordered types."
  (compare (a b) "Return :lt, :eq, or :gt."))

;;; Type class with default method

(tc:deftypeclass printable ()
  "Types that can be printed to a stream."
  (print-to (object stream) "Print OBJECT to STREAM."
   :default (format stream "~A" (show-value object))))

;;; ============================================================
;;; Instances for built-in types
;;; ============================================================

(tc:definstance eq-class number
  (equal-p (a b) (= a b)))

(tc:definstance showable number
  (show-value (n) (princ-to-string n)))

(tc:definstance ord number
  (compare (a b)
    (cond ((< a b) :lt)
          ((= a b) :eq)
          (t :gt))))

(tc:definstance eq-class string
  (equal-p (a b) (string= a b)))

(tc:definstance showable string
  (show-value (s) s))

(tc:definstance ord string
  (compare (a b)
    (cond ((string< a b) :lt)
          ((string= a b) :eq)
          (t :gt))))

(tc:definstance showable list
  (show-value (xs) (format nil "(~{~A~^ ~})" (mapcar #'show-value xs))))

;;; ============================================================
;;; Type class definition tests
;;; ============================================================

(deftest test-deftypeclass-creates-generics
  "deftypeclass should create generic functions for each method"
  (assert-true (fboundp 'show-value))
  (assert-true (fboundp 'equal-p))
  (assert-true (fboundp 'compare)))

(deftest test-deftypeclass-registers-metadata
  "deftypeclass should register type class info"
  (let ((tc (tc:find-typeclass 'showable)))
    (assert-true (not (null tc)))
    (assert-true (equal '(show-value) (tc:typeclass-methods 'showable)))))

(deftest test-deftypeclass-superclasses
  "deftypeclass should record superclass requirements"
  (assert-true (equal '(eq-class) (tc:typeclass-superclasses 'ord))))

;;; ============================================================
;;; Instance tests
;;; ============================================================

(deftest test-definstance-basic
  "definstance should create working methods"
  (assert-true (equal "42" (show-value 42)))
  (assert-true (equal "hello" (show-value "hello")))
  (assert-true (equal "(1 2 3)" (show-value '(1 2 3)))))

(deftest test-definstance-equality
  "eq-class instances should provide equality"
  (assert-true (equal-p 42 42))
  (assert-true (not (equal-p 42 43)))
  (assert-true (equal-p "foo" "foo"))
  (assert-true (not (equal-p "foo" "bar"))))

(deftest test-definstance-ord
  "ord instances should provide ordering"
  (assert-true (eq :lt (compare 1 2)))
  (assert-true (eq :eq (compare 5 5)))
  (assert-true (eq :gt (compare 10 3)))
  (assert-true (eq :lt (compare "apple" "banana")))
  (assert-true (eq :eq (compare "same" "same")))
  (assert-true (eq :gt (compare "zebra" "alpha"))))

(deftest test-definstance-superclass-enforcement
  "definstance should require superclass instances"
  ;; ord requires eq-class. number and string already have eq-class.
  ;; The instances above should have succeeded.
  (let ((tc (tc:find-typeclass 'ord)))
    (assert-true (not (null tc)))))

(deftest test-definstance-default-method
  "Type classes with defaults should use the default when not overridden"
  ;; printable has a default that calls show-value.
  ;; Since number has show-value, printable's default should work.
  (let ((output (with-output-to-string (s)
                  (print-to 42 s))))
    (assert-true (equal "42" output))))

(deftest test-definstance-registers
  "definstance should register instances for introspection"
  (assert-true (not (null (tc:find-instance 'showable 'number))))
  (assert-true (not (null (tc:find-instance 'showable 'string))))
  (assert-true (not (null (tc:find-instance 'eq-class 'number))))
  (assert-true (null (tc:find-instance 'showable 'hash-table))))

(deftest test-typeclass-instances-list
  "typeclass-instances should list all registered types"
  (let ((instances (tc:typeclass-instances 'showable)))
    (assert-true (member 'number instances))
    (assert-true (member 'string instances))
    (assert-true (member 'list instances))))

;;; ============================================================
;;; EQL specializer tests
;;; ============================================================

(tc:deftypeclass format-handler ()
  "Format-specific handling."
  (format-encode (format object) "Encode object for format.")
  (format-name (format) "Human-readable format name."))

(tc:definstance format-handler (eql :text)
  (format-encode (fmt obj)
    (declare (ignore fmt))
    (princ-to-string obj))
  (format-name (fmt)
    (declare (ignore fmt))
    "Plain Text"))

(tc:definstance format-handler (eql :json)
  (format-encode (fmt obj)
    (declare (ignore fmt))
    (format nil "{\"value\":~S}" obj))
  (format-name (fmt)
    (declare (ignore fmt))
    "JSON"))

(deftest test-eql-specializer-dispatch
  "definstance with eql specializers should dispatch on format keyword"
  (assert-true (equal "42" (format-encode :text 42)))
  (assert-true (equal "{\"value\":42}" (format-encode :json 42)))
  (assert-true (equal "Plain Text" (format-name :text)))
  (assert-true (equal "JSON" (format-name :json))))

;;; ============================================================
;;; Error condition tests
;;; ============================================================

(deftest test-unknown-typeclass-error
  "definstance for unknown type class should signal error"
  (assert-true (handler-case
          (progn
            (tc:check-instance-completeness 'nonexistent-tc 'string '(foo))
            nil)
        (error () t))))

(deftest test-missing-method-error
  "definstance missing a required method should signal error"
  ;; showable requires show-value. Passing empty method list should error.
  (assert-true (handler-case
          (progn
            (tc:check-instance-completeness 'showable 'hash-table '())
            nil)
        (error () t))))

;;; ============================================================
;;; extend-type sugar macro tests
;;; ============================================================

;; Define type classes for sugar macro testing

(tc:deftypeclass describable ()
  "Types with a description."
  (describe-value (object) "Return a description string."))

(tc:deftypeclass measurable ()
  "Types with a size."
  (measure (object) "Return a numeric measure."))

;; Test struct for extend-type tests
(defstruct widget name weight)

;; Single typeclass via extend-type
(tc:extend-type widget
  describable
  (describe-value (w) (format nil "widget:~A" (widget-name w))))

(deftest test-extend-type-single-typeclass
  "extend-type with a single typeclass should create working methods"
  (let ((w (make-widget :name "gear" :weight 5)))
    (assert-equal "widget:gear" (describe-value w))))

;; Multiple typeclasses via extend-type
(defstruct gadget label size)

(tc:extend-type gadget
  describable
  (describe-value (g) (format nil "gadget:~A" (gadget-label g)))

  measurable
  (measure (g) (gadget-size g)))

(deftest test-extend-type-multiple-typeclasses
  "extend-type with multiple typeclasses should implement all of them"
  (let ((g (make-gadget :label "sprocket" :size 42)))
    (assert-equal "gadget:sprocket" (describe-value g))
    (assert-= 42 (measure g))))

(deftest test-extend-type-registers-instances
  "extend-type should register instances in the typeclass registry"
  (assert-not-null (tc:find-instance 'describable 'widget))
  (assert-not-null (tc:find-instance 'describable 'gadget))
  (assert-not-null (tc:find-instance 'measurable 'gadget)))

;;; ============================================================
;;; extend-typeclass sugar macro tests
;;; ============================================================

(tc:deftypeclass sizable ()
  "Types with a size."
  (get-size (object) "Return the size."))

(tc:extend-typeclass sizable
  list
  (get-size (xs) (length xs))

  vector
  (get-size (v) (length v))

  string
  (get-size (s) (length s)))

(deftest test-extend-typeclass-multiple-types
  "extend-typeclass should implement one typeclass for multiple types"
  (assert-= 3 (get-size '(a b c)))
  (assert-= 4 (get-size #(1 2 3 4)))
  (assert-= 5 (get-size "hello")))

(deftest test-extend-typeclass-registers-instances
  "extend-typeclass should register all type instances"
  (assert-not-null (tc:find-instance 'sizable 'list))
  (assert-not-null (tc:find-instance 'sizable 'vector))
  (assert-not-null (tc:find-instance 'sizable 'string)))
