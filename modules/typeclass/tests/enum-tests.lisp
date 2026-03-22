;;;; epsilon.enum tests

(defpackage epsilon.enum-test
  (:use :cl :epsilon.test :epsilon.syntax :epsilon.enum)
  (:enter t))

;;; Define test enums

(defenum test-color (:red :green :blue))

(defenum pitch-class (:c :cs :d :ds :e :f :fs :g :gs :a :as :b))

;;; Tests

(deftest defenum-creates-predicate ()
  (assert-true (test-color-p :red))
  (assert-true (test-color-p :green))
  (assert-true (test-color-p :blue))
  (assert-true (not (test-color-p :yellow)))
  (assert-true (not (test-color-p "red")))
  (assert-true (not (test-color-p 42))))

(deftest defenum-creates-values-accessor ()
  (assert-true (equal (test-color-values) '(:red :green :blue)))
  (assert-true (= 12 (length (pitch-class-values)))))

(deftest defenum-from-string ()
  (assert-true (eq :red (test-color-from-string "red")))
  (assert-true (eq :red (test-color-from-string "RED")))
  (assert-true (eq :red (test-color-from-string "Red")))
  (assert-true (null (test-color-from-string "yellow"))))

(deftest defenum-to-string ()
  (assert-true (string= "red" (test-color-to-string :red)))
  (assert-true (string= "green" (test-color-to-string :green)))
  (assert-true (null (test-color-to-string :yellow))))

(deftest generic-enum-p ()
  (assert-true (enum-p 'test-color :red))
  (assert-true (not (enum-p 'test-color :yellow)))
  (assert-true (enum-p 'pitch-class :c))
  (assert-true (enum-p 'pitch-class :fs)))

(deftest generic-enum-from-string ()
  (assert-true (eq :c (enum-from-string 'pitch-class "c")))
  (assert-true (eq :fs (enum-from-string 'pitch-class "FS")))
  (assert-true (null (enum-from-string 'pitch-class "invalid"))))

(deftest type-checking ()
  (assert-true (typep :red 'test-color))
  (assert-true (typep :c 'pitch-class))
  (assert-true (not (typep :invalid 'test-color))))
