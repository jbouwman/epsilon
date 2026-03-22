;;;; Tests for defstruct+ with :deriving support

(defpackage epsilon.struct-plus-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.struct-plus sp)
            (epsilon.typeclass tc)
            (epsilon.typeclass.std tc-std))
  (:enter t))

;;; ============================================================
;;; Basic :deriving show
;;; ============================================================

(sp:defstruct+ color
  (:deriving show)
  (red 0 :type fixnum)
  (green 0 :type fixnum)
  (blue 0 :type fixnum))

(deftest test-defstruct+-basic-show
  "defstruct+ with :deriving show should generate show-value"
  (let ((c (make-color :red 255 :green 128 :blue 0)))
    (let ((s (tc-std:show-value c)))
      (assert-true (stringp s))
      (assert-true (search "COLOR" s))
      (assert-true (search "255" s))
      (assert-true (search "128" s)))))

;;; ============================================================
;;; Multiple derived typeclasses
;;; ============================================================

(sp:defstruct+ point2d
  (:deriving show eq-class hash-class ord)
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(deftest test-defstruct+-eq-class
  "defstruct+ with :deriving eq-class should generate equal-p"
  (let ((p1 (make-point2d :x 1 :y 2))
        (p2 (make-point2d :x 1 :y 2))
        (p3 (make-point2d :x 3 :y 4)))
    (assert-true (tc-std:equal-p p1 p2))
    (assert-not (tc-std:equal-p p1 p3))))

(deftest test-defstruct+-hash-class
  "defstruct+ with :deriving hash-class should generate hash-value"
  (let ((p1 (make-point2d :x 1 :y 2))
        (p2 (make-point2d :x 1 :y 2)))
    (assert-= (tc-std:hash-value p1) (tc-std:hash-value p2))))

(deftest test-defstruct+-ord
  "defstruct+ with :deriving ord should generate compare"
  (let ((p1 (make-point2d :x 1 :y 2))
        (p2 (make-point2d :x 1 :y 3))
        (p3 (make-point2d :x 1 :y 2)))
    (assert-eq :lt (tc-std:compare p1 p2))
    (assert-eq :eq (tc-std:compare p1 p3))
    (assert-eq :gt (tc-std:compare p2 p1))))

;;; ============================================================
;;; Custom :conc-name
;;; ============================================================

(sp:defstruct+ (rgb (:conc-name rgb-))
  (:deriving show)
  (red 0)
  (green 0)
  (blue 0))

(deftest test-defstruct+-custom-conc-name
  "defstruct+ with custom :conc-name should derive correctly"
  (let ((c (make-rgb :red 100 :green 200 :blue 50)))
    (assert-= 100 (rgb-red c))
    (assert-= 200 (rgb-green c))
    (let ((s (tc-std:show-value c)))
      (assert-true (stringp s))
      (assert-true (search "RGB" s)))))

;;; ============================================================
;;; Custom constructor
;;; ============================================================

(sp:defstruct+ (bounded-value (:constructor %make-bounded-value))
  (:deriving show eq-class)
  (value 0)
  (min-val 0)
  (max-val 100))

(defun make-bounded-value (&key (value 0) (min-val 0) (max-val 100))
  (let ((clamped (max min-val (min max-val value))))
    (%make-bounded-value :value clamped :min-val min-val :max-val max-val)))

(deftest test-defstruct+-custom-constructor
  "defstruct+ with custom constructor should still derive"
  (let ((v (make-bounded-value :value 150 :max-val 100)))
    (assert-= 100 (bounded-value-value v))
    (assert-true (stringp (tc-std:show-value v)))))

;;; ============================================================
;;; No :deriving (plain defstruct behavior)
;;; ============================================================

(sp:defstruct+ simple-box
  (width 0)
  (height 0))

(deftest test-defstruct+-no-deriving
  "defstruct+ without :deriving behaves like plain defstruct"
  (let ((b (make-simple-box :width 10 :height 20)))
    (assert-= 10 (simple-box-width b))
    (assert-= 20 (simple-box-height b))))

;;; ============================================================
;;; Instance registration
;;; ============================================================

(deftest test-defstruct+-find-instance
  "defstruct+ derived instances should be registered in the typeclass system"
  ;; color has show
  (assert-not-null (tc:find-instance 'tc-std:show 'color))
  ;; point2d: check show and hash-class via find-instance
  ;; (eq-class and ord are NOT checked here because typeclass-tests.lisp
  ;; re-defines those typeclasses, replacing the registry entry and wiping
  ;; instances. The functional tests above verify they work correctly.)
  (assert-not-null (tc:find-instance 'tc-std:show 'point2d))
  (assert-not-null (tc:find-instance 'tc-std:hash-class 'point2d))
  ;; simple-box has no deriving
  (assert-nil (tc:find-instance 'tc-std:show 'simple-box)))

;;; ============================================================
;;; Struct with doc string
;;; ============================================================

(sp:defstruct+ velocity
  "A 2D velocity vector."
  (:deriving show eq-class)
  (dx 0.0 :type single-float)
  (dy 0.0 :type single-float))

(deftest test-defstruct+-with-docstring
  "defstruct+ should handle doc strings correctly"
  (let ((v (make-velocity :dx 1.0 :dy 2.0)))
    (assert-true (tc-std:equal-p v (make-velocity :dx 1.0 :dy 2.0)))
    (assert-not (tc-std:equal-p v (make-velocity :dx 3.0 :dy 2.0)))))
