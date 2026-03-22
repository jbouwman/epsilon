;;;; Tests for epsilon.annotation

(defpackage epsilon.annotation-tests
  (:use :cl :epsilon.test)
  (:require (epsilon.annotation ann))
  (:enter t))

;;; Basic binding and consumption

(deftest test-pending-annotations-default-nil
  "Pending annotations are NIL by default"
  (assert-nil ann:*pending-annotations*))

(deftest test-with-annotations-binds
  "with-annotations pushes a binding onto *pending-annotations*"
  (ann:with-annotations (:timeout 30)
    (assert-equal '((:timeout . 30)) ann:*pending-annotations*)))

(deftest test-with-annotations-nests
  "Multiple with-annotations compose via nesting"
  (ann:with-annotations (:category :integration)
    (ann:with-annotations (:timeout 30)
      (assert-equal 2 (length ann:*pending-annotations*))
      (assert-equal 30 (cdr (assoc :timeout ann:*pending-annotations*)))
      (assert-equal :integration (cdr (assoc :category ann:*pending-annotations*))))))

(deftest test-consume-annotations-returns-and-clears
  "consume-annotations returns pending and resets to NIL"
  (ann:with-annotations (:timeout 60)
    (let ((result (ann:consume-annotations)))
      (assert-equal '((:timeout . 60)) result)
      (assert-nil ann:*pending-annotations*))))

(deftest test-get-annotation
  "get-annotation looks up a key"
  (ann:with-annotations (:timeout 30)
    (assert-equal 30 (ann:get-annotation :timeout))
    (assert-nil (ann:get-annotation :missing))
    (assert-equal :default (ann:get-annotation :missing :default))))

;;; Symbol plist storage

(deftest test-set-and-get-annotations
  "annotations and set-annotations use symbol plist"
  (let ((sym (gensym "TEST")))
    (ann:set-annotations sym '((:timeout . 30) (:category . :unit)))
    (assert-equal '((:timeout . 30) (:category . :unit))
                  (ann:annotations sym))
    ;; Cleanup
    (remprop sym 'ann::annotations)))

;;; Reader macro round-trip

(deftest test-reader-annotation-pair
  "Reader #@(:key value) form produces with-annotations wrapping"
  (let* ((*package* (find-package :epsilon.annotation-tests))
         (form (read-from-string "#@(:timeout 30) (deftest foo)")))
    (assert-equal 'ann:with-annotations (first form))
    (assert-equal '(:timeout 30) (second form))
    (assert-equal '(deftest foo) (third form))))

(deftest test-reader-annotation-bare-keyword
  "Reader #@:keyword form produces with-annotations (:keyword t)"
  (let* ((*package* (find-package :epsilon.annotation-tests))
         (form (read-from-string "#@:parallel (deftest bar)")))
    (assert-equal 'ann:with-annotations (first form))
    (assert-equal '(:parallel t) (second form))
    (assert-equal '(deftest bar) (third form))))

(deftest test-reader-annotation-nesting
  "Multiple #@ lines nest correctly"
  (let* ((*package* (find-package :epsilon.annotation-tests))
         (form (read-from-string "#@(:category :integration) #@(:timeout 30) (deftest baz)")))
    ;; Outer: with-annotations (:category :integration)
    (assert-equal 'ann:with-annotations (first form))
    (assert-equal '(:category :integration) (second form))
    ;; Inner: with-annotations (:timeout 30)
    (let ((inner (third form)))
      (assert-equal 'ann:with-annotations (first inner))
      (assert-equal '(:timeout 30) (second inner))
      (assert-equal '(deftest baz) (third inner)))))
