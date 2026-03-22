(defpackage epsilon.option-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.option opt)
            (epsilon.sequence seq))
  (:enter t))

;;; Constructor tests

(deftest test-some-basic
  (let ((s (opt:some 42)))
    (assert-true (opt:some-p s))
    (assert-true (not (opt:none-p s)))
    (assert-true (opt:option-p s))))

(deftest test-some-rejects-nil
  (assert-condition (error) (opt:some nil)))

(deftest test-none-basic
  (let ((n (opt:none)))
    (assert-true (opt:none-p n))
    (assert-true (not (opt:some-p n)))
    (assert-true (opt:option-p n))))

(deftest test-none-singleton
  (assert-true (eq (opt:none) (opt:none))))

(deftest test-option-constructor
  (assert-true (opt:some-p (opt:option 42)))
  (assert-true (opt:none-p (opt:option nil))))

;;; Predicate tests

(deftest test-predicates
  (assert-true (opt:some-p (opt:some 1)))
  (assert-true (not (opt:some-p (opt:none))))
  (assert-true (not (opt:some-p 42)))
  (assert-true (opt:none-p (opt:none)))
  (assert-true (not (opt:none-p (opt:some 1))))
  (assert-true (not (opt:none-p nil)))
  (assert-true (opt:option-p (opt:some 1)))
  (assert-true (opt:option-p (opt:none)))
  (assert-true (not (opt:option-p 42)))
  (assert-true (not (opt:option-p nil))))

;;; Accessor tests

(deftest test-unwrap-some
  (assert-true (= 42 (opt:unwrap (opt:some 42)))))

(deftest test-unwrap-none-errors
  (assert-condition (error) (opt:unwrap (opt:none))))

(deftest test-expect-some
  (assert-true (= 42 (opt:expect (opt:some 42) "should not fail"))))

(deftest test-expect-none-errors-with-message
  (assert-condition (error)
    (opt:expect (opt:none) "custom message")))

(deftest test-unwrap-or
  (assert-true (= 42 (opt:unwrap-or (opt:some 42) 0)))
  (assert-true (= 0 (opt:unwrap-or (opt:none) 0))))

(deftest test-unwrap-or-else
  (assert-true (= 42 (opt:unwrap-or-else (opt:some 42)
                                (lambda () (error "should not be called")))))
  (let ((called nil))
    (assert-true (= 99 (opt:unwrap-or-else (opt:none)
                                  (lambda ()
                                    (setf called t)
                                    99))))
    (assert-true called)))

;;; Transformation tests

(deftest test-map-some
  (let ((result (opt:map #'1+ (opt:some 1))))
    (assert-true (opt:some-p result))
    (assert-true (= 2 (opt:unwrap result)))))

(deftest test-map-none
  (assert-true (opt:none-p (opt:map #'1+ (opt:none)))))

(deftest test-map-chain
  (let ((result (opt:map #'1+
                         (opt:map #'1+
                                  (opt:some 1)))))
    (assert-true (= 3 (opt:unwrap result)))))

(deftest test-flatmap-some-returns-some
  (let ((result (opt:flatmap (lambda (x) (opt:some (* x 2)))
                             (opt:some 5))))
    (assert-true (opt:some-p result))
    (assert-true (= 10 (opt:unwrap result)))))

(deftest test-flatmap-some-returns-none
  (let ((result (opt:flatmap (lambda (x)
                               (declare (ignore x))
                               (opt:none))
                             (opt:some 5))))
    (assert-true (opt:none-p result))))

(deftest test-flatmap-none
  (let ((called nil))
    (let ((result (opt:flatmap (lambda (x)
                                 (setf called t)
                                 (opt:some x))
                               (opt:none))))
      (assert-true (opt:none-p result))
      (assert-true (not called)))))

(deftest test-flatmap-requires-option-return
  (assert-condition (error) (opt:flatmap (lambda (x) x) (opt:some 5))))

(deftest test-filter-matches
  (assert-true (opt:some-p (opt:filter #'evenp (opt:some 2))))
  (assert-true (= 2 (opt:unwrap (opt:filter #'evenp (opt:some 2))))))

(deftest test-filter-no-match
  (assert-true (opt:none-p (opt:filter #'evenp (opt:some 3)))))

(deftest test-filter-none
  (assert-true (opt:none-p (opt:filter #'evenp (opt:none)))))

(deftest test-flatten-some-some
  (let ((result (opt:flatten (opt:some (opt:some 42)))))
    (assert-true (opt:some-p result))
    (assert-true (= 42 (opt:unwrap result)))))

(deftest test-flatten-some-none
  (assert-true (opt:none-p (opt:flatten (opt:some (opt:none))))))

(deftest test-flatten-none
  (assert-true (opt:none-p (opt:flatten (opt:none)))))

(deftest test-flatten-requires-nested-option
  (assert-condition (error) (opt:flatten (opt:some 42))))

;;; Combinator tests

(deftest test-and-then
  ;; Same as flatmap
  (let ((result (opt:and-then (opt:some 5)
                              (lambda (x) (opt:some (* x 2))))))
    (assert-true (= 10 (opt:unwrap result)))))

(deftest test-or-else-some
  (let ((result (opt:or-else (opt:some 1) (opt:some 99))))
    (assert-true (= 1 (opt:unwrap result)))))

(deftest test-or-else-none
  (let ((result (opt:or-else (opt:none) (opt:some 99))))
    (assert-true (= 99 (opt:unwrap result)))))

(deftest test-zip-both-some
  (let ((result (opt:zip (opt:some 1) (opt:some 2))))
    (assert-true (opt:some-p result))
    (assert-true (equal '(1 2) (opt:unwrap result)))))

(deftest test-zip-first-none
  (assert-true (opt:none-p (opt:zip (opt:none) (opt:some 2)))))

(deftest test-zip-second-none
  (assert-true (opt:none-p (opt:zip (opt:some 1) (opt:none)))))

(deftest test-zip-with-both-some
  (let ((result (opt:zip-with #'+ (opt:some 1) (opt:some 2))))
    (assert-true (opt:some-p result))
    (assert-true (= 3 (opt:unwrap result)))))

(deftest test-zip-with-any-none
  (assert-true (opt:none-p (opt:zip-with #'+ (opt:none) (opt:some 2))))
  (assert-true (opt:none-p (opt:zip-with #'+ (opt:some 1) (opt:none)))))

;;; Conversion tests

(deftest test-to-list-some
  (assert-true (equal '(42) (opt:to-list (opt:some 42)))))

(deftest test-to-list-none
  (assert-true (null (opt:to-list (opt:none)))))

(deftest test-to-seq-some
  (let ((s (opt:to-seq (opt:some 42))))
    (assert-true (seq:sequence-p s))
    (assert-true (not (seq:empty-p s)))
    (assert-true (= 42 (seq:first s)))))

(deftest test-to-seq-none
  (let ((s (opt:to-seq (opt:none))))
    (assert-true (seq:sequence-p s))
    (assert-true (seq:empty-p s))))

(deftest test-from-nullable
  (assert-true (opt:some-p (opt:from-nullable 42)))
  (assert-true (opt:none-p (opt:from-nullable nil))))

;;; Print representation test

(deftest test-print-some
  (let ((str (format nil "~A" (opt:some 42))))
    (assert-true (search "SOME" str))
    (assert-true (search "42" str))))

;;; Integration tests

(deftest test-option-chain
  ;; Simulate a chain of operations that might fail
  (flet ((safe-divide (x y)
           (if (zerop y)
               (opt:none)
               (opt:some (/ x y))))
         (safe-sqrt (x)
           (if (minusp x)
               (opt:none)
               (opt:some (sqrt x)))))
    ;; Successful chain
    (let ((result (opt:flatmap #'safe-sqrt
                               (safe-divide 16 4))))
      (assert-true (opt:some-p result))
      (assert-true (= 2.0 (opt:unwrap result))))
    ;; Division by zero
    (assert-true (opt:none-p (opt:flatmap #'safe-sqrt
                                 (safe-divide 16 0))))
    ;; Negative sqrt
    (assert-true (opt:none-p (opt:flatmap #'safe-sqrt
                                 (safe-divide -16 4))))))
