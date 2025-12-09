(defpackage #:epsilon.option.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:opt #:epsilon.option)
   (#:seq #:epsilon.sequence)))

(in-package #:epsilon.option.tests)

;;; Constructor tests

(deftest test-some-basic
  (let ((s (opt:some 42)))
    (is (opt:some-p s))
    (is (not (opt:none-p s)))
    (is (opt:option-p s))))

(deftest test-some-rejects-nil
  (is-thrown (error) (opt:some nil)))

(deftest test-none-basic
  (let ((n (opt:none)))
    (is (opt:none-p n))
    (is (not (opt:some-p n)))
    (is (opt:option-p n))))

(deftest test-none-singleton
  (is (eq (opt:none) (opt:none))))

(deftest test-option-constructor
  (is (opt:some-p (opt:option 42)))
  (is (opt:none-p (opt:option nil))))

;;; Predicate tests

(deftest test-predicates
  (is (opt:some-p (opt:some 1)))
  (is (not (opt:some-p (opt:none))))
  (is (not (opt:some-p 42)))
  (is (opt:none-p (opt:none)))
  (is (not (opt:none-p (opt:some 1))))
  (is (not (opt:none-p nil)))
  (is (opt:option-p (opt:some 1)))
  (is (opt:option-p (opt:none)))
  (is (not (opt:option-p 42)))
  (is (not (opt:option-p nil))))

;;; Accessor tests

(deftest test-unwrap-some
  (is (= 42 (opt:unwrap (opt:some 42)))))

(deftest test-unwrap-none-errors
  (is-thrown (error) (opt:unwrap (opt:none))))

(deftest test-expect-some
  (is (= 42 (opt:expect (opt:some 42) "should not fail"))))

(deftest test-expect-none-errors-with-message
  (is-thrown (error "custom message")
    (opt:expect (opt:none) "custom message")))

(deftest test-unwrap-or
  (is (= 42 (opt:unwrap-or (opt:some 42) 0)))
  (is (= 0 (opt:unwrap-or (opt:none) 0))))

(deftest test-unwrap-or-else
  (is (= 42 (opt:unwrap-or-else (opt:some 42)
                                (lambda () (error "should not be called")))))
  (let ((called nil))
    (is (= 99 (opt:unwrap-or-else (opt:none)
                                  (lambda ()
                                    (setf called t)
                                    99))))
    (is called)))

;;; Transformation tests

(deftest test-map-some
  (let ((result (opt:map #'1+ (opt:some 1))))
    (is (opt:some-p result))
    (is (= 2 (opt:unwrap result)))))

(deftest test-map-none
  (is (opt:none-p (opt:map #'1+ (opt:none)))))

(deftest test-map-chain
  (let ((result (opt:map #'1+
                         (opt:map #'1+
                                  (opt:some 1)))))
    (is (= 3 (opt:unwrap result)))))

(deftest test-flatmap-some-returns-some
  (let ((result (opt:flatmap (lambda (x) (opt:some (* x 2)))
                             (opt:some 5))))
    (is (opt:some-p result))
    (is (= 10 (opt:unwrap result)))))

(deftest test-flatmap-some-returns-none
  (let ((result (opt:flatmap (lambda (x)
                               (declare (ignore x))
                               (opt:none))
                             (opt:some 5))))
    (is (opt:none-p result))))

(deftest test-flatmap-none
  (let ((called nil))
    (let ((result (opt:flatmap (lambda (x)
                                 (setf called t)
                                 (opt:some x))
                               (opt:none))))
      (is (opt:none-p result))
      (is (not called)))))

(deftest test-flatmap-requires-option-return
  (is-thrown (error) (opt:flatmap (lambda (x) x) (opt:some 5))))

(deftest test-filter-matches
  (is (opt:some-p (opt:filter #'evenp (opt:some 2))))
  (is (= 2 (opt:unwrap (opt:filter #'evenp (opt:some 2))))))

(deftest test-filter-no-match
  (is (opt:none-p (opt:filter #'evenp (opt:some 3)))))

(deftest test-filter-none
  (is (opt:none-p (opt:filter #'evenp (opt:none)))))

(deftest test-flatten-some-some
  (let ((result (opt:flatten (opt:some (opt:some 42)))))
    (is (opt:some-p result))
    (is (= 42 (opt:unwrap result)))))

(deftest test-flatten-some-none
  (is (opt:none-p (opt:flatten (opt:some (opt:none))))))

(deftest test-flatten-none
  (is (opt:none-p (opt:flatten (opt:none)))))

(deftest test-flatten-requires-nested-option
  (is-thrown (error) (opt:flatten (opt:some 42))))

;;; Combinator tests

(deftest test-and-then
  ;; Same as flatmap
  (let ((result (opt:and-then (opt:some 5)
                              (lambda (x) (opt:some (* x 2))))))
    (is (= 10 (opt:unwrap result)))))

(deftest test-or-else-some
  (let ((result (opt:or-else (opt:some 1) (opt:some 99))))
    (is (= 1 (opt:unwrap result)))))

(deftest test-or-else-none
  (let ((result (opt:or-else (opt:none) (opt:some 99))))
    (is (= 99 (opt:unwrap result)))))

(deftest test-zip-both-some
  (let ((result (opt:zip (opt:some 1) (opt:some 2))))
    (is (opt:some-p result))
    (is (equal '(1 2) (opt:unwrap result)))))

(deftest test-zip-first-none
  (is (opt:none-p (opt:zip (opt:none) (opt:some 2)))))

(deftest test-zip-second-none
  (is (opt:none-p (opt:zip (opt:some 1) (opt:none)))))

(deftest test-zip-with-both-some
  (let ((result (opt:zip-with #'+ (opt:some 1) (opt:some 2))))
    (is (opt:some-p result))
    (is (= 3 (opt:unwrap result)))))

(deftest test-zip-with-any-none
  (is (opt:none-p (opt:zip-with #'+ (opt:none) (opt:some 2))))
  (is (opt:none-p (opt:zip-with #'+ (opt:some 1) (opt:none)))))

;;; Conversion tests

(deftest test-to-list-some
  (is (equal '(42) (opt:to-list (opt:some 42)))))

(deftest test-to-list-none
  (is (null (opt:to-list (opt:none)))))

(deftest test-to-seq-some
  (let ((s (opt:to-seq (opt:some 42))))
    (is (seq:sequence-p s))
    (is (not (seq:empty-p s)))
    (is (= 42 (seq:first s)))))

(deftest test-to-seq-none
  (let ((s (opt:to-seq (opt:none))))
    (is (seq:sequence-p s))
    (is (seq:empty-p s))))

(deftest test-from-nullable
  (is (opt:some-p (opt:from-nullable 42)))
  (is (opt:none-p (opt:from-nullable nil))))

;;; Print representation test

(deftest test-print-some
  (let ((str (format nil "~A" (opt:some 42))))
    (is (search "SOME" str))
    (is (search "42" str))))

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
      (is (opt:some-p result))
      (is (= 2.0 (opt:unwrap result))))
    ;; Division by zero
    (is (opt:none-p (opt:flatmap #'safe-sqrt
                                 (safe-divide 16 0))))
    ;; Negative sqrt
    (is (opt:none-p (opt:flatmap #'safe-sqrt
                                 (safe-divide -16 4))))))
