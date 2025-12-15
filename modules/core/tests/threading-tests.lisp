(defpackage #:epsilon.threading.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:th #:epsilon.threading)))

(in-package #:epsilon.threading.tests)

;;; Helper functions for testing

(defun add1 (x) (+ x 1))
(defun mul2 (x) (* x 2))
(defun add (x y) (+ x y))
(defun sub (x y) (- x y))
(defun make-list-3 (a b c) (list a b c))

;;; -> tests

(deftest test-thread-first-single-symbol
  (is (= 2 (th:-> 1 add1)))
  (is (= 4 (th:-> 2 mul2))))

(deftest test-thread-first-single-form
  (is (= 3 (th:-> 1 (add 2))))
  (is (= -1 (th:-> 1 (sub 2)))))  ; (sub 1 2) = -1

(deftest test-thread-first-multiple
  (is (= 4 (th:-> 1 add1 mul2)))       ; (* (+ 1 1) 2) = 4
  (is (= 6 (th:-> 1 (add 2) mul2))))   ; (* (+ 1 2) 2) = 6

(deftest test-thread-first-no-forms
  (is (= 42 (th:-> 42))))

(deftest test-thread-first-mixed
  ;; (-> 10 (- 3) (/ 2)) => (/ (- 10 3) 2) = 3.5
  (is (= 7/2 (th:-> 10 (- 3) (/ 2)))))

;;; ->> tests

(deftest test-thread-last-single-symbol
  (is (= 2 (th:->> 1 add1)))
  (is (= 4 (th:->> 2 mul2))))

(deftest test-thread-last-single-form
  (is (= 3 (th:->> 1 (add 2))))    ; (add 2 1) = 3
  (is (= 1 (th:->> 1 (sub 2)))))   ; (sub 2 1) = 1

(deftest test-thread-last-multiple
  (is (= 4 (th:->> 1 add1 mul2))))

(deftest test-thread-last-no-forms
  (is (= 42 (th:->> 42))))

(deftest test-thread-last-position-matters
  ;; (-> 10 (- 3)) = (- 10 3) = 7
  ;; (->> 10 (- 3)) = (- 3 10) = -7
  (is (= 7 (th:-> 10 (- 3))))
  (is (= -7 (th:->> 10 (- 3)))))

;;; as-> tests

(deftest test-as-thread-basic
  (is (= 42 (th:as-> 42 $))))

(deftest test-as-thread-single-form
  (is (= 2 (th:as-> 1 $ (+ $ 1)))))

(deftest test-as-thread-multiple-positions
  ;; Can place $ anywhere in the form
  (is (= 3 (th:as-> 1 $
             (+ $ 1)    ; $ = 1, result = 2
             (+ $ 1)))) ; $ = 2, result = 3
  ;; Mixed positions
  (is (equal '(2 1 3)
             (th:as-> 1 x
               (make-list-3 (1+ x) x (+ x 2))))))

(deftest test-as-thread-arbitrary-symbol
  (is (= 10 (th:as-> 5 value
              (+ value value)))))

;;; some-> tests

(deftest test-some-thread-nil-start
  (is (null (th:some-> nil add1))))

(deftest test-some-thread-non-nil
  (is (= 2 (th:some-> 1 add1))))

(deftest test-some-thread-short-circuit
  (let ((called nil))
    (flet ((set-called (x)
             (setf called t)
             x))
      (is (null (th:some-> nil add1 set-called)))
      (is (not called)))))

(deftest test-some-thread-chain
  ;; Returns nil when intermediate is nil
  (is (null (th:some-> '(:a 1 :b 2)
                       (getf :c)    ; Returns nil
                       add1)))
  ;; Works when all non-nil
  (is (= 2 (th:some-> '(:a 1 :b 2)
                      (getf :a)
                      add1))))

;;; some->> tests

(deftest test-some-thread-last-nil
  (is (null (th:some->> nil add1))))

(deftest test-some-thread-last-non-nil
  (is (= 2 (th:some->> 1 add1))))

(deftest test-some-thread-last-short-circuit
  (let ((called nil))
    (flet ((maybe-nil (x)
             (declare (ignore x))
             nil)
           (set-called (x)
             (setf called t)
             x))
      (is (null (th:some->> 1 maybe-nil set-called)))
      (is (not called)))))

;;; cond-> tests

(deftest test-cond-thread-no-clauses
  (is (= 42 (th:cond-> 42))))

(deftest test-cond-thread-true-clause
  (is (= 2 (th:cond-> 1
             (t (add 1))))))

(deftest test-cond-thread-false-clause
  (is (= 1 (th:cond-> 1
             (nil (add 1))))))

(deftest test-cond-thread-mixed-clauses
  (is (= 3 (th:cond-> 1
             (t (add 1))       ; Applied: 1 + 1 = 2
             (nil (add 100))   ; Skipped
             (t (add 1))))))   ; Applied: 2 + 1 = 3

(deftest test-cond-thread-dynamic-tests
  (let ((apply-discount t)
        (is-member nil))
    (is (= 90 (th:cond-> 100
                (apply-discount (- 10))    ; Applied: 100 - 10 = 90
                (is-member (- 5)))))))     ; Skipped

;;; cond->> tests

(deftest test-cond-thread-last-no-clauses
  (is (= 42 (th:cond->> 42))))

(deftest test-cond-thread-last-true-clause
  ;; (cond->> 10 (t (- 3))) => (- 3 10) = -7
  (is (= -7 (th:cond->> 10
              (t (- 3))))))

(deftest test-cond-thread-last-false-clause
  (is (= 10 (th:cond->> 10
              (nil (- 3))))))

(deftest test-cond-thread-last-mixed
  ;; First clause: (- 20 10) = 10, second skipped
  (is (= 10 (th:cond->> 10
              (t (- 20))
              (nil (- 5))))))

;;; Integration tests

(deftest test-nested-threading
  ;; Can nest threading macros
  (is (= 6 (th:-> 1
                 (+ (th:-> 2 add1))   ; 1 + 3 = 4
                 (+ 2)))))            ; 4 + 2 = 6

(deftest test-threading-with-lambda
  ;; Use as-> when you need the value in non-first position
  (is (= 4 (th:as-> 2 x
             (funcall (lambda (y) (* y y)) x)))))
