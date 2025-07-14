(defpackage #:epsilon.lib.sequence.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string)))

(in-package #:epsilon.lib.sequence.tests)

(deftest test-sequence-creation
  (is (seq:sequence-p (seq:seq '(1 2 3))))
  (is (seq:sequence-p (seq:seq #(1 2 3))))
  (is (seq:sequence-p (seq:seq nil)))
  (is (seq:empty-p (seq:seq nil)))
  (is (not (seq:empty-p (seq:seq '(1))))))

(deftest test-sequence-realization
  (let ((s (seq:seq '(1 2 3))))
    (is (= 1 (seq:first s)))
    (is (= 2 (seq:first (seq:rest s))))
    (is (= 3 (seq:first (seq:rest (seq:rest s)))))
    (is (seq:empty-p (seq:rest (seq:rest (seq:rest s)))))))

(deftest test-realize
  (is (equal '(1 2 3) (seq:realize (seq:seq '(1 2 3)))))
  (is (equal '() (seq:realize (seq:seq nil)))))

(deftest test-map
  (is (equal '(2 3 4) (seq:realize (seq:map (lambda (x) (1+ x)) (seq:seq '(1 2 3))))))
  (is (equal '(5 7 9) (seq:realize (seq:map #'+ (seq:seq '(1 2 3)) (seq:seq '(4 5 6))))))
  (is (seq:empty-p (seq:map #'+ (seq:seq '()) (seq:seq '(1 2 3))))))

(deftest test-drop
  (is (equal '(3) (seq:realize (seq:drop 2 (seq:seq '(1 2 3))))))
  (is (equal '() (seq:realize (seq:drop 4 (seq:seq '(1 2 3))))))
  (is (seq:empty-p (seq:drop 1 (seq:seq '(1))))))

(deftest test-filter
  (is (equal '(1 3 5) (seq:realize (seq:filter #'oddp (seq:seq '(1 2 3 4 5 6))))))
  (is (equal '(2 4 6) (seq:realize (seq:filter #'evenp (seq:seq '(1 2 3 4 5 6))))))
  (is (seq:empty-p (seq:filter (constantly nil) (seq:seq '(1 2 3))))))

(deftest test-split
  (is (equal '("hello" "world") (seq:realize (str:split #\Space "hello world"))))
  (is (equal '("a" "b" "c") (seq:realize (str:split #\, "a,b,c"))))
  (is (equal '("abc") (seq:realize (str:split #\, "abc"))))
  (is (equal '("" "") (seq:realize (str:split #\a "a"))))
  (is (equal '("") (seq:realize (str:split #\a "")))))

(deftest test-every-p
  ;; Test with all elements satisfying predicate
  (is (seq:every-p #'evenp (seq:seq '(2 4 6 8))))
  (is (seq:every-p #'oddp (seq:seq '(1 3 5 7))))
  
  ;; Test with some elements not satisfying predicate
  (is (not (seq:every-p #'evenp (seq:seq '(2 4 5 8)))))
  (is (not (seq:every-p #'oddp (seq:seq '(1 3 4 7)))))
  
  ;; Test with empty sequence (should return true)
  (is (seq:every-p #'evenp seq:*empty*))
  (is (seq:every-p (constantly nil) seq:*empty*))
  
  ;; Test with single element
  (is (seq:every-p #'evenp (seq:seq '(2))))
  (is (not (seq:every-p #'evenp (seq:seq '(3))))))

(deftest test-not-empty-p
  ;; Test with non-empty sequences
  (is (seq:not-empty-p (seq:seq '(1 2 3))))
  (is (seq:not-empty-p (seq:seq '(1))))
  
  ;; Test with empty sequence
  (is (not (seq:not-empty-p seq:*empty*)))
  (is (not (seq:not-empty-p (seq:seq '()))))
  (is (not (seq:not-empty-p (seq:seq nil)))))
