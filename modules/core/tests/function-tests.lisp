(defpackage #:epsilon.function.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:fn #:epsilon.function)))

(in-package #:epsilon.function.tests)

;;; Helper functions

(defun add1 (x) (+ x 1))
(defun mul2 (x) (* x 2))
(defun add (x y) (+ x y))

;;; Existing function tests (compose, partial)

(deftest test-compose-basic
  (is (= 4 (funcall (fn:compose #'mul2 #'add1) 1)))  ; (* (+ 1 1) 2)
  (is (= 3 (funcall (fn:compose #'add1 #'mul2) 1)))) ; (+ (* 1 2) 1)

(deftest test-compose-single
  (is (= 2 (funcall (fn:compose #'add1) 1))))

(deftest test-compose-many
  (let ((f (fn:compose #'add1 #'add1 #'add1)))
    (is (= 4 (funcall f 1)))))

(deftest test-partial-basic
  (let ((add5 (fn:partial #'+ 5)))
    (is (= 8 (funcall add5 3)))
    (is (= 15 (funcall add5 10)))))

(deftest test-partial-multiple-args
  (let ((make-list-with-prefix (fn:partial #'list 'prefix)))
    (is (equal '(prefix a b) (funcall make-list-with-prefix 'a 'b)))))

;;; Pipe tests

(deftest test-pipe-basic
  (is (= 4 (funcall (fn:pipe #'add1 #'mul2) 1)))  ; (* (+ 1 1) 2)
  (is (= 3 (funcall (fn:pipe #'mul2 #'add1) 1)))) ; (+ (* 1 2) 1)

(deftest test-pipe-empty
  (is (= 42 (funcall (fn:pipe) 42))))

(deftest test-pipe-single
  (is (= 2 (funcall (fn:pipe #'add1) 1))))

(deftest test-pipe-many
  (let ((f (fn:pipe #'add1 #'mul2 #'add1)))  ; (+ (* (+ x 1) 2) 1)
    (is (= 5 (funcall f 1)))   ; (+ (* 2 2) 1) = 5
    (is (= 11 (funcall f 4))))) ; (+ (* 5 2) 1) = 11

(deftest test-pipe-vs-compose
  ;; pipe is the reverse of compose
  (let ((pipe-fn (fn:pipe #'add1 #'mul2))
        (compose-fn (fn:compose #'mul2 #'add1)))
    (is (= (funcall pipe-fn 5)
           (funcall compose-fn 5)))))

;;; Juxt tests

(deftest test-juxt-basic
  ;; Note: (last '(1 2 3 4 5)) returns (5), not 5
  (is (equal '(1 (5) 5)
             (funcall (fn:juxt #'first #'last #'length) '(1 2 3 4 5)))))

(deftest test-juxt-single
  (is (equal '(2)
             (funcall (fn:juxt #'add1) 1))))

(deftest test-juxt-multiple-args
  (is (equal '(3 -1)
             (funcall (fn:juxt #'+ #'-) 1 2))))

(deftest test-juxt-returns-list
  (let ((result (funcall (fn:juxt #'evenp #'oddp) 2)))
    (is (listp result))
    (is (= 2 (length result)))
    (is (first result))      ; 2 is even
    (is (not (second result))))) ; 2 is not odd

;;; Complement* tests

(deftest test-complement-basic
  (is (funcall (fn:complement* #'evenp) 3))
  (is (not (funcall (fn:complement* #'evenp) 2))))

(deftest test-complement-with-multiple-args
  (is (funcall (fn:complement* #'<) 5 3))   ; (not (< 5 3)) = T
  (is (not (funcall (fn:complement* #'<) 3 5)))) ; (not (< 3 5)) = NIL

(deftest test-complement-double-negation
  (let ((double-neg (fn:complement* (fn:complement* #'evenp))))
    (is (funcall double-neg 2))
    (is (not (funcall double-neg 3)))))

;;; Constantly* tests

(deftest test-constantly-basic
  (let ((always-42 (fn:constantly* 42)))
    (is (= 42 (funcall always-42)))
    (is (= 42 (funcall always-42 1)))
    (is (= 42 (funcall always-42 1 2 3)))))

(deftest test-constantly-nil
  (let ((always-nil (fn:constantly* nil)))
    (is (null (funcall always-nil 1 2 3)))))

(deftest test-constantly-with-mapcar
  (is (equal '(42 42 42)
             (mapcar (fn:constantly* 42) '(a b c)))))

;;; Flip tests

(deftest test-flip-basic
  (is (= 7 (funcall (fn:flip #'-) 3 10)))  ; (- 10 3) = 7
  (is (equal '(b . a) (funcall (fn:flip #'cons) 'a 'b))))

(deftest test-flip-with-extra-args
  ;; flip only swaps first two args, rest pass through
  (is (equal '(b a c d)
             (funcall (fn:flip #'list) 'a 'b 'c 'd))))

(deftest test-flip-double
  ;; flip(flip(f)) = f
  (let ((double-flip (fn:flip (fn:flip #'cons))))
    (is (equal '(a . b) (funcall double-flip 'a 'b)))))

;;; Curry tests

(deftest test-curry-basic
  (let ((add5 (fn:curry #'+ 5)))
    (is (= 8 (funcall add5 3)))))

(deftest test-curry-multiple
  (let ((add-1-2 (fn:curry #'+ 1 2)))
    (is (= 6 (funcall add-1-2 3)))))

(deftest test-curry-is-partial
  ;; curry should be the same as partial
  (let ((curry-fn (fn:curry #'+ 1 2))
        (partial-fn (fn:partial #'+ 1 2)))
    (is (= (funcall curry-fn 3 4)
           (funcall partial-fn 3 4)))))

;;; Rcurry tests

(deftest test-rcurry-basic
  (is (= 9 (funcall (fn:rcurry #'- 1) 10))))  ; (- 10 1) = 9

(deftest test-rcurry-vs-curry
  ;; curry prepends, rcurry appends
  (let ((curry-fn (fn:curry #'- 1))    ; (- 1 x)
        (rcurry-fn (fn:rcurry #'- 1))) ; (- x 1)
    (is (= -9 (funcall curry-fn 10)))  ; (- 1 10) = -9
    (is (= 9 (funcall rcurry-fn 10))))) ; (- 10 1) = 9

(deftest test-rcurry-multiple
  (is (equal '(1 2 3)
             (funcall (fn:rcurry #'list 3) 1 2))))

(deftest test-rcurry-many-args
  (is (equal '(a b c d)
             (funcall (fn:rcurry #'list 'c 'd) 'a 'b))))

;;; Integration tests

(deftest test-pipe-with-partial
  (let ((process (fn:pipe (fn:partial #'+ 10)    ; add 10
                          (fn:partial #'* 2))))  ; multiply by 2
    (is (= 24 (funcall process 2)))))  ; (* (+ 2 10) 2) = 24

(deftest test-compose-juxt-filter
  ;; Use juxt to get multiple properties from a single value
  (let ((number-info (fn:juxt #'evenp #'plusp #'zerop)))
    (is (equal '(t t nil) (funcall number-info 4)))    ; 4: even, positive, not zero
    (is (equal '(t nil t) (funcall number-info 0)))    ; 0: even, not positive, is zero
    (is (equal '(nil t nil) (funcall number-info 3)))) ; 3: odd, positive, not zero
  ;; Compose with every to check all conditions
  (let ((positive-and-even-p (fn:compose (fn:partial #'every #'identity)
                                          (fn:juxt #'evenp #'plusp))))
    (is (funcall positive-and-even-p 4))      ; even and positive
    (is (not (funcall positive-and-even-p 3))) ; odd (not even)
    (is (not (funcall positive-and-even-p -4)))))

(deftest test-flip-with-rcurry
  ;; Combining flip and rcurry
  (let ((prepend-to-list (fn:flip (fn:rcurry #'cons))))
    ;; (flip (rcurry cons)) with args (list elem) = (cons elem list)
    (is (equal '(0 . (1 2 3))
               (funcall prepend-to-list '(1 2 3) 0)))))
