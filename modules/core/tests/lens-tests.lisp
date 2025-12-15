(defpackage #:epsilon.lens.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:lens #:epsilon.lens)
   (#:map #:epsilon.map)))

(in-package #:epsilon.lens.tests)

;;; Core operation tests

(deftest test-view-first
  (is (= 1 (lens:view (lens:first-lens) '(1 2 3)))))

(deftest test-view-second
  (is (= 2 (lens:view (lens:second-lens) '(1 2 3)))))

(deftest test-set-first
  (is (equal '(10 2 3)
             (lens:set (lens:first-lens) 10 '(1 2 3)))))

(deftest test-set-second
  (is (equal '(1 20 3)
             (lens:set (lens:second-lens) 20 '(1 2 3)))))

(deftest test-over-first
  (is (equal '(2 2 3)
             (lens:over (lens:first-lens) #'1+ '(1 2 3)))))

;;; nth-lens tests

(deftest test-nth-view
  (is (= 3 (lens:view (lens:nth-lens 2) '(1 2 3 4 5)))))

(deftest test-nth-set
  (is (equal '(1 2 30 4 5)
             (lens:set (lens:nth-lens 2) 30 '(1 2 3 4 5)))))

(deftest test-nth-over
  (is (equal '(1 2 6 4 5)
             (lens:over (lens:nth-lens 2) #'(lambda (x) (* x 2)) '(1 2 3 4 5)))))

;;; key-lens tests (for maps)

(deftest test-key-view
  (let ((m (map:assoc map:+empty+ :name "Alice")))
    (is (equal "Alice" (lens:view (lens:at :name) m)))))

(deftest test-key-view-default
  (let ((m map:+empty+))
    (is (equal "default" (lens:view (lens:at :name "default") m)))))

(deftest test-key-set
  (let ((m (map:assoc map:+empty+ :name "Alice")))
    (let ((result (lens:set (lens:at :name) "Bob" m)))
      (is (equal "Bob" (map:get result :name))))))

(deftest test-key-over
  (let ((m (map:assoc map:+empty+ :age 30)))
    (let ((result (lens:over (lens:at :age) #'1+ m)))
      (is (= 31 (map:get result :age))))))

;;; Composition tests

(deftest test-compose-two-lenses
  (let ((data '((1 2) (3 4) (5 6))))
    (let ((lens (lens:>>> (lens:first-lens) (lens:second-lens))))
      (is (= 2 (lens:view lens data))))))

(deftest test-compose-set
  (let ((data '((1 2) (3 4) (5 6))))
    (let* ((lens (lens:>>> (lens:first-lens) (lens:second-lens)))
           (result (lens:set lens 20 data)))
      (is (equal '((1 20) (3 4) (5 6)) result)))))

(deftest test-compose-over
  (let ((data '((1 2) (3 4) (5 6))))
    (let* ((lens (lens:>>> (lens:first-lens) (lens:second-lens)))
           (result (lens:over lens #'(lambda (x) (* x 10)) data)))
      (is (equal '((1 20) (3 4) (5 6)) result)))))

(deftest test-compose-three-lenses
  (let ((data '(((a b) c) d)))
    (let ((lens (lens:>>> (lens:first-lens) (lens:first-lens) (lens:second-lens))))
      (is (eq 'b (lens:view lens data))))))

(deftest test-compose-map-keys
  (let ((data (map:assoc
               (map:assoc map:+empty+ :outer
                          (map:assoc map:+empty+ :inner 42))
               :other "stuff")))
    (let ((lens (lens:>>> (lens:at :outer) (lens:at :inner))))
      (is (= 42 (lens:view lens data)))
      (is (= 100 (lens:view lens (lens:set lens 100 data)))))))

;;; <<< composition (right-to-left)

(deftest test-reverse-compose
  (let ((data '((1 2) (3 4))))
    (let ((lens (lens:<<< (lens:second-lens) (lens:first-lens))))
      ;; <<< reverses, so this is first then second
      (is (= 2 (lens:view lens data))))))

;;; each-lens tests

(deftest test-each-view
  (is (equal '(1 2 3) (lens:view (lens:each-lens) '(1 2 3)))))

(deftest test-each-over
  (is (equal '(2 3 4)
             (lens:over (lens:each-lens) #'1+ '(1 2 3)))))

(deftest test-each-set-error
  (is-thrown (error)
    (lens:set (lens:each-lens) 42 '(1 2 3))))

;;; filtered-lens tests

(deftest test-filtered-view
  (is (equal '(2 4 6)
             (lens:view (lens:filtered-lens #'evenp) '(1 2 3 4 5 6)))))

(deftest test-filtered-over
  (is (equal '(1 4 3 8 5 12)
             (lens:over (lens:filtered-lens #'evenp)
                        #'(lambda (x) (* x 2))
                        '(1 2 3 4 5 6)))))

;;; car/cdr lens tests

(deftest test-car-lens
  (is (= 1 (lens:view (lens:car-lens) '(1 . 2))))
  (is (equal '(10 . 2) (lens:set (lens:car-lens) 10 '(1 . 2)))))

(deftest test-cdr-lens
  (is (= 2 (lens:view (lens:cdr-lens) '(1 . 2))))
  (is (equal '(1 . 20) (lens:set (lens:cdr-lens) 20 '(1 . 2)))))

;;; identity-lens tests

(deftest test-identity-view
  (is (equal '(1 2 3) (lens:view (lens:identity-lens) '(1 2 3)))))

(deftest test-identity-set
  (is (equal '(a b c) (lens:set (lens:identity-lens) '(a b c) '(1 2 3)))))

;;; aref-lens tests

(deftest test-aref-view
  (is (= 5 (lens:view (lens:aref-lens 1) #(3 5 7)))))

(deftest test-aref-set
  (is (equalp #(3 50 7) (lens:set (lens:aref-lens 1) 50 #(3 5 7)))))

;;; zoom macro tests

(deftest test-zoom-single
  (let ((data (map:assoc map:+empty+ :name "alice")))
    (let ((result (lens:zoom (lens:at :name) data
                    string-upcase)))
      (is (equal "ALICE" (map:get result :name))))))

(deftest test-zoom-composed
  (let ((data (map:assoc map:+empty+ :user
                         (map:assoc map:+empty+ :name "bob"))))
    (let ((result (lens:zoom (lens:>>> (lens:at :user) (lens:at :name)) data
                    string-upcase)))
      (is (equal "BOB" (map:get (map:get result :user) :name))))))

;;; deflens macro tests

(lens:deflens test-first-lens (lst)
  :get (first lst)
  :set (cons value (rest lst)))

(deftest test-deflens
  (let ((lens (test-first-lens)))
    (is (= 1 (lens:view lens '(1 2 3))))
    (is (equal '(10 2 3) (lens:set lens 10 '(1 2 3))))))

;;; Complex nested structure tests

(deftest test-complex-nesting
  (let ((data (map:assoc map:+empty+ :users
                         (list (map:assoc (map:assoc map:+empty+ :name "Alice") :age 30)
                               (map:assoc (map:assoc map:+empty+ :name "Bob") :age 25)))))
    ;; Focus on first user's age
    (let ((first-user-age (lens:>>> (lens:at :users) (lens:first-lens) (lens:at :age))))
      (is (= 30 (lens:view first-user-age data)))
      (let ((updated (lens:over first-user-age #'1+ data)))
        (is (= 31 (lens:view first-user-age updated)))))))

;;; Immutability tests

(deftest test-original-unchanged
  (let ((original '(1 2 3)))
    (let ((modified (lens:set (lens:first-lens) 10 original)))
      (is (equal '(1 2 3) original))
      (is (equal '(10 2 3) modified)))))

(deftest test-map-original-unchanged
  (let ((original (map:assoc map:+empty+ :x 1)))
    (let ((modified (lens:set (lens:at :x) 10 original)))
      (is (= 1 (map:get original :x)))
      (is (= 10 (map:get modified :x))))))
