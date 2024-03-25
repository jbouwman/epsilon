(tool.test:define-test-package #:lib.regex/tests
  (:local-nicknames
   (#:re #:lib.regex)))

(in-package #:lib.regex/tests)

(deftest scan ()
  (is (equalp (multiple-value-list (re:scan "(a)*b" "xaaabd"))
              (list 1 5 #(3) #(4))))

  (is (equalp (multiple-value-list (re:scan "(a)*b" "xaaabd" :start 1))
              (list 1 5 #(3) #(4))))

  (is (equalp (multiple-value-list (re:scan "(a)*b" "xaaabd" :start 2))
              (list 2 5 #(3) #(4))))

  (is (null (re:scan "(a)*b" "xaaabd" :end 4)))

  (is (equalp (multiple-value-list (re:scan '(:greedy-repetition 0 nil #\b) "bbbc"))
              (list 0 3 #() #())))

  (is (null (re:scan '(:greedy-repetition 4 6 #\b) "bbbc"))))

(deftest create-scanner ()
  (let ((s (re:create-scanner "(([a-c])+)x")))
    (is (equalp (multiple-value-list (re:scan s "abcxy"))
                (list 0 4 #(0 2) #(3 3))))))

(deftest scan-to-strings ()
  (is (equalp (multiple-value-list (re:scan-to-strings "[^b]*b" "aaabd"))
              (list "aaab" #())))

  (is (equalp (multiple-value-list (re:scan-to-strings "([^b])*b" "aaabd"))
              (list "aaab" #("a"))))

  (is (equalp (multiple-value-list (re:scan-to-strings "(([^b])*)b" "aaabd"))
              (list "aaab" #("aaa" "a")))))
