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

(deftest register-groups-bind ()
  (is (equalp (re:register-groups-bind (first second third fourth)
                  ("((a)|(b)|(c))+" "abababc" :sharedp t)
                (list first second third fourth))
              (list "c" "a" "b" "c")))

  (is (equalp (re:register-groups-bind (nil second third fourth)
                  ("((a)|(b)|(c))()+" "abababc" :start 6)
                (list second third fourth))
              (list nil nil "c")))

  (is (null (re:register-groups-bind (first)
                ("(a|b)+" "accc" :start 1)
              first)))

  (is (equalp (re:register-groups-bind (fname lname (#'parse-integer date month year))
                  ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})" "Frank Zappa 21.12.1940")
                (list fname lname (encode-universal-time 0 0 0 date month year 0)))
              (list "Frank" "Zappa" 1292889600))))
