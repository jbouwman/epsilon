(defpackage epsilon.string-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.string str)
            (epsilon.sequence seq))
  (:enter t))

;;;; Basic Operations Tests

(deftest test-concat
  "Test string concatenation"
  (assert-true (string= (str:concat "hello" " " "world") "hello world"))
  (assert-true (string= (str:concat) ""))
  (assert-true (string= (str:concat "single") "single"))
  (assert-true (string= (str:concat "" "a" "") "a")))

(deftest test-empty-p
  "Test empty string detection"
  (assert-true (str:empty-p ""))
  (assert-true (str:empty-p nil))
  (assert-true (not (str:empty-p " ")))
  (assert-true (not (str:empty-p "a"))))

(deftest test-first-last-char
  "Test first and last character extraction"
  (assert-true (char= (str:first-char "hello") #\h))
  (assert-true (char= (str:last-char "hello") #\o))
  (assert-true (null (str:first-char "")))
  (assert-true (null (str:last-char "")))
  (assert-true (null (str:first-char nil)))
  (assert-true (null (str:last-char nil))))

;;;; String Replacement Tests

(deftest test-replace-all
  "Test replacing all occurrences"
  (assert-true (string= (str:replace-all "hello world" "o" "0") "hell0 w0rld"))
  (assert-true (string= (str:replace-all "aaa" "a" "b") "bbb"))
  (assert-true (string= (str:replace-all "test" "x" "y") "test"))
  (assert-true (string= (str:replace-all "" "a" "b") ""))
  (assert-true (string= (str:replace-all "abcabc" "abc" "x") "xx"))
  ;; Test HTML escaping use case
  (assert-true (string= (str:replace-all "<div>" "<" "&lt;") "&lt;div>"))
  (assert-true (string= (str:replace-all "a&b&c" "&" "&amp;") "a&amp;b&amp;c"))
  ;; Edge cases
  (assert-true (null (str:replace-all nil "a" "b")))
  (assert-true (string= (str:replace-all "hello" "" "x") "hello"))
  (assert-true (string= (str:replace-all "hello" "hello" "") "")))

(deftest test-replace-first
  "Test replacing first occurrence"
  (assert-true (string= (str:replace-first "hello hello" "hello" "hi") "hi hello"))
  (assert-true (string= (str:replace-first "aaa" "a" "b") "baa"))
  (assert-true (string= (str:replace-first "test" "x" "y") "test"))
  (assert-true (null (str:replace-first nil "a" "b")))
  (assert-true (string= (str:replace-first "" "a" "b") "")))

(deftest test-replace
  "Test flexible replacement"
  (assert-true (string= (str:replace "aaa" "a" "b" :all t) "bbb"))
  (assert-true (string= (str:replace "aaa" "a" "b" :count 2) "bba"))
  (assert-true (string= (str:replace "aaa" "a" "b") "baa"))
  (assert-true (string= (str:replace "hello" "l" "L" :count 0) "hello"))
  (assert-true (null (str:replace nil "a" "b"))))

;;;; Case Conversion Tests

(deftest test-upcase
  "Test uppercase conversion"
  (assert-true (string= (str:upcase "hello") "HELLO"))
  (assert-true (string= (str:upcase "Hello World!") "HELLO WORLD!"))
  (assert-true (string= (str:upcase "") ""))
  (assert-true (string= (str:upcase "ABC123") "ABC123"))
  (assert-true (null (str:upcase nil))))

(deftest test-downcase
  "Test lowercase conversion"
  (assert-true (string= (str:downcase "HELLO") "hello"))
  (assert-true (string= (str:downcase "Hello World!") "hello world!"))
  (assert-true (string= (str:downcase "") ""))
  (assert-true (string= (str:downcase "abc123") "abc123"))
  (assert-true (null (str:downcase nil))))

(deftest test-capitalize
  "Test capitalization"
  (assert-true (string= (str:capitalize "hello world") "Hello World"))
  (assert-true (string= (str:capitalize "HELLO WORLD") "Hello World"))
  (assert-true (string= (str:capitalize "") ""))
  (assert-true (string= (str:capitalize "123abc") "123Abc"))
  (assert-true (string= (str:capitalize "hello-world") "Hello-World"))
  (assert-true (null (str:capitalize nil))))

;;;; Search and Match Tests

(deftest test-contains-p
  "Test substring containment"
  (assert-true (str:contains-p "hello world" "world"))
  (assert-true (str:contains-p "hello" ""))
  (assert-true (not (str:contains-p "hello" "xyz")))
  (assert-true (not (str:contains-p "" "a"))))

(deftest test-starts-ends-with
  "Test string prefix/suffix checking"
  (assert-true (str:starts-with-p "hello world" "hello"))
  (assert-true (str:ends-with-p "hello world" "world"))
  (assert-true (not (str:starts-with-p "hello" "world")))
  (assert-true (not (str:ends-with-p "hello" "world")))
  (assert-true (str:starts-with-p "hello" ""))
  (assert-true (str:ends-with-p "hello" "")))

(deftest test-index-of
  "Test finding substring index"
  (assert-true (= (str:index-of "hello world" "world") 6))
  (assert-true (= (str:index-of "hello" "hello") 0))
  (assert-true (null (str:index-of "hello" "xyz")))
  (assert-true (= (str:index-of "aaa" "a") 0))
  (assert-true (= (str:index-of "hello world" "o") 4))
  (assert-true (= (str:index-of "hello world" "o" :start 5) 7))
  (assert-true (null (str:index-of nil "a")))
  (assert-true (null (str:index-of "hello" nil))))

;;;; Substring Operations Tests

(deftest test-substring
  "Test substring extraction"
  (assert-true (string= (str:substring "hello" 1 4) "ell"))
  (assert-true (string= (str:substring "hello" 0 5) "hello"))
  (assert-true (string= (str:substring "hello" 2) "llo"))
  (assert-true (string= (str:substring "hello" 5) ""))
  (assert-true (string= (str:substring "hello" 10) ""))
  (assert-true (null (str:substring nil 0)))
  (assert-true (string= (str:substring "hello" 0 10) "hello")))

;;;; Trimming and Padding Tests

(deftest test-trim-variants
  "Test different trim operations"
  (assert-true (string= (str:trim "  hello  ") "hello"))
  (assert-true (string= (str:trim (coerce (list #\Tab #\Newline #\h #\e #\l #\l #\o #\Return #\Newline) 'string)) "hello"))
  (assert-true (string= (str:strip-left "...hello" #\.) "hello"))
  (assert-true (string= (str:strip-right "hello..." #\.) "hello"))
  (assert-true (string= (str:strip "...hello..." #\.) "hello"))
  (assert-true (string= (str:trim "") ""))
  (assert-true (string= (str:strip "aaa" #\a) ""))
  ;; Edge cases for strip
  (assert-true (string= (str:strip-left "aaa" #\a) ""))
  (assert-true (string= (str:strip-right "aaa" #\a) ""))
  (assert-true (string= (str:strip "" #\a) ""))
  (assert-true (string= (str:strip "abc" #\x) "abc")))

(deftest test-padding
  "Test string padding"
  (assert-true (string= (str:pad-left "hi" 5) "   hi"))
  (assert-true (string= (str:pad-left "hi" 5 #\*) "***hi"))
  (assert-true (string= (str:pad-right "hi" 5) "hi   "))
  (assert-true (string= (str:pad-right "hi" 5 #\*) "hi***"))
  (assert-true (string= (str:pad-left "hello" 3) "hello"))
  (assert-true (null (str:pad-left nil 5)))
  (assert-true (null (str:pad-right nil 5))))

;;;; Transformation Tests

(deftest test-reverse
  "Test string reversal"
  (assert-true (string= (str:reverse "hello") "olleh"))
  (assert-true (string= (str:reverse "") ""))
  (assert-true (string= (str:reverse "a") "a"))
  (assert-true (string= (str:reverse "12345") "54321"))
  (assert-true (null (str:reverse nil))))

;;;; String Analysis Tests

(deftest test-blank-p
  "Test blank string detection"
  (assert-true (str:blank-p nil))
  (assert-true (str:blank-p ""))
  (assert-true (str:blank-p "   "))
  (assert-true (str:blank-p (coerce (list #\Tab #\Newline #\Return) 'string)))
  (assert-true (not (str:blank-p " a ")))
  (assert-true (not (str:blank-p "hello"))))

;;;; Join and Split Tests

(deftest test-join
  "Test string joining"
  (assert-true (string= (str:join #\, (seq:seq '("a" "b" "c"))) "a,b,c"))
  (assert-true (string= (str:join #\Space (seq:seq '("hello" "world"))) "hello world"))
  (assert-true (string= (str:join #\, seq:*empty*) ""))
  (assert-true (string= (str:join #\, (seq:seq '("single"))) "single")))

(deftest test-split
  "Test string splitting"
  (assert-true (equal (seq:realize (str:split #\, "a,b,c")) '("a" "b" "c")))
  (assert-true (equal (seq:realize (str:split #\Space "hello world")) '("hello" "world")))
  (assert-true (equal (seq:realize (str:split #\, "single")) '("single")))
  (assert-true (equal (seq:realize (str:split #\, "")) '("")))
  (assert-true (equal (seq:realize (str:split #\, ",a,b,")) '("" "a" "b" ""))))

;;;; Edge Cases and Error Handling

(deftest test-nil-handling
  "Test nil input handling across functions"
  (assert-true (null (str:upcase nil)))
  (assert-true (null (str:downcase nil)))
  (assert-true (null (str:replace-all nil "a" "b")))
  (assert-true (str:blank-p nil))
  (assert-true (null (str:substring nil 0)))
  (assert-true (null (str:reverse nil))))

(deftest test-empty-string-handling
  "Test empty string handling"
  (assert-true (string= (str:upcase "") ""))
  (assert-true (string= (str:replace-all "" "a" "b") ""))
  (assert-true (string= (str:trim "") ""))
  (assert-true (string= (str:pad-left "" 3) "   ")))

;;;; Performance Edge Cases

(deftest test-large-string-operations
  "Test operations on large strings"
  (let ((large-string (make-string 1000 :initial-element #\a)))
    (assert-true (= (length (str:replace-all large-string "a" "bb")) 2000))
    (assert-true (= (length (str:reverse large-string)) 1000))))
