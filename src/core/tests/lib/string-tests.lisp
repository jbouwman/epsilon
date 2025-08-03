(defpackage :epsilon.string.tests
  (:use :cl :epsilon.test)
  (:local-nicknames 
   (:str :epsilon.string)
   (:seq :epsilon.sequence)))

(in-package :epsilon.string.tests)

;;;; Basic Operations Tests

(deftest test-concat
  "Test string concatenation"
  (is (string= (str:concat "hello" " " "world") "hello world"))
  (is (string= (str:concat) ""))
  (is (string= (str:concat "single") "single"))
  (is (string= (str:concat "" "a" "") "a")))

(deftest test-empty-p
  "Test empty string detection"
  (is (str:empty-p ""))
  (is (str:empty-p nil))
  (is (not (str:empty-p " ")))
  (is (not (str:empty-p "a"))))

(deftest test-first-last-char
  "Test first and last character extraction"
  (is (char= (str:first-char "hello") #\h))
  (is (char= (str:last-char "hello") #\o))
  (is (null (str:first-char "")))
  (is (null (str:last-char "")))
  (is (null (str:first-char nil)))
  (is (null (str:last-char nil))))

;;;; String Replacement Tests

(deftest test-replace-all
  "Test replacing all occurrences"
  (is (string= (str:replace-all "hello world" "o" "0") "hell0 w0rld"))
  (is (string= (str:replace-all "aaa" "a" "b") "bbb"))
  (is (string= (str:replace-all "test" "x" "y") "test"))
  (is (string= (str:replace-all "" "a" "b") ""))
  (is (string= (str:replace-all "abcabc" "abc" "x") "xx"))
  ;; Test HTML escaping use case
  (is (string= (str:replace-all "<div>" "<" "&lt;") "&lt;div>"))
  (is (string= (str:replace-all "a&b&c" "&" "&amp;") "a&amp;b&amp;c"))
  ;; Edge cases
  (is (null (str:replace-all nil "a" "b")))
  (is (string= (str:replace-all "hello" "" "x") "hello"))
  (is (string= (str:replace-all "hello" "hello" "") "")))

(deftest test-replace-first
  "Test replacing first occurrence"
  (is (string= (str:replace-first "hello hello" "hello" "hi") "hi hello"))
  (is (string= (str:replace-first "aaa" "a" "b") "baa"))
  (is (string= (str:replace-first "test" "x" "y") "test"))
  (is (null (str:replace-first nil "a" "b")))
  (is (string= (str:replace-first "" "a" "b") "")))

(deftest test-replace
  "Test flexible replacement"
  (is (string= (str:replace "aaa" "a" "b" :all t) "bbb"))
  (is (string= (str:replace "aaa" "a" "b" :count 2) "bba"))
  (is (string= (str:replace "aaa" "a" "b") "baa"))
  (is (string= (str:replace "hello" "l" "L" :count 0) "hello"))
  (is (null (str:replace nil "a" "b"))))

;;;; Case Conversion Tests

(deftest test-upcase
  "Test uppercase conversion"
  (is (string= (str:upcase "hello") "HELLO"))
  (is (string= (str:upcase "Hello World!") "HELLO WORLD!"))
  (is (string= (str:upcase "") ""))
  (is (string= (str:upcase "ABC123") "ABC123"))
  (is (null (str:upcase nil))))

(deftest test-downcase
  "Test lowercase conversion"
  (is (string= (str:downcase "HELLO") "hello"))
  (is (string= (str:downcase "Hello World!") "hello world!"))
  (is (string= (str:downcase "") ""))
  (is (string= (str:downcase "abc123") "abc123"))
  (is (null (str:downcase nil))))

(deftest test-capitalize
  "Test capitalization"
  (is (string= (str:capitalize "hello world") "Hello World"))
  (is (string= (str:capitalize "HELLO WORLD") "Hello World"))
  (is (string= (str:capitalize "") ""))
  (is (string= (str:capitalize "123abc") "123Abc"))
  (is (string= (str:capitalize "hello-world") "Hello-World"))
  (is (null (str:capitalize nil))))

;;;; Search and Match Tests

(deftest test-contains-p
  "Test substring containment"
  (is (str:contains-p "hello world" "world"))
  (is (str:contains-p "hello" ""))
  (is (not (str:contains-p "hello" "xyz")))
  (is (not (str:contains-p "" "a"))))

(deftest test-starts-ends-with
  "Test string prefix/suffix checking"
  (is (str:starts-with-p "hello world" "hello"))
  (is (str:ends-with-p "hello world" "world"))
  (is (not (str:starts-with-p "hello" "world")))
  (is (not (str:ends-with-p "hello" "world")))
  (is (str:starts-with-p "hello" ""))
  (is (str:ends-with-p "hello" "")))

(deftest test-index-of
  "Test finding substring index"
  (is (= (str:index-of "hello world" "world") 6))
  (is (= (str:index-of "hello" "hello") 0))
  (is (null (str:index-of "hello" "xyz")))
  (is (= (str:index-of "aaa" "a") 0))
  (is (= (str:index-of "hello world" "o") 4))
  (is (= (str:index-of "hello world" "o" :start 5) 7))
  (is (null (str:index-of nil "a")))
  (is (null (str:index-of "hello" nil))))

(deftest test-last-index-of
  "Test finding last substring index"
  (is (= (str:last-index-of "hello world" "o") 7))
  (is (= (str:last-index-of "aaa" "a") 2))
  (is (null (str:last-index-of "hello" "xyz")))
  (is (null (str:last-index-of nil "a")))
  (is (null (str:last-index-of "hello" nil))))

(deftest test-count-substring
  "Test counting substring occurrences"
  (is (= (str:count-substring "hello world" "o") 2))
  (is (= (str:count-substring "aaa" "a") 3))
  (is (= (str:count-substring "aaa" "aa") 1))  ; Non-overlapping
  (is (= (str:count-substring "test" "xyz") 0))
  (is (= (str:count-substring nil "a") 0))
  (is (= (str:count-substring "hello" nil) 0))
  (is (= (str:count-substring "hello" "") 0)))

;;;; Substring Operations Tests

(deftest test-substring
  "Test substring extraction"
  (is (string= (str:substring "hello" 1 4) "ell"))
  (is (string= (str:substring "hello" 0 5) "hello"))
  (is (string= (str:substring "hello" 2) "llo"))
  (is (string= (str:substring "hello" 5) ""))
  (is (string= (str:substring "hello" 10) ""))
  (is (null (str:substring nil 0)))
  (is (string= (str:substring "hello" 0 10) "hello")))

(deftest test-substring-before-after
  "Test substring before/after delimiter"
  (is (string= (str:substring-before "hello:world" ":") "hello"))
  (is (string= (str:substring-after "hello:world" ":") "world"))
  (is (null (str:substring-before "hello" ":")))
  (is (null (str:substring-after "hello" ":")))
  (is (null (str:substring-before nil ":")))
  (is (null (str:substring-after nil ":")))
  (is (string= (str:substring-before "a:b:c" ":") "a"))
  (is (string= (str:substring-after "a:b:c" ":") "b:c")))

(deftest test-substring-between
  "Test substring between delimiters"
  (is (string= (str:substring-between "[hello]" "[" "]") "hello"))
  (is (string= (str:substring-between "<tag>content</tag>" "<tag>" "</tag>") "content"))
  (is (null (str:substring-between "hello" "[" "]")))
  (is (null (str:substring-between "[hello" "[" "]")))
  (is (null (str:substring-between nil "[" "]"))))

;;;; Trimming and Padding Tests

(deftest test-trim-variants
  "Test different trim operations"
  (is (string= (str:trim "  hello  ") "hello"))
  (is (string= (str:trim "\t\nhello\r\n") "hello"))
  (is (string= (str:strip-left "...hello" #\.) "hello"))
  (is (string= (str:strip-right "hello..." #\.) "hello"))
  (is (string= (str:strip "...hello..." #\.) "hello"))
  (is (string= (str:trim "") ""))
  (is (string= (str:strip "aaa" #\a) ""))
  ;; Edge cases for strip
  (is (string= (str:strip-left "aaa" #\a) ""))
  (is (string= (str:strip-right "aaa" #\a) ""))
  (is (string= (str:strip "" #\a) ""))
  (is (string= (str:strip "abc" #\x) "abc")))

(deftest test-padding
  "Test string padding"
  (is (string= (str:pad-left "hi" 5) "   hi"))
  (is (string= (str:pad-left "hi" 5 #\*) "***hi"))
  (is (string= (str:pad-right "hi" 5) "hi   "))
  (is (string= (str:pad-right "hi" 5 #\*) "hi***"))
  (is (string= (str:center "hi" 5) " hi  "))
  (is (string= (str:center "hi" 6) "  hi  "))
  (is (string= (str:pad-left "hello" 3) "hello"))
  (is (null (str:pad-left nil 5)))
  (is (null (str:pad-right nil 5)))
  (is (null (str:center nil 5))))

;;;; Transformation Tests

(deftest test-reverse
  "Test string reversal"
  (is (string= (str:reverse "hello") "olleh"))
  (is (string= (str:reverse "") ""))
  (is (string= (str:reverse "a") "a"))
  (is (string= (str:reverse "12345") "54321"))
  (is (null (str:reverse nil))))

(deftest test-repeat
  "Test string repetition"
  (is (string= (str:repeat "ab" 3) "ababab"))
  (is (string= (str:repeat "x" 5) "xxxxx"))
  (is (string= (str:repeat "hello" 0) ""))
  (is (string= (str:repeat "" 10) ""))
  (is (null (str:repeat nil 3))))

;;;; String Analysis Tests

(deftest test-blank-p
  "Test blank string detection"
  (is (str:blank-p nil))
  (is (str:blank-p ""))
  (is (str:blank-p "   "))
  (is (str:blank-p (coerce (list #\Tab #\Newline #\Return) 'string)))
  (is (not (str:blank-p " a ")))
  (is (not (str:blank-p "hello"))))

;;;; String Comparison Tests

(deftest test-equals-ignore-case
  "Test case-insensitive comparison"
  (is (str:equals-ignore-case "hello" "HELLO"))
  (is (str:equals-ignore-case "HeLLo" "hEllO"))
  (is (not (str:equals-ignore-case "hello" "world")))
  (is (not (str:equals-ignore-case nil "hello")))
  (is (not (str:equals-ignore-case "hello" nil))))

(deftest test-compare
  "Test string comparison"
  (is (= (str:compare "a" "b") -1))
  (is (= (str:compare "b" "a") 1))
  (is (= (str:compare "hello" "hello") 0))
  (is (= (str:compare "hello" "hello world") -1))
  (is (= (str:compare "hello world" "hello") 1))
  (is (null (str:compare nil "hello")))
  (is (null (str:compare "hello" nil))))

(deftest test-compare-ignore-case
  "Test case-insensitive comparison"
  (is (= (str:compare-ignore-case "A" "b") -1))
  (is (= (str:compare-ignore-case "B" "a") 1))
  (is (= (str:compare-ignore-case "HELLO" "hello") 0))
  (is (null (str:compare-ignore-case nil "hello")))
  (is (null (str:compare-ignore-case "hello" nil))))

;;;; Join and Split Tests

(deftest test-join
  "Test string joining"
  (is (string= (str:join #\, (seq:seq '("a" "b" "c"))) "a,b,c"))
  (is (string= (str:join #\Space (seq:seq '("hello" "world"))) "hello world"))
  (is (string= (str:join #\, seq:*empty*) ""))
  (is (string= (str:join #\, (seq:seq '("single"))) "single")))

(deftest test-split
  "Test string splitting"
  (is (equal (seq:realize (str:split #\, "a,b,c")) '("a" "b" "c")))
  (is (equal (seq:realize (str:split #\Space "hello world")) '("hello" "world")))
  (is (equal (seq:realize (str:split #\, "single")) '("single")))
  (is (equal (seq:realize (str:split #\, "")) '("")))
  (is (equal (seq:realize (str:split #\, ",a,b,")) '("" "a" "b" ""))))

;;;; Edge Cases and Error Handling

(deftest test-nil-handling
  "Test nil input handling across functions"
  (is (null (str:upcase nil)))
  (is (null (str:downcase nil)))
  (is (null (str:replace-all nil "a" "b")))
  (is (str:blank-p nil))
  (is (null (str:substring nil 0)))
  (is (null (str:reverse nil))))

(deftest test-empty-string-handling
  "Test empty string handling"
  (is (string= (str:upcase "") ""))
  (is (string= (str:replace-all "" "a" "b") ""))
  (is (string= (str:trim "") ""))
  (is (string= (str:pad-left "" 3) "   "))
  (is (= (str:count-substring "" "a") 0)))

;;;; Performance Edge Cases

(deftest test-large-string-operations
  "Test operations on large strings"
  (let ((large-string (str:repeat "a" 1000)))
    (is (= (str:count-substring large-string "a") 1000))
    (is (= (length (str:replace-all large-string "a" "bb")) 2000))
    (is (= (length (str:reverse large-string)) 1000))))
