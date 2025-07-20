(defpackage :epsilon.lib.regex.tests
  (:use
   :cl
   :epsilon.test)
  (:local-nicknames
   (:re :epsilon.lib.regex)))

(in-package :epsilon.lib.regex.tests)

(deftest search-test
  "Test search"
  
  ;; Basic search functionality
  (is-equalp 
    (multiple-value-list (re:search "\\d+" "abc123def"))
    (list "123" #())
    "Basic digit search")
  
  (is-equalp
    (multiple-value-list (re:search "(\\d+)" "abc123def"))
    (list "123" #("123"))
    "Search with capture group")
  
  (is-equalp
    (multiple-value-list (re:search "(\\w+)\\s+(\\w+)" "hello world"))
    (list "hello world" #("hello" "world"))
    "Search with multiple capture groups")
  
  ;; No match cases
  (is (null (re:search "\\d+" "abcdef"))
    "Search with no match")
  
  ;; Start/end parameters
  (is-equalp
    (multiple-value-list (re:search "\\d+" "abc123def456" :start 6))
    (list "456" #())
    "Search with start parameter")
  
  (is (null (re:search "\\d+" "abcdef456" :end 6))
    "Search with end parameter limiting match"))



(deftest match
  "Test re.match() equivalent - matches only at beginning"
  
  ;; Successful matches at beginning
  (is-equalp
    (multiple-value-list (re:match "\\w+" "hello world"))
    (list "hello" #())
    "Match at beginning of string")
  
  (is-equalp
    (multiple-value-list (re:match "(\\w+)" "hello world"))
    (list "hello" #("hello"))
    "Match with capture group at beginning")
  
  ;; Failed matches (pattern exists but not at beginning)
  (is (null (re:match "world" "hello world"))
    "Match fails when pattern not at beginning")
  
  (is (null (re:match "\\d+" "abc123"))
    "Match fails when digits not at beginning")
  
  ;; Successful match of entire string
  (is-equalp
    (multiple-value-list (re:match "\\w+\\s+\\w+" "hello world"))
    (list "hello world" #())
    "Match entire string from beginning")
  
  ;; Start parameter
  (is-equalp
    (multiple-value-list (re:match "world" "hello world" :start 6))
    (list "world" #())
    "Match with start parameter"))

(deftest findall
  "Test re.findall() equivalent"
  
  ;; Basic findall
  (is-equalp
    (re:findall "\\d+" "abc123def456ghi789")
    (list "123" "456" "789")
    "Find all digit sequences")
  
  (is-equalp
    (re:findall "\\w+" "hello world test")
    (list "hello" "world" "test")
    "Find all words")
  
  ;; No matches
  (is-equalp
    (re:findall "\\d+" "abcdef")
    (list)
    "Findall with no matches")
  
  ;; Single match
  (is-equalp
    (re:findall "world" "hello world")
    (list "world")
    "Findall with single match")
  
  ;; Start/end parameters
  (is-equalp
    (re:findall "\\d+" "123abc456def789" :start 3 :end 9)
    (list "456")
    "Findall with start and end parameters"))

(deftest finditer
  "Test finditer() equivalent - returns position pairs"
  
  ;; Basic finditer
  (is-equalp
    (re:finditer "\\d+" "abc123def456")
    (list (cons 3 6) (cons 9 12))
    "Find all positions of digit sequences")
  
  (is-equalp
    (re:finditer "\\w+" "a bb ccc")
    (list (cons 0 1) (cons 2 4) (cons 5 8))
    "Find all word positions")
  
  ;; No matches
  (is-equalp
    (re:finditer "\\d+" "abcdef")
    (list)
    "Finditer with no matches")
  
  ;; Single match
  (is-equalp
    (re:finditer "world" "hello world")
    (list (cons 6 11))
    "Finditer with single match"))

(deftest sub
  "Test re.sub() equivalent"
  
  ;; Replace all by default
  (is-equalp
    (re:sub "\\d+" "X" "abc123def456")
    "abcXdefX"
    "Replace all digit sequences")
  
  (is-equalp
    (re:sub "\\s+" " " "hello    world   test")
    "hello world test"
    "Replace multiple spaces with single space")
  
  ;; Limited count
  (is-equalp
    (re:sub "\\d+" "X" "abc123def456ghi789" :count 2)
    "abcXdefXghi789"
    "Replace with count limit")
  
  (is-equalp
    (re:sub "\\d+" "X" "abc123def456" :count 1)
    "abcXdef456"
    "Replace first occurrence only")
  
  ;; No matches
  (is-equalp
    (re:sub "\\d+" "X" "abcdef")
    "abcdef"
    "Sub with no matches returns original")
  
  ;; With capture groups in replacement
  (is-equalp
    (re:sub "(\\w+)" "[\\1]" "hello world")
    "[hello] [world]"
    "Sub with capture group replacement")
  
  ;; Start/end parameters
  (is-equalp
    (re:sub "\\d+" "X" "123abc456def789" :start 3 :end 9)
    "123abcXdef789"
    "Sub with start and end parameters"))

(deftest subn
  "Test re.subn() equivalent - returns count of replacements"
  
  ;; Replace all and count
  (is-equalp
    (multiple-value-list (re:subn "\\d+" "X" "abc123def456"))
    (list "abcXdefX" 2)
    "Replace all and return count")
  
  (is-equalp
    (multiple-value-list (re:subn "\\w+" "X" "hello world test"))
    (list "X X X" 3)
    "Replace all words and count")
  
  ;; Limited count
  (is-equalp
    (multiple-value-list (re:subn "\\d+" "X" "abc123def456ghi789" :count 2))
    (list "abcXdefXghi789" 2)
    "Replace with count limit and return actual count")
  
  (is-equalp
    (multiple-value-list (re:subn "\\d+" "X" "abc123def456" :count 5))
    (list "abcXdefX" 2)
    "Count limit higher than actual matches")
  
  ;; No matches
  (is-equalp
    (multiple-value-list (re:subn "\\d+" "X" "abcdef"))
    (list "abcdef" 0)
    "Subn with no matches returns zero count"))

(deftest compile-test
  "Test re.compile() equivalent"
  
  ;; Basic compilation and usage
  (let ((pattern (re:compile "\\d+")))
    (is (functionp pattern)
      "Compile returns a function")
    
    (is-equalp
      (multiple-value-list (re:scan pattern "abc123def"))
      (list 3 6 #() #())
      "Compiled pattern works with scan")
    
    (is-equalp
      (multiple-value-list (re:scan-to-strings pattern "abc123def"))
      (list "123" #())
      "Compiled pattern works with scan-to-strings"))
  
  ;; Complex pattern compilation
  (let ((pattern (re:compile "(\\w+)\\s+(\\w+)")))
    (is-equalp
      (multiple-value-list (re:scan-to-strings pattern "hello world"))
      (list "hello world" #("hello" "world"))
      "Complex compiled pattern works")))

(deftest edge-cases
  "Test edge cases and error conditions"
  
  ;; Empty strings
  (is (null (re:search "\\d+" ""))
    "Search in empty string")
  
  (is-equalp
    (re:findall ".*" "")
    (list "")
    "Findall matches empty string")
  
  (is-equalp
    (re:sub "x" "y" "")
    ""
    "Sub on empty string")
  
  ;; Empty patterns
  (is-equalp
    (multiple-value-list (re:search "" "hello"))
    (list "" #())
    "Empty pattern matches")
  
  ;; Zero-width matches
  (is-equalp
    (re:findall "\\b" "hello world")
    (list "" "" "" "")
    "Zero-width word boundaries")
  
  ;; Special characters in replacement
  (is-equalp
    (re:sub "hello" "\\& \\&" "hello world")
    "hello hello world"
    "Replacement with reference to whole match"))

(deftest python-compatibility
  "Test cases that mirror Python's re module behavior"
  
  ;; Basic search vs match difference
  (is-equalp
    (multiple-value-list (re:search "world" "hello world"))
    (list "world" #())
    "Search finds pattern anywhere")
  
  (is (null (re:match "world" "hello world"))
    "Match requires pattern at beginning")
  
  ;; Findall with groups
  (is-equalp
    (re:findall "(\\w+)" "hello world")
    (list "hello" "world")
    "Findall returns capture groups when present")
  
  ;; Count parameter behavior
  (is-equalp
    (re:sub "l" "L" "hello" :count 0)
    "heLLo"
    "Count 0 means replace all")
  
  (is-equalp
    (re:sub "l" "L" "hello" :count 1)
    "heLlo"
    "Count 1 means replace first only")
  
  ;; Case sensitivity
  (is (null (re:search "HELLO" "hello world"))
    "Search is case sensitive by default")
  
  ;; Multiple capture groups
  (is-equalp
    (multiple-value-list (re:search "(\\d{2})-(\\d{2})-(\\d{4})" "Date: 12-31-2023"))
    (list "12-31-2023" #("12" "31" "2023"))
    "Multiple capture groups in date pattern"))
