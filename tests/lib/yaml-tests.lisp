(defpackage #:epsilon.lib.yaml.tests
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames
   (#:yaml #:epsilon.lib.yaml)))

(in-package #:epsilon.lib.yaml.tests)

;; Tests
(deftest basic-scalar-test
  (is-equal
      "hello"
      (yaml:parse-string "hello")
      "Simple scalar should parse correctly"))

(deftest basic-sequence-test
  (is-equal
      '("item1" "item2")
      (yaml:parse-string "- item1
- item2"))
      "Simple sequence should parse correctly")

(deftest basic-mapping-test
  (is-equal
      '(("key" . "value"))
      (yaml:parse-string "key: value")
      "Simple mapping should parse correctly"))

(deftest simple-sequence-test
  (is-equal '("item1" "item2" "item3")
             (yaml:parse-string "- item1
- item2
- item3")
      "Basic sequence should parse correctly"))

(deftest multiline-sequence-test
  (is-equal (list "item1" 
                   "item2 continued
on next line"
                   "item3")
             (yaml:parse-string "- item1
- item2 continued
  on next line
- item3")
      "Multiline sequence items should parse correctly"))

(deftest nested-sequence-test
  (is-equal '("item1" ("nested1" "nested2") "item3")
             (yaml:parse-string "- item1
- - nested1
  - nested2
- item3")
      "Nested sequences should parse correctly"))

(deftest empty-sequence-items-test
  (is-equal '(nil "item2" nil)
             (yaml:parse-string "- 
- item2
- ")
      "Empty sequence items should parse as nil"))

;; FIXME
#++
(deftest mixed-sequence-test
  (is-equal '("scalar" ("nested1" "nested2") "key: value")
             (yaml:parse-string "- scalar
- - nested1
  - nested2
- key: value")
      "Mixed content sequence should parse correctly"))

 ;; FIXME
#++
(deftest indented-sequence-test
  (is-equal '(("key" . ("item1" "item2")))
             (yaml:parse-string "key:
  - item1
  - item2")
      "Indented sequence should parse correctly"))

(deftest complex-nested-sequence-test
  (is-equal '("simple"
               ("nested1" 
                "multiline nested
continued"
                ("deep1" "deep2"))
               "final")
             (yaml:parse-string "- simple
- - nested1
  - multiline nested
    continued
  - - deep1
    - deep2
- final")
      "Complex nested sequence with multiline items should parse correctly"))
