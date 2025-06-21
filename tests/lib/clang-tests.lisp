(defpackage :epsilon.lib.clang.test
  (:use
   :cl
   :epsilon.lib.syntax
   :epsilon.tool.test)
  (:local-nicknames 
   (:p :epsilon.lib.parser)
   (:c :epsilon.lib.clang)))

(in-package :epsilon.lib.clang.test)

(defun parse (combinator string)
  (->> (make-string-input-stream string)
       c:tokenize
       (p:parse combinator)))

(deftest test-combinator-parser
  "Test that combinator parser can parse simple C declarations"
  (is (parse (c::typedef-declaration)
             "typedef int my_int_t;")))

