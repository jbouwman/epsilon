(defpackage :epsilon.lib.clang.test
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames 
   (:fs :epsilon.sys.fs)
   (:parser :epsilon.lib.clang)
   (:seq :epsilon.lib.sequence)
   (:map :epsilon.lib.map)
   (:p :epsilon.lib.parser)
   (:grammar :epsilon.lib.clang-grammar)
   (:c :epsilon.lib.clang-combinators)))

(in-package :epsilon.lib.clang.test)

(defun parse (string combinator)
  (let ((tokens (parser::tokenize (make-string-input-stream string))))
    (p:parse combinator tokens)))

(deftest test-combinator-parser ()
  "Test that combinator parser can parse simple C declarations"
  (let ((parse-result (parse "typedef int my_int_t;"
                             (grammar:typedef-declaration))))
    (is (p:success-p parse-result) "Parse should succeed")
    (when (p:success-p parse-result)
      (let ((typedef-ast (first (p:success-value parse-result))))
        (is (eq (getf typedef-ast :type) :typedef)
            "Should be a typedef")
        (is (string= (getf typedef-ast :name) "my_int_t")
            "Should have correct name")
        (is-equal (getf typedef-ast :underlying-type) '("int")
            "Should have correct underlying type")))))

