(defpackage :epsilon.clang.tests
  (:use
   :cl
   :epsilon.syntax
   :epsilon.test)
  (:local-nicknames
   (#:fs #:epsilon.file)
   (#:lexer #:epsilon.lexer)
   (#:seq #:epsilon.sequence)
   (#:p #:epsilon.parser)
   (#:c #:epsilon.clang))
  (:enter t))

;; Helper functions for tokenization and parsing

(defun tokenize (string)
  "Tokenize C source string into token sequence"
  (->> (make-string-input-stream string)
       c:tokenize))

(defun parse (combinator string)
  "Parse C source string using given combinator"
  (->> string
       tokenize
       (p:parse combinator)))

(defun token-types (string)
  "Extract token types from tokenized string"
  (->> string
       tokenize
       seq:realize
       (map 'list (lambda (token) (lexer:token-type token)))))

(defun token-values (string)
  "Extract token values from tokenized string"
  (->> string
       tokenize
       seq:realize
       (map 'list (lambda (token) (lexer:token-value token)))))

;;;; Tokenization tests

(deftest tokenize-identifiers
  "Test tokenizing C identifiers"
  (assert-equal (token-values "identifier") '("identifier"))
  (assert-equal (token-values "var1 var2") '("var1" "var2"))
  (assert-equal (token-values "_underscore") '("_underscore"))
  (assert-equal (token-values "CamelCase") '("CamelCase"))
  (assert-equal (token-values "snake_case") '("snake_case"))
  (assert-equal (token-values "var123") '("var123")))

(deftest tokenize-keywords
  "Test tokenizing C keywords"
  (assert-equal (token-types "int") '(:keyword))
  (assert-equal (token-types "typedef struct union enum")
            '(:keyword :keyword :keyword :keyword))
  (assert-equal (token-values "void char short int long")
            '("void" "char" "short" "int" "long"))
  (assert-equal (token-values "float double signed unsigned")
            '("float" "double" "signed" "unsigned"))
  (assert-equal (token-values "const volatile restrict")
            '("const" "volatile" "restrict"))
  (assert-equal (token-values "static extern auto register")
            '("static" "extern" "auto" "register")))

(deftest tokenize-numbers
  "Test tokenizing numeric literals"
  (assert-equal (token-types "42") '(:number))
  (assert-equal (token-values "42") '("42"))
  (assert-equal (token-values "0") '("0"))
  (assert-equal (token-values "123") '("123"))
  (assert-equal (token-values "3.14") '("3.14"))
  (assert-equal (token-values "2.5") '("2.5"))
  (assert-equal (token-values "0.0") '("0.0")))

(deftest tokenize-strings
  "Test tokenizing string literals"
  (skip "String literal tokenization not yet implemented")
  (assert-equal (token-types "\"hello\"") '(:string))
  (assert-equal (token-values "\"hello\"") '("hello"))
  (assert-equal (token-values "\"\"") '(""))
  (assert-equal (token-values "\"hello world\"") '("hello world"))
  (assert-equal (token-values "\"line1\\nline2\"") '("line1\\nline2")))

(deftest tokenize-symbols
  "Test tokenizing C symbols and operators"
  (assert-equal (token-values "(") '("("))
  (assert-equal (token-values ")") '(")"))
  (assert-equal (token-values "{") '("{"))
  (assert-equal (token-values "}") '("}"))
  (assert-equal (token-values "[") '("["))
  (assert-equal (token-values "]") '("]"))
  (assert-equal (token-values ";") '(";"))
  (assert-equal (token-values ",") '(","))
  (assert-equal (token-values "*") '("*"))
  (assert-equal (token-values "&") '("&"))
  (assert-equal (token-values "=") '("=")))

(deftest tokenize-complex-expression
  "Test tokenizing complex C expressions"
  (let ((tokens (token-values "int main(void) { return 0; }")))
    (assert-equal tokens '("int" "main" "(" "void" ")" "{" "return" "0" ";" "}")))
  (let ((tokens (token-values "struct point { int x, y; };")))
    (assert-equal tokens '("struct" "point" "{" "int" "x" "," "y" ";" "}" ";"))))

(deftest tokenize-whitespace
  "Test that whitespace is handled"
  (assert-equal (token-values "  int   main  ") '("int" "main"))
  #+fixme (assert-equal (token-values "int\tmain\n") '("int" "main"))
  #+fixme (assert-equal (token-values "a b\tc\nd") '("a" "b" "c" "d")))

;;;; Parser combinator tests

(deftest parse-identifiers
  "Test parsing C identifiers"
  (let ((result (parse (c::c-identifier) "identifier")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "identifier"))
  (let ((result (parse (c::c-identifier) "var123")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "var123")))

(deftest parse-keywords
  "Test parsing C keywords"
  (let ((result (parse (c::c-keyword "int") "int")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "int"))
  (let ((result (parse (c::c-keyword "struct") "struct")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "struct")))

(deftest parse-type-specifiers
  "Test parsing C type specifiers"
  (let ((result (parse (c::type-specifier) "int")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "int"))
  (let ((result (parse (c::type-specifier) "void")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "void"))
  (let ((result (parse (c::type-specifier) "double")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "double")))

(deftest parse-storage-class
  "Test parsing storage class specifiers"
  (let ((result (parse (c::storage-class) "static")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "static"))
  (let ((result (parse (c::storage-class) "extern")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "extern")))

(deftest parse-type-qualifiers
  "Test parsing type qualifiers"
  (let ((result (parse (c::type-qualifier) "const")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "const"))
  (let ((result (parse (c::type-qualifier) "volatile")))
    (assert-true (p:success-p result))
    (assert-equal (p:success-value result) "volatile")))

;;;; Declaration tests

(deftest parse-typedef-declaration
  "Test parsing typedef declarations"
  (skip "Typedef declaration parsing not yet implemented")
  (let ((result (parse (c::typedef-declaration) "typedef int my_int_t;")))
    (assert-true (p:success-p result))
    (let ((typedef-list (p:success-value result)))
      (assert-true (listp typedef-list))
      (assert-true (= (length typedef-list) 1))))
  (let ((result (parse (c::typedef-declaration) "typedef unsigned int uint32_t;")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::typedef-declaration) "typedef struct point point_t;")))
    (assert-true (p:success-p result))))

(deftest parse-variable-declaration
  "Test parsing variable declarations"
  (skip "Variable declaration parsing not yet implemented")
  (let ((result (parse (c::variable-declaration) "int x;")))
    (assert-true (p:success-p result))
    (let ((var-list (p:success-value result)))
      (assert-true (listp var-list))
      (assert-true (= (length var-list) 1))))
  (let ((result (parse (c::variable-declaration) "int x, y, z;")))
    (assert-true (p:success-p result))
    (let ((var-list (p:success-value result)))
      (assert-true (= (length var-list) 3))))
  (let ((result (parse (c::variable-declaration) "const char msg;")))
    (assert-true (p:success-p result))))

(deftest parse-function-declaration
  "Test parsing function declarations"
  (skip "Function declaration parsing not yet implemented")
  (let ((result (parse (c::function-declaration) "int main();")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::function-declaration) "void func(int x, char y);")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::function-declaration) "static inline int add(int a, int b);")))
    (assert-true (p:success-p result))))

;;;; Structure tests

(deftest parse-struct-specifier
  "Test parsing struct specifiers"
  (let ((result (parse (c::struct-specifier) "struct point { int x; int y; }")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::struct-specifier) "struct point")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::struct-specifier) "struct { int x; int y; }")))
    (assert-true (p:success-p result))))

(deftest parse-union-specifier
  "Test parsing union specifiers"
  (let ((result (parse (c::union-specifier) "union data { int i; float f; }")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::union-specifier) "union data")))
    (assert-true (p:success-p result))))

(deftest parse-enum-specifier
  "Test parsing enum specifiers"
  (let ((result (parse (c::enum-specifier) "enum color { RED, GREEN, BLUE }")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::enum-specifier) "enum status { OK = 0, ERROR = 1 }")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::enum-specifier) "enum color")))
    (assert-true (p:success-p result))))

;;;; Complex parsing tests

(deftest parse-complex-typedef
  "Test parsing complex typedef declarations"
  (skip "Complex typedef parsing not yet implemented")
  (let ((result (parse (c::typedef-declaration) "typedef struct node { int data; struct node *next; } node_t;")))
    (assert-true (p:success-p result))))

(deftest parse-external-declarations
  "Test parsing various external declarations"
  (skip "External declaration parsing not yet implemented")
  (let ((result (parse (c::external-declaration) "int global_var;")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::external-declaration) "extern void func();")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::external-declaration) "typedef int my_type;")))
    (assert-true (p:success-p result))))

;;;; Whitespace and formatting tests

(deftest parse-with-whitespace
  "Test parsing with various whitespace patterns"
  (skip "Whitespace handling in parser not yet complete")
  (let ((result (parse (c::typedef-declaration) "  typedef   int   my_int_t  ;  ")))
    (assert-true (p:success-p result)))
  (let ((result (parse (c::variable-declaration) "int\tx\n;")))
    (assert-true (p:success-p result))))

;;;; Error handling tests

(deftest parse-invalid-syntax
  "Test that invalid C syntax produces parse failures"
  (let ((result (parse (c::typedef-declaration) "typedef int")))
    (assert-true (not (p:success-p result))))
  (let ((result (parse (c::variable-declaration) "int x")))
    (assert-true (not (p:success-p result))))
  (let ((result (parse (c::c-identifier) "123invalid")))
    (assert-true (not (p:success-p result)))))

(deftest parse-empty-input
  "Test parsing empty input"
  (let ((result (parse (c::c-identifier) "")))
    (assert-true (not (p:success-p result)))))

;;;; AST node creation tests

(deftest ast-node-creation
  "Test AST node creation functions"
  (let ((typedef-node (c::make-c-typedef "my_type" '("int"))))
    (assert-true (listp typedef-node))
    (assert-eq (first typedef-node) :type)
    (assert-eq (second typedef-node) :typedef))
  (let ((var-node (c::make-c-variable "x" '("int"))))
    (assert-true (listp var-node))
    (assert-eq (second var-node) :variable)))
