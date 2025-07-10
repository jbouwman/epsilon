(defpackage :epsilon.lib.clang.tests
  (:use
   :cl
   :epsilon.lib.syntax
   :epsilon.tool.test)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:lexer #:epsilon.lib.lexer.impl)
   (#:seq #:epsilon.lib.sequence)
   (#:p #:epsilon.lib.parser.impl)
   (#:c #:epsilon.lib.clang.impl)))

(in-package :epsilon.lib.clang.tests)

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
  (is-equal (token-values "identifier") '("identifier"))
  (is-equal (token-values "var1 var2") '("var1" "var2"))
  (is-equal (token-values "_underscore") '("_underscore"))
  (is-equal (token-values "CamelCase") '("CamelCase"))
  (is-equal (token-values "snake_case") '("snake_case"))
  (is-equal (token-values "var123") '("var123")))

(deftest tokenize-keywords
  "Test tokenizing C keywords"
  (is-equal (token-types "int") '(:keyword))
  (is-equal (token-types "typedef struct union enum") 
            '(:keyword :keyword :keyword :keyword))
  (is-equal (token-values "void char short int long") 
            '("void" "char" "short" "int" "long"))
  (is-equal (token-values "float double signed unsigned") 
            '("float" "double" "signed" "unsigned"))
  (is-equal (token-values "const volatile restrict") 
            '("const" "volatile" "restrict"))
  (is-equal (token-values "static extern auto register") 
            '("static" "extern" "auto" "register")))

(deftest tokenize-numbers
  "Test tokenizing numeric literals"
  (is-equal (token-types "42") '(:number))
  (is-equal (token-values "42") '("42"))
  (is-equal (token-values "0") '("0"))
  (is-equal (token-values "123") '("123"))
  (is-equal (token-values "3.14") '("3.14"))
  (is-equal (token-values "2.5") '("2.5"))
  (is-equal (token-values "0.0") '("0.0")))

(deftest tokenize-strings
  "Test tokenizing string literals"
  (skip)
  (is-equal (token-types "\"hello\"") '(:string))
  (is-equal (token-values "\"hello\"") '("hello"))
  (is-equal (token-values "\"\"") '(""))
  (is-equal (token-values "\"hello world\"") '("hello world"))
  (is-equal (token-values "\"line1\\nline2\"") '("line1\\nline2")))

(deftest tokenize-symbols
  "Test tokenizing C symbols and operators"
  (is-equal (token-values "(") '("("))
  (is-equal (token-values ")") '(")"))
  (is-equal (token-values "{") '("{"))
  (is-equal (token-values "}") '("}"))
  (is-equal (token-values "[") '("["))
  (is-equal (token-values "]") '("]"))
  (is-equal (token-values ";") '(";"))
  (is-equal (token-values ",") '(","))
  (is-equal (token-values "*") '("*"))
  (is-equal (token-values "&") '("&"))
  (is-equal (token-values "=") '("=")))

(deftest tokenize-complex-expression
  "Test tokenizing complex C expressions"
  (let ((tokens (token-values "int main(void) { return 0; }")))
    (is-equal tokens '("int" "main" "(" "void" ")" "{" "return" "0" ";" "}")))
  (let ((tokens (token-values "struct point { int x, y; };")))
    (is-equal tokens '("struct" "point" "{" "int" "x" "," "y" ";" "}" ";"))))

(deftest tokenize-whitespace
  "Test that whitespace is properly handled"
  (is-equal (token-values "  int   main  ") '("int" "main"))
  #+fixme (is-equal (token-values "int\tmain\n") '("int" "main"))
  #+fixme (is-equal (token-values "a b\tc\nd") '("a" "b" "c" "d")))

;;;; Parser combinator tests

(deftest parse-identifiers
  "Test parsing C identifiers"
  (let ((result (parse (c::c-identifier) "identifier")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "identifier"))
  (let ((result (parse (c::c-identifier) "var123")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "var123")))

(deftest parse-keywords
  "Test parsing C keywords"
  (let ((result (parse (c::c-keyword "int") "int")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "int"))
  (let ((result (parse (c::c-keyword "struct") "struct")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "struct")))

(deftest parse-type-specifiers
  "Test parsing C type specifiers"
  (let ((result (parse (c::type-specifier) "int")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "int"))
  (let ((result (parse (c::type-specifier) "void")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "void"))
  (let ((result (parse (c::type-specifier) "double")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "double")))

(deftest parse-storage-class
  "Test parsing storage class specifiers"
  (let ((result (parse (c::storage-class) "static")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "static"))
  (let ((result (parse (c::storage-class) "extern")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "extern")))

(deftest parse-type-qualifiers
  "Test parsing type qualifiers"
  (let ((result (parse (c::type-qualifier) "const")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "const"))
  (let ((result (parse (c::type-qualifier) "volatile")))
    (is (p:success-p result))
    (is-equal (p:success-value result) "volatile")))

;;;; Declaration tests

(deftest parse-typedef-declaration
  "Test parsing typedef declarations"
  (skip)
  (let ((result (parse (c::typedef-declaration) "typedef int my_int_t;")))
    (is (p:success-p result))
    (let ((typedef-list (p:success-value result)))
      (is (listp typedef-list))
      (is (= (length typedef-list) 1))))
  (let ((result (parse (c::typedef-declaration) "typedef unsigned int uint32_t;")))
    (is (p:success-p result)))
  (let ((result (parse (c::typedef-declaration) "typedef struct point point_t;")))
    (is (p:success-p result))))

(deftest parse-variable-declaration
  "Test parsing variable declarations"
  (skip)
  (let ((result (parse (c::variable-declaration) "int x;")))
    (is (p:success-p result))
    (let ((var-list (p:success-value result)))
      (is (listp var-list))
      (is (= (length var-list) 1))))
  (let ((result (parse (c::variable-declaration) "int x, y, z;")))
    (is (p:success-p result))
    (let ((var-list (p:success-value result)))
      (is (= (length var-list) 3))))
  (let ((result (parse (c::variable-declaration) "const char msg;")))
    (is (p:success-p result))))

(deftest parse-function-declaration
  "Test parsing function declarations"
  (skip)
  (let ((result (parse (c::function-declaration) "int main();")))
    (is (p:success-p result)))
  (let ((result (parse (c::function-declaration) "void func(int x, char y);")))
    (is (p:success-p result)))
  (let ((result (parse (c::function-declaration) "static inline int add(int a, int b);")))
    (is (p:success-p result))))

;;;; Structure tests

(deftest parse-struct-specifier
  "Test parsing struct specifiers"
  (let ((result (parse (c::struct-specifier) "struct point { int x; int y; }")))
    (is (p:success-p result)))
  (let ((result (parse (c::struct-specifier) "struct point")))
    (is (p:success-p result)))
  (let ((result (parse (c::struct-specifier) "struct { int x; int y; }")))
    (is (p:success-p result))))

(deftest parse-union-specifier
  "Test parsing union specifiers"
  (let ((result (parse (c::union-specifier) "union data { int i; float f; }")))
    (is (p:success-p result)))
  (let ((result (parse (c::union-specifier) "union data")))
    (is (p:success-p result))))

(deftest parse-enum-specifier
  "Test parsing enum specifiers"
  (let ((result (parse (c::enum-specifier) "enum color { RED, GREEN, BLUE }")))
    (is (p:success-p result)))
  (let ((result (parse (c::enum-specifier) "enum status { OK = 0, ERROR = 1 }")))
    (is (p:success-p result)))
  (let ((result (parse (c::enum-specifier) "enum color")))
    (is (p:success-p result))))

;;;; Complex parsing tests

(deftest parse-complex-typedef
  "Test parsing complex typedef declarations"
  (skip)
  (let ((result (parse (c::typedef-declaration) "typedef struct node { int data; struct node *next; } node_t;")))
    (is (p:success-p result))))

(deftest parse-external-declarations
  "Test parsing various external declarations"
  (skip)
  (let ((result (parse (c::external-declaration) "int global_var;")))
    (is (p:success-p result)))
  (let ((result (parse (c::external-declaration) "extern void func();")))
    (is (p:success-p result)))
  (let ((result (parse (c::external-declaration) "typedef int my_type;")))
    (is (p:success-p result))))

;;;; Whitespace and formatting tests

(deftest parse-with-whitespace
  "Test parsing with various whitespace patterns"
  (skip)
  (let ((result (parse (c::typedef-declaration) "  typedef   int   my_int_t  ;  ")))
    (is (p:success-p result)))
  (let ((result (parse (c::variable-declaration) "int\tx\n;")))
    (is (p:success-p result))))

;;;; Error handling tests

(deftest parse-invalid-syntax
  "Test that invalid C syntax produces parse failures"
  (let ((result (parse (c::typedef-declaration) "typedef int")))
    (is (not (p:success-p result))))
  (let ((result (parse (c::variable-declaration) "int x")))
    (is (not (p:success-p result))))
  (let ((result (parse (c::c-identifier) "123invalid")))
    (is (not (p:success-p result)))))

(deftest parse-empty-input
  "Test parsing empty input"
  (let ((result (parse (c::c-identifier) "")))
    (is (not (p:success-p result)))))

;;;; AST node creation tests

(deftest ast-node-creation
  "Test AST node creation functions"
  (let ((typedef-node (c::make-c-typedef "my_type" '("int"))))
    (is (listp typedef-node))
    (is-eq (first typedef-node) :type)
    (is-eq (second typedef-node) :typedef))
  (let ((var-node (c::make-c-variable "x" '("int"))))
    (is (listp var-node))
    (is-eq (second var-node) :variable)))
