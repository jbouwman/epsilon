(defpackage :epsilon.lib.clang-combinators
  (:use :cl)
  (:local-nicknames 
   (:p :epsilon.lib.parser)
   (:lexer :epsilon.lib.lexer)
   (:seq :epsilon.lib.sequence))
  (:shadow
   :keyword
   :symbol)
  (:export
   ;; Token-level combinators
   :identifier
   :keyword
   :symbol
   :string-literal
   :number-literal
   
   ;; C-specific token combinators
   :c-identifier
   :c-keyword
   :type-specifier
   :storage-class
   :type-qualifier
   
   ;; Structural combinators
   :parens
   :braces
   :brackets
   :semicolon
   :comma
   :star
   :ampersand
   
   ;; Higher-level patterns
   :parameter-list
   :identifier-list
   :declaration-specifiers))

(in-package :epsilon.lib.clang-combinators)

;; Basic token predicates
(defun token-type-p (expected-type)
  "Create predicate for token type."
  (lambda (token)
    (and token (eq (lexer:token-type token) expected-type))))

(defun token-value-p (expected-value)
  "Create predicate for token value."
  (lambda (token)
    (and token (string= (lexer:token-value token) expected-value))))

;; Basic token combinators
(defun identifier ()
  "Parse an identifier token."
  (p:label (p:satisfy (token-type-p :identifier)) "identifier"))

(defun keyword (name)
  "Parse a specific keyword."
  (p:label (p:satisfy (lambda (token)
                        (and (eq (lexer:token-type token) :keyword)
                             (string= (lexer:token-value token) name))))
           (format nil "keyword '~A'" name)))

(defun symbol (sym)
  "Parse a specific symbol."
  (p:label (p:satisfy (token-value-p sym)) (format nil "symbol '~A'" sym)))

(defun string-literal ()
  "Parse a string literal."
  (p:label (p:satisfy (token-type-p :string)) "string literal"))

(defun number-literal ()
  "Parse a number literal."
  (p:label (p:satisfy (token-type-p :number)) "number literal"))

;; C-specific combinators
(defun c-identifier ()
  "Parse C identifier, returning its name."
  (p:bind (identifier)
          (lambda (token)
            (p:return (lexer:token-value token)))))

(defun c-keyword (name)
  "Parse C keyword, returning the keyword name."
  (p:bind (keyword name)
          (lambda (token)
            (p:return (lexer:token-value token)))))

(defun type-specifier ()
  "Parse type specifiers."
  (p:choice
   (c-keyword "void")
   (c-keyword "char")
   (c-keyword "short")
   (c-keyword "int")
   (c-keyword "long")
   (c-keyword "float")
   (c-keyword "double")
   (c-keyword "signed")
   (c-keyword "unsigned")
   (c-keyword "_Bool")
   (c-keyword "_Complex")
   (c-keyword "_Imaginary")))

(defun storage-class ()
  "Parse storage class specifiers."
  (p:choice
   (c-keyword "typedef")
   (c-keyword "extern")
   (c-keyword "static")
   (c-keyword "auto")
   (c-keyword "register")))

(defun type-qualifier ()
  "Parse type qualifiers."
  (p:choice
   (c-keyword "const")
   (c-keyword "restrict")
   (c-keyword "volatile")))

;; Structural combinators
(defun parens (parser)
  "Parse parser between parentheses."
  (p:between (symbol "(") (symbol ")") parser))

(defun braces (parser)
  "Parse parser between braces."
  (p:between (symbol "{") (symbol "}") parser))

(defun brackets (parser)
  "Parse parser between brackets."
  (p:between (symbol "[") (symbol "]") parser))

(defun semicolon ()
  "Parse semicolon."
  (symbol ";"))

(defun comma ()
  "Parse comma."
  (symbol ","))

(defun star ()
  "Parse asterisk."
  (symbol "*"))

(defun ampersand ()
  "Parse ampersand."
  (symbol "&"))

;; Higher-level combinators
(defun parameter-list (parameter-parser)
  "Parse comma-separated parameter list in parentheses."
  (parens
   (p:optional
    (p:sepBy1 parameter-parser (comma))
    '())))

(defun identifier-list ()
  "Parse comma-separated identifier list."
  (p:sepBy1 (c-identifier) (comma)))

(defun declaration-specifiers ()
  "Parse declaration specifiers (storage class, type specifiers, qualifiers)."
  (p:many1
   (p:choice
    (storage-class)
    (type-specifier)
    (type-qualifier)
    ;; struct/union/enum specifiers would go here
    (c-identifier)))) ; typedef-name

;; Utility functions for working with parse results
(defun extract-token-value (result)
  "Extract token value from parse result."
  (if (p:success-p result)
      (lexer:token-value (p:success-value result))
      nil))

(defun extract-values (result)
  "Extract list of values from parse result."
  (if (p:success-p result)
      (mapcar #'lexer:token-value (p:success-value result))
      nil))
