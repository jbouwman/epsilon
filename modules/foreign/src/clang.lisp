(defpackage epsilon.clang
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames 
   (p epsilon.parser)
   (lexer epsilon.lexer)
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:shadow
   keyword
   symbol)
  (:export
   tokenize
   parse
   struct-specifier
   translation-unit
   external-declaration))

(in-package epsilon.clang)

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
  (p:bind ((token (identifier)))
    (p:return (lexer:token-value token))))

(defun c-keyword (name)
  "Parse C keyword, returning the keyword name."
  (p:bind ((token (keyword name)))
    (p:return (lexer:token-value token))))

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
    (p:sep+ parameter-parser (comma))
    '())))

(defun identifier-list ()
  "Parse comma-separated identifier list."
  (p:sep+ (c-identifier) (comma)))

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


;; AST node constructors
(defun make-c-declaration (type &rest args)
  "Create a C declaration AST node."
  (list* :type type args))

(defun make-c-typedef (name underlying-type)
  "Create typedef AST node."
  (make-c-declaration :typedef :name name :underlying-type underlying-type))

(defun make-c-struct (name fields)
  "Create struct AST node."
  (make-c-declaration :struct :name name :fields fields))

(defun make-c-union (name fields)
  "Create union AST node."
  (make-c-declaration :union :name name :fields fields))

(defun make-c-enum (name values)
  "Create enum AST node."
  (make-c-declaration :enum :name name :values values))

(defun make-c-function (name return-type parameters)
  "Create function AST node."
  (make-c-declaration :function :name name :return-type return-type :parameters parameters))

(defun make-c-variable (name type)
  "Create variable AST node."
  (make-c-declaration :variable :name name :type type))

;; Type specifiers
(defun basic-type-specifier ()
  "Parse basic type specifiers."
  (p:choice
   (type-specifier)
   (struct-or-union-specifier)
   (enum-specifier)
   (typedef-name)))

(defun struct-or-union-specifier ()
  "Parse struct or union specifier."
  (p:choice
   (struct-specifier)
   (union-specifier)))

(defun struct-specifier ()
  "Parse struct specifier."
  (p:bind ((_ (c-keyword "struct"))
            (name (p:optional (c-identifier))))
    (p:choice
     ;; struct name { fields }
     (p:bind ((fields (braces (struct-declaration-list))))
       (p:return (make-c-struct name fields)))
     ;; struct name (forward declaration or reference)
     (p:return (make-c-struct name '())))))

(defun union-specifier ()
  "Parse union specifier."
  (p:bind ((_ (c-keyword "union"))
            (name (p:optional (c-identifier))))
    (p:choice
     ;; union name { fields }
     (p:bind ((fields (braces (struct-declaration-list))))
       (p:return (make-c-union name fields)))
     ;; union name
     (p:return (make-c-union name '())))))

(defun enum-specifier ()
  "Parse enum specifier."
  (p:bind ((_ (c-keyword "enum"))
            (name (p:optional (c-identifier))))
    (p:choice
     ;; enum name { values }
     (p:bind ((values (braces (enumerator-list))))
       (p:return (make-c-enum name values)))
     ;; enum name
     (p:return (make-c-enum name '())))))

(defun typedef-name ()
  "Parse typedef name (simplified - just identifier for now)."
  (c-identifier))

(defun struct-declaration-list ()
  "Parse list of struct declarations."
  (p:many (struct-declaration)))

(defun struct-declaration ()
  "Parse single struct field declaration."
  (p:bind ((spec (declaration-specifiers))
            (declarators (p:sep+ (struct-declarator) (comma)))
            (_ (semicolon)))
    (p:return (list :field-declaration 
                    :specifiers spec 
                    :declarators declarators))))

(defun struct-declarator ()
  "Parse struct declarator (simplified)."
  (p:choice
   (simple-declarator)
   ;; Bit fields: declarator : constant-expression
   (p:bind ((decl (simple-declarator))
             (_ (symbol ":"))
             (width (constant-expression)))
     (p:return (list :bit-field :declarator decl :width width)))))

(defun enumerator-list ()
  "Parse comma-separated list of enumerators."
  (p:sep+ (enumerator) (comma)))

(defun enumerator ()
  "Parse single enumerator."
  (p:bind ((name (c-identifier)))
    (p:choice
     ;; name = value
     (p:bind ((_ (symbol "="))
               (value (constant-expression)))
       (p:return (list name value)))
     ;; just name
     (p:return (list name nil)))))

;; Declarators
(defun simple-declarator ()
  "Parse simple declarator (just identifier for now)."
  (c-identifier))

(defun parameter-type-list ()
  "Parse parameter type list."
  (p:sep+ (parameter-declaration) (comma)))

(defun parameter-declaration ()
  "Parse parameter declaration."
  (p:bind ((spec (declaration-specifiers))
            (decl (p:optional (simple-declarator))))
    (p:return (list :parameter :specifiers spec :declarator decl))))

;; Expressions (simplified)
(defun constant-expression ()
  "Parse constant expression (simplified to just numbers and identifiers)."
  (p:choice
   (number-literal)
   (c-identifier)))

;; Top-level declarations
(defun type-specifiers-only ()
  "Parse type specifiers and qualifiers (no storage class)."
  (p:many1
   (p:choice
    (type-specifier)
    (type-qualifier)
    (c-identifier)))) ; typedef-name

(defun typedef-declaration ()
  "Parse typedef declaration."
  (p:bind ((_ (c-keyword "typedef"))
           (spec (type-specifiers-only))
           (names (p:sep+ (simple-declarator) (comma)))
           (_ (semicolon)))
    (p:return
      (mapcar (lambda (name)
                (make-c-typedef name spec))
              names))))

(defun function-declaration ()
  "Parse function declaration."
  (p:bind ((return-type (declaration-specifiers))
            (name (simple-declarator))
            (params (parameter-list (parameter-declaration)))
            (_ (semicolon)))
    (p:return (make-c-function name return-type params))))

(defun variable-declaration ()
  "Parse variable declaration."
  (p:bind ((type (declaration-specifiers))
           (names (p:sep+ (simple-declarator) (comma)))
           (_ (semicolon)))
    (p:return (mapcar (lambda (name)
                        (make-c-variable name type))
                      names))))

(defun external-declaration ()
  "Parse top-level declaration."
  (p:choice
   (typedef-declaration)
   (struct-specifier)
   (union-specifier) 
   (enum-specifier)
   (function-declaration)
   (variable-declaration)))

(defun translation-unit ()
  "Parse translation unit (sequence of external declarations)."
  (p:many (external-declaration)))

;; AST node types (using lists instead of structs to avoid duplication)

(defun c-typedef-p (node)
  "Check if node is a typedef"
  (and (listp node) (eq (getf node :type) :typedef)))

(defun c-function-p (node)
  "Check if node is a function"
  (and (listp node) (eq (getf node :type) :function)))

(defun c-struct-p (node)
  "Check if node is a struct"
  (and (listp node) (eq (getf node :type) :struct)))

(defun c-union-p (node)
  "Check if node is a union"
  (and (listp node) (eq (getf node :type) :union)))

(defun c-enum-p (node)
  "Check if node is an enum"
  (and (listp node) (eq (getf node :type) :enum)))

(defun c-variable-p (node)
  "Check if node is a variable"
  (and (listp node) (eq (getf node :type) :variable)))

;; Database structure

(defstruct type-database
  (typedefs map:+empty+)
  (functions map:+empty+)
  (structs map:+empty+)
  (unions map:+empty+)
  (enums map:+empty+))

;; Character classification utilities

(defun alpha-p (char)
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (char= char #\_)))

(defun alphanum-p (char)
  (or (alpha-p char)
      (digit-char-p char)))

;; Token recognizer functions using epsilon.lexer

(defun recognize-identifier-or-keyword (lexer)
  "Recognize identifier or keyword from lexer"
  (when (and (not (lexer:at-end-p lexer))
             (alpha-p (lexer:peek lexer)))
    (let ((word (lexer:consume-while lexer #'alphanum-p)))
      (lexer:make-token lexer 
                        (if (member word '("typedef" "struct" "union" "enum" "static" "inline" "const" "volatile" "restrict" "extern"
                                           "void" "char" "short" "int" "long" "float" "double" "signed" "unsigned"
                                           "_Bool" "_Complex" "_Imaginary" "auto" "register") :test #'string=)
                            :keyword
                            :identifier)
                        word))))

(defun recognize-number (lexer)
  "Recognize numeric literal from lexer"
  (when (and (not (lexer:at-end-p lexer))
             (digit-char-p (lexer:peek lexer)))
    (let ((number (lexer:consume-while lexer (lambda (ch)
                                               (or (digit-char-p ch) (char= ch #\.))))))
      (lexer:make-token lexer :number number))))

(defun recognize-string (lexer)
  "Recognize string literal from lexer"
  (when (and (not (lexer:at-end-p lexer))
             (char= (lexer:peek lexer) #\"))
    (lexer:next lexer) ; consume opening quote
    (let ((string-content (with-output-to-string (s)
                            (loop while (and (not (lexer:at-end-p lexer))
                                             (not (char= (lexer:peek lexer) #\")))
                                  do (let ((ch (lexer:peek lexer)))
                                       (if (char= ch #\\)
                                           (progn
                                             (lexer:next lexer) ; consume backslash
                                             (when (not (lexer:at-end-p lexer))
                                               (write-char (lexer:next lexer) s))) ; consume escaped char
                                           (write-char (lexer:next lexer) s)))))))
      (when (not (lexer:at-end-p lexer)) ; consume closing quote if present
        (lexer:next lexer))
      (lexer:make-token lexer :string string-content))))

(defun recognize-single-char (lexer)
  "Recognize single character token from lexer"
  (when (not (lexer:at-end-p lexer))
    (let ((ch (lexer:next lexer)))
      (let ((lexer:token-type
              (case ch
                (#\( :lparen)
                (#\) :rparen)
                (#\{ :lbrace)
                (#\} :rbrace)
                (#\[ :lbracket)
                (#\] :rbracket)
                (#\; :semicolon)
                (#\, :comma)
                (#\* :star)
                (#\& :ampersand)
                (#\= :equals)
                (#\< :less)
                (#\> :greater)
                (t :other))))
        (lexer:make-token lexer lexer:token-type (string ch))))))

(defun tokenize (stream)
  "Split character stream into lazy sequence of tokens using epsilon.lexer"
  (let ((lexer (lexer:make-lexer stream)))
    (labels ((next-token ()
               (lexer:skip-whitespace lexer)
               (when (not (lexer:at-end-p lexer))
                 (let ((ch (lexer:peek lexer)))
                   (cond
                     ;; Identifiers and keywords
                     ((alpha-p ch)
                      (recognize-identifier-or-keyword lexer))
                     
                     ;; Numeric literals
                     ((digit-char-p ch)
                      (recognize-number lexer))
                     
                     ;; String literals
                     ((char= ch #\")
                      (recognize-string lexer))
                     
                     ;; Single character tokens
                     (t
                      (recognize-single-char lexer))))))
             (token-stream ()
               (let ((token (next-token)))
                 (if token
                     (seq:cons token (token-stream))
                     seq:*empty*))))
      (token-stream))))

(defun read-sections (text)
  "Split text by lines starting with #, returning lazy sequence of sections"
  (let ((lines (str:split #\newline text)))
    (seq:map (lambda (section-lines)
               (map:make-map :source (seq:first section-lines)
                             :text (str:join #\newline (seq:rest section-lines))))
             (seq:partition-when (lambda (line)
                                   (and (> (length line) 0)
                                        (char= (char line 0) #\#)))
                                 lines))))
