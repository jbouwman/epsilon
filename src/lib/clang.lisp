(defpackage :epsilon.lib.clang
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:lexer :epsilon.lib.lexer)
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:seq :epsilon.lib.sequence))
  (:export
   :parse-clang-output
   :build-type-database
   :make-type-database))

(in-package :epsilon.lib.clang)

;; AST node types  

(defstruct c-typedef
  name
  underlying-type)

(defstruct c-function
  name
  return-type
  parameters)

(defstruct c-struct
  name
  fields)

(defstruct c-union
  name
  fields)

(defstruct c-enum
  name
  values)

(defstruct c-field
  name
  type)

(defstruct c-parameter
  name
  type)

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

;; Token recognizer functions using epsilon.lib.lexer

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
  "Split character stream into lazy sequence of tokens using epsilon.lib.lexer"
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
