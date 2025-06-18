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
   :make-type-database
   :get-types
   :get-functions
   :get-typedefs))

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
                        (if (member word '("typedef" "struct" "union" "enum" "static" "inline" "const" "volatile" "restrict" "extern") :test #'string=)
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
  "Split character stream into tokens using epsilon.lib.lexer"
  (let ((lexer (lexer:make-lexer stream))
        (tokens '()))
    (loop while (not (lexer:at-end-p lexer))
          do (progn
               ;; Skip whitespace
               (lexer:skip-whitespace lexer)
               (when (not (lexer:at-end-p lexer))
                 (let ((ch (lexer:peek lexer)))
                   (cond
                     ;; Identifiers and keywords
                     ((alpha-p ch)
                      (let ((token (recognize-identifier-or-keyword lexer)))
                        (when token (push token tokens))))
                     
                     ;; Numeric literals
                     ((digit-char-p ch)
                      (let ((token (recognize-number lexer)))
                        (when token (push token tokens))))
                     
                     ;; String literals
                     ((char= ch #\")
                      (let ((token (recognize-string lexer)))
                        (when token (push token tokens))))
                     
                     ;; Single character tokens
                     (t
                      (let ((token (recognize-single-char lexer)))
                        (when token (push token tokens)))))))))
    (nreverse tokens)))

;; Parser
(defun parse-section (tokens)
  "Parse a section of tokens into C declarations"
  (let ((declarations '())
        (pos 0))
    (flet ((peek-token (&optional (offset 0))
             (when (< (+ pos offset) (length tokens))
               (nth (+ pos offset) tokens)))
           (consume-token ()
             (prog1 (peek-token)
               (incf pos)))
           (match-token (type)
             (and (peek-token)
                  (eq (lexer:token-type (peek-token)) type)))
           (match-value (value)
             (and (peek-token)
                  (string= (lexer:token-value (peek-token)) value))))
      
      (loop while (< pos (length tokens))
            do (cond
                 ;; typedef
                 ((match-value "typedef")
                  (let ((typedef (parse-typedef tokens pos)))
                    (when typedef
                      (push typedef declarations)
                      (setf pos (getf typedef :end-pos)))))
                 
                 ;; struct
                 ((match-value "struct")
                  (let ((struct (parse-struct tokens pos)))
                    (when struct
                      (push struct declarations)
                      (setf pos (getf struct :end-pos)))))
                 
                 ;; function declarations
                 ((and (peek-token)
                       (peek-token 1)
                       (peek-token 2)
                       (eq (lexer:token-type (peek-token 1)) :identifier)
                       (eq (lexer:token-type (peek-token 2)) :lparen))
                  (let ((func (parse-function tokens pos)))
                    (when func
                      (push func declarations)
                      (setf pos (getf func :end-pos)))))
                 
                 ;; Skip unrecognized tokens
                 (t (incf pos))))
      declarations)))

(defun parse-typedef (tokens start-pos)
  "Parse a typedef declaration"
  (let ((pos start-pos))
    (when (and (< pos (length tokens))
               (string= (lexer:token-value (nth pos tokens)) "typedef"))
      (incf pos) ; skip 'typedef'
      
      ;; Simple case: typedef existing_type new_name;
      (when (and (< (+ pos 2) (length tokens))
                 (eq (lexer:token-type (nth pos tokens)) :identifier)
                 (eq (lexer:token-type (nth (1+ pos) tokens)) :identifier)
                 (eq (lexer:token-type (nth (+ pos 2) tokens)) :semicolon))
        (let ((underlying-type (lexer:token-value (nth pos tokens)))
              (new-name (lexer:token-value (nth (1+ pos) tokens))))
          (list :type :typedef
                :name new-name
                :underlying-type underlying-type
                :end-pos (+ pos 3)))))))

(defun parse-struct (tokens start-pos)
  "Parse a struct declaration"
  (let ((pos start-pos))
    (when (and (< pos (length tokens))
               (string= (lexer:token-value (nth pos tokens)) "struct"))
      (incf pos) ; skip 'struct'
      
      ;; Get struct name if present
      (let ((struct-name nil))
        (when (and (< pos (length tokens))
                   (eq (lexer:token-type (nth pos tokens)) :identifier))
          (setf struct-name (lexer:token-value (nth pos tokens)))
          (incf pos))
        
        ;; Look for opening brace
        (when (and (< pos (length tokens))
                   (eq (lexer:token-type (nth pos tokens)) :lbrace))
          (incf pos) ; skip '{'
          
          ;; Parse fields (simplified)
          (let ((fields '()))
            (loop while (and (< pos (length tokens))
                             (not (eq (lexer:token-type (nth pos tokens)) :rbrace)))
                  do (when (eq (lexer:token-type (nth pos tokens)) :identifier)
                       (let ((field-type (lexer:token-value (nth pos tokens))))
                         (incf pos)
                         (when (and (< pos (length tokens))
                                    (eq (lexer:token-type (nth pos tokens)) :identifier))
                           (let ((field-name (lexer:token-value (nth pos tokens))))
                             (push (list :name field-name :type field-type) fields)
                             (incf pos)))))
                     ;; Skip to next statement
                  while (and (< pos (length tokens))
                             (not (eq (lexer:token-type (nth pos tokens)) :semicolon)))
                  do (incf pos)
                  when (< pos (length tokens))
                    do (incf pos)) ; skip semicolon
            
            ;; Skip closing brace
            (when (and (< pos (length tokens))
                       (eq (lexer:token-type (nth pos tokens)) :rbrace))
              (incf pos))
            
            ;; Skip optional semicolon
            (when (and (< pos (length tokens))
                       (eq (lexer:token-type (nth pos tokens)) :semicolon))
              (incf pos))
            
            (list :type :struct
                  :name struct-name
                  :fields (nreverse fields)
                  :end-pos pos)))))))

(defun parse-function (tokens start-pos)
  "Parse a function declaration"
  (let ((pos start-pos))
    ;; Return type
    (when (and (< pos (length tokens))
               (eq (lexer:token-type (nth pos tokens)) :identifier))
      (let ((return-type (lexer:token-value (nth pos tokens))))
        (incf pos)
        
        ;; Function name
        (when (and (< pos (length tokens))
                   (eq (lexer:token-type (nth pos tokens)) :identifier))
          (let ((func-name (lexer:token-value (nth pos tokens))))
            (incf pos)
            
            ;; Opening parenthesis
            (when (and (< pos (length tokens))
                       (eq (lexer:token-type (nth pos tokens)) :lparen))
              (incf pos)
              
              ;; Parse parameters (simplified)
              (let ((params '()))
                (loop while (and (< pos (length tokens))
                                 (not (eq (lexer:token-type (nth pos tokens)) :rparen)))
                      do (when (eq (lexer:token-type (nth pos tokens)) :identifier)
                           (let ((param-type (lexer:token-value (nth pos tokens))))
                             (incf pos)
                             (when (and (< pos (length tokens))
                                        (eq (lexer:token-type (nth pos tokens)) :identifier))
                               (let ((param-name (lexer:token-value (nth pos tokens))))
                                 (push (list :name param-name :type param-type) params)
                                 (incf pos)))))
                         ;; Skip commas and other tokens
                      while (and (< pos (length tokens))
                                 (not (member (lexer:token-type (nth pos tokens)) '(:rparen :identifier))))
                      do (incf pos))
                
                ;; Skip closing parenthesis
                (when (and (< pos (length tokens))
                           (eq (lexer:token-type (nth pos tokens)) :rparen))
                  (incf pos))
                
                ;; Skip to semicolon
                (loop while (and (< pos (length tokens))
                                 (not (eq (lexer:token-type (nth pos tokens)) :semicolon)))
                      do (incf pos))
                
                ;; Skip semicolon
                (when (and (< pos (length tokens))
                           (eq (lexer:token-type (nth pos tokens)) :semicolon))
                  (incf pos))
                
                (list :type :function
                      :name func-name
                      :return-type return-type
                      :parameters (nreverse params)
                      :end-pos pos)))))))))

;; Main parsing function
(defun parse-clang-output (text)
  "Parse clang -E output into a type database"
  (let ((database (make-type-database))
        (sections (split-by-preprocessor-lines text)))
    
    (dolist (section sections)
      (when (and section (> (length (string-trim '(#\space #\tab #\newline #\return) section)) 0))
        (let* ((tokens (tokenize (make-string-input-stream section)))
               (declarations (parse-section tokens)))
          
          (dolist (decl declarations)
            (case (getf decl :type)
              (:typedef
               (setf (type-database-typedefs database)
                     (map:assoc (type-database-typedefs database)
                                (getf decl :name)
                                decl)))
              (:struct
               (setf (type-database-structs database)
                     (map:assoc (type-database-structs database)
                                (getf decl :name)
                                decl)))
              (:function
               (setf (type-database-functions database)
                     (map:assoc (type-database-functions database)
                                (getf decl :name)
                                decl))))))))
    
    database))

(defun split-by-preprocessor-lines (text)
  "Split text by lines starting with #"
  (let ((lines (seq:realize (str:split #\newline text)))
        (sections '())
        (current-section '()))
    
    (dolist (line lines)
      (if (and (> (length line) 0) (char= (char line 0) #\#))
          ;; Preprocessor line - finish current section and start new one
          (progn
            (when current-section
              (push (str:join (string #\newline) (nreverse current-section)) sections)
              (setf current-section '()))
            ;; Don't include preprocessor lines in sections
            )
          ;; Regular line - add to current section  
          (push line current-section)))
    
    ;; Add final section
    (when current-section
      (push (str:join (string #\newline) (nreverse current-section)) sections))
    
    (nreverse sections)))

;; Convenience functions
(defun get-types (database)
  "Get all typedefs from database"
  (map:seq (type-database-typedefs database)))

(defun get-functions (database)  
  "Get all functions from database"
  (map:seq (type-database-functions database)))

(defun get-typedefs (database)
  "Get all typedefs from database"
  (map:seq (type-database-typedefs database)))
