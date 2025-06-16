(defpackage :epsilon.lib.c-parser
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:str :epsilon.lib.string)
   (:seq :epsilon.lib.sequence))
  (:export
   :parse-clang-output
   :make-type-database
   :get-types
   :get-functions
   :get-typedefs))

(in-package :epsilon.lib.c-parser)

;; Token types
(defstruct token
  type
  value
  position)

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

;; Lexer
(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun alpha-p (char)
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (char= char #\_)))

(defun alphanum-p (char)
  (or (alpha-p char)
      (digit-char-p char)))

(defun tokenize (text)
  "Split text into tokens"
  (let ((tokens '())
        (pos 0)
        (len (length text)))
    (flet ((%peek (&optional (offset 0))
             (when (< (+ pos offset) len)
               (char text (+ pos offset))))
           (advance ()
             (incf pos))
           (read-while (predicate)
             (let ((start pos))
               (loop while (and (< pos len) (funcall predicate (%peek)))
                     do (advance))
               (subseq text start pos))))
      
      (loop while (< pos len)
            do (let ((ch (%peek)))
                 (cond
                   ;; Skip whitespace
                   ((whitespace-p ch)
                    (advance))
                   
                   ;; Identifiers and keywords
                   ((alpha-p ch)
                    (let ((word (read-while #'alphanum-p)))
                      (push (make-token :type (if (member word '("typedef" "struct" "union" "enum" "static" "inline" "const" "volatile" "restrict" "extern") :test #'string=)
                                                  :keyword
                                                  :identifier)
                                        :value word
                                        :position pos)
                            tokens)))
                   
                   ;; Numeric literals
                   ((digit-char-p ch)
                    (let ((number (read-while (lambda (c) (or (digit-char-p c) (char= c #\.))))))
                      (push (make-token :type :number
                                        :value number
                                        :position pos)
                            tokens)))
                   
                   ;; String literals
                   ((char= ch #\")
                    (advance) ; skip opening quote
                    (let ((start pos))
                      (loop while (and (< pos len) (not (char= (%peek) #\")))
                            do (if (char= (%peek) #\\)
                                   (progn (advance) (advance)) ; skip escaped char
                                   (advance)))
                      (when (< pos len) (advance)) ; skip closing quote
                      (push (make-token :type :string
                                        :value (subseq text start (1- pos))
                                        :position pos)
                            tokens)))
                   
                   ;; Single character tokens
                   (t
                    (let ((token-type
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
                      (push (make-token :type token-type
                                        :value (string ch)
                                        :position pos)
                            tokens)
                      (advance))))))
      (nreverse tokens))))

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
                  (eq (token-type (peek-token)) type)))
           (match-value (value)
             (and (peek-token)
                  (string= (token-value (peek-token)) value))))
      
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
                       (eq (token-type (peek-token 1)) :identifier)
                       (eq (token-type (peek-token 2)) :lparen))
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
               (string= (token-value (nth pos tokens)) "typedef"))
      (incf pos) ; skip 'typedef'
      
      ;; Simple case: typedef existing_type new_name;
      (when (and (< (+ pos 2) (length tokens))
                 (eq (token-type (nth pos tokens)) :identifier)
                 (eq (token-type (nth (1+ pos) tokens)) :identifier)
                 (eq (token-type (nth (+ pos 2) tokens)) :semicolon))
        (let ((underlying-type (token-value (nth pos tokens)))
              (new-name (token-value (nth (1+ pos) tokens))))
          (list :type :typedef
                :name new-name
                :underlying-type underlying-type
                :end-pos (+ pos 3)))))))

(defun parse-struct (tokens start-pos)
  "Parse a struct declaration"
  (let ((pos start-pos))
    (when (and (< pos (length tokens))
               (string= (token-value (nth pos tokens)) "struct"))
      (incf pos) ; skip 'struct'
      
      ;; Get struct name if present
      (let ((struct-name nil))
        (when (and (< pos (length tokens))
                   (eq (token-type (nth pos tokens)) :identifier))
          (setf struct-name (token-value (nth pos tokens)))
          (incf pos))
        
        ;; Look for opening brace
        (when (and (< pos (length tokens))
                   (eq (token-type (nth pos tokens)) :lbrace))
          (incf pos) ; skip '{'
          
          ;; Parse fields (simplified)
          (let ((fields '()))
            (loop while (and (< pos (length tokens))
                             (not (eq (token-type (nth pos tokens)) :rbrace)))
                  do (when (eq (token-type (nth pos tokens)) :identifier)
                       (let ((field-type (token-value (nth pos tokens))))
                         (incf pos)
                         (when (and (< pos (length tokens))
                                    (eq (token-type (nth pos tokens)) :identifier))
                           (let ((field-name (token-value (nth pos tokens))))
                             (push (list :name field-name :type field-type) fields)
                             (incf pos)))))
                  ;; Skip to next statement
                  while (and (< pos (length tokens))
                             (not (eq (token-type (nth pos tokens)) :semicolon)))
                  do (incf pos)
                  when (< pos (length tokens))
                  do (incf pos)) ; skip semicolon
            
            ;; Skip closing brace
            (when (and (< pos (length tokens))
                       (eq (token-type (nth pos tokens)) :rbrace))
              (incf pos))
            
            ;; Skip optional semicolon
            (when (and (< pos (length tokens))
                       (eq (token-type (nth pos tokens)) :semicolon))
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
               (eq (token-type (nth pos tokens)) :identifier))
      (let ((return-type (token-value (nth pos tokens))))
        (incf pos)
        
        ;; Function name
        (when (and (< pos (length tokens))
                   (eq (token-type (nth pos tokens)) :identifier))
          (let ((func-name (token-value (nth pos tokens))))
            (incf pos)
            
            ;; Opening parenthesis
            (when (and (< pos (length tokens))
                       (eq (token-type (nth pos tokens)) :lparen))
              (incf pos)
              
              ;; Parse parameters (simplified)
              (let ((params '()))
                (loop while (and (< pos (length tokens))
                                 (not (eq (token-type (nth pos tokens)) :rparen)))
                      do (when (eq (token-type (nth pos tokens)) :identifier)
                           (let ((param-type (token-value (nth pos tokens))))
                             (incf pos)
                             (when (and (< pos (length tokens))
                                        (eq (token-type (nth pos tokens)) :identifier))
                               (let ((param-name (token-value (nth pos tokens))))
                                 (push (list :name param-name :type param-type) params)
                                 (incf pos)))))
                      ;; Skip commas and other tokens
                      while (and (< pos (length tokens))
                                 (not (member (token-type (nth pos tokens)) '(:rparen :identifier))))
                      do (incf pos))
                
                ;; Skip closing parenthesis
                (when (and (< pos (length tokens))
                           (eq (token-type (nth pos tokens)) :rparen))
                  (incf pos))
                
                ;; Skip to semicolon
                (loop while (and (< pos (length tokens))
                                 (not (eq (token-type (nth pos tokens)) :semicolon)))
                      do (incf pos))
                
                ;; Skip semicolon
                (when (and (< pos (length tokens))
                           (eq (token-type (nth pos tokens)) :semicolon))
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
        (let* ((tokens (tokenize section))
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
