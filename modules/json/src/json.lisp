;;;; JSON parser using parser combinators with tokenization

(defpackage epsilon.json
  (:use cl)
  (:local-nicknames
   (p epsilon.parser)
   (seq epsilon.sequence)
   (lexer epsilon.lexer))
  (:export
   encode
   decode
   tokenize
   parse))

(in-package :epsilon.json)

;;; Tokenizer

(deftype json-token-type ()
  '(member :string :number :true :false :null
    :lbrace :rbrace :lbracket :rbracket
    :comma :colon :eof))

(defun tokenize-string (lexer)
  "Tokenize a JSON string literal from LEXER, handling escape sequences.
   Example: tokenizes \"hello\" to (:string . \"hello\")"
  (multiple-value-bind (start-pos start-line start-column)
      (lexer:lexer-position lexer)
    (unless (char= (lexer:next lexer) #\")
      (lexer:lexer-error lexer "Expected string"))
    
    (let ((value (with-output-to-string (s)
                   (loop for ch = (lexer:next lexer)
                         do (case ch
                              ((nil) (lexer:lexer-error lexer "Unterminated string"))
                              (#\" (return))
                              (#\\ (let ((next (lexer:next lexer)))
                                     (case next
                                       (#\" (write-char #\" s))
                                       (#\\ (write-char #\\ s))
                                       (#\/ (write-char #\/ s))
                                       (#\b (write-char #\Backspace s))
                                       (#\f (write-char #\Page s))
                                       (#\n (write-char #\Newline s))
                                       (#\r (write-char #\Return s))
                                       (#\t (write-char #\Tab s))
                                       (#\u (let ((code (parse-integer 
                                                         (with-output-to-string (hex)
                                                           (dotimes (i 4)
                                                             (write-char (or (lexer:next lexer)
                                                                             (lexer:lexer-error lexer "Invalid unicode escape"))
                                                                         hex)))
                                                         :radix 16)))
                                              (write-char (code-char code) s)))
                                       (otherwise (lexer:lexer-error lexer "Invalid escape sequence")))))
                              (otherwise (write-char ch s)))))))
      (lexer:%make-token :type :string 
                         :value value 
                         :position start-pos
                         :line start-line
                         :column start-column))))

(defun tokenize-number (lexer)
  "Tokenize a JSON number."
  (multiple-value-bind (start-pos start-line start-column)
      (lexer:lexer-position lexer)
    (let ((has-decimal nil)
          (has-exponent nil))
      
      (let ((number-str (with-output-to-string (s)
                          ;; Sign
                          (let ((ch (lexer:peek lexer)))
                            (when (and ch (char= ch #\-))
                              (write-char (lexer:next lexer) s)))
                          
                          ;; Integer part
                          (loop for ch = (lexer:peek lexer)
                                while (and ch (digit-char-p ch))
                                do (write-char (lexer:next lexer) s))
                          
                          ;; Decimal part
                          (let ((ch (lexer:peek lexer)))
                            (when (and ch (char= ch #\.))
                              (write-char (lexer:next lexer) s)
                              (setf has-decimal t)
                              (loop for ch = (lexer:peek lexer)
                                    while (and ch (digit-char-p ch))
                                    do (write-char (lexer:next lexer) s))))
                          
                          ;; Exponent
                          (let ((ch (lexer:peek lexer)))
                            (when (and ch (member ch '(#\e #\E)))
                              (write-char (lexer:next lexer) s)
                              (setf has-exponent t)
                              (let ((ch (lexer:peek lexer)))
                                (when (and ch (member ch '(#\+ #\-)))
                                  (write-char (lexer:next lexer) s)))
                              (loop for ch = (lexer:peek lexer)
                                    while (and ch (digit-char-p ch))
                                    do (write-char (lexer:next lexer) s)))))))
        
        (let ((value (if (or has-decimal has-exponent)
                         (read-from-string number-str)
                         (parse-integer number-str))))
          (lexer:%make-token :type :number 
                             :value value 
                             :position start-pos
                             :line start-line
                             :column start-column))))))

(defun tokenize-keyword (lexer keyword token-type)
  "Tokenize a JSON keyword (true, false, null)."
  (multiple-value-bind (start-pos start-line start-column)
      (lexer:lexer-position lexer)
    (unless (lexer:consume-string lexer keyword)
      (lexer:lexer-error lexer (format nil "Expected '~A'" keyword)))
    (lexer:%make-token :type token-type 
                       :value (case token-type
                                (:true t)
                                (:false nil)
                                (:null nil))
                       :position start-pos
                       :line start-line
                       :column start-column)))

(defun tokenize-syntax (lexer type value)
  (prog1
      (lexer:make-token lexer type value)
    (lexer:next lexer)))

(defun next-token (lexer)
  "Get the next JSON token from the lexer."
  (lexer:skip-whitespace lexer)
  (if (lexer:at-end-p lexer)
      (lexer:make-token lexer :eof nil)
      (let ((ch (lexer:peek lexer)))
        (case ch
          (#\" (tokenize-string lexer))
          (#\{ (tokenize-syntax lexer :lbrace #\{))
          (#\} (tokenize-syntax lexer :rbrace #\}))
          (#\[ (tokenize-syntax lexer :lbracket #\[))
          (#\] (tokenize-syntax lexer :rbracket #\]))
          (#\, (tokenize-syntax lexer :comma #\,))
          (#\: (tokenize-syntax lexer :colon #\:))
          (#\t (tokenize-keyword lexer "true" :true))
          (#\f (tokenize-keyword lexer "false" :false))
          (#\n (tokenize-keyword lexer "null" :null))
          ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (tokenize-number lexer))
          (otherwise
           (lexer:lexer-error lexer (format nil "Unexpected character: ~A" ch)))))))

(defun tokenize (input)
  "Tokenize JSON input into a lazy sequence of tokens."
  (let ((lexer (if (stringp input)
                   (lexer:make-lexer (make-string-input-stream input))
                   (lexer:make-lexer input))))
    (labels ((token-stream ()
               (let ((token (next-token lexer)))
                 (if (eq (lexer:token-type token) :eof)
                     (seq:cons token seq:*empty*)
                     (seq:cons token (token-stream))))))
      (token-stream))))

;;;; Parser

(defun token-p (type)
  "Parse a token of the expected type."
  (p:satisfy (lambda (token)
               (eq (lexer:token-type token) type))
             (format nil "token ~A" type)))

(defun json-atom (type)
  (p:bind ((token (token-p type)))
    (p:return (lexer:token-value token))))

(defun json-array ()
  "Parse JSON array."
  (p:bind ((_ (token-p :lbracket)))
    (p:choice (p:bind ((_ (token-p :rbracket)))
                (p:return '()))
              (p:bind ((values (p:sep+ (json-value)
                                       (token-p :comma)))
                       (_ (token-p :rbracket)))
                (p:return values)))))

(defun json-pair ()
  "Parse JSON key-value pair."
  (p:bind ((key (json-atom :string))
           (_ (token-p :colon))
           (value (json-value)))
    (p:return (cons key value))))

(defun json-object ()
  "Parse JSON object."
  (p:bind ((_ (token-p :lbrace)))
    (p:choice (p:bind ((_ (token-p :rbrace)))
                (p:return '()))
              (p:bind ((pairs (p:sep+ (json-pair)
                                      (token-p :comma)))
                       (_ (token-p :rbrace)))
                (p:return pairs)))))

(defun json-value ()
  "Parse any JSON value."
  (p:choice (json-atom :string)
            (json-atom :number)
            (json-object)
            (json-array)
            (json-atom :true)
            (json-atom :false)
            (json-atom :null)))

(defun json-document ()
  "Parse complete JSON document."
  (p:bind ((value (json-value))
           (_ (token-p :eof)))
    (p:return value)))

;; Public API

(define-condition json-parse-error (error)
  ((message :initarg :message :reader error-message)
   (position :initarg :position :reader error-position :initform nil))
  (:report (lambda (condition stream)
             (format stream "JSON parse error: ~A" 
                     (error-message condition)))))

(defun parse (input)
  "Parse JSON from character sequence or string.
   Returns parsed value or signals error on parse failure."
  (let* ((tokens (tokenize input))
         (result (p:parse (json-document) tokens)))
    (if (p:success-p result)
        (p:success-value result)
        (error 'json-parse-error :message
               (p:failure-message result)))))

(defun decode (input)
  "Parse JSON from character sequence or string (alias for parse)"
  (parse input))

(defun parse-file (filename)
  "Parse JSON from a file."
  (with-open-file (stream filename :direction :input)
    (parse stream)))

;;; JSON Encoding

(defun write-json-string (string stream)
  "Write a JSON-escaped string to stream"
  (write-char #\" stream)
  (loop for char across string do
    (case char
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (#\Backspace (write-string "\\b" stream))
      (#\Page (write-string "\\f" stream))
      (t (write-char char stream))))
  (write-char #\" stream))

(defun write-json-value (value stream &key pretty (indent 0))
  "Write a Lisp value as JSON to stream"
  (let ((indent-str (when pretty (make-string (* indent 2) :initial-element #\Space))))
    (typecase value
      (null (write-string "null" stream))
      ((eql t) (write-string "true" stream))
      (string (write-json-string value stream))
      (number (princ value stream))
      (list 
       (cond
         ;; Check if it's an alist (association list)
         ((and (consp value) (consp (first value)) (atom (car (first value))))
          ;; Treat as object
          (write-char #\{ stream)
          (when pretty (terpri stream))
          (loop for (key . val) in value
                for first-p = t then nil
                unless first-p do
                  (write-char #\, stream)
                  (when pretty (terpri stream))
                do
                  (when pretty
                    (write-string (make-string (* (1+ indent) 2) :initial-element #\Space) stream))
                  (write-json-string (string key) stream)
                  (write-char #\: stream)
                  (when pretty (write-char #\Space stream))
                  (write-json-value val stream :pretty pretty :indent (1+ indent)))
          (when pretty
            (terpri stream)
            (write-string indent-str stream))
          (write-char #\} stream))
         ;; Regular list - treat as array
         (t
          (write-char #\[ stream)
          (when pretty (terpri stream))
          (loop for item in value
                for first-p = t then nil
                unless first-p do
                  (write-char #\, stream)
                  (when pretty (terpri stream))
                do
                  (when pretty
                    (write-string (make-string (* (1+ indent) 2) :initial-element #\Space) stream))
                  (write-json-value item stream :pretty pretty :indent (1+ indent)))
          (when pretty
            (terpri stream)
            (write-string indent-str stream))
          (write-char #\] stream))))
      ;; Handle epsilon maps
      (t
       (if (and (fboundp 'epsilon.map:map-p) 
                (funcall (symbol-function 'epsilon.map:map-p) value))
           ;; It's an epsilon map - convert to object
           (let ((pairs (coerce (funcall (symbol-function 'epsilon.map:seq) value) 'list)))
             (write-char #\{ stream)
             (when pretty (terpri stream))
             (loop for pair in pairs
                   for first-p = t then nil
                   unless first-p do
                     (write-char #\, stream)
                     (when pretty (terpri stream))
                   do
                     (when pretty
                       (write-string (make-string (* (1+ indent) 2) :initial-element #\Space) stream))
                     (write-json-string (string (car pair)) stream)
                     (write-char #\: stream)
                     (when pretty (write-char #\Space stream))
                     (write-json-value (cdr pair) stream :pretty pretty :indent (1+ indent)))
             (when pretty
               (terpri stream)
               (write-string indent-str stream))
             (write-char #\} stream))
           ;; Unknown type - convert to string
           (write-json-string (princ-to-string value) stream))))))

(defun encode (value stream &key pretty)
  "Encode a Lisp value as JSON to a stream"
  (write-json-value value stream :pretty pretty)
  (when pretty (terpri stream)))

(defun encode-to-string (value &key pretty)
  "Encode a Lisp value as JSON to a string"
  (with-output-to-string (stream)
    (encode value stream :pretty pretty)))
