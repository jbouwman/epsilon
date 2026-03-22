;;;; JSON parser using parser combinators with tokenization

(defpackage epsilon.json
  (:use :cl)
  (:require (epsilon.parser p)
            (epsilon.sequence seq)
            (epsilon.lexer lexer)
            (epsilon.map map))
  (:enter t))

;;; Dynamic variables for hooks during parsing/encoding

(defvar *object-hook* nil
  "Function called on each parsed object, enabling custom deserialization.
   If set, the function receives each parsed object (epsilon.map) and can
   transform it into a different representation.")

(defvar *default-encoder* nil
  "Function called for values that cannot be directly serialized to JSON.
   If set, receives the value and should return a JSON-serializable value.
   If the function returns the same value, it will be converted to string.")

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
      (lexer:make-token :type :string
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
          (lexer:make-token :type :number
                             :value value
                             :position start-pos
                             :line start-line
                             :column start-column))))))

(defun tokenize-keyword (lexer keyword token-type)
  "Tokenize a JSON keyword (true, false, null)."
  (multiple-value-bind (start-pos start-line start-column)
      (lexer:lexer-position lexer)
    (unless (lexer:consume-string lexer keyword)
      (lexer:lexer-error lexer #~"Expected '~{keyword}'"))
    (lexer:make-token :type token-type
                       :value (case token-type
                                (:true t)
                                (:false nil)
                                (:null nil))
                       :position start-pos
                       :line start-line
                       :column start-column)))

(defun tokenize-syntax (lexer type value)
  (prog1
      (lexer:make-token-here lexer type value)
    (lexer:next lexer)))

(defun next-token (lexer)
  "Get the next JSON token from the lexer."
  (lexer:skip-whitespace lexer)
  (if (lexer:at-end-p lexer)
      (lexer:make-token-here lexer :eof nil)
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
           (lexer:lexer-error lexer #~"Unexpected character: ~{ch}"))))))

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
             #~"token ~{type}"))

(defun json-atom (type)
  (p:bind ((token (token-p type)))
    (p:return (lexer:token-value token))))

(defun json-array ()
  "Parse JSON array. Returns a vector for consistent iteration with ACROSS."
  (p:bind ((_ (token-p :lbracket)))
    (p:choice (p:bind ((_ (token-p :rbracket)))
                (p:return #()))
              (p:bind ((values (p:sep+ (json-value)
                                       (token-p :comma)))
                       (_ (token-p :rbracket)))
                (p:return (coerce values 'vector))))))

(defun json-pair ()
  "Parse JSON key-value pair."
  (p:bind ((key (json-atom :string))
           (_ (token-p :colon))
           (value (json-value)))
    (p:return (cons key value))))

(defun json-object ()
  "Parse JSON object. Returns an epsilon.map for consistent access with map:get.
   If *object-hook* is bound, applies it to the parsed object."
  (p:bind ((_ (token-p :lbrace)))
    (p:choice (p:bind ((_ (token-p :rbrace)))
                (p:return (let ((obj (map:make-map)))
                            (if *object-hook*
                                (funcall *object-hook* obj)
                                obj))))
              (p:bind ((pairs (p:sep+ (json-pair)
                                      (token-p :comma)))
                       (_ (token-p :rbrace)))
                (p:return (let ((obj (reduce (lambda (m pair)
                                               (map:assoc m (car pair) (cdr pair)))
                                             pairs
                                             :initial-value (map:make-map))))
                            (if *object-hook*
                                (funcall *object-hook* obj)
                                obj)))))))

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

;;; Public API

(define-condition json-parse-error (error)
  ((message :initarg :message :reader error-message)
   (position :initarg :position :reader error-position :initform nil))
  (:report (lambda (condition stream)
             (let ((msg (error-message condition)))
               (write-string #~"JSON parse error: ~{msg}" stream)))))

(defun apply-object-hook (value)
  "Apply object hook to value if it's an object and hook is set."
  (if (and *object-hook* (map:map-p value))
      (funcall *object-hook* value)
      value))

(defun parse (input &key object-hook)
  "Parse JSON from character sequence or string.
   Returns parsed value or signals error on parse failure.

   Keyword arguments:
     :object-hook - Function called on each parsed object (epsilon.map).
                    Enables custom deserialization of objects.
                    Example: (json:parse str :object-hook #'convert-to-instance)

   Example:
     ;; Parse with custom object hook
     (json:parse json-string
       :object-hook (lambda (obj)
                      (if (map:get obj \"__type__\")
                          (deserialize-custom obj)
                          obj)))"
  (let* ((*object-hook* object-hook)
         (tokens (tokenize input))
         (result (p:parse (json-document) tokens)))
    (if (p:success-p result)
        (let ((value (p:success-value result)))
          (apply-object-hook value))
        (error 'json-parse-error :message
               (p:failure-message result)))))

(defun decode (input &key object-hook)
  "Parse JSON from character sequence or string (alias for parse)"
  (parse input :object-hook object-hook))

(defun parse-file (filename &key object-hook)
  "Parse JSON from a file."
  (with-open-file (stream filename :direction :input)
    (parse stream :object-hook object-hook)))

;;; Streaming Parser
;;;
;;; For processing large JSON files or streams containing multiple JSON values
;;; without loading everything into memory at once.

(defun stream-parse (input &key object-hook on-value on-error)
  "Parse multiple JSON values from a stream, calling ON-VALUE for each.
   This enables processing large files or continuous JSON streams efficiently.

   Arguments:
     INPUT     - Input stream or string
     :object-hook - Function called on each parsed object
     :on-value - Function called with each parsed JSON value (required)
     :on-error - Function called on parse errors; receives error condition.
                 If not provided, errors are signaled normally.

   Example:
     (json:stream-parse input-stream
       :on-value (lambda (obj) (process obj))
       :on-error (lambda (err) (log:warn \"Parse error: ~A\" err)))"
  (unless on-value
    (error "stream-parse requires :on-value callback"))
  (let* ((*object-hook* object-hook)
         (lexer (if (stringp input)
                    (lexer:make-lexer (make-string-input-stream input))
                    (lexer:make-lexer input))))
    (loop
      ;; Skip whitespace between values
      (lexer:skip-whitespace lexer)
      ;; Check for end of input
      (when (lexer:at-end-p lexer)
        (return))
      ;; Try to parse next value
      (handler-case
          (let* ((tokens (labels ((token-stream ()
                                    (let ((token (next-token lexer)))
                                      (if (eq (lexer:token-type token) :eof)
                                          (seq:cons token seq:*empty*)
                                          (seq:cons token (token-stream))))))
                           (token-stream)))
                 (result (p:parse (json-value) tokens)))
            (if (p:success-p result)
                (funcall on-value (apply-object-hook (p:success-value result)))
                (if on-error
                    (funcall on-error (make-condition 'json-parse-error
                                                      :message (p:failure-message result)))
                    (error 'json-parse-error :message (p:failure-message result)))))
        (error (e)
          (if on-error
              (funcall on-error e)
              (error e)))))))

(defun stream-parse-file (filename &key object-hook on-value on-error)
  "Parse multiple JSON values from a file, calling ON-VALUE for each.
   See STREAM-PARSE for full documentation."
  (with-open-file (stream filename :direction :input)
    (stream-parse stream
                  :object-hook object-hook
                  :on-value on-value
                  :on-error on-error)))

;;; JSON Lines (JSONL) Support
;;;
;;; JSON Lines is a format where each line is a valid JSON value.
;;; Common for log files, data pipelines, and streaming data.

(defun parse-lines (input &key object-hook)
  "Parse JSON Lines (JSONL) format, returning a lazy sequence of parsed values.
   Each line in the input should be a valid JSON value.
   Empty lines are skipped.

   Arguments:
     INPUT - Input stream or string
     :object-hook - Function called on each parsed object

   Returns:
     Lazy sequence of parsed JSON values

   Example:
     (seq:for-each (lambda (obj) (process obj))
       (json:parse-lines input-stream))"
  (let ((*object-hook* object-hook)
        (stream (if (stringp input)
                    (make-string-input-stream input)
                    input)))
    (labels ((read-lines ()
               (let ((line (read-line stream nil nil)))
                 (cond
                   ((null line) seq:*empty*)
                   ((string= (string-trim '(#\Space #\Tab #\Return) line) "")
                    (read-lines))  ; Skip empty lines
                   (t
                    (seq:cons (parse line :object-hook *object-hook*)
                              (read-lines)))))))
      (read-lines))))

(defun parse-lines-file (filename &key object-hook)
  "Parse JSON Lines from a file, returning a list.
   See PARSE-LINES for full documentation."
  (with-open-file (stream filename :direction :input)
    ;; Force the lazy sequence since the file will be closed
    (seq:realize (parse-lines stream :object-hook object-hook))))

(defun encode-lines (values stream &key default)
  "Encode a sequence of values as JSON Lines format.
   Each value is written as a single JSON line.

   Arguments:
     VALUES - Sequence of values to encode (list, vector, or lazy sequence)
     STREAM - Output stream
     :default - Function to encode non-standard types

   Example:
     (with-open-file (out \"data.jsonl\" :direction :output)
       (json:encode-lines objects out))"
  (etypecase values
    (list
     (dolist (value values)
       (encode value stream :default default)
       (terpri stream)))
    (vector
     (loop for value across values do
       (encode value stream :default default)
       (terpri stream)))
    (t  ; Assume lazy sequence
     (seq:each (lambda (value)
                 (encode value stream :default default)
                 (terpri stream))
               values))))

(defun encode-lines-to-string (values &key default)
  "Encode a sequence of values as JSON Lines, returning a string.
   See ENCODE-LINES for full documentation."
  (with-output-to-string (stream)
    (encode-lines values stream :default default)))

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

(defun write-json-value (value stream &key pretty (indent 0) default)
  "Write a Lisp value as JSON to stream.

   Keyword arguments:
     :pretty - Enable pretty-printing with indentation
     :indent - Current indentation level (internal use)
     :default - Function to encode non-standard types. Receives the value
                and should return a JSON-serializable value."
  (let ((indent-str (when pretty (make-string (* indent 2) :initial-element #\Space))))
    (typecase value
      ((eql :null) (write-string "null" stream))
      ((eql :false) (write-string "false" stream))
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
                  (write-json-value val stream :pretty pretty :indent (1+ indent) :default default))
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
                  (write-json-value item stream :pretty pretty :indent (1+ indent) :default default))
          (when pretty
            (terpri stream)
            (write-string indent-str stream))
          (write-char #\] stream))))
      ;; Handle vectors/arrays as JSON arrays
      (vector
       (write-char #\[ stream)
       (when pretty (terpri stream))
       (loop for item across value
             for first-p = t then nil
             unless first-p do
               (write-char #\, stream)
               (when pretty (terpri stream))
             do
               (when pretty
                 (write-string (make-string (* (1+ indent) 2) :initial-element #\Space) stream))
               (write-json-value item stream :pretty pretty :indent (1+ indent) :default default))
       (when pretty
         (terpri stream)
         (write-string indent-str stream))
       (write-char #\] stream))
      ;; Handle CL hash-tables
      (hash-table
       (write-char #\{ stream)
       (when pretty (terpri stream))
       (loop for key being the hash-keys of value using (hash-value val)
             for first-p = t then nil
             unless first-p do
               (write-char #\, stream)
               (when pretty (terpri stream))
             do
               (when pretty
                 (write-string (make-string (* (1+ indent) 2) :initial-element #\Space) stream))
               (write-json-string (if (stringp key) key (princ-to-string key)) stream)
               (write-char #\: stream)
               (when pretty (write-char #\Space stream))
               (write-json-value val stream :pretty pretty :indent (1+ indent) :default default))
       (when pretty
         (terpri stream)
         (write-string indent-str stream))
       (write-char #\} stream))
      ;; Handle epsilon maps and unknown types
      (t
       (cond
         ;; Epsilon map - convert to object
         ((and (fboundp 'epsilon.map:map-p)
               (funcall (symbol-function 'epsilon.map:map-p) value))
          (let ((pairs (coerce (funcall (symbol-function 'epsilon.map:to-alist) value) 'list)))
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
                    (write-json-value (cdr pair) stream :pretty pretty :indent (1+ indent) :default default))
            (when pretty
              (terpri stream)
              (write-string indent-str stream))
            (write-char #\} stream)))
         ;; Try default encoder if provided
         (default
          (let ((encoded (funcall default value)))
            (if (eq encoded value)
                ;; Default returned same value, fall back to string
                (write-json-string (princ-to-string value) stream)
                ;; Default returned different value, encode it
                (write-json-value encoded stream :pretty pretty :indent indent :default default))))
         ;; Unknown type - convert to string
         (t
          (write-json-string (princ-to-string value) stream)))))))

(defun encode (value stream &key pretty default)
  "Encode a Lisp value as JSON to a stream.

   Keyword arguments:
     :pretty - Enable pretty-printing with indentation
     :default - Function to encode non-standard types. Receives the value
                and should return a JSON-serializable value.

   Example:
     ;; Encode with custom default handler
     (json:encode obj stream
       :default (lambda (val)
                  (typecase val
                    (timestamp (timestamp-to-string val))
                    (otherwise val))))"
  (write-json-value value stream :pretty pretty :default default)
  (when pretty (terpri stream)))

(defun encode-to-string (value &key pretty default)
  "Encode a Lisp value as JSON to a string.
   See ENCODE for keyword arguments."
  (with-output-to-string (stream)
    (encode value stream :pretty pretty :default default)))

;;; JSON Pointer (RFC 6901)
;;;
;;; JSON Pointer defines a string syntax for identifying a specific value
;;; within a JSON document. A JSON Pointer is a string of tokens separated
;;; by '/' characters, where each token identifies a level in the structure.

(define-condition json-pointer-error (error)
  ((pointer :initarg :pointer :reader pointer-error-pointer)
   (message :initarg :message :reader pointer-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON Pointer error at '~A': ~A"
                     (pointer-error-pointer condition)
                     (pointer-error-message condition)))))

(defun pointer-unescape (token)
  "Unescape a JSON Pointer token per RFC 6901.
   ~1 becomes / and ~0 becomes ~"
  (let ((result (make-array (length token) :element-type 'character
                                           :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length token))
          do (let ((ch (char token i)))
               (if (and (char= ch #\~) (< (1+ i) (length token)))
                   (let ((next (char token (1+ i))))
                     (cond
                       ((char= next #\1)
                        (vector-push-extend #\/ result)
                        (incf i 2))
                       ((char= next #\0)
                        (vector-push-extend #\~ result)
                        (incf i 2))
                       (t
                        (vector-push-extend ch result)
                        (incf i))))
                   (progn
                     (vector-push-extend ch result)
                     (incf i)))))
    (coerce result 'string)))

(defun pointer-escape (token)
  "Escape a string for use as a JSON Pointer token per RFC 6901.
   ~ becomes ~0 and / becomes ~1"
  (with-output-to-string (s)
    (loop for ch across token do
      (case ch
        (#\~ (write-string "~0" s))
        (#\/ (write-string "~1" s))
        (otherwise (write-char ch s))))))

(defun parse-pointer (pointer)
  "Parse a JSON Pointer string into a list of tokens.
   Returns nil for the root pointer \"\".
   Signals error for invalid pointers."
  (cond
    ((string= pointer "") nil)
    ((not (char= (char pointer 0) #\/))
     (error 'json-pointer-error
            :pointer pointer
            :message "JSON Pointer must be empty or start with '/'"))
    (t
     (let ((tokens (split-string (subseq pointer 1) "/")))
       (mapcar #'pointer-unescape tokens)))))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER (a single-character string)."
  (let ((delim-char (char delimiter 0))
        (result nil)
        (start 0))
    (loop for i from 0 to (length string)
          do (when (or (= i (length string))
                       (char= (char string i) delim-char))
               (push (subseq string start i) result)
               (setf start (1+ i))))
    (nreverse result)))

(defun get-pointer (document pointer)
  "Get the value at the specified JSON Pointer location.

   Arguments:
     DOCUMENT - A parsed JSON document (epsilon.map, vector, or scalar)
     POINTER  - JSON Pointer string (e.g., \"/users/0/name\")

   Returns:
     The value at the specified location, or signals JSON-POINTER-ERROR
     if the path does not exist.

   Example:
     (json:get-pointer doc \"/users/0/name\")  ; => \"Alice\"
     (json:get-pointer doc \"\")               ; => entire document
     (json:get-pointer doc \"/\")              ; => value at key \"\""
  (let ((tokens (parse-pointer pointer))
        (current document))
    (dolist (token tokens current)
      (cond
        ;; Navigate into epsilon.map
        ((map:map-p current)
         (if (map:contains-p current token)
             (setf current (map:get current token))
             (error 'json-pointer-error
                    :pointer pointer
                    :message #~"Key '~{token}' not found in object")))
        ;; Navigate into array (vector)
        ((vectorp current)
         (let ((index (handler-case (parse-integer token)
                        (error () nil))))
           (cond
             ((null index)
              (error 'json-pointer-error
                     :pointer pointer
                     :message #~"Invalid array index '~{token}'"))
             ((or (< index 0) (>= index (length current)))
              (error 'json-pointer-error
                     :pointer pointer
                     :message #~"Array index ~{index} out of bounds (length ~{(length current)})"))
             (t
              (setf current (aref current index))))))
        ;; Cannot navigate further
        (t
         (error 'json-pointer-error
                :pointer pointer
                :message #~"Cannot navigate into ~{(type-of current)} with token '~{token}'"))))))

(defun set-pointer (document pointer value)
  "Return a new document with VALUE set at the specified JSON Pointer location.
   This creates a new document (immutable update) and does not modify the original.

   Arguments:
     DOCUMENT - A parsed JSON document
     POINTER  - JSON Pointer string
     VALUE    - The new value to set

   Returns:
     A new document with the value set at the specified location.

   Example:
     (json:set-pointer doc \"/users/0/name\" \"Bob\")
     (json:set-pointer doc \"/settings/theme\" \"light\")"
  (let ((tokens (parse-pointer pointer)))
    (if (null tokens)
        value  ; Root pointer - replace entire document
        (set-pointer-internal document tokens value pointer))))

(defun set-pointer-internal (current tokens value original-pointer)
  "Internal helper for set-pointer that builds the new structure."
  (let ((token (first tokens))
        (rest-tokens (rest tokens)))
    (cond
      ;; Update epsilon.map
      ((map:map-p current)
       (if (null rest-tokens)
           ;; Final token - set the value
           (map:assoc current token value)
           ;; Navigate deeper
           (if (map:contains-p current token)
               (map:assoc current token
                          (set-pointer-internal (map:get current token)
                                                rest-tokens value original-pointer))
               (error 'json-pointer-error
                      :pointer original-pointer
                      :message #~"Key '~{token}' not found in object"))))
      ;; Update array (vector)
      ((vectorp current)
       (let ((index (handler-case (parse-integer token)
                      (error () nil))))
         (cond
           ((null index)
            (error 'json-pointer-error
                   :pointer original-pointer
                   :message #~"Invalid array index '~{token}'"))
           ((or (< index 0) (>= index (length current)))
            (error 'json-pointer-error
                   :pointer original-pointer
                   :message #~"Array index ~{index} out of bounds"))
           (t
            (let ((new-array (copy-seq current)))
              (setf (aref new-array index)
                    (if (null rest-tokens)
                        value
                        (set-pointer-internal (aref current index)
                                              rest-tokens value original-pointer)))
              new-array)))))
      ;; Cannot navigate
      (t
       (error 'json-pointer-error
              :pointer original-pointer
              :message #~"Cannot navigate into ~{(type-of current)}")))))

(defun pointer-exists-p (document pointer)
  "Check if a JSON Pointer location exists in the document.

   Arguments:
     DOCUMENT - A parsed JSON document
     POINTER  - JSON Pointer string

   Returns:
     T if the location exists, NIL otherwise.

   Example:
     (json:pointer-exists-p doc \"/users/0/name\")  ; => T or NIL"
  (handler-case
      (progn (get-pointer document pointer) t)
    (json-pointer-error () nil)))

;;; JSON Patch (RFC 6902)
;;;
;;; JSON Patch defines a JSON document structure for expressing a sequence
;;; of operations to apply to a JSON document.

(define-condition json-patch-error (error)
  ((operation :initarg :operation :reader patch-error-operation)
   (message :initarg :message :reader patch-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON Patch error in ~A: ~A"
                     (patch-error-operation condition)
                     (patch-error-message condition)))))

(defun apply-patch-operation (document operation)
  "Apply a single JSON Patch operation to a document.
   Returns the new document."
  (let ((op (map:get operation "op"))
        (path (map:get operation "path")))
    (unless op
      (error 'json-patch-error
             :operation operation
             :message "Missing 'op' field"))
    (unless path
      (error 'json-patch-error
             :operation operation
             :message "Missing 'path' field"))
    (cond
      ((string= op "add")
       (patch-add document path (map:get operation "value")))
      ((string= op "remove")
       (patch-remove document path))
      ((string= op "replace")
       (patch-replace document path (map:get operation "value")))
      ((string= op "move")
       (let ((from (map:get operation "from")))
         (unless from
           (error 'json-patch-error
                  :operation operation
                  :message "Missing 'from' field for move operation"))
         (patch-move document from path)))
      ((string= op "copy")
       (let ((from (map:get operation "from")))
         (unless from
           (error 'json-patch-error
                  :operation operation
                  :message "Missing 'from' field for copy operation"))
         (patch-copy document from path)))
      ((string= op "test")
       (patch-test document path (map:get operation "value")))
      (t
       (error 'json-patch-error
              :operation operation
              :message #~"Unknown operation: ~{op}")))))

(defun patch-add (document path value)
  "Add a value at the specified path."
  (let ((tokens (parse-pointer path)))
    (if (null tokens)
        value  ; Root - replace document
        (patch-add-internal document tokens value path))))

(defun patch-add-internal (current tokens value original-path)
  "Internal helper for patch-add."
  (let ((token (first tokens))
        (rest-tokens (rest tokens)))
    (cond
      ((map:map-p current)
       (if (null rest-tokens)
           (map:assoc current token value)
           (if (map:contains-p current token)
               (map:assoc current token
                          (patch-add-internal (map:get current token)
                                              rest-tokens value original-path))
               ;; Create intermediate object for non-existent keys
               (map:assoc current token
                          (patch-add-internal (map:make-map)
                                              rest-tokens value original-path)))))
      ((vectorp current)
       (let* ((len (length current))
              (index (if (string= token "-")
                         len  ; Append to end
                         (handler-case (parse-integer token)
                           (error () nil)))))
         (cond
           ((null index)
            (error 'json-patch-error
                   :operation (map:from-pairs `(("op" . "add") ("path" . ,original-path)))
                   :message #~"Invalid array index '~{token}'"))
           ((or (< index 0) (> index len))
            (error 'json-patch-error
                   :operation (map:from-pairs `(("op" . "add") ("path" . ,original-path)))
                   :message #~"Array index ~{index} out of bounds"))
           ((null rest-tokens)
            ;; Insert at index
            (let ((new-array (make-array (1+ len))))
              (loop for i from 0 below index
                    do (setf (aref new-array i) (aref current i)))
              (setf (aref new-array index) value)
              (loop for i from index below len
                    do (setf (aref new-array (1+ i)) (aref current i)))
              new-array))
           (t
            ;; Navigate deeper
            (let ((new-array (copy-seq current)))
              (setf (aref new-array index)
                    (patch-add-internal (aref current index)
                                        rest-tokens value original-path))
              new-array)))))
      (t
       (error 'json-patch-error
              :operation (map:from-pairs `(("op" . "add") ("path" . ,original-path)))
              :message #~"Cannot add to ~{(type-of current)}")))))

(defun patch-remove (document path)
  "Remove the value at the specified path."
  (let ((tokens (parse-pointer path)))
    (when (null tokens)
      (error 'json-patch-error
             :operation (map:from-pairs `(("op" . "remove") ("path" . ,path)))
             :message "Cannot remove root document"))
    (patch-remove-internal document tokens path)))

(defun patch-remove-internal (current tokens original-path)
  "Internal helper for patch-remove."
  (let ((token (first tokens))
        (rest-tokens (rest tokens)))
    (cond
      ((map:map-p current)
       (unless (map:contains-p current token)
         (error 'json-patch-error
                :operation (map:from-pairs `(("op" . "remove") ("path" . ,original-path)))
                :message #~"Key '~{token}' not found"))
       (if (null rest-tokens)
           (map:dissoc current token)
           (map:assoc current token
                      (patch-remove-internal (map:get current token)
                                             rest-tokens original-path))))
      ((vectorp current)
       (let ((index (handler-case (parse-integer token)
                      (error () nil))))
         (cond
           ((null index)
            (error 'json-patch-error
                   :operation (map:from-pairs `(("op" . "remove") ("path" . ,original-path)))
                   :message #~"Invalid array index '~{token}'"))
           ((or (< index 0) (>= index (length current)))
            (error 'json-patch-error
                   :operation (map:from-pairs `(("op" . "remove") ("path" . ,original-path)))
                   :message #~"Array index ~{index} out of bounds"))
           ((null rest-tokens)
            ;; Remove at index
            (let* ((len (length current))
                   (new-array (make-array (1- len))))
              (loop for i from 0 below index
                    do (setf (aref new-array i) (aref current i)))
              (loop for i from (1+ index) below len
                    do (setf (aref new-array (1- i)) (aref current i)))
              new-array))
           (t
            ;; Navigate deeper
            (let ((new-array (copy-seq current)))
              (setf (aref new-array index)
                    (patch-remove-internal (aref current index)
                                           rest-tokens original-path))
              new-array)))))
      (t
       (error 'json-patch-error
              :operation (map:from-pairs `(("op" . "remove") ("path" . ,original-path)))
              :message #~"Cannot remove from ~{(type-of current)}")))))

(defun patch-replace (document path value)
  "Replace the value at the specified path."
  (unless (pointer-exists-p document path)
    (error 'json-patch-error
           :operation (map:from-pairs `(("op" . "replace") ("path" . ,path)))
           :message "Path does not exist"))
  (set-pointer document path value))

(defun patch-move (document from to)
  "Move a value from one path to another."
  (let ((value (get-pointer document from)))
    (patch-add (patch-remove document from) to value)))

(defun patch-copy (document from to)
  "Copy a value from one path to another."
  (let ((value (get-pointer document from)))
    (patch-add document to value)))

(defun patch-test (document path value)
  "Test that a value at the path equals the expected value.
   Returns the document unchanged if test passes, signals error otherwise."
  (let ((current-value (get-pointer document path)))
    (unless (json-equal-p current-value value)
      (error 'json-patch-error
             :operation (map:from-pairs `(("op" . "test") ("path" . ,path) ("value" . ,value)))
             :message #~"Test failed: expected ~{value}, got ~{current-value}")))
  document)

(defun json-equal-p (a b)
  "Test if two JSON values are equal."
  (cond
    ((and (map:map-p a) (map:map-p b))
     (and (= (map:size a) (map:size b))
          (seq:every-p (lambda (pair)
                       (let ((key (car pair))
                             (val (cdr pair)))
                         (and (map:contains-p b key)
                              (json-equal-p val (map:get b key)))))
                     (map:to-alist a))))
    ((and (vectorp a) (vectorp b))
     (and (= (length a) (length b))
          (every #'json-equal-p a b)))
    (t (equal a b))))

(defun apply-patch (document patch)
  "Apply a JSON Patch (RFC 6902) to a document.
   PATCH should be a vector of operations (epsilon.maps).

   Each operation is an object with at least 'op' and 'path' fields.
   Supported operations: add, remove, replace, move, copy, test

   Arguments:
     DOCUMENT - The JSON document to patch
     PATCH    - Vector of patch operations

   Returns:
     A new document with all patches applied.

   Example:
     (json:apply-patch doc
       #((json:parse \"{\\\"op\\\": \\\"add\\\", \\\"path\\\": \\\"/name\\\", \\\"value\\\": \\\"Alice\\\"}\")
         (json:parse \"{\\\"op\\\": \\\"remove\\\", \\\"path\\\": \\\"/old-field\\\"}\")))"
  (reduce #'apply-patch-operation patch :initial-value document))
