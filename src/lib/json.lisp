;;;;  TODO
;;;;
;;;;  JSON string escape parsing (\", \\, \n, etc.) could be extracted as
;;;;  epsilon.lib.string-escape for reuse in other text formats.
;;;;
;;;;  JSON number parsing (parse-number function) could extend epsilon.lib.type
;;;;  with numeric parsing for multiple formats.

(defpackage :epsilon.lib.json
  (:use :cl)
  (:local-nicknames
   (:lexer
    :epsilon.lib.lexer))
  (:export
   :parse
   :parse-string))

(in-package :epsilon.lib.json)

(define-condition json-parse-error (error)
  ((message :initarg :message :reader json-error-message)
   (position :initarg :position :reader json-error-position))
  (:report (lambda (c s)
             (format s "JSON parse error at position ~A: ~A"
                     (json-error-position c)
                     (json-error-message c)))))

(defun skip-whitespace (lexer)
  (lexer:skip-whitespace lexer))

(defun %parse-string (lexer)
  (unless (char= (lexer:next lexer) #\")
    (lexer:lexer-error lexer "Expected string"))
  
  (with-output-to-string (s)
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
               (otherwise (write-char ch s))))))

(defun parse-number (lexer)
  (let* ((has-decimal nil)
         (has-exponent nil)
         (number-str (with-output-to-string (s)
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
    
    (if (or has-decimal has-exponent)
        (read-from-string number-str)
        (parse-integer number-str))))

(defun parse-value (lexer)
  (skip-whitespace lexer)
  (let ((ch (lexer:peek lexer)))
    (case ch
      (#\" (%parse-string lexer))
      (#\{ (parse-object lexer))
      (#\[ (parse-array lexer))
      (#\t (parse-true lexer))
      (#\f (parse-false lexer))
      (#\n (parse-null lexer))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (parse-number lexer))
      (otherwise (lexer:lexer-error lexer "Invalid JSON value")))))

(defun parse-object (lexer)
  (unless (char= (lexer:next lexer) #\{)
    (lexer:lexer-error lexer "Expected '{'"))
  
  (skip-whitespace lexer)
  (if (char= (lexer:peek lexer) #\})
      (progn (lexer:next lexer) nil)
      (loop for first = t then nil
            collect (progn
                      (when (not first)
                        (skip-whitespace lexer)
                        (unless (char= (lexer:next lexer) #\,)
                          (lexer:lexer-error lexer "Expected ',' between object members")))
                      (skip-whitespace lexer)
                      (let ((key (%parse-string lexer)))
                        (skip-whitespace lexer)
                        (unless (char= (lexer:next lexer) #\:)
                          (lexer:lexer-error lexer "Expected ':' after object key"))
                        (cons key (parse-value lexer))))
            until (progn
                    (skip-whitespace lexer)
                    (when (char= (lexer:peek lexer) #\})
                      (lexer:next lexer)
                      t)))))

(defun parse-array (lexer)
  (unless (char= (lexer:next lexer) #\[)
    (lexer:lexer-error lexer "Expected '['"))
  
  (skip-whitespace lexer)
  (if (char= (lexer:peek lexer) #\])
      (progn (lexer:next lexer) nil)
      (loop for first = t then nil
            collect (progn
                      (when (not first)
                        (skip-whitespace lexer)
                        (unless (char= (lexer:next lexer) #\,)
                          (lexer:lexer-error lexer "Expected ',' between array elements")))
                      (parse-value lexer))
            until (progn
                    (skip-whitespace lexer)
                    (when (char= (lexer:peek lexer) #\])
                      (lexer:next lexer)
                      t)))))

(defun parse-true (lexer)
  (unless (lexer:consume-string lexer "true")
    (lexer:lexer-error lexer "Expected 'true'"))
  t)

(defun parse-false (lexer)
  (unless (lexer:consume-string lexer "false")
    (lexer:lexer-error lexer "Expected 'false'"))
  nil)

(defun parse-null (lexer)
  (unless (lexer:consume-string lexer "null")
    (lexer:lexer-error lexer "Expected 'null'"))
  nil)

(defun parse (stream)
  "Parse a JSON string into Lisp data structures.
   Objects are represented as alists, arrays as lists."
  (let ((lexer (lexer:make-lexer stream)))
    (let ((result (parse-value lexer)))
      (skip-whitespace lexer)
      (when (not (lexer:at-end-p lexer))
        (lexer:lexer-error lexer "Unexpected trailing content"))
      result)))

(defun parse-string (string)
  (with-input-from-string (stream string)
    (parse stream)))
