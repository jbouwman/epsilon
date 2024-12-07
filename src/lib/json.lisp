(defpackage #:epsilon.lib.json
  (:use #:cl))

(in-package #:epsilon.lib.json)

(define-condition json-parse-error (error)
  ((message :initarg :message :reader json-error-message)
   (position :initarg :position :reader json-error-position))
  (:report (lambda (c s)
            (format s "JSON parse error at position ~A: ~A"
                    (json-error-position c)
                    (json-error-message c)))))

(defstruct json-parser
  (source nil)
  (position 0)
  (length 0))

(defun make-json-error (parser message)
  (error 'json-parse-error 
         :message message 
         :position (json-parser-position parser)))

(defun peek-char* (parser)
  (when (< (json-parser-position parser)
           (json-parser-length parser))
    (char (json-parser-source parser)
          (json-parser-position parser))))

(defun next-char (parser)
  (let ((ch (peek-char* parser)))
    (when ch
      (incf (json-parser-position parser)))
    ch))

(defun skip-whitespace (parser)
  (loop while (member (peek-char* parser) '(#\Space #\Tab #\Newline #\Return))
        do (next-char parser)))

(defun parse-string (parser)
  (unless (char= (next-char parser) #\")
    (make-json-error parser "Expected string"))
  
  (with-output-to-string (s)
    (loop for ch = (next-char parser)
          do (case ch
               ((nil) (make-json-error parser "Unterminated string"))
               (#\" (return))
               (#\\ (let ((next (next-char parser)))
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
                                           (write-char (or (next-char parser)
                                                         (make-json-error parser "Invalid unicode escape"))
                                                     hex)))
                                       :radix 16)))
                              (write-char (code-char code) s)))
                       (otherwise (make-json-error parser "Invalid escape sequence")))))
               (otherwise (write-char ch s))))))

(defun parse-number (parser)
  (let* ((has-decimal nil)
         (has-exponent nil)
         (number-str (with-output-to-string (s)
                      ;; Sign
                      (let ((ch (peek-char* parser)))
                        (when (and ch (char= ch #\-))
                          (write-char (next-char parser) s)))
                      
                      ;; Integer part  
                      (loop for ch = (peek-char* parser)
                            while (and ch (digit-char-p ch))
                            do (write-char (next-char parser) s))
                      
                      ;; Decimal part
                      (let ((ch (peek-char* parser)))
                        (when (and ch (char= ch #\.))
                          (write-char (next-char parser) s)
                          (setf has-decimal t)
                          (loop for ch = (peek-char* parser)
                                while (and ch (digit-char-p ch)) 
                                do (write-char (next-char parser) s))))
                      
                      ;; Exponent
                      (let ((ch (peek-char* parser)))
                        (when (and ch (member ch '(#\e #\E)))
                          (write-char (next-char parser) s)
                          (setf has-exponent t)
                          (let ((ch (peek-char* parser)))
                            (when (and ch (member ch '(#\+ #\-)))
                              (write-char (next-char parser) s)))
                          (loop for ch = (peek-char* parser)
                                while (and ch (digit-char-p ch))
                                do (write-char (next-char parser) s)))))))
    
    (if (or has-decimal has-exponent)
        (read-from-string number-str)
        (parse-integer number-str))))

(defun parse-value (parser)
  (skip-whitespace parser)
  (let ((ch (peek-char* parser)))
    (case ch
      (#\" (parse-string parser))
      (#\{ (parse-object parser))
      (#\[ (parse-array parser))
      (#\t (parse-true parser))
      (#\f (parse-false parser))
      (#\n (parse-null parser))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (parse-number parser))
      (otherwise (make-json-error parser "Invalid JSON value")))))

(defun parse-object (parser)
  (unless (char= (next-char parser) #\{)
    (make-json-error parser "Expected '{'"))
  
  (skip-whitespace parser)
  (if (char= (peek-char* parser) #\})
      (progn (next-char parser) nil)
      (loop for first = t then nil
            collect (progn
                     (when (not first)
                       (skip-whitespace parser)
                       (unless (char= (next-char parser) #\,)
                         (make-json-error parser "Expected ',' between object members")))
                     (skip-whitespace parser)
                     (let ((key (parse-string parser)))
                       (skip-whitespace parser)
                       (unless (char= (next-char parser) #\:)
                         (make-json-error parser "Expected ':' after object key"))
                       (cons key (parse-value parser))))
            until (progn
                   (skip-whitespace parser)
                   (when (char= (peek-char* parser) #\})
                     (next-char parser)
                     t)))))

(defun parse-array (parser)
  (unless (char= (next-char parser) #\[)
    (make-json-error parser "Expected '['"))
  
  (skip-whitespace parser)
  (if (char= (peek-char* parser) #\])
      (progn (next-char parser) nil)
      (loop for first = t then nil
            collect (progn
                     (when (not first)
                       (skip-whitespace parser)
                       (unless (char= (next-char parser) #\,)
                         (make-json-error parser "Expected ',' between array elements")))
                     (parse-value parser))
            until (progn
                   (skip-whitespace parser)
                   (when (char= (peek-char* parser) #\])
                     (next-char parser)
                     t)))))

(defun parse-true (parser)
  (unless (and (char= (next-char parser) #\t)
               (char= (next-char parser) #\r)
               (char= (next-char parser) #\u)
               (char= (next-char parser) #\e))
    (make-json-error parser "Expected 'true'"))
  t)

(defun parse-false (parser)
  (unless (and (char= (next-char parser) #\f)
               (char= (next-char parser) #\a)
               (char= (next-char parser) #\l)
               (char= (next-char parser) #\s)
               (char= (next-char parser) #\e))
    (make-json-error parser "Expected 'false'"))
  nil)

(defun parse-null (parser)
  (unless (and (char= (next-char parser) #\n)
               (char= (next-char parser) #\u)
               (char= (next-char parser) #\l)
               (char= (next-char parser) #\l))
    (make-json-error parser "Expected 'null'"))
  nil)

(defun parse-json (string)
  "Parse a JSON string into Lisp data structures.
   Objects are represented as alists, arrays as lists."
  (let ((parser (make-json-parser :source string
                                 :position 0
                                 :length (length string))))
    (let ((result (parse-value parser)))
      (skip-whitespace parser)
      (when (< (json-parser-position parser)
               (json-parser-length parser))
        (make-json-error parser "Unexpected trailing content"))
      result)))
