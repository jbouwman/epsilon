;;;; A lexing framework for parsing character streams into tokens.
;;;;
;;;; - Character stream abstraction with peek/next operations
;;;; - Position tracking for error reporting
;;;; - Backtracking support for speculative parsing
;;;; - Token abstraction for structured output
;;;; - Error handling with position information
;;;;
;;;; 1. Create a lexer from a string or stream
;;;; 2. Use peek-char / next for character-level parsing
;;;; 3. Use make-token for structured output
;;;; 4. Use save-position / restore-position for backtracking
;;;; 5. Use lexer-error for position-aware error reporting

(defpackage #:epsilon.lexer
  (:use #:cl)
  (:export
   ;; Core lexer structure
   #:lexer
   #:make-lexer
   
   ;; Character access
   #:peek
   #:peek-n
   #:peek-ahead
   #:next
   #:at-end-p
   
   ;; Position management
   #:lexer-position
   
   ;; Token structure
   #:token
   #:make-token
   #:%make-token
   #:token-type
   #:token-value
   #:token-position
   #:token-line
   #:token-column
   
   ;; Error handling
   #:lexer-error
   #:lexer-error-message
   #:lexer-error-position
   #:lexer-error-line
   #:lexer-error-column
   
   ;; Utility functions
   #:skip-while
   #:skip-whitespace
   #:consume-while
   #:consume-string))

(in-package #:epsilon.lexer)

;;;;  Core Data Structures

(defstruct (lexer (:constructor %make-lexer)
                  (:conc-name %lexer-))
  "Core lexer structure that abstracts character stream access"
  (source nil :type stream)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (buffer nil :type list)  ; Buffer for peeked characters
  (buffer-size 0 :type fixnum))

(defstruct (token (:constructor %make-token))
  "Token structure for structured lexer output"
  (type nil)
  (value nil)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum))

(define-condition lexer-error (error)
  ((message :initarg :message :reader lexer-error-message)
   (position :initarg :position :reader lexer-error-position)
   (line :initarg :line :reader lexer-error-line)
   (column :initarg :column :reader lexer-error-column))
  (:report (lambda (c s)
             (format s "Lexer error at line ~A, column ~A (position ~A): ~A"
                     (lexer-error-line c)
                     (lexer-error-column c)
                     (lexer-error-position c)
                     (lexer-error-message c)))))

;;;; Constructor

(defun make-lexer (stream)
  "Create a lexer for parsing a character stream"
  (%make-lexer :source stream))

;;;; Core Character Access Functions

;;; TODO appropriate inline declarations

(defun peek (lexer)
  "Peek at the next character without consuming it. Returns nil at end of input."
  (if (%lexer-buffer lexer)
      (first (%lexer-buffer lexer))
      (peek-char nil (%lexer-source lexer) nil nil)))

(defun next (lexer)
  "Consume and return the next character. Returns nil at end of input."
  (let ((ch (if (%lexer-buffer lexer)
                (prog1 (first (%lexer-buffer lexer))
                  (setf (%lexer-buffer lexer) (rest (%lexer-buffer lexer)))
                  (decf (%lexer-buffer-size lexer)))
                (read-char (%lexer-source lexer) nil nil))))
    (when ch
      (incf (%lexer-position lexer))
      ;; Update line and column tracking
      (if (char= ch #\Newline)
          (progn
            (incf (%lexer-line lexer))
            (setf (%lexer-column lexer) 1))
          (incf (%lexer-column lexer))))
    ch))

(defun peek-n (lexer n)
  "Peek at the next N characters without consuming them. Returns a list of characters."
  (let ((buffer-size (%lexer-buffer-size lexer)))
    ;; Fill buffer if needed
    (when (< buffer-size n)
      (let ((needed (- n buffer-size)))
        (loop repeat needed
              for ch = (peek-char nil (%lexer-source lexer) nil nil)
              while ch
              do (progn
                   (setf (%lexer-buffer lexer) 
                         (append (%lexer-buffer lexer) (list ch)))
                   (incf (%lexer-buffer-size lexer))
                   (read-char (%lexer-source lexer))))))
    ;; Return first n characters from buffer (or less if at EOF)
    (loop for i from 0 below n
          for chars on (%lexer-buffer lexer)
          while chars
          collect (first chars))))

(defun peek-ahead (lexer n)
  "Peek at the character N positions ahead without consuming. Returns nil at end."
  (let ((chars (peek-n lexer (1+ n))))
    (when (= (length chars) (1+ n))
      (nth n chars))))

(defun at-end-p (lexer)
  "Test whether we're at the end of the input"
  (null (peek lexer)))

;;;;  Position Management

(defun lexer-position (lexer)
  "Get current position information as values: position, line, column"
  (values (%lexer-position lexer)
          (%lexer-line lexer)
          (%lexer-column lexer)))

;;;; Token Creation

(defun make-token (lexer type value)
  "Create a token with current position information"
  (%make-token :type type
               :value value
               :position (%lexer-position lexer)
               :line (%lexer-line lexer)
               :column (%lexer-column lexer)))

;;;; Error Handling

(defun lexer-error (lexer message)
  "Signal a lexer error with position information"
  (error 'lexer-error
         :message message
         :position (%lexer-position lexer)
         :line (%lexer-line lexer)
         :column (%lexer-column lexer)))

;;;; Utility Functions

(defun skip-while (lexer predicate)
  "Skip characters while predicate returns true"
  (loop while (and (not (at-end-p lexer))
                   (funcall predicate (peek lexer)))
        do (next lexer)))

(defun skip-whitespace (lexer)
  "Skip whitespace characters (space, tab, newline, return)"
  (skip-while lexer (lambda (ch)
                      (member ch '(#\Space #\Tab #\Newline #\Return)))))

(defun consume-while (lexer predicate)
  "Consume characters while predicate returns true, returning the consumed string"
  (with-output-to-string (s)
    (loop while (and (not (at-end-p lexer))
                     (funcall predicate (peek lexer)))
          do (write-char (next lexer) s))))

(defun consume-string (lexer expected)
  "Consume a specific string, signaling an error if not found"
  (loop for expected-char across expected
        for actual-char = (next lexer)
        unless (and actual-char (char= actual-char expected-char))
          do (lexer-error lexer (format nil "expected '~A'" expected))
        finally (return t)))
