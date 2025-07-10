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

(defpackage #:epsilon.lib.lexer.impl
  (:use #:cl)
  (:export
   ;; Core lexer structure
   #:lexer
   #:make-lexer
   
   ;; Character access
   #:peek
   #:next
   #:at-end-p
   
   ;; Position management
   #:lexer-position
   #:save-position
   #:restore-position
   
   ;; Token structure
   #:token
   #:make-token
   #:%make-token
   #:token-type
   #:token-value
   #:token-position
   
   ;; Error handling
   #:lexer-error
   #:lexer-error-message
   #:lexer-error-position
   
   ;; Utility functions
   #:skip-while
   #:skip-whitespace
   #:consume-while
   #:consume-string))

(in-package #:epsilon.lib.lexer.impl)

;;;;  Core Data Structures

(defstruct (lexer (:constructor %make-lexer)
                  (:conc-name %lexer-))
  "Core lexer structure that abstracts character stream access"
  (source nil :type stream)
  (position 0 :type fixnum)
  (saved-positions nil :type list)
  (line 1 :type fixnum)
  (column 1 :type fixnum))

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
  (peek-char nil (%lexer-source lexer) nil nil))

(defun next (lexer)
  "Consume and return the next character. Returns nil at end of input."
  (let ((ch (peek lexer)))
    (when ch
      (incf (%lexer-position lexer))
      ;; Update line and column tracking
      (if (char= ch #\Newline)
          (progn
            (incf (%lexer-line lexer))
            (setf (%lexer-column lexer) 1))
          (incf (%lexer-column lexer)))
      ;; Actually consume the character for streams
      (read-char (%lexer-source lexer)))
    ch))

(defun at-end-p (lexer)
  "Test whether we're at the end of the input"
  (null (peek lexer)))

;;;;  Position Management

;;;; FIXME after save position, we need to record peeked and read
;;;; characters, because repositioning the stream is too
;;;; expensive. Or better yet, use a buffered stream class.

(defun lexer-position (lexer)
  "Get current position information as values: position, line, column"
  (values (%lexer-position lexer)
          (%lexer-line lexer)
          (%lexer-column lexer)))

(defun save-position (lexer)
  "Save the current position for later restoration"
  (push (list (%lexer-position lexer)
              (%lexer-line lexer)
              (%lexer-column lexer))
        (%lexer-saved-positions lexer)))

(defun restore-position (lexer)
  "Restore the most recently saved position"
  (unless (%lexer-saved-positions lexer)
    (error "No saved positions to restore"))
  (destructuring-bind (pos line col) (pop (%lexer-saved-positions lexer))
    (setf (%lexer-position lexer) pos
          (%lexer-line lexer) line
          (%lexer-column lexer) col)))

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
