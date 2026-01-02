;;;; interpolation.lisp - String interpolation reader macro
;;;;
;;;; Provides Python-style string interpolation with #~"..." syntax.
;;;; Expressions are embedded using ~{expr} within the string.
;;;;
;;;; Examples:
;;;;   #~"Hello, ~{name}!"           ; Variable interpolation
;;;;   #~"Sum: ~{(+ a b)}"           ; Expression interpolation
;;;;   #~"100~~ complete"            ; Escaped tilde
;;;;   #~"Line1\nLine2"              ; Standard escape sequences

(defpackage :epsilon.interpolation
  (:use :cl)
  (:local-nicknames (:str :epsilon.string))
  (:export
   #:install-interpolation-reader
   #:uninstall-interpolation-reader
   #:with-interpolation-reader
   #:parse-interpolated-string))

(in-package :epsilon.interpolation)

;;; Parser for interpolation expressions

(defun read-until-close-brace (stream)
  "Read characters until matching close brace, handling nested braces.
   Returns the content as a string (excluding the final closing brace)."
  (let ((chars '())
        (depth 1))
    (loop while (> depth 0) do
      (let ((char (read-char stream t nil t)))
        (cond
          ((char= char #\{)
           (incf depth)
           (push char chars))
          ((char= char #\})
           (decf depth)
           (when (> depth 0)
             (push char chars)))
          ((char= char #\")
           ;; Handle string literals within expressions
           (push char chars)
           (loop for c = (read-char stream t nil t)
                 do (push c chars)
                 until (char= c #\")))
          (t
           (push char chars)))))
    (coerce (nreverse chars) 'string)))

(defun parse-interpolation-expr (stream)
  "Parse ~{...} interpolation content, returning the Lisp expression.
   The opening brace has already been consumed."
  (let ((content (read-until-close-brace stream)))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Newline) content)))
      (error "Empty interpolation expression ~~{} is not allowed"))
    ;; Parse the expression from the content string
    (handler-case
        (read-from-string content)
      (error (e)
        (error "Invalid expression in interpolation ~~{~A}: ~A" content e)))))

(defun parse-interpolated-string (stream char arg)
  "Parse #~\"...\" interpolated string syntax.

   ~{expr} embeds an expression that will be converted to string.
   ~~ is an escaped tilde (produces a single ~).
   Standard escape sequences (\\n, \\t, \\r, \\\", \\\\) are supported.

   Examples:
     #~\"Hello, ~{name}!\"       => (str:concat \"Hello, \" (princ-to-string name) \"!\")
     #~\"Sum is ~{(+ a b)}\"     => (str:concat \"Sum is \" (princ-to-string (+ a b)))
     #~\"100~~ complete\"        => \"100~ complete\"
     #~\"plain string\"          => \"plain string\""
  (declare (ignore char arg))
  ;; Read the opening quote
  (let ((quote-char (read-char stream t nil t)))
    (unless (char= quote-char #\")
      (error "Interpolated string must start with \", got: ~A" quote-char)))

  (let ((parts '())
        (current-literal '()))
    (labels ((flush-literal ()
               "Flush accumulated literal characters as a string part."
               (when current-literal
                 (push (coerce (nreverse current-literal) 'string) parts)
                 (setf current-literal '())))
             (add-char (c)
               "Add a character to the current literal accumulator."
               (push c current-literal)))
      (loop
        (let ((char (read-char stream t nil t)))
          (cond
            ;; End of string
            ((char= char #\")
             (flush-literal)
             (return))

            ;; Tilde - check for interpolation or escape
            ((char= char #\~)
             (let ((next (peek-char nil stream t nil t)))
               (cond
                 ;; Escaped tilde ~~
                 ((char= next #\~)
                  (read-char stream t nil t)
                  (add-char #\~))
                 ;; Interpolation expression ~{...}
                 ((char= next #\{)
                  (read-char stream t nil t)  ; consume {
                  (flush-literal)
                  (push (parse-interpolation-expr stream) parts))
                 ;; Literal tilde (not followed by ~ or {)
                 (t
                  (add-char #\~)))))

            ;; Backslash escape sequences
            ((char= char #\\)
             (let ((escaped (read-char stream t nil t)))
               (add-char (case escaped
                           (#\n #\Newline)
                           (#\t #\Tab)
                           (#\r #\Return)
                           (#\" #\")
                           (#\\ #\\)
                           (#\~ #\~)
                           (t escaped)))))

            ;; Regular character
            (t (add-char char))))))

    ;; Generate the result form
    (let ((reversed-parts (nreverse parts)))
      (cond
        ;; Empty string
        ((null reversed-parts)
         "")
        ;; Single literal string (no interpolation)
        ((and (= (length reversed-parts) 1)
              (stringp (first reversed-parts)))
         (first reversed-parts))
        ;; Multiple parts - generate str:concat call
        (t
         `(str:concat
           ,@(mapcar (lambda (part)
                       (if (stringp part)
                           part
                           `(princ-to-string ,part)))
                     reversed-parts)))))))

;;; Readtable management

(defvar *interp-original-readtable* nil
  "Saved readtable for uninstall.")

(defvar *interp-readtable* nil
  "The readtable with interpolation reader installed.")

(defun install-interpolation-reader ()
  "Install the #~\"...\" interpolation reader macro.

   After installation, you can use:
     #~\"Hello, ~{name}!\"      ; Variable interpolation
     #~\"Sum: ~{(+ a b)}\"      ; Expression interpolation
     #~\"100~~ complete\"       ; Escaped tilde

   Returns T on success."
  (unless *interp-original-readtable*
    (setf *interp-original-readtable* (copy-readtable *readtable*)))
  (let ((rt (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\~ #'parse-interpolated-string rt)
    (setf *interp-readtable* rt)
    (setf *readtable* rt))
  t)

(defun uninstall-interpolation-reader ()
  "Remove the interpolation reader macro, restoring original readtable.
   Returns T on success, NIL if no reader was installed."
  (when *interp-original-readtable*
    (setf *readtable* (copy-readtable *interp-original-readtable*))
    (setf *interp-original-readtable* nil)
    (setf *interp-readtable* nil)
    t))

(defmacro with-interpolation-reader (&body body)
  "Execute BODY with interpolation reader extensions enabled.
   Restores original readtable after BODY completes.

   Example:
     (with-interpolation-reader
       (let ((name \"World\"))
         (eval (read-from-string \"#~\\\"Hello, ~{name}!\\\"\"))))"
  (let ((saved (gensym "SAVED-READTABLE")))
    `(let ((,saved *readtable*))
       (unwind-protect
            (progn
              (install-interpolation-reader)
              ,@body)
         (setf *readtable* ,saved)
         (setf *interp-original-readtable* nil)))))
