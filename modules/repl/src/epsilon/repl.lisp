;;;; epsilon.repl -- minimal stdio read/eval/print loop
;;;;
;;;; This is the simple REPL that ships with the core distribution.
;;;; It reads forms from *standard-input*, evaluates them in the
;;;; current image, prints values, traps errors instead of dropping
;;;; into the SBCL debugger, and exits cleanly on EOF or Ctrl-C.
;;;;
;;;; Deliberately not in scope:
;;;;   - parent/child supervision (use `epsilon server`)
;;;;   - LSP / MCP / wire-protocol surfaces (`epsilon server`)
;;;;   - line editing / history (use rlwrap or a terminal that has them)
;;;;   - debugger walker / restart selection (errors print and continue)
;;;;
;;;; The history variables *, **, ***, /, //, ///, +, ++, +++ follow
;;;; the standard CL conventions so paste-from-elsewhere works.

(defpackage epsilon.repl
  (:use :cl)
  (:export
   #:repl
   #:run))

(in-package :epsilon.repl)

;;; --------------------------------------------------------------------------
;;; History variables
;;; --------------------------------------------------------------------------
;;;
;;; Per CLHS 25.1.5: a conforming implementation maintains *, +, /, etc. for
;;; us inside its own toplevel.  This loop runs *outside* that toplevel
;;; (we own the read/eval/print cycle) so we have to maintain them ourselves.

(defun %shift-history-after-read (form)
  "Advance the +/++/+++ ring with FORM (the most recent input)."
  (setf +++ ++
        ++  +
        +   form))

(defparameter %history-vars
  '(* ** *** + ++ +++ / // ///)
  "Forms whose evaluation should NOT advance the history rings.
   Mirrors SBCL's REPL convention: `*` is a lookup, not a new
   eval, so typing `*` returns the current value of `*` without
   shifting it -- a subsequent `**` then still sees the
   second-most-recent USER-VISIBLE value rather than what `*`
   just returned.")

(defun %shift-history-after-eval (form values)
  "Advance the */**/*** and ///// ring with the eval result --
   UNLESS FORM is one of the history-variable lookups, in which
   case the shift is skipped (the user is asking what * is, not
   producing a new most-recent value).
   VALUES is a list of all return values."
  (unless (and (symbolp form) (member form %history-vars))
    (setf ***  **
          **   *
          *    (first values)
          ///  //
          //   /
          /    values)))

;;; --------------------------------------------------------------------------
;;; Output helpers
;;; --------------------------------------------------------------------------

(defun %shortest-package-name (package)
  "The shortest of the package's primary name and any nicknames.
   Mirrors SBCL's prompt convention -- it prints CL-USER, not
   COMMON-LISP-USER, because CL-USER is shorter."
  (let ((candidates (cons (package-name package)
                          (package-nicknames package))))
    (reduce (lambda (a b) (if (< (length a) (length b)) a b))
            candidates)))

(defun %prompt-string ()
  "Two-segment prompt: shortest package name (primary or nickname),
   then '> '.  Mirrors the SBCL REPL."
  (format nil "~A> " (or (and *package* (%shortest-package-name *package*))
                          "?")))

(defun %print-banner (out)
  (format out "~&Epsilon REPL~%")
  (format out "Lisp implementation: ~A ~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version))
  (format out "Type :exit, :quit, or send EOF (Ctrl-D) to leave.~%~%"))

(defun %print-values (values out)
  "Print one line per value in VALUES; nothing for an empty result list."
  (dolist (v values)
    (let ((*print-readably* nil)
          (*print-pretty* t))
      (format out "~&~S~%" v))))

;;; --------------------------------------------------------------------------
;;; Read step
;;; --------------------------------------------------------------------------
;;;
;;; Returns one of:
;;;   :eof            -- stream closed
;;;   :directive  s   -- a single-keyword directive like :exit
;;;   form            -- a Lisp form (cons, symbol, number, etc.)
;;;   :read-error     -- malformed input; we already printed a message

(defun %directive-p (form)
  "T iff FORM is a top-level keyword that names one of our REPL
   directives.  We restrict this to the line forms `:exit` and `:quit`
   so a one-form keyword expression like `:foo` (a perfectly valid
   value) is still evaluated normally."
  (and (keywordp form)
       (member (symbol-name form) '("EXIT" "QUIT") :test #'string=)))

;;; Line-based reading: collect a line at a time into a pending
;;; buffer, then attempt to `read-from-string` one form. If the
;;; form is incomplete (`end-of-file` mid-form), pull another
;;; line and retry. If the stream ends with an incomplete form,
;;; that's a reader error -- not a clean stream-end. This matches
;;; SBCL's interactive REPL behavior: each Enter triggers a parse
;;; attempt, multi-line forms get a continuation, and `(unbalanced`
;;; followed by `(+ 1 2)` on the next line yields a reader error
;;; for the first plus a successful eval of the second.

(defvar *%pending-input* ""
  "Per-REPL-invocation pending input buffer; bound by `repl` to
   an empty string at startup. Holds characters read past the
   current form (e.g., when one line contains multiple forms)
   plus any incomplete form awaiting more input.")

(defun %try-parse-buffer ()
  "Try to read one form from `*%pending-input*`. Returns:
     :empty       -- buffer is whitespace-only; need more input.
     :need-more   -- form is incomplete; need another line.
     :read-error  -- malformed input; buffer cleared.
     form         -- a complete form; buffer advanced past it."
  (let ((buf (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                *%pending-input*)))
    (setf *%pending-input* buf)
    (cond
      ((zerop (length buf)) :empty)
      (t
       (handler-case
           (multiple-value-bind (form pos) (read-from-string buf)
             (setf *%pending-input* (subseq buf pos))
             form)
         (end-of-file () :need-more)
         (error (e)
           (format *standard-output* "~&; reader error: ~A~%" e)
           (setf *%pending-input* "")
           :read-error))))))

(defun %read-one (in)
  "Read the next form from IN. Pulls lines into `*%pending-input*`
   until a complete form is parsed; classifies EOF / mid-form-EOF /
   reader errors / interrupts.

   Multi-line forms work IFF the input stream is interactive: a
   terminal session at the prompt accumulates lines until the form
   is complete (matching SBCL's continuation-prompt behavior).
   Non-interactive streams (scripts piped in, string-streams in
   tests) are fail-fast: an incomplete form at the end of a
   single line is a reader error and the REPL drops to the next
   line. This split matches what most user-facing REPLs do
   (Python, Ruby, JS) for non-interactive input."
  (let ((interactive-p (interactive-stream-p in)))
    (handler-case
        (loop
          (let ((parsed (%try-parse-buffer)))
            (cond
              ((eq parsed :read-error) (return :read-error))
              ((eq parsed :empty)
               (let ((line (read-line in nil :%eof%)))
                 (cond
                   ((eq line :%eof%) (return :eof))
                   (t
                    (setf *%pending-input*
                          (concatenate 'string *%pending-input*
                                        line (string #\Newline)))))))
              ((eq parsed :need-more)
               (cond
                 ;; Non-interactive: fail-fast. Incomplete form at
                 ;; this line boundary is a reader error.
                 ((not interactive-p)
                  (format *standard-output*
                          "~&; reader error: incomplete form: ~A~%"
                          (string-trim '(#\Space #\Tab #\Newline)
                                        *%pending-input*))
                  (setf *%pending-input* "")
                  (return :read-error))
                 (t
                  ;; Interactive: pull another line.
                  (let ((line (read-line in nil :%eof%)))
                    (cond
                      ((eq line :%eof%)
                       ;; Mid-form EOF on an interactive stream.
                       (format *standard-output*
                               "~&; reader error: incomplete form at EOF: ~A~%"
                               (string-trim '(#\Space #\Tab #\Newline)
                                             *%pending-input*))
                       (setf *%pending-input* "")
                       (return :read-error))
                      (t
                       (setf *%pending-input*
                             (concatenate 'string *%pending-input*
                                           line (string #\Newline)))))))))
              (t
               (return (cond
                         ((%directive-p parsed) (list :directive parsed))
                         (t parsed)))))))
      (sb-sys:interactive-interrupt ()
        ;; Mid-read interrupt: discard whatever was being typed and
        ;; come back with a fresh prompt rather than crashing the REPL.
        (setf *%pending-input* "")
        (clear-input in)
        (terpri *standard-output*)
        :read-error))))

;;; --------------------------------------------------------------------------
;;; Eval step
;;; --------------------------------------------------------------------------

(defun %eval-one (form)
  "Evaluate FORM, returning (values LIST-OF-VALUES OK-P).
   On error: prints a one-line summary, returns (values nil nil)."
  (handler-case
      (let ((vs (multiple-value-list (eval form))))
        (values vs t))
    (sb-sys:interactive-interrupt ()
      (format *standard-output* "~&; interrupted~%")
      (values nil nil))
    (error (e)
      (format *standard-output* "~&; error: ~A: ~A~%" (type-of e) e)
      (values nil nil))))

;;; --------------------------------------------------------------------------
;;; Driver
;;; --------------------------------------------------------------------------

(defun repl (&key (input *standard-input*)
                  (output *standard-output*)
                  (banner t)
                  (initial-package "CL-USER"))
  "Run a minimal stdio REPL.  Reads from INPUT, prints to OUTPUT,
   evaluates each form in the current image.  Returns NIL when the
   user exits via :exit, :quit, EOF, or interrupt.

   INITIAL-PACKAGE is the package the REPL starts in.  After the
   first `(in-package :foo)` (or any other change to *PACKAGE*) the
   REPL follows the user wherever they went."
  (let ((*standard-input* input)
        (*standard-output* output)
        (*package* (or (find-package initial-package) (find-package "CL-USER")))
        (*%pending-input* ""))
    (when banner (%print-banner output))
    (loop
      (write-string (%prompt-string) output)
      (force-output output)
      (let ((form (%read-one input)))
        (cond
          ((eq form :eof) (terpri output) (return))
          ((eq form :read-error) (terpri output))
          ((and (consp form) (eq (first form) :directive))
           (let ((d (second form)))
             (when (member (symbol-name d) '("EXIT" "QUIT") :test #'string=)
               (return))))
          (t
           (%shift-history-after-read form)
           (multiple-value-bind (values ok-p) (%eval-one form)
             (when ok-p
               (%shift-history-after-eval form values)
               (%print-values values output)))))))))

(defun run (&key (banner t) (initial-package "CL-USER"))
  "Top-level entry point: run REPL on stdin/stdout, returning NIL on exit.
   Wrapped so that an unhandled SB-SYS:INTERACTIVE-INTERRUPT outside
   any read/eval (e.g. between forms) also exits cleanly instead of
   surfacing a backtrace."
  (handler-case
      (repl :banner banner :initial-package initial-package)
    (sb-sys:interactive-interrupt ()
      (terpri))))
