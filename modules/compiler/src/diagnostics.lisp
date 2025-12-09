;;;; diagnostics.lisp - Structured compiler diagnostics
;;;;
;;;; Provides rich diagnostic information with source spans,
;;;; fix suggestions, and related locations. Goes beyond standard
;;;; Common Lisp condition handling.

(defpackage :epsilon.compiler.diagnostics
  (:use :cl)
  (:export
   ;; Severity
   #:severity
   #:+error+
   #:+warning+
   #:+style-warning+
   #:+note+
   #:+info+
   #:+hint+

   ;; Source spans
   #:source-span
   #:make-source-span
   #:source-span-p
   #:source-span-file
   #:source-span-start-line
   #:source-span-start-column
   #:source-span-end-line
   #:source-span-end-column
   #:source-span-contains-p
   #:source-span-overlaps-p

   ;; Diagnostics
   #:diagnostic
   #:make-diagnostic
   #:diagnostic-p
   #:diagnostic-severity
   #:diagnostic-code
   #:diagnostic-message
   #:diagnostic-location
   #:diagnostic-source-context
   #:diagnostic-related
   #:diagnostic-notes
   #:diagnostic-enclosing-form
   #:diagnostic-fixes
   #:diagnostic-category
   #:diagnostic-documentation-url

   ;; Suggested fixes
   #:suggested-fix
   #:make-suggested-fix
   #:suggested-fix-p
   #:suggested-fix-description
   #:suggested-fix-edits
   #:suggested-fix-confidence

   ;; Source edits
   #:source-edit
   #:make-source-edit
   #:source-edit-p
   #:source-edit-span
   #:source-edit-replacement

   ;; Related diagnostics
   #:related-diagnostic
   #:make-related-diagnostic
   #:related-diagnostic-p
   #:related-diagnostic-message
   #:related-diagnostic-location

   ;; Diagnostic creation helpers
   #:make-error
   #:make-warning
   #:make-style-warning
   #:make-note
   #:make-info
   #:make-hint

   ;; Diagnostic emission
   #:emit-diagnostic
   #:*diagnostic-handler*

   ;; Formatting
   #:format-diagnostic
   #:format-diagnostic-text
   #:format-diagnostic-json
   #:format-source-context

   ;; ANSI colors
   #:+reset-color+
   #:severity-color-code
   #:with-color-output))

(in-package :epsilon.compiler.diagnostics)

;;; Severity levels

(deftype severity ()
  '(member :error :warning :style-warning :note :info :hint))

(defconstant +error+ :error)
(defconstant +warning+ :warning)
(defconstant +style-warning+ :style-warning)
(defconstant +note+ :note)
(defconstant +info+ :info)
(defconstant +hint+ :hint)

;;; Source span structure

(defstruct (source-span
            (:constructor %make-source-span)
            (:copier nil))
  "A span of source code, from start to end position."
  (file nil :type (or pathname string null))
  (start-line 0 :type (integer 0))
  (start-column 0 :type (integer 0))
  (end-line 0 :type (integer 0))
  (end-column 0 :type (integer 0)))

(defun make-source-span (&key file
                              (start-line 0)
                              (start-column 0)
                              (end-line nil)
                              (end-column nil))
  "Create a source span.

   If end-line/end-column are not provided, they default to start values."
  (%make-source-span
   :file file
   :start-line start-line
   :start-column start-column
   :end-line (or end-line start-line)
   :end-column (or end-column start-column)))

(defun source-span-contains-p (span line column)
  "Check if SPAN contains the position at LINE:COLUMN."
  (and span
       (or (> line (source-span-start-line span))
           (and (= line (source-span-start-line span))
                (>= column (source-span-start-column span))))
       (or (< line (source-span-end-line span))
           (and (= line (source-span-end-line span))
                (<= column (source-span-end-column span))))))

(defun source-span-overlaps-p (span1 span2)
  "Check if two source spans overlap."
  (and span1 span2
       (equal (source-span-file span1) (source-span-file span2))
       (not (or (< (source-span-end-line span1) (source-span-start-line span2))
                (> (source-span-start-line span1) (source-span-end-line span2))))))

;;; Suggested fix structure

(defstruct (suggested-fix
            (:constructor %make-suggested-fix)
            (:copier nil))
  "A suggested code fix with source edits."
  (description "" :type string)
  (edits nil :type list)  ; List of source-edit
  (confidence :high :type (member :high :medium :low)))

(defun make-suggested-fix (&key description edits (confidence :high))
  "Create a suggested fix."
  (%make-suggested-fix
   :description (or description "")
   :edits edits
   :confidence confidence))

;;; Source edit structure

(defstruct (source-edit
            (:constructor %make-source-edit)
            (:copier nil))
  "A single source code edit - replace span with replacement text."
  (span nil :type (or source-span null))
  (replacement "" :type string))

(defun make-source-edit (&key span replacement)
  "Create a source edit."
  (%make-source-edit
   :span span
   :replacement (or replacement "")))

;;; Related diagnostic structure

(defstruct (related-diagnostic
            (:constructor %make-related-diagnostic)
            (:copier nil))
  "A related location with a message."
  (message "" :type string)
  (location nil :type (or source-span null)))

(defun make-related-diagnostic (&key message location)
  "Create a related diagnostic."
  (%make-related-diagnostic
   :message (or message "")
   :location location))

;;; Main diagnostic structure

(defstruct (diagnostic
            (:constructor %make-diagnostic)
            (:copier nil))
  "A compiler diagnostic with rich information.

   Includes source location, related diagnostics, and fix suggestions."
  ;; Core information
  (severity :error :type severity)
  (code nil :type (or string null))  ; e.g., \"E0001\", \"W0042\"
  (message "" :type string)

  ;; Location
  (location nil :type (or source-span null))
  (source-context nil :type (or string null))  ; Source lines around error

  ;; Additional context
  (related nil :type list)        ; List of related-diagnostic
  (notes nil :type list)          ; List of strings
  (enclosing-form nil :type t)    ; The form being compiled

  ;; Fix suggestions
  (fixes nil :type list)          ; List of suggested-fix

  ;; Metadata
  (category nil :type (or keyword null))  ; :type-error, :undefined-variable, etc.
  (documentation-url nil :type (or string null)))

(defun make-diagnostic (&key (severity :error)
                             code
                             message
                             location
                             source-context
                             related
                             notes
                             enclosing-form
                             fixes
                             category
                             documentation-url)
  "Create a diagnostic."
  (%make-diagnostic
   :severity severity
   :code code
   :message (or message "")
   :location location
   :source-context source-context
   :related related
   :notes notes
   :enclosing-form enclosing-form
   :fixes fixes
   :category category
   :documentation-url documentation-url))

;;; Diagnostic creation helpers

(defun make-error (message &key location code related fixes category)
  "Create an error diagnostic."
  (make-diagnostic :severity :error
                   :message message
                   :location location
                   :code code
                   :related related
                   :fixes fixes
                   :category category))

(defun make-warning (message &key location code related fixes category)
  "Create a warning diagnostic."
  (make-diagnostic :severity :warning
                   :message message
                   :location location
                   :code code
                   :related related
                   :fixes fixes
                   :category category))

(defun make-style-warning (message &key location code related fixes)
  "Create a style-warning diagnostic."
  (make-diagnostic :severity :style-warning
                   :message message
                   :location location
                   :code code
                   :related related
                   :fixes fixes
                   :category :style))

(defun make-note (message &key location)
  "Create a note diagnostic."
  (make-diagnostic :severity :note
                   :message message
                   :location location))

(defun make-info (message &key location)
  "Create an info diagnostic."
  (make-diagnostic :severity :info
                   :message message
                   :location location))

(defun make-hint (message &key location fixes)
  "Create a hint diagnostic."
  (make-diagnostic :severity :hint
                   :message message
                   :location location
                   :fixes fixes))

;;; Diagnostic emission

(defvar *diagnostic-handler* nil
  "Current diagnostic handler function.")

(defun emit-diagnostic (diagnostic)
  "Emit a diagnostic through the current handler.

   If no handler is installed, does nothing."
  (when *diagnostic-handler*
    (funcall *diagnostic-handler* diagnostic))
  diagnostic)

;;; ANSI color codes

(defvar +reset-color+ (format nil "~C[0m" #\Escape)
  "ANSI reset code.")

(defun severity-color-code (severity)
  "Get the ANSI color code for a severity level."
  (ecase severity
    (:error (format nil "~C[1;31m" #\Escape))        ; Bold red
    (:warning (format nil "~C[1;33m" #\Escape))      ; Bold yellow
    (:style-warning (format nil "~C[33m" #\Escape))  ; Yellow
    (:note (format nil "~C[1;36m" #\Escape))         ; Bold cyan
    (:info (format nil "~C[36m" #\Escape))           ; Cyan
    (:hint (format nil "~C[32m" #\Escape))))         ; Green

(defmacro with-color-output ((stream severity) &body body)
  "Execute BODY with colored output for SEVERITY."
  `(progn
     (write-string (severity-color-code ,severity) ,stream)
     (unwind-protect
          (progn ,@body)
       (write-string +reset-color+ ,stream))))

;;; Diagnostic formatting

(defun format-diagnostic (diagnostic &key (style :text) (color t) (stream nil))
  "Format a diagnostic for display.

   STYLE can be :text, :json, or :sarif.
   COLOR enables ANSI colors (only for :text style)."
  (let ((output
          (ecase style
            (:text (format-diagnostic-text diagnostic color))
            (:json (format-diagnostic-json diagnostic)))))
    (if stream
        (write-string output stream)
        output)))

(defun format-diagnostic-text (diagnostic color)
  "Format diagnostic as human-readable text."
  (with-output-to-string (s)
    (let ((loc (diagnostic-location diagnostic))
          (sev (diagnostic-severity diagnostic)))

      ;; Location line: file:line:column:
      (when loc
        (format s "~A:~D:~D: "
                (or (source-span-file loc) "<unknown>")
                (source-span-start-line loc)
                (source-span-start-column loc)))

      ;; Severity with optional color
      (if color
          (format s "~A~A~A: "
                  (severity-color-code sev)
                  (string-downcase sev)
                  +reset-color+)
          (format s "~A: " (string-downcase sev)))

      ;; Optional error code
      (when (diagnostic-code diagnostic)
        (format s "[~A] " (diagnostic-code diagnostic)))

      ;; Message
      (format s "~A~%" (diagnostic-message diagnostic))

      ;; Source context with pointer
      (when (diagnostic-source-context diagnostic)
        (format s "  ~A~%" (diagnostic-source-context diagnostic))
        (when (and loc (plusp (source-span-start-column loc)))
          (format s "  ~V@T^~%" (source-span-start-column loc))))

      ;; Related diagnostics
      (dolist (rel (diagnostic-related diagnostic))
        (let ((rel-loc (related-diagnostic-location rel)))
          (when rel-loc
            (format s "  ~A:~D: "
                    (or (source-span-file rel-loc) "<unknown>")
                    (source-span-start-line rel-loc)))
          (format s "note: ~A~%" (related-diagnostic-message rel))))

      ;; Notes
      (dolist (note (diagnostic-notes diagnostic))
        (format s "  note: ~A~%" note))

      ;; Fix suggestions
      (dolist (fix (diagnostic-fixes diagnostic))
        (format s "  suggestion (~A): ~A~%"
                (suggested-fix-confidence fix)
                (suggested-fix-description fix))))))

(defun format-diagnostic-json (diagnostic)
  "Format diagnostic as JSON."
  (with-output-to-string (s)
    (format s "{")
    (format s "\"severity\":\"~A\""
            (string-downcase (diagnostic-severity diagnostic)))
    (when (diagnostic-code diagnostic)
      (format s ",\"code\":\"~A\"" (diagnostic-code diagnostic)))
    (format s ",\"message\":\"~A\""
            (escape-json-string (diagnostic-message diagnostic)))
    (when (diagnostic-location diagnostic)
      (let ((loc (diagnostic-location diagnostic)))
        (format s ",\"location\":{")
        (when (source-span-file loc)
          (format s "\"file\":\"~A\"," (escape-json-string (namestring (source-span-file loc)))))
        (format s "\"startLine\":~D" (source-span-start-line loc))
        (format s ",\"startColumn\":~D" (source-span-start-column loc))
        (format s ",\"endLine\":~D" (source-span-end-line loc))
        (format s ",\"endColumn\":~D}" (source-span-end-column loc))))
    (when (diagnostic-category diagnostic)
      (format s ",\"category\":\"~A\"" (string-downcase (diagnostic-category diagnostic))))
    (format s "}")))

(defun escape-json-string (string)
  "Escape a string for JSON output."
  (with-output-to-string (s)
    (loop for char across string
          do (case char
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (#\Return (write-string "\\r" s))
               (#\Tab (write-string "\\t" s))
               (otherwise (write-char char s))))))

(defun format-source-context (file line &key (context-lines 2))
  "Extract source context around LINE from FILE.

   Returns the source line with CONTEXT-LINES before and after."
  (when (and file (probe-file file))
    (handler-case
        (with-open-file (s file :direction :input)
          (let ((lines '())
                (start-line (max 1 (- line context-lines)))
                (end-line (+ line context-lines)))
            (loop for i from 1
                  for source-line = (read-line s nil nil)
                  while source-line
                  when (<= start-line i end-line)
                    do (push (format nil "~4D | ~A" i source-line) lines)
                  when (> i end-line)
                    do (return))
            (format nil "~{~A~^~%~}" (nreverse lines))))
      (error () nil))))
