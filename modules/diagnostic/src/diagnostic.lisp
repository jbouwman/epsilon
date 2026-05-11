;;;; diagnostic.lisp - Canonical compiler/runtime diagnostics
;;;;
;;;; Structured diagnostic type with source locations, fix suggestions,
;;;; and related diagnostics. Used by compiler, compile-tracking,
;;;; stacktrace, LSP, and analyze modules.

(cl:defpackage :epsilon.diagnostic
  (:use :cl)
  (:local-nicknames
   (loc epsilon.source-location))
  (:export
   ;; Severity
   #:severity

   ;; Diagnostic
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
   #:source-edit-location
   #:source-edit-replacement

   ;; Related diagnostics
   #:related-diagnostic
   #:make-related-diagnostic
   #:related-diagnostic-p
   #:related-diagnostic-message
   #:related-diagnostic-location

   ;; Creation helpers
   #:make-error
   #:make-warning
   #:make-style-warning
   #:make-note
   #:make-info
   #:make-hint

   ;; Emission
   #:emit-diagnostic
   #:*diagnostic-handler*

   ;; Formatting (portable, no ANSI color)
   #:format-diagnostic
   #:format-diagnostic-text
   #:format-diagnostic-json))

(in-package :epsilon.diagnostic)

;;; ====================================================================
;;; Severity
;;; ====================================================================

(deftype severity ()
  '(member :error :warning :style-warning :note :info :hint))

;;; ====================================================================
;;; Suggested Fix
;;; ====================================================================

(defstruct (suggested-fix
            (:constructor %make-suggested-fix)
            (:copier nil))
  "A suggested code fix with source edits."
  (description "" :type string)
  (edits nil :type list)
  (confidence :high :type (member :high :medium :low)))

(defun make-suggested-fix (&key description edits (confidence :high))
  "Create a suggested fix."
  (%make-suggested-fix
   :description (or description "")
   :edits edits
   :confidence confidence))

;;; ====================================================================
;;; Source Edit
;;; ====================================================================

(defstruct (source-edit
            (:constructor %make-source-edit)
            (:copier nil))
  "A single source code edit - replace location range with replacement text."
  (location nil :type (or loc:source-location null))
  (replacement "" :type string))

(defun make-source-edit (&key location replacement)
  "Create a source edit."
  (%make-source-edit
   :location location
   :replacement (or replacement "")))

;;; ====================================================================
;;; Related Diagnostic
;;; ====================================================================

(defstruct (related-diagnostic
            (:constructor %make-related-diagnostic)
            (:copier nil))
  "A related location with a message."
  (message "" :type string)
  (location nil :type (or loc:source-location null)))

(defun make-related-diagnostic (&key message location)
  "Create a related diagnostic."
  (%make-related-diagnostic
   :message (or message "")
   :location location))

;;; ====================================================================
;;; Diagnostic
;;; ====================================================================

(defstruct (diagnostic
            (:constructor %make-diagnostic)
            (:copier nil))
  "A compiler or runtime diagnostic with rich information.

   Uses source-location for position, supports related diagnostics,
   fix suggestions, and categorization."
  ;; Core
  (severity :error :type severity)
  (code nil :type (or string null))
  (message "" :type string)
  ;; Location
  (location nil :type (or loc:source-location null))
  (source-context nil :type (or string null))
  ;; Context
  (related nil :type list)
  (notes nil :type list)
  (enclosing-form nil :type t)
  ;; Fixes
  (fixes nil :type list)
  ;; Metadata
  (category nil :type (or keyword null))
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

;;; ====================================================================
;;; Creation Helpers
;;; ====================================================================

(defun make-error (message &key location code related fixes category)
  "Create an error diagnostic."
  (make-diagnostic :severity :error :message message :location location
                   :code code :related related :fixes fixes :category category))

(defun make-warning (message &key location code related fixes category)
  "Create a warning diagnostic."
  (make-diagnostic :severity :warning :message message :location location
                   :code code :related related :fixes fixes :category category))

(defun make-style-warning (message &key location code related fixes)
  "Create a style-warning diagnostic."
  (make-diagnostic :severity :style-warning :message message :location location
                   :code code :related related :fixes fixes :category :style))

(defun make-note (message &key location)
  "Create a note diagnostic."
  (make-diagnostic :severity :note :message message :location location))

(defun make-info (message &key location)
  "Create an info diagnostic."
  (make-diagnostic :severity :info :message message :location location))

(defun make-hint (message &key location fixes)
  "Create a hint diagnostic."
  (make-diagnostic :severity :hint :message message :location location :fixes fixes))

;;; ====================================================================
;;; Emission
;;; ====================================================================

(defvar *diagnostic-handler* nil
  "Current diagnostic handler function. Called with one argument (diagnostic).")

(defun emit-diagnostic (diagnostic)
  "Emit a diagnostic through the current handler. Returns the diagnostic."
  (when *diagnostic-handler*
    (funcall *diagnostic-handler* diagnostic))
  diagnostic)

;;; ====================================================================
;;; Formatting (portable, no ANSI color)
;;; ====================================================================

(defun format-diagnostic (diagnostic &key (style :text) (stream nil))
  "Format a diagnostic for display. STYLE is :text or :json."
  (let ((output (ecase style
                  (:text (format-diagnostic-text diagnostic))
                  (:json (format-diagnostic-json diagnostic)))))
    (if stream
        (write-string output stream)
        output)))

(defun format-diagnostic-text (diagnostic)
  "Format diagnostic as human-readable text (no color)."
  (with-output-to-string (s)
    (let ((loc (diagnostic-location diagnostic))
          (sev (diagnostic-severity diagnostic)))
      ;; Location
      (when loc
        (format s "~A: " (loc:format-source-location loc)))
      ;; Severity
      (format s "~A: " (string-downcase sev))
      ;; Error code
      (when (diagnostic-code diagnostic)
        (format s "[~A] " (diagnostic-code diagnostic)))
      ;; Message
      (format s "~A~%" (diagnostic-message diagnostic))
      ;; Source context
      (when (diagnostic-source-context diagnostic)
        (format s "  ~A~%" (diagnostic-source-context diagnostic)))
      ;; Related
      (dolist (rel (diagnostic-related diagnostic))
        (let ((rel-loc (related-diagnostic-location rel)))
          (when rel-loc
            (format s "  ~A: " (loc:format-source-location rel-loc)))
          (format s "note: ~A~%" (related-diagnostic-message rel))))
      ;; Notes
      (dolist (note (diagnostic-notes diagnostic))
        (format s "  note: ~A~%" note))
      ;; Fixes
      (dolist (fix (diagnostic-fixes diagnostic))
        (format s "  suggestion (~A): ~A~%"
                (suggested-fix-confidence fix)
                (suggested-fix-description fix))))))

(defun %escape-json-string (string)
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

(defun format-diagnostic-json (diagnostic)
  "Format diagnostic as JSON string."
  (with-output-to-string (s)
    (format s "{")
    (format s "\"severity\":\"~A\""
            (string-downcase (diagnostic-severity diagnostic)))
    (when (diagnostic-code diagnostic)
      (format s ",\"code\":\"~A\"" (diagnostic-code diagnostic)))
    (format s ",\"message\":\"~A\""
            (%escape-json-string (diagnostic-message diagnostic)))
    (when (diagnostic-location diagnostic)
      (let ((loc (diagnostic-location diagnostic)))
        (format s ",\"location\":{")
        (let ((first t))
          (when (loc:source-location-file loc)
            (format s "\"file\":\"~A\""
                    (%escape-json-string
                     (let ((f (loc:source-location-file loc)))
                       (if (pathnamep f) (namestring f) f))))
            (setf first nil))
          (when (loc:source-location-offset loc)
            (unless first (write-char #\, s))
            (format s "\"offset\":~D" (loc:source-location-offset loc))
            (setf first nil))
          (when (loc:source-location-end-offset loc)
            (unless first (write-char #\, s))
            (format s "\"endOffset\":~D" (loc:source-location-end-offset loc))
            (setf first nil))
          (when (loc:source-location-line loc)
            (unless first (write-char #\, s))
            (format s "\"startLine\":~D" (loc:source-location-line loc))
            (setf first nil))
          (when (loc:source-location-column loc)
            (unless first (write-char #\, s))
            (format s "\"startColumn\":~D" (loc:source-location-column loc))
            (setf first nil))
          (when (loc:source-location-end-line loc)
            (unless first (write-char #\, s))
            (format s "\"endLine\":~D" (loc:source-location-end-line loc))
            (setf first nil))
          (when (loc:source-location-end-column loc)
            (unless first (write-char #\, s))
            (format s "\"endColumn\":~D" (loc:source-location-end-column loc))))
        (format s "}")))
    (when (diagnostic-category diagnostic)
      (format s ",\"category\":\"~A\""
              (string-downcase (diagnostic-category diagnostic))))
    (format s "}")))
