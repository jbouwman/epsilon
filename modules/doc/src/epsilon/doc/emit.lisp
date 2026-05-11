;;;; epsilon.doc.emit -- Documentation output formatting
;;;;
;;;; Emits documentation plists in JSON or S-expression format.

(defpackage epsilon.doc.emit
  (:use :cl)
  (:import (epsilon.json json))
  (:export
   #:emit))

;;; Plist to map conversion for JSON

(defun plist-to-map (plist)
  "Convert a documentation PLIST to a map suitable for JSON encoding.
   Skips NIL values.  Recursively converts nested plists and lists of plists.
   Keyword values are converted to lowercase strings."
  (let ((pairs nil))
    (loop for (key val) on plist by #'cddr
          when val
          do (let ((k (string-downcase (symbol-name key)))
                   (v (convert-value val)))
               (push v pairs)
               (push k pairs)))
    (apply #'epsilon.map:make-map pairs)))

(defun convert-value (value)
  "Convert a documentation value for JSON serialization."
  (cond
    ;; Keyword -> lowercase string
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ;; List of plists (e.g., params, examples)
    ((and (listp value)
          (consp (first value))
          (keywordp (first (first value))))
     (mapcar #'plist-to-map value))
    ;; Plain list (e.g., :see references, :tags, lambda-list)
    ((listp value)
     (mapcar (lambda (v)
               (if (keywordp v)
                 (string-downcase (symbol-name v))
                 (if (symbolp v)
                   (symbol-name v)
                   v)))
             value))
    ;; Everything else (strings, numbers)
    (t value)))

;;; Emission

(defun emit (format data)
  "Emit documentation DATA in FORMAT (:json or :sexp).

   :param format -- :json or :sexp.
   :param data   -- A documentation plist from describe-symbol or describe-module.
   :returns      -- A string in the requested format."
  (case format
    (:sexp (let ((*print-case* :upcase))
             (prin1-to-string data)))
    (:json (json:encode-to-string (plist-to-map data)))
    (otherwise (error "Unknown emit format: ~A" format))))
