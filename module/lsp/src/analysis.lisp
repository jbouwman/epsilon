(defpackage #:epsilon.lsp.analysis
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:reader #:epsilon.lib.reader)
   (#:string #:epsilon.lib.string))
  (:export
   #:analyze-document
   #:find-definition
   #:find-references
   #:get-symbols
   #:get-hover-info
   #:get-completions
   #:document-analysis
   #:make-document-analysis
   #:extract-symbols
   #:check-syntax-errors
   #:position-to-offset
   #:offset-to-position))

(in-package #:epsilon.lsp.analysis)

(defstruct document-analysis
  "Analysis result for a document."
  uri
  content
  symbols
  definitions
  references
  errors)

(defstruct symbol-info
  "Information about a symbol in the code."
  name
  type                    ; :function, :variable, :constant, :class, :package
  position                ; (line . column)
  range                   ; ((start-line . start-col) . (end-line . end-col))
  documentation
  package)

(defun analyze-document (uri content)
  "Analyze a document and return analysis results."
  (let ((symbols (extract-symbols content))
        (definitions (extract-definitions content))
        (references (extract-references content))
        (errors (check-syntax-errors content)))
    
    (make-document-analysis
     :uri uri
     :content content
     :symbols symbols
     :definitions definitions
     :references references
     :errors errors)))

;;; Symbol Extraction

(defun extract-symbols (content)
  "Extract all symbols from the document content."
  (let ((symbols '())
        (line-number 1))
    
    ;; Simple regex-based symbol extraction for now
    ;; TODO: Replace with proper AST-based analysis
    (let ((lines (string:split content #\Newline)))
      (dolist (line lines)
        (extract-symbols-from-line line line-number symbols)
        (incf line-number)))
    
    (nreverse symbols)))

(defun extract-symbols-from-line (line line-number symbols)
  "Extract symbols from a single line."
  ;; Look for function definitions
  (when (search "(defun " line)
    (let ((start (search "(defun " line)))
      (when start
        (let* ((after-defun (+ start 7))
               (space-pos (position #\Space line :start after-defun))
               (paren-pos (position #\( line :start after-defun)))
          (when (and space-pos (or (not paren-pos) (< space-pos paren-pos)))
            (let ((symbol-name (string-trim " " (subseq line after-defun space-pos))))
              (when (> (length symbol-name) 0)
                (push (make-symbol-info
                       :name symbol-name
                       :type :function
                       :position (cons line-number start)
                       :range (cons (cons line-number start) 
                                   (cons line-number (length line))))
                      symbols))))))))
  
  ;; Look for variable definitions
  (when (search "(defvar " line)
    (let ((start (search "(defvar " line)))
      (when start
        (let* ((after-defvar (+ start 8))
               (space-pos (position #\Space line :start after-defvar)))
          (when space-pos
            (let ((symbol-name (string-trim " " (subseq line after-defvar space-pos))))
              (when (> (length symbol-name) 0)
                (push (make-symbol-info
                       :name symbol-name
                       :type :variable
                       :position (cons line-number start)
                       :range (cons (cons line-number start) 
                                   (cons line-number (length line))))
                      symbols))))))))
  
  ;; Look for package definitions
  (when (search "(defpackage " line)
    (let ((start (search "(defpackage " line)))
      (when start
        (let* ((after-defpackage (+ start 12))
               (space-pos (position #\Space line :start after-defpackage)))
          (when space-pos
            (let ((package-name (string-trim " " (subseq line after-defpackage space-pos))))
              (when (> (length package-name) 0)
                (push (make-symbol-info
                       :name package-name
                       :type :package
                       :position (cons line-number start)
                       :range (cons (cons line-number start) 
                                   (cons line-number (length line))))
                      symbols)))))))))

(defun extract-definitions (content)
  "Extract all definitions from the document."
  ;; For now, definitions are the same as symbols
  ;; TODO: Separate definitions from references
  (extract-symbols content))

(defun extract-references (content)
  "Extract all symbol references from the document."
  ;; TODO: Implement reference extraction
  (declare (ignore content))
  '())

(defun check-syntax-errors (content)
  "Check for syntax errors in the content."
  ;; TODO: Implement syntax checking using reader
  (handler-case
      (let ((forms '()))
        (with-input-from-string (stream content)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (push form forms)))
        '()) ; No errors
    (error (e)
      (list (format nil "Syntax error: ~A" e)))))

;;; Language Server Features

(defun find-definition (content position)
  "Find the definition of the symbol at the given position."
  (declare (ignore content position))
  ;; TODO: Implement definition lookup
  nil)

(defun find-references (content symbol-name)
  "Find all references to the given symbol."
  (declare (ignore content symbol-name))
  ;; TODO: Implement reference finding
  '())

(defun get-symbols (content &optional filter)
  "Get all symbols in the document, optionally filtered."
  (let ((symbols (extract-symbols content)))
    (if filter
        (remove-if-not filter symbols)
        symbols)))

(defun get-hover-info (content position)
  "Get hover information for the symbol at the given position."
  (declare (ignore content position))
  ;; TODO: Implement hover information
  nil)

(defun get-completions (content position)
  "Get completion suggestions for the given position."
  (declare (ignore content position))
  ;; TODO: Implement completion suggestions
  '())

;;; Utility Functions

(defun position-to-offset (content line column)
  "Convert line/column position to character offset."
  (let ((lines (string:split content #\Newline))
        (offset 0))
    (dotimes (i (1- line))
      (when (< i (length lines))
        (incf offset (length (nth i lines)))
        (incf offset 1))) ; newline character
    (+ offset column)))

(defun offset-to-position (content offset)
  "Convert character offset to line/column position."
  (let ((line 1)
        (column 0)
        (current-offset 0))
    (loop for char across content
          while (< current-offset offset)
          do (if (char= char #\Newline)
                 (progn
                   (incf line)
                   (setf column 0))
                 (incf column))
             (incf current-offset))
    (cons line column)))