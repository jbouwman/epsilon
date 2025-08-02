(defpackage #:epsilon.lsp.analysis
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:build #:epsilon.tool.build))
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
   #:offset-to-position
   #:symbol-at-position
   #:completion-items
   #:diagnostic-info))

(in-package #:epsilon.lsp.analysis)

(defstruct document-analysis
  "Analysis result for a document."
  uri
  content
  symbols
  definitions
  references
  errors
  forms             ; Parsed lisp forms
  package-info      ; Package definition info
  dependencies      ; Required packages/nicknames
  exports           ; Exported symbols
  imports)          ; Imported symbols with nicknames

(defstruct symbol-info
  "Information about a symbol in the code."
  name
  type                    ; :function, :variable, :constant, :class, :package, :macro
  position                ; (line . column)
  range                   ; ((start-line . start-col) . (end-line . end-col))
  documentation
  package
  signature               ; Function signature/lambda list
  form                    ; Original form for context
  definition-type         ; :defun, :defvar, :defpackage, :defclass, etc.
  qualified-name)         ; Fully qualified name with package

(defun analyze-document (uri content)
  "Analyze a document."
  (multiple-value-bind (forms errors) (parse-document-forms content)
    (let* ((package-info (extract-package-info forms))
           (dependencies (extract-dependencies forms))
           (symbols (extract-symbols-from-forms forms content))
           (definitions (filter-definitions symbols))
           (references (extract-references-from-forms forms))
           (exports (extract-exports forms))
           (imports (extract-imports forms)))
      
      (make-document-analysis
       :uri uri
       :content content
       :symbols symbols
       :definitions definitions
       :references references
       :errors errors
       :forms forms
       :package-info package-info
       :dependencies dependencies
       :exports exports
       :imports imports))))

;;; Form Parsing

(defun parse-document-forms (content)
  "Parse all forms in the document, returning forms and any errors."
  (let ((forms '())
        (errors '())
        (line-number 1)
        (char-offset 0))
    
    (handler-case
        (with-input-from-string (stream content)
          (loop
            (let ((start-pos (file-position stream)))
              (multiple-value-bind (form condition)
                  (ignore-errors (read stream nil :eof))
                (cond
                  ((eq form :eof)
                   (return))
                  (condition
                   (push (format nil "Parse error at position ~D: ~A" start-pos condition) errors)
                   (return))
                  (t
                   (let ((end-pos (file-position stream)))
                     (multiple-value-bind (start-line start-col)
                         (offset-to-position content start-pos)
                       (multiple-value-bind (end-line end-col)
                           (offset-to-position content end-pos)
                         (push (list form start-line start-col end-line end-col) forms))))))))) 
      (error (e)
        (push (format nil "Document parse error: ~A" e) errors)))
    
    (values (nreverse forms) errors)))

(defun extract-symbols-from-forms (forms content)
  "Extract all symbols from parsed forms with full context."
  (let ((symbols '()))
    (dolist (form-data forms)
      (destructuring-bind (form start-line start-col end-line end-col) form-data
        (let ((extracted (analyze-form form start-line start-col end-line end-col content)))
          (when extracted
            (if (listp extracted)
                (setf symbols (append extracted symbols))
                (push extracted symbols))))))
    (nreverse symbols)))

(defun analyze-form (form start-line start-col end-line end-col content)
  "Analyze a single form and extract symbol information."
  (when (listp form)
    (let ((operator (first form)))
      (case operator
        ((defun)
         (analyze-defun-form form start-line start-col end-line end-col))
        ((defvar defparameter defconstant)
         (analyze-defvar-form form start-line start-col end-line end-col operator))
        ((defpackage)
         (analyze-defpackage-form form start-line start-col end-line end-col))
        ((defmacro)
         (analyze-defmacro-form form start-line start-col end-line end-col))
        ((defclass defstruct)
         (analyze-defclass-form form start-line start-col end-line end-col operator))
        ((defmethod defgeneric)
         (analyze-defmethod-form form start-line start-col end-line end-col operator))
        (t nil)))))

(defun analyze-defun-form (form start-line start-col end-line end-col)
  "Analyze a defun form."
  (when (>= (length form) 3)
    (let ((name (second form))
          (lambda-list (third form))
          (doc-string (when (and (>= (length form) 4)
                                 (stringp (fourth form)))
                        (fourth form))))
      (make-symbol-info
       :name (symbol-name name)
       :type :function
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :signature lambda-list
       :documentation doc-string
       :definition-type :defun
       :form form
       :qualified-name (symbol-name name)))))

(defun analyze-defvar-form (form start-line start-col end-line end-col type)
  "Analyze a defvar/defparameter/defconstant form."
  (when (>= (length form) 2)
    (let ((name (second form))
          (doc-string (when (and (>= (length form) 4)
                                 (stringp (fourth form)))
                        (fourth form))))
      (make-symbol-info
       :name (symbol-name name)
       :type (case type
               ((defvar defparameter) :variable)
               (defconstant :constant))
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :documentation doc-string
       :definition-type type
       :form form
       :qualified-name (symbol-name name)))))

(defun analyze-defpackage-form (form start-line start-col end-line end-col)
  "Analyze a defpackage form."
  (when (>= (length form) 2)
    (let ((name (second form)))
      (make-symbol-info
       :name (symbol-name name)
       :type :package
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :definition-type :defpackage
       :form form
       :qualified-name (symbol-name name)))))

(defun analyze-defmacro-form (form start-line start-col end-line end-col)
  "Analyze a defmacro form."
  (when (>= (length form) 3)
    (let ((name (second form))
          (lambda-list (third form))
          (doc-string (when (and (>= (length form) 4)
                                 (stringp (fourth form)))
                        (fourth form))))
      (make-symbol-info
       :name (symbol-name name)
       :type :macro
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :signature lambda-list
       :documentation doc-string
       :definition-type :defmacro
       :form form
       :qualified-name (symbol-name name)))))

(defun analyze-defclass-form (form start-line start-col end-line end-col type)
  "Analyze a defclass/defstruct form."
  (when (>= (length form) 2)
    (let ((name (second form)))
      (make-symbol-info
       :name (symbol-name name)
       :type :class
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :definition-type type
       :form form
       :qualified-name (symbol-name name)))))

(defun analyze-defmethod-form (form start-line start-col end-line end-col type)
  "Analyze a defmethod/defgeneric form."
  (when (>= (length form) 3)
    (let ((name (second form))
          (lambda-list (third form)))
      (make-symbol-info
       :name (symbol-name name)
       :type :function
       :position (cons start-line start-col)
       :range (cons (cons start-line start-col) (cons end-line end-col))
       :signature lambda-list
       :definition-type type
       :form form
       :qualified-name (symbol-name name)))))

(defun filter-definitions (symbols)
  "Filter symbols to only include definitions."
  (remove-if-not (lambda (symbol)
                   (member (symbol-info-definition-type symbol)
                           '(:defun :defvar :defparameter :defconstant 
                             :defpackage :defmacro :defclass :defstruct
                             :defmethod :defgeneric)))
                 symbols))

(defun extract-references-from-forms (forms)
  "Extract symbol references from parsed forms."
  (let ((references '()))
    (dolist (form-data forms)
      (destructuring-bind (form start-line start-col end-line end-col) form-data
        (let ((refs (collect-symbol-references form start-line start-col)))
          (setf references (append refs references)))))
    (nreverse references)))

(defun collect-symbol-references (form line col)
  "Recursively collect symbol references from a form."
  (let ((references '()))
    (labels ((collect-refs (obj current-line current-col)
               (cond
                 ((symbolp obj)
                  (unless (keywordp obj)
                    (push (make-symbol-info
                           :name (symbol-name obj)
                           :type :reference
                           :position (cons current-line current-col)
                           :qualified-name (symbol-name obj))
                          references)))
                 ((listp obj)
                  (dolist (item obj)
                    (collect-refs item current-line current-col))))))
      (collect-refs form line col)
      references)))

(defun check-syntax-errors (content)
  "Check for syntax errors in the content."
  (let ((errors '()))
    (handler-case
        (with-input-from-string (stream content)
          (loop 
            (let ((pos (file-position stream)))
              (handler-case
                  (let ((form (read stream nil :eof)))
                    (when (eq form :eof)
                      (return)))
                (end-of-file ()
                  (push (make-diagnostic pos "Unexpected end of file") errors)
                  (return))
                (reader-error (e)
                  (push (make-diagnostic pos (format nil "Reader error: ~A" e)) errors)
                  (return))))))
      (error (e)
        (push (make-diagnostic 0 (format nil "Parse error: ~A" e)) errors)))
    errors))

(defstruct diagnostic
  "Diagnostic information for errors/warnings."
  position
  message
  severity)  ; :error, :warning, :information, :hint

(defun make-diagnostic (position message &optional (severity :error))
  "Create a diagnostic entry."
  (make-instance 'diagnostic :position position :message message :severity severity))

;;; Advanced Analysis Functions

(defun extract-package-info (forms)
  "Extract package information from forms."
  (dolist (form-data forms)
    (destructuring-bind (form start-line start-col end-line end-col) form-data
      (declare (ignore start-line start-col end-line end-col))
      (when (and (listp form) (eq (first form) 'defpackage))
        (return (analyze-package-definition form)))))
  nil)

(defun analyze-package-definition (form)
  "Analyze a defpackage form for package information."
  (let ((name (second form))
        (clauses (cddr form))
        (uses '())
        (nicknames '())
        (exports '()))
    
    (dolist (clause clauses)
      (when (listp clause)
        (case (first clause)
          (:use (setf uses (append uses (rest clause))))
          (:local-nicknames (setf nicknames (append nicknames (rest clause))))
          (:export (setf exports (append exports (rest clause)))))))
    
    (map:make-map
     "name" (symbol-name name)
     "uses" (mapcar #'symbol-name uses)
     "nicknames" nicknames
     "exports" (mapcar #'symbol-name exports))))

(defun extract-dependencies (forms)
  "Extract package dependencies from forms."
  (let ((deps '()))
    (dolist (form-data forms)
      (destructuring-bind (form start-line start-col end-line end-col) form-data
        (declare (ignore start-line start-col end-line end-col))
        (when (and (listp form) (eq (first form) 'defpackage))
          (let ((clauses (cddr form)))
            (dolist (clause clauses)
              (when (listp clause)
                (case (first clause)
                  (:use (setf deps (append deps (mapcar #'symbol-name (rest clause)))))
                  (:local-nicknames
                   (dolist (nickname (rest clause))
                     (when (listp nickname)
                       (push (symbol-name (second nickname)) deps))))))))
          (return))))
    (remove-duplicates deps :test #'string=)))

(defun extract-exports (forms)
  "Extract exported symbols from package definition."
  (let ((package-info (extract-package-info forms)))
    (when package-info
      (map:get package-info "exports"))))

(defun extract-imports (forms)
  "Extract imported symbols and nicknames."
  (let ((package-info (extract-package-info forms)))
    (when package-info
      (append (map:get package-info "uses")
              (mapcar #'first (map:get package-info "nicknames"))))))

;;; Language Server Features

(defun find-definition (analysis symbol-name position)
  "Find the definition of the symbol at the given position."
  (dolist (symbol (document-analysis-definitions analysis))
    (when (string= (symbol-info-name symbol) symbol-name)
      (return symbol)))
  nil)

(defun find-references (analysis symbol-name)
  "Find all references to the given symbol."
  (remove-if-not (lambda (ref)
                   (string= (symbol-info-name ref) symbol-name))
                 (document-analysis-references analysis)))

(defun get-symbols (content &optional filter)
  "Get all symbols in the document, optionally filtered."
  (let ((symbols (extract-symbols content)))
    (if filter
        (remove-if-not filter symbols)
        symbols)))

(defun get-hover-info (analysis position)
  "Get hover information for the symbol at the given position."
  (let ((symbol (symbol-at-position analysis position)))
    (when symbol
      (let ((hover-text (format nil "**~A** (~A)" 
                                (symbol-info-name symbol)
                                (symbol-info-type symbol))))
        (when (symbol-info-signature symbol)
          (setf hover-text (format nil "~A~%~%**Signature:** ~A" 
                                   hover-text 
                                   (symbol-info-signature symbol))))
        (when (symbol-info-documentation symbol)
          (setf hover-text (format nil "~A~%~%~A" 
                                   hover-text 
                                   (symbol-info-documentation symbol))))
        hover-text))))

(defun get-completions (analysis position prefix)
  "Get completion suggestions for the given position."
  (let ((completions '()))
    ;; Add symbols from current document
    (dolist (symbol (document-analysis-symbols analysis))
      (when (str:starts-with-p (symbol-info-name symbol) prefix)
        (push (make-completion-item symbol) completions)))
    
    ;; Add Common Lisp built-ins if no package prefix
    (unless (position #\: prefix)
      (dolist (builtin '("defun" "defvar" "defpackage" "let" "lambda" "if" "when" "unless"
                         "cond" "case" "loop" "dolist" "dotimes" "format" "mapcar" "length"))
        (when (str:starts-with-p builtin prefix)
          (push (map:make-map
                 "label" builtin
                 "kind" 3  ; Function
                 "detail" "Common Lisp") completions))))
    
    completions))

(defun make-completion-item (symbol)
  "Create a completion item from a symbol."
  (map:make-map
   "label" (symbol-info-name symbol)
   "kind" (case (symbol-info-type symbol)
            (:function 3)
            (:variable 6)
            (:class 7)
            (:package 9)
            (:macro 3)
            (t 1))  ; Text
   "detail" (format nil "~A" (symbol-info-type symbol))
   "documentation" (or (symbol-info-documentation symbol) "")
   "insertText" (symbol-info-name symbol)))

(defun symbol-at-position (analysis position)
  "Find the symbol at the given position."
  (let ((line (car position))
        (column (cdr position)))
    (dolist (symbol (document-analysis-symbols analysis))
      (let ((sym-line (car (symbol-info-position symbol)))
            (range (symbol-info-range symbol)))
        (when (and (= line sym-line)
                   (let ((start-col (cdr (car range)))
                         (end-col (cdr (cdr range))))
                     (and (>= column start-col) (<= column end-col))))
          (return symbol))))
    nil))

;;; Utility Functions

(defun position-to-offset (content line column)
  "Convert line/column position (1-based) to character offset."
  (let ((lines (str:split content #\Newline))
        (offset 0))
    (dotimes (i (1- line))
      (when (< i (length lines))
        (incf offset (length (nth i lines)))
        (incf offset 1))) ; newline character
    (+ offset (1- column)))) ; Convert to 0-based column

(defun offset-to-position (content offset)
  "Convert character offset to line/column position (1-based)."
  (let ((line 1)
        (column 1)
        (current-offset 0))
    (loop for char across content
          while (< current-offset offset)
          do (if (char= char #\Newline)
                 (progn
                   (incf line)
                   (setf column 1))
                 (incf column))
             (incf current-offset))
    (values line column)))
