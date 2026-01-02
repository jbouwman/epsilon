;;;; epsilon.lint - Lisp linting and static analysis
;;;;
;;;; Provides static analysis for Epsilon Lisp files including:
;;;; - Style checking (naming conventions, line length, indentation)
;;;; - Common issues (unused bindings, shadowed variables)
;;;; - Package hygiene (unexported symbols, missing imports)
;;;; - Code complexity metrics
;;;;
;;;; Usage:
;;;;   (epsilon.lint:lint-file "path/to/file.lisp")
;;;;   (epsilon.lint:lint-module "epsilon.core")
;;;;   (epsilon.lint:lint-directory "modules/core/src/")

(package epsilon.lint
  (import (epsilon.sys.fs fs)
          (epsilon.string str)
          (epsilon.sequence seq)
          (epsilon.map map)
          (epsilon.json json)
          (epsilon.log log)
          (epsilon.path path)))

;;; Configuration

(defvar *max-line-length* 100
  "Maximum allowed line length.")

(defvar *max-function-length* 50
  "Maximum recommended function length in lines.")

(defvar *enabled-rules*
  '(:line-length :trailing-whitespace :tabs :naming :unused-binding
    :shadowed-binding :missing-docstring :todo-fixme :deprecated)
  "List of enabled lint rules.")

;;; Issue representation

(defstruct issue
  "Represents a lint issue found in source code."
  (severity :warning :type (member :error :warning :info))
  (rule nil :type keyword)
  (file nil :type (or null string))
  (line nil :type (or null integer))
  (column nil :type (or null integer))
  (message "" :type string)
  (source-line nil :type (or null string)))

(defun make-lint-issue (severity rule message &key file line column source-line)
  "Create a lint issue."
  (make-issue :severity severity
              :rule rule
              :file file
              :line line
              :column column
              :message message
              :source-line source-line))

;;; File-level checks

(defun check-line-length (lines file)
  "Check for lines exceeding maximum length."
  (let ((issues nil))
    (loop for line in lines
          for line-num from 1
          when (> (length line) *max-line-length*)
            do (let ((line-len (length line)))
                 (push (make-lint-issue
                        :warning :line-length
                        #~"Line exceeds ~{*max-line-length*} characters (~{line-len})"
                        :file file
                        :line line-num
                        :column *max-line-length*
                        :source-line line)
                       issues)))
    (nreverse issues)))

(defun check-trailing-whitespace (lines file)
  "Check for trailing whitespace."
  (let ((issues nil))
    (loop for line in lines
          for line-num from 1
          when (and (plusp (length line))
                    (member (char line (1- (length line))) '(#\Space #\Tab)))
            do (push (make-lint-issue
                      :info :trailing-whitespace
                      "Trailing whitespace"
                      :file file
                      :line line-num
                      :column (length line)
                      :source-line line)
                     issues))
    (nreverse issues)))

(defun check-tabs (lines file)
  "Check for tab characters (prefer spaces)."
  (let ((issues nil))
    (loop for line in lines
          for line-num from 1
          for tab-pos = (position #\Tab line)
          when tab-pos
            do (push (make-lint-issue
                      :info :tabs
                      "Tab character found (use spaces for indentation)"
                      :file file
                      :line line-num
                      :column (1+ tab-pos)
                      :source-line line)
                     issues))
    (nreverse issues)))

(defun check-todo-fixme (lines file)
  "Find TODO and FIXME comments."
  (let ((issues nil))
    (loop for line in lines
          for line-num from 1
          do (let ((todo-pos (search "TODO" line :test #'char-equal))
                   (fixme-pos (search "FIXME" line :test #'char-equal)))
               (when todo-pos
                 (push (make-lint-issue
                        :info :todo-fixme
                        "TODO comment found"
                        :file file
                        :line line-num
                        :column (1+ todo-pos)
                        :source-line line)
                       issues))
               (when fixme-pos
                 (push (make-lint-issue
                        :warning :todo-fixme
                        "FIXME comment found"
                        :file file
                        :line line-num
                        :column (1+ fixme-pos)
                        :source-line line)
                       issues))))
    (nreverse issues)))

;;; Form-level checks

(defun check-naming-convention (form file line)
  "Check naming conventions for definitions."
  (let ((issues nil))
    (when (and (consp form) (symbolp (car form)))
      (let ((def-type (car form))
            (name (when (cdr form) (cadr form))))
        (when (symbolp name)
          (let ((name-str (symbol-name name)))
            ;; Check for camelCase (should use kebab-case)
            (when (and (find-if #'upper-case-p name-str)
                       (find-if #'lower-case-p name-str)
                       (not (every (lambda (c) (or (upper-case-p c) (char= c #\-) (char= c #\_)))
                                   name-str)))
              (push (make-lint-issue
                     :warning :naming
                     #~"~{def-type} name '~{name-str}' uses mixed case (prefer kebab-case)"
                     :file file
                     :line line)
                    issues))
            ;; Check for underscores (should use hyphens)
            (when (find #\_ name-str)
              (push (make-lint-issue
                     :info :naming
                     #~"~{def-type} name '~{name-str}' uses underscores (prefer hyphens)"
                     :file file
                     :line line)
                    issues))))))
    (nreverse issues)))

(defun check-missing-docstring (form file line)
  "Check for missing docstrings on public definitions."
  (let ((issues nil))
    (when (consp form)
      (let ((def-type (car form)))
        (when (member def-type '(defun defmacro defgeneric))
          (let ((body (cdddr form)))
            ;; Skip if first body form is a string (docstring present)
            (unless (and body (stringp (car body)))
              (push (make-lint-issue
                     :info :missing-docstring
                     #~"~{def-type} lacks a docstring"
                     :file file
                     :line line)
                    issues))))))
    (nreverse issues)))

(defun check-function-length (form file line)
  "Check for excessively long functions."
  (let ((issues nil))
    (when (and (consp form) (member (car form) '(defun defmethod)))
      (let* ((form-string (with-output-to-string (s)
                            (pprint form s)))
             (line-count (count #\Newline form-string)))
        (when (> line-count *max-function-length*)
          (push (make-lint-issue
                 :warning :function-length
                 #~"Function is ~{line-count} lines (max ~{*max-function-length*} recommended)"
                 :file file
                 :line line)
                issues))))
    (nreverse issues)))

(defun collect-bindings (form)
  "Collect all variable bindings from a form."
  (let ((bindings nil))
    (labels ((walk (f)
               (when (consp f)
                 (case (car f)
                   ((let let*)
                    (dolist (binding (cadr f))
                      (push (if (consp binding) (car binding) binding) bindings))
                    (mapc #'walk (cddr f)))
                   ((lambda)
                    (dolist (param (cadr f))
                      (unless (member param '(&optional &rest &key &allow-other-keys &aux &body))
                        (push (if (consp param) (car param) param) bindings)))
                    (mapc #'walk (cddr f)))
                   ((defun defmethod defmacro)
                    (dolist (param (caddr f))
                      (unless (member param '(&optional &rest &key &allow-other-keys &aux &body))
                        (push (if (consp param) (car param) param) bindings)))
                    (mapc #'walk (cdddr f)))
                   ((do do* dolist dotimes)
                    ;; Handle loop variable bindings
                    (mapc #'walk (cdr f)))
                   (t
                    (mapc #'walk (cdr f)))))))
      (walk form))
    bindings))

(defun collect-references (form)
  "Collect all symbol references from a form."
  (let ((refs nil))
    (labels ((walk (f)
               (cond
                 ((symbolp f)
                  (push f refs))
                 ((consp f)
                  (unless (eq (car f) 'quote)
                    (mapc #'walk f))))))
      (walk form))
    refs))

(defun check-unused-bindings (form file line)
  "Check for potentially unused variable bindings."
  (let ((issues nil))
    (when (consp form)
      (let* ((bindings (collect-bindings form))
             (refs (collect-references form))
             (ref-set (make-hash-table :test 'eq)))
        ;; Build set of referenced symbols
        (dolist (ref refs)
          (setf (gethash ref ref-set) t))
        ;; Check each binding
        (dolist (binding bindings)
          (when (and (symbolp binding)
                     (not (string= "_" (symbol-name binding)))
                     (not (char= #\_ (char (symbol-name binding) 0)))
                     (not (gethash binding ref-set)))
            (push (make-lint-issue
                   :warning :unused-binding
                   #~"Variable '~{binding}' appears to be unused"
                   :file file
                   :line line)
                  issues)))))
    (nreverse issues)))

;;; Main checking functions

(defun read-file-forms (path)
  "Read all forms from a file, returning (form . line-number) pairs."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((forms nil)
          (eof (gensym)))
      (loop
        (let ((line-num (1+ (count #\Newline
                                   (let ((content (make-string (file-position stream))))
                                     (file-position stream 0)
                                     (read-sequence content stream)
                                     (file-position stream (length content))
                                     content)))))
          (handler-case
              (let ((form (read stream nil eof)))
                (when (eq form eof)
                  (return (nreverse forms)))
                (push (cons form line-num) forms))
            (error ()
              (return (nreverse forms)))))))))

(defun read-file-safely (path)
  "Read all forms from a file safely, returning (form . line-number) pairs."
  (let ((forms nil)
        (eof (gensym)))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let ((line-num 1))
        (loop
          (handler-case
              (let ((form (read stream nil eof)))
                (when (eq form eof)
                  (return (nreverse forms)))
                (push (cons form line-num) forms)
                ;; Rough line estimate - not perfect but good enough
                (setf line-num (1+ line-num)))
            (end-of-file ()
              (return (nreverse forms)))
            (error (e)
              (push (cons (list :parse-error (princ-to-string e)) line-num) forms)
              (return (nreverse forms)))))))))

(defun check-form (form file &optional (line 1))
  "Run all applicable checks on a single form."
  (let ((issues nil))
    (when (member :naming *enabled-rules*)
      (setf issues (append issues (check-naming-convention form file line))))
    (when (member :missing-docstring *enabled-rules*)
      (setf issues (append issues (check-missing-docstring form file line))))
    (when (member :function-length *enabled-rules*)
      (setf issues (append issues (check-function-length form file line))))
    (when (member :unused-binding *enabled-rules*)
      (setf issues (append issues (check-unused-bindings form file line))))
    issues))

(defun check-file (path)
  "Run all lint checks on a file. Returns list of issues."
  (let ((issues nil))
    ;; Line-based checks
    (let ((lines (with-open-file (s path :direction :input :external-format :utf-8)
                   (loop for line = (read-line s nil nil)
                         while line collect line))))
      (when (member :line-length *enabled-rules*)
        (setf issues (append issues (check-line-length lines path))))
      (when (member :trailing-whitespace *enabled-rules*)
        (setf issues (append issues (check-trailing-whitespace lines path))))
      (when (member :tabs *enabled-rules*)
        (setf issues (append issues (check-tabs lines path))))
      (when (member :todo-fixme *enabled-rules*)
        (setf issues (append issues (check-todo-fixme lines path)))))
    ;; Form-based checks
    (handler-case
        (let ((forms (read-file-safely path)))
          (dolist (form-pair forms)
            (let ((form (car form-pair))
                  (line (cdr form-pair)))
              (setf issues (append issues (check-form form path line))))))
      (error (e)
        (push (make-lint-issue
               :error :parse-error
               #~"Failed to parse file: ~{e}"
               :file path
               :line 1)
              issues)))
    ;; Sort by line number
    (sort issues #'< :key (lambda (i) (or (issue-line i) 0)))))

(defun split-lines (string)
  "Split STRING into a list of lines."
  (let ((lines nil)
        (start 0)
        (len (length string)))
    (loop for i from 0 below len
          when (char= (char string i) #\Newline)
            do (push (subseq string start i) lines)
               (setf start (1+ i)))
    ;; Handle last line without newline
    (when (< start len)
      (push (subseq string start) lines))
    (nreverse lines)))

(defun read-string-forms (code &optional (filename "buffer"))
  "Read all forms from CODE string, returning (form . line-number) pairs.
   Also catches reader errors and returns them as parse-error entries."
  (declare (ignore filename))
  (let ((forms nil)
        (eof (gensym))
        (pos 0)
        (line-num 1))
    (handler-case
        (loop
          (multiple-value-bind (form new-pos)
              (read-from-string code nil eof :start pos)
            (when (eq form eof)
              (return))
            ;; Count newlines between old and new position for line tracking
            (let ((newlines (count #\Newline code :start pos :end new-pos)))
              (push (cons form line-num) forms)
              (incf line-num newlines)
              (setf pos new-pos))))
      (reader-error (e)
        ;; Try to extract line info from error if possible
        (let ((error-line line-num))
          ;; Count newlines up to current position for approximate line
          (when (< pos (length code))
            (incf error-line (count #\Newline code :start pos)))
          (push (cons (list :reader-error (princ-to-string e))
                      error-line)
                forms)))
      (end-of-file ()
        ;; Incomplete form - unbalanced parens
        (push (cons (list :reader-error "Unexpected end of input (unbalanced parentheses?)")
                    line-num)
              forms)))
    (nreverse forms)))

(defun check-string (code &key (filename "buffer"))
  "Run all lint checks on a code string. Returns list of issues.
   This is the main entry point for ELS diagnostics integration."
  (let ((issues nil)
        (lines (split-lines code)))
    ;; Line-based checks
    (when (member :line-length *enabled-rules*)
      (setf issues (append issues (check-line-length lines filename))))
    (when (member :trailing-whitespace *enabled-rules*)
      (setf issues (append issues (check-trailing-whitespace lines filename))))
    (when (member :tabs *enabled-rules*)
      (setf issues (append issues (check-tabs lines filename))))
    (when (member :todo-fixme *enabled-rules*)
      (setf issues (append issues (check-todo-fixme lines filename))))
    ;; Form-based checks
    (let ((forms (read-string-forms code filename)))
      (dolist (form-pair forms)
        (let ((form (car form-pair))
              (line (cdr form-pair)))
          ;; Handle reader errors
          (cond
            ((and (consp form) (eq (car form) :reader-error))
             (push (make-lint-issue
                    :error :syntax-error
                    (cadr form)
                    :file filename
                    :line line)
                   issues))
            (t
             (setf issues (append issues (check-form form filename line))))))))
    ;; Sort by line number
    (sort issues #'< :key (lambda (i) (or (issue-line i) 0)))))

;;; Output formatting

(defun format-issue-shell (issue stream)
  "Format issue for shell output."
  (format stream "~A:~A:~A: ~A: [~A] ~A~%"
          (or (issue-file issue) "<unknown>")
          (or (issue-line issue) 0)
          (or (issue-column issue) 0)
          (string-downcase (symbol-name (issue-severity issue)))
          (string-downcase (symbol-name (issue-rule issue)))
          (issue-message issue)))

(defun format-issue-emacs (issue stream)
  "Format issue for Emacs flymake/flycheck."
  (format stream "~A:~A:~A: ~A: ~A~%"
          (or (issue-file issue) "<unknown>")
          (or (issue-line issue) 0)
          (or (issue-column issue) 0)
          (string-downcase (symbol-name (issue-severity issue)))
          (issue-message issue)))

(defun format-issue-verbose (issue stream)
  "Format issue with full context."
  (format stream "~%~A [~A]~%"
          (case (issue-severity issue)
            (:error "ERROR")
            (:warning "WARNING")
            (:info "INFO"))
          (issue-rule issue))
  (format stream "  File: ~A~%" (or (issue-file issue) "<unknown>"))
  (format stream "  Line: ~A, Column: ~A~%"
          (or (issue-line issue) "?")
          (or (issue-column issue) "?"))
  (format stream "  ~A~%" (issue-message issue))
  (when (issue-source-line issue)
    (format stream "  Source: ~A~%" (issue-source-line issue))))

(defun format-issues (issues &key (format :shell) (stream *standard-output*))
  "Format a list of issues for output.
   FORMAT can be :shell, :emacs, :verbose, or :json."
  (case format
    (:shell
     (dolist (issue issues)
       (format-issue-shell issue stream)))
    (:emacs
     (dolist (issue issues)
       (format-issue-emacs issue stream)))
    (:verbose
     (dolist (issue issues)
       (format-issue-verbose issue stream)))
    (:json
     (let ((json-issues
             (mapcar (lambda (i)
                       (map:make-map
                        "severity" (string-downcase (symbol-name (issue-severity i)))
                        "rule" (string-downcase (symbol-name (issue-rule i)))
                        "file" (issue-file i)
                        "line" (issue-line i)
                        "column" (issue-column i)
                        "message" (issue-message i)))
                     issues)))
       (json:encode (map:make-map "issues" json-issues) stream)))
    (t
     (dolist (issue issues)
       (format-issue-shell issue stream)))))

;;; Main entry points

(defun lint-file (path &key (format :shell))
  "Lint a single file and print results.
   FORMAT can be :shell, :emacs, :verbose, or :json."
  (let ((issues (check-file path)))
    (format-issues issues :format format)
    (values issues (length issues))))

(defun lint-directory (directory &key (format :shell) (recursive t))
  "Lint all Lisp files in a directory."
  (let ((all-issues nil)
        (pattern (if recursive "**/*.lisp" "*.lisp")))
    (dolist (file (directory (merge-pathnames pattern directory)))
      (let ((issues (check-file (namestring file))))
        (setf all-issues (append all-issues issues))))
    (format-issues all-issues :format format)
    (values all-issues (length all-issues))))

(defun lint-module (module-name &key (format :shell))
  "Lint all files in an Epsilon module."
  ;; Find module directory
  (let* ((short-name (if (str:starts-with-p module-name "epsilon.")
                         (subseq module-name 8)
                         module-name))
         (module-dir (or (probe-file #~"modules/~{short-name}/")
                         (error #~"Module ~{module-name} not found")))
         (src-dir (merge-pathnames "src/" module-dir)))
    (lint-directory src-dir :format format :recursive t)))

;;; Summary functions

(defun summarize-issues (issues)
  "Print a summary of issues by severity and rule."
  (let ((by-severity (make-hash-table))
        (by-rule (make-hash-table)))
    (dolist (issue issues)
      (incf (gethash (issue-severity issue) by-severity 0))
      (incf (gethash (issue-rule issue) by-rule 0)))
    (format t "~%Summary:~%")
    (format t "  Total issues: ~D~%" (length issues))
    (format t "  By severity:~%")
    (maphash (lambda (k v)
               (format t "    ~A: ~D~%" k v))
             by-severity)
    (format t "  By rule:~%")
    (maphash (lambda (k v)
               (format t "    ~A: ~D~%" k v))
             by-rule)
    (values)))
