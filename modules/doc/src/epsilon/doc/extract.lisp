;;;; epsilon.doc.extract -- Symbol and module documentation extraction
;;;;
;;;; Extracts structured documentation from loaded symbols and modules,
;;;; integrating docstring parsing, annotation metadata, and source locations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-introspect"))

(defpackage epsilon.doc.extract
  (:use :cl)
  (:import (epsilon.doc.parse parse)
           (epsilon.annotate ann)
           (epsilon.loader loader))
  (:export
   #:describe-symbol
   #:describe-module
   #:describe-package))

(defun describe-symbol (symbol-name &key package)
  "Extract structured documentation for SYMBOL-NAME.
   Returns a plist with :name, :package, :type, :lambda-list, :summary,
   :params, :returns, :signals, :see, :examples, :since, :source, etc."
  (let* ((pkg (if package (find-package package) *package*))
         (sym (find-symbol (string symbol-name) pkg)))
    (unless sym (return-from describe-symbol nil))
    (let* ((type (cond
                   ((fboundp sym)
                    (if (macro-function sym) :macro :function))
                   ((boundp sym)
                    (if (constantp sym) :constant :variable))
                   (t :unknown)))
           (docstring (or (documentation sym 'function)
                          (documentation sym 'variable)
                          (documentation sym 'type)))
           (parsed (when docstring (parse:parse-docstring docstring)))
           (annotations (ann:annotations sym))
           (merged (if annotations
                     (parse:merge-annotations (or parsed (list :summary "")) annotations)
                     (or parsed (list :summary ""))))
           (lambda-list (when (fboundp sym)
                          (ignore-errors
                            (sb-kernel:%fun-lambda-list
                             (or (macro-function sym)
                                 (symbol-function sym)))))))
      (let ((result (list :name (symbol-name sym)
                          :package (package-name (symbol-package sym))
                          :type type)))
        (when lambda-list
          (setf (getf result :lambda-list) lambda-list))
        ;; Source location
        (let ((source (symbol-source-location sym type)))
          (when source
            (setf (getf result :source) source)))
        ;; Merge parsed doc fields
        (loop for (key val) on merged by #'cddr
              do (unless (getf result key)
                   (setf (getf result key) val)))
        result))))

;;; Source location extraction

(defun symbol-source-location (sym type)
  "Extract source location for SYM of TYPE (:function, :macro, :variable, :constant).
   Returns a plist (:file ... :line ...) or NIL."
  (handler-case
      (let* ((sb-type (case type
                        (:function :function)
                        (:macro :macro)
                        (:variable :variable)
                        (:constant :variable)
                        (otherwise :function)))
             (sources (sb-introspect:find-definition-sources-by-name sym sb-type)))
        (when sources
          (let* ((source (first sources))
                 (pathname (sb-introspect:definition-source-pathname source))
                 (offset (sb-introspect:definition-source-character-offset source)))
            (when pathname
              (let* ((filepath (namestring pathname))
                     (line-num (when offset
                                 (offset-to-line filepath offset)))
                     (result (list :file filepath)))
                (when line-num
                  (setf (getf result :line) line-num))
                result)))))
    (error () nil)))

(defun offset-to-line (file offset)
  "Convert character OFFSET to a 1-based line number in FILE, or NIL on error."
  (handler-case
      (with-open-file (s file :direction :input)
        (let ((line 1)
              (pos 0))
          (loop while (< pos offset)
                for ch = (read-char s nil nil)
                while ch
                do (incf pos)
                   (when (char= ch #\Newline)
                     (incf line)))
          line))
    (error () nil)))

(defun describe-package (package-designator)
  "Extract documentation for all exported symbols in PACKAGE-DESIGNATOR.
   Returns a list of describe-symbol plists."
  (let* ((pkg (find-package package-designator))
         (symbols nil))
    (when pkg
      (do-external-symbols (sym pkg)
        (let ((doc (describe-symbol (symbol-name sym)
                     :package (package-name pkg))))
          (when doc (push doc symbols)))))
    (sort symbols #'string< :key (lambda (p) (getf p :name)))))

(defun describe-module (module-name)
  "Extract structured documentation for MODULE-NAME.
   Returns a plist with :name, :description, :packages, :symbols.

   Uses the epsilon.loader environment to retrieve module metadata."
  (let* ((mod (loader:get-module module-name))
         (metadata (when mod (loader:module-metadata mod)))
         (description (or (getf metadata :description) ""))
         (doc-text (getf metadata :documentation))
         (module-prefix (string-upcase module-name))
         ;; Collect all symbols from packages that match this module prefix
         (all-symbols nil)
         (packages nil))
    ;; Find packages belonging to this module by name prefix
    (dolist (pkg (list-all-packages))
      (let ((pkg-name (package-name pkg)))
        (when (or (string= pkg-name module-prefix)
                  (and (> (length pkg-name) (length module-prefix))
                       (string= pkg-name module-prefix
                                :end1 (length module-prefix))
                       (char= (char pkg-name (length module-prefix)) #\.)))
          (push pkg-name packages)
          (do-external-symbols (sym pkg)
            (let ((doc (describe-symbol (symbol-name sym)
                         :package pkg-name)))
              (when doc (push doc all-symbols)))))))
    (setf packages (sort packages #'string<))
    (setf all-symbols (sort all-symbols #'string<
                            :key (lambda (p) (getf p :name))))
    (let ((result (list :name module-name
                        :description description)))
      (when doc-text
        (setf (getf result :documentation) doc-text))
      (when packages
        (setf (getf result :packages) packages))
      (when all-symbols
        (setf (getf result :symbols) all-symbols))
      result)))
