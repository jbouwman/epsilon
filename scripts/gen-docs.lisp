;;; Documentation Generator for Epsilon
;;; Extracts API documentation from packages and source files

(require :sb-introspect)

(defpackage #:epsilon.tool.gendoc
  (:use #:common-lisp)
  (:export #:generate-package-docs
           #:generate-all-docs))

(in-package #:epsilon.tool.gendoc)

;;; Utilities

(defun package-exports (package-name)
  "Get all exported symbols from a package."
  (let ((package (find-package package-name))
        (exports nil))
    (when package
      (do-external-symbols (symbol package)
        (push symbol exports)))
    (sort exports #'string< :key #'symbol-name)))

(defun symbol-documentation-safe (symbol type)
  "Safely get documentation for a symbol."
  (handler-case
      (documentation symbol type)
    (error () nil)))

(defun symbol-type (symbol)
  "Determine the type of a symbol (function, macro, variable, etc.)."
  (cond
    ((fboundp symbol)
     (cond
       ((macro-function symbol) :macro)
       ((special-operator-p symbol) :special-operator)
       (t :function)))
    ((boundp symbol) :variable)
    ((find-class symbol nil) :class)
    (t :unknown)))

(defun format-symbol-signature (symbol)
  "Format a function/macro signature if available."
  (when (fboundp symbol)
    (handler-case
        (let ((lambda-list (sb-introspect:function-lambda-list symbol)))
          (if lambda-list
              (format nil "(~A~{ ~A~})" (symbol-name symbol) lambda-list)
              (format nil "(~A ...)" (symbol-name symbol))))
      (error ()
        (format nil "(~A ...)" (symbol-name symbol))))))

(defun format-markdown-symbol (symbol)
  "Format a symbol as markdown documentation."
  (let ((type (symbol-type symbol))
        (doc (symbol-documentation-safe symbol 
                                        (case (symbol-type symbol)
                                          (:function 'function)
                                          (:macro 'function)
                                          (:variable 'variable)
                                          (t 'function))))
        (signature (format-symbol-signature symbol)))
    
    (format nil "### ~A~%~%~A~%~%~@[**Signature**: `~A`~%~%~]~@[~A~%~%~]---~%~%"
            (symbol-name symbol)
            (case type
              (:function "**Type**: Function")
              (:macro "**Type**: Macro") 
              (:variable "**Type**: Variable")
              (:class "**Type**: Class")
              (t "**Type**: Symbol"))
            signature
            doc)))

(defun parse-source-comments (file-path)
  "Parse source file for documentation comments."
  (when (probe-file file-path)
    (with-open-file (stream file-path :direction :input)
      (let ((comments nil)
            (current-comment nil))
        (loop for line = (read-line stream nil nil)
              while line
              do (cond
                   ;; Multi-line comment block
                   ((search ";;;" line)
                    (if current-comment
                        (push (string-trim " " (subseq line (+ 3 (search ";;;" line)))) current-comment)
                        (setf current-comment (list (string-trim " " (subseq line (+ 3 (search ";;;" line))))))))
                   ;; End comment block  
                   ((and current-comment (not (search ";;" line)))
                    (push (reverse current-comment) comments)
                    (setf current-comment nil))))
        comments))))

(defun generate-package-docs (package-name &key output-file source-file)
  "Generate markdown documentation for a package."
  (let ((package (find-package package-name)))
    (unless package
      (error "Package ~A not found" package-name))
    
    (let ((exports (package-exports package-name))
          (package-doc (documentation package t))
          (source-comments (when source-file 
                            (parse-source-comments source-file))))
      
      (with-output-to-string (out)
        ;; Package header
        (format out "# ~A~%~%" (package-name package))
        
        ;; Package documentation
        (when package-doc
          (format out "~A~%~%" package-doc))
        
        ;; Source comments
        (when source-comments
          (format out "## Overview~%~%")
          (dolist (comment-block source-comments)
            (format out "~{~A~%~}~%" comment-block)))
        
        ;; Exported symbols
        (when exports
          (format out "## API Reference~%~%")
          (dolist (symbol exports)
            (format out "~A" (format-markdown-symbol symbol))))
        
        ;; Save to file if specified
        (when output-file
          (with-open-file (file output-file :direction :output 
                                :if-exists :supersede)
            (write-string (get-output-stream-string out) file)))))))

(defun find-source-file (package-name)
  "Try to find the source file for a package."
  (let* ((package-string (string-downcase (string package-name)))
         (lib-pos (search "lib." package-string))
         (possible-paths nil))
    
    ;; Handle epsilon.lib.* packages
    (when lib-pos
      (let ((module-name (subseq package-string (+ lib-pos 4))))
        (push (format nil "src/core/src/lib/~A.lisp" module-name) possible-paths)))
    
    ;; Handle other epsilon packages
    (let ((dot-pos (position #\. package-string)))
      (when dot-pos
        (let* ((prefix (subseq package-string 0 dot-pos))
               (suffix (subseq package-string (1+ dot-pos))))
          (push (format nil "src/~A/src/~A.lisp" suffix prefix) possible-paths)
          (push (format nil "src/~A/src/~A.lisp" prefix suffix) possible-paths))))
    
    ;; Fallback paths
    (push (format nil "src/~A.lisp" (substitute #\/ #\. package-string)) possible-paths)
    
    (find-if #'probe-file possible-paths)))

;;; Simple test function
(defun test-map-docs ()
  "Generate documentation for epsilon.lib.map as a test."
  (let ((source-file "src/core/src/lib/map.lisp"))
    (generate-package-docs 'epsilon.lib.map 
                          :source-file source-file)))