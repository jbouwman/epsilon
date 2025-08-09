;;;; Tests for LSP Analysis Implementation
;;;;
;;;; Tests for the epsilon.lsp.analysis module

(defpackage :epsilon.lsp.tests.analysis
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:analysis #:epsilon.lsp.analysis)))

(in-package :epsilon.lsp.tests.analysis)

(deftest symbol-extraction-basic
  "Test basic symbol extraction from Lisp code."
  (let* ((code "(defun hello-world ()
  (format t \"Hello, World!\"))

(defvar *global-var* 42)

(defpackage #:test-package
  (:use #:common-lisp))")
         (symbols (analysis:extract-symbols code)))
    
    (is (> (length symbols) 0))
    
    ;; Check for function symbol
    (let ((hello-fn (find "hello-world" symbols 
                          :key #'analysis:symbol-info-name 
                          :test #'string=)))
      (is (not (null hello-fn)))
      (is-equal :function (analysis:symbol-info-type hello-fn)))
    
    ;; Check for variable symbol
    (let ((global-var (find "*global-var*" symbols 
                            :key #'analysis:symbol-info-name 
                            :test #'string=)))
      (is (not (null global-var)))
      (is-equal :variable (analysis:symbol-info-type global-var)))
    
    ;; Check for package symbol
    (let ((package-sym (find "#:test-package" symbols 
                             :key #'analysis:symbol-info-name 
                             :test #'string=)))
      (is (not (null package-sym)))
      (is-equal :package (analysis:symbol-info-type package-sym)))))

(deftest document-analysis-creation
  "Test document analysis creation."
  (let* ((uri "file:///test.lisp")
         (content "(defun test-fn () 'test)")
         (analysis-result (analysis:analyze-document uri content)))
    
    (is (analysis:document-analysis-p analysis-result))
    (is-equal uri (analysis:document-analysis-uri analysis-result))
    (is-equal content (analysis:document-analysis-content analysis-result))
    (is (listp (analysis:document-analysis-symbols analysis-result)))
    (is (listp (analysis:document-analysis-definitions analysis-result)))
    (is (listp (analysis:document-analysis-references analysis-result)))
    (is (listp (analysis:document-analysis-errors analysis-result)))))

(deftest syntax-error-detection
  "Test syntax error detection."
  (let* ((bad-code "(defun broken-fn (")  ; Missing closing paren
         (errors (analysis:check-syntax-errors bad-code)))
    (is (> (length errors) 0))))

(deftest valid-syntax-no-errors
  "Test that valid syntax produces no errors."
  (let* ((good-code "(defun valid-fn () 'valid)")
         (errors (analysis:check-syntax-errors good-code)))
    (is-equal 0 (length errors))))

(deftest position-offset-conversion
  "Test position to offset conversion utilities."
  (let ((content "line1
line2
line3"))
    ;; Test position to offset
    (is-equal 0 (analysis:position-to-offset content 1 0))
    (is-equal 6 (analysis:position-to-offset content 2 0))
    (is-equal 12 (analysis:position-to-offset content 3 0))
    
    ;; Test offset to position
    (is-equal '(1 . 0) (analysis:offset-to-position content 0))
    (is-equal '(2 . 0) (analysis:offset-to-position content 6))
    (is-equal '(3 . 0) (analysis:offset-to-position content 12))))

(deftest symbol-position-tracking
  "Test that symbols have correct position information."
  (let* ((code "(defun test-fn ()\n  'result)")
         (symbols (analysis:extract-symbols code)))
    
    (when symbols
      (let ((symbol (first symbols)))
        (is (not (null (analysis:symbol-info-position symbol))))
        (is (not (null (analysis:symbol-info-range symbol))))
        (let ((pos (analysis:symbol-info-position symbol)))
          (is (numberp (car pos)))
          (is (numberp (cdr pos))))))))