(defpackage #:epsilon.lsp.tests.analysis
  (:use #:common-lisp #:epsilon.test)
  (:local-nicknames
   (#:analysis #:epsilon.lsp.analysis)))

(in-package #:epsilon.lsp.tests.analysis)

(define-test-suite analysis-tests
  "Test suite for LSP analysis implementation.")

(define-test symbol-extraction-basic
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
      (is (eq :function (analysis:symbol-info-type hello-fn))))
    
    ;; Check for variable symbol
    (let ((global-var (find "*global-var*" symbols 
                            :key #'analysis:symbol-info-name 
                            :test #'string=)))
      (is (not (null global-var)))
      (is (eq :variable (analysis:symbol-info-type global-var))))
    
    ;; Check for package symbol
    (let ((package-sym (find "#:test-package" symbols 
                             :key #'analysis:symbol-info-name 
                             :test #'string=)))
      (is (not (null package-sym)))
      (is (eq :package (analysis:symbol-info-type package-sym))))))

(define-test document-analysis-creation
  "Test document analysis creation."
  (let* ((uri "file:///test.lisp")
         (content "(defun test-fn () 'test)")
         (analysis (analysis:analyze-document uri content)))
    
    (is (analysis:document-analysis-p analysis))
    (is (string= uri (analysis:document-analysis-uri analysis)))
    (is (string= content (analysis:document-analysis-content analysis)))
    (is (listp (analysis:document-analysis-symbols analysis)))
    (is (listp (analysis:document-analysis-definitions analysis)))
    (is (listp (analysis:document-analysis-references analysis)))
    (is (listp (analysis:document-analysis-errors analysis)))))

(define-test syntax-error-detection
  "Test syntax error detection."
  (let* ((bad-code "(defun broken-fn (")  ; Missing closing paren
         (errors (analysis:check-syntax-errors bad-code)))
    (is (> (length errors) 0))))

(define-test valid-syntax-no-errors
  "Test that valid syntax produces no errors."
  (let* ((good-code "(defun valid-fn () 'valid)")
         (errors (analysis:check-syntax-errors good-code)))
    (is (= 0 (length errors)))))

(define-test position-offset-conversion
  "Test position to offset conversion utilities."
  (let ((content "line1\nline2\nline3"))
    ;; Test position to offset
    (is (= 0 (analysis:position-to-offset content 1 0)))
    (is (= 6 (analysis:position-to-offset content 2 0)))
    (is (= 12 (analysis:position-to-offset content 3 0)))
    
    ;; Test offset to position
    (is (equal '(1 . 0) (analysis:offset-to-position content 0)))
    (is (equal '(2 . 0) (analysis:offset-to-position content 6)))
    (is (equal '(3 . 0) (analysis:offset-to-position content 12)))))

(define-test symbol-position-tracking
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

(run-test-suite 'analysis-tests)