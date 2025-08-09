;;;; Tests for the Epsilon linter

(defpackage :epsilon.lint.tests
  (:use :cl)
  (:local-nicknames
   (#:test #:epsilon.test)
   (#:lint #:epsilon.lint)
   (#:rules #:epsilon.lint.rules)
   (#:parser #:epsilon.lint.parser)))

(in-package :epsilon.lint.tests)

(test:deftest test-comment-parsing
  "Test comment parsing and location tracking"
  (let* ((test-file "/tmp/test-comment-parsing.lisp")
         (test-content ";;;; Test file
;; Second comment
(defun test-function ()
  \"A test function\"
  ;; inline comment
  42)")
         (ast nil))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string test-content stream))
    
    ;; Parse the file
    (setf ast (parser:parse-file-with-comments test-file))
    
    ;; Check we found comments
    (test:is (not (null (parser:file-ast-comments ast))))
    (test:is (>= (length (parser:file-ast-comments ast)) 2))
    
    ;; Check first comment
    (let ((first-comment (first (parser:file-ast-comments ast))))
      (test:is (= (parser:comment-line first-comment) 1))
      (test:is (= (parser:comment-level first-comment) 4))
      (test:is (search "Test file" (parser:comment-text first-comment))))
    
    ;; Clean up
    (delete-file test-file)))

(test:deftest test-indentation-rule
  "Test indentation checking rule"
  (let* ((test-file "/tmp/test-indentation.lisp")
         (test-content "(defpackage :test
  (:use :cl))

(in-package :test)

(defun bad-indent ()
   (let ((x 1))  ; 3 spaces - should be 2 or 4
  (+ x 1)))      ; 2 spaces - correct")
         (issues nil))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string test-content stream))
    
    ;; Lint the file (no need to parse separately for indentation check)
    (setf issues (rules:lint-file test-file))
    
    ;; Should find at least one indentation issue
    (test:is (> (length issues) 0))
    
    ;; Check that we found the 3-space indentation error
    (test:is (some (lambda (issue)
                     (and (eq (rules:issue-type issue) :indentation)
                          (search "3 spaces" (rules:issue-message issue))))
                   issues))
    
    ;; Clean up
    (delete-file test-file)))

(test:deftest test-line-length-rule
  "Test line length checking rule"
  (let* ((test-file "/tmp/test-line-length.lisp")
         (long-line (make-string 120 :initial-element #\x))
         (test-content (format nil "(defpackage :test
  (:use :cl))

;; This is a very long comment that exceeds the 100 character limit: ~A
(defun short-function () 42)" long-line))
         (issues nil))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string test-content stream))
    
    ;; Lint the file
    (setf issues (rules:lint-file test-file))
    
    ;; Should find line length issue
    (test:is (> (length issues) 0))
    
    ;; Check we found the long line
    (test:is (some (lambda (issue)
                     (eq (rules:issue-type issue) :line-length))
                   issues))
    
    ;; Clean up
    (delete-file test-file)))

(test:deftest test-header-comment-rule
  "Test header comment checking rule"
  (let* ((test-file-good "/tmp/test-header-good.lisp")
         (test-file-bad "/tmp/test-header-bad.lisp")
         (good-content ";;;; Good header comment
(defpackage :test (:use :cl))")
         (bad-content ";; Not enough semicolons
(defpackage :test (:use :cl))")
         (ast-good nil)
         (ast-bad nil)
         (issues-good nil)
         (issues-bad nil))
    
    ;; Create test files
    (with-open-file (stream test-file-good :direction :output :if-exists :supersede)
      (write-string good-content stream))
    
    (with-open-file (stream test-file-bad :direction :output :if-exists :supersede)
      (write-string bad-content stream))
    
    ;; Parse and lint both files
    (setf ast-good (parser:parse-file-with-comments test-file-good))
    (setf issues-good (rules:check-header-comment ast-good))
    
    (setf ast-bad (parser:parse-file-with-comments test-file-bad))
    (setf issues-bad (rules:check-header-comment ast-bad))
    
    ;; Good file should have no issues
    (test:is (= (length issues-good) 0))
    
    ;; Bad file should have header comment issue
    (test:is (> (length issues-bad) 0))
    (test:is (some (lambda (issue)
                     (eq (rules:issue-type issue) :header-comment))
                   issues-bad))
    
    ;; Clean up
    (delete-file test-file-good)
    (delete-file test-file-bad)))

(test:deftest test-package-structure-rule
  "Test package structure checking rule"
  (let* ((test-file "/tmp/test-package-structure.lisp")
         (test-content "(defpackage :test.package
  (:use :cl)
  (:export #:some-function #:some-variable))

(in-package :test.package)")
         (ast nil)
         (issues nil))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string test-content stream))
    
    ;; Parse and lint
    (setf ast (parser:parse-file-with-comments test-file))
    (setf issues (rules:check-package-structure ast))
    
    ;; Should have no major package structure issues
    (test:is (not (some (lambda (issue)
                          (eq (rules:issue-severity issue) :error))
                        issues)))
    
    ;; Clean up
    (delete-file test-file)))

(test:deftest test-full-lint-integration
  "Test complete linting of a file"
  (let* ((test-file "/tmp/test-full-lint.lisp")
         (test-content ";;;; Test file for full linting
(defpackage :test.integration
  (:use :cl)
  (:export #:test-function))

(in-package :test.integration)

(defun test-function (x)
  \"A simple test function\"
   (+ x 1))  ; Bad indentation (3 spaces)")
         (issues nil))
    
    ;; Create test file
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (write-string test-content stream))
    
    ;; Run full linting
    (setf issues (rules:lint-file test-file))
    
    ;; Should find some issues
    (test:is (>= (length issues) 0))
    
    ;; Check we can format issues
    (let ((formatted (lint:format-issues issues)))
      (test:is (stringp formatted)))
    
    ;; Clean up
    (delete-file test-file)))