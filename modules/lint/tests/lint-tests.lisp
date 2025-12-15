;;;; Tests for epsilon.lint module

(defpackage epsilon.lint.tests
  (:use cl epsilon.test epsilon.lint))

(in-package epsilon.lint.tests)

(deftest test-check-line-length
  "Test line length checking"
  (let ((epsilon.lint:*max-line-length* 80))
    ;; Short line should pass
    (let ((issues (epsilon.lint::check-line-length '("short line") "test.lisp")))
      (is-= (length issues) 0 "Short line should not create issues"))
    ;; Long line should fail
    (let* ((long-line (make-string 100 :initial-element #\x))
           (issues (epsilon.lint::check-line-length (list long-line) "test.lisp")))
      (is-= (length issues) 1 "Long line should create one issue")
      (is-eq (issue-severity (first issues)) :warning)
      (is-eq (issue-rule (first issues)) :line-length))))

(deftest test-check-trailing-whitespace
  "Test trailing whitespace detection"
  (let* ((lines (list "no trailing"
                      "has trailing "  ; ends with space
                      (format nil "also trailing~C" #\Tab)))  ; ends with tab
         (issues (epsilon.lint::check-trailing-whitespace lines "test.lisp")))
    (is-= (length issues) 2 "Should detect two trailing whitespace issues")))

(deftest test-check-tabs
  "Test tab detection"
  (let* ((lines (list "no tabs"
                      (format nil "~Ctab at start" #\Tab)
                      (format nil "tab~Cin middle" #\Tab)))
         (issues (epsilon.lint::check-tabs lines "test.lisp")))
    (is-= (length issues) 2 "Should detect two tab issues")))

(deftest test-check-todo-fixme
  "Test TODO/FIXME detection"
  (let* ((lines '("normal code"
                  ";; TODO: implement this"
                  ";; FIXME: broken"))
         (issues (epsilon.lint::check-todo-fixme lines "test.lisp")))
    (is-= (length issues) 2 "Should detect TODO and FIXME")
    ;; TODO should be info level
    (is-eq (issue-severity (first issues)) :info)
    ;; FIXME should be warning level
    (is-eq (issue-severity (second issues)) :warning)))

(deftest test-make-lint-issue
  "Test issue creation"
  (let ((issue (epsilon.lint::make-lint-issue :warning :test-rule "Test message"
                                              :file "test.lisp"
                                              :line 10
                                              :column 5)))
    (is-eq (issue-severity issue) :warning)
    (is-eq (issue-rule issue) :test-rule)
    (is-equal (issue-file issue) "test.lisp")
    (is-= (issue-line issue) 10)
    (is-= (issue-column issue) 5)
    (is-equal (issue-message issue) "Test message")))

(deftest test-format-issue-shell
  "Test shell format output"
  (let ((issue (epsilon.lint::make-lint-issue :warning :test-rule "Test message"
                                              :file "test.lisp"
                                              :line 10
                                              :column 5)))
    (let ((output (with-output-to-string (s)
                    (epsilon.lint::format-issue-shell issue s))))
      (is (search "test.lisp" output) "Should contain filename")
      (is (search "10" output) "Should contain line number")
      (is (search "warning" output) "Should contain severity"))))

(deftest test-format-issue-emacs
  "Test Emacs format output"
  (let ((issue (epsilon.lint::make-lint-issue :error :parse-error "Syntax error"
                                              :file "bad.lisp"
                                              :line 1
                                              :column 1)))
    (let ((output (with-output-to-string (s)
                    (epsilon.lint::format-issue-emacs issue s))))
      (is (search "bad.lisp:1:1:" output) "Should have proper format")
      (is (search "error:" output) "Should contain error level"))))

(deftest test-enabled-rules
  "Test that enabled rules filter works"
  (let ((epsilon.lint:*enabled-rules* '(:line-length)))
    ;; Only line-length should be checked
    (is (member :line-length epsilon.lint:*enabled-rules*))
    (is-not (member :tabs epsilon.lint:*enabled-rules*))))
