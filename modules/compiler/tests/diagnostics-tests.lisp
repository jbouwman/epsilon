;;;; diagnostics-tests.lisp - Tests for compiler diagnostics

(defpackage :epsilon.compiler.diagnostics.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:diag :epsilon.compiler.diagnostics)))

(in-package :epsilon.compiler.diagnostics.tests)

;;; Source span tests

(deftest test-make-source-span
  (let ((span (diag:make-source-span
               :file "/test/file.lisp"
               :start-line 10
               :start-column 5)))
    (is (diag:source-span-p span))
    (is (equal "/test/file.lisp" (diag:source-span-file span)))
    (is (= 10 (diag:source-span-start-line span)))
    (is (= 5 (diag:source-span-start-column span)))
    ;; End defaults to start
    (is (= 10 (diag:source-span-end-line span)))
    (is (= 5 (diag:source-span-end-column span)))))

(deftest test-source-span-with-end
  (let ((span (diag:make-source-span
               :file "/test/file.lisp"
               :start-line 10
               :start-column 5
               :end-line 12
               :end-column 20)))
    (is (= 10 (diag:source-span-start-line span)))
    (is (= 12 (diag:source-span-end-line span)))
    (is (= 20 (diag:source-span-end-column span)))))

(deftest test-source-span-contains-p
  (let ((span (diag:make-source-span
               :start-line 10
               :start-column 5
               :end-line 10
               :end-column 20)))
    (is (diag:source-span-contains-p span 10 10))
    (is (diag:source-span-contains-p span 10 5))
    (is (diag:source-span-contains-p span 10 20))
    (is (not (diag:source-span-contains-p span 10 4)))
    (is (not (diag:source-span-contains-p span 10 21)))
    (is (not (diag:source-span-contains-p span 9 10)))))

;;; Diagnostic tests

(deftest test-make-diagnostic
  (let ((d (diag:make-diagnostic
            :severity :error
            :code "E0001"
            :message "Test error message")))
    (is (diag:diagnostic-p d))
    (is (eq :error (diag:diagnostic-severity d)))
    (is (equal "E0001" (diag:diagnostic-code d)))
    (is (equal "Test error message" (diag:diagnostic-message d)))))

(deftest test-make-error-helper
  (let ((d (diag:make-error "Something went wrong"
                            :code "E0042"
                            :category :type-error)))
    (is (eq :error (diag:diagnostic-severity d)))
    (is (equal "Something went wrong" (diag:diagnostic-message d)))
    (is (equal "E0042" (diag:diagnostic-code d)))
    (is (eq :type-error (diag:diagnostic-category d)))))

(deftest test-make-warning-helper
  (let ((d (diag:make-warning "Deprecated usage")))
    (is (eq :warning (diag:diagnostic-severity d)))
    (is (equal "Deprecated usage" (diag:diagnostic-message d)))))

(deftest test-make-style-warning-helper
  (let ((d (diag:make-style-warning "Variable not used")))
    (is (eq :style-warning (diag:diagnostic-severity d)))
    (is (eq :style (diag:diagnostic-category d)))))

(deftest test-make-note-helper
  (let ((d (diag:make-note "Additional information")))
    (is (eq :note (diag:diagnostic-severity d)))))

;;; Suggested fix tests

(deftest test-make-suggested-fix
  (let ((fix (diag:make-suggested-fix
              :description "Add missing import"
              :confidence :high)))
    (is (diag:suggested-fix-p fix))
    (is (equal "Add missing import" (diag:suggested-fix-description fix)))
    (is (eq :high (diag:suggested-fix-confidence fix)))))

(deftest test-source-edit
  (let* ((span (diag:make-source-span :start-line 5 :start-column 0
                                      :end-line 5 :end-column 10))
         (edit (diag:make-source-edit
                :span span
                :replacement "(corrected)")))
    (is (diag:source-edit-p edit))
    (is (equal "(corrected)" (diag:source-edit-replacement edit)))
    (is (diag:source-span-p (diag:source-edit-span edit)))))

;;; Related diagnostic tests

(deftest test-make-related-diagnostic
  (let* ((loc (diag:make-source-span :file "other.lisp" :start-line 20))
         (rel (diag:make-related-diagnostic
               :message "First defined here"
               :location loc)))
    (is (diag:related-diagnostic-p rel))
    (is (equal "First defined here" (diag:related-diagnostic-message rel)))
    (is (diag:source-span-p (diag:related-diagnostic-location rel)))))

;;; Diagnostic emission tests

(deftest test-emit-diagnostic
  (let ((collected nil))
    (let ((diag:*diagnostic-handler*
            (lambda (d) (push d collected))))
      (diag:emit-diagnostic
       (diag:make-error "Test error"))
      (diag:emit-diagnostic
       (diag:make-warning "Test warning")))
    (is (= 2 (length collected)))
    (is (eq :warning (diag:diagnostic-severity (first collected))))
    (is (eq :error (diag:diagnostic-severity (second collected))))))

(deftest test-emit-diagnostic-no-handler
  ;; Should not error when no handler is installed
  (let ((diag:*diagnostic-handler* nil))
    (is (diag:diagnostic-p
         (diag:emit-diagnostic (diag:make-error "Test"))))))

;;; Formatting tests

(deftest test-format-diagnostic-text
  (let* ((span (diag:make-source-span
                :file "/test/file.lisp"
                :start-line 10
                :start-column 5))
         (d (diag:make-error "Undefined variable X"
                             :location span
                             :code "E0001"))
         (text (diag:format-diagnostic d :style :text :color nil)))
    (is (stringp text))
    (is (search "/test/file.lisp" text))
    (is (search "10:5" text))
    (is (search "error" text))
    (is (search "Undefined variable X" text))
    (is (search "E0001" text))))

(deftest test-format-diagnostic-json
  (let* ((d (diag:make-error "Test error" :code "E0001"))
         (json (diag:format-diagnostic d :style :json)))
    (is (stringp json))
    (is (search "\"severity\":\"error\"" json))
    (is (search "\"message\":\"Test error\"" json))
    (is (search "\"code\":\"E0001\"" json))))

;;; Color tests

(deftest test-severity-color-codes
  ;; Just verify they return strings with escape codes
  (is (stringp (diag:severity-color-code :error)))
  (is (stringp (diag:severity-color-code :warning)))
  (is (stringp (diag:severity-color-code :note)))
  (is (search (string #\Escape) (diag:severity-color-code :error))))
