;;;; diagnostics-tests.lisp - Tests for compiler diagnostics (using core types)

(defpackage :epsilon.compiler.diagnostics.tests
  (:use :cl :epsilon.test)
  (:import
   (:epsilon.diagnostic :diag)
   (:epsilon.source-location :loc)))

;;; Source location tests (canonical type from core)

(deftest test-make-source-location-for-compiler
  (let ((sl (loc:make-source-location
             :file "/test/file.lisp"
             :line 10
             :column 5)))
    (assert-true (loc:source-location-p sl))
    (assert-true (equal "/test/file.lisp" (loc:source-location-file sl)))
    (assert-true (= 10 (loc:source-location-line sl)))
    (assert-true (= 5 (loc:source-location-column sl)))))

(deftest test-source-location-with-end
  (let ((sl (loc:make-source-location
             :file "/test/file.lisp"
             :line 10
             :column 5
             :end-line 12
             :end-column 20)))
    (assert-true (= 10 (loc:source-location-line sl)))
    (assert-true (= 12 (loc:source-location-end-line sl)))
    (assert-true (= 20 (loc:source-location-end-column sl)))))

(deftest test-source-location-contains-p
  (let ((sl (loc:make-source-location
             :line 10
             :column 5
             :end-line 10
             :end-column 20)))
    (assert-true (loc:source-location-contains-p sl 10 10))
    (assert-true (loc:source-location-contains-p sl 10 5))
    (assert-true (loc:source-location-contains-p sl 10 20))
    (assert-true (not (loc:source-location-contains-p sl 10 4)))
    (assert-true (not (loc:source-location-contains-p sl 10 21)))
    (assert-true (not (loc:source-location-contains-p sl 9 10)))))

;;; Diagnostic tests (canonical type from core)

(deftest test-make-diagnostic
  (let ((d (diag:make-diagnostic
            :severity :error
            :code "E0001"
            :message "Test error message")))
    (assert-true (diag:diagnostic-p d))
    (assert-true (eq :error (diag:diagnostic-severity d)))
    (assert-true (equal "E0001" (diag:diagnostic-code d)))
    (assert-true (equal "Test error message" (diag:diagnostic-message d)))))

(deftest test-make-error-helper
  (let ((d (diag:make-error "Something went wrong"
                            :code "E0042"
                            :category :type-error)))
    (assert-true (eq :error (diag:diagnostic-severity d)))
    (assert-true (equal "Something went wrong" (diag:diagnostic-message d)))
    (assert-true (equal "E0042" (diag:diagnostic-code d)))
    (assert-true (eq :type-error (diag:diagnostic-category d)))))

(deftest test-make-warning-helper
  (let ((d (diag:make-warning "Deprecated usage")))
    (assert-true (eq :warning (diag:diagnostic-severity d)))
    (assert-true (equal "Deprecated usage" (diag:diagnostic-message d)))))

(deftest test-make-style-warning-helper
  (let ((d (diag:make-style-warning "Variable not used")))
    (assert-true (eq :style-warning (diag:diagnostic-severity d)))
    (assert-true (eq :style (diag:diagnostic-category d)))))

(deftest test-make-note-helper
  (let ((d (diag:make-note "Additional information")))
    (assert-true (eq :note (diag:diagnostic-severity d)))))

;;; Suggested fix tests

(deftest test-make-suggested-fix
  (let ((fix (diag:make-suggested-fix
              :description "Add missing import"
              :confidence :high)))
    (assert-true (diag:suggested-fix-p fix))
    (assert-true (equal "Add missing import" (diag:suggested-fix-description fix)))
    (assert-true (eq :high (diag:suggested-fix-confidence fix)))))

(deftest test-source-edit
  (let* ((sl (loc:make-source-location :line 5 :column 0
                                       :end-line 5 :end-column 10))
         (edit (diag:make-source-edit
                :location sl
                :replacement "(corrected)")))
    (assert-true (diag:source-edit-p edit))
    (assert-true (equal "(corrected)" (diag:source-edit-replacement edit)))
    (assert-true (loc:source-location-p (diag:source-edit-location edit)))))

;;; Related diagnostic tests

(deftest test-make-related-diagnostic
  (let* ((sl (loc:make-source-location :file "other.lisp" :line 20))
         (rel (diag:make-related-diagnostic
               :message "First defined here"
               :location sl)))
    (assert-true (diag:related-diagnostic-p rel))
    (assert-true (equal "First defined here" (diag:related-diagnostic-message rel)))
    (assert-true (loc:source-location-p (diag:related-diagnostic-location rel)))))

;;; Diagnostic emission tests

(deftest test-emit-diagnostic
  (let ((collected nil))
    (let ((diag:*diagnostic-handler*
            (lambda (d) (push d collected))))
      (diag:emit-diagnostic
       (diag:make-error "Test error"))
      (diag:emit-diagnostic
       (diag:make-warning "Test warning")))
    (assert-true (= 2 (length collected)))
    (assert-true (eq :warning (diag:diagnostic-severity (first collected))))
    (assert-true (eq :error (diag:diagnostic-severity (second collected))))))

(deftest test-emit-diagnostic-no-handler
  ;; Should not error when no handler is installed
  (let ((diag:*diagnostic-handler* nil))
    (assert-true (diag:diagnostic-p
         (diag:emit-diagnostic (diag:make-error "Test"))))))

;;; Formatting tests

(deftest test-format-diagnostic-text
  (let* ((sl (loc:make-source-location
              :file "/test/file.lisp"
              :line 10
              :column 5))
         (d (diag:make-error "Undefined variable X"
                             :location sl
                             :code "E0001"))
         (text (diag:format-diagnostic d :style :text)))
    (assert-true (stringp text))
    (assert-true (search "/test/file.lisp" text))
    (assert-true (search "10:5" text))
    (assert-true (search "error" text))
    (assert-true (search "Undefined variable X" text))
    (assert-true (search "E0001" text))))

(deftest test-format-diagnostic-json
  (let* ((d (diag:make-error "Test error" :code "E0001"))
         (json (diag:format-diagnostic d :style :json)))
    (assert-true (stringp json))
    (assert-true (search "\"severity\":\"error\"" json))
    (assert-true (search "\"message\":\"Test error\"" json))
    (assert-true (search "\"code\":\"E0001\"" json))))
