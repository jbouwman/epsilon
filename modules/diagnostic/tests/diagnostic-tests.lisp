(cl:defpackage epsilon.diagnostic-tests
  (:use :cl :epsilon.test :epsilon.diagnostic)
  (:local-nicknames (loc :epsilon.source-location)))

(cl:in-package :epsilon.diagnostic-tests)

(deftest test-make-error
  "make-error builds a diagnostic with :error severity"
  (let ((d (make-error "boom")))
    (assert-true (diagnostic-p d))
    (assert-eq :error (diagnostic-severity d))
    (assert-equal "boom" (diagnostic-message d))))

(deftest test-make-warning-with-code-and-category
  "make-warning preserves code and category"
  (let ((d (make-warning "fishy" :code "W042" :category :style)))
    (assert-eq :warning (diagnostic-severity d))
    (assert-equal "W042" (diagnostic-code d))
    (assert-eq :style (diagnostic-category d))))

(deftest test-make-style-warning-stamps-style-category
  "make-style-warning forces :category to :style"
  (let ((d (make-style-warning "stylish")))
    (assert-eq :style-warning (diagnostic-severity d))
    (assert-eq :style (diagnostic-category d))))

(deftest test-make-diagnostic-defaults
  "Default constructor: empty message string, error severity, nil code"
  (let ((d (make-diagnostic)))
    (assert-eq :error (diagnostic-severity d))
    (assert-equal "" (diagnostic-message d))
    (assert-true (null (diagnostic-code d)))
    (assert-true (null (diagnostic-fixes d)))
    (assert-true (null (diagnostic-related d)))))

(deftest test-make-suggested-fix
  "Suggested fix carries description, edits, confidence"
  (let* ((edit (make-source-edit :replacement "new"))
         (fix (make-suggested-fix :description "Replace X with new"
                                  :edits (list edit)
                                  :confidence :medium)))
    (assert-true (suggested-fix-p fix))
    (assert-equal "Replace X with new" (suggested-fix-description fix))
    (assert-eq :medium (suggested-fix-confidence fix))
    (assert-equal "new" (source-edit-replacement (first (suggested-fix-edits fix))))))

(deftest test-suggested-fix-defaults
  "Confidence defaults to :high; description defaults to empty string"
  (let ((fix (make-suggested-fix)))
    (assert-eq :high (suggested-fix-confidence fix))
    (assert-equal "" (suggested-fix-description fix))))

(deftest test-related-diagnostic
  "Related-diagnostic carries message and location"
  (let ((rel (make-related-diagnostic :message "see also")))
    (assert-true (related-diagnostic-p rel))
    (assert-equal "see also" (related-diagnostic-message rel))
    (assert-true (null (related-diagnostic-location rel)))))

(deftest test-emit-diagnostic-without-handler
  "Emit returns the diagnostic; with no handler bound, no error"
  (let ((d (make-error "x")))
    (assert-eq d (emit-diagnostic d))))

(deftest test-emit-diagnostic-runs-handler
  "Bound *diagnostic-handler* sees the diagnostic"
  (let* ((seen nil)
         (*diagnostic-handler* (lambda (d) (push d seen))))
    (let ((d (make-warning "captured")))
      (emit-diagnostic d)
      (assert-eq d (first seen))
      (assert-equal 1 (length seen)))))

(deftest test-format-diagnostic-text-without-location
  "Text formatter prints severity and message"
  (let ((out (format-diagnostic (make-error "kaboom") :style :text)))
    (assert-true (search "error:" out))
    (assert-true (search "kaboom" out))))

(deftest test-format-diagnostic-text-includes-code
  "Text formatter inserts [CODE] when present"
  (let ((out (format-diagnostic (make-error "bad" :code "E001"))))
    (assert-true (search "[E001]" out))))

(deftest test-format-diagnostic-json-shape
  "JSON formatter emits severity and message keys"
  (let ((out (format-diagnostic (make-warning "json-msg") :style :json)))
    (assert-true (search "\"severity\":\"warning\"" out))
    (assert-true (search "\"message\":\"json-msg\"" out))))

(deftest test-format-diagnostic-json-escapes-quotes
  "JSON formatter backslash-escapes embedded double-quotes"
  (let ((out (format-diagnostic (make-error "say \"hi\"") :style :json)))
    (assert-true (search "say \\\"hi\\\"" out))))

(deftest test-format-diagnostic-with-source-location
  "Text formatter prepends file:line:col when location present"
  (let* ((sl (loc:make-source-location :file "x.lisp" :line 7 :column 3))
         (d (make-error "err" :location sl))
         (out (format-diagnostic d)))
    (assert-true (search "x.lisp" out))
    (assert-true (search "7" out))))
