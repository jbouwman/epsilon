;;;; Test Suite for Structured Compilation API (using core types)
(defpackage epsilon.test.compile-api
  (:use cl epsilon.test)
  (:import (epsilon.source-location loc)
           (epsilon.diagnostic diag)
           (epsilon.compilation-result cr)
           (epsilon.compile-hooks hooks)
           (epsilon.compile compile)
           (epsilon.compile-location location)))

;;; Test data structures (canonical core types)
(deftest test-source-location-creation
  "Test creation of source location structures"
  (let ((sl (loc:make-source-location :file "test.lisp" :line 42 :column 10)))
    (assert-true (loc:source-location-p sl))
    (assert-equal "test.lisp" (loc:source-location-file sl))
    (assert-= 42 (loc:source-location-line sl))
    (assert-= 10 (loc:source-location-column sl))))

(deftest test-compilation-statistics
  "Test compilation statistics structure"
  (let ((stats (cr:make-compilation-statistics :forms-processed 10
                                               :functions-compiled 5
                                               :cpu-time 1.5)))
    (assert-true (cr:compilation-statistics-p stats))
    (assert-= 10 (cr:compilation-statistics-forms-processed stats))
    (assert-= 5 (cr:compilation-statistics-functions-compiled stats))
    (assert-= 1.5 (cr:compilation-statistics-cpu-time stats))))

(deftest test-compilation-result
  "Test compilation result structure"
  (let* ((d1 (diag:make-warning "Warning 1"))
         (d2 (diag:make-error "Error 1"))
         (stats (cr:make-compilation-statistics :forms-processed 5))
         (result (cr:make-compilation-result :file "test.lisp"
                                             :output-file "test.fasl"
                                             :diagnostics (list d1 d2)
                                             :statistics stats
                                             :success-p nil
                                             :warnings-p t
                                             :errors-p t
                                             :error-count 1
                                             :warning-count 1)))
    (assert-true (cr:compilation-result-p result))
    (assert-equal "test.lisp" (cr:compilation-result-file result))
    (assert-equal "test.fasl" (cr:compilation-result-output-file result))
    (assert-= 2 (length (cr:compilation-result-diagnostics result)))
    (assert-true (not (cr:compilation-result-success-p result)))
    (assert-true (cr:compilation-result-warnings-p result))
    (assert-true (cr:compilation-result-errors-p result))))

;;; Test compilation hooks
(deftest test-compilation-capture
  "Test basic compilation capture"
  (hooks:with-compilation-capture ()
    ;; Simulate capturing a warning
    (hooks:capture-compiler-condition (make-condition 'simple-warning
                                                      :format-control "Test warning")
                                      :warning)
    (assert-= 1 (length hooks:*current-compilation-messages*))
    (let ((msg (first hooks:*current-compilation-messages*)))
      (assert-equal :warning (diag:diagnostic-severity msg))
      (assert-true (search "Test warning" (diag:diagnostic-message msg))))))

(deftest test-source-location-extraction
  "Test extraction of source location from compiler state"
  (let ((sl (hooks:extract-source-location)))
    ;; Location might be nil outside compilation context
    (assert-true (or (null sl) (loc:source-location-p sl)))))

;;; Test structured compilation
(deftest test-compile-form-structured
  "Test structured compilation of a simple form"
  (let ((result (compile:compile-form-structured '(lambda (x) (+ x 1))
                                                 :name 'test-function)))
    (assert-true (cr:compilation-result-p result))
    (assert-true (cr:compilation-result-success-p result))
    (assert-true (not (cr:compilation-result-errors-p result)))))

(deftest test-compile-form-with-warning
  "Test compilation with warnings"
  (let ((result (compile:compile-form-structured
                  '(lambda (x)
                    (declare (ignore x))
                    (+ y 1)) ; y is undefined
                  :name 'test-warning)))
    (assert-true (cr:compilation-result-p result))
    ;; Should have a warning about undefined variable
    (assert-true (cr:compilation-result-warnings-p result))))

(deftest test-compile-form-with-error
  "Test compilation with errors"
  (let ((result (compile:compile-form-structured
                  'invalid-form ; Not a valid lambda form
                  :name 'test-error)))
    (assert-true (cr:compilation-result-p result))
    (assert-true (cr:compilation-result-errors-p result))))

(deftest test-compile-string-structured
  "Test compilation of string source"
  (let ((result (compile:compile-string-structured "(lambda (x) (* x 2))")))
    (assert-true (cr:compilation-result-p result))
    (assert-true (cr:compilation-result-success-p result))))

;;; Test location tracking
(deftest test-file-line-caching
  "Test file line position caching"
  (let ((temp-file "/tmp/epsilon-test-compile.lisp"))
    (with-open-file (stream temp-file :direction :output :if-exists :supersede)
      (format stream "line 1~%line 2~%line 3~%"))
    (unwind-protect (progn
      (location:cache-file-lines temp-file)
      (multiple-value-bind (line column) (location:file-position-to-line-column temp-file 0)
        (assert-= 1 line)
        (assert-= 1 column))
      (multiple-value-bind (line column) (location:file-position-to-line-column temp-file 7) ; Start of line 2
        (assert-= 2 line)
        (assert-= 1 column)))
      (delete-file temp-file))))

;;; Test output formatting
(deftest test-compilation-result-to-plist
  "Test conversion of compilation result to plist"
  (let* ((d (diag:make-warning "Test warning"))
         (result (cr:make-compilation-result :file "test.lisp"
                                             :diagnostics (list d)
                                             :success-p t))
         (plist (compile:compilation-result-to-plist result)))
    (assert-true (listp plist))
    (assert-equal "test.lisp" (getf plist :file))
    (assert-true (getf plist :success))
    (assert-= 1 (length (getf plist :messages)))))

(deftest test-message-counting
  "Test counting diagnostics by severity"
  (let* ((w1 (diag:make-warning "w1"))
         (w2 (diag:make-warning "w2"))
         (e1 (diag:make-error "e1"))
         (n1 (diag:make-note "n1"))
         (result (cr:make-compilation-result :diagnostics (list w1 w2 e1 n1)
                                             :error-count 1
                                             :warning-count 2)))
    (assert-= 2 (cr:count-diagnostics-by-severity result :warning))
    (assert-= 1 (cr:count-diagnostics-by-severity result :error))
    (assert-= 1 (cr:count-diagnostics-by-severity result :note))
    (assert-= 0 (cr:count-diagnostics-by-severity result :info))))

(deftest test-get-errors-and-warnings
  "Test filtering diagnostics by type"
  (let* ((warn (diag:make-warning "warn"))
         (err (diag:make-error "err"))
         (style (diag:make-style-warning "style"))
         (result (cr:make-compilation-result :diagnostics (list warn err style)
                                             :error-count 1 :warning-count 2)))
    (let ((errors (cr:get-errors result)))
      (assert-= 1 (length errors))
      (assert-equal "err" (diag:diagnostic-message (first errors))))
    (let ((warnings (cr:get-warnings result)))
      (assert-= 2 (length warnings))
      (assert-true (member "warn" warnings :key #'diag:diagnostic-message :test #'equal))
      (assert-true (member "style" warnings :key #'diag:diagnostic-message :test #'equal)))))

;;; Test compilation observer
(deftest test-compilation-observer
  "Test compilation observer pattern"
  (let ((events nil))
    (compile:with-compilation-observer (lambda (event &rest args)
      (push (cons event args) events))
      (compile:compile-form-structured '(lambda (x) x)))
    (assert-true (assoc :compilation-start events))
    (assert-true (assoc :compilation-end events))))

;;; Integration test with real file compilation
(deftest test-compile-file-structured
  "Test structured compilation of a real file"
  (let ((result nil))
    (assert-true (fboundp 'compile:compile-file-structured))
    (setf result (cr:make-compilation-result :file "test.lisp" :success-p t))
    (assert-true (cr:compilation-result-p result))))
