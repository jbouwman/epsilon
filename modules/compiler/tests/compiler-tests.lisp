;;;; compiler-tests.lisp - Tests for main compiler interface

(defpackage :epsilon.compiler.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:compiler :epsilon.compiler)
   (:config :epsilon.compiler.config)
   (:diag :epsilon.compiler.diagnostics)
   (:progress :epsilon.compiler.progress)))

(in-package :epsilon.compiler.tests)

;;; Compilation result tests

(deftest test-make-compilation-result
  (let ((result (compiler:make-compilation-result
                 :success t
                 :forms-processed 42)))
    (is (compiler:compilation-result-p result))
    (is (eq t (compiler:compilation-result-success result)))
    (is (= 42 (compiler:compilation-result-forms-processed result)))
    (is (= 0 (compiler:compilation-result-error-count result)))))

(deftest test-compilation-result-defaults
  (let ((result (compiler:make-compilation-result)))
    (is (eq nil (compiler:compilation-result-success result)))
    (is (eq nil (compiler:compilation-result-warnings-p result)))
    (is (= 0 (compiler:compilation-result-error-count result)))
    (is (= 0 (compiler:compilation-result-warning-count result)))
    (is (null (compiler:compilation-result-diagnostics result)))))

;;; Compilation artifacts tests

(deftest test-make-compilation-artifacts
  (let ((arts (compiler:make-compilation-artifacts
               :fasl-file #p"/build/test.fasl"
               :metadata '((:version . "1.0")))))
    (is (compiler:compilation-artifacts-p arts))
    (is (pathnamep (compiler:compilation-artifacts-fasl-file arts)))
    (is (equal '((:version . "1.0"))
               (compiler:compilation-artifacts-metadata arts)))))

;;; Compile forms tests

(deftest test-compile-forms-simple
  (let ((result (compiler:compile-forms
                 '((+ 1 2)
                   (* 3 4)))))
    (is (compiler:compilation-result-p result))
    (is (= 2 (compiler:compilation-result-forms-processed result)))
    (is (compiler:compilation-result-success result))))

(deftest test-compile-forms-empty
  (let ((result (compiler:compile-forms nil)))
    (is (compiler:compilation-result-p result))
    (is (= 0 (compiler:compilation-result-forms-processed result)))))

;;; Progress integration tests

(deftest test-progress-with-console-handler
  (let ((output (make-string-output-stream)))
    (progress:with-progress-handler
        (progress:console-progress-handler :stream output :verbose nil)
      (progress:emit-progress :compilation-started)
      (progress:emit-progress :file-started :file #p"test.lisp")
      (progress:emit-progress :file-finished :file #p"test.lisp")
      (progress:emit-progress :compilation-finished))
    (let ((text (get-output-stream-string output)))
      (is (search "Compiling" text))
      (is (search "Done" text)))))

(deftest test-progress-json-handler
  (let ((output (make-string-output-stream)))
    (progress:with-progress-handler
        (progress:json-progress-handler :stream output)
      (progress:emit-progress :compilation-started))
    (let ((text (get-output-stream-string output)))
      (is (search "\"kind\":\"compilation-started\"" text)))))

(deftest test-progress-aggregator
  (let ((aggregator nil))
    (progress:with-progress-aggregator (agg)
      (setf aggregator agg)
      (progress:emit-progress :compilation-started
                              :data '(:file-count 3))
      (progress:emit-progress :file-finished)
      (progress:emit-progress :file-finished)
      (progress:emit-progress :form-finished)
      (progress:emit-progress :function-compiled)
      (progress:emit-progress :compilation-finished))

    (is (progress:progress-aggregator-p aggregator))
    (is (= 3 (progress:progress-aggregator-files-total aggregator)))
    (is (= 2 (progress:progress-aggregator-files-completed aggregator)))
    (is (= 1 (progress:progress-aggregator-forms-processed aggregator)))
    (is (= 1 (progress:progress-aggregator-functions-compiled aggregator)))))

(deftest test-progress-summary
  (let ((aggregator nil))
    (progress:with-progress-aggregator (agg)
      (setf aggregator agg)
      (progress:emit-progress :file-finished)
      (progress:emit-progress :compilation-finished))

    (let ((summary (progress:progress-summary aggregator)))
      (is (listp summary))
      (is (= 1 (getf summary :files-completed)))
      (is (numberp (getf summary :elapsed-seconds))))))

;;; Diagnostic handler tests

(deftest test-diagnostic-handler-integration
  (let ((collected nil))
    (let ((diag:*diagnostic-handler*
            (lambda (d) (push d collected))))
      (diag:emit-diagnostic (diag:make-error "Error 1"))
      (diag:emit-diagnostic (diag:make-warning "Warning 1")))

    (is (= 2 (length collected)))
    (is (eq :warning (diag:diagnostic-severity (first collected))))
    (is (eq :error (diag:diagnostic-severity (second collected))))))

;;; Re-exported symbol tests

(deftest test-reexports
  ;; Verify key symbols are exported from main package
  (is (fboundp 'compiler:make-compiler-config))
  (is (fboundp 'compiler:make-diagnostic))
  (is (fboundp 'compiler:make-source-span))
  (is (fboundp 'compiler:make-fasl-options))
  (is (fboundp 'compiler:compile-source)))
