;;;; compiler-tests.lisp - Tests for main compiler interface

(defpackage :epsilon.compiler.tests
  (:use :cl :epsilon.test)
  (:import
   (:epsilon.compiler :compiler)
   (:epsilon.compiler.config :config)
   (:epsilon.compiler.fasl :fasl)
   (:epsilon.diagnostic :diag)
   (:epsilon.compiler.progress :progress)
   (:epsilon.compiler.fasl :fasl)
   (:epsilon.source-location :loc)))

;;; Compilation result tests

(deftest test-make-compilation-result
  (let ((result (compiler:make-compilation-result
                 :success t
                 :forms-processed 42)))
    (assert-true (compiler:compilation-result-p result))
    (assert-true (eq t (compiler:compilation-result-success result)))
    (assert-true (= 42 (compiler:compilation-result-forms-processed result)))
    (assert-true (= 0 (compiler:compilation-result-error-count result)))))

(deftest test-compilation-result-defaults
  (let ((result (compiler:make-compilation-result)))
    (assert-true (eq nil (compiler:compilation-result-success result)))
    (assert-true (eq nil (compiler:compilation-result-warnings-p result)))
    (assert-true (= 0 (compiler:compilation-result-error-count result)))
    (assert-true (= 0 (compiler:compilation-result-warning-count result)))
    (assert-true (null (compiler:compilation-result-diagnostics result)))))

;;; Compilation artifacts tests

(deftest test-make-compilation-artifacts
  (let ((arts (compiler:make-compilation-artifacts
               :fasl-file #p"/build/test.fasl"
               :metadata '((:version . "1.0")))))
    (assert-true (compiler:compilation-artifacts-p arts))
    (assert-true (pathnamep (compiler:compilation-artifacts-fasl-file arts)))
    (assert-true (equal '((:version . "1.0"))
               (compiler:compilation-artifacts-metadata arts)))))

;;; Compile forms tests

(deftest test-compile-forms-simple
  (let ((result (compiler:compile-forms
                 '((+ 1 2)
                   (* 3 4)))))
    (assert-true (compiler:compilation-result-p result))
    (assert-true (= 2 (compiler:compilation-result-forms-processed result)))
    (assert-true (compiler:compilation-result-success result))))

(deftest test-compile-forms-empty
  (let ((result (compiler:compile-forms nil)))
    (assert-true (compiler:compilation-result-p result))
    (assert-true (= 0 (compiler:compilation-result-forms-processed result)))))

;;; CST-aware form compilation tests (IMPL-320)

(deftest test-compile-forms-from-cst-simple
  "compile-forms-from-cst processes forms read from a code string."
  (let ((result (compiler:compile-forms-from-cst "(+ 1 2) (* 3 4)")))
    (assert-true (compiler:compilation-result-p result))
    (assert-true (compiler:compilation-result-success result))
    (assert-true (= 2 (compiler:compilation-result-forms-processed result)))
    (assert-true (null (compiler:compilation-result-diagnostics result)))))

(deftest test-compile-forms-from-cst-empty
  "compile-forms-from-cst handles empty input."
  (let ((result (compiler:compile-forms-from-cst "")))
    (assert-true (compiler:compilation-result-p result))
    (assert-true (= 0 (compiler:compilation-result-forms-processed result)))))

(deftest test-compile-forms-from-cst-skips-trivia
  "compile-forms-from-cst ignores comments and whitespace between forms."
  (let ((result (compiler:compile-forms-from-cst
                 ";; leading comment
(+ 1 2)
;; mid
(* 3 4)
;; trailing")))
    (assert-true (compiler:compilation-result-success result))
    (assert-true (= 2 (compiler:compilation-result-forms-processed result)))))

(deftest test-compile-forms-from-cst-warning-location
  "Warnings emitted during compile carry a byte-accurate source-location.
   With SBCL's source-path resolving to the offending sub-expression, the
   undefined-variable warning points at the (+ x 1) call rather than the
   enclosing defun."
  (let* ((code "(defun ok () 1)
(defun broken () (+ x 1))")
         (result (compiler:compile-forms-from-cst code :name "<eval>"))
         (diagnostics (compiler:compilation-result-diagnostics result)))
    (assert-true (compiler:compilation-result-warnings-p result))
    (assert-true (= 1 (length diagnostics)))
    (let* ((d (first diagnostics))
           (location (diag:diagnostic-location d)))
      (assert-true (eq :warning (diag:diagnostic-severity d)))
      (assert-true (loc:source-location-p location))
      (assert-true (equal "<eval>" (loc:source-location-file location)))
      ;; (defun broken () (+ x 1)) starts at byte 16; (+ x 1) is 17 bytes
      ;; further in, so absolute offset 33, line 2, column 17. The cell
      ;; (+ x 1) is 7 bytes long, so end-offset is 40.
      (assert-= 33 (loc:source-location-offset location))
      (assert-= 40 (loc:source-location-end-offset location))
      (assert-= 2 (loc:source-location-line location))
      (assert-= 17 (loc:source-location-column location))
      (assert-= 2 (loc:source-location-end-line location))
      (assert-= 24 (loc:source-location-end-column location)))))

(deftest test-compile-forms-from-cst-name-defaults
  "Without :name, the location file falls back to a placeholder string."
  (let* ((result (compiler:compile-forms-from-cst "(defun broken () (+ x 1))"))
         (d (first (compiler:compilation-result-diagnostics result))))
    (when d
      (let ((location (diag:diagnostic-location d)))
        (when location
          (assert-true (stringp (loc:source-location-file location))))))))

(deftest test-compile-forms-from-cst-subexpr-nested
  "Warnings resolve to the offending sub-expression's byte range, not just
   the top-level form. (progn (- 3 undef-z)) -> the (- 3 undef-z) cell."
  (let* ((code "(progn (- 3 undef-z))")
         (result (compiler:compile-forms-from-cst code :name "<eval>"))
         (d (first (compiler:compilation-result-diagnostics result)))
         (loc (and d (diag:diagnostic-location d))))
    (assert-true (loc:source-location-p loc))
    ;; "(progn " is 7 bytes; (- 3 undef-z) spans bytes [7, 20).
    (assert-= 7 (loc:source-location-offset loc))
    (assert-= 20 (loc:source-location-end-offset loc))
    (assert-= 1 (loc:source-location-line loc))
    (assert-= 7 (loc:source-location-column loc))
    (assert-= 1 (loc:source-location-end-line loc))
    (assert-= 20 (loc:source-location-end-column loc))))

(deftest test-compile-forms-from-cst-subexpr-multiline
  "End-line / end-column resolve correctly when the offending sub-expression
   spans different lines from the start of the source."
  (let* ((code "(defun ok () 1)
(progn
  (- 3 undef-z))")
         (result (compiler:compile-forms-from-cst code :name "<eval>"))
         (d (first (compiler:compilation-result-diagnostics result)))
         (loc (and d (diag:diagnostic-location d))))
    (assert-true (loc:source-location-p loc))
    ;; (- 3 undef-z) sits on line 3 starting at column 2.
    (assert-= 3 (loc:source-location-line loc))
    (assert-= 2 (loc:source-location-column loc))
    (assert-= 3 (loc:source-location-end-line loc))
    (assert-= 15 (loc:source-location-end-column loc))))

(deftest test-compile-forms-from-cst-subexpr-in-second-form
  "Sub-expression resolution composes with multi-form input — the second
   top-level form's nested cell is offset relative to the whole source."
  (let* ((code "(defun ok () 1)
(progn (- 3 undef-z))")
         (result (compiler:compile-forms-from-cst code :name "<eval>"))
         (d (first (compiler:compilation-result-diagnostics result)))
         (loc (and d (diag:diagnostic-location d))))
    (assert-true (loc:source-location-p loc))
    ;; First form occupies bytes 0..14, then a newline (15), then "(progn "
    ;; is 16..22, so (- 3 undef-z) starts at offset 23 on line 2.
    (assert-= 23 (loc:source-location-offset loc))
    (assert-= 2 (loc:source-location-line loc))
    (assert-= 7 (loc:source-location-column loc))))

(deftest test-navigate-by-source-path-empty
  "Empty path returns the form unchanged (location falls back to wrapper)."
  (assert-true (equal '(lambda () 1)
                      (compiler::navigate-by-source-path '(lambda () 1) nil))))

(deftest test-navigate-by-source-path-walks-indices
  "Source path (2 2 0) navigates lambda -> form -> third subform."
  (let ((tree '(lambda () (progn a (- 3 z)))))
    (assert-true (equal '(- 3 z)
                        (compiler::navigate-by-source-path tree '(2 2 0))))))

(deftest test-navigate-by-source-path-out-of-range
  "An out-of-range index yields NIL rather than nth-walking off the end."
  (assert-true (null (compiler::navigate-by-source-path
                      '(lambda () (foo)) '(99 0))))
  (assert-true (null (compiler::navigate-by-source-path
                      '(lambda () (foo)) '(2 99 0)))))

(deftest test-navigate-by-source-path-skips-non-integers
  "Non-integer markers (e.g. ORIGINAL-SOURCE-START forms) are ignored."
  (let ((tree '(lambda () (a b c))))
    (assert-true (equal '(a b c)
                        (compiler::navigate-by-source-path
                         tree '((sb-c::original-source-start 0 0) 2 0))))))

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
      (assert-true (search "Compiling" text))
      (assert-true (search "Done" text)))))

(deftest test-progress-json-handler
  (let ((output (make-string-output-stream)))
    (progress:with-progress-handler
        (progress:json-progress-handler :stream output)
      (progress:emit-progress :compilation-started))
    (let ((text (get-output-stream-string output)))
      (assert-true (search "\"kind\":\"compilation-started\"" text)))))

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

    (assert-true (progress:progress-aggregator-p aggregator))
    (assert-true (= 3 (progress:progress-aggregator-files-total aggregator)))
    (assert-true (= 2 (progress:progress-aggregator-files-completed aggregator)))
    (assert-true (= 1 (progress:progress-aggregator-forms-processed aggregator)))
    (assert-true (= 1 (progress:progress-aggregator-functions-compiled aggregator)))))

(deftest test-progress-summary
  (let ((aggregator nil))
    (progress:with-progress-aggregator (agg)
      (setf aggregator agg)
      (progress:emit-progress :file-finished)
      (progress:emit-progress :compilation-finished))

    (let ((summary (progress:progress-summary aggregator)))
      (assert-true (listp summary))
      (assert-true (= 1 (getf summary :files-completed)))
      (assert-true (numberp (getf summary :elapsed-seconds))))))

;;; Diagnostic handler tests

(deftest test-diagnostic-handler-integration
  (let ((collected nil))
    (let ((diag:*diagnostic-handler*
            (lambda (d) (push d collected))))
      (diag:emit-diagnostic (diag:make-error "Error 1"))
      (diag:emit-diagnostic (diag:make-warning "Warning 1")))

    (assert-true (= 2 (length collected)))
    (assert-true (eq :warning (diag:diagnostic-severity (first collected))))
    (assert-true (eq :error (diag:diagnostic-severity (second collected))))))

;;; Package symbol tests

(deftest test-compiler-package-symbols
  ;; Verify key symbols defined in main compiler package
  (assert-true (fboundp 'compiler:compile-source))
  (assert-true (fboundp 'compiler:make-compilation-result))
  (assert-true (fboundp 'compiler:make-compilation-artifacts))
  ;; Verify sub-package symbols accessible via local nicknames
  (assert-true (fboundp 'config:make-compiler-config))
  (assert-true (fboundp 'diag:make-diagnostic))
  (assert-true (fboundp 'fasl:make-fasl-options)))

;;; compile-source diagnostic capture (IMPL-320 Stage 1+)

(deftest test-compile-source-captures-warnings
  "compile-source routes through compile-file-safely, so the CST source
   tracking hooks fire and SBCL warnings flow through *diagnostic-handler*
   instead of escaping to stderr. The captured diagnostic carries a byte
   range pointing at the offending sub-expression, not just the file."
  (let ((src-path #p"/tmp/epsilon-compiler-test-warning.lisp")
        (fasl-path #p"/tmp/epsilon-compiler-test-warning.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (write-string "(defun ok () 1)
(defun bad () (+ undef-token 1))" s))
          (let* ((result (compiler:compile-source src-path
                                                  :output-file fasl-path))
                 (diagnostics (compiler:compilation-result-diagnostics result))
                 (warning (find :warning diagnostics
                                :key #'diag:diagnostic-severity)))
            (assert-true warning)
            (assert-true (search "UNDEF-TOKEN"
                                 (diag:diagnostic-message warning)))
            (let ((loc (diag:diagnostic-location warning)))
              (assert-true (loc:source-location-p loc))
              (assert-true (search "epsilon-compiler-test-warning"
                                   (loc:source-location-file loc)))
              ;; (+ undef-token 1) sits on line 2, column 14.
              (assert-= 2 (loc:source-location-line loc))
              (assert-= 14 (loc:source-location-column loc))
              (assert-true (numberp (loc:source-location-offset loc)))
              (assert-true (numberp (loc:source-location-end-offset loc)))
              (assert-true (> (loc:source-location-end-offset loc)
                              (loc:source-location-offset loc))))))
      (when (probe-file src-path) (delete-file src-path))
      (when (probe-file fasl-path) (delete-file fasl-path))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings")
                      fasl-path)))
        (when (probe-file sidecar) (delete-file sidecar))))))

;;; compile-source-from-cst (IMPL-320 Stage 2)

(deftest test-compile-source-from-cst-success
  "compile-source-from-cst reads INPUT-FILE through the CST reader only,
   compiles each top-level form in memory, and returns a compilation-
   result without producing a FASL."
  (let ((src #p"/tmp/epsilon-csfc-success.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string "(defun csfc-success-a () 1)
(defun csfc-success-b () 2)" s))
          (let ((result (compiler:compile-source-from-cst src)))
            (assert-true (compiler:compilation-result-success result))
            (assert-= 2 (compiler:compilation-result-forms-processed result))
            (assert-true (null (compiler:compilation-result-diagnostics result)))))
      (when (probe-file src) (delete-file src)))))

(deftest test-compile-source-from-cst-subexpr-diagnostic
  "Warnings emitted during in-memory compile resolve to the offending
   sub-expression's byte range, with the input file's absolute path
   recorded as the location's file. (Same machinery as compile-forms-
   from-cst, but driven from a file.)"
  (let ((src #p"/tmp/epsilon-csfc-warn.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string ";; preamble line 1
;; preamble line 2
(defun csfc-warn-ok () 1)
(defun csfc-warn-bad () (+ csfc-warn-missing 1))" s))
          (let* ((result (compiler:compile-source-from-cst src))
                 (diagnostics (compiler:compilation-result-diagnostics result))
                 (warning (find :warning diagnostics
                                :key #'diag:diagnostic-severity)))
            (assert-true warning)
            (assert-true (search "CSFC-WARN-MISSING"
                                 (diag:diagnostic-message warning)))
            (let ((loc (diag:diagnostic-location warning)))
              (assert-true (loc:source-location-p loc))
              (assert-true (search "epsilon-csfc-warn"
                                   (loc:source-location-file loc)))
              ;; (+ csfc-warn-missing 1) sits on line 4. The (defun
              ;; csfc-warn-bad () header is "(defun csfc-warn-bad () "
              ;; = 24 bytes, so the inner call starts at column 24.
              (assert-= 4 (loc:source-location-line loc))
              (assert-= 24 (loc:source-location-column loc))
              ;; End-offset must be set — that's the whole point.
              (assert-true (numberp (loc:source-location-end-offset loc)))
              (assert-true (> (loc:source-location-end-offset loc)
                              (loc:source-location-offset loc))))))
      (when (probe-file src) (delete-file src)))))

(deftest test-compile-source-from-cst-empty-file
  "An empty file processes zero forms and emits no diagnostics."
  (let ((src #p"/tmp/epsilon-csfc-empty.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create))
          (let ((result (compiler:compile-source-from-cst src)))
            (assert-= 0 (compiler:compilation-result-forms-processed result))
            (assert-true (null (compiler:compilation-result-diagnostics result)))))
      (when (probe-file src) (delete-file src)))))

(deftest test-compile-source-from-cst-honors-reader-conditionals
  "compile-source-from-cst evaluates #+/#- against *features* and skips
   the unselected form, so a #-sbcl branch defining an undefined
   reference doesn't reach the compile pass and produce a warning."
  (let ((src #p"/tmp/epsilon-csfc-conditionals.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string "(defun csfc-cond-a () 1)
#+sbcl (defun csfc-cond-on-sbcl () 2)
#-sbcl (defun csfc-cond-not-on-sbcl () (no-such-fn))" s))
          (let* ((result (compiler:compile-source-from-cst src))
                 (diagnostics (compiler:compilation-result-diagnostics result)))
            ;; The #-sbcl branch references no-such-fn; if conditionals
            ;; were ignored we'd see undefined-function diagnostics here.
            (assert-true (compiler:compilation-result-success result))
            (assert-true (every (lambda (d)
                                  (not (search "NO-SUCH-FN"
                                               (diag:diagnostic-message d))))
                                diagnostics))))
      (when (probe-file src) (delete-file src)))))
