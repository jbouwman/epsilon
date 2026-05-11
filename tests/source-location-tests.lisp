;;;; source-location-tests.lisp - Tests for canonical source location types
;;;;
;;;; Tests for source-location, source-file-cache, offset/line conversion,
;;;; diagnostic, and compilation-result types.

(defpackage epsilon.source-location-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:import (epsilon.source-location loc)
           (epsilon.diagnostic diag)
           (epsilon.compilation-result cr)))

;;; ====================================================================
;;; Source Location Construction
;;; ====================================================================

(deftest test-make-source-location-empty
  (let ((sl (loc:make-source-location)))
    (assert-true (loc:source-location-p sl))
    (assert-true (null (loc:source-location-file sl)))
    (assert-true (null (loc:source-location-offset sl)))
    (assert-true (null (loc:source-location-line sl)))))

(deftest test-make-source-location-offset-only
  (let ((sl (loc:make-source-location :offset 10 :end-offset 20)))
    (assert-equal 10 (loc:source-location-offset sl))
    (assert-equal 20 (loc:source-location-end-offset sl))
    (assert-true (null (loc:source-location-line sl)))))

(deftest test-make-source-location-line-only
  (let ((sl (loc:make-source-location :file "test.lisp" :line 5 :column 3)))
    (assert-equal "test.lisp" (loc:source-location-file sl))
    (assert-equal 5 (loc:source-location-line sl))
    (assert-equal 3 (loc:source-location-column sl))
    (assert-true (null (loc:source-location-offset sl)))))

(deftest test-make-source-location-both
  (let ((sl (loc:make-source-location :file "test.lisp"
                                      :offset 42 :end-offset 50
                                      :line 3 :column 10
                                      :end-line 3 :end-column 18)))
    (assert-equal 42 (loc:source-location-offset sl))
    (assert-equal 3 (loc:source-location-line sl))
    (assert-equal 10 (loc:source-location-column sl))
    (assert-equal 18 (loc:source-location-end-column sl))))

;;; ====================================================================
;;; Copy
;;; ====================================================================

(deftest test-copy-source-location
  (let* ((orig (loc:make-source-location :file "a.lisp" :line 1 :column 0))
         (copy (loc:copy-source-location orig :line 5)))
    (assert-equal "a.lisp" (loc:source-location-file copy))
    (assert-equal 5 (loc:source-location-line copy))
    (assert-equal 0 (loc:source-location-column copy))
    ;; Original unchanged
    (assert-equal 1 (loc:source-location-line orig))))

;;; ====================================================================
;;; Predicates
;;; ====================================================================

(deftest test-contains-p
  (let ((sl (loc:make-source-location :line 5 :column 3 :end-line 5 :end-column 10)))
    (assert-true (loc:source-location-contains-p sl 5 5))
    (assert-true (loc:source-location-contains-p sl 5 3))
    (assert-true (loc:source-location-contains-p sl 5 10))
    (assert-true (not (loc:source-location-contains-p sl 5 2)))
    (assert-true (not (loc:source-location-contains-p sl 5 11)))
    (assert-true (not (loc:source-location-contains-p sl 4 5)))
    (assert-true (not (loc:source-location-contains-p sl 6 5)))))

(deftest test-contains-p-multiline
  (let ((sl (loc:make-source-location :line 3 :column 5 :end-line 7 :end-column 2)))
    (assert-true (loc:source-location-contains-p sl 5 0))
    (assert-true (loc:source-location-contains-p sl 3 5))
    (assert-true (loc:source-location-contains-p sl 7 2))
    (assert-true (not (loc:source-location-contains-p sl 3 4)))
    (assert-true (not (loc:source-location-contains-p sl 7 3)))))

(deftest test-overlaps-p
  (let ((sl1 (loc:make-source-location :line 3 :column 0 :end-line 5 :end-column 0))
        (sl2 (loc:make-source-location :line 4 :column 0 :end-line 6 :end-column 0))
        (sl3 (loc:make-source-location :line 6 :column 0 :end-line 8 :end-column 0)))
    (assert-true (loc:source-location-overlaps-p sl1 sl2))
    (assert-true (loc:source-location-overlaps-p sl2 sl1))
    (assert-true (not (loc:source-location-overlaps-p sl1 sl3)))))

(deftest test-overlaps-p-different-files
  (let ((sl1 (loc:make-source-location :file "a.lisp" :line 1 :column 0 :end-line 5 :end-column 0))
        (sl2 (loc:make-source-location :file "b.lisp" :line 1 :column 0 :end-line 5 :end-column 0)))
    (assert-true (not (loc:source-location-overlaps-p sl1 sl2)))))

;;; ====================================================================
;;; Formatting
;;; ====================================================================

(deftest test-format-source-location
  (assert-equal "test.lisp:5:3"
                (loc:format-source-location
                 (loc:make-source-location :file "test.lisp" :line 5 :column 3)))
  (assert-equal "test.lisp"
                (loc:format-source-location
                 (loc:make-source-location :file "test.lisp")))
  (assert-true (null (loc:format-source-location nil))))

;;; ====================================================================
;;; Line Offset Utilities
;;; ====================================================================

(deftest test-build-line-offsets
  (let ((offsets (loc:build-line-offsets "abc
def
ghi")))
    (assert-equal 3 (length offsets))
    (assert-equal 0 (aref offsets 0))
    (assert-equal 4 (aref offsets 1))
    (assert-equal 8 (aref offsets 2))))

(deftest test-build-line-offsets-empty
  (let ((offsets (loc:build-line-offsets "")))
    (assert-equal 1 (length offsets))
    (assert-equal 0 (aref offsets 0))))

(deftest test-offset-to-line-column
  (let ((offsets (loc:build-line-offsets "abc
def
ghi")))
    ;; First char
    (assert-equal '(1 . 0) (loc:offset-to-line-column offsets 0))
    ;; Last char of line 1
    (assert-equal '(1 . 2) (loc:offset-to-line-column offsets 2))
    ;; The newline itself
    (assert-equal '(1 . 3) (loc:offset-to-line-column offsets 3))
    ;; First char of line 2
    (assert-equal '(2 . 0) (loc:offset-to-line-column offsets 4))
    ;; Middle of line 3
    (assert-equal '(3 . 1) (loc:offset-to-line-column offsets 9))))

(deftest test-line-column-to-offset
  (let ((offsets (loc:build-line-offsets "abc
def
ghi")))
    (assert-equal 0 (loc:line-column-to-offset offsets 1 0))
    (assert-equal 4 (loc:line-column-to-offset offsets 2 0))
    (assert-equal 9 (loc:line-column-to-offset offsets 3 1))
    (assert-true (null (loc:line-column-to-offset offsets 0 0)))
    (assert-true (null (loc:line-column-to-offset offsets 4 0)))))

(deftest test-roundtrip-offset-line-column
  (let* ((source "hello
world
test line here")
         (offsets (loc:build-line-offsets source)))
    ;; Roundtrip: offset -> line/col -> offset
    (dolist (off '(0 3 5 6 10 11 15 20))
      (when (< off (length source))
        (let* ((lc (loc:offset-to-line-column offsets off))
               (back (loc:line-column-to-offset offsets (car lc) (cdr lc))))
          (assert-equal off back))))))

;;; ====================================================================
;;; Resolve Source Location
;;; ====================================================================

(deftest test-resolve-offset-to-line
  (let* ((source "abc
def
ghi")
         (sl (loc:make-source-location :offset 4 :end-offset 7))
         (resolved (loc:resolve-source-location sl :source source)))
    (assert-equal 2 (loc:source-location-line resolved))
    (assert-equal 0 (loc:source-location-column resolved))
    (assert-equal 4 (loc:source-location-offset resolved))))

(deftest test-resolve-line-to-offset
  (let* ((source "abc
def
ghi")
         (sl (loc:make-source-location :line 2 :column 1))
         (resolved (loc:resolve-source-location sl :source source)))
    (assert-equal 5 (loc:source-location-offset resolved))
    (assert-equal 2 (loc:source-location-line resolved))))

(deftest test-resolve-nil
  (assert-true (null (loc:resolve-source-location nil))))

;;; ====================================================================
;;; Diagnostic Construction
;;; ====================================================================

(deftest test-make-diagnostic-basic
  (let ((d (diag:make-error "undefined variable" :code "E0001")))
    (assert-true (diag:diagnostic-p d))
    (assert-equal :error (diag:diagnostic-severity d))
    (assert-equal "undefined variable" (diag:diagnostic-message d))
    (assert-equal "E0001" (diag:diagnostic-code d))))

(deftest test-make-diagnostic-with-location
  (let* ((sl (loc:make-source-location :file "test.lisp" :line 10 :column 5))
         (d (diag:make-warning "unused variable" :location sl)))
    (assert-equal :warning (diag:diagnostic-severity d))
    (assert-true (loc:source-location-p (diag:diagnostic-location d)))
    (assert-equal 10 (loc:source-location-line (diag:diagnostic-location d)))))

(deftest test-make-diagnostic-all-severities
  (assert-equal :error (diag:diagnostic-severity (diag:make-error "e")))
  (assert-equal :warning (diag:diagnostic-severity (diag:make-warning "w")))
  (assert-equal :style-warning (diag:diagnostic-severity (diag:make-style-warning "sw")))
  (assert-equal :note (diag:diagnostic-severity (diag:make-note "n")))
  (assert-equal :info (diag:diagnostic-severity (diag:make-info "i")))
  (assert-equal :hint (diag:diagnostic-severity (diag:make-hint "h"))))

(deftest test-diagnostic-with-fix
  (let* ((fix (diag:make-suggested-fix :description "Add declaration"
                                       :confidence :medium))
         (d (diag:make-warning "unused" :fixes (list fix))))
    (assert-equal 1 (length (diag:diagnostic-fixes d)))
    (assert-equal "Add declaration"
                  (diag:suggested-fix-description (first (diag:diagnostic-fixes d))))
    (assert-equal :medium (diag:suggested-fix-confidence (first (diag:diagnostic-fixes d))))))

(deftest test-diagnostic-with-related
  (let* ((rel (diag:make-related-diagnostic
               :message "first defined here"
               :location (loc:make-source-location :file "a.lisp" :line 3 :column 0)))
         (d (diag:make-error "duplicate definition" :related (list rel))))
    (assert-equal 1 (length (diag:diagnostic-related d)))
    (assert-equal "first defined here"
                  (diag:related-diagnostic-message (first (diag:diagnostic-related d))))))

;;; ====================================================================
;;; Diagnostic Emission
;;; ====================================================================

(deftest test-emit-diagnostic
  (let ((captured nil))
    (let ((diag:*diagnostic-handler* (lambda (d) (push d captured))))
      (diag:emit-diagnostic (diag:make-error "test"))
      (assert-equal 1 (length captured))
      (assert-equal "test" (diag:diagnostic-message (first captured))))))

(deftest test-emit-diagnostic-no-handler
  ;; Should not error when no handler is set
  (let ((diag:*diagnostic-handler* nil))
    (let ((d (diag:emit-diagnostic (diag:make-error "test"))))
      (assert-true (diag:diagnostic-p d)))))

;;; ====================================================================
;;; Diagnostic Formatting
;;; ====================================================================

(deftest test-format-diagnostic-text
  (let* ((sl (loc:make-source-location :file "test.lisp" :line 10 :column 5))
         (d (diag:make-error "undefined variable x" :location sl :code "E0001"))
         (text (diag:format-diagnostic d :style :text)))
    (assert-true (search "test.lisp:10:5" text))
    (assert-true (search "error" text))
    (assert-true (search "[E0001]" text))
    (assert-true (search "undefined variable x" text))))

(deftest test-format-diagnostic-json
  (let* ((sl (loc:make-source-location :file "test.lisp" :line 10 :column 5))
         (d (diag:make-error "bad" :location sl))
         (json (diag:format-diagnostic d :style :json)))
    (assert-true (search "\"severity\":\"error\"" json))
    (assert-true (search "\"message\":\"bad\"" json))
    (assert-true (search "\"startLine\":10" json))))

;;; ====================================================================
;;; Compilation Result
;;; ====================================================================

(deftest test-make-compilation-result-basic
  (let ((r (cr:make-compilation-result :success-p t :file "test.lisp")))
    (assert-true (cr:compilation-result-p r))
    (assert-true (cr:compilation-result-success-p r))
    (assert-equal "test.lisp" (cr:compilation-result-file r))
    (assert-equal 0 (cr:compilation-result-error-count r))
    ;; Default statistics
    (assert-true (cr:compilation-statistics-p (cr:compilation-result-statistics r)))))

(deftest test-compilation-result-with-diagnostics
  (let* ((d1 (diag:make-error "err"))
         (d2 (diag:make-warning "warn"))
         (d3 (diag:make-warning "warn2"))
         (r (cr:make-compilation-result :diagnostics (list d1 d2 d3)
                                        :error-count 1
                                        :warning-count 2)))
    (assert-equal 3 (length (cr:compilation-result-diagnostics r)))
    (assert-equal 1 (cr:count-diagnostics-by-severity r :error))
    (assert-equal 2 (cr:count-diagnostics-by-severity r :warning))
    (assert-equal 1 (length (cr:get-errors r)))
    (assert-equal 2 (length (cr:get-warnings r)))))

(deftest test-compilation-statistics
  (let ((s (cr:make-compilation-statistics :forms-processed 10
                                           :functions-compiled 5
                                           :bytes-consed 1024
                                           :cpu-time 0.5)))
    (assert-true (cr:compilation-statistics-p s))
    (assert-equal 10 (cr:compilation-statistics-forms-processed s))
    (assert-equal 5 (cr:compilation-statistics-functions-compiled s))
    (assert-equal 1024 (cr:compilation-statistics-bytes-consed s))))

(deftest test-compilation-artifacts
  (let ((a (cr:make-compilation-artifacts :fasl-file #p"/tmp/test.fasl"
                                          :metadata '(:version "1.0"))))
    (assert-true (cr:compilation-artifacts-p a))
    (assert-equal #p"/tmp/test.fasl" (cr:compilation-artifacts-fasl-file a))
    (assert-equal '(:version "1.0") (cr:compilation-artifacts-metadata a))))
