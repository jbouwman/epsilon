;;;; fasl-tests.lisp - Tests for FASL operations

(defpackage :epsilon.compiler.fasl.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.compiler.fasl fasl)))

;;; FASL options tests

(deftest test-make-fasl-options
  (let ((opts (fasl:make-fasl-options)))
    (assert-true (fasl:fasl-options-p opts))
    (assert-true (eq :relative (fasl:fasl-options-path-mode opts)))
    (assert-true (eq t (fasl:fasl-options-include-docstrings opts)))
    (assert-true (eq t (fasl:fasl-options-include-source-locations opts)))
    (assert-true (eq nil (fasl:fasl-options-compress opts)))
    (assert-true (eq t (fasl:fasl-options-deterministic opts)))))

(deftest test-fasl-options-custom-values
  (let ((opts (fasl:make-fasl-options
               :path-mode :absolute
               :include-source t
               :compress t
               :compression-level 9)))
    (assert-true (eq :absolute (fasl:fasl-options-path-mode opts)))
    (assert-true (eq t (fasl:fasl-options-include-source opts)))
    (assert-true (eq t (fasl:fasl-options-compress opts)))
    (assert-true (= 9 (fasl:fasl-options-compression-level opts)))))

(deftest test-with-fasl-options
  (let ((outer fasl:*fasl-options*))
    (fasl:with-fasl-options (fasl:make-fasl-options :path-mode :absolute)
      (assert-true (eq :absolute (fasl:fasl-options-path-mode fasl:*fasl-options*)))
      (assert-true (not (eq outer fasl:*fasl-options*))))
    (assert-true (eq outer fasl:*fasl-options*))))

;;; FASL metadata tests

(deftest test-make-fasl-metadata
  (let ((meta (fasl:make-fasl-metadata
               :epsilon-version "0.1.0"
               :sbcl-version "2.3.0"
               :source-file "/test/file.lisp")))
    (assert-true (fasl:fasl-metadata-p meta))
    (assert-true (equal "0.1.0" (fasl:fasl-metadata-epsilon-version meta)))
    (assert-true (equal "2.3.0" (fasl:fasl-metadata-sbcl-version meta)))
    (assert-true (equal "/test/file.lisp" (fasl:fasl-metadata-source-file meta)))))

(deftest test-fasl-metadata-defaults
  (let ((meta (fasl:make-fasl-metadata)))
    (assert-true (= 1 (fasl:fasl-metadata-format-version meta)))
    (assert-true (null (fasl:fasl-metadata-dependencies meta)))
    (assert-true (null (fasl:fasl-metadata-exports meta)))))

;;; Path transformer tests

(deftest test-make-path-transformer-absolute
  (let* ((opts (fasl:make-fasl-options :path-mode :absolute))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (assert-true (functionp transformer))
    (assert-true (equal "/some/path.lisp"
               (funcall transformer "/some/path.lisp")))))

(deftest test-make-path-transformer-relative
  (let* ((opts (fasl:make-fasl-options
                :path-mode :relative
                :source-root #p"/project/src/"))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (assert-true (functionp transformer))
    (assert-true (equal "bar/baz.lisp"
               (funcall transformer "/project/src/bar/baz.lisp")))))

(deftest test-make-path-transformer-none
  (let* ((opts (fasl:make-fasl-options :path-mode :none))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (assert-true (functionp transformer))
    (assert-true (null (funcall transformer "/any/path.lisp")))))

;;; Source location remapping tests

(deftest test-source-location-remapping
  (assert-true (null fasl:*source-location-remapping*))
  (fasl:with-source-root-mapping ("/old/path/" "/new/path/")
    (assert-true (= 1 (length fasl:*source-location-remapping*)))
    (assert-true (equal "/old/path/" (caar fasl:*source-location-remapping*)))
    (assert-true (equal "/new/path/" (cdar fasl:*source-location-remapping*))))
  (assert-true (null fasl:*source-location-remapping*)))

(deftest test-nested-source-root-mapping
  (fasl:with-source-root-mapping ("/a/" "/b/")
    (fasl:with-source-root-mapping ("/c/" "/d/")
      (assert-true (= 2 (length fasl:*source-location-remapping*)))))
  (assert-true (null fasl:*source-location-remapping*)))

;;; FASL info tests

(deftest test-fasl-info-nonexistent
  ;; Should return NIL for nonexistent file
  (let ((info (fasl:fasl-info #p"/nonexistent/file.fasl")))
    (assert-true (null info))))

;;; Verify fasl tests

(deftest test-verify-fasl-nonexistent
  (assert-true (null (fasl:verify-fasl #p"/nonexistent/file.fasl"))))

;;; Constants tests

(deftest test-fasl-constants
  (assert-true (integerp fasl:+epsilon-fasl-magic+))
  (assert-true (= 1 fasl:+epsilon-fasl-version+)))
