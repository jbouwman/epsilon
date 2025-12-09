;;;; fasl-tests.lisp - Tests for FASL operations

(defpackage :epsilon.compiler.fasl.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:fasl :epsilon.compiler.fasl)))

(in-package :epsilon.compiler.fasl.tests)

;;; FASL options tests

(deftest test-make-fasl-options
  (let ((opts (fasl:make-fasl-options)))
    (is (fasl:fasl-options-p opts))
    (is (eq :relative (fasl:fasl-options-path-mode opts)))
    (is (eq t (fasl:fasl-options-include-docstrings opts)))
    (is (eq t (fasl:fasl-options-include-source-locations opts)))
    (is (eq nil (fasl:fasl-options-compress opts)))
    (is (eq t (fasl:fasl-options-deterministic opts)))))

(deftest test-fasl-options-custom-values
  (let ((opts (fasl:make-fasl-options
               :path-mode :absolute
               :include-source t
               :compress t
               :compression-level 9)))
    (is (eq :absolute (fasl:fasl-options-path-mode opts)))
    (is (eq t (fasl:fasl-options-include-source opts)))
    (is (eq t (fasl:fasl-options-compress opts)))
    (is (= 9 (fasl:fasl-options-compression-level opts)))))

(deftest test-with-fasl-options
  (let ((outer fasl:*fasl-options*))
    (fasl:with-fasl-options (fasl:make-fasl-options :path-mode :absolute)
      (is (eq :absolute (fasl:fasl-options-path-mode fasl:*fasl-options*)))
      (is (not (eq outer fasl:*fasl-options*))))
    (is (eq outer fasl:*fasl-options*))))

;;; FASL metadata tests

(deftest test-make-fasl-metadata
  (let ((meta (fasl:make-fasl-metadata
               :epsilon-version "0.1.0"
               :sbcl-version "2.3.0"
               :source-file "/test/file.lisp")))
    (is (fasl:fasl-metadata-p meta))
    (is (equal "0.1.0" (fasl:fasl-metadata-epsilon-version meta)))
    (is (equal "2.3.0" (fasl:fasl-metadata-sbcl-version meta)))
    (is (equal "/test/file.lisp" (fasl:fasl-metadata-source-file meta)))))

(deftest test-fasl-metadata-defaults
  (let ((meta (fasl:make-fasl-metadata)))
    (is (= 1 (fasl:fasl-metadata-format-version meta)))
    (is (null (fasl:fasl-metadata-dependencies meta)))
    (is (null (fasl:fasl-metadata-exports meta)))))

;;; Path transformer tests

(deftest test-make-path-transformer-absolute
  (let* ((opts (fasl:make-fasl-options :path-mode :absolute))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (is (functionp transformer))
    (is (equal "/some/path.lisp"
               (funcall transformer "/some/path.lisp")))))

(deftest test-make-path-transformer-relative
  (let* ((opts (fasl:make-fasl-options
                :path-mode :relative
                :source-root #p"/project/src/"))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (is (functionp transformer))
    (is (equal "bar/baz.lisp"
               (funcall transformer "/project/src/bar/baz.lisp")))))

(deftest test-make-path-transformer-none
  (let* ((opts (fasl:make-fasl-options :path-mode :none))
         (transformer (fasl:make-path-transformer opts #p"/project/src/foo.lisp")))
    (is (functionp transformer))
    (is (null (funcall transformer "/any/path.lisp")))))

;;; Source location remapping tests

(deftest test-source-location-remapping
  (is (null fasl:*source-location-remapping*))
  (fasl:with-source-root-mapping ("/old/path/" "/new/path/")
    (is (= 1 (length fasl:*source-location-remapping*)))
    (is (equal "/old/path/" (caar fasl:*source-location-remapping*)))
    (is (equal "/new/path/" (cdar fasl:*source-location-remapping*))))
  (is (null fasl:*source-location-remapping*)))

(deftest test-nested-source-root-mapping
  (fasl:with-source-root-mapping ("/a/" "/b/")
    (fasl:with-source-root-mapping ("/c/" "/d/")
      (is (= 2 (length fasl:*source-location-remapping*)))))
  (is (null fasl:*source-location-remapping*)))

;;; FASL info tests

(deftest test-fasl-info-nonexistent
  ;; Should return NIL for nonexistent file
  (let ((info (fasl:fasl-info #p"/nonexistent/file.fasl")))
    (is (null info))))

;;; Verify fasl tests

(deftest test-verify-fasl-nonexistent
  (is (null (fasl:verify-fasl #p"/nonexistent/file.fasl"))))

;;; Constants tests

(deftest test-fasl-constants
  (is (integerp fasl:+epsilon-fasl-magic+))
  (is (= 1 fasl:+epsilon-fasl-version+)))
