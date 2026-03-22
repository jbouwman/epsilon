;;;; Tests for epsilon.loader
;;;;
;;;; Tests the module loader functionality.

(defpackage epsilon.loader-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.loader loader)
            (epsilon.file fs))
  (:enter t))

;;; ---------------------------------------------------------------------------
;;; File Detection Tests
;;; ---------------------------------------------------------------------------

(deftest test-lisp-file-p-detects-lisp-files
  "lisp-file-p returns T for .lisp files"
  (assert-true (loader:lisp-file-p "foo.lisp"))
  (assert-true (loader:lisp-file-p "/path/to/bar.lisp"))
  (assert-true (loader:lisp-file-p "src/epsilon/http/client.lisp")))

(deftest test-lisp-file-p-rejects-non-lisp-files
  "lisp-file-p returns NIL for non-.lisp files"
  (assert-true (not (loader:lisp-file-p "foo.txt")))
  (assert-true (not (loader:lisp-file-p "foo.lispl")))
  (assert-true (not (loader:lisp-file-p "foo.lisp.bak")))
  (assert-true (not (loader:lisp-file-p "foo"))))

;;; ---------------------------------------------------------------------------
;;; Path Derivation Tests
;;; ---------------------------------------------------------------------------

(deftest test-derive-package-simple
  "derive-package-from-path works for simple paths"
  (assert-true (equal "epsilon.http.client"
             (loader:derive-package-from-path
              "src/epsilon/http/client.lisp"
              "src/"))))

(deftest test-derive-package-nested
  "derive-package-from-path works for deeply nested paths"
  (assert-true (equal "epsilon.http.request.parser"
             (loader:derive-package-from-path
              "/proj/src/epsilon/http/request/parser.lisp"
              "/proj/src/"))))

(deftest test-derive-package-single-level
  "derive-package-from-path works for single-level package"
  (assert-true (equal "epsilon"
             (loader:derive-package-from-path
              "src/epsilon.lisp"
              "src/"))))

(deftest test-derive-package-no-trailing-slash
  "derive-package-from-path handles src-root without trailing slash"
  (assert-true (equal "epsilon.http"
             (loader:derive-package-from-path
              "src/epsilon/http.lisp"
              "src"))))

;;; ---------------------------------------------------------------------------
;;; Package to Path Tests
;;; ---------------------------------------------------------------------------

(deftest test-package-to-path-simple
  "package-to-path converts dotted name to path"
  (assert-true (equal "epsilon/http/client.lisp"
             (loader:package-to-path "epsilon.http.client"))))

(deftest test-package-to-path-single
  "package-to-path works for single-component package"
  (assert-true (equal "epsilon.lisp"
             (loader:package-to-path "epsilon"))))

(deftest test-package-to-path-symbol
  "package-to-path works with symbol input"
  (assert-true (equal "epsilon/http.lisp"
             (loader:package-to-path 'epsilon.http))))

;;; ---------------------------------------------------------------------------
;;; Validation Tests
;;; ---------------------------------------------------------------------------

(deftest test-validate-matching-package
  "validate-package-path returns T for matching package"
  (multiple-value-bind (valid expected)
      (loader:validate-package-path "epsilon.http.client"
                                     "src/epsilon/http/client.lisp"
                                     "src/")
    (assert-true valid)
    (assert-true (equal "epsilon.http.client" expected))))

(deftest test-validate-mismatched-package
  "validate-package-path returns NIL for mismatched package"
  (multiple-value-bind (valid expected)
      (loader:validate-package-path "epsilon.wrong"
                                     "src/epsilon/http/client.lisp"
                                     "src/")
    (assert-true (not valid))
    (assert-true (equal "epsilon.http.client" expected))))

;;; ---------------------------------------------------------------------------
;;; Form Analysis Tests
;;; ---------------------------------------------------------------------------

(deftest test-find-package-form-defpackage
  "find-package-form finds defpackage"
  (let ((forms '((defpackage :foo (:use :cl)) (defun bar () 42))))
    (assert-true (equal '(defpackage :foo (:use :cl))
               (loader:find-package-form forms)))))

(deftest test-find-package-form-none
  "find-package-form returns NIL when no package form"
  (let ((forms '((defun bar () 42) (defvar *x* 1))))
    (assert-true (null (loader:find-package-form forms)))))

(deftest test-extract-package-name
  "extract-package-name gets name from defpackage form"
  (assert-true (equal "FOO" (loader:extract-package-name '(defpackage :foo (:use :cl))))))

;;; ---------------------------------------------------------------------------
;;; Symbol Visibility Tests
;;; ---------------------------------------------------------------------------

(deftest test-public-symbol-p-normal
  "public-symbol-p returns T for normal symbols"
  (assert-true (loader:public-symbol-p 'foo))
  (assert-true (loader:public-symbol-p 'bar))
  (assert-true (loader:public-symbol-p 'my-function)))

(deftest test-public-symbol-p-percent-prefix
  "public-symbol-p returns NIL for %-prefixed symbols"
  (assert-true (not (loader:public-symbol-p '%internal)))
  (assert-true (not (loader:public-symbol-p '%helper)))
  (assert-true (not (loader:public-symbol-p '%foo-bar))))

(deftest test-public-symbol-p-dash-prefix
  "public-symbol-p returns NIL for dash-prefixed symbols"
  (assert-true (not (loader:public-symbol-p '-private)))
  (assert-true (not (loader:public-symbol-p '-helper)))
  (assert-true (not (loader:public-symbol-p '-internal-fn))))

(deftest test-has-package-form-p-true
  "has-package-form-p returns T when defpackage form present"
  (assert-true (loader:has-package-form-p '((defpackage :foo (:use :cl)) (defun bar () nil)))))

(deftest test-has-package-form-p-false
  "has-package-form-p returns NIL when no package form"
  (assert-true (not (loader:has-package-form-p '((defun bar () nil) (defvar *x* 1))))))

;;; ---------------------------------------------------------------------------
;;; Export Tests
;;; ---------------------------------------------------------------------------

(deftest test-sync-package-exports-with-explicit-exports
  "sync-package-exports respects explicit exports"
  ;; Create a test package with some symbols
  (let ((pkg-name "EPSILON.TEST.EXPLICIT.EXPORTS"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (let ((pkg (make-package pkg-name :use nil)))
      (unwind-protect
           (progn
             ;; Define some symbols in the package
             (let ((*package* pkg))
               (intern "PUBLIC-FN" pkg)
               (intern "HELPER-FN" pkg)
               (intern "ANOTHER-FN" pkg)
               ;; Set function bindings so they're "bound"
               (setf (symbol-function (find-symbol "PUBLIC-FN" pkg))
                     (lambda () nil))
               (setf (symbol-function (find-symbol "HELPER-FN" pkg))
                     (lambda () nil))
               (setf (symbol-function (find-symbol "ANOTHER-FN" pkg))
                     (lambda () nil)))
             ;; Sync with explicit exports - only PUBLIC-FN should be exported
             (loader:sync-package-exports pkg-name
                                           :explicit-exports '("PUBLIC-FN"))
             ;; Check that only PUBLIC-FN is exported
             (multiple-value-bind (sym status)
                 (find-symbol "PUBLIC-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :external status)))
             (multiple-value-bind (sym status)
                 (find-symbol "HELPER-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :internal status)))
             (multiple-value-bind (sym status)
                 (find-symbol "ANOTHER-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :internal status))))
        ;; Cleanup
        (delete-package pkg)))))

(deftest test-sync-package-exports-auto-export
  "sync-package-exports auto-exports when no explicit exports"
  ;; Create a test package
  (let ((pkg-name "EPSILON.TEST.AUTO.EXPORTS"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (let ((pkg (make-package pkg-name :use nil)))
      (unwind-protect
           (progn
             ;; Define symbols - some public, some private
             (let ((*package* pkg))
               (intern "PUBLIC-FN" pkg)
               (intern "%PRIVATE-FN" pkg)
               (intern "-INTERNAL-FN" pkg)
               ;; Set function bindings
               (setf (symbol-function (find-symbol "PUBLIC-FN" pkg))
                     (lambda () nil))
               (setf (symbol-function (find-symbol "%PRIVATE-FN" pkg))
                     (lambda () nil))
               (setf (symbol-function (find-symbol "-INTERNAL-FN" pkg))
                     (lambda () nil)))
             ;; Sync without explicit exports - auto-export should apply
             (loader:sync-package-exports pkg-name)
             ;; PUBLIC-FN should be exported (public name, bound)
             (multiple-value-bind (sym status)
                 (find-symbol "PUBLIC-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :external status)))
             ;; %-prefixed should not be exported
             (multiple-value-bind (sym status)
                 (find-symbol "%PRIVATE-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :internal status)))
             ;; --prefixed should not be exported
             (multiple-value-bind (sym status)
                 (find-symbol "-INTERNAL-FN" pkg-name)
               (declare (ignore sym))
               (assert-true (eq :internal status))))
        ;; Cleanup
        (delete-package pkg)))))

;;; ---------------------------------------------------------------------------
;;; Consolidated Module Loading Tests (IMPL-086)
;;; ---------------------------------------------------------------------------

(deftest test-parse-epsilon-header-present
  "parse-epsilon-header extracts metadata from header"
  ;; Create a temp file with header
  (let ((temp-file (fs:join-paths (namestring (user-homedir-pathname)) "test-header.lisp")))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output :if-exists :supersede)
             (format out ";;;; epsilon-module~%")
             (format out ";;;; :requires (epsilon epsilon.json)~%")
             (format out ";;;; :provides (test-package)~%")
             (format out "~%")
             (format out "(defpackage :test-package (:use :cl))~%"))
           (let ((header (loader:parse-epsilon-header temp-file)))
             (assert-true (not (null header)))
             ;; Compare symbol names since symbols may be in different packages
             (let ((requires (getf header :requires))
                   (provides (getf header :provides)))
               (assert-true (= 2 (length requires)))
               (assert-true (string-equal "EPSILON" (symbol-name (first requires))))
               (assert-true (string-equal "EPSILON.JSON" (symbol-name (second requires))))
               (assert-true (= 1 (length provides)))
               (assert-true (string-equal "TEST-PACKAGE" (symbol-name (first provides)))))))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-parse-epsilon-header-absent
  "parse-epsilon-header returns NIL when no header"
  (let ((temp-file (fs:join-paths (namestring (user-homedir-pathname)) "test-no-header.lisp")))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output :if-exists :supersede)
             (format out "(defpackage :test-package (:use :cl))~%")
             (format out "(in-package :test-package)~%"))
           (assert-true (null (loader:parse-epsilon-header temp-file))))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-has-epsilon-header-p
  "has-epsilon-header-p detects header presence"
  (let ((with-header (fs:join-paths (namestring (user-homedir-pathname)) "test-with-hdr.lisp"))
        (without-header (fs:join-paths (namestring (user-homedir-pathname)) "test-without-hdr.lisp")))
    (unwind-protect
         (progn
           ;; Create file with header
           (with-open-file (out with-header :direction :output :if-exists :supersede)
             (format out ";;;; epsilon-module~%")
             (format out ";;;; :requires (epsilon)~%"))
           ;; Create file without header
           (with-open-file (out without-header :direction :output :if-exists :supersede)
             (format out "(defpackage :test (:use :cl))~%"))
           (assert-true (loader:has-epsilon-header-p with-header))
           (assert-true (not (loader:has-epsilon-header-p without-header))))
      (when (probe-file with-header) (delete-file with-header))
      (when (probe-file without-header) (delete-file without-header)))))

(deftest test-extract-defpackage-info
  "extract-defpackage-info extracts info from defpackage form"
  (let ((temp-file (fs:join-paths (namestring (user-homedir-pathname)) "test-defpkg.lisp")))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output :if-exists :supersede)
             (format out "(defpackage :my-utils~%")
             (format out "  (:use :cl)~%")
             (format out "  (:local-nicknames (:json :epsilon.json))~%")
             (format out "  (:export #:foo #:bar))~%")
             (format out "(in-package :my-utils)~%"))
           (let ((info (loader:extract-defpackage-info temp-file)))
             (assert-true (equal "MY-UTILS" (loader:unified-source-info-package-name info)))
             (assert-true (eq :lisp (loader:unified-source-info-file-type info)))
             (assert-true (member "CL" (loader:unified-source-info-use info) :test #'string-equal))))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-extract-unified-source-info-lisp-no-header
  "extract-unified-source-info handles plain .lisp files"
  (let ((temp-file (fs:join-paths (namestring (user-homedir-pathname)) "test-plain.lisp")))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output :if-exists :supersede)
             (format out "(defpackage :plain-pkg (:use :cl))~%")
             (format out "(in-package :plain-pkg)~%"))
           (let ((info (loader:extract-unified-source-info temp-file)))
             (assert-true (not (null info)))
             (assert-true (eq :lisp (loader:unified-source-info-file-type info)))
             (assert-true (not (loader:unified-source-info-header-p info)))))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-extract-unified-source-info-lisp-with-header
  "extract-unified-source-info handles .lisp files with header"
  (let ((temp-file (fs:join-paths (namestring (user-homedir-pathname)) "test-header2.lisp")))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output :if-exists :supersede)
             (format out ";;;; epsilon-module~%")
             (format out ";;;; :requires (epsilon)~%")
             (format out ";;;; :provides (header-pkg)~%")
             (format out "~%")
             (format out "(defpackage :header-pkg (:use :cl))~%"))
           (let ((info (loader:extract-unified-source-info temp-file)))
             (assert-true (not (null info)))
             (assert-true (eq :lisp (loader:unified-source-info-file-type info)))
             (assert-true (loader:unified-source-info-header-p info))
             (assert-true (member "EPSILON"
                         (loader:unified-source-info-requires info)
                         :test #'string-equal))))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-unified-source-info-struct
  "unified-source-info struct holds all necessary metadata"
  (let ((info (loader:make-unified-source-info
               :uri "/path/to/file.lisp"
               :package-name "TEST-PKG"
               :file-type :lisp
               :requires '("DEP1" "DEP2")
               :provides '("TEST-PKG")
               :imports '((nick . pkg))
               :exports '("FOO" "BAR")
               :use '("CL")
               :shadow nil
               :header-p t)))
    (assert-true (equal "/path/to/file.lisp" (loader:unified-source-info-uri info)))
    (assert-true (equal "TEST-PKG" (loader:unified-source-info-package-name info)))
    (assert-true (eq :lisp (loader:unified-source-info-file-type info)))
    (assert-true (= 2 (length (loader:unified-source-info-requires info))))
    (assert-true (loader:unified-source-info-header-p info))))

;;; ---------------------------------------------------------------------------
;;; Scan Module Directory Tests
;;; ---------------------------------------------------------------------------

(defun %test-packages-path ()
  "Return the absolute path to the test-packages fixture directory."
  (let ((epsilon-home (epsilon.sys.env:getenv "EPSILON_HOME")))
    (when epsilon-home
      (let ((dir (merge-pathnames "tests/test-packages/"
                                  (if (char= (char epsilon-home (1- (length epsilon-home))) #\/)
                                      epsilon-home
                                      (concatenate 'string epsilon-home "/")))))
        (namestring dir)))))

(deftest test-scan-module-directory-finds-modules
  "scan-module-directory discovers modules in subdirectories"
  (let ((test-dir (%test-packages-path)))
    (when test-dir
      (let ((env (loader:make-build-environment)))
        (loader:scan-module-directory env test-dir)
        ;; Both myapp and mylib should be registered
        (let ((modules (loader:modules env)))
          (assert-true (epsilon.map:get modules "myapp")
                       "myapp should be registered")
          (assert-true (epsilon.map:get modules "mylib")
                       "mylib should be registered"))))))

(deftest test-scan-module-directory-returns-count
  "scan-module-directory returns the number of subdirectories scanned"
  (let ((test-dir (%test-packages-path)))
    (when test-dir
      (let* ((env (loader:make-build-environment))
             (count (loader:scan-module-directory env test-dir)))
        (assert-true (= 2 count)
                     (format nil "Expected 2 subdirectories scanned, got ~A" count))))))

(deftest test-scan-module-directory-nonexistent
  "scan-module-directory returns 0 for nonexistent path"
  (let* ((env (loader:make-build-environment))
         (count (loader:scan-module-directory env "/tmp/nonexistent-dir-for-epsilon-test")))
    (assert-true (= 0 count)
                 (format nil "Expected 0 for nonexistent dir, got ~A" count))))

;;; ---------------------------------------------------------------------------
;;; Unified Source Info and Build Order Tests
;;; ---------------------------------------------------------------------------

(deftest test-unified-build-order-simple
  "unified-build-order sorts sources by dependencies"
  (let ((sources (list
                  (loader:make-unified-source-info
                   :uri "/a.lisp"
                   :package-name "PKG-A"
                   :provides '("PKG-A")
                   :requires '("PKG-B"))
                  (loader:make-unified-source-info
                   :uri "/b.lisp"
                   :package-name "PKG-B"
                   :provides '("PKG-B")
                   :requires nil))))
    (multiple-value-bind (sorted cycles)
        (loader:unified-build-order sources)
      (assert-true (null cycles))
      (assert-true (= 2 (length sorted)))
      ;; B should come before A (A depends on B)
      (assert-true (string= "/b.lisp"
                   (loader:unified-source-info-uri (first sorted))))
      (assert-true (string= "/a.lisp"
                   (loader:unified-source-info-uri (second sorted)))))))
