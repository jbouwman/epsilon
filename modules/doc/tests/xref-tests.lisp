;;;; Tests for epsilon.doc.xref -- cross-reference index

(defpackage epsilon.doc.xref-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.xref xref)
           (epsilon.annotate ann)))

;;; Test fixtures: symbols with :see annotations

(defpackage epsilon.doc.xref-tests.fixtures
  (:use :cl)
  (:export #:impl-tagged-fn #:impl-tagged-fn-2 #:untagged-fn))

(in-package :epsilon.doc.xref-tests.fixtures)

(defun impl-tagged-fn ()
  "A function that references IMPL-297.

   :see IMPL-297"
  nil)

(defun impl-tagged-fn-2 ()
  "Another function.

   :see IMPL-297, IMPL-100"
  nil)

(defun untagged-fn ()
  "No IMPL references here."
  nil)

(in-package :epsilon.doc.xref-tests)

;; Set up annotations on the fixtures
(ann:set-annotations 'epsilon.doc.xref-tests.fixtures::impl-tagged-fn
                     '((:see . "IMPL-297")))

;;;; extract-see-refs tests

(deftest test-extract-see-refs-from-docstring
  "extract-see-refs finds IMPL references in docstrings."
  (let ((refs (xref:extract-see-refs 'epsilon.doc.xref-tests.fixtures::impl-tagged-fn)))
    (assert-true (member "IMPL-297" refs :test #'string=))))

(deftest test-extract-see-refs-multiple
  "extract-see-refs finds multiple IMPL references."
  (let ((refs (xref:extract-see-refs 'epsilon.doc.xref-tests.fixtures::impl-tagged-fn-2)))
    (assert-true (member "IMPL-297" refs :test #'string=))
    (assert-true (member "IMPL-100" refs :test #'string=))))

(deftest test-extract-see-refs-none
  "extract-see-refs returns NIL for untagged symbols."
  (let ((refs (xref:extract-see-refs 'epsilon.doc.xref-tests.fixtures::untagged-fn)))
    (assert-nil refs)))

(deftest test-extract-see-refs-merges-annotation-and-docstring
  "extract-see-refs merges :see from both docstring and annotations."
  ;; impl-tagged-fn has IMPL-297 in both docstring and annotation
  (let ((refs (xref:extract-see-refs 'epsilon.doc.xref-tests.fixtures::impl-tagged-fn)))
    ;; Should be deduplicated
    (assert-equal 1 (count "IMPL-297" refs :test #'string=))))

;;;; build-xref-index tests

(deftest test-build-xref-index-from-symbols
  "build-xref-index maps IMPL refs to symbols."
  (let ((index (xref:build-xref-index
                 :symbols '(epsilon.doc.xref-tests.fixtures::impl-tagged-fn
                            epsilon.doc.xref-tests.fixtures::impl-tagged-fn-2
                            epsilon.doc.xref-tests.fixtures::untagged-fn))))
    (assert-not-null index)
    ;; IMPL-297 should map to both tagged functions
    (let ((entries (xref:xref-lookup "IMPL-297" :index index)))
      (assert-not-null entries)
      (assert-true (>= (length entries) 2)))
    ;; IMPL-100 should map to impl-tagged-fn-2
    (let ((entries (xref:xref-lookup "IMPL-100" :index index)))
      (assert-not-null entries)
      (assert-equal 1 (length entries)))))

(deftest test-xref-lookup-nonexistent
  "xref-lookup returns NIL for unknown refs."
  (let ((index (xref:build-xref-index :symbols nil)))
    (assert-nil (xref:xref-lookup "IMPL-999" :index index))))

;;;; parse-impl-file-refs tests

(deftest test-parse-impl-file-refs
  "parse-impl-file-refs extracts file paths from implement doc text."
  (let* ((text (format nil "## Files~%~%**Files:**~%- `epsilon-contrib/doc/src/epsilon/doc.lisp`~%- `epsilon-contrib/doc/tests/parse-tests.lisp`"))
         (refs (xref:parse-impl-file-refs text)))
    (assert-equal 2 (length refs))
    (assert-true (member "epsilon-contrib/doc/src/epsilon/doc.lisp" refs :test #'string=))
    (assert-true (member "epsilon-contrib/doc/tests/parse-tests.lisp" refs :test #'string=))))

(deftest test-parse-impl-file-refs-empty
  "parse-impl-file-refs returns NIL for text without file refs."
  (assert-nil (xref:parse-impl-file-refs "No files here.")))

;;;; extract-impl-number tests

(deftest test-extract-impl-number
  "extract-impl-number parses IMPL number from filename."
  (assert-equal "IMPL-297" (xref:extract-impl-number "297_structured-lisp-documentation.md"))
  (assert-equal "IMPL-006" (xref:extract-impl-number "006_voice-leading-enhancement.md"))
  (assert-nil (xref:extract-impl-number "index.md"))
  (assert-nil (xref:extract-impl-number "archive")))

;;;; scan-impl-docs tests

(deftest test-scan-impl-docs
  "scan-impl-docs finds implement documents."
  (let ((results (xref:scan-impl-docs)))
    ;; Should find many impl docs
    (assert-true (> (length results) 10))
    ;; IMPL-297 should be among them
    (let ((impl-297 (assoc "IMPL-297" results :test #'string=)))
      (assert-not-null impl-297))))

;;;; build-full-xref-index tests

(deftest test-build-full-xref-index
  "build-full-xref-index returns an index (possibly empty)."
  (let ((index (xref:build-full-xref-index)))
    ;; Should be a list (possibly empty)
    (assert-true (listp index))))

;;;; validate-xref tests

(deftest test-validate-xref-returns-structure
  "validate-xref returns a plist with :errors, :warnings, :valid-p."
  (let ((result (xref:validate-xref)))
    (assert-not-null result)
    (assert-true (listp (getf result :errors)))
    (assert-true (listp (getf result :warnings)))
    ;; :valid-p is a boolean
    (assert-true (or (eq t (getf result :valid-p))
                     (null (getf result :valid-p))))))
