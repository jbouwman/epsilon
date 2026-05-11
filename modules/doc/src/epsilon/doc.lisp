;;;; epsilon.doc -- Documentation extraction, indexing, and structured docstring parsing
;;;;
;;;; Provides tools for extracting machine-readable documentation from Epsilon
;;;; Lisp code.  Parses structured docstrings with keyword sections (:param,
;;;; :returns, :signals, :see, :example, :since, :deprecated, :note), merges
;;;; with #@ annotation metadata, and emits documentation in JSON or
;;;; S-expression format.

(defpackage epsilon.doc
  (:use :cl)
  (:import (epsilon.doc.parse parse)
           (epsilon.doc.extract extract)
           (epsilon.doc.emit emit)
           (epsilon.doc.xref xref)
           (epsilon.doc.coverage coverage))
  (:export
   ;; Docstring parsing
   #:parse-docstring

   ;; Symbol and module extraction
   #:describe-symbol
   #:describe-module
   #:describe-package

   ;; Annotation merging
   #:merge-annotations

   ;; Cross-reference index
   #:build-xref-index
   #:build-full-xref-index
   #:xref-lookup
   #:extract-see-refs
   #:parse-impl-file-refs
   #:generate-xref-json
   #:validate-xref
   #:scan-impl-docs

   ;; Emission
   #:emit

   ;; Coverage
   #:module-coverage
   #:package-coverage))

;;; Re-exports via fdefinition

(setf (fdefinition 'parse-docstring) #'parse:parse-docstring)
(setf (fdefinition 'merge-annotations) #'parse:merge-annotations)
(setf (fdefinition 'describe-symbol) #'extract:describe-symbol)
(setf (fdefinition 'describe-module) #'extract:describe-module)
(setf (fdefinition 'describe-package) #'extract:describe-package)
(setf (fdefinition 'emit) #'emit:emit)
(setf (fdefinition 'build-xref-index) #'xref:build-xref-index)
(setf (fdefinition 'xref-lookup) #'xref:xref-lookup)
(setf (fdefinition 'extract-see-refs) #'xref:extract-see-refs)
(setf (fdefinition 'parse-impl-file-refs) #'xref:parse-impl-file-refs)
(setf (fdefinition 'build-full-xref-index) #'xref:build-full-xref-index)
(setf (fdefinition 'generate-xref-json) #'xref:generate-xref-json)
(setf (fdefinition 'validate-xref) #'xref:validate-xref)
(setf (fdefinition 'scan-impl-docs) #'xref:scan-impl-docs)
(setf (fdefinition 'module-coverage) #'coverage:module-coverage)
(setf (fdefinition 'package-coverage) #'coverage:package-coverage)
