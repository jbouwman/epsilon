;;;; epsilon.doc.coverage -- Documentation coverage metrics
;;;;
;;;; Computes per-module documentation coverage: symbol coverage, parameter
;;;; coverage, section coverage, and cross-reference coverage.

(defpackage epsilon.doc.coverage
  (:use :cl)
  (:import (epsilon.doc.parse parse)
           (epsilon.doc.xref xref)
           (epsilon.loader loader))
  (:export
   #:module-coverage
   #:package-coverage))

;;; Package-level coverage

(defun package-coverage (package-designator)
  "Compute documentation coverage metrics for PACKAGE-DESIGNATOR.

   Returns a plist with:
     :total-symbols       -- Total exported symbols.
     :documented-symbols  -- Symbols with docstrings.
     :symbol-coverage     -- Ratio of documented to total (0.0 to 1.0).
     :total-params        -- Total function parameters across all exported functions.
     :documented-params   -- Parameters with :param documentation.
     :parameter-coverage  -- Ratio of documented to total params.
     :xref-symbols        -- Symbols with :see cross-references.
     :xref-coverage       -- Ratio of xref-symbols to total."
  (let ((pkg (find-package package-designator)))
    (unless pkg (return-from package-coverage nil))
    (let ((total 0)
          (documented 0)
          (total-params 0)
          (documented-params 0)
          (xref-count 0))
      (do-external-symbols (sym pkg)
        (incf total)
        (let ((docstring (or (documentation sym 'function)
                             (documentation sym 'variable)
                             (documentation sym 'type))))
          (when docstring
            (incf documented)
            (let ((parsed (parse:parse-docstring docstring)))
              ;; Count params for functions
              (when (fboundp sym)
                (let* ((lambda-list (ignore-errors
                                      (sb-kernel:%fun-lambda-list
                                       (or (macro-function sym)
                                           (symbol-function sym)))))
                       (param-names (extract-param-names lambda-list))
                       (doc-params (getf parsed :params)))
                  (incf total-params (length param-names))
                  (incf documented-params (length doc-params))))
              ;; Count xref
              (when (getf parsed :see)
                (incf xref-count))))
          ;; Count params even for undocumented functions
          (when (and (not docstring) (fboundp sym))
            (let ((lambda-list (ignore-errors
                                 (sb-kernel:%fun-lambda-list
                                  (or (macro-function sym)
                                      (symbol-function sym))))))
              (incf total-params (length (extract-param-names lambda-list)))))))
      (list :total-symbols total
            :documented-symbols documented
            :symbol-coverage (if (zerop total) 1.0 (/ (float documented) total))
            :total-params total-params
            :documented-params documented-params
            :parameter-coverage (if (zerop total-params) 1.0 (/ (float documented-params) total-params))
            :xref-symbols xref-count
            :xref-coverage (if (zerop total) 0.0 (/ (float xref-count) total))))))

(defun extract-param-names (lambda-list)
  "Extract parameter names from LAMBDA-LIST, ignoring &-keywords."
  (when (or (null lambda-list) (not (listp lambda-list)))
    (return-from extract-param-names nil))
  (loop for item in lambda-list
        unless (and (symbolp item)
                    (char= (char (symbol-name item) 0) #\&))
          collect (if (listp item) (car item) item)))

;;; Module-level coverage

(defun module-coverage (module-name)
  "Compute documentation coverage metrics for MODULE-NAME.
   Aggregates coverage across all packages belonging to the module."
  (let ((total-sym 0)
        (doc-sym 0)
        (total-param 0)
        (doc-param 0)
        (xref-sym 0)
        (module-prefix (string-upcase module-name))
        (packages-found nil))
    (dolist (pkg (list-all-packages))
      (let ((pkg-name (package-name pkg)))
        (when (or (string= pkg-name module-prefix)
                  (and (> (length pkg-name) (length module-prefix))
                       (string= pkg-name module-prefix
                                :end1 (length module-prefix))
                       (char= (char pkg-name (length module-prefix)) #\.)))
          (push pkg-name packages-found)
          (let ((cov (package-coverage pkg-name)))
            (when cov
              (incf total-sym (getf cov :total-symbols))
              (incf doc-sym (getf cov :documented-symbols))
              (incf total-param (getf cov :total-params))
              (incf doc-param (getf cov :documented-params))
              (incf xref-sym (getf cov :xref-symbols)))))))
    (list :total-symbols total-sym
          :documented-symbols doc-sym
          :symbol-coverage (if (zerop total-sym) 1.0 (/ (float doc-sym) total-sym))
          :total-params total-param
          :documented-params doc-param
          :parameter-coverage (if (zerop total-param) 1.0 (/ (float doc-param) total-param))
          :xref-symbols xref-sym
          :xref-coverage (if (zerop total-sym) 0.0 (/ (float xref-sym) total-sym))
          :packages packages-found)))
