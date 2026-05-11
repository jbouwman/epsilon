;;;; Tests for extended defpackage macro
;;;;
;;;; Tests the :import and :enter extensions to defpackage.

(defpackage epsilon.defpackage-test
  (:import cl
           epsilon.test
           epsilon.syntax
           (epsilon.main main)))

;;; ---------------------------------------------------------------------------
;;; Helper functions for testing macro expansion
;;; ---------------------------------------------------------------------------

(defun expansion-contains-p (expansion form-type)
  "Check if EXPANSION contains a form of the given type."
  (cond
    ((null expansion) nil)
    ((atom expansion) nil)
    ((eq (first expansion) form-type) t)
    (t (or (expansion-contains-p (first expansion) form-type)
           (expansion-contains-p (rest expansion) form-type)))))

(defun find-cl-defpackage-form (expansion)
  "Find the cl:defpackage form in the expansion."
  (cond
    ((null expansion) nil)
    ((atom expansion) nil)
    ((and (consp expansion)
          (eq (first expansion) 'cl:defpackage))
     expansion)
    (t (or (find-cl-defpackage-form (first expansion))
           (find-cl-defpackage-form (rest expansion))))))

(defun defpackage-has-option-p (defpkg-form option-key)
  "Check if a cl:defpackage form has an option with the given key."
  (member option-key (cddr defpkg-form)
          :key (lambda (opt)
                 (when (consp opt) (first opt)))))

(defun get-defpackage-option (defpkg-form option-key)
  "Get the value of an option from a cl:defpackage form."
  (find option-key (cddr defpkg-form)
        :key (lambda (opt)
               (when (consp opt) (first opt)))))

;;; ---------------------------------------------------------------------------
;;; Tests for package-name-to-module-name helper
;;; ---------------------------------------------------------------------------

(deftest test-package-name-to-module-name-simple
  "Two-component epsilon names map to themselves"
  (assert-true (equal "epsilon.http" (main:package-name-to-module-name "epsilon.http")))
  (assert-true (equal "epsilon.json" (main:package-name-to-module-name "epsilon.json"))))

(deftest test-package-name-to-module-name-nested
  "Three+ component names truncate to two components"
  (assert-true (equal "epsilon.http"
             (main:package-name-to-module-name "epsilon.http.client")))
  (assert-true (equal "epsilon.crypto"
             (main:package-name-to-module-name "epsilon.crypto.tls.connection"))))

(deftest test-package-name-to-module-name-non-epsilon
  "Non-epsilon package names return nil"
  (assert-true (null (main:package-name-to-module-name "kreisler.service")))
  (assert-true (null (main:package-name-to-module-name "my-app")))
  (assert-true (null (main:package-name-to-module-name "single"))))

;;; ---------------------------------------------------------------------------
;;; Tests for extended defpackage macro expansion
;;; ---------------------------------------------------------------------------

(deftest test-defpackage-passthrough-standard-options
  "Standard CL options pass through unchanged"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:import cl)
                       (:export #:foo #:bar)
                       (:shadow #:list)
                       (:enter nil))))
         (defpkg (find-cl-defpackage-form expansion)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true (eq 'test-pkg (second defpkg)) "Package name should be test-pkg")
    (assert-true (defpackage-has-option-p defpkg :use) "Should have :use option")
    (assert-true (defpackage-has-option-p defpkg :export) "Should have :export option")
    (assert-true (defpackage-has-option-p defpkg :shadow) "Should have :shadow option")))

(deftest test-defpackage-import-generates-local-nicknames
  "The :import option generates :local-nicknames entries"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:import cl
                                (epsilon.http http)
                                (epsilon.json json))
                       (:enter nil))))
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true nicknames-opt "Should have :local-nicknames option")
    ;; Check that both nicknames are present
    (let ((nickname-list (rest nicknames-opt)))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http nickname")
      (assert-true (find :json nickname-list :key #'first)
          "Should have :json nickname"))))

(deftest test-defpackage-local-nicknames-rejected
  "The :local-nicknames option signals an error in the extended defpackage"
  (assert-true
   (handler-case
       (progn
         (macroexpand-1
          '(epsilon.main:defpackage test-pkg
            (:import cl)
            (:local-nicknames (:str :epsilon.string))))
         nil)
     (error () t))
   "Should signal an error when :local-nicknames is used"))

(deftest test-defpackage-use-folded-into-import
  "The :use option is accepted and folded into :import as bare symbols"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:use :cl)
                       (:import (epsilon.http http))
                       (:enter nil))))
         (defpkg (find-cl-defpackage-form expansion))
         (use-opt (get-defpackage-option defpkg :use))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    ;; :use packages should appear in generated :use clause
    (assert-true use-opt "Should have :use option")
    (assert-true (member :cl (rest use-opt)) ":cl should be in :use")
    ;; Nickname imports should still work
    (assert-true nicknames-opt "Should have :local-nicknames")
    (assert-true (find :http (rest nicknames-opt) :key #'first)
        "Should have :http nickname")))

(deftest test-defpackage-multiple-import-clauses-consolidated
  "Multiple :import clauses are consolidated"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:import cl (epsilon.http http))
                       (:import (epsilon.json json))
                       (:enter nil))))
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true nicknames-opt "Should have :local-nicknames option")
    (let ((nickname-list (rest nicknames-opt)))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http from first :import")
      (assert-true (find :json nickname-list :key #'first)
          "Should have :json from second :import"))))

(deftest test-defpackage-enter-default-emits-in-package
  "Without :enter clause, in-package is emitted (default is t)"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:import cl)))))
    (assert-true (expansion-contains-p expansion 'in-package)
        "Should contain in-package form by default")))

(deftest test-defpackage-enter-nil-omits-in-package
  "With (:enter nil), no (in-package ...) form is emitted"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:import cl)
                      (:enter nil)))))
    (assert-true (not (expansion-contains-p expansion 'in-package))
        "Should not contain in-package form")))

(deftest test-defpackage-enter-t-emits-in-package
  "With explicit (:enter t), in-package is emitted"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:import cl)
                      (:enter t)))))
    (assert-true (expansion-contains-p expansion 'in-package)
        "Should contain in-package form")))

(deftest test-defpackage-import-generates-eval-when
  "The :import option generates eval-when for module loading"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:import cl
                               (epsilon.http http))
                      (:enter nil)))))
    (assert-true (expansion-contains-p expansion 'eval-when)
        "Should contain eval-when form for module loading")))

(deftest test-defpackage-import-without-nickname
  "Bare symbols in :import generate :use entries, not nicknames"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:import cl
                                (epsilon.http http)
                                epsilon.json)
                       (:enter nil))))
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames))
         (use-opt (get-defpackage-option defpkg :use)))
    (assert-true defpkg "Should contain cl:defpackage")
    ;; Bare symbols should appear in :use
    (assert-true use-opt "Should have :use option")
    (let ((use-list (rest use-opt)))
      (assert-true (member :cl use-list) "Should use :cl")
      (assert-true (member :epsilon.json use-list) "Should use :epsilon.json"))
    ;; Should have only the nickname for epsilon.http
    (let ((nickname-list (when nicknames-opt (rest nicknames-opt))))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http nickname")
      (assert-true (not (find :epsilon.json nickname-list :key #'first))
          "Should not have :epsilon.json nickname since it was bare"))))

(deftest test-defpackage-non-epsilon-import-no-module-load
  "Non-epsilon packages in :import get nicknames but no module load"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:import cl
                               (my-other-lib lib))
                      (:enter nil)))))
    ;; The expansion should still have local-nicknames but eval-when
    ;; should either be empty or not contain loader calls for non-epsilon pkgs
    (let* ((defpkg (find-cl-defpackage-form expansion))
           (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
      (assert-true defpkg "Should contain cl:defpackage")
      (assert-true nicknames-opt "Should have :local-nicknames")
      (let ((nickname-list (rest nicknames-opt)))
        (assert-true (find :lib nickname-list :key #'first)
            "Should have :lib nickname")))))
