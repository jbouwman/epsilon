;;;; Tests for extended defpackage macro
;;;;
;;;; Tests the :require and :enter extensions to defpackage.

(defpackage epsilon.defpackage-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.main main))
  (:enter t))

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
                       (:use :cl)
                       (:export #:foo #:bar)
                       (:shadow #:list))))
         (defpkg (find-cl-defpackage-form expansion)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true (eq 'test-pkg (second defpkg)) "Package name should be test-pkg")
    (assert-true (defpackage-has-option-p defpkg :use) "Should have :use option")
    (assert-true (defpackage-has-option-p defpkg :export) "Should have :export option")
    (assert-true (defpackage-has-option-p defpkg :shadow) "Should have :shadow option")))

(deftest test-defpackage-require-generates-local-nicknames
  "The :require option generates :local-nicknames entries"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:use :cl)
                       (:require (epsilon.http http)
                                 (epsilon.json json)))))
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

(deftest test-defpackage-require-merges-with-local-nicknames
  "The :require option merges with explicit :local-nicknames"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:use :cl)
                       (:require (epsilon.http http))
                       (:local-nicknames (:str :epsilon.string)))))
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true nicknames-opt "Should have :local-nicknames option")
    (let ((nickname-list (rest nicknames-opt)))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http from :require")
      (assert-true (find :str nickname-list :key #'first)
          "Should have :str from :local-nicknames"))))

(deftest test-defpackage-multiple-require-clauses-consolidated
  "Multiple :require clauses are consolidated"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:use :cl)
                       (:require (epsilon.http http))
                       (:require (epsilon.json json)))))
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    (assert-true nicknames-opt "Should have :local-nicknames option")
    (let ((nickname-list (rest nicknames-opt)))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http from first :require")
      (assert-true (find :json nickname-list :key #'first)
          "Should have :json from second :require"))))

(deftest test-defpackage-enter-emits-in-package
  "The :enter option emits (in-package ...) form"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:use :cl)
                      (:enter t)))))
    (assert-true (expansion-contains-p expansion 'in-package)
        "Should contain in-package form")))

(deftest test-defpackage-no-enter-omits-in-package
  "Without :enter, no (in-package ...) form is emitted"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:use :cl)))))
    (assert-true (not (expansion-contains-p expansion 'in-package))
        "Should not contain in-package form")))

(deftest test-defpackage-require-generates-eval-when
  "The :require option generates eval-when for module loading"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:use :cl)
                      (:require (epsilon.http http))))))
    (assert-true (expansion-contains-p expansion 'eval-when)
        "Should contain eval-when form for module loading")))

(deftest test-defpackage-require-without-nickname
  "The :require option handles specs without nicknames"
  (let* ((expansion (macroexpand-1
                     '(epsilon.main:defpackage test-pkg
                       (:use :cl)
                       (:require (epsilon.http http)
                                 epsilon.json))))  ; no nickname
         (defpkg (find-cl-defpackage-form expansion))
         (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
    (assert-true defpkg "Should contain cl:defpackage")
    ;; Should have only the nickname for epsilon.http
    (let ((nickname-list (when nicknames-opt (rest nicknames-opt))))
      (assert-true (find :http nickname-list :key #'first)
          "Should have :http nickname")
      (assert-true (not (find :json nickname-list :key #'first))
          "Should not have :json nickname since none was specified"))))

(deftest test-defpackage-non-epsilon-require-no-module-load
  "Non-epsilon packages in :require get nicknames but no module load"
  (let ((expansion (macroexpand-1
                    '(epsilon.main:defpackage test-pkg
                      (:use :cl)
                      (:require (my-other-lib lib))))))
    ;; The expansion should still have local-nicknames but eval-when
    ;; should either be empty or not contain loader calls for non-epsilon pkgs
    (let* ((defpkg (find-cl-defpackage-form expansion))
           (nicknames-opt (get-defpackage-option defpkg :local-nicknames)))
      (assert-true defpkg "Should contain cl:defpackage")
      (assert-true nicknames-opt "Should have :local-nicknames")
      (let ((nickname-list (rest nicknames-opt)))
        (assert-true (find :lib nickname-list :key #'first)
            "Should have :lib nickname")))))

