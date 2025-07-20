;;;; Tests for Enhanced Module System 2.0
;;;;
;;;; This file tests the new module macro and its various features including
;;;; modern import/export syntax, conditional compilation, and backward compatibility.

(defpackage :epsilon.module.tests
  (:use 
   :cl 
   :epsilon.test)
  (:local-nicknames
   (:module :epsilon.module)))

(in-package :epsilon.module.tests)

(deftest module-basic-syntax
  "Test basic module syntax with simple use and export"
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :export (foo bar)))))
    (is (eq (first form) 'progn))
    (let ((defpackage-form (second form))
          (in-package-form (third form)))
      (is (eq (first defpackage-form) 'defpackage))
      (is (string= (second defpackage-form) "TEST-PACKAGE"))
      (is (eq (first in-package-form) 'in-package))
      (is (string= (second in-package-form) "TEST-PACKAGE")))))

(deftest module-import-aliases
  "Test import with local nicknames"
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :import ((map epsilon.map)
                           (str epsilon.string))
                  :export (process-data)))))
    (let ((defpackage-form (second form)))
      (let ((local-nicknames-clause 
             (find-if (lambda (clause) 
                        (and (consp clause) 
                             (eq (first clause) :local-nicknames)))
                      (cddr defpackage-form))))
        (is (not (null local-nicknames-clause)))
        (is (member '(map epsilon.map) (rest local-nicknames-clause) :test #'equal))
        (is (member '(str epsilon.string) (rest local-nicknames-clause) :test #'equal))))))

(deftest module-selective-imports
  "Test selective symbol imports using 'from' syntax"
  (skip)
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :import (((make-map get assoc) from epsilon.map)
                           ((split join) from epsilon.string))
                  :export (process-data)))))
    (let ((defpackage-form (second form)))
      ;; Check for import-from clauses
      (let ((import-clauses 
             (remove-if-not (lambda (clause) 
                              (and (consp clause) 
                                   (eq (first clause) :import-from)))
                            (cddr defpackage-form))))
        (is (= (length import-clauses) 2))
        (is (member '(:import-from epsilon.map make-map get assoc) 
                   import-clauses :test #'equal))
        (is (member '(:import-from epsilon.string split join) 
                   import-clauses :test #'equal))))))

(deftest module-shadow-symbols
  "Test symbol shadowing"
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :shadow (map reduce filter)
                  :export (my-map)))))
    (let ((defpackage-form (second form)))
      (let ((shadow-clause 
             (find-if (lambda (clause) 
                        (and (consp clause) 
                             (eq (first clause) :shadow)))
                      (cddr defpackage-form))))
        (is (not (null shadow-clause)))
        (is (equal (rest shadow-clause) '(map reduce filter)))))))

(deftest module-conditional-compilation
  "Test conditional compilation with :when clause"
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :when (featurep :test-mode)
                  :export (debug-function)))))
    (is (eq (first form) 'when))
    (is (equal (second form) '(featurep :test-mode)))
    (let ((defpackage-form (third form))
          (in-package-form (fourth form)))
      (is (eq (first defpackage-form) 'defpackage))
      (is (eq (first in-package-form) 'in-package)))))

(deftest module-mixed-imports
  "Test combination of aliased and selective imports"
  (skip)
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl epsilon.syntax)
                  :import ((map epsilon.map)
                           ((split join trim) from epsilon.string)
                           (json epsilon.json))
                  :export (process-text)))))
    (let ((defpackage-form (second form)))
      ;; Check local nicknames
      (let ((local-nicknames-clause 
             (find-if (lambda (clause) 
                        (and (consp clause) 
                             (eq (first clause) :local-nicknames)))
                      (cddr defpackage-form))))
        (is (member '(map epsilon.map) (rest local-nicknames-clause) :test #'equal))
        (is (member '(json epsilon.json) (rest local-nicknames-clause) :test #'equal)))
      
      ;; Check import-from
      (let ((import-clauses 
             (remove-if-not (lambda (clause) 
                              (and (consp clause) 
                                   (eq (first clause) :import-from)))
                            (cddr defpackage-form))))
        (is (member '(:import-from epsilon.string split join trim) 
                   import-clauses :test #'equal))))))

(deftest module-re-export
  "Test re-export functionality"
  (let ((form (macroexpand-1 
               '(module:module test-package
                  :use (cl)
                  :import ((config my-app.config))
                  :export (start-server)
                  :re-export (config-setting get-env)))))
    (let ((defpackage-form (second form)))
      (let ((export-clause 
             (find-if (lambda (clause) 
                        (and (consp clause) 
                             (eq (first clause) :export)))
                      (cddr defpackage-form))))
        (is (member 'start-server (rest export-clause)))
        (is (member 'config-setting (rest export-clause)))
        (is (member 'get-env (rest export-clause)))))))

(deftest backward-compatibility-defmodule
  "Test that defmodule still works for backward compatibility"
  (let ((form (macroexpand-1 
               '(module:defmodule test-package
                  :use (cl)
                  :export (old-function)))))
    (is (eq (first form) 'module:module))
    (is (eq (second form) 'test-package))))

(deftest module-error-handling
  "Test error handling for invalid import specifications"
  (is-thrown (error)
    (macroexpand-1 
     '(module:module test-package
        :import (invalid-import-spec-with-too-many-elements)))))

;; Integration test with actual package creation
(deftest module-integration-test
  "Test that the module macro actually creates working packages"
  (skip)
  ;; Clean up any existing package
  (when (find-package "TEST-INTEGRATION")
    (delete-package "TEST-INTEGRATION"))
  
  ;; Create a test module
  (eval '(module:module test-integration
           :use (cl)
           :import ((seq epsilon.sequence))
           :export (test-function)))
  
  ;; Verify the package was created
  (let ((pkg (find-package "TEST-INTEGRATION")))
    (is (not (null pkg)))
    (is (string= (package-name pkg) "TEST-INTEGRATION"))
    
    ;; Check basic package properties
    (is (member 'cl (package-use-list pkg) :key #'package-name :test #'string=))
    
    ;; Clean up carefully
    (when pkg
      (unuse-package (package-use-list pkg) pkg)
      (delete-package pkg))))

;; Performance test
(deftest module-macro-performance
  "Test that module macro expansion is reasonably fast"
  (skip)
  (let ((start-time (get-internal-real-time)))
    ;; Expand the macro 1000 times
    (dotimes (i 1000)
      (macroexpand-1 
       '(module:module test-package
          :use (cl epsilon.syntax)
          :import ((map epsilon.map)
                   (str epsilon.string)
                   ((split join) from epsilon.text))
          :shadow (reduce filter)
          :export (main process-data start-server))))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      ;; Should complete 1000 expansions in under 1 second
      (is (< elapsed 1.0))
      (format t "~&Module macro performance: ~D expansions in ~,3F seconds~%" 
              1000 elapsed))))
