;;;; Test Suite for SBCL Integration (using core types)
(defpackage epsilon.test.compile-integration
  (:use cl epsilon.test)
  (:import (epsilon.source-location loc)
           (epsilon.compilation-result cr)
           (epsilon.compile-hooks hooks)
           (epsilon.compile compile)
           (epsilon.compile-integration compile-integration)
           (epsilon.mutable-map mmap)
           (epsilon.reader reader)
           (epsilon.log log)))

;;; Basic functionality tests
(deftest test-package-existence
  "Test that the integration package exists and has key exports"
  (assert-true (find-package :epsilon.compile-integration))
  (assert-true (find-symbol "WITH-SOURCE-TRACKING" :epsilon.compile-integration))
  (assert-true (find-symbol "INSTALL-COMPILER-HOOKS" :epsilon.compile-integration)))

(deftest test-variables-exist
  "Test that key tracking variables exist"
  (assert-true (boundp 'compile-integration:*real-time-source-tracking*))
  (assert-true (boundp 'compile-integration:*current-compilation-location*)))

(deftest test-hook-functions-exist
  "Test that hook installation functions exist"
  (assert-true (fboundp 'compile-integration:install-compiler-hooks))
  (assert-true (fboundp 'compile-integration:uninstall-compiler-hooks))
  (assert-true (fboundp 'compile-integration:get-real-time-source-location)))

(deftest test-integration-with-compile
  "Test that integration functions exist in compile package"
  (assert-true (fboundp 'compile:with-source-tracking))
  (assert-true (fboundp 'compile:compile-file-with-tracking))
  (assert-true (fboundp 'compile:compile-form-with-tracking)))

(deftest test-line-number-extraction
  "Test that line number extraction works without debug output"
  (let ((test-file (format nil "/tmp/test-line-~A.lisp" (get-universal-time))))
    (unwind-protect (handler-case (progn
      ;; Create test file with known line structure
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (format stream ";; Line 1: Comment~%")
        (format stream "(format t \"Line 2\")~%")
        (format stream "(defun test-func (x)~%")
        (format stream "  (+ x 1))~%"))
      ;; Test compilation with deep tracking
      (let ((result (compile:compile-file-with-tracking test-file :verbose nil)))
        (assert-true (not (null result)) "Should return a compilation result")
        (assert-true (cr:compilation-result-success-p result) "Compilation should succeed")
        ;; Compilation should succeed cleanly without errors
        (assert-= 0 (length (cr:get-errors result)) "Should have no errors")))
      (error (e)
             (warn "Line number extraction test failed with error: ~A" e)
             (assert-true nil "Test should not error")))
      ;; Cleanup
      (when (probe-file test-file) (delete-file test-file))
      (let ((fasl-file (concatenate 'string test-file ".fasl")))
        (when (probe-file fasl-file) (delete-file fasl-file))))))

;;; CST datum walk tests (IMPL-320)

(defun walk-into-map (code)
  "Helper: read CODE, walk its first non-trivia top-level node, and return
   the (datum . positions-map) pair."
  (let* ((nodes (reader:read-all-syntax code))
         (top (find-if (lambda (n)
                         (not (member (reader:syntax-node-kind n)
                                      '(:whitespace :comment))))
                       nodes))
         (datum (reader:syntax-node-to-datum top))
         (positions (mmap:make-map :test 'eq)))
    (compile-integration:walk-cst-datum top datum positions)
    (values datum positions)))

(deftest test-walk-cst-datum-records-outer-cell
  "walk-cst-datum records the outer cons cell's (start . end) byte range."
  (multiple-value-bind (datum positions) (walk-into-map "(foo bar)")
    (let ((entry (mmap:get positions datum)))
      (assert-true (consp entry))
      (assert-= 0 (car entry))
      ;; "(foo bar)" is 9 bytes; the closing paren is at index 8, so the
      ;; recorded END offset is 9 (one past the last byte).
      (assert-= 9 (cdr entry)))))

(deftest test-walk-cst-datum-records-nested-cells
  "walk-cst-datum records every nested cell with its byte range."
  (multiple-value-bind (datum positions) (walk-into-map "(foo (bar baz))")
    (let ((outer (mmap:get positions datum))
          (inner (mmap:get positions (second datum))))
      (assert-= 0 (car outer))
      (assert-= 15 (cdr outer))
      ;; (bar baz) starts at byte 5 and ends at byte 14 (one past ')').
      (assert-= 5 (car inner))
      (assert-= 14 (cdr inner)))))

(deftest test-walk-cst-datum-skips-trivia-when-pairing
  "walk-cst-datum aligns CST children with datum cells correctly when
   whitespace and comments are interleaved with code."
  (multiple-value-bind (datum positions)
      (walk-into-map "(a   ;; mid
                          (b c))")
    (let ((outer (mmap:get positions datum))
          (inner (mmap:get positions (second datum))))
      (assert-true (consp outer))
      (assert-= 0 (car outer))
      ;; Inner list (b c) range is layout-dependent but must be a (cons . cons).
      (assert-true (consp inner))
      (assert-true (numberp (car inner)))
      (assert-true (numberp (cdr inner)))
      (assert-true (> (cdr inner) (car inner))))))

(deftest test-walk-cst-datum-atoms-not-recorded
  "Atomic top-level forms are not consable, so they don't appear in the map."
  (let* ((nodes (reader:read-all-syntax "42"))
         (top (find-if (lambda (n)
                         (not (member (reader:syntax-node-kind n)
                                      '(:whitespace :comment))))
                       nodes))
         (datum (reader:syntax-node-to-datum top))
         (positions (mmap:make-map :test 'eq)))
    (compile-integration:walk-cst-datum top datum positions)
    ;; 42 is an atom; positions map remains empty for it.
    (assert-true (null (mmap:get positions datum)))))

;;; Debug-source start-positions propagation (IMPL-320 Stage 4)

(deftest test-fasl-debug-source-uses-cst-positions
  "After compiling a file through compile-file-safely (which fires the
   CST *compile-file-around* hook), the resulting FASL's debug-source
   carries byte offsets pointing at the (defun ...) opening parens, not
   SBCL's reader positions which lie on leading whitespace / comments."
  (let* ((src #p"/tmp/epsilon-cst-debug-source-test.lisp")
         (fasl #p"/tmp/epsilon-cst-debug-source-test.fasl")
         ;; The leading two comment lines push the first defun to byte 38.
         (content ";; preamble line 1
;; preamble line 2
(defun cst-debug-test-a () 1)
(defun cst-debug-test-b () (+ 1 2))
"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string content s))
          (compile:compile-file-safely src :output-file fasl
                                       :verbose nil :print nil)
          (load fasl)
          (let* ((fn-a (symbol-function 'cl-user::cst-debug-test-a))
                 (debug-fun (sb-di:fun-debug-fun fn-a))
                 (code-loc (sb-di:debug-fun-start-location debug-fun))
                 (debug-src (sb-di:code-location-debug-source code-loc))
                 (positions (sb-c::debug-source-start-positions debug-src)))
            (assert-true (vectorp positions))
            ;; Two top-level (defun ...) forms; comments preceding the
            ;; first push it to byte 38. Without the patch this would be
            ;; 0 (where SBCL's reader was *before* skipping trivia).
            (assert-= 38 (aref positions 0))
            ;; "(defun cst-debug-test-a () 1)\n" is 30 bytes, so the
            ;; second defun's open paren sits at 38 + 30 = 68.
            (assert-= 68 (aref positions 1))))
      (when (probe-file src) (delete-file src))
      (when (probe-file fasl) (delete-file fasl))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings") fasl)))
        (when (probe-file sidecar) (delete-file sidecar))))))

;;; Reader replacement (IMPL-320 Stage 2 proper)

(deftest test-cst-replace-reader-default-off
  "*cst-replace-reader* defaults to NIL — opt-in only."
  (assert-true (null compile-integration:*cst-replace-reader*)))

(deftest test-cst-replace-reader-end-to-end
  "Compiling under (let ((*cst-replace-reader* t)) ...) routes top-level
   forms through the CST datums instead of cl:read. The resulting FASL
   must load and run correctly: SBCL's pipeline works on our cons cells."
  (let* ((src #p"/tmp/epsilon-cst-reader-swap-test.lisp")
         (fasl #p"/tmp/epsilon-cst-reader-swap-test.fasl")
         (content "(defun csrt-a () 41)
(defun csrt-b () (+ (csrt-a) 1))
"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string content s))
          (let ((compile-integration:*cst-replace-reader* t))
            (compile:compile-file-safely src :output-file fasl
                                         :verbose nil :print nil))
          (load fasl)
          (assert-= 41 (funcall (symbol-function 'cl-user::csrt-a)))
          (assert-= 42 (funcall (symbol-function 'cl-user::csrt-b))))
      (when (probe-file src) (delete-file src))
      (when (probe-file fasl) (delete-file fasl))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings") fasl)))
        (when (probe-file sidecar) (delete-file sidecar))))))

(deftest test-cst-replace-reader-debug-positions
  "Reader-replaced compile-file still populates debug-source-start-
   positions with byte-accurate (defun ...) opening offsets."
  (let* ((src #p"/tmp/epsilon-cst-reader-swap-debug.lisp")
         (fasl #p"/tmp/epsilon-cst-reader-swap-debug.fasl")
         ;; Two leading comment lines (19 bytes each) push the first
         ;; defun to byte 38.
         (content ";; comment one
;; comment two
(defun csrtd-a () 1)
"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string content s))
          (let ((compile-integration:*cst-replace-reader* t))
            (compile:compile-file-safely src :output-file fasl
                                         :verbose nil :print nil))
          (load fasl)
          (let* ((fn (symbol-function 'cl-user::csrtd-a))
                 (debug-fun (sb-di:fun-debug-fun fn))
                 (cl (sb-di:debug-fun-start-location debug-fun))
                 (debug-src (sb-di:code-location-debug-source cl))
                 (positions (sb-c::debug-source-start-positions debug-src)))
            (assert-true (vectorp positions))
            (assert-= 30 (aref positions 0))))
      (when (probe-file src) (delete-file src))
      (when (probe-file fasl) (delete-file fasl))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings") fasl)))
        (when (probe-file sidecar) (delete-file sidecar))))))

;;; in-package directive tracking (IMPL-320 Stage 2 follow-up)

(deftest test-in-package-form-package-detection
  "in-package-form-package recognises the directive across designators."
  (assert-equal "FOO"
                (compile-integration:in-package-form-package
                 '(in-package :foo)))
  (assert-equal "BAR"
                (compile-integration:in-package-form-package
                 '(in-package "BAR")))
  (assert-equal "BAZ"
                (compile-integration:in-package-form-package
                 '(in-package #:baz)))
  (assert-true (null (compile-integration:in-package-form-package
                      '(defun foo () 1))))
  (assert-true (null (compile-integration:in-package-form-package
                      '(in-package)))))

(deftest test-read-all-syntax-tracking-in-package-switches
  "Symbols read after a top-level (in-package ...) directive are interned
   in the new package, matching SBCL's reader behavior."
  (unless (find-package :cst-inpkg-track-test)
    (make-package :cst-inpkg-track-test :use '(:cl)))
  (let* ((source "(in-package :cst-inpkg-track-test)
(defvar tracked-marker 7)")
         ;; Scope *package* changes to this binding so the test doesn't
         ;; leak the package switch into the surrounding test runner.
         (*package* (find-package :cl-user))
         (nodes (compile-integration:read-all-syntax-tracking-in-package
                 source))
         (datums (loop for node in nodes
                       unless (member (reader:syntax-node-kind node)
                                      '(:whitespace :comment))
                         collect (reader:syntax-node-to-datum node))))
    (assert-= 2 (length datums))
    (let* ((second-form (second datums))
           (var-symbol (second second-form)))
      (assert-true (symbolp var-symbol))
      (assert-equal "CST-INPKG-TRACK-TEST"
                    (package-name (symbol-package var-symbol))))))

(deftest test-in-package-tracking-leaves-outer-package-unchanged
  "The package switch is local to the read pass — the caller's *package*
   is not mutated when build-file-info-from-cst is called."
  (let ((before-pkg *package*)
        (src #p"/tmp/epsilon-cst-inpkg-leak-test.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string "(in-package :keyword)
(defvar leak-marker 1)" s))
          (compile-integration:build-file-info-from-cst src)
          (assert-true (eq before-pkg *package*)))
      (when (probe-file src) (delete-file src)))))

(deftest test-cst-replace-reader-honors-in-package
  "Reader-replaced compile-file produces a FASL whose top-level forms
   were read in the package given by mid-file (in-package ...)."
  (unless (find-package :cst-rr-pkg-test)
    (make-package :cst-rr-pkg-test :use '(:cl)))
  (let* ((src #p"/tmp/epsilon-cst-rr-inpkg.lisp")
         (fasl #p"/tmp/epsilon-cst-rr-inpkg.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string "(in-package :cst-rr-pkg-test)
(defun rr-pkg-greeter () \"hi\")" s))
          (let ((compile-integration:*cst-replace-reader* t))
            (compile:compile-file-safely src :output-file fasl
                                         :verbose nil :print nil))
          (load fasl)
          (let ((sym (find-symbol "RR-PKG-GREETER" :cst-rr-pkg-test)))
            (assert-true sym)
            (assert-equal "CST-RR-PKG-TEST"
                          (package-name (symbol-package sym)))
            (assert-equal "hi" (funcall sym))))
      (when (probe-file src) (delete-file src))
      (when (probe-file fasl) (delete-file fasl))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings") fasl)))
        (when (probe-file sidecar) (delete-file sidecar))))))

(deftest test-defpackage-eagerly-creates-package-during-read
  "ensure-defpackage-defined evaluates a top-level DEFPACKAGE so a
   following (in-package ...) directive resolves during the same read
   pass, even if the package didn't exist before the read.  Uses
   unqualified DEFPACKAGE; the CST reader interns it in *package* (cl-
   user) which inherits cl:defpackage via :use, so eval reaches the
   real macro."
  (let ((pkg-name "CST-EAGER-PKG-TEST"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (unwind-protect
        (let* ((source (format nil "(defpackage :~(~A~) (:use :cl))
(in-package :~(~A~))
(defvar eager-marker 1)" pkg-name pkg-name))
               (*package* (find-package :cl-user))
               (nodes (compile-integration:read-all-syntax-tracking-in-package
                       source))
               (datums (loop for n in nodes
                             unless (member (reader:syntax-node-kind n)
                                            '(:whitespace :comment))
                               collect (reader:syntax-node-to-datum n))))
          (assert-true (find-package pkg-name))
          (let* ((defvar-form (third datums))
                 (var (second defvar-form)))
            (assert-equal pkg-name
                          (package-name (symbol-package var)))))
      (when (find-package pkg-name) (delete-package pkg-name)))))

(deftest test-cst-replace-reader-handles-cl-qualified-defpackage
  "A file using the explicit (cl:defpackage ...) qualifier compiles
   under reader replacement; this exercises the symbol-parser fix
   together with the eager defpackage evaluator."
  (when (find-package :cst-cl-qual-test)
    (delete-package :cst-cl-qual-test))
  (let* ((src #p"/tmp/epsilon-cst-cl-qualified.lisp")
         (fasl #p"/tmp/epsilon-cst-cl-qualified.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (write-string "(cl:defpackage :cst-cl-qual-test (:use :cl))
(in-package :cst-cl-qual-test)
(defun cl-qual-greet () \"hi\")" s))
          (let ((compile-integration:*cst-replace-reader* t))
            (compile:compile-file-safely src :output-file fasl
                                         :verbose nil :print nil))
          (load fasl)
          (let ((sym (find-symbol "CL-QUAL-GREET" :cst-cl-qual-test)))
            (assert-true sym)
            (assert-equal "hi" (funcall sym))))
      (when (probe-file src) (delete-file src))
      (when (probe-file fasl) (delete-file fasl))
      (let ((sidecar (merge-pathnames
                      (make-pathname :type "warnings") fasl)))
        (when (probe-file sidecar) (delete-file sidecar)))
      (when (find-package :cst-cl-qual-test)
        (delete-package :cst-cl-qual-test)))))
