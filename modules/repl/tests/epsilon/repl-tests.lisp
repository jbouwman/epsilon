(defpackage epsilon.repl-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.repl repl)))

(in-package :epsilon.repl-tests)

;;; --------------------------------------------------------------------------
;;; %directive-p
;;; --------------------------------------------------------------------------

(deftest test-directive-p-recognises-exit-and-quit
  "Only :exit and :quit are repl directives"
  (assert-true (repl::%directive-p :exit))
  (assert-true (repl::%directive-p :quit))
  (assert-true (repl::%directive-p :EXIT))
  (assert-true (repl::%directive-p :Quit)))

(deftest test-directive-p-rejects-other-keywords
  "Random keywords are values, not directives"
  (assert-false (repl::%directive-p :foo))
  (assert-false (repl::%directive-p :hello))
  (assert-false (repl::%directive-p nil))
  (assert-false (repl::%directive-p 'cl-user::exit)))

;;; --------------------------------------------------------------------------
;;; End-to-end via string streams
;;; --------------------------------------------------------------------------
;;;
;;; Each test feeds a script of forms through the REPL with EOF at
;;; the end and inspects the captured output.  The REPL itself
;;; returns NIL on EOF, so the assertions are on the printed text.

(defun run-script (script &key (initial-package "CL-USER"))
  "Drive the REPL with SCRIPT (a string of newline-separated forms),
   returning the captured output."
  (with-input-from-string (in script)
    (with-output-to-string (out)
      (repl:repl :input in
                 :output out
                 :banner nil
                 :initial-package initial-package))))

(deftest test-repl-evaluates-forms-and-prints-values
  "Each form's value is printed; the prompt precedes each read"
  (let ((output (run-script "(+ 1 2)
")))
    (assert-true (search "CL-USER>" output))
    (assert-true (search "3" output))))

(deftest test-repl-handles-multiple-forms
  "Two forms in sequence both get evaluated and printed"
  (let ((output (run-script "(+ 1 2)
(* 3 4)
")))
    (assert-true (search "3" output))
    (assert-true (search "12" output))))

(deftest test-repl-prints-multiple-values
  "Values from VALUES print one per line"
  (let ((output (run-script "(values 1 2 3)
")))
    (assert-true (search "1" output))
    (assert-true (search "2" output))
    (assert-true (search "3" output))))

(deftest test-repl-prints-no-line-for-no-values
  "(values) returns nothing; no value line is printed"
  (let ((output (run-script "(values)
")))
    ;; Two prompt occurrences (first prompt, post-eval prompt) and
    ;; nothing else of substance.  Allow trailing whitespace.
    (assert-equal 2
                  (loop for i = (search "CL-USER>" output :start2 0) then
                                (search "CL-USER>" output :start2 (1+ i))
                        while i count i))))

(deftest test-repl-traps-errors-and-continues
  "An eval error prints a one-line ; error: line and the loop continues"
  (let ((output (run-script "(error \"boom\")
(+ 1 2)
")))
    (assert-true (search "; error:" output))
    (assert-true (search "boom" output))
    ;; The next form still ran -- we see its value.
    (assert-true (search "3" output))))

(deftest test-repl-traps-reader-errors
  "Malformed input prints ; reader error: and the loop continues"
  (let ((output (run-script "(unbalanced
(+ 1 2)
")))
    (assert-true (search "; reader error:" output))
    (assert-true (search "3" output))))

(deftest test-repl-exits-on-exit-directive
  "Top-level :exit ends the loop without evaluating later forms"
  (let ((output (run-script ":exit
(this-would-error)
")))
    ;; (this-would-error) was never read/evaluated.
    (assert-false (search "this-would-error" output))
    (assert-false (search "; error:" output))))

(deftest test-repl-exits-on-quit-directive
  ":quit also ends the loop"
  (let ((output (run-script ":quit
(does-not-run)
")))
    (assert-false (search "does-not-run" output))))

(deftest test-repl-tracks-package-changes
  "After (in-package :foo) the prompt reflects the new package"
  (let ((output (run-script "(in-package :keyword)
")))
    ;; First prompt is CL-USER, post-in-package prompt is KEYWORD.
    (assert-true (search "CL-USER>" output))
    (assert-true (search "KEYWORD>" output))))

(deftest test-repl-history-star
  "After (+ 1 2), * holds 3; * is then 12 after (* 3 4); ** is the prior 3"
  (let ((output (run-script "(+ 1 2)
(* 3 4)
*
**
")))
    ;; Last two values printed are 12 (current *) and 3 (** = prior *).
    ;; Order: 3, 12, 12, 3
    (assert-true (search "3" output))
    (assert-true (search "12" output))
    ;; Make sure we see both '3' and '12' more than once.
    (let ((count-3 (loop for i = (search "3" output :start2 0) then
                                 (search "3" output :start2 (1+ i))
                         while i count i)))
      ;; '3' appears at the +/result for (+ 1 2), ** lookup, ..., so >= 2
      (assert-true (>= count-3 2)))))

(deftest test-repl-banner-when-enabled
  "With :banner t the output starts with the banner text"
  (let ((output (with-output-to-string (out)
                  (with-input-from-string (in "")
                    (repl:repl :input in :output out :banner t)))))
    (assert-true (search "Epsilon REPL" output))
    (assert-true (search "Lisp implementation" output))))

(deftest test-repl-no-banner-when-disabled
  "With :banner nil the banner is suppressed"
  (let ((output (with-output-to-string (out)
                  (with-input-from-string (in "")
                    (repl:repl :input in :output out :banner nil)))))
    (assert-false (search "Epsilon REPL" output))))

(deftest test-repl-initial-package-honored
  "Starting in :KEYWORD shows KEYWORD in the prompt"
  (let ((output (run-script "" :initial-package "KEYWORD")))
    (assert-true (search "KEYWORD>" output))))
