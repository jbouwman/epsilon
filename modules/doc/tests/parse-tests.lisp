;;;; Tests for epsilon.doc.parse -- structured docstring parser

(defpackage epsilon.doc.parse-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.parse parse)
           (epsilon.annotate ann)))

;;; Helper

(defun parsed-section (result key)
  "Extract a section value from a parse-docstring result plist."
  (getf result key))

;;;; Plain / unstructured docstrings

(deftest test-nil-docstring
  "NIL input returns NIL."
  (assert-nil (parse:parse-docstring nil)))

(deftest test-empty-docstring
  "Empty string returns plist with empty summary."
  (let ((result (parse:parse-docstring "")))
    (assert-equal "" (parsed-section result :summary))))

(deftest test-plain-docstring
  "A docstring with no keyword sections returns everything as :summary."
  (let ((result (parse:parse-docstring "Transpose PITCH by INTERVAL.")))
    (assert-equal "Transpose PITCH by INTERVAL." (parsed-section result :summary))
    (assert-nil (parsed-section result :params))
    (assert-nil (parsed-section result :returns))
    (assert-nil (parsed-section result :signals))
    (assert-nil (parsed-section result :see))
    (assert-nil (parsed-section result :examples))
    (assert-nil (parsed-section result :since))
    (assert-nil (parsed-section result :deprecated))
    (assert-nil (parsed-section result :note))))

(deftest test-multiline-plain-docstring
  "Multi-line prose without sections is all summary."
  (let* ((text (format nil "First line.~%~%Second paragraph.~%Third line."))
         (result (parse:parse-docstring text)))
    (assert-equal text (parsed-section result :summary))))

;;;; :param section

(deftest test-single-param
  "Parse a single :param section."
  (let* ((text (format nil "Summary text.~%~%   :param pitch -- A pitch struct."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal "Summary text." (parsed-section result :summary))
    (assert-equal 1 (length params))
    (assert-equal "pitch" (getf (first params) :name))
    (assert-equal "A pitch struct." (getf (first params) :doc))))

(deftest test-multiple-params
  "Parse multiple :param sections."
  (let* ((text (format nil "Do a thing.~%~%   :param pitch       -- A pitch struct.~%   :param interval    -- An interval struct.~%   :param direction   -- :up or :down."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal "Do a thing." (parsed-section result :summary))
    (assert-equal 3 (length params))
    (assert-equal "pitch" (getf (first params) :name))
    (assert-equal "A pitch struct." (getf (first params) :doc))
    (assert-equal "interval" (getf (second params) :name))
    (assert-equal "An interval struct." (getf (second params) :doc))
    (assert-equal "direction" (getf (third params) :name))
    (assert-equal ":up or :down." (getf (third params) :doc))))

(deftest test-param-without-dashes
  "Parse :param without -- separator (just whitespace)."
  (let* ((text (format nil "Summary.~%~%   :param name  The name string."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal 1 (length params))
    (assert-equal "name" (getf (first params) :name))
    (assert-equal "The name string." (getf (first params) :doc))))

(deftest test-param-multiline-doc
  "A :param with continuation lines."
  (let* ((text (format nil "Summary.~%~%   :param pitch -- A pitch struct or~%                  pitch designator string."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal 1 (length params))
    (assert-equal "pitch" (getf (first params) :name))
    ;; Continuation lines are joined
    (assert-true (search "pitch designator string" (getf (first params) :doc)))))

(deftest test-param-hyphenated-name
  "Parse :param with a hyphenated parameter name."
  (let* ((text (format nil "Summary.~%~%   :param error-code -- The error code string."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal 1 (length params))
    (assert-equal "error-code" (getf (first params) :name))
    (assert-equal "The error code string." (getf (first params) :doc))))

(deftest test-param-hyphenated-no-dashes
  "Parse :param with hyphenated name and no -- separator."
  (let* ((text (format nil "Summary.~%~%   :param my-long-param  The value."))
         (result (parse:parse-docstring text))
         (params (parsed-section result :params)))
    (assert-equal "my-long-param" (getf (first params) :name))
    (assert-equal "The value." (getf (first params) :doc))))

;;;; :returns section

(deftest test-returns-section
  "Parse :returns section."
  (let* ((text (format nil "Summary.~%~%   :returns A new pitch struct."))
         (result (parse:parse-docstring text)))
    (assert-equal "A new pitch struct." (parsed-section result :returns))))

(deftest test-returns-with-dashes
  "Parse :returns with -- separator."
  (let* ((text (format nil "Summary.~%~%   :returns -- A new pitch struct."))
         (result (parse:parse-docstring text)))
    (assert-equal "A new pitch struct." (parsed-section result :returns))))

(deftest test-returns-multiline
  "Parse multiline :returns section."
  (let* ((text (format nil "Summary.~%~%   :returns A new pitch struct. Does not~%            mutate the input."))
         (result (parse:parse-docstring text)))
    (assert-true (search "mutate the input" (parsed-section result :returns)))))

;;;; :signals section

(deftest test-signals-section
  "Parse :signals section."
  (let* ((text (format nil "Summary.~%~%   :signals `invalid-pitch` if PITCH cannot be parsed."))
         (result (parse:parse-docstring text)))
    (assert-true (search "invalid-pitch" (parsed-section result :signals)))))

(deftest test-signals-multiline
  "Parse multiline :signals with multiple conditions."
  (let* ((text (format nil "Summary.~%~%   :signals `invalid-pitch` if PITCH cannot be parsed.~%            `interval-overflow` if result is outside MIDI range."))
         (result (parse:parse-docstring text)))
    (assert-true (search "invalid-pitch" (parsed-section result :signals)))
    (assert-true (search "interval-overflow" (parsed-section result :signals)))))

;;;; :see section

(deftest test-see-section
  "Parse :see section."
  (let* ((text (format nil "Summary.~%~%   :see IMPL-098, `kreisler.music:enharmonic-resolve`"))
         (result (parse:parse-docstring text))
         (sees (parsed-section result :see)))
    (assert-true (listp sees))
    (assert-true (member "IMPL-098" sees :test #'string=))
    (assert-true (member "kreisler.music:enharmonic-resolve" sees :test #'string=))))

(deftest test-see-single-ref
  "Parse :see with a single reference."
  (let* ((text (format nil "Summary.~%~%   :see IMPL-297"))
         (result (parse:parse-docstring text))
         (sees (parsed-section result :see)))
    (assert-equal 1 (length sees))
    (assert-equal "IMPL-297" (first sees))))

;;;; :example section

(deftest test-example-section
  "Parse :example section with form and result."
  (let* ((text (format nil "Summary.~%~%   :example~%     (transpose (make-pitch :c 4) (make-interval :perfect-fifth))~%     ;=> #<PITCH E4>"))
         (result (parse:parse-docstring text))
         (examples (parsed-section result :examples)))
    (assert-true (listp examples))
    (assert-equal 1 (length examples))
    (let ((ex (first examples)))
      (assert-true (search "transpose" (getf ex :form)))
      (assert-equal "#<PITCH E4>" (getf ex :result)))))

(deftest test-example-no-result
  "Parse :example with no ;=> result marker."
  (let* ((text (format nil "Summary.~%~%   :example~%     (do-something 42)"))
         (result (parse:parse-docstring text))
         (examples (parsed-section result :examples)))
    (assert-equal 1 (length examples))
    (assert-true (search "do-something" (getf (first examples) :form)))
    (assert-nil (getf (first examples) :result))))

(deftest test-multiple-examples
  "Parse :example section with multiple example blocks."
  (let* ((text (format nil "Summary.~%~%   :example~%     (foo 1)~%     ;=> 2~%~%     (foo 3)~%     ;=> 4"))
         (result (parse:parse-docstring text))
         (examples (parsed-section result :examples)))
    (assert-equal 2 (length examples))
    (assert-equal "2" (getf (first examples) :result))
    (assert-equal "4" (getf (second examples) :result))))

;;;; :since section

(deftest test-since-section
  "Parse :since section."
  (let* ((text (format nil "Summary.~%~%   :since 1.3.0"))
         (result (parse:parse-docstring text)))
    (assert-equal "1.3.0" (parsed-section result :since))))

;;;; :deprecated section

(deftest test-deprecated-section
  "Parse :deprecated section."
  (let* ((text (format nil "Summary.~%~%   :deprecated Use new-function instead."))
         (result (parse:parse-docstring text)))
    (assert-equal "Use new-function instead." (parsed-section result :deprecated))))

;;;; :note section

(deftest test-note-section
  "Parse :note section."
  (let* ((text (format nil "Summary.~%~%   :note This is an implementation note."))
         (result (parse:parse-docstring text)))
    (assert-equal "This is an implementation note." (parsed-section result :note))))

(deftest test-note-multiline
  "Parse multiline :note."
  (let* ((text (format nil "Summary.~%~%   :note This is line one.~%         This is line two."))
         (result (parse:parse-docstring text)))
    (assert-true (search "line one" (parsed-section result :note)))
    (assert-true (search "line two" (parsed-section result :note)))))

;;;; Combined sections

(deftest test-full-structured-docstring
  "Parse a fully structured docstring with all sections."
  (let* ((text (format nil "Transpose PITCH by INTERVAL in the given DIRECTION.~%~%   Computes the new pitch by adding or subtracting.~%~%   :param pitch       -- A `pitch` struct or pitch designator string.~%   :param interval    -- An `interval` struct or interval designator.~%   :param direction   -- :up (default) or :down.~%   :returns           -- A new `pitch` struct. Does not mutate the input.~%   :signals           -- `invalid-pitch` if PITCH cannot be parsed.~%                         `interval-overflow` if result is outside MIDI range.~%   :see               -- IMPL-098, `kreisler.music:enharmonic-resolve`~%   :example~%     (transpose (make-pitch :c 4) (make-interval :perfect-fifth))~%     ;=> #<PITCH E4>~%   :since 1.3.0"))
         (result (parse:parse-docstring text)))
    ;; Summary includes prose before first section
    (assert-true (search "Transpose PITCH" (parsed-section result :summary)))
    (assert-true (search "adding or subtracting" (parsed-section result :summary)))
    ;; Params
    (assert-equal 3 (length (parsed-section result :params)))
    ;; Returns
    (assert-true (search "new" (parsed-section result :returns)))
    ;; Signals
    (assert-true (search "invalid-pitch" (parsed-section result :signals)))
    ;; See
    (assert-true (member "IMPL-098" (parsed-section result :see) :test #'string=))
    ;; Examples
    (assert-equal 1 (length (parsed-section result :examples)))
    ;; Since
    (assert-equal "1.3.0" (parsed-section result :since))))

;;;; Edge cases

(deftest test-unrecognized-keyword-stays-in-prose
  "A keyword that is not in the recognized set stays in prose/summary."
  (let* ((text (format nil "Summary with :foobar at start of conceptual line.~%~%   :foobar should remain in summary."))
         (result (parse:parse-docstring text)))
    (assert-true (search ":foobar" (parsed-section result :summary)))))

(deftest test-keyword-not-at-line-start
  "A recognized keyword mid-line is not treated as a section."
  (let* ((text "The function returns :since it was added.")
         (result (parse:parse-docstring text)))
    (assert-equal text (parsed-section result :summary))
    (assert-nil (parsed-section result :since))))

(deftest test-whitespace-only-docstring
  "Whitespace-only docstring returns trimmed empty summary."
  (let* ((result (parse:parse-docstring "   ")))
    (assert-equal "" (parsed-section result :summary))))

(deftest test-indented-sections
  "Sections with varying indentation are parsed correctly."
  (let* ((text (format nil "Summary.~%~%  :param x -- The x value.~%      :returns The result."))
         (result (parse:parse-docstring text)))
    (assert-equal 1 (length (parsed-section result :params)))
    (assert-true (search "result" (parsed-section result :returns)))))

(deftest test-section-with-empty-content
  "A section keyword with no content after it."
  (let* ((text (format nil "Summary.~%~%   :deprecated~%   :since 2.0.0"))
         (result (parse:parse-docstring text)))
    ;; :deprecated has no content before :since starts
    (assert-equal "" (parsed-section result :deprecated))
    (assert-equal "2.0.0" (parsed-section result :since))))

;;;; Annotation merging

(deftest test-merge-annotations-empty
  "Merging with no annotations returns parsed docstring as-is."
  (let* ((parsed (parse:parse-docstring "Summary."))
         (merged (parse:merge-annotations parsed nil)))
    (assert-equal "Summary." (parsed-section merged :summary))))

(deftest test-merge-see-annotations
  "Merging :see from both docstring and annotations."
  (let* ((text (format nil "Summary.~%~%   :see IMPL-098"))
         (parsed (parse:parse-docstring text))
         (annotations '((:see . "IMPL-297")))
         (merged (parse:merge-annotations parsed annotations))
         (sees (parsed-section merged :see)))
    (assert-true (member "IMPL-098" sees :test #'string=))
    (assert-true (member "IMPL-297" sees :test #'string=))))

(deftest test-merge-since-annotation-overrides
  "Annotation :since overrides docstring :since."
  (let* ((text (format nil "Summary.~%~%   :since 1.0.0"))
         (parsed (parse:parse-docstring text))
         (annotations '((:since . "2.0.0")))
         (merged (parse:merge-annotations parsed annotations)))
    (assert-equal "2.0.0" (parsed-section merged :since))))

(deftest test-merge-tags-from-annotations
  "Annotation :tags are preserved in merged result."
  (let* ((parsed (parse:parse-docstring "Summary."))
         (annotations '((:tags . (:music :pitch))))
         (merged (parse:merge-annotations parsed annotations)))
    (assert-equal '(:music :pitch) (parsed-section merged :tags))))

(deftest test-merge-stability-from-annotations
  "Annotation :stability is preserved in merged result."
  (let* ((parsed (parse:parse-docstring "Summary."))
         (annotations '((:stability . :stable)))
         (merged (parse:merge-annotations parsed annotations)))
    (assert-equal :stable (parsed-section merged :stability))))

(deftest test-merge-see-deduplicates
  "Merging :see does not duplicate entries."
  (let* ((text (format nil "Summary.~%~%   :see IMPL-098"))
         (parsed (parse:parse-docstring text))
         (annotations '((:see . "IMPL-098")))
         (merged (parse:merge-annotations parsed annotations))
         (sees (parsed-section merged :see)))
    (assert-equal 1 (length sees))))
